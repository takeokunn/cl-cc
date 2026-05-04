(in-package :cl-cc/expand)

;;; ─── Restart and condition protocol split from stdlib ───────────────────────

;; HANDLER-BIND (FR-201) — non-unwinding handler protocol via dynamic registry.
;; Uses PROGV for true dynamic binding so called functions (signal) see the handlers.
;; Handlers are called in the dynamic context of the signaler (no stack unwind).
;; If a handler returns normally it "declines" and the next handler is tried.
;; Handlers can transfer control via invoke-restart, throw, or return-from.
(register-macro 'handler-bind
  (lambda (form env)
    (declare (ignore env))
    (let ((bindings (second form))
          (body (cddr form)))
      (if (null bindings)
          `(progn ,@body)
          (let ((new-handlers-var (gensym "HANDLERS"))
                (entries (mapcar (lambda (b)
                                  `(list ',(first b) ,(second b)))
                                bindings)))
            `(let ((,new-handlers-var (append (list ,@entries) *%condition-handlers*)))
               (progv '(*%condition-handlers*) (list ,new-handlers-var)
                 ,@body)))))))

;; SIGNAL (FR-201) — walk handler-bind registry without unwinding.
;; Defined as macro so it expands inline and reads *%condition-handlers* in caller's scope.
(our-defmacro signal (condition &rest args)
  (declare (ignore args))
  (let ((cond-var (gensym "COND"))
        (entry-var (gensym "ENTRY"))
        (type-var (gensym "TYPE"))
        (fn-var (gensym "FN"))
        (kind-var (gensym "KIND")))
    `(let ((,cond-var ,condition))
        (dolist (,entry-var *%condition-handlers*)
          (let ((,type-var (first ,entry-var))
                (,fn-var (second ,entry-var)))
            (when (or (typep ,cond-var ,type-var)
                      (and (consp ,cond-var)
                           (let ((,kind-var (first ,cond-var)))
                             (or (eq ,kind-var ,type-var)
                                 (and (eq ,kind-var 'type-error)
                                      (or (eq ,type-var 'type-error)
                                          (eq ,type-var 'error)
                                          (eq ,type-var 'serious-condition)
                                          (eq ,type-var 'condition)))))))
              (funcall ,fn-var ,cond-var))))
        nil)))

;;; ── Restart Protocol (FR-202/FR-421) ────────────────────────────────────────
;;;
;;; Minimal restart protocol using catch/throw and a dynamic restart registry.
;;; *%active-restarts* is a list of (name . catch-tag) entries.
;;; restart-case pushes entries and wraps form in nested catches.
;;; invoke-restart throws to the matching tag.

;; RESTART-CASE (FR-202) — working restart protocol via catch/throw
(register-macro 'restart-case
  (lambda (form env)
    (declare (ignore env))
    (labels ((restart-option-key-p (key)
               (or (eq key :report) (eq key :interactive) (eq key :test)))
             (strip-restart-options (tail)
               (if (and tail (keywordp (car tail)) (restart-option-key-p (car tail)))
                   (strip-restart-options (cddr tail))
                   tail))
             (parse-clauses (remaining counter acc)
               (if (null remaining)
                   (nreverse acc)
                   (let* ((clause (car remaining))
                          (name (first clause))
                          (lambda-list (second clause))
                          (body (strip-restart-options (cddr clause)))
                          (next-counter (+ counter 1))
                          (tag (intern (format nil "RESTART-~A-~D" name next-counter))))
                     (parse-clauses (cdr remaining)
                                    next-counter
                                    (cons (list name tag lambda-list body) acc)))))
             (restart-body-form (lambda-list body result-var)
               (if (and lambda-list (car lambda-list))
                   (list (quote let)
                         (list (list (car lambda-list) result-var))
                         (cons (quote progn) (or body (list nil))))
                   (cons (quote progn) (or body (list nil)))))
             (wrap-catches (parsed result-var wrapped)
               (if (null parsed)
                   wrapped
                   (let* ((pc (car parsed))
                          (tag (second pc))
                          (lambda-list (third pc))
                          (body (fourth pc))
                          (next-wrapped
                            (list (quote let)
                                  (list (list result-var
                                              (list (quote catch)
                                                    (list (quote quote) tag)
                                                    wrapped)))
                                  (restart-body-form lambda-list body result-var))))
                     (wrap-catches (cdr parsed) result-var next-wrapped))))
             (restart-entry-form (pc)
               (list (quote cons)
                     (list (quote quote) (first pc))
                     (list (quote quote) (second pc))))
             (restart-entry-forms (parsed acc)
               (if (null parsed)
                   (nreverse acc)
                   (restart-entry-forms (cdr parsed)
                                        (cons (restart-entry-form (car parsed)) acc)))))
      (let ((clauses (cddr form))
            (form-expr (second form)))
        (if (null clauses)
            form-expr
            (let* ((parsed-clauses (parse-clauses clauses 0 nil))
                   (result-var (gensym "RESULT"))
                   (restarts-var (gensym "RESTARTS"))
                   (wrapped (wrap-catches (reverse parsed-clauses) result-var form-expr))
                   (restart-entries (restart-entry-forms parsed-clauses nil)))
              (list (quote let)
                    (list (cons restarts-var (list (cons (quote list) restart-entries))))
                    (list (quote let)
                          (list (list (quote *%active-restarts*)
                                      (list (quote append)
                                            restarts-var
                                            (quote *%active-restarts*))))
                          wrapped))))))))

;; RESTART-BIND (FR-202) — binds restarts for the dynamic extent of body
(our-defmacro restart-bind (bindings &body body)
  "Establish restart bindings around BODY."
  (if (null bindings)
      `(progn ,@body)
      (let ((restarts-var (gensym "RESTARTS"))
            (entries (mapcar (lambda (b) `(cons ',(first b) ',(gensym "RBTAG")))
                             bindings)))
        `(let ((,restarts-var (list ,@entries)))
           (let ((*%active-restarts* (append ,restarts-var *%active-restarts*)))
             ,@body)))))

;; INVOKE-RESTART — look up and throw to the restart's catch tag
(our-defmacro invoke-restart (name &rest args)
  "Invoke restart NAME. Transfers control to the matching restart-case clause."
  (let ((entry-var (gensym "ENTRY"))
        (name-var (gensym "NAME")))
    `(let* ((,name-var ,name)
            (,entry-var (assoc ,name-var *%active-restarts* :test #'eq)))
       (if ,entry-var
           (throw (cdr ,entry-var) ,(if args (first args) nil))
           (error (format nil "No restart named ~A is active" ,name-var))))))

;; INVOKE-RESTART-INTERACTIVELY — same as invoke-restart (no interactive support)
(our-defmacro invoke-restart-interactively (name)
  `(invoke-restart ,name))

;; RESTART-NAME — extract name from a restart entry
(our-defmacro restart-name (restart)
  `(if (consp ,restart) (car ,restart) ,restart))

;; FIND-RESTART — search the active restart list
(our-defmacro find-restart (name &optional condition)
  (declare (ignore condition))
  (let ((name-var (gensym "NAME")))
    `(let ((,name-var ,name))
       (assoc ,name-var *%active-restarts* :test #'eq))))

;; COMPUTE-RESTARTS — return all active restarts
(our-defmacro compute-restarts (&optional condition)
  (declare (ignore condition))
  '*%active-restarts*)

;; RESTART-P — check if something is a restart
(register-macro 'restart-p
  (lambda (form env) (declare (ignore env)) `(consp ,(second form))))

;; ABORT — invoke the abort restart, or signal error if none
(our-defmacro abort (&optional condition)
  (declare (ignore condition))
  `(let ((r (find-restart 'abort)))
     (if r
         (invoke-restart 'abort)
         (error "ABORT invoked"))))

;; CONTINUE — invoke the continue restart if available
(our-defmacro continue (&optional condition)
  (declare (ignore condition))
  `(let ((r (find-restart 'continue)))
     (when r (invoke-restart 'continue))))

;; MUFFLE-WARNING — invoke the muffle-warning restart if available
(our-defmacro muffle-warning (&optional condition)
  (declare (ignore condition))
  `(let ((r (find-restart 'muffle-warning)))
     (when r (invoke-restart 'muffle-warning))))

;; USE-VALUE — invoke the use-value restart with a value
(our-defmacro use-value (value &optional condition)
  (declare (ignore condition))
  `(let ((r (find-restart 'use-value)))
     (if r
         (invoke-restart 'use-value ,value)
         ,value)))

;; STORE-VALUE — invoke the store-value restart with a value
(our-defmacro store-value (value &optional condition)
  (declare (ignore condition))
  `(let ((r (find-restart 'store-value)))
     (if r
         (invoke-restart 'store-value ,value)
         ,value)))
