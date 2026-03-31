(in-package :cl-cc)

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
        (fn-var (gensym "FN")))
    `(let ((,cond-var ,condition))
       (dolist (,entry-var *%condition-handlers*)
         (let ((,type-var (first ,entry-var))
               (,fn-var (second ,entry-var)))
           (when (or (eq ,type-var t) (eq ,type-var 'condition)
                     (eq ,type-var 'error) (eq ,type-var 'warning)
                     (eq ,type-var 'serious-condition))
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
    (let ((clauses (cddr form))
          (form-expr (second form)))
      (if (null clauses)
          form-expr
          ;; Parse each clause: (name lambda-list [:report r] [:interactive i] [:test t] . body)
          (let ((parsed-clauses nil)
                (counter 0))
            (dolist (clause clauses)
              (let* ((name (first clause))
                     (lambda-list (second clause))
                     (rest (cddr clause))
                     (body (progn
                             (loop while (and rest (keywordp (car rest))
                                              (member (car rest) '(:report :interactive :test)))
                                   do (setf rest (cddr rest)))
                             rest))
                     (tag (intern (format nil "RESTART-~A-~D" name (incf counter))
                                  (find-package :cl-cc))))
                (push (list name tag lambda-list body) parsed-clauses)))
            (setf parsed-clauses (nreverse parsed-clauses))
            ;; Build nested catches from inside out
            (let* ((result-var (gensym "RESULT"))
                   (restarts-var (gensym "RESTARTS"))
                   ;; innermost: the form wrapped in handler-case
                   (inner form-expr)
                   ;; Wrap each restart as a catch tag
                   (wrapped inner))
              ;; Build the catch nesting from last to first
              (dolist (pc (reverse parsed-clauses))
                (let ((tag (second pc))
                      (ll  (third pc))
                      (body (fourth pc)))
                  (setf wrapped
                        `(let ((,result-var (catch ',tag ,wrapped)))
                           ;; If we got here via throw, result-var holds the thrown value
                           ;; Run the restart body; bind lambda-list params if any
                           ,(if (and ll (car ll))
                                `(let ((,(car ll) ,result-var)) ,@(or body '(nil)))
                                `(progn ,@(or body '(nil))))))))
              ;; Build the *%active-restarts* binding
              (let ((restart-entries (mapcar (lambda (pc)
                                              `(cons ',(first pc) ',(second pc)))
                                            parsed-clauses)))
                `(let ((,restarts-var (list ,@restart-entries)))
                   (let ((*%active-restarts* (append ,restarts-var *%active-restarts*)))
                     ,wrapped)))))))))

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

;; WITH-SIMPLE-RESTART (FR-422) — stub: just executes body
(register-macro 'with-simple-restart
  (lambda (form env)
    (declare (ignore env))
    ;; (with-simple-restart (name fmt args...) body...)
    `(progn ,@(cddr form))))

;; WITH-CONDITION-RESTARTS (FR-423) — stub: just executes body
(our-defmacro with-condition-restarts (condition restarts &body body)
  (declare (ignore condition restarts))
  `(progn ,@body))
