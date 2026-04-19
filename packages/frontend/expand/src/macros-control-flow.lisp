;;;; macros-control-flow.lisp — Control-flow bootstrap macros
(in-package :cl-cc/expand)

;; WHEN macro
(register-macro 'when
  (lambda (form env)
    (declare (ignore env))
    (list 'if (second form) (cons 'progn (cddr form)) nil)))

;; UNLESS macro
(register-macro 'unless
  (lambda (form env)
    (declare (ignore env))
    (list 'if (second form) nil (cons 'progn (cddr form)))))

;; COND macro
(register-macro 'cond
  (lambda (form env)
    (declare (ignore env))
    (let ((clauses (cdr form)))
      (if (null clauses)
          nil
          (let ((clause (car clauses)))
            (if (null (cdr clause))
                (cons 'or (list (car clause) (cons 'cond (cdr clauses))))
                (list 'if (car clause)
                      (cons 'progn (cdr clause))
                      (cons 'cond (cdr clauses)))))))))

;; AND macro
(register-macro 'and
  (lambda (form env)
    (declare (ignore env))
    (let ((args (cdr form)))
      (cond ((null args) t)
            ((null (cdr args)) (car args))
            (t (list 'if (car args) (cons 'and (cdr args)) nil))))))

;; OR macro
(register-macro 'or
  (lambda (form env)
    (declare (ignore env))
    (let ((args (cdr form)))
      (cond ((null args) nil)
            ((null (cdr args)) (car args))
            (t (let ((tmp (gensym "OR")))
                 (list 'let (list (list tmp (car args)))
                       (list 'if tmp tmp (cons 'or (cdr args))))))))))

;; LET* macro (recursive let)
(register-macro 'let*
  (lambda (form env)
    (declare (ignore env))
    (let ((bindings (second form))
          (body (cddr form)))
      (if (null bindings)
          (cons 'progn body)
          (let ((binding (car bindings)))
            (list 'let (list binding)
                  (cons 'let* (cons (cdr bindings) body))))))))

;; DEFUN macro
(our-defmacro defun (name params &body body)
  `(setf (fdefinition ',name)
         (lambda ,params ,@body)))

;; DEFUN/C macro
;; Minimal design-by-contract support: :requires, :ensures, :invariant.
(our-defmacro defun/c (name params &rest clauses)
  (let ((requires nil)
        (ensures nil)
        (invariant nil)
        (body clauses))
    (loop while (and body
                     (keywordp (first body))
                     (member (first body) '(:requires :ensures :invariant) :test #'eq)
                     (cdr body))
          do (let ((key (pop body))
                   (value (pop body)))
               (case key
                 (:requires (setf requires value))
                 (:ensures  (setf ensures value))
                 (:invariant (setf invariant value)))))
    (let* ((pre-check  (remove nil (list requires invariant)))
           (post-check (remove nil (list ensures invariant)))
           (pre-form   (when pre-check
                         `(unless (and ,@pre-check)
                            (error "Precondition failed in ~S" ',name))))
           (post-form  (when post-check
                         `(unless (and ,@post-check)
                            (error "Postcondition failed in ~S" ',name)))))
      ;; Use a result-sym from the same package as the function name so that
      ;; :ensures clauses can reference RESULT without cross-package mismatch.
      (let ((result-sym (intern "RESULT" (symbol-package name))))
        `(defun ,name ,params
           ,@(when pre-form (list pre-form))
           (let ((,result-sym (progn ,@body)))
             ,@(when post-form (list post-form))
             ,result-sym))))))

;; PROG1 macro
(register-macro 'prog1
  (lambda (form env)
    (declare (ignore env))
    (let ((first-form (second form))
          (body (cddr form))
          (result (gensym "RESULT")))
      (cons 'let
            (cons (list (list result first-form))
                  (append body (list result)))))))

;; PROG2 macro
(register-macro 'prog2
  (lambda (form env)
    (declare (ignore env))
    (let ((first-form (second form))
          (second-form (third form))
          (body (cdddr form))
          (result (gensym "RESULT")))
      (list 'progn
            first-form
            (cons 'let
                  (cons (list (list result second-form))
                        (append body (list result))))))))

;; DOLIST macro
(our-defmacro dolist (binding-spec &body body)
  "Iterate over a list, binding each element to a variable in BODY.
   BINDING-SPEC is (var list-form &optional result-form)."
  (let ((var (first binding-spec))
        (list-form (second binding-spec))
        (result-form (third binding-spec))
        (list-var (gensym "LIST"))
        (start-tag (gensym "START"))
        (end-tag (gensym "END")))
    `(block nil
       (let ((,list-var ,list-form)
             (,var nil))
         (tagbody
            ,start-tag
            (if (null ,list-var)
                (go ,end-tag))
            (setq ,var (car ,list-var))
            (setq ,list-var (cdr ,list-var))
            ,@body
            (go ,start-tag)
            ,end-tag)
         ,result-form))))

;; DOTIMES macro
(our-defmacro dotimes (binding-spec &body body)
  "Iterate count times, binding a variable to 0, 1, 2, ... count-1 in BODY.
   BINDING-SPEC is (var count-form &optional result-form)."
  (let ((var (first binding-spec))
        (count-form (second binding-spec))
        (result-form (third binding-spec))
        (count-var (gensym "COUNT"))
        (start-tag (gensym "START"))
        (end-tag (gensym "END")))
    `(block nil
       (let ((,var 0)
             (,count-var ,count-form))
         (tagbody
            ,start-tag
            (if (>= ,var ,count-var)
                (go ,end-tag))
            ,@body
            (setq ,var (+ ,var 1))
            (go ,start-tag)
            ,end-tag)
         ,result-form))))

(defun expand-do-form (var-clauses end-clause body let-kw make-step-forms)
  "Shared expansion for DO and DO*. LET-KW is 'let or 'let*.
MAKE-STEP-FORMS is (vars steps) -> list of step forms to splice into tagbody."
  (let ((start-tag (gensym "START"))
        (end-tag   (gensym "END"))
        (vars   (mapcar #'car var-clauses))
        (inits  (mapcar (lambda (c) (if (consp (cdr c)) (cadr c) nil)) var-clauses))
        (steps  (mapcar (lambda (c) (if (cddr c) (caddr c) (car c))) var-clauses))
        (test    (car end-clause))
        (results (cdr end-clause)))
    `(block nil
       (,let-kw ,(mapcar #'list vars inits)
         (tagbody
            ,start-tag
            (if ,test (go ,end-tag))
            ,@body
            ,@(funcall make-step-forms vars steps)
            (go ,start-tag)
            ,end-tag)
         (progn ,@results)))))

;; DO macro (parallel steps)
(our-defmacro do (var-clauses end-clause &body body)
  "General iteration construct.
   VAR-CLAUSES: ((var init step) ...)  END-CLAUSE: (test result ...)
   Evaluates BODY, then updates all vars simultaneously (psetq), then tests."
  (expand-do-form var-clauses end-clause body 'let
                  (lambda (vars steps) `((psetq ,@(mapcan #'list vars steps))))))

;; DO* macro (sequential steps)
(our-defmacro do* (var-clauses end-clause &body body)
  "General iteration construct with sequential binding.
   VAR-CLAUSES: ((var init step) ...)  END-CLAUSE: (test result ...)
   Like DO but bindings and steps are sequential (LET*/SETQ)."
  (expand-do-form var-clauses end-clause body 'let*
                  (lambda (vars steps)
                    (mapcar (lambda (var step) `(setq ,var ,step)) vars steps))))

;; CASE and TYPECASE macros are in macros-control-flow-case.lisp (loaded next).
