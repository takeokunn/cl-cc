(in-package :cl-cc/prolog)

;;; Built-in Predicate Dispatch Table (data layer)
;;;
;;; Each handler is (lambda (args env k)) — continuation-passing style:
;;; args = predicate arguments, env = current bindings, k = success continuation.

(defvar *builtin-predicates*)

(declaim (ftype function subst-for-eval eval-lisp-condition
                      solve-conjunction solve-goal our-eval))

(defun prolog-cut-handler (args env k)
  (declare (ignore args))
  (funcall k env)
  (signal 'prolog-cut))

(defun prolog-and-handler (args env k)
  (solve-conjunction args env k))

(defun prolog-or-handler (args env k)
  (dolist (alt args)
    (solve-goal alt env k)))

(defun prolog-unify-handler (args env k)
  (let ((new-env (unify (first args) (second args) env)))
    (unless (unify-failed-p new-env)
      (funcall k new-env))))

(defun prolog-not-unify-handler (args env k)
  (let ((v1 (logic-substitute (first args) env))
        (v2 (logic-substitute (second args) env)))
    (when (not (equal v1 v2))
      (funcall k env))))

(defun prolog-when-handler (args env k)
  (when (eval-lisp-condition (first args) env)
    (funcall k env)))

(defun subst-for-eval (form env)
  "Like logic-substitute, but wraps substituted non-self-evaluating symbols in
   (quote ...) so they survive CL eval, and skips (quote ...) forms."
  (cond
    ((logic-var-p form)
     (let ((val (logic-substitute form env)))
       (if (logic-var-p val)
           val
           (if (and (symbolp val) val (not (keywordp val)))
               `(quote ,val)
               val))))
    ((and (consp form) (eq (car form) 'quote))
     form)
    ((consp form)
     (cons (subst-for-eval (car form) env)
           (subst-for-eval (cdr form) env)))
    (t form)))

(defun eval-lisp-condition (condition env)
  "Evaluate a Lisp condition embedded in Prolog rules."
  (handler-case
      (let ((substituted (subst-for-eval condition env)))
        (typecase substituted
          (cons (our-eval substituted))
          (t substituted)))
    (error () nil)))

(defun %invoke-builtin-goal (predicate args env k)
  "Invoke the built-in predicate handler for PREDICATE, if one exists."
  (let ((builtin (gethash predicate *builtin-predicates*)))
    (when builtin
      (funcall builtin args env k)
      t)))

(eval-when (:load-toplevel :execute)
  (setf *builtin-predicates*
        (make-builtin-predicate-table *builtin-predicate-specs*)))
