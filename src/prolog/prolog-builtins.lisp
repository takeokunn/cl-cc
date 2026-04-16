(in-package :cl-cc/prolog)

;;; Built-in Predicate Dispatch Table (data layer)
;;;
;;; Each handler is (lambda (args env k)) — continuation-passing style:
;;; args = predicate arguments, env = current bindings, k = success continuation.

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

(defun %goal-predicate-and-args (goal)
  "Return GOAL's predicate and arg list.
GOAL may be a prolog-goal object or a raw list form."
  (if (prolog-goal-p goal)
      (values (goal-predicate goal) (goal-args goal))
      (values (car goal) (cdr goal))))

(defun %atomic-cut-goal-p (goal)
  "Return true when GOAL denotes the cut operator as a bare atom."
  (and (symbolp goal)
       (not (prolog-goal-p goal))
       (string= (symbol-name goal) "!")))

(defun %invoke-builtin-goal (predicate args env k)
  "Invoke the built-in predicate handler for PREDICATE, if one exists."
  (let ((builtin (gethash predicate *builtin-predicates*)))
    (when builtin
      (funcall builtin args env k)
      t)))

(defun %solve-prolog-rule (rule args env k)
  "Attempt to solve RULE against ARGS and call K for each matching environment."
  (let* ((fresh-rule (rename-variables rule))
         (head (rule-head fresh-rule))
         (body (rule-body fresh-rule))
         (new-env (unify args (cdr head) env)))
    (unless (unify-failed-p new-env)
      (handler-case
          (let ((result (if body
                            (solve-conjunction body new-env k)
                            (funcall k new-env))))
            (when (eq result :cut)
              (return-from %solve-prolog-rule :cut)))
        (prolog-cut ()
          (return-from %solve-prolog-rule :cut))))))

(defun %make-builtin-predicates ()
  "Build the built-in predicate dispatch hash table from *builtin-predicate-specs*.
*builtin-predicate-specs* is defined in prolog-data.lisp, which loads before this file."
  (let ((ht (make-hash-table :test 'eq)))
    (dolist (spec *builtin-predicate-specs* ht)
      (destructuring-bind (predicate handler) spec
        (setf (gethash predicate ht) (symbol-function handler))))))

(defvar *builtin-predicates* nil
  "Hash table mapping built-in predicate symbols to CPS handler functions.")

(eval-when (:load-toplevel :execute)
  (setf *builtin-predicates* (%make-builtin-predicates)))
