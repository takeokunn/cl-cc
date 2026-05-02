;;;; packages/compile/src/codegen-fold-eval.lisp — Compile-time Partial Evaluator Engine
;;;
;;; Contains: %compile-time-eval-binop, %compile-time-eval-call,
;;;           %evaluate-ast-sequence, %evaluate-ast.
;;;
;;; Data tables and helpers (%evaluate-ast-then, %fold-ast-binop,
;;;   *compile-time-eval-fns*, *compile-time-simple-binops*, etc.)
;;;   are in codegen-fold.lisp.
;;;
;;; Load order: after codegen-fold.lisp, before codegen-fold-optimize.lisp.

(in-package :cl-cc/compile)

(defun %compile-time-eval-binop (op lhs rhs)
  "Evaluate OP on compile-time constant LHS and RHS; return nil if not foldable."
  (let ((simple (assoc op *compile-time-simple-binops*)))
    (cond
      (simple (funcall (symbol-function (cdr simple)) lhs rhs))
      ((eq op '/) (let ((q (and (not (zerop rhs)) (/ lhs rhs))))
                    (when (integerp q) q)))
      ((eq op '1+) (when (and (integerp lhs) (null rhs)) (1+ lhs)))
      ((eq op '1-) (when (and (integerp lhs) (null rhs)) (1- lhs))))))

(defun %compile-time-eval-call (func args depth)
  (cond
    ((and (typep func 'ast-var)
           (= (length args) 1)
           (string= (symbol-name (ast-var-name func)) "STRING-LENGTH")
           (stringp (first args)))
      (values (length (first args)) t))
    ((and (typep func 'ast-var)
          (gethash (ast-var-name func) *compile-time-eval-fns*))
     (let ((fn (gethash (ast-var-name func) *compile-time-eval-fns*)))
       (funcall fn args)))
    ((and (typep func 'ast-var)
          (<= 0 depth)
          (multiple-value-bind (entry found-p)
              (%compile-time-lookup (ast-var-name func) *compile-time-function-env*)
            (declare (ignore entry))
            found-p))
     (multiple-value-bind (defun-node found-p)
         (%compile-time-lookup (ast-var-name func) *compile-time-function-env*)
       (when (and found-p (typep defun-node 'ast-defun)
                  (null (ast-defun-optional-params defun-node))
                  (null (ast-defun-rest-param defun-node))
                  (null (ast-defun-key-params defun-node)))
         (let ((param-bindings (mapcar #'cons (ast-defun-params defun-node) args)))
           (%evaluate-ast-sequence (ast-defun-body defun-node)
                                   param-bindings
                                   *compile-time-function-env*
                                   (1- depth))))))
    ((typep func 'ast-lambda)
     (let ((param-bindings (mapcar #'cons (ast-lambda-params func) args)))
       (%evaluate-ast-sequence (ast-lambda-body func)
                               param-bindings
                               *compile-time-function-env*
                               (1- depth))))
    (t nil)))

(defun %evaluate-ast-sequence (forms value-env function-env depth)
  "Evaluate FORMS left-to-right under VALUE-ENV and FUNCTION-ENV.
Returns (values last-value t) on success, (values nil nil) on first failure."
  (when (null forms)
    (return-from %evaluate-ast-sequence (values nil t)))
  (let ((*compile-time-value-env* value-env)
        (*compile-time-function-env* function-env))
    (let ((last-value nil))
      (dolist (form forms (values last-value t))
        (multiple-value-bind (value ok)
            (%evaluate-ast form depth)
          (unless ok
            (return-from %evaluate-ast-sequence (values nil nil)))
          (setf last-value value))))))

(defun %evaluate-ast (node depth)
  (when (minusp depth)
    (return-from %evaluate-ast (values nil nil)))
  (typecase node
    (ast-int   (values (ast-int-value node) t))
    (ast-quote (values (ast-quote-value node) t))
    (ast-var
     (multiple-value-bind (value found-p)
         (%compile-time-lookup (ast-var-name node) *compile-time-value-env*)
       (if found-p (values value t) (values nil nil))))
    (ast-binop
     (%evaluate-ast-then
      (ast-binop-lhs node) depth
      (lambda (lhs)
        (%evaluate-ast-then
         (ast-binop-rhs node) depth
         (lambda (rhs)
           (let ((value (%compile-time-eval-binop (ast-binop-op node) lhs rhs)))
             (if (and value (integerp value))
                 (values value t)
                 (values nil nil))))))))
    (ast-if
     (%evaluate-ast-then
      (ast-if-cond node) depth
      (lambda (cond-value)
        (%evaluate-ast (if (opt-falsep cond-value)
                           (ast-if-else node)
                           (ast-if-then node))
                       (1- depth)))))
    (ast-progn
     (%evaluate-ast-sequence (ast-progn-forms node)
                              *compile-time-value-env*
                              *compile-time-function-env*
                              (1- depth)))
    (ast-block
     (let ((tag (gensym "BLOCK-")))
       (let ((*compile-time-block-env* (acons (ast-block-name node)
                                              tag
                                              *compile-time-block-env*)))
         (catch tag
           (%evaluate-ast-sequence (ast-block-body node)
                                   *compile-time-value-env*
                                   *compile-time-function-env*
                                   (1- depth))))))
    (ast-return-from
     (multiple-value-bind (tag found-p)
         (%compile-time-lookup (ast-return-from-name node) *compile-time-block-env*)
       (if found-p
           (%evaluate-ast-then
            (ast-return-from-value node) depth
            (lambda (value) (throw tag value)))
           (values nil nil))))
    (ast-let
     (let ((bindings nil))
       (dolist (binding (ast-let-bindings node))
         (multiple-value-bind (value ok)
             (%evaluate-ast (cdr binding) (1- depth))
           (unless ok (return-from %evaluate-ast (values nil nil)))
           (push (cons (car binding) value) bindings)))
       (%evaluate-ast-sequence (ast-let-body node)
                               (append (nreverse bindings) *compile-time-value-env*)
                               *compile-time-function-env*
                               (1- depth))))
    (ast-the
     (%evaluate-ast (ast-the-value node) (1- depth)))
    (ast-call
     (let* ((func (ast-call-func node))
            (args nil))
       (dolist (arg (ast-call-args node))
         (multiple-value-bind (value ok)
             (%evaluate-ast arg (1- depth))
           (unless ok (return-from %evaluate-ast (values nil nil)))
           (push value args)))
       (%compile-time-eval-call func (nreverse args) depth)))
    (t (values nil nil))))
