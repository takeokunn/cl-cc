(in-package :cl-cc)

;;; AST-Based CPS Transformation

(defgeneric cps-transform-ast (node k)
  (:documentation "Transform an AST node to continuation-passing style.
NODE is an AST node, K is the continuation (a symbol or lambda expression).
Returns a CPS-transformed S-expression."))

;;; Helper: Transform a sequence of forms, passing result to continuation
(defun %cps-transform-sequence-step (forms k empty-result)
  "Shared CPS sequence walker used by sequence-like special forms."
  (if (null forms)
      empty-result
      (let ((tmp (gensym "SEQ")))
        (if (null (cdr forms))
            (cps-transform-ast (car forms) k)
            (cps-transform-ast (car forms)
                               `(lambda (,tmp)
                                  (declare (ignore ,tmp))
                                  ,(%cps-transform-sequence-step
                                    (cdr forms) k empty-result)))))))

(defun cps-transform-sequence (forms k)
  "Transform a sequence of forms in CPS. Returns the value of the last form."
  (%cps-transform-sequence-step forms k `(funcall ,k nil)))

;;; Core AST Types

(defmethod cps-transform-ast ((node ast-int) k)
  `(funcall ,k ,(ast-int-value node)))

(defmethod cps-transform-ast ((node ast-var) k)
  `(funcall ,k ,(ast-var-name node)))

(defmethod cps-transform-ast ((node ast-binop) k)
  (let ((op (ast-binop-op node))
        (lhs (ast-binop-lhs node))
        (rhs (ast-binop-rhs node))
        (va (gensym "A"))
        (vb (gensym "B")))
    (cps-transform-ast lhs
                       (list 'lambda (list va)
                             (cps-transform-ast rhs
                                                (list 'lambda (list vb)
                                                      (list 'funcall k
                                                            (list op va vb))))))))

(defmethod cps-transform-ast ((node ast-if) k)
  (let ((v (gensym "COND"))
        (k-var (gensym "K")))
    `(let ((,k-var ,k))
       ,(cps-transform-ast (ast-if-cond node)
                           (list 'lambda (list v)
                                 (list 'if (list '%cps-falsep v)
                                       (cps-transform-ast (ast-if-else node) k-var)
                                       (cps-transform-ast (ast-if-then node) k-var)))))))

(defmethod cps-transform-ast ((node ast-progn) k)
  (cps-transform-sequence (ast-progn-forms node) k))

(defmethod cps-transform-ast ((node ast-print) k)
  (let ((v (gensym "PRINT")))
    (cps-transform-ast (ast-print-expr node)
                       (list 'lambda (list v)
                             (list 'progn
                                   (list 'print v)
                                   (list 'funcall k v))))))

(defmethod cps-transform-ast ((node ast-let) k)
  (let ((bindings (ast-let-bindings node))
        (body (ast-let-body node)))
    (labels ((expand-bindings (rest env-k)
               (if (null rest)
                   (cps-transform-sequence body env-k)
                   (let* ((binding (car rest))
                          (sym (car binding))
                          (val (cdr binding))
                          (tmp (gensym (symbol-name sym))))
                     (cps-transform-ast val
                                        `(lambda (,tmp)
                                           (let ((,sym ,tmp))
                                             ,(expand-bindings (cdr rest) env-k))))))))
      (expand-bindings bindings k))))

;;; Lambda and Closures

(defmethod cps-transform-ast ((node ast-lambda) k)
  "Transform a lambda expression to CPS. The continuation becomes an extra parameter."
  (let* ((params (ast-lambda-params node))
         (body   (ast-lambda-body node))
         (k-var  (gensym "K")))
    `(funcall ,k
              (lambda (,@params ,k-var)
                ,(cps-transform-sequence body k-var)))))

(defmethod cps-transform-ast ((node ast-function) k)
  "Transform #'var to CPS (function reference)."
  `(funcall ,k (function ,(ast-function-name node))))

;;; Control-flow and non-local CPS transforms moved to cps-ast-control.lisp.
