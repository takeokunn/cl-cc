(in-package :cl-cc/compile)

;;; AST-Based CPS Transformation
;;;
;;; The CPS transform converts AST nodes into continuation-passing style:
;;; every expression evaluation is threaded through lambda continuations.
;;; This enables proper handling of control flow (exceptions, blocks, go)
;;; and is the foundation for tail-call optimization and advanced transforms.
;;;
;;; Architecture:
;;;   cps-ast.lisp           — Generic function, sequence walker, core types
;;;   cps-ast-control.lisp   — block/return-from, tagbody/go, catch/throw,
;;;                             unwind-protect, flet/labels
;;;   cps-ast-extended.lisp  — OOP/mutation: setq, defvar, handler-case,
;;;                             make-instance, slot-value, defclass, defgeneric,
;;;                             defmethod, set-gethash + helper functions
;;;   cps-ast-functional.lisp — quote, the, values, mvb, apply, mvc, mvprog1,
;;;                             ast-call, entry points

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
                               (list 'lambda (list tmp)
                                     (list 'declare (list 'ignore tmp))
                                     (%cps-transform-sequence-step
                                      (cdr forms) k empty-result)))))))

(defun cps-transform-sequence (forms k)
  "Transform a sequence of forms in CPS. Returns the value of the last form."
  (%cps-transform-sequence-step forms k (list 'funcall k nil)))

;;; Core AST Types

(defmethod cps-transform-ast ((node ast-int) k)
  (list 'funcall k (ast-int-value node)))

(defmethod cps-transform-ast ((node ast-var) k)
  (list 'funcall k (ast-var-name node)))

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
    (list 'let (list (list k-var k))
          (cps-transform-ast (ast-if-cond node)
                             (list 'lambda (list v)
                                    (list 'if (list 'or
                                                    (list 'null v)
                                                    (list 'and (list 'numberp v)
                                                          (list 'zerop v)))
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
                                        (list 'lambda (list tmp)
                                              (list 'let (list (list sym tmp))
                                                    (expand-bindings (cdr rest) env-k))))))))
      (expand-bindings bindings k))))

;;; Lambda and Closures

(defmethod cps-transform-ast ((node ast-lambda) k)
  "Transform a lambda expression to CPS. The continuation becomes an extra parameter."
  (let* ((params (ast-lambda-params node))
         (optional (ast-lambda-optional-params node))
         (rest-param (ast-lambda-rest-param node))
         (key-params (ast-lambda-key-params node))
         (decls (ast-lambda-declarations node))
         (body   (ast-lambda-body node))
         (k-var  (gensym "K")))
    (if (or optional rest-param key-params)
        (%cps-funcall k
                      (append (list 'lambda
                                    (%cps-extended-lambda-list params optional rest-param key-params))
                              decls
                              (mapcar #'ast-to-sexp body)))
        (list 'funcall k
              (cons 'lambda
                    (cons (append params (list k-var))
                          (list (cps-transform-sequence body k-var))))))))

(defmethod cps-transform-ast ((node ast-function) k)
  "Transform #'var to CPS (function reference)."
  (list 'funcall k (list 'function (ast-function-name node))))

;;; Control-flow and non-local CPS transforms moved to cps-ast-control.lisp.
