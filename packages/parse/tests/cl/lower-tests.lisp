;;;; tests/unit/parse/cl/lower-tests.lisp — CL lowering unit tests

(in-package :cl-cc/test)

(defsuite cl-lower-suite
  :description "Lowering tests for parse/cl/lower.lisp"
  :parent cl-cc-unit-suite)

(in-suite cl-lower-suite)

(defun lower (sexp)
  "Lower an s-expression to an AST node."
  (cl-cc::lower-sexp-to-ast sexp))

(deftest lower-integer-produces-ast-int
  "lower-sexp-to-ast: integer -> ast-int with correct value."
  (let ((node (lower 42)))
    (assert-true (cl-cc::ast-int-p node))
    (assert-= 42 (cl-cc::ast-int-value node))))

(deftest lower-unary-minus-becomes-negation
  "lower-sexp-to-ast: unary minus is lowered as 0 - x."
  (let ((node (lower '(- 7))))
    (assert-true (cl-cc::ast-binop-p node))
    (assert-eq '- (cl-cc::ast-binop-op node))
    (assert-= 0 (cl-cc::ast-int-value (cl-cc::ast-binop-lhs node)))
    (assert-= 7 (cl-cc::ast-int-value (cl-cc::ast-binop-rhs node)))))

(deftest lower-if-form
  "lower-sexp-to-ast: if form -> ast-if."
  (let ((node (lower '(if x 1 2))))
    (assert-true (cl-cc::ast-if-p node))
    (assert-eq 'x (cl-cc::ast-var-name (cl-cc::ast-if-cond node)))))

(deftest lower-setq-multi-var
  "lower-sexp-to-ast: multi-var setq -> ast-progn of setq forms."
  (let ((node (lower '(setq a 1 b 2))))
    (assert-true (cl-cc::ast-progn-p node))
    (assert-= 2 (length (cl-cc::ast-progn-forms node)))))
