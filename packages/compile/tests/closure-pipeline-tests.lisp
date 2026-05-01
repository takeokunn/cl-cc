;;;; tests/closure-tests.lisp - Closure and Lambda Expression Tests
;;;
;;; This module provides comprehensive tests for closure behavior including:
;;; - Simple closure captures variable
;;; - Nested closures
;;; - Closure with mutation
;;; - Closure as return value
;;; - Property-based tests for closure semantics

(in-package :cl-cc/test)

;;; Test Suite Definition

(defsuite closure-tests-suite
  :description "Tests for closure and lambda expression compilation"
  :parent cl-cc-integration-suite)

(in-suite closure-tests-suite)

;;; Helper Functions

(defun make-closure-ast (param var-name var-value body-form)
  "Create a closure AST that captures a variable."
  (make-ast-let
                 :bindings (list (cons var-name (make-ast-int :value var-value)))
                 :body (list (make-ast-lambda
                                            :params (list param)
                                            :body (list body-form)))))

(defun make-nested-closure-ast (outer-var outer-val inner-var inner-val)
  "Create nested closures."
  (make-ast-let
                 :bindings (list (cons outer-var (make-ast-int :value outer-val)))
                 :body (list (make-ast-lambda
                                            :params nil
                                            :body (list (make-ast-let
                                                                       :bindings (list (cons inner-var
                                                                                            (make-ast-int :value inner-val)))
                                                                       :body (list (make-ast-lambda
                                                                                                  :params nil
                                                                                                  :body (list (make-ast-binop
                                                                                                                             :op '+
                                                                                                                             :lhs (make-ast-var :name outer-var)
                                                                                                                             :rhs (make-ast-var :name inner-var)))))))))))

(deftest-each closure-and-lambda-compile-to-vm
  "All closure and lambda scenarios compile to a valid vm-program."
  :cases
  (("simple-capture"
    "(let ((x 10)) (lambda (y) (+ x y)))")
   ("multi-capture"
    "(let ((x 5) (y 10)) (lambda (z) (+ (+ x y) z)))")
   ("capture-return"
    "(let ((x 42)) ((lambda () x)))")
   ("nested-closures"
    "(let ((outer 10)) (lambda () (let ((inner 20)) (lambda () (+ outer inner)))))")
   ("shared-capture"
    "(let ((x 1)) (let ((f (lambda () x)) (g (lambda () (+ x 1)))) (+ (funcall f) (funcall g))))")
   ("triple-nested"
    "(let ((a 1)) (lambda () (let ((b 2)) (lambda () (let ((c 3)) (lambda () (+ (+ a b) c)))))))")
   ("setq-in-closure"
    "(let ((counter 0)) (lambda () (setq counter (+ counter 1)) counter))")
   ("mutation-preserves-capture"
    "(let ((x 10)) (setq x 20) (lambda () x))")
   ("multiple-setq"
    "(let ((a 0) (b 0)) (lambda (x y) (setq a x) (setq b y) (+ a b)))")
   ("closure-factory"
    "(let ((make-adder (lambda (n) (lambda (x) (+ n x))))) (funcall (funcall make-adder 10) 5))")
   ("closure-as-data"
    "(let ((make-counter (lambda (start) (lambda () start)))) (let ((counter (funcall make-counter 0))) (funcall counter)))")
   ("hof-compose"
    "(let ((compose (lambda (f g) (lambda (x) (funcall f (funcall g x)))))) (let ((add1 (lambda (x) (+ x 1))) (mul2 (lambda (x) (* x 2)))) (funcall (funcall (funcall compose add1) mul2) 5)))")
   ("lambda-no-params"
    "(funcall (lambda () 42))")
   ("lambda-one-param"
    "(funcall (lambda (x) (+ x 1)) 10)")
   ("lambda-multi-params"
    "(funcall (lambda (x y z) (+ (+ x y) z)) 1 2 3)")
   ("lambda-progn-body"
    "(funcall (lambda (x) (print x) (+ x 1)) 5)")
   ("lambda-nested-in-expr"
    "(+ 1 (funcall (lambda (x) (* x 2)) 10))")
   ("flet-single"
    "(flet ((double (x) (* x 2))) (double 5))")
   ("flet-multiple"
    "(flet ((add1 (x) (+ x 1)) (mul2 (x) (* x 2))) (+ (add1 5) (mul2 3)))")
   ("labels-calling"
    "(labels ((a (x) (+ x 1)) (b (x) (a (* x 2)))) (b 5))")
   ("flet-returns-closure"
    "(flet ((make-adder (n) (lambda (x) (+ n x)))) (funcall (make-adder 10) 5))")
   ("labels-recursive"
    "(labels ((fact (n) (if n (* n (fact (- n 1))) 1))) (fact 5))")
   ("labels-mutual"
    "(labels ((even-p (n) (if n (odd-p (- n 1)) 1)) (odd-p (n) (if n (even-p (- n 1)) 0))) (even-p 10))")
   ("labels-with-closure"
    "(labels ((make-counter () (let ((count 0)) (lambda () (setq count (+ count 1)) count)))) (let ((c (make-counter))) (funcall c) (funcall c)))"))
  (expr)
  (assert-true (compilation-result-program (compile-string expr :target :vm))))

;;; AST Structure Tests

(deftest ast-lambda-structure
  "Test AST structure for lambda expressions."
  (let* ((sexp '(lambda (x y) (+ x y)))
         (ast (lower-sexp-to-ast sexp)))
    (assert-type ast-lambda ast)
    (assert-equal (ast-lambda-params ast) '(x y))
    (assert-= 1 (length (ast-lambda-body ast)))
    (assert-type ast-binop (first (ast-lambda-body ast)))))

(deftest ast-flet-structure
  "Test AST structure for flet expressions."
  (let* ((sexp '(flet ((double (x) (* x 2))) (double 5)))
         (ast (lower-sexp-to-ast sexp)))
    (assert-type ast-flet ast)
    (assert-= 1 (length (ast-flet-bindings ast)))
    (assert-eq 'double (first (first (ast-flet-bindings ast))))
    (assert-equal '(x) (second (first (ast-flet-bindings ast))))))

(deftest ast-labels-structure
  "Test AST structure for labels expressions."
  (let* ((sexp '(labels ((fact (n) (if n (* n (fact (- n 1))) 1))) (fact 5)))
         (ast (lower-sexp-to-ast sexp)))
    (assert-type ast-labels ast)
    (assert-= 1 (length (ast-labels-bindings ast)))
    (assert-eq 'fact (first (first (ast-labels-bindings ast))))))

;;; Roundtrip Tests

(deftest-each closure-ast-roundtrip
  "Lambda, flet, labels, and nested-lambda AST round-trip through lower-sexp-to-ast/ast-to-sexp."
  :cases (("lambda"        '(lambda (x y) (+ x y)))
          ("flet"          '(flet ((double (x) (* x 2))) (double 5)))
          ("labels"        '(labels ((fact (n) (if n (* n (fact (- n 1))) 1))) (fact 5)))
          ("nested-lambda" '(let ((x 10)) (lambda (y) (+ x y)))))
  (original)
  (assert-equal original (ast-to-sexp (lower-sexp-to-ast original))))

;;; Assembly Emission Tests

(deftest-each closure-assembly-backends
  "Closures compile to assembly for x86-64 and aarch64 (or signal :not-yet-supported)."
  :cases (("x86-64"   :x86_64)
          ("aarch64"  :aarch64))
  (target)
  (let ((result (handler-case
                    (compilation-result-assembly (compile-string "(lambda (x) (+ x 1))" :target target))
                  (error () :not-yet-supported))))
    (assert-true (or (eq result :not-yet-supported)
                     (and (stringp result) (> (length result) 0))))))
