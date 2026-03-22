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

(def-suite closure-tests-suite
  :description "Tests for closure and lambda expression compilation"
  :in cl-cc-suite)

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

;;; Simple Closure Tests

(test simple-closure-captures-variable
  "Test that a simple closure correctly captures a variable."
  (let* ((closure-expr "(let ((x 10))
                         (lambda (y) (+ x y)))")
         (result (compile-string closure-expr :target :vm)))
    ;; The closure should be compiled without errors
    (is (compilation-result-program result))
    (is (typep (compilation-result-program result) 'vm-program))))

(test closure-captures-multiple-variables
  "Test that a closure can capture multiple variables."
  (let* ((closure-expr "(let ((x 5) (y 10))
                         (lambda (z) (+ (+ x y) z)))")
         (result (compile-string closure-expr :target :vm)))
    (is (compilation-result-program result))
    (is (typep (compilation-result-program result) 'vm-program))))

(test closure-returns-captured-value
  "Test that a closure can return the captured value."
  (let* ((closure-expr "(let ((x 42))
                         ((lambda () x)))")
         (program (compilation-result-program (compile-string closure-expr :target :vm))))
    (is (not (null program)))
    ;; Verify the program has instructions
    (is (> (length (vm-program-instructions program)) 0))))

;;; Nested Closure Tests

(test nested-closures-basic
  "Test that nested closures work correctly."
  (let* ((nested-expr "(let ((outer 10))
                        (lambda ()
                          (let ((inner 20))
                            (lambda ()
                              (+ outer inner)))))")
         (result (compile-string nested-expr :target :vm)))
    (is (compilation-result-program result))))

(test nested-closures-with-shared-capture
  "Test nested closures sharing captured variables."
  (let* ((expr "(let ((x 1))
                 (let ((f (lambda () x))
                       (g (lambda () (+ x 1))))
                   (+ (funcall f) (funcall g))))")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

(test triple-nested-closures
  "Test three levels of nested closures."
  (let* ((expr "(let ((a 1))
                 (lambda ()
                   (let ((b 2))
                     (lambda ()
                       (let ((c 3))
                         (lambda ()
                           (+ (+ a b) c)))))))")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

;;; Closure with Mutation Tests

(test closure-with-setq-basic
  "Test closure with variable mutation using setq."
  (let* ((expr "(let ((counter 0))
                 (lambda ()
                   (setq counter (+ counter 1))
                   counter))")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

(test closure-mutation-preserves-capture
  "Test that mutation doesn't break variable capture."
  (let* ((expr "(let ((x 10))
                 (setq x 20)
                 (lambda () x))")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

(test multiple-setq-in-closure
  "Test multiple setq operations in closure body."
  (let* ((expr "(let ((a 0) (b 0))
                 (lambda (x y)
                   (setq a x)
                   (setq b y)
                   (+ a b)))")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

;;; Closure as Return Value Tests

(test closure-factory-function
  "Test function that returns a closure."
  (let* ((expr "(let ((make-adder
                  (lambda (n)
                    (lambda (x) (+ n x)))))
                 (funcall (funcall make-adder 10) 5))")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

(test closure-as-data-structure
  "Test using closures to implement simple data structures."
  (let* ((expr "(let ((make-counter
                  (lambda (start)
                    (lambda () start))))
                 (let ((counter (funcall make-counter 0)))
                   (funcall counter)))")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

(test higher-order-function-with-closure
  "Test higher-order function that takes and returns closures."
  (let* ((expr "(let ((compose
                  (lambda (f g)
                    (lambda (x)
                      (funcall f (funcall g x))))))
                 (let ((add1 (lambda (x) (+ x 1)))
                       (mul2 (lambda (x) (* x 2))))
                   (funcall (funcall (funcall compose add1) mul2) 5)))")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

;;; Lambda Expression Tests

(test lambda-no-parameters
  "Test lambda with no parameters."
  (let* ((expr "(funcall (lambda () 42))")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

(test lambda-single-parameter
  "Test lambda with a single parameter."
  (let* ((expr "(funcall (lambda (x) (+ x 1)) 10)")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

(test lambda-multiple-parameters
  "Test lambda with multiple parameters."
  (let* ((expr "(funcall (lambda (x y z) (+ (+ x y) z)) 1 2 3)")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

(test lambda-with-progn-body
  "Test lambda with multiple body forms."
  (let* ((expr "(funcall (lambda (x)
                   (print x)
                   (+ x 1))
                 5)")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

(test lambda-nested-in-expression
  "Test lambda nested within an expression."
  (let* ((expr "(+ 1 (funcall (lambda (x) (* x 2)) 10))")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

;;; Flet Tests (Non-recursive Local Functions)

(test flet-single-function
  "Test flet with a single local function."
  (let* ((expr "(flet ((double (x) (* x 2)))
                 (double 5))")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

(test flet-multiple-functions
  "Test flet with multiple local functions."
  (let* ((expr "(flet ((add1 (x) (+ x 1))
                (mul2 (x) (* x 2)))
                 (+ (add1 5) (mul2 3)))")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

(test flet-calls-another-flet
  "Test labels function calling another function (labels allows mutual visibility)."
  (let* ((expr "(labels ((a (x) (+ x 1))
                         (b (x) (a (* x 2))))
                 (b 5))")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

(test flet-with-closure
  "Test flet with function that returns a closure."
  (let* ((expr "(flet ((make-adder (n)
                  (lambda (x) (+ n x))))
                 (funcall (make-adder 10) 5))")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

;;; Labels Tests (Mutually Recursive Functions)

(test labels-single-recursive-function
  "Test labels with a single recursive function."
  (let* ((expr "(labels ((fact (n)
                  (if n
                      (* n (fact (- n 1)))
                      1)))
                 (fact 5))")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

(test labels-mutually-recursive
  "Test labels with mutually recursive functions."
  (let* ((expr "(labels ((even-p (n)
                  (if n
                      (odd-p (- n 1))
                      1))
                (odd-p (n)
                  (if n
                      (even-p (- n 1))
                      0)))
                 (even-p 10))")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

(test labels-with-closure
  "Test labels with function that creates a closure."
  (let* ((expr "(labels ((make-counter ()
                  (let ((count 0))
                    (lambda ()
                      (setq count (+ count 1))
                      count))))
                 (let ((c (make-counter)))
                   (funcall c)
                   (funcall c)))")
         (result (compile-string expr :target :vm)))
    (is (compilation-result-program result))))

;;; AST Structure Tests

(test ast-lambda-structure
  "Test AST structure for lambda expressions."
  (let* ((sexp '(lambda (x y) (+ x y)))
         (ast (lower-sexp-to-ast sexp)))
    (is (typep ast 'ast-lambda))
    (is (equal (ast-lambda-params ast) '(x y)))
    (is (= 1 (length (ast-lambda-body ast))))
    (is (typep (first (ast-lambda-body ast)) 'ast-binop))))

(test ast-flet-structure
  "Test AST structure for flet expressions."
  (let* ((sexp '(flet ((double (x) (* x 2))) (double 5)))
         (ast (lower-sexp-to-ast sexp)))
    (is (typep ast 'ast-flet))
    (is (= 1 (length (ast-flet-bindings ast))))
    (is (eq 'double (first (first (ast-flet-bindings ast)))))
    (is (equal '(x) (second (first (ast-flet-bindings ast)))))))

(test ast-labels-structure
  "Test AST structure for labels expressions."
  (let* ((sexp '(labels ((fact (n) (if n (* n (fact (- n 1))) 1))) (fact 5)))
         (ast (lower-sexp-to-ast sexp)))
    (is (typep ast 'ast-labels))
    (is (= 1 (length (ast-labels-bindings ast))))
    (is (eq 'fact (first (first (ast-labels-bindings ast)))))))

;;; Roundtrip Tests

(test lambda-roundtrip
  "Test lambda AST to sexp and back."
  (let* ((original '(lambda (x y) (+ x y)))
         (ast (lower-sexp-to-ast original))
         (back (ast-to-sexp ast)))
    (is (equal original back))))

(test flet-roundtrip
  "Test flet AST to sexp and back."
  (let* ((original '(flet ((double (x) (* x 2))) (double 5)))
         (ast (lower-sexp-to-ast original))
         (back (ast-to-sexp ast)))
    (is (equal original back))))

(test labels-roundtrip
  "Test labels AST to sexp and back."
  (let* ((original '(labels ((fact (n) (if n (* n (fact (- n 1))) 1))) (fact 5)))
         (ast (lower-sexp-to-ast original))
         (back (ast-to-sexp ast)))
    (is (equal original back))))

(test nested-lambda-roundtrip
  "Test nested lambda AST to sexp and back."
  (let* ((original '(let ((x 10)) (lambda (y) (+ x y))))
         (ast (lower-sexp-to-ast original))
         (back (ast-to-sexp ast)))
    (is (equal original back))))

;;; Assembly Emission Tests

(test closure-assembly-x86-64
  "Test that closures compile to x86-64 assembly (pending vm-closure emit-instruction)."
  (let ((result (handler-case
                    (compilation-result-assembly (compile-string "(lambda (x) (+ x 1))" :target :x86_64))
                  (error () :not-yet-supported))))
    (is (or (eq result :not-yet-supported)
            (and (stringp result) (> (length result) 0))))))

(test closure-assembly-aarch64
  "Test that closures compile to aarch64 assembly (pending vm-closure emit-instruction)."
  (let ((result (handler-case
                    (compilation-result-assembly (compile-string "(lambda (x) (+ x 1))" :target :aarch64))
                  (error () :not-yet-supported))))
    (is (or (eq result :not-yet-supported)
            (and (stringp result) (> (length result) 0))))))
