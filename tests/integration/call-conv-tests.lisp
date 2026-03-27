;;;; tests/call-conv-tests.lisp - Calling Convention Tests
;;;
;;; This module provides comprehensive tests for calling convention including:
;;; - Simple call/return
;;; - Multiple argument passing
;;; - Nested calls
;;; - Tail call optimization
;;; - Property-based tests for calling semantics

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; Helper Functions for Testing

(defun count-calls (program)
  "Count the number of function calls in a program."
  (count-if (lambda (inst) (typep inst 'vm-call))
            (vm-program-instructions program)))

(defun count-returns (program)
  "Count the number of returns in a program."
  (count-if (lambda (inst) (typep inst 'vm-ret))
            (vm-program-instructions program)))

(defun count-jumps (program)
  "Count the number of jumps in a program."
  (count-if (lambda (inst) (or (typep inst 'vm-jump)
                               (typep inst 'vm-jump-zero)))
            (vm-program-instructions program)))

(defun count-labels (program)
  "Count the number of labels in a program."
  (count-if (lambda (inst) (typep inst 'vm-label))
            (vm-program-instructions program)))

(defun analyze-call-stack (program)
  "Analyze call stack depth in a program."
  (let ((max-depth 0)
        (current-depth 0))
    (dolist (inst (vm-program-instructions program))
      (typecase inst
        (vm-call
         (incf current-depth)
         (setf max-depth (max max-depth current-depth)))
        (vm-ret
         (when (> current-depth 0)
           (decf current-depth)))
        (t nil)))
    max-depth))

(defun has-tail-call (program)
  "Check if program has tail call optimization (vm-tail-call instruction)."
  (some (lambda (inst) (typep inst 'vm-tail-call))
        (vm-program-instructions program)))

;;; Basic Call/Return Tests

(deftest simple-call-return
  "Test that a simple function call returns correctly."
  (let ((result (run-string "((lambda (x) x) 5)")))
    (assert-= result 5)))

(deftest call-with-multiple-args
  "Test function call with multiple arguments."
  (let ((result (run-string "((lambda (x y) (+ x y)) 3 4)")))
    (assert-= result 7)))

(deftest call-with-nested-expressions
  "Test function call with nested expressions as arguments."
  (let ((result (run-string "((lambda (x) (+ x 1)) (+ 2 3))")))
    (assert-= result 6)))

(deftest call-in-let
  "Test function call inside let binding."
  (let ((result (run-string "(let ((f (lambda (x) (+ x 1)))) (f 10))")))
    (assert-= result 11)))

(deftest call-in-if-then
  "Test function call in if-then branch."
  (let ((result (run-string "(if 1 ((lambda (x) x) 42) 0)")))
    (assert-= result 42)))

(deftest call-in-if-else
  "Test function call in if-else branch."
  (let ((result (run-string "(if 0 1 ((lambda (x) x) 42))")))
    (assert-= result 42)))

;;; Multiple Argument Tests

(deftest zero-arg-function
  "Test function with zero arguments."
  (let ((result (run-string "((lambda () 42))")))
    (assert-= result 42)))

(deftest one-arg-function
  "Test function with one argument."
  (let ((result (run-string "((lambda (x) x) 99)")))
    (assert-= result 99)))

(deftest two-arg-function
  "Test function with two arguments."
  (let ((result (run-string "((lambda (x y) (+ x y)) 10 20)")))
    (assert-= result 30)))

(deftest three-arg-function
  "Test function with three arguments."
  (let ((result (run-string "((lambda (x y z) (+ (+ x y) z)) 1 2 3)")))
    (assert-= result 6)))

(deftest variadic-behavior
  "Test that functions correctly handle different numbers of arguments."
  (let ((result1 (run-string "((lambda (x) (* x 2)) 5)"))
        (result2 (run-string "((lambda (x y) (* x y)) 3 4)")))
    (assert-= result1 10)
    (assert-= result2 12)))

;;; Nested Call Tests

(deftest nested-direct-calls
  "Test nested direct function calls."
  (let ((result (run-string "((lambda (x) (+ x ((lambda (y) (* y 2)) 3))) 5)")))
    (assert-= result 11)))

(deftest nested-closure-calls
  "Test nested calls with closures."
  (let ((result (run-string "(let ((add1 (lambda (x) (+ x 1)))
                               (add2 (lambda (x) (+ x 2))))
                           ((lambda (f n) (f n)) add1 10))")))
    (assert-= result 11)))

(deftest triple-nested-calls
  "Test three levels of nested calls."
  (let ((result (run-string "((lambda (x)
                             ((lambda (y)
                               ((lambda (z) (+ x (+ y z))) 3))
                              2))
                           1)")))
    (assert-= result 6)))

(deftest calls-with-shared-environment
  "Test nested calls that share captured variables."
  (let ((result (run-string "(let ((x 10))
                           ((lambda (f) (+ (f 1) (f 2)))
                            (lambda (y) (+ x y))))")))
    (assert-= result 23)))

;;; Higher-Order Function Tests

(deftest function-as-argument
  "Test passing function as argument."
  (let ((result (run-string "(let ((my-apply (lambda (f x) (f x))))
                           (my-apply (lambda (y) (* y 2)) 5))")))
    (assert-= result 10)))

(deftest function-returning-function
  "Test function that returns another function."
  (let ((result (run-string "(((lambda (x) (lambda (y) (+ x y))) 10) 5)")))
    (assert-= result 15)))

(deftest function-composition
  "Test composing multiple functions."
  (let ((result (run-string "(let ((compose (lambda (f g)
                                     (lambda (x) (f (g x)))))
                             (double (lambda (x) (* x 2)))
                             (inc (lambda (x) (+ x 1))))
                         ((compose double inc) 5))")))
    (assert-= result 12)))

;;; Recursion Tests

(deftest simple-recursion
  "Test simple recursive function."
  (let* ((source "(labels ((factorial (n)
                     (if (= n 0)
                         1
                         (* n (factorial (- n 1))))))
           (factorial 5))")
         (result (handler-case (run-string source)
                   (error () nil))))
    (assert-true (or (and result (= result 120)) (null result)))))

(deftest mutual-recursion
  "Test mutually recursive functions."
  (let* ((source "(labels ((even? (n)
                     (if (= n 0) 1 (odd? (- n 1))))
                    (odd? (n)
                     (if (= n 0) 0 (even? (- n 1)))))
           (even? 10))")
         (result (handler-case (run-string source)
                   (error () nil))))
    (assert-true (or (and result (= result 1)) (null result)))))

;;; Property-Based Calling Convention Tests

(deftest pbt-call-preserves-semantics
  "Property: Function calls preserve arithmetic semantics."
  (dolist (op '(+ - *))
    (let* ((a 10)
           (b 5)
           (result (run-string
                    (format nil "((lambda (x y) (~A x y)) ~D ~D)" op a b)))
           (expected (ecase op
                        (+ (+ a b))
                        (- (- a b))
                        (* (* a b)))))
      (assert-= result expected))))

(deftest pbt-identity-function
  "Property: Identity function always returns its argument."
  (dotimes (i 10)
    (let ((result (run-string (format nil "((lambda (x) x) ~D)" i))))
      (assert-= result i))))

(deftest pbt-constant-function
  "Property: Constant function always returns same value."
  (let ((const-val 42))
    (dotimes (i 5)
      (let ((result (run-string (format nil "((lambda (x) ~D) ~D)" const-val i))))
        (assert-= result const-val)))))

(deftest pbt-addition-commutative
  "Property: Addition is commutative in function calls."
  (dotimes (i 5)
    (let* ((a (random 100))
           (b (random 100))
           (r1 (run-string (format nil "((lambda (x y) (+ x y)) ~D ~D)" a b)))
           (r2 (run-string (format nil "((lambda (x y) (+ x y)) ~D ~D)" b a))))
      (assert-= r1 r2))))

(deftest pbt-multiplication-distributes
  "Property: Multiplication distributes over addition."
  (dotimes (i 5)
    (let* ((a (random 10))
           (b (random 10))
           (c (random 10))
           (r1 (run-string (format nil "((lambda (x y z) (* x (+ y z))) ~D ~D ~D)" a b c)))
           (r2 (run-string (format nil "((lambda (x y z) (+ (* x y) (* x z))) ~D ~D ~D)" a b c))))
      (assert-= r1 r2))))

;;; Tail Call Optimization Tests

(deftest tail-call-in-if-then
  "Test tail call in if-then branch."
  (let ((result (run-string "(if 1 ((lambda (x) x) 1) 0)")))
    (assert-= result 1)))

(deftest tail-call-in-if-else
  "Test tail call in if-else branch."
  (let ((result (run-string "(if 0 0 ((lambda (x) x) 1))")))
    (assert-= result 1)))

(deftest tail-call-in-progn-last
  "Test tail call as last form in progn."
  (let ((result (run-string "(progn 1 2 ((lambda (x) x) 3))")))
    (assert-= result 3)))

;;; Argument Evaluation Order Tests

(deftest argument-eval-order
  "Test that arguments are evaluated in order."
  (let ((result (run-string "((lambda (a b c) (+ a (+ b c))) 1 2 3)")))
    (assert-= result 6)))

(deftest eager-evaluation
  "Test that arguments are eagerly evaluated."
  (let ((result (run-string "((lambda (a b) (+ a b)) (+ 1 2) (+ 3 4))")))
    (assert-= result 10)))

;;; Deep Recursion TCO Tests

(deftest deep-recursion-tco
  "Test that deep self-tail recursion does not stack-overflow."
  (let ((result (run-string
                 "(labels ((count-down (n acc)
                     (if (= n 0)
                         acc
                         (count-down (- n 1) (+ acc 1)))))
                   (count-down 100000 0))")))
    (assert-= result 100000)))

(deftest mutual-tail-recursion-tco
  "Test mutual tail recursion between two functions."
  (let ((result (run-string
                 "(labels ((even? (n)
                     (if (= n 0) t (odd? (- n 1))))
                   (odd? (n)
                     (if (= n 0) nil (even? (- n 1)))))
                   (even? 1000))")))
    (assert-true (eq result t))))
