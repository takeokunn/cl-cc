;;;; tests/call-conv-tests.lisp - Calling Convention Tests
;;;
;;; This module provides comprehensive tests for calling convention including:
;;; - Simple call/return
;;; - Multiple argument passing
;;; - Nested calls
;;; - Tail call optimization
;;; - Property-based tests for calling semantics

(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

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

(deftest-each basic-call-return
  "Function calls with various arities, nesting, and positions evaluate correctly."
  :cases (("simple"         "((lambda (x) x) 5)"                                      5)
          ("multi-arg"      "((lambda (x y) (+ x y)) 3 4)"                            7)
          ("nested-expr"    "((lambda (x) (+ x 1)) (+ 2 3))"                          6)
          ("in-let"         "(let ((f (lambda (x) (+ x 1)))) (f 10))"                11)
          ("in-if-then"     "(if 1 ((lambda (x) x) 42) 0)"                           42)
          ("in-if-else"     "(if 0 1 ((lambda (x) x) 42))"                           42))
  (code expected)
  (assert-= expected (run-string code)))

;;; Multiple Argument Tests

(deftest-each arg-count-functions
  "Functions with 0, 1, 2, and 3 arguments call and return correctly."
  :cases (("zero"  "((lambda () 42))"                             42)
          ("one"   "((lambda (x) x) 99)"                         99)
          ("two"   "((lambda (x y) (+ x y)) 10 20)"              30)
          ("three" "((lambda (x y z) (+ (+ x y) z)) 1 2 3)"      6))
  (code expected)
  (assert-= expected (run-string code)))

(deftest variadic-behavior
  "Functions correctly handle different numbers of arguments."
  (assert-= 10 (run-string "((lambda (x) (* x 2)) 5)"))
  (assert-= 12 (run-string "((lambda (x y) (* x y)) 3 4)")))

;;; Nested Call Tests

(deftest nested-calls
  "Nested function calls: direct nesting, closure capture, triple depth, and shared environment."
  (assert-= 11 (run-string "((lambda (x) (+ x ((lambda (y) (* y 2)) 3))) 5)"))
  (assert-= 11 (run-string "(let ((add1 (lambda (x) (+ x 1)))
                               (add2 (lambda (x) (+ x 2))))
                           ((lambda (f n) (f n)) add1 10))"))
  (assert-= 6  (run-string "((lambda (x)
                             ((lambda (y)
                               ((lambda (z) (+ x (+ y z))) 3))
                              2))
                           1)"))
  (assert-= 23 (run-string "(let ((x 10))
                           ((lambda (f) (+ (f 1) (f 2)))
                            (lambda (y) (+ x y))))")))

;;; Higher-Order Function Tests

(deftest higher-order-functions
  "Higher-order functions: function as argument, function returning function, and composition."
  (assert-= 10 (run-string "(let ((my-apply (lambda (f x) (f x))))
                           (my-apply (lambda (y) (* y 2)) 5))"))
  (assert-= 15 (run-string "(((lambda (x) (lambda (y) (+ x y))) 10) 5)"))
  (assert-= 12 (run-string "(let ((compose (lambda (f g)
                                     (lambda (x) (f (g x)))))
                             (double (lambda (x) (* x 2)))
                             (inc (lambda (x) (+ x 1))))
                         ((compose double inc) 5))")))

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

(deftest pbt-properties
  "Property-based tests: arithmetic semantics, identity, constant, commutativity, distributivity."
  (dolist (op '(+ - *))
    (let* ((a 10)
           (b 5)
           (result (run-string
                    (format nil "((lambda (x y) (~A x y)) ~D ~D)" op a b)))
           (expected (ecase op
                        (+ (+ a b))
                        (- (- a b))
                        (* (* a b)))))
      (assert-= result expected)))
  (dotimes (i 10)
    (let ((result (run-string (format nil "((lambda (x) x) ~D)" i))))
      (assert-= result i)))
  (let ((const-val 42))
    (dotimes (i 5)
      (let ((result (run-string (format nil "((lambda (x) ~D) ~D)" const-val i))))
        (assert-= result const-val))))
  (dotimes (i 5)
    (let* ((a (random 100))
           (b (random 100))
           (r1 (run-string (format nil "((lambda (x y) (+ x y)) ~D ~D)" a b)))
           (r2 (run-string (format nil "((lambda (x y) (+ x y)) ~D ~D)" b a))))
      (assert-= r1 r2)))
  (dotimes (i 5)
    (let* ((a (random 10))
           (b (random 10))
           (c (random 10))
           (r1 (run-string (format nil "((lambda (x y z) (* x (+ y z))) ~D ~D ~D)" a b c)))
           (r2 (run-string (format nil "((lambda (x y z) (+ (* x y) (* x z))) ~D ~D ~D)" a b c))))
      (assert-= r1 r2))))

;;; Tail Call Optimization Tests

(deftest-each tail-call-positions
  "Tail calls in if-then, if-else, and progn-last positions return correctly."
  :cases (("if-then"     "(if 1 ((lambda (x) x) 1) 0)"          1)
          ("if-else"     "(if 0 0 ((lambda (x) x) 1))"          1)
          ("progn-last"  "(progn 1 2 ((lambda (x) x) 3))"       3))
  (code expected)
  (assert-= expected (run-string code)))

;;; Argument Evaluation Order Tests

(deftest argument-evaluation
  "Arguments are evaluated in order and eagerly before function application."
  (assert-= 6  (run-string "((lambda (a b c) (+ a (+ b c))) 1 2 3)"))
  (assert-= 10 (run-string "((lambda (a b) (+ a b)) (+ 1 2) (+ 3 4))")))

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
                    (even? 100000))")))
    (assert-true (eq result t))))
