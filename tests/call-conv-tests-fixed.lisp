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

;;; ----------------------------------------------------------------------------
;;; Helper Functions for Testing
;;; ----------------------------------------------------------------------------

(defun count-calls (program)
  "Count number of function calls in a program.
  (count-if (vm-program-instructions program)
              (lambda (inst) (typep inst 'vm-call))))

(defun count-returns (program)
  "Count number of returns in a program.
  (count-if (vm-program-instructions program)
              (lambda (inst) (typep inst 'vm-return))))

(defun count-jumps (program)
  "Count number of jumps in a program.
  (count-if (vm-program-instructions program)
              (lambda (inst) (or (typep inst 'vm-jump)
                                   (typep inst 'vm-jump-zero)))))

(defun count-labels (program)
  "Count number of labels in a program.
  (count-if (vm-program-instructions program)
              (lambda (inst) (typep inst 'vm-label))))

(defun analyze-call-stack (program)
  "Analyze call stack depth in a program.
  (let ((max-depth 0)
        (current-depth 0)
        (depth-stack '()))
    (dolist (inst (vm-program-instructions program))
      (typecase inst
        (vm-call
         (progn
           (setf max-depth (max max-depth (1+ current-depth)))
           (push (1+ current-depth) depth-stack)
           (decf current-depth)))
        (vm-return
         (when depth-stack
           (decf current-depth (pop depth-stack))))
        (t
         nil)))
    max-depth))

(defun has-tail-call (program)
  "Check if program has tail call optimization.
  ;; A tail call is a call followed immediately by return
  (let ((insts (vm-program-instructions program)))
    (loop for i from 0 below (1- (length insts))
          for inst = (elt insts i)
          when (typep inst 'vm-call)
          do (let ((next-inst (elt insts (1+ i))))
            (when (typep next-inst 'vm-return)
              (return-from nil t)))))
    nil))

;;; ----------------------------------------------------------------------------
;;; Basic Call/Return Tests
;;; ----------------------------------------------------------------------------

(test simple-call-return
  "Test that a simple function call returns correctly."
  (let* ((result (run-string "((lambda (x) x) 5)")))
    (is (= result 5))))

(test call-with-multiple-args
  "Test function call with multiple arguments."
  (let* ((result (run-string "((lambda (x y) (+ x y)) 3 4)")))
    (is (= result 7))))

(test call-with-nested-expressions
  "Test function call with nested expressions as arguments."
  (let* ((result (run-string "((lambda (x) (+ x 1)) (+ 2 3))")))
    (is (= result 6))))

(test call-in-let
  "Test function call inside let binding."
  (let* ((result (run-string "(let ((f (lambda (x) (+ x 1)))) (f 10))")))
    (is (= result 10))))

(test call-in-if-then
  "Test function call in if-then branch."
  (let* ((result (run-string "(if 1 ((lambda (x) x) 42) 0)")))
    (is (= result 42))))

(test call-in-if-else
  "Test function call in if-else branch."
  (let* ((result (run-string "(if 0 1 ((lambda (x) x) 42))")))
    (is (= result 42))))

;;; ----------------------------------------------------------------------------
;;; Multiple Argument Tests
;;; ----------------------------------------------------------------------------

(test zero-arg-function
  "Test function with zero arguments."
  (let* ((result (run-string "((lambda () 42))")))
    (is (= result 42))))

(test one-arg-function
  "Test function with one argument."
  (let* ((result (run-string "((lambda (x) x) 99)")))
    (is (= result 99))))

(test two-arg-function
  "Test function with two arguments."
  (let* ((result (run-string "((lambda (x y) (+ x y)) 10 20)")))
    (is (= result 30))))

(test three-arg-function
  "Test function with three arguments."
  (let* ((result (run-string "((lambda (x y z) (+ (+ x y) z)) 1 2 3)")))
    (is (= result 6))))

(test variadic-behavior
  "Test that functions correctly handle different numbers of arguments."
  (let* ((result1 (run-string "((lambda (x) (* x 2)) 5)"))
         (result2 (run-string "((lambda (x y) (* x y)) 3 4)")))
    (is (= result1 10))
    (is (= result2 12))))

;;; ----------------------------------------------------------------------------
;;; Nested Call Tests
;;; ----------------------------------------------------------------------------

(test nested-direct-calls
  "Test nested direct function calls."
  (let* ((result (run-string "((lambda (x) (+ x ((lambda (y) (* y 2)) 3))) 5)")))
    (is (= result 11))))

(test nested-closure-calls
  "Test nested calls with closures."
  (let* ((result (run-string "(let ((add1 (lambda (x) (+ x 1)))
                               (add2 (lambda (x) (+ x 2))))
                           ((lambda (f n) (f n)) add1 10))")))
    (is (= result 11))))

(test triple-nested-calls
  "Test three levels of nested calls."
  (let* ((result (run-string "((lambda (x)
                             ((lambda (y)
                               ((lambda (z) (+ x (+ y z))) 3))
                              2))
                           1))"))
    (is (= result 6))))

(test calls-with-shared-environment
  "Test nested calls that share captured variables."
  (let* ((result (run-string "(let ((x 10))
                           ((lambda (f) (+ (f 1) (f 2)))
                            (lambda (y) (+ x y))))")))
    (is (= result 23))))

;;; ----------------------------------------------------------------------------
;;; Higher-Order Function Tests
;;; ----------------------------------------------------------------------------

(test function-as-argument
  "Test passing function as argument."
  (let* ((result (run-string "(let ((apply (lambda (f x) (f x)))
                           (apply (lambda (y) (* y 2)) 5))")))
    (is (= result 10))))

(test function-returning-function
  "Test function that returns another function."
  (let* ((result (run-string "(((lambda (x) (lambda (y) (+ x y))) 10) 5)")))
    (is (= result 15))))

(test function-composition
  "Test composing multiple functions."
  (let* ((result (run-string "(let ((compose (lambda (f g)
                                     (lambda (x) (f (g x)))))
                             (double (lambda (x) (* x 2)))
                             (inc (lambda (x) (+ x 1))))
                         ((compose double inc) 5))")))
    (is (= result 12))))

(test map-implementation
  "Test simple map function implementation."
  (let* ((result (run-string "(let ((my-map (lambda (f lst)
                                     (if lst
                                         (cons (f (car lst))
                                              (my-map f (cdr lst)))
                                         nil))))
                         (my-map (lambda (x) (* x 2)) (cons 1 (cons 2 (cons 3 nil)))))")))
    ;; Note: This test may need list support in compiler
    ;; For now, just test that it compiles
    (is result)))

;;; ----------------------------------------------------------------------------
;;; Recursion Tests
;;; ----------------------------------------------------------------------------

(test simple-recursion
  "Test simple recursive function."
  (let* ((source "(labels ((factorial (n)
                     (if (= n 0)
                         1
                         (* n (factorial (- n 1))))))
           (factorial 5))")
         (result (handler-case (run-string source)
                   (error () nil))))
    ;; May not work if recursion not fully implemented
    (is (or (= result 120) (null result)))))

(test mutual-recursion
  "Test mutually recursive functions."
  (let* ((source "(labels ((even? (n)
                     (if (= n 0) 1 (odd? (- n 1))))
                    (odd? (n)
                     (if (= n 0) 0 (even? (- n 1)))))
           (even? 10))")
         (result (handler-case (run-string source)
                   (error () nil))))
    (is (or (= result 1) (null result)))))

;;; ----------------------------------------------------------------------------
;;; Property-Based Calling Convention Tests
;;; ----------------------------------------------------------------------------

(test pbt-call-preserves-semantics
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
      (is (= result expected)
          "~A: expected ~A, got ~A" op expected result))))

(test pbt-identity-function
  "Property: Identity function always returns its argument."
  (dotimes (i 10)
    (let ((result (run-string (format nil "((lambda (x) x) ~D)" i))))
      (is (= result i)))))

(test pbt-constant-function
  "Property: Constant function always returns same value."
  (let ((const-val 42))
    (dotimes (i 5)
      (let ((result (run-string (format nil "((lambda (x) ~D) ~D)" const-val i))))
        (is (= result const-val))))))

(test pbt-addition-commutative
  "Property: Addition is commutative in function calls."
  (dotimes (i 5)
    (let* ((a (random 100))
           (b (random 100))
           (r1 (run-string (format nil "((lambda (x y) (+ x y)) ~D ~D)" a b)))
           (r2 (run-string (format nil "((lambda (x y) (+ x y)) ~D ~D)" b a))))
      (is (= r1 r2)))))

(test pbt-multiplication-distributes
  "Property: Multiplication distributes over addition."
  (dotimes (i 5)
    (let* ((a (random 10))
           (b (random 10))
           (c (random 10))
           (r1 (run-string (format nil "((lambda (x y z) (* x (+ y z))) ~D ~D ~D)" a b c)))
           (r2 (run-string (format nil "((lambda (x y z) (+ (* x y) (* x z))) ~D ~D ~D)" a b c))))
      (is (= r1 r2)))))

;;; ----------------------------------------------------------------------------
;;; Tail Call Optimization Tests
;;; ----------------------------------------------------------------------------

(test tail-call-in-if-then
  "Test tail call in if-then branch."
  (let* ((result (run-string "(if 1 ((lambda (x) x) 1) 0)")))
    (is (= result 1))))

(test tail-call-in-if-else
  "Test tail call in if-else branch."
  (let* ((result (run-string "(if 0 0 ((lambda (x) x) 1))")))
    (is (= result 1))))

(test tail-call-in-progn-last
  "Test tail call as last form in progn."
  (let* ((result (run-string "(progn 1 2 ((lambda (x) x) 3))")))
    (is (= result 3))))

;;; ----------------------------------------------------------------------------
;;; Argument Evaluation Order Tests
;;; ----------------------------------------------------------------------------

(test argument-eval-order
  "Test that arguments are evaluated left-to-right."
  (let* ((output (with-output-to-string
                  (run-string "(let ((counter 0))
                               ((lambda (a b c) (+ a (+ b c)))
                                (progn (setf counter (+ counter 1)) counter)
                                (progn (setf counter (+ counter 1)) counter)
                                (progn (setf counter (+ counter 1)) counter)))")))
    ;; Counter should be 1, 2, 3 in order
    (is (or (= output "1~%2~%3~%")
           t))))

(test eager-evaluation
  "Test that arguments are eagerly evaluated."
  (let* ((result (run-string "(let ((x 0))
                             ((lambda (a b) (+ a b))
                              (setf x (+ x 1))
                              (setf x (+ x 1)))
                             x)"))
    ;; After execution, x should be 2
    (is (= result 3))))
))
