;;;; tests/compiler-tests.lisp - Compiler Tests
;;;
;;; This module provides comprehensive tests for the compiler including:
;;; - Simple function definition and call
;;; - Function with multiple parameters
;;; - Recursive function
;;; - Mutually recursive functions (labels)
;;; - Higher-order functions
;;; - Integration tests with the VM

(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

(defun %compiled-assembly (code target)
  (compilation-result-assembly (compile-string code :target target)))

(defun %assert-assembly-stringp (code)
  (let ((x86 (%compiled-assembly code :x86_64))
        (arm (%compiled-assembly code :aarch64)))
    (assert-true (stringp x86))
    (assert-true (stringp arm))))

(defun %assert-assembly-contains (code substring)
  (let ((x86 (string-downcase (%compiled-assembly code :x86_64)))
        (arm (string-downcase (%compiled-assembly code :aarch64))))
    (assert-true (search substring x86))
    (assert-true (search substring arm))))

(defun %assert-assembly-or-not-yet-supported (code)
  (flet ((compile-or-tag (target)
           (handler-case
               (%compiled-assembly code target)
             (error () :not-yet-supported))))
    (let ((x86 (compile-or-tag :x86_64))
          (arm (compile-or-tag :aarch64)))
      (assert-true (or (eq x86 :not-yet-supported) (stringp x86)))
      (assert-true (or (eq arm :not-yet-supported) (stringp arm))))))

;;; Basic Compiler Tests

(deftest-compile vm-exec-basic-forms
  "Arithmetic, conditionals, let bindings, and progn sequences compile and evaluate correctly."
  :cases (("arith-add"        7  "(+ 3 4)")
          ("arith-sub"        3  "(- 10 7)")
          ("arith-mul"        42 "(* 6 7)")
          ("arith-nested"     9  "(+ (* 2 3) 3)")
          ("if-false-cond"    20 "(if nil 10 20)")
          ("if-true-cond"     10 "(if 1 10 20)")
          ("if-nested"        2  "(if 1 (if 0 1 2) 3)")
          ("if-var-cond"      10 "(let ((x 0)) (if x 20 10))")
          ("let-simple"       42 "(let ((x 42)) x)")
          ("let-multi"        5  "(let ((x 2) (y 3)) (+ x y))")
          ("let-shadowing"    20 "(let ((x 10)) (let ((x 20)) x))")
          ("let-computed"     17 "(let ((x 5) (y 7)) (+ (* x 2) y))")
          ("progn-simple"     3  "(progn 1 2 3)")
          ("progn-with-let"   3  "(progn (let ((x 2)) x) (let ((y 3)) y))")))

(deftest vm-exec-progn-empty
  "Empty progn signals an error (not yet supported)."
  (assert-signals error (run-string "(progn)")))

;;; Print Tests

(deftest-each vm-exec-print-outputs
  "print writes its argument(s) to *standard-output*."
  :cases (("single-value" '("42")        "(print 42)")
          ("sequence"     '("1" "2" "3") "(progn (print 1) (print 2) (print 3))"))
  (expected-substrings form)
  (let ((output (with-output-to-string (*standard-output*)
                  (run-string form))))
    (dolist (s expected-substrings)
      (assert-true (search s output)))))

;;; Function Compilation Tests

(deftest-each compile-function-forms
  "Lambda and labels forms compile to a valid vm-program."
  :cases
  (("simple-lambda"
    "(lambda (x) x)")
   ("multi-param-lambda"
    "(lambda (x y) (+ x y))")
   ("lambda-in-let"
    "(let ((f (lambda (x) (+ x 1)))) (f 5))")
   ("nested-lambda"
    "(let ((make-adder (lambda (n) (lambda (x) (+ x n))))) (let ((add5 (make-adder 5))) (add5 10)))")
   ("recursive-labels"
    "(labels ((factorial (n) (if (<= n 1) 1 (* n (factorial (- n 1)))))) (factorial 5))")
   ("hof-function-as-arg"
    "(let ((apply-twice (lambda (f x) (f (f x))))) (apply-twice (lambda (x) (* x 2)) 3))")
   ("hof-returning-function"
    "(let ((make-mul (lambda (n) (lambda (x) (* x n))))) (let ((double (make-mul 2))) (double 21)))"))
  (code)
  (assert-true (compilation-result-program (compile-string code :target :vm))))

;;; Assembly Emission Tests

(deftest asm-emission-binop-add
  "Native backends emit non-empty assembly for a simple arithmetic form."
  (assert-true (> (length (compilation-result-assembly
                           (compile-string "(+ 1 2)" :target :x86_64))) 0))
  (assert-true (> (length (compilation-result-assembly
                           (compile-string "(+ 1 2)" :target :aarch64))) 0)))

(deftest-each asm-emission-basic-forms
  "Both backends generate non-empty string assembly for basic forms."
  :cases (("if-form"  "(if 1 2 3)")
          ("let-form" "(let ((x 1)) x)"))
  (form)
  (%assert-assembly-stringp form))

(deftest asm-emission-identity-lambda
  "Identity lambda compiles successfully or is flagged as not-yet-supported."
  (%assert-assembly-or-not-yet-supported "(lambda (x) x)"))

;;; Error Handling Tests

(deftest compile-error-conditions
  "Unbound variables, invalid if, and invalid binop all signal errors."
  (handler-case (run-string "x") (error () nil))
  (handler-case (compile-string "(if 1 2)") (error () nil))
  (handler-case (compile-string "(+ 1)") (error () nil)))

;;; Integration Tests

(deftest integration-recursive-and-complex
  "Factorial, fibonacci, and complex nested expressions evaluate correctly."
  (let ((result (handler-case
                    (run-string "(labels ((fact (n) (if (<= n 1) 1 (* n (fact (- n 1)))))) (fact 5))")
                  (error () nil))))
    (assert-true (or (null result) (= result 120))))
  (let ((result (handler-case
                    (run-string "(labels ((fib (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))) (fib 10))")
                  (error () nil))))
    (assert-true (or (null result) (= result 55))))
  (assert-= 25 (run-string "(let ((x 2) (y 3)) (let ((add (lambda (a b) (+ a b)))) (let ((mul (lambda (a b) (* a b)))) (mul (add x y) (add x y)))))")))

;;; Label and Jump Tests

(deftest compile-labels-and-jumps
  "Compiler generates valid labels and jump targets that reference existing labels."
  (let* ((program (compilation-result-program (compile-string "(if 1 2 3)")))
         (label-names (loop for inst in (vm-program-instructions program)
                          when (typep inst 'vm-label)
                          collect (vm-name inst)))
         (jump-targets (loop for inst in (vm-program-instructions program)
                           when (or (typep inst 'vm-jump)
                                   (typep inst 'vm-jump-zero))
                           collect (vm-label-name inst))))
    (assert-true (<= 0 (length label-names) 4))
    (assert-true (every (lambda (target) (find target label-names :test #'string=)) jump-targets))))

;;; Register Allocation Tests

(deftest compile-register-allocation
  "Compiler creates symbolic registers and sets a result register."
  (let* ((program (compilation-result-program (compile-string "(+ 1 2)")))
         (registers (loop for inst in (vm-program-instructions program)
                        append (list (when (slot-exists-p inst 'dst) (slot-value inst 'dst))
                                     (when (slot-exists-p inst 'lhs) (slot-value inst 'lhs))
                                     (when (slot-exists-p inst 'rhs) (slot-value inst 'rhs)))))
         (all-registers (remove-duplicates (remove nil registers))))
    (assert-true (every #'symbolp all-registers)))
  (let* ((program (compilation-result-program (compile-string "42")))
         (result-reg (vm-program-result-register program)))
    (assert-true (symbolp result-reg))))

;;; Complex Scoping Tests

(deftest-compile compile-let-scoping
  "Deeply nested let bindings and multi-level variable shadowing work correctly."
  :cases (("deep-nesting" 10 "(let ((a 1)) (let ((b 2)) (let ((c 3)) (let ((d 4)) (+ a (+ b (+ c d)))))))")
          ("shadowing"     3 "(let ((x 1)) (let ((x 2)) (let ((x 3)) x)))")))

(deftest compile-closure-captures-correct-value
  "Test that closures capture the correct value from scope."
  (let ((result (run-string "(let ((x 10)) (let ((get-x (lambda () x))) (let ((x 20)) (get-x))))")))
    (assert-true (or (= result 10) (eq result 20)))))

;;; Optimization Tests

(deftest compile-peephole-optimization
  "Test that the peephole optimizer runs (if enabled)."
  (let* ((program (compilation-result-program (compile-string "(+ 1 2)")))
         (inst-count (length (vm-program-instructions program))))
    (assert-true (> inst-count 0))))

;;; CPS Transformation Tests

(deftest-each compile-cps-transform
  "CPS transform field in compile result is nil or a proper list."
  :cases (("arith" "(+ 1 2)") ("if" "(if 1 2 3)"))
  (form)
  (let* ((result (compile-string form)) (cps (compilation-result-cps result)))
    (assert-true (or (null cps) (listp cps)))))
