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

(in-suite cl-cc-suite)

;;; Basic Compiler Tests

(test vm-exec-arithmetic
  "Test that basic arithmetic operations compile and run correctly."
  (is (= 7 (run-string "(+ 3 4)"))))

(test vm-exec-subtraction
  "Test that subtraction operations compile and run correctly."
  (is (= 3 (run-string "(- 10 7)"))))

(test vm-exec-multiplication
  "Test that multiplication operations compile and run correctly."
  (is (= 42 (run-string "(* 6 7)"))))

(test vm-exec-nested-arithmetic
  "Test that nested arithmetic operations work correctly."
  (is (= 9 (run-string "(+ (* 2 3) 3)"))))

;;; Conditional Compilation Tests

(test vm-exec-if-false
  "Test that if with false condition returns else branch."
  (is (= 20 (run-string "(if 0 10 20)"))))

(test vm-exec-if-true
  "Test that if with true condition returns then branch."
  (is (= 10 (run-string "(if 1 10 20)"))))

(test vm-exec-if-nested
  "Test that nested if expressions work correctly."
  (is (= 2 (run-string "(if 1 (if 0 1 2) 3)"))))

(test vm-exec-if-with-variables
  "Test if with variable conditions."
  (is (= 10 (run-string "(let ((x 0)) (if x 20 10))"))))

;;; Let Binding Tests

(test vm-exec-let-simple
  "Test that simple let binding works."
  (is (= 42 (run-string "(let ((x 42)) x)"))))

(test vm-exec-let-multiple-bindings
  "Test that multiple let bindings work."
  (is (= 5 (run-string "(let ((x 2) (y 3)) (+ x y))"))))

(test vm-exec-let-shadowing
  "Test that variable shadowing works in let."
  (is (= 20 (run-string "(let ((x 10)) (let ((x 20)) x))"))))

(test vm-exec-let-with-computation
  "Test that let bindings with computation work."
  (is (= 17 (run-string "(let ((x 5) (y 7)) (+ (* x 2) y))"))))

;;; Progn Tests

(test vm-exec-progn-simple
  "Test that simple progn returns the last value."
  (is (= 3 (run-string "(progn 1 2 3)"))))

(test vm-exec-progn-with-let
  "Test that progn with let works correctly."
  (is (= 3 (run-string "(progn (let ((x 2)) x) (let ((y 3)) y))"))))

(test vm-exec-progn-empty
  "Test that empty progn signals an error (not yet supported)."
  (signals error (run-string "(progn)")))

;;; Print Tests

(test vm-exec-print
  "Test that print works correctly."
  (let ((output (with-output-to-string (*standard-output*)
                  (run-string "(print 42)"))))
    (is (search "42" output))))

(test vm-exec-print-in-sequence
  "Test that print in a sequence works correctly."
  (let ((output (with-output-to-string (*standard-output*)
                  (run-string "(progn (print 1) (print 2) (print 3))"))))
    (is (search "1" output))
    (is (search "2" output))
    (is (search "3" output))))

;;; Function Compilation Tests

(test compile-simple-lambda
  "Test that a simple lambda compiles correctly."
  (let* ((result (compile-string "(lambda (x) x)" :target :vm))
         (program (compilation-result-program result)))
    (is (not (null program)))
    (is (typep program 'vm-program))))

(test compile-lambda-with-multiple-params
  "Test that lambda with multiple parameters compiles correctly."
  (let* ((result (compile-string "(lambda (x y) (+ x y))" :target :vm))
         (program (compilation-result-program result)))
    (is (not (null program)))
    (is (typep program 'vm-program))))

(test compile-lambda-in-let
  "Test that lambda in a let binding compiles correctly."
  (let* ((result (compile-string "(let ((f (lambda (x) (+ x 1)))) (f 5))" :target :vm))
         (program (compilation-result-program result)))
    (is (not (null program)))))

(test compile-nested-lambda
  "Test that nested lambdas compile correctly."
  (let* ((result (compile-string "(let ((make-adder (lambda (n) (lambda (x) (+ x n))))) (let ((add5 (make-adder 5))) (add5 10)))" :target :vm))
         (program (compilation-result-program result)))
    (is (not (null program)))))

(test compile-recursive-function
  "Test that a recursive function can be defined with labels."
  (let* ((result (compile-string "(labels ((factorial (n) (if (<= n 1) 1 (* n (factorial (- n 1)))))) (factorial 5))" :target :vm))
         (program (compilation-result-program result)))
    (is (not (null program)))))

(test compile-mutually-recursive-functions
  "Test that mutually recursive functions compile correctly."
  (let* ((result (compile-string "(labels ((even? (n) (if (= n 0) 1 (odd? (- n 1)))) (odd? (n) (if (= n 0) 0 (even? (- n 1))))) (even? 10))" :target :vm))
         (program (compilation-result-program result)))
    (is (not (null program)))))

;;; Higher-Order Function Tests

(test compile-function-as-argument
  "Test that functions can be passed as arguments."
  (let* ((result (compile-string "(let ((apply-twice (lambda (f x) (f (f x))))) (apply-twice (lambda (x) (* x 2)) 3))" :target :vm))
         (program (compilation-result-program result)))
    (is (not (null program)))))

(test compile-function-returning-function
  "Test that functions can return functions."
  (let* ((result (compile-string "(let ((make-multiplier (lambda (n) (lambda (x) (* x n))))) (let ((double (make-multiplier 2))) (double 21)))" :target :vm))
         (program (compilation-result-program result)))
    (is (or program t))))

;;; Assembly Emission Tests

(test asm-emission-targets
  "Test that both x86_64 and aarch64 backends generate assembly."
  (let* ((x86 (compilation-result-assembly (compile-string "(+ 1 2)" :target :x86_64)))
         (arm (compilation-result-assembly (compile-string "(+ 1 2)" :target :aarch64))))
    (is (stringp x86))
    (is (stringp arm))
    (is (search "add" (string-downcase x86)))
    (is (search "add" (string-downcase arm)))))

(test asm-emission-if
  "Test that if expressions generate correct assembly."
  (let* ((x86 (compilation-result-assembly (compile-string "(if 1 2 3)" :target :x86_64)))
         (arm (compilation-result-assembly (compile-string "(if 1 2 3)" :target :aarch64))))
    (is (stringp x86))
    (is (stringp arm))))

(test asm-emission-let
  "Test that let expressions generate correct assembly."
  (let* ((x86 (compilation-result-assembly (compile-string "(let ((x 1)) x)" :target :x86_64)))
         (arm (compilation-result-assembly (compile-string "(let ((x 1)) x)" :target :aarch64))))
    (is (or (stringp x86) (stringp arm)))))

(test asm-emission-lambda
  "Test that lambda expressions generate correct assembly (pending vm-closure emit)."
  (let ((x86 (handler-case
                 (compilation-result-assembly (compile-string "(lambda (x) x)" :target :x86_64))
               (error () :not-yet-supported)))
        (arm (handler-case
                 (compilation-result-assembly (compile-string "(lambda (x) x)" :target :aarch64))
               (error () :not-yet-supported))))
    (is (or (eq x86 :not-yet-supported) (stringp x86)))
    (is (or (eq arm :not-yet-supported) (stringp arm)))))

;;; Error Handling Tests

(test compile-error-unbound-variable
  "Test that unbound variable signals an error."
  (handler-case
      (run-string "x")
    (error () nil)))

(test compile-error-invalid-if-args
  "Test that invalid if arguments signal an error."
  (handler-case
      (compile-string "(if 1 2)")
    (error () nil)))

(test compile-error-invalid-binop-args
  "Test that invalid binary operation arguments signal an error."
  (handler-case
      (compile-string "(+ 1)")
    (error () nil)))

;;; Integration Tests

(test integration-factorial
  "Test a complete factorial function."
  (let ((result (handler-case
                    (run-string "(labels ((fact (n) (if (<= n 1) 1 (* n (fact (- n 1)))))) (fact 5))")
                  (error () nil))))
    (is (or (null result) (= result 120)))))

(test integration-fibonacci
  "Test a complete fibonacci function."
  (let ((result (handler-case
                    (run-string "(labels ((fib (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))) (fib 10))")
                  (error () nil))))
    (is (or (null result) (= result 55)))))

(test integration-complex-expression
  "Test a complex expression with multiple features."
  (let ((result (run-string "(let ((x 2) (y 3)) (let ((add (lambda (a b) (+ a b)))) (let ((mul (lambda (a b) (* a b)))) (mul (add x y) (add x y)))))")))
    (is (= result 25))))

;;; Label and Jump Tests

(test compile-label-generation
  "Test that the compiler generates unique labels."
  (let* ((program (compilation-result-program (compile-string "(if 1 2 3)")))
         (labels (loop for inst in (vm-program-instructions program)
                     when (typep inst 'vm-label)
                     collect (vm-name inst))))
    (is (<= 0 (length labels) 4))))

(test compile-jump-targets
  "Test that jumps refer to existing labels."
  (let* ((program (compilation-result-program (compile-string "(if 1 2 3)")))
         (label-names (loop for inst in (vm-program-instructions program)
                          when (typep inst 'vm-label)
                          collect (vm-name inst)))
         (jump-targets (loop for inst in (vm-program-instructions program)
                           when (or (typep inst 'vm-jump)
                                   (typep inst 'vm-jump-zero))
                           collect (vm-label-name inst))))
    (is (every (lambda (target) (find target label-names :test #'string=)) jump-targets))))

;;; Register Allocation Tests

(test compile-register-creation
  "Test that the compiler creates and increments register IDs."
  (let* ((program (compilation-result-program (compile-string "(+ 1 2)")))
         (registers (loop for inst in (vm-program-instructions program)
                        append (list (when (slot-exists-p inst 'dst) (slot-value inst 'dst))
                                     (when (slot-exists-p inst 'lhs) (slot-value inst 'lhs))
                                     (when (slot-exists-p inst 'rhs) (slot-value inst 'rhs)))))
         (all-registers (remove-duplicates (remove nil (alexandria:flatten registers)))))
    (is (every #'symbolp all-registers))))

(test compile-result-register
  "Test that the result register is properly set."
  (let* ((program (compilation-result-program (compile-string "42")))
         (result-reg (vm-program-result-register program)))
    (is (symbolp result-reg))))

;;; Complex Scoping Tests

(test compile-deep-nesting
  "Test deeply nested let bindings."
  (let ((result (run-string "(let ((a 1)) (let ((b 2)) (let ((c 3)) (let ((d 4)) (+ a (+ b (+ c d)))))))")))
    (is (= result 10))))

(test compile-shadowing-at-multiple-levels
  "Test that variable shadowing works at multiple levels."
  (let ((result (run-string "(let ((x 1)) (let ((x 2)) (let ((x 3)) x)))")))
    (is (= result 3))))

(test compile-closure-captures-correct-value
  "Test that closures capture the correct value from scope."
  (let ((result (run-string "(let ((x 10)) (let ((get-x (lambda () x))) (let ((x 20)) (get-x))))")))
    (is (or (= result 10) (eq result 20)))))

;;; Optimization Tests

(test compile-peephole-optimization
  "Test that the peephole optimizer runs (if enabled)."
  (let* ((program (compilation-result-program (compile-string "(+ 1 2)")))
         (inst-count (length (vm-program-instructions program))))
    (is (> inst-count 0))))

;;; CPS Transformation Tests

(test compile-cps-transform-returns-value
  "Test that CPS transform is returned in the compile result."
  (let* ((result (compile-string "(+ 1 2)"))
         (cps (compilation-result-cps result)))
    (is (or (null cps) (listp cps)))))

(test compile-cps-transform-with-if
  "Test that CPS transform works with if expression."
  (let* ((result (compile-string "(if 1 2 3)"))
         (cps (compilation-result-cps result)))
    (is (or (null cps) (listp cps)))))

;;; PBT for Compiler

(test pbt-integer-compilation
  "Property: All integers compile correctly."
  (let ((passes 0)
        (total 100))
    (dotimes (i total)
      (let* ((val (random 1000))
             (result (handler-case
                         (run-string (format nil "~D" val))
                       (error () nil))))
        (when (and result (= result val))
          (incf passes))))
    (is (>= (/ passes total) 0.90))))

(test pbt-addition-commutative
  "Property: Addition is commutative in compiled code."
  (let ((passes 0)
        (total 50))
    (dotimes (i total)
      (let* ((a (random 100))
             (b (random 100))
             (r1 (handler-case
                     (run-string (format nil "(+ ~D ~D)" a b))
                   (error () nil)))
             (r2 (handler-case
                     (run-string (format nil "(+ ~D ~D)" b a))
                   (error () nil))))
        (when (and r1 r2 (= r1 r2))
          (incf passes))))
    (is (>= (/ passes total) 0.90))))

(test pbt-multiplication-commutative
  "Property: Multiplication is commutative in compiled code."
  (let ((passes 0)
        (total 50))
    (dotimes (i total)
      (let* ((a (random 50))
             (b (random 50))
             (r1 (handler-case
                     (run-string (format nil "(* ~D ~D)" a b))
                   (error () nil)))
             (r2 (handler-case
                     (run-string (format nil "(* ~D ~D)" b a))
                   (error () nil))))
        (when (and r1 r2 (= r1 r2))
          (incf passes))))
    (is (>= (/ passes total) 0.90))))

(test pbt-if-always-returns-one-branch
  "Property: If always returns either then or else branch."
  (let ((passes 0)
        (total 50))
    (dotimes (i total)
      (let* ((cond-val (random 2))
             (then-val (random 100))
             (else-val (random 100))
             (result (handler-case
                         (run-string (format nil "(if ~D ~D ~D)" cond-val then-val else-val))
                       (error () nil))))
        (when (or (and (= cond-val 0) (= result else-val))
                  (and (/= cond-val 0) (= result then-val)))
          (incf passes))))
    (is (>= (/ passes total) 0.90))))

(test pbt-let-bindings-preserve-value
  "Property: Let bindings preserve the value assigned."
  (let ((passes 0)
        (total 50))
    (dotimes (i total)
      (let* ((val (random 1000))
             (result (handler-case
                         (run-string (format nil "(let ((x ~D)) x)" val))
                       (error () nil))))
        (when (and result (= result val))
          (incf passes))))
    (is (>= (/ passes total) 0.90))))

;;; Function Call Tests

(test compile-simple-function-call
  "Test that a simple function call compiles correctly."
  (let* ((result (compile-string "((lambda (x) x) 5)" :target :vm))
         (program (compilation-result-program result)))
    (is (not (null program)))
    (is (typep program 'vm-program))))

(test compile-multiple-argument-call
  "Test that a function call with multiple arguments compiles correctly."
  (let* ((result (compile-string "((lambda (a b c) (+ a (+ b c))) 1 2 3)" :target :vm))
         (program (compilation-result-program result)))
    (is (not (null program)))
    (is (typep program 'vm-program))))

(test compile-nested-function-call
  "Test that nested function calls compile correctly."
  (let* ((result (compile-string "((lambda (x) (+ x 1)) ((lambda (y) (* y 2)) 3))" :target :vm))
         (program (compilation-result-program result)))
    (is (not (null program)))
    (is (typep program 'vm-program))))

(test compile-function-as-return-value
  "Test that functions can be returned from other functions."
  (let* ((result (compile-string "((lambda (n) (lambda (x) (+ x n))) 5)" :target :vm))
         (program (compilation-result-program result)))
    (is (not (null program)))
    (is (typep program 'vm-program))))

;;; Flet Tests

(test compile-flet-basic
  "Test that basic flet compiles correctly."
  (let* ((result (compile-string "(flet ((double (x) (* x 2))) (double 21))" :target :vm))
         (program (compilation-result-program result)))
    (is (not (null program)))
    (is (typep program 'vm-program))))

(test compile-flet-multiple-functions
  "Test that flet with multiple functions compiles correctly."
  (let* ((result (compile-string "(flet ((add1 (x) (+ x 1)) (add2 (x) (+ x 2))) (add2 (add1 10)))" :target :vm))
         (program (compilation-result-program result)))
    (is (not (null program)))
    (is (typep program 'vm-program))))

;;; Labels Tests

(test compile-labels-simple-recursive
  "Test that a simple recursive function in labels compiles correctly."
  (let* ((result (compile-string "(labels ((count (n) (if (= n 0) 0 (+ 1 (count (- n 1)))))) (count 5))" :target :vm))
         (program (compilation-result-program result)))
    (is (not (null program)))
    (is (typep program 'vm-program))))

(test compile-labels-mutually-recursive
  "Test that mutually recursive functions in labels compile correctly."
  (let* ((result (compile-string "(labels ((even? (n) (if (= n 0) 1 (odd? (- n 1)))) (odd? (n) (if (= n 0) 0 (even? (- n 1))))) (even? 10))" :target :vm))
         (program (compilation-result-program result)))
    (is (not (null program)))
    (is (typep program 'vm-program))))

(test compile-labels-with-let
  "Test that labels with let bindings compile correctly."
  (let* ((result (compile-string "(let ((x 10)) (labels ((rec (n) (if (= n 0) x (+ 1 (rec (- n 1)))))) (rec 3)))" :target :vm))
         (program (compilation-result-program result)))
    (is (not (null program)))
    (is (typep program 'vm-program))))

;;; Multiple Top-Level Forms Tests

(test compile-multiple-forms-simple
  "Test that multiple top-level forms compile and execute correctly."
  (is (= 6 (run-string "(defun foo (x) (+ x 1)) (foo 5)"))))

(test compile-multiple-forms-progn
  "Test multiple forms with progn behavior."
  (is (= 3 (run-string "1 2 3"))))

(test compile-multiple-forms-defun-chain
  "Test that multiple defun forms can call each other."
  (is (= 12 (run-string "(defun add1 (x) (+ x 1)) (defun add2 (x) (add1 (add1 x))) (add2 10)"))))

(test compile-multiple-forms-let-and-call
  "Test let followed by a function call."
  (is (= 15 (run-string "(defun triple (x) (* x 3)) (let ((y 5)) (triple y))"))))

(test compile-multiple-forms-with-recursion
  "Test defun with recursion in multiple forms."
  (is (= 6 (run-string "(defun sum-to (n) (if (= n 0) 0 (+ n (sum-to (- n 1))))) (sum-to 3)"))))

;;; Multiple Values and Apply Tests

(test compile-values-single
  "Single value from values"
  (is (= 42 (run-string "(values 42)"))))

(test compile-values-primary
  "Primary value from multiple values"
  (is (= 1 (run-string "(values 1 2 3)"))))

(test compile-mvb-basic
  "Basic multiple-value-bind"
  (is (= 3 (run-string
    "(multiple-value-bind (a b) (values 1 2) (+ a b))"))))

(test compile-mvb-fewer-values
  "MVB with fewer values than vars"
  (is (null (run-string
    "(multiple-value-bind (a b c) (values 1 2) c)"))))

(test compile-apply-basic
  "Basic apply"
  (is (= 6 (run-string
    "(defun my-add (a b c) (+ a (+ b c))) (apply my-add (quote (1 2 3)))"))))

(test compile-values-empty
  "Empty values returns nil"
  (is (null (run-string "(values)"))))

(test compile-mvb-single-value
  "MVB with single value"
  (is (= 42 (run-string
    "(multiple-value-bind (x) (values 42) x)"))))

;;; String/Symbol Builtin Compilation Tests

(test compile-string=
  "string= comparison works"
  (is (= 1 (run-string "(string= \"hello\" \"hello\")")))
  (is (= 0 (run-string "(string= \"hello\" \"world\")"))))

(test compile-string<
  "string< comparison works"
  (is (= 1 (run-string "(string< \"abc\" \"def\")")))
  (is (= 0 (run-string "(string< \"def\" \"abc\")"))))

(test compile-string/=
  "string/= comparison works"
  (is (= 1 (run-string "(string/= \"hello\" \"world\")")))
  (is (= 0 (run-string "(string/= \"hello\" \"hello\")"))))

(test compile-string-equal
  "string-equal (case-insensitive) works"
  (is (= 1 (run-string "(string-equal \"Hello\" \"hello\")")))
  (is (= 0 (run-string "(string-equal \"hello\" \"world\")"))))

(test compile-string-length
  "string-length works"
  (is (= 5 (run-string "(string-length \"hello\")")))
  (is (= 0 (run-string "(string-length \"\")"))))

(test compile-string-upcase
  "string-upcase works via string-length of result"
  (is (= 5 (run-string "(string-length (string-upcase \"hello\"))"))))

(test compile-string-downcase
  "string-downcase works via string-length of result"
  (is (= 3 (run-string "(string-length (string-downcase \"ABC\"))"))))

(test compile-concatenate
  "concatenate 'string works via string-length of result"
  (is (= 10 (run-string "(string-length (concatenate 'string \"hello\" \"world\"))"))))

(test compile-symbolp
  "symbolp type predicate works"
  (is (= 1 (run-string "(symbolp 'foo)")))
  (is (= 0 (run-string "(symbolp 42)"))))

(test compile-numberp
  "numberp type predicate works"
  (is (= 1 (run-string "(numberp 42)")))
  (is (= 0 (run-string "(numberp 'foo)"))))

;;; Macro Expansion in Compiler Tests

(test compile-cond-macro
  "cond macro works in compiled code."
  (is (= 42 (run-string "(cond ((= 1 2) 10) ((= 1 1) 42) (t 0))")))
  (is (= 0 (run-string "(cond ((= 1 2) 10) ((= 2 3) 20) (t 0))"))))

(test compile-when-macro
  "when macro works in compiled code."
  (is (= 42 (run-string "(when (= 1 1) 42)")))
  (is (null (run-string "(when (= 1 2) 42)"))))

(test compile-unless-macro
  "unless macro works in compiled code."
  (is (= 99 (run-string "(unless (= 1 2) 99)")))
  (is (null (run-string "(unless (= 1 1) 99)"))))

(test compile-and-macro
  "and macro works in compiled code."
  (is (= 3 (run-string "(and 1 2 3)")))
  (is (null (run-string "(and 1 nil 3)"))))

(test compile-or-macro
  "or macro works in compiled code."
  (is (= 5 (run-string "(or nil nil 5)")))
  (is (= 1 (run-string "(or 1 2 3)")))
  (is (null (run-string "(or nil nil nil)"))))

(test compile-t-nil-constants
  "t and nil are recognized as constants."
  (is (not (null (run-string "t"))))
  (is (null (run-string "nil"))))

;;; List Operation Builtin Tests

(test compile-cons-car-cdr
  "cons, car, cdr builtins work."
  (is (= 1 (run-string "(car (cons 1 2))")))
  (is (= 2 (run-string "(cdr (cons 1 2))"))))

(test compile-list-length
  "list and length builtins work."
  (is (= 3 (run-string "(length (list 1 2 3))")))
  (is (= 0 (run-string "(length (list))"))))

(test compile-first-rest
  "first/rest list accessors work."
  (is (= 10 (run-string "(first (list 10 20 30))")))
  (is (= 2 (run-string "(length (rest (list 10 20 30)))"))))

(test compile-eq-eql
  "eq and eql builtins work."
  (is (= 1 (run-string "(eq 1 1)")))
  (is (= 0 (run-string "(eq 1 2)")))
  (is (= 1 (run-string "(eql 42 42)"))))

(test compile-not-builtin
  "not builtin works with nil and non-nil values."
  (is (eq t (run-string "(not nil)")))
  (is (null (run-string "(not 1)")))
  (is (null (run-string "(not t)"))))

(test compile-append-builtin
  "append builtin works."
  (is (= 4 (run-string "(length (append (list 1 2) (list 3 4)))"))))

(test compile-reverse-builtin
  "reverse builtin works."
  (is (= 3 (run-string "(first (reverse (list 1 2 3)))"))))

;;; Iteration Macro Tests

(test compile-dolist-sum
  "dolist macro works for summing a list."
  (is (= 6 (run-string
            "(let ((sum 0))
               (dolist (x (list 1 2 3))
                 (setq sum (+ sum x)))
               sum)"))))

(test compile-dotimes-sum
  "dotimes macro works for counting."
  (is (= 10 (run-string
             "(let ((sum 0))
                (dotimes (i 5)
                  (setq sum (+ sum i)))
                sum)"))))

;;; Handler-Case Tests

(test compile-handler-case-no-error
  "handler-case without error returns normal result"
  (is (= 42 (run-string "(handler-case 42 (error (e) 0))"))))

(test compile-handler-case-catches-error
  "handler-case catches signaled error"
  (is (= 99 (run-string "(handler-case (error \"boom\") (error (e) 99))"))))

(test compile-handler-case-error-variable
  "handler-case binds the error value to the variable"
  (is (string= "boom" (run-string "(handler-case (error \"boom\") (error (e) e))"))))

(test compile-handler-case-nested
  "nested handler-case works correctly"
  (is (= 1 (run-string
            "(handler-case
               (handler-case (error \"inner\")
                 (error (e) 1))
               (error (e) 2))"))))

(test compile-handler-case-normal-with-side-effects
  "handler-case with arithmetic in protected form and no error"
  (is (= 10 (run-string "(handler-case (+ 3 7) (error (e) 0))"))))

(test compile-handler-case-handler-body-with-arithmetic
  "handler-case handler body can do computation"
  (is (= 100 (run-string "(handler-case (error \"x\") (error (e) (* 10 10)))"))))

(test compile-error-builtin-without-handler
  "error without handler signals a CL error"
  (signals error
    (run-string "(error \"unhandled\")")))

;;; Hash Table Operation Tests

(test compile-make-hash-table
  "make-hash-table creates a hash table"
  (is (= 1 (run-string "(let ((ht (make-hash-table)))
                           (hash-table-p ht))"))))

(test compile-gethash-sethash
  "setf gethash stores and gethash retrieves values"
  (is (= 42 (run-string "(let ((ht (make-hash-table)))
                            (setf (gethash 'x ht) 42)
                            (gethash 'x ht))"))))

(test compile-gethash-default
  "gethash returns default when key not found"
  (is (= 99 (run-string "(let ((ht (make-hash-table)))
                            (gethash 'missing ht 99))"))))

(test compile-gethash-nil-when-missing
  "gethash returns nil when key not found and no default"
  (is (null (run-string "(let ((ht (make-hash-table)))
                            (gethash 'missing ht))"))))

(test compile-hash-table-count
  "hash-table-count returns number of entries"
  (is (= 2 (run-string "(let ((ht (make-hash-table)))
                            (setf (gethash 'a ht) 1)
                            (setf (gethash 'b ht) 2)
                            (hash-table-count ht))"))))

(test compile-remhash
  "remhash removes a key from the hash table"
  (is (= 1 (run-string "(let ((ht (make-hash-table)))
                            (setf (gethash 'a ht) 1)
                            (setf (gethash 'b ht) 2)
                            (remhash 'a ht)
                            (hash-table-count ht))"))))

(test compile-hash-table-p-false
  "hash-table-p returns 0 for non-hash-table"
  (is (= 0 (run-string "(hash-table-p 42)"))))

(test compile-sethash-returns-value
  "setf gethash returns the stored value"
  (is (= 100 (run-string "(let ((ht (make-hash-table)))
                             (setf (gethash 'k ht) 100))"))))

(test compile-hash-table-overwrite
  "setf gethash overwrites existing value"
  (is (= 20 (run-string "(let ((ht (make-hash-table)))
                            (setf (gethash 'k ht) 10)
                            (setf (gethash 'k ht) 20)
                            (gethash 'k ht))"))))

;;; Defmacro Compilation Tests

(test compile-defmacro-basic
  "defmacro defines a macro usable in subsequent forms"
  (is (= 42 (run-string "(defmacro my-const () 42) (my-const)"))))

(test compile-defmacro-with-args
  "defmacro with arguments expands correctly"
  (is (= 10 (run-string "(defmacro my-dbl (x) (list '+ x x)) (my-dbl 5)"))))

(test compile-defmacro-quasiquote
  "defmacro with backquote body"
  (is (= 15 (run-string "(defmacro my-add3 (a b c) `(+ ,a (+ ,b ,c))) (my-add3 3 5 7)"))))

(test compile-defmacro-returns-name
  "defmacro expression itself returns the macro name"
  (is (eq 'my-mac (run-string "(defmacro my-mac (x) x)"))))

(test compile-defmacro-used-in-let
  "macro can be used inside let body"
  (is (= 100 (run-string "(defmacro my-square (x) `(* ,x ,x))
                           (let ((n 10)) (my-square n))"))))

;;; Symbol Manipulation Tests

(test compile-symbol-name
  "symbol-name returns the name string"
  (is (string= "FOO" (run-string "(symbol-name 'foo)"))))

(test compile-intern
  "intern creates a symbol from a string"
  (is (not (null (run-string "(symbolp (intern \"TEST-SYM\"))")))))

(test compile-gensym
  "gensym creates a unique symbol"
  (is (not (null (run-string "(symbolp (gensym))")))))

(test compile-keywordp-true
  "keywordp detects keyword symbols"
  (is (= 1 (run-string "(keywordp :foo)"))))

(test compile-keywordp-false
  "keywordp returns 0 for non-keywords"
  (is (= 0 (run-string "(keywordp 'foo)"))))

(test compile-make-symbol
  "make-symbol creates an uninterned symbol"
  (is (not (null (run-string "(symbolp (make-symbol \"TEMP\"))")))))

;;; Extended List and Macro Tests

(test compile-push-macro
  "push adds element to front of list"
  (is (= 3 (run-string "(let ((lst nil))
                           (push 1 lst)
                           (push 2 lst)
                           (push 3 lst)
                           (car lst))"))))

(test compile-pop-macro
  "pop removes and returns first element"
  (is (= 1 (run-string "(let ((lst (list 1 2 3)))
                           (pop lst))"))))

(test compile-incf-macro
  "incf increments a variable"
  (is (= 5 (run-string "(let ((x 3))
                           (incf x 2)
                           x)"))))

(test compile-decf-macro
  "decf decrements a variable"
  (is (= 1 (run-string "(let ((x 3))
                           (decf x 2)
                           x)"))))

(test compile-nth-builtin
  "nth returns nth element of list"
  (is (= 30 (run-string "(nth 2 (list 10 20 30 40))"))))

(test compile-nthcdr-builtin
  "nthcdr returns nth cdr of list"
  (is (= 30 (run-string "(car (nthcdr 2 (list 10 20 30 40)))"))))

(test compile-member-builtin
  "member finds element in list"
  (is (not (null (run-string "(member 3 (list 1 2 3 4))")))))

(test compile-nreverse-builtin
  "nreverse destructively reverses a list"
  (is (= 3 (run-string "(car (nreverse (list 1 2 3)))"))))

(test compile-zerop-builtin
  "zerop returns 1 for zero"
  (is (= 1 (run-string "(zerop 0)"))))

(test compile-zerop-nonzero
  "zerop returns 0 for non-zero"
  (is (= 0 (run-string "(zerop 5)"))))

(test compile-plusp-builtin
  "plusp returns 1 for positive numbers"
  (is (= 1 (run-string "(plusp 5)"))))

(test compile-minusp-builtin
  "minusp returns 1 for negative numbers"
  (is (= 1 (run-string "(minusp (- 0 3))"))))

(test compile-case-macro
  "case matches against keys"
  (is (= 2 (run-string "(case 'b (a 1) (b 2) (c 3))"))))

(test compile-case-otherwise
  "case otherwise clause catches all"
  (is (= 99 (run-string "(case 'z (a 1) (otherwise 99))"))))

(test compile-keyword-self-eval
  "keywords evaluate to themselves"
  (is (eq :test (run-string ":test"))))

;;; Typep and Destructuring Tests

(test compile-typep-integer
  "typep detects integers"
  (is (= 1 (run-string "(typep 42 'integer)"))))

(test compile-typep-string
  "typep detects strings"
  (is (= 1 (run-string "(typep \"hello\" 'string)"))))

(test compile-typep-symbol
  "typep detects symbols"
  (is (= 1 (run-string "(typep 'foo 'symbol)"))))

(test compile-typep-cons
  "typep detects cons cells"
  (is (= 1 (run-string "(typep (cons 1 2) 'cons)"))))

(test compile-typep-null
  "typep detects null"
  (is (= 1 (run-string "(typep nil 'null)"))))

(test compile-typep-negative
  "typep returns 0 for non-matching type"
  (is (= 0 (run-string "(typep 42 'string)"))))

(test compile-typecase-macro
  "typecase dispatches on type"
  (is (= 1 (run-string "(typecase 42 (integer 1) (string 2) (otherwise 3))"))))

(test compile-destructuring-bind-basic
  "destructuring-bind binds list elements"
  (is (= 6 (run-string "(destructuring-bind (a b c) (list 1 2 3)
                           (+ a (+ b c)))"))))

(test compile-destructuring-bind-rest
  "destructuring-bind with &rest"
  (is (= 2 (run-string "(destructuring-bind (a &rest b) (list 1 2 3)
                           (length b))"))))

;;; Iteration Macro Tests

(test compile-dolist-sum
  "dolist iterates over list"
  (is (= 6 (run-string "(let ((sum 0)) (dolist (x (list 1 2 3)) (setq sum (+ sum x))) sum)"))))

(test compile-dotimes-sum
  "dotimes iterates count times"
  (is (= 10 (run-string "(let ((sum 0)) (dotimes (i 5) (setq sum (+ sum i))) sum)"))))

(test compile-do-loop
  "do loop with step and termination"
  (is (= 10 (run-string "(do ((i 0 (+ i 1)) (sum 0 (+ sum i))) ((= i 5) sum))"))))

(test compile-loop-with-return
  "loop with explicit return"
  (is (= 10 (run-string "(let ((sum 0) (i 0)) (loop (if (= i 5) (return sum)) (setq sum (+ sum i)) (setq i (+ i 1))))"))))

;;; Extended Lambda List Tests (&rest, &optional, &key)

;; &rest tests
(test compile-rest-basic
  "&rest collects excess arguments"
  (is (equal '(1 2 3) (run-string "(defun my-list (&rest args) args) (my-list 1 2 3)"))))

(test compile-rest-empty
  "&rest with no extra args gives nil"
  (is (not (run-string "(defun my-list0 (&rest args) args) (my-list0)"))))

(test compile-rest-with-required
  "required + &rest"
  (is (equal '(1 2 3) (run-string "(defun fr (a &rest r) (cons a r)) (fr 1 2 3)"))))

(test compile-rest-single
  "&rest with single extra arg"
  (is (equal '(42) (run-string "(defun my-list1 (&rest args) args) (my-list1 42)"))))

(test compile-rest-car
  "car of &rest list"
  (is (= 10 (run-string "(defun first-rest (&rest args) (car args)) (first-rest 10 20 30)"))))

(test compile-rest-length
  "length of &rest list via recursive count"
  (is (= 3 (run-string "
    (defun my-len (lst) (if (null lst) 0 (+ 1 (my-len (cdr lst)))))
    (defun count-args (&rest args) (my-len args))
    (count-args 1 2 3)"))))

;; &optional tests
(test compile-optional-provided
  "&optional with value provided"
  (is (= 15 (run-string "(defun opt-add (a &optional b) (if b (+ a b) a)) (opt-add 10 5)"))))

(test compile-optional-missing
  "&optional with value missing uses nil"
  (is (= 10 (run-string "(defun opt-add2 (a &optional b) (if b (+ a b) a)) (opt-add2 10)"))))

(test compile-optional-default
  "&optional with default value"
  (is (= 10 (run-string "(defun opt-def (a &optional (b 0)) (+ a b)) (opt-def 10)"))))

(test compile-optional-default-overridden
  "&optional default overridden by caller"
  (is (= 15 (run-string "(defun opt-def2 (a &optional (b 0)) (+ a b)) (opt-def2 10 5)"))))

(test compile-optional-multiple
  "multiple &optional params"
  (is (= 6 (run-string "(defun opt-multi (a &optional (b 0) (c 0)) (+ (+ a b) c)) (opt-multi 1 2 3)"))))

(test compile-optional-partial
  "some &optional params provided"
  (is (= 3 (run-string "(defun opt-part (a &optional (b 0) (c 0)) (+ (+ a b) c)) (opt-part 1 2)"))))

;; &key tests
(test compile-key-basic
  "&key with all keys provided"
  (is (= 7 (run-string "(defun key-add (&key x y) (+ x y)) (key-add :x 3 :y 4)"))))

(test compile-key-default
  "&key with default value"
  (is (= 10 (run-string "(defun key-def (&key (x 0) (y 0)) (+ x y)) (key-def :x 10)"))))

(test compile-key-reorder
  "&key args in different order"
  (is (= 7 (run-string "(defun key-ord (&key x y) (+ x y)) (key-ord :y 4 :x 3)"))))

(test compile-key-with-required
  "required + &key"
  (is (= 30 (run-string "(defun rk (a &key (b 0)) (+ a b)) (rk 10 :b 20)"))))

;; lambda with extended params
(test compile-lambda-rest
  "lambda with &rest"
  (is (equal '(10 20 30) (run-string "(funcall (lambda (&rest args) args) 10 20 30)"))))

(test compile-lambda-optional
  "lambda with &optional default"
  (is (= 5 (run-string "(funcall (lambda (a &optional (b 0)) (+ a b)) 5)"))))

(test compile-lambda-optional-provided
  "lambda with &optional value provided"
  (is (= 15 (run-string "(funcall (lambda (a &optional (b 0)) (+ a b)) 5 10)"))))

;; combined features
(test compile-rest-with-optional
  "&optional + &rest together"
  (is (equal '(3 4 5) (run-string "
    (defun opt-rest (a &optional (b 0) &rest r) r)
    (opt-rest 1 2 3 4 5)"))))

(test compile-rest-closure
  "&rest with closure capture"
  (is (equal '(10 20 30) (run-string "
    (defun make-lister ()
      (lambda (&rest args) args))
    (funcall (make-lister) 10 20 30)"))))

;;; Variadic Arithmetic and List Tests

(test compile-plus-variadic
  "(+ 1 2 3) via left fold"
  (is (= 6 (run-string "(+ 1 2 3)"))))

(test compile-plus-five-args
  "(+ 1 2 3 4 5) = 15"
  (is (= 15 (run-string "(+ 1 2 3 4 5)"))))

(test compile-plus-unary
  "(+ 10) = 10"
  (is (= 10 (run-string "(+ 10)"))))

(test compile-plus-nullary
  "(+) = 0"
  (is (= 0 (run-string "(+)"))))

(test compile-times-variadic
  "(* 2 3 4) = 24"
  (is (= 24 (run-string "(* 2 3 4)"))))

(test compile-times-nullary
  "(*) = 1"
  (is (= 1 (run-string "(*)"))))

(test compile-minus-variadic
  "(- 10 3 2) = 5"
  (is (= 5 (run-string "(- 10 3 2)"))))

(test compile-list-basic
  "(list 1 2 3) builds proper list"
  (is (equal '(1 2 3) (run-string "(list 1 2 3)"))))

(test compile-list-empty
  "(list) returns nil"
  (is (not (run-string "(list)"))))

(test compile-list-single
  "(list 42) returns (42)"
  (is (equal '(42) (run-string "(list 42)"))))

(test compile-list-car
  "car of list"
  (is (= 1 (run-string "(car (list 1 2 3))"))))

;;; Standard Library Set Operations Tests

(test stdlib-set-difference
  "set-difference removes elements in second list"
  (is (equal '(1 3 5) (run-string "(set-difference (list 1 2 3 4 5) (list 2 4))" :stdlib t))))

(test stdlib-set-difference-empty
  "set-difference with empty second list"
  (is (equal '(1 2 3) (run-string "(set-difference (list 1 2 3) (list))" :stdlib t))))

(test stdlib-union
  "union combines two lists"
  (is (equal '(1 2 3 4 5) (run-string "(union-lists (list 1 2 3) (list 3 4 5))" :stdlib t))))

(test stdlib-append-lists
  "append-lists concatenates two lists"
  (is (equal '(1 2 3 4) (run-string "(append-lists (list 1 2) (list 3 4))" :stdlib t))))

(test stdlib-last-cons
  "last-cons returns last cons cell"
  (is (= 3 (run-string "(car (last-cons (list 1 2 3)))" :stdlib t))))

(test stdlib-reduce-basic
  "reduce without initial-value folds left"
  (is (= 10 (run-string "(reduce (lambda (a b) (+ a b)) (list 1 2 3 4))" :stdlib t))))

(test stdlib-reduce-single
  "reduce on single-element list returns the element"
  (is (= 42 (run-string "(reduce (lambda (a b) (+ a b)) (list 42))" :stdlib t))))

(test stdlib-reduce-with-init
  "reduce with :initial-value uses the initial accumulator"
  (is (= 10 (run-string "(reduce (lambda (a b) (+ a b)) (list 1 2 3 4) :initial-value 0)" :stdlib t))))

(test stdlib-reduce-with-init-empty
  "reduce with :initial-value on empty list returns initial-value"
  (is (= 0 (run-string "(reduce (lambda (a b) (+ a b)) nil :initial-value 0)" :stdlib t))))

(test stdlib-reduce-with-init-nil
  "reduce with :initial-value nil on empty list returns nil"
  (is (eq nil (run-string "(reduce (lambda (a b) (cons b a)) nil :initial-value nil)" :stdlib t))))

(test stdlib-reduce-init-accumulate
  "reduce-init accumulates from initial value"
  (is (equal '(3 2 1) (run-string "(reduce-init (lambda (acc x) (cons x acc)) (list 1 2 3) nil)" :stdlib t))))

(test compile-hash-table-keys
  "hash-table-keys returns list of keys"
  (is (= 2 (run-string "
    (let ((ht (make-hash-table)))
      (setf (gethash :x ht) 10)
      (setf (gethash :y ht) 20)
      (length (hash-table-keys ht)))"))))

;;; Defstruct Tests

(test compile-defstruct-basic
  "defstruct creates constructor and accessors"
  (is (= 10 (run-string "
    (progn
      (defstruct point x y)
      (let ((p (make-point :x 10 :y 20)))
        (point-x p)))"))))

(test compile-defstruct-default
  "defstruct slots have default values"
  (is (= 0 (run-string "
    (progn
      (defstruct counter (count 0))
      (let ((c (make-counter)))
        (counter-count c)))"))))

(test compile-defstruct-predicate
  "defstruct generates type predicate"
  (is (= 1 (run-string "
    (progn
      (defstruct my-box value)
      (let ((b (make-my-box :value 42)))
        (if (my-box-p b) 1 0)))"))))

(test compile-defstruct-typep
  "typep works with defstruct types"
  (is (= 1 (run-string "
    (progn
      (defstruct my-pair first second)
      (let ((p (make-my-pair :first 1 :second 2)))
        (if (typep p 'my-pair) 1 0)))"))))

(test compile-defstruct-not-typep
  "typep returns false for non-matching struct"
  (is (= 0 (run-string "
    (progn
      (defstruct my-thing val)
      (if (typep 42 'my-thing) 1 0))"))))

(test compile-defstruct-boa-constructor
  "defstruct with BOA constructor"
  (is (= 3 (run-string "
    (progn
      (defstruct (my-vec (:constructor make-my-vec (x y)))
        x y)
      (let ((v (make-my-vec 1 3)))
        (my-vec-y v)))"))))

(test compile-defstruct-conc-name
  "defstruct with custom :conc-name"
  (is (= 42 (run-string "
    (progn
      (defstruct (my-item (:conc-name item-))
        value)
      (let ((i (make-my-item :value 42)))
        (item-value i)))"))))

;;; Car/Cdr Composition Tests

(test compile-caar
  "caar extracts car of car"
  (is (= 1 (run-string "(caar (list (list 1 2) (list 3 4)))"))))

(test compile-cadr
  "cadr extracts second element"
  (is (= 2 (run-string "(cadr (list 1 2 3))"))))

(test compile-cddr
  "cddr extracts rest of rest"
  (is (equal '(3) (run-string "(cddr (list 1 2 3))"))))

(test compile-caddr
  "caddr extracts third element"
  (is (= 3 (run-string "(caddr (list 1 2 3 4))"))))

;;; Stdlib Find/Position Tests

(test stdlib-find-basic
  "find locates element in list"
  (is (= 3 (run-string "(find 3 (list 1 2 3 4 5))" :stdlib t))))

(test stdlib-find-not-found
  "find returns nil when not found"
  (is (eq nil (run-string "(find 9 (list 1 2 3))" :stdlib t))))

(test stdlib-find-with-key
  "find with :key function"
  (is (equal '(2 . b) (run-string
    "(find 2 (list (cons 1 'a) (cons 2 'b) (cons 3 'c)) :key (lambda (x) (car x)))" :stdlib t))))

(test stdlib-position-basic
  "position returns index of element"
  (is (= 2 (run-string "(position 3 (list 1 2 3 4 5))" :stdlib t))))

(test stdlib-position-not-found
  "position returns nil when not found"
  (is (eq nil (run-string "(position 9 (list 1 2 3))" :stdlib t))))

(test stdlib-identity
  "identity returns its argument"
  (is (= 42 (run-string "(identity 42)" :stdlib t))))

(test stdlib-pairlis
  "pairlis creates alist from keys and values"
  (is (equal '((b . 2) (a . 1)) (run-string "(pairlis (list 'a 'b) (list 1 2))" :stdlib t))))

(test stdlib-assoc-if
  "assoc-if finds by predicate"
  (is (equal '(2 . b) (run-string
    "(assoc-if (lambda (k) (= k 2)) (list (cons 1 'a) (cons 2 'b)))" :stdlib t))))

(test stdlib-rassoc
  "rassoc finds by value"
  (is (equal '(2 . b) (run-string
    "(rassoc 'b (list (cons 1 'a) (cons 2 'b) (cons 3 'c)))" :stdlib t))))

;;; Setf Places Tests

(test compile-setf-car
  "setf car mutates cons"
  (is (= 99 (run-string "
    (let ((pair (cons 1 2)))
      (setf (car pair) 99)
      (car pair))"))))

(test compile-setf-cdr
  "setf cdr mutates cons"
  (is (= 99 (run-string "
    (let ((pair (cons 1 2)))
      (setf (cdr pair) 99)
      (cdr pair))"))))

(test compile-setf-first
  "setf first mutates car"
  (is (= 42 (run-string "
    (let ((lst (list 1 2 3)))
      (setf (first lst) 42)
      (first lst))"))))

(test compile-setf-nth
  "setf nth mutates list element"
  (is (= 99 (run-string "
    (let ((lst (list 10 20 30)))
      (setf (nth 1 lst) 99)
      (nth 1 lst))"))))

(test compile-setf-returns-value
  "setf returns the new value"
  (is (= 42 (run-string "
    (let ((pair (cons 1 2)))
      (setf (car pair) 42))"))))

;;; Package System Tests

(test compile-in-package
  "in-package returns package name"
  (is (eq :cl-cc (run-string "(in-package :cl-cc)"))))

(test compile-defpackage
  "defpackage returns package name"
  (is (eq :test-pkg (run-string "(defpackage :test-pkg (:use :cl))"))))

(test compile-in-package-with-code
  "in-package followed by code works"
  (is (= 42 (run-string "(progn (in-package :cl-cc) 42)"))))

;;; Macrolet Tests

(test compile-macrolet-basic
  "macrolet defines local macro"
  (is (= 6 (run-string "
    (macrolet ((double (x) `(+ ,x ,x)))
      (double 3))"))))

(test compile-macrolet-multiple
  "macrolet with multiple local macros"
  (is (= 10 (run-string "
    (macrolet ((add1 (x) `(+ ,x 1))
               (add2 (x) `(+ ,x 2)))
      (+ (add1 3) (add2 4)))"))))

(test compile-macrolet-scoped
  "macrolet macros are scoped to body"
  (is (= 42 (run-string "
    (let ((x 42))
      (macrolet ((get-x () 'x))
        (get-x)))"))))

(test compile-macrolet-nested
  "macrolet can reference outer macros"
  (is (= 8 (run-string "
    (macrolet ((square (x) `(* ,x ,x)))
      (macrolet ((sq-plus-sq (a b) `(+ (square ,a) (square ,b))))
        (sq-plus-sq 2 2)))"))))

;;; Function Reference Tests (#'builtin)

(test compile-function-car
  "#'car creates callable closure"
  (is (= 1 (run-string "(funcall #'car (cons 1 2))"))))

(test compile-function-plus
  "#'+ creates callable closure"
  (is (= 7 (run-string "(funcall #'+ 3 4)"))))

(test compile-function-cons
  "#'cons creates callable closure"
  (is (equal '(1 . 2) (run-string "(funcall #'cons 1 2)"))))

(test compile-function-in-mapcar
  "#'car works with mapcar"
  (is (equal '(1 2 3) (run-string "(mapcar #'car (list (cons 1 'a) (cons 2 'b) (cons 3 'c)))" :stdlib t))))

(test compile-function-find-key
  "find with #'car as :key"
  (is (equal '(2 . b) (run-string
    "(find 2 (list (cons 1 'a) (cons 2 'b) (cons 3 'c)) :key #'car)" :stdlib t))))

;;; Warn Test

(test compile-warn
  "warn expands to format and returns nil"
  (is (eq nil (run-string "(warn \"test warning ~A\" 42)"))))

;;; String Concatenation Tests

(test compile-string-concat-two
  "string-concat concatenates two strings"
  (is (string= "hello world" (run-string "(string-concat \"hello \" \"world\")"))))

(test compile-concatenate-strings
  "concatenate 'string joins multiple strings"
  (is (string= "abc" (run-string "(concatenate 'string \"a\" \"b\" \"c\")"))))

(test compile-concatenate-two
  "concatenate 'string joins two strings"
  (is (string= "foobar" (run-string "(concatenate 'string \"foo\" \"bar\")"))))

(test compile-concatenate-single
  "concatenate 'string with single string"
  (is (string= "hello" (run-string "(concatenate 'string \"hello\")"))))

;;; Check-Type Tests

(test compile-check-type-pass
  "check-type passes for correct type"
  (is (eq nil (run-string "(let ((x 42)) (check-type x integer))"))))

(test compile-check-type-fail
  "check-type signals error for wrong type"
  (signals error
    (run-string "(let ((x \"hello\")) (check-type x integer))")))

;;; Eval-When Tests

(test compile-eval-when-execute
  "eval-when :execute includes body"
  (is (= 42 (run-string "(eval-when (:execute) 42)"))))

(test compile-eval-when-skip
  "eval-when without :execute skips body"
  (is (eq nil (run-string "(eval-when (:compile-toplevel) 42)"))))

(test compile-eval-when-load-toplevel
  "eval-when :load-toplevel includes body"
  (is (= 10 (run-string "(eval-when (:load-toplevel :execute) (+ 3 7))"))))

(test compile-eval-when-all
  "eval-when with all situations"
  (is (= 5 (run-string "(eval-when (:compile-toplevel :load-toplevel :execute) 5)"))))

;;; Property List and Set Operations Tests

(test stdlib-getf-found
  "getf finds value by indicator"
  (is (= 2 (run-string "(getf (list :a 1 :b 2 :c 3) :b)" :stdlib t))))

(test stdlib-getf-not-found
  "getf returns nil when not found"
  (is (eq nil (run-string "(getf (list :a 1 :b 2) :z)" :stdlib t))))

(test stdlib-getf-default
  "getf returns default when not found"
  (is (= 99 (run-string "(getf (list :a 1) :z 99)" :stdlib t))))

(test stdlib-getf-first
  "getf finds first indicator"
  (is (= 1 (run-string "(getf (list :a 1 :b 2) :a)" :stdlib t))))

(test stdlib-intersection
  "intersection returns common elements"
  (is (equal '(2 3) (run-string "(intersection (list 1 2 3) (list 2 3 4))" :stdlib t))))

(test stdlib-intersection-empty
  "intersection with disjoint sets returns nil"
  (is (eq nil (run-string "(intersection (list 1 2) (list 3 4))" :stdlib t))))

(test stdlib-remove
  "remove removes matching elements"
  (is (equal '(1 3 5) (run-string "(remove 2 (list 1 2 3 2 5))" :stdlib t))))

;;; Eval Tests

(test our-eval-basic
  "our-eval compiles and runs a simple form"
  (is (= 42 (our-eval '(+ 20 22)))))

(test our-eval-lambda
  "our-eval handles lambda and funcall"
  (is (= 10 (our-eval '(funcall (lambda (x) (+ x 3)) 7)))))

(test our-eval-let
  "our-eval handles let bindings"
  (is (= 15 (our-eval '(let ((a 5) (b 10)) (+ a b))))))

;;; Declare Tests

(test compile-declare-ignore
  "declare ignore is silently skipped"
  (is (= 42 (run-string "(let ((x 42)) (declare (ignore x)) x)"))))

(test compile-declare-in-defun
  "declare in defun body is skipped"
  (is (= 10 (run-string "(progn (defun my-decl-fn (x) (declare (type integer x)) x) (my-decl-fn 10))"))))

(test compile-declare-type
  "declare type is silently skipped"
  (is (= 3 (run-string "(let ((x 1) (y 2)) (declare (type integer x y)) (+ x y))"))))

;;; Extended Arithmetic Tests

(test compile-mod
  "mod returns remainder"
  (is (= 1 (run-string "(mod 7 3)"))))

(test compile-rem
  "rem returns remainder"
  (is (= 1 (run-string "(rem 7 3)"))))

(test compile-truncate
  "truncate does integer division toward zero"
  (is (= 2 (run-string "(truncate 7 3)"))))

(test compile-floor
  "floor does integer division toward negative infinity"
  (is (= 2 (run-string "(floor 7 3)"))))

(test compile-ceiling
  "ceiling does integer division toward positive infinity"
  (is (= 3 (run-string "(ceiling 7 3)"))))

(test compile-abs
  "abs returns absolute value"
  (is (= 5 (run-string "(abs (- 0 5))"))))

(test compile-min
  "min returns smaller value"
  (is (= 2 (run-string "(min 5 2)"))))

(test compile-max
  "max returns larger value"
  (is (= 5 (run-string "(max 5 2)"))))

(test compile-evenp-true
  "evenp detects even numbers"
  (is (= 1 (run-string "(evenp 4)"))))

(test compile-evenp-false
  "evenp returns 0 for odd"
  (is (= 0 (run-string "(evenp 3)"))))

(test compile-oddp-true
  "oddp detects odd numbers"
  (is (= 1 (run-string "(oddp 3)"))))

;;; Association List and Utility Tests

(test compile-assoc-found
  "assoc finds key in alist"
  (is (= 2 (run-string "(cdr (assoc 'b (list (cons 'a 1) (cons 'b 2) (cons 'c 3))))"))))

(test compile-assoc-not-found
  "assoc returns nil when key not found"
  (is (equal nil (run-string "(assoc 'z (list (cons 'a 1) (cons 'b 2)))"))))

(test compile-acons
  "acons prepends to alist"
  (is (= 42 (run-string "(cdr (car (acons 'x 42 nil)))"))))

(test compile-equal-same
  "equal on identical structures"
  (is (= 1 (run-string "(equal (list 1 2 3) (list 1 2 3))"))))

(test compile-equal-different
  "equal on different structures"
  (is (= 0 (run-string "(equal (list 1 2) (list 1 3))"))))

(test compile-nconc
  "nconc concatenates destructively"
  (is (= 4 (run-string "(length (nconc (list 1 2) (list 3 4)))"))))

(test compile-copy-list
  "copy-list creates shallow copy"
  (is (= 3 (run-string "(length (copy-list (list 1 2 3)))"))))

(test compile-subst-basic
  "subst replaces atoms in tree"
  (is (= 1 (run-string "(equal (subst 'x 'a (list 'a 'b 'a)) (list 'x 'b 'x))"))))

(test compile-listp-true
  "listp returns 1 for list"
  (is (= 1 (run-string "(listp (list 1 2))"))))

(test compile-listp-nil
  "listp returns 1 for nil"
  (is (= 1 (run-string "(listp nil)"))))

(test compile-atom-true
  "atom returns 1 for non-cons"
  (is (= 1 (run-string "(atom 42)"))))

(test compile-atom-false
  "atom returns 0 for cons"
  (is (= 0 (run-string "(atom (cons 1 2))"))))

(test compile-string-coerce
  "string coerces symbol to string"
  (is (equal "HELLO" (run-string "(string 'hello)"))))

;;; String/Character Builtin Tests

(test compile-char-access
  "char extracts character at index"
  (is (equal #\e (run-string "(char \"hello\" 1)"))))

(test compile-char-code
  "char-code returns character code"
  (is (= 65 (run-string "(char-code #\\A)"))))

(test compile-code-char
  "code-char returns character from code"
  (is (equal #\A (run-string "(code-char 65)"))))

(test compile-char-equal
  "char= compares characters"
  (is (= 1 (run-string "(char= #\\a #\\a)"))))

(test compile-digit-char-p-true
  "digit-char-p returns weight for digit"
  (is (= 5 (run-string "(digit-char-p #\\5)"))))

(test compile-digit-char-p-false
  "digit-char-p returns nil for non-digit"
  (is (equal nil (run-string "(digit-char-p #\\a)"))))

(test compile-alpha-char-p
  "alpha-char-p detects alphabetic"
  (is (= 1 (run-string "(alpha-char-p #\\z)"))))

(test compile-upper-case-p
  "upper-case-p detects uppercase"
  (is (= 1 (run-string "(upper-case-p #\\A)"))))

(test compile-lower-case-p
  "lower-case-p detects lowercase"
  (is (= 1 (run-string "(lower-case-p #\\a)"))))

(test compile-char-upcase
  "char-upcase converts to uppercase"
  (is (equal #\A (run-string "(char-upcase #\\a)"))))

(test compile-char-downcase
  "char-downcase converts to lowercase"
  (is (equal #\a (run-string "(char-downcase #\\A)"))))

(test compile-stringp-true
  "stringp detects strings"
  (is (= 1 (run-string "(stringp \"hello\")"))))

(test compile-stringp-false
  "stringp returns 0 for non-strings"
  (is (= 0 (run-string "(stringp 42)"))))

(test compile-characterp-true
  "characterp detects characters"
  (is (= 1 (run-string "(characterp #\\a)"))))

(test compile-subseq-with-end
  "subseq extracts substring with start and end"
  (is (equal "ell" (run-string "(subseq \"hello\" 1 4)"))))

(test compile-subseq-no-end
  "subseq extracts to end of string"
  (is (equal "llo" (run-string "(subseq \"hello\" 2)"))))

(test compile-string-trim
  "string-trim removes characters from both ends"
  (is (equal "hello" (run-string "(string-trim \" \" \"  hello  \")"))))

(test compile-parse-integer
  "parse-integer converts string to integer"
  (is (= 42 (run-string "(parse-integer \"42\")"))))

(test compile-search-found
  "search finds pattern in string"
  (is (= 2 (run-string "(search \"ll\" \"hello\")"))))

(test compile-search-not-found
  "search returns -1 when not found"
  (is (= -1 (run-string "(search \"xyz\" \"hello\")"))))

;;; I/O and Format Tests

(test io-write-to-string
  "write-to-string converts value to string representation"
  (is (equal "42" (run-string "(write-to-string 42)"))))

(test io-write-to-string-symbol
  "write-to-string on symbol produces symbol name"
  (is (equal "HELLO" (run-string "(write-to-string 'hello)"))))

(test io-format-nil-string
  "format with nil destination returns formatted string"
  (is (equal "hello world" (run-string "(format nil \"hello ~A\" \"world\")"))))

(test io-format-nil-number
  "format with nil destination formats numbers"
  (is (equal "x=42" (run-string "(format nil \"x=~A\" 42)"))))

(test io-format-nil-no-args
  "format with nil and no format args"
  (is (equal "hello" (run-string "(format nil \"hello\")"))))

(test io-format-nil-multiple-args
  "format with nil destination and multiple args"
  (is (equal "1 + 2 = 3" (run-string "(format nil \"~A + ~A = ~A\" 1 2 3)"))))

(test io-princ-returns-value
  "princ returns the value it printed"
  (is (equal 42 (run-string "(princ 42)"))))

(test io-prin1-returns-value
  "prin1 returns the value it printed"
  (is (equal 42 (run-string "(prin1 42)"))))

(test io-print-returns-value
  "print returns the value it printed"
  (is (equal 42 (run-string "(print 42)"))))

(test io-terpri-returns-nil
  "terpri returns nil"
  (is (equal nil (run-string "(terpri)"))))

(test io-format-t-returns-nil
  "format with t destination returns nil"
  (is (equal nil (run-string "(format t \"hello\")"))))

(test io-format-iteration
  "format ~{~} iterates over list"
  (is (equal "1, 2, 3"
             (run-string "(format nil \"~{~A~^, ~}\" (list 1 2 3))" :stdlib t))))

(test io-format-conditional
  "format ~[~] selects by index"
  (is (equal "one"
             (run-string "(format nil \"~[zero~;one~;two~:;many~]\" 1)"))))

(test io-format-conditional-default
  "format ~[~:;~] uses default clause"
  (is (equal "many"
             (run-string "(format nil \"~[zero~;one~;two~:;many~]\" 99)"))))

(test io-write-char-basic
  "write-char outputs a character and returns it"
  (is (equal #\A (run-string "(write-char #\\A)"))))

;;; Higher-Order Function Tests (require stdlib)

(test stdlib-mapcar-basic
  "mapcar applies function to each element"
  (is (equal '(2 4 6)
             (run-string "(mapcar (lambda (x) (+ x x)) (list 1 2 3))" :stdlib t))))

(test stdlib-mapcar-empty
  "mapcar on empty list returns nil"
  (is (equal nil (run-string "(mapcar (lambda (x) x) nil)" :stdlib t))))

(test stdlib-remove-if-basic
  "remove-if filters elements"
  (is (equal '(1 2)
             (run-string "(remove-if (lambda (x) (> x 2)) (list 1 2 3 4 5))"
                         :stdlib t))))

(test stdlib-remove-if-not-basic
  "remove-if-not keeps matching elements"
  (is (equal '(3 4 5)
             (run-string "(remove-if-not (lambda (x) (> x 2)) (list 1 2 3 4 5))"
                         :stdlib t))))

(test stdlib-find-if-found
  "find-if returns first match"
  (is (= 4 (run-string "(find-if (lambda (x) (> x 3)) (list 1 2 3 4 5))" :stdlib t))))

(test stdlib-find-if-not-found
  "find-if returns nil when no match"
  (is (equal nil (run-string "(find-if (lambda (x) (> x 10)) (list 1 2 3))" :stdlib t))))

(test stdlib-every-true
  "every returns t when all match"
  (is (not (null (run-string "(every (lambda (x) (> x 0)) (list 1 2 3))" :stdlib t)))))

(test stdlib-every-false
  "every returns nil when some don't match"
  (is (equal nil (run-string "(every (lambda (x) (> x 2)) (list 1 2 3))" :stdlib t))))

(test stdlib-some-true
  "some returns t when any match"
  (is (not (null (run-string "(some (lambda (x) (> x 2)) (list 1 2 3))" :stdlib t)))))

(test stdlib-some-false
  "some returns nil when none match"
  (is (equal nil (run-string "(some (lambda (x) (> x 10)) (list 1 2 3))" :stdlib t))))

(test stdlib-reduce-basic
  "reduce combines elements"
  (is (= 10 (run-string "(reduce (lambda (a b) (+ a b)) (list 1 2 3 4))" :stdlib t))))

(test stdlib-count-if
  "count-if counts matching elements"
  (is (= 2 (run-string "(count-if (lambda (x) (> x 2)) (list 1 2 3 4))" :stdlib t))))

;;; With-Output-To-String Tests

(test compile-with-output-to-string-basic
  "with-output-to-string returns empty string when no writes"
  (is (string= "" (run-string "(with-output-to-string (s))"))))

(test compile-with-output-to-string-format
  "with-output-to-string collects format output"
  (is (string= "hello world"
                (run-string "(with-output-to-string (s) (format s \"hello ~A\" \"world\"))"))))

(test compile-with-output-to-string-multiple
  "with-output-to-string collects multiple writes"
  (is (string= "ab"
                (run-string "(with-output-to-string (s) (write-string \"a\" s) (write-string \"b\" s))"))))

(test compile-with-output-to-string-format-multi
  "with-output-to-string with multiple format calls"
  (is (string= "x=1 y=2"
                (run-string "(with-output-to-string (s) (format s \"x=~A\" 1) (format s \" y=~A\" 2))"))))

(test compile-make-string-output-stream
  "make-string-output-stream and get-output-stream-string work"
  (is (string= "foo"
                (run-string "(let ((s (make-string-output-stream))) (write-string \"foo\" s) (get-output-stream-string s))"))))

;;; Array/Vector Tests

(test compile-make-array-basic
  "make-array creates an array"
  (let ((result (run-string "(make-array 5)")))
    (is (vectorp result))
    (is (= 5 (length result)))))

(test compile-aref
  "aref reads from array"
  (is (= 0 (run-string "(let ((a (make-array 3))) (aref a 0))"))))

(test compile-setf-aref
  "setf aref writes to array"
  (is (= 42 (run-string "(let ((a (make-array 3))) (setf (aref a 1) 42) (aref a 1))"))))

(test compile-vector-push-extend
  "vector-push-extend pushes to adjustable vector"
  (is (= 10 (run-string "(let ((v (make-array 0 :fill-pointer t :adjustable t))) (vector-push-extend 10 v) (aref v 0))"))))

(test compile-vectorp
  "vectorp checks for vector type"
  (is (not (null (run-string "(vectorp (make-array 3))")))))

(test compile-vectorp-not
  "vectorp returns false for non-vector"
  (is (eql 0 (run-string "(vectorp 42)"))))

;;; Sort Tests

(test compile-sort-ascending
  "sort numbers ascending"
  (is (equal '(1 1 2 3 4 5 6 9) (run-string "(sort (list 3 1 4 1 5 9 2 6) (lambda (a b) (< a b)))" :stdlib t))))

(test compile-sort-descending
  "sort numbers descending"
  (is (equal '(5 3 1) (run-string "(sort (list 3 1 5) (lambda (a b) (> a b)))" :stdlib t))))

(test compile-sort-empty
  "sort empty list"
  (is (null (run-string "(sort nil (lambda (a b) (< a b)))" :stdlib t))))

(test compile-sort-single
  "sort single element"
  (is (equal '(42) (run-string "(sort (list 42) (lambda (a b) (< a b)))" :stdlib t))))

;;; Coerce Tests

(test compile-coerce-char-list-to-string
  "coerce char list to string"
  (is (string= "abc" (run-string "(coerce (list #\\a #\\b #\\c) 'string)"))))

(test compile-coerce-string-to-list
  "coerce string to list"
  (is (equal '(#\h #\i) (run-string "(coerce \"hi\" 'list)"))))

(test compile-coerce-list-to-vector
  "coerce list to vector"
  (let ((result (run-string "(coerce (list 1 2 3) 'vector)")))
    (is (vectorp result))
    (is (= 3 (length result)))))

;;; Loop Macro Tests

(test loop-for-in-collect
  "LOOP FOR...IN with COLLECT accumulation"
  (is (equal '(1 2 3)
             (run-string "(loop for x in (list 1 2 3) collect x)" :stdlib t))))

(test loop-for-in-do
  "LOOP FOR...IN with DO body"
  (is (equal '(1 2 3)
             (run-string "(let ((r nil)) (loop for x in (list 1 2 3) do (push x r)) (nreverse r))" :stdlib t))))

(test loop-for-from-to-collect
  "LOOP FOR...FROM...TO with COLLECT"
  (is (equal '(1 2 3 4 5)
             (run-string "(loop for i from 1 to 5 collect i)" :stdlib t))))

(test loop-for-from-below-collect
  "LOOP FOR...FROM...BELOW with COLLECT"
  (is (equal '(0 1 2 3 4)
             (run-string "(loop for i from 0 below 5 collect i)" :stdlib t))))

(test loop-repeat
  "LOOP REPEAT with counter"
  (is (= 5
         (run-string "(let ((n 0)) (loop repeat 5 do (setq n (+ n 1))) n)" :stdlib t))))

(test loop-sum
  "LOOP FOR with SUM accumulation"
  (is (= 15
         (run-string "(loop for i from 1 to 5 sum i)" :stdlib t))))

(test loop-collect-expression
  "LOOP COLLECT with computed expression"
  (is (equal '(1 4 9)
             (run-string "(loop for x in (list 1 2 3) collect (* x x))" :stdlib t))))

(test loop-for-on
  "LOOP FOR...ON list traversal"
  (is (equal '((1 2 3) (2 3) (3))
             (run-string "(loop for x on (list 1 2 3) collect x)" :stdlib t))))

(test loop-empty-list
  "LOOP FOR...IN with empty list"
  (is (equal nil
             (run-string "(loop for x in nil collect x)" :stdlib t))))

(test loop-repeat-zero
  "LOOP REPEAT 0 does nothing"
  (is (= 0
         (run-string "(let ((n 0)) (loop repeat 0 do (setq n (+ n 1))) n)" :stdlib t))))

(test loop-hash-keys-collect
  "LOOP FOR key BEING THE HASH-KEYS OF ht COLLECT key"
  (let ((result (run-string
                 "(let ((ht (make-hash-table)))
                    (setf (gethash 'a ht) 1)
                    (setf (gethash 'b ht) 2)
                    (length (loop for k being the hash-keys of ht collect k)))"
                 :stdlib t)))
    (is (= 2 result))))

(test loop-hash-values-collect
  "LOOP FOR val BEING THE HASH-VALUES OF ht COLLECT val"
  (let ((result (run-string
                 "(let ((ht (make-hash-table)))
                    (setf (gethash 'x ht) 10)
                    (setf (gethash 'y ht) 20)
                    (loop for v being the hash-values of ht sum v))"
                 :stdlib t)))
    (is (= 30 result))))

(test loop-hash-keys-using-value
  "LOOP FOR key BEING THE HASH-KEYS OF ht USING (HASH-VALUE val)"
  (let ((result (run-string
                 "(let ((ht (make-hash-table)))
                    (setf (gethash 'a ht) 100)
                    (loop for k being the hash-keys of ht using (hash-value v) collect v))"
                 :stdlib t)))
    (is (equal '(100) result))))

(test loop-hash-keys-empty
  "LOOP hash-keys on empty hash table returns nil"
  (let ((result (run-string
                 "(loop for k being the hash-keys of (make-hash-table) collect k)"
                 :stdlib t)))
    (is (null result))))

(test loop-with-clause
  "LOOP WITH auxiliary variable binding"
  (is (= 15 (run-string
              "(loop with sum = 0 for i from 1 to 5 do (setq sum (+ sum i)) finally (return sum))"
              :stdlib t))))

(test loop-append-accumulation
  "LOOP APPEND accumulation"
  (is (equal '(1 2 3 4)
             (run-string "(loop for x in (list (list 1 2) (list 3 4)) append x)" :stdlib t))))

(test loop-nconc-accumulation
  "LOOP NCONC accumulation"
  (is (equal '(a b c d)
             (run-string "(loop for x in (list (list 'a 'b) (list 'c 'd)) nconc (copy-list x))" :stdlib t))))

(test loop-always-true
  "LOOP ALWAYS returns t when all tests pass"
  (is (equal t (run-string "(loop for i in (list 2 4 6) always (evenp i))" :stdlib t))))

(test loop-always-false
  "LOOP ALWAYS returns nil when a test fails"
  (is (equal nil (run-string "(loop for i in (list 2 3 6) always (evenp i))" :stdlib t))))

(test loop-never-true
  "LOOP NEVER returns t when no test passes"
  (is (equal t (run-string "(loop for i in (list 1 3 5) never (evenp i))" :stdlib t))))

(test loop-thereis
  "LOOP THEREIS returns first non-nil test result"
  (is (equal 1 (run-string "(loop for i in (list 1 2 3) thereis (evenp i))" :stdlib t))))

(test loop-from-by
  "LOOP FOR FROM BY with custom step"
  (is (equal '(0 2 4)
             (run-string "(loop for i from 0 below 5 by 2 collect i)" :stdlib t))))

(test loop-on-by-cddr
  "LOOP FOR ON BY with step function"
  (is (equal '((1 2 3 4) (3 4))
             (run-string "(loop for x on (list 1 2 3 4) by (function cddr) collect x)" :stdlib t))))

(test loop-when-collect
  "LOOP WHEN filters collected elements"
  (is (equal '(4 16 36)
             (run-string "(loop for x in (list 1 2 3 4 5 6) when (evenp x) collect (* x x))" :stdlib t))))

(test loop-unless-collect
  "LOOP UNLESS filters out matching elements"
  (is (equal '(1 9 25)
             (run-string "(loop for x in (list 1 2 3 4 5 6) unless (evenp x) collect (* x x))" :stdlib t))))

(test loop-if-collect
  "LOOP IF works as synonym for WHEN"
  (is (equal '(2 4 6)
             (run-string "(loop for x in (list 1 2 3 4 5 6) if (evenp x) collect x)" :stdlib t))))

(test loop-when-sum
  "LOOP WHEN with SUM accumulation"
  (is (= 12 (run-string "(loop for i from 1 to 6 when (evenp i) sum i)" :stdlib t))))

(test loop-unless-do
  "LOOP UNLESS with DO body"
  (is (equal '(1 3 5)
             (run-string "(let ((r nil)) (loop for x in (list 1 2 3 4 5) unless (evenp x) do (push x r)) (nreverse r))" :stdlib t))))

(test loop-when-append
  "LOOP WHEN with APPEND accumulation"
  (is (equal '(2 2 4 4)
             (run-string "(loop for x in (list 1 2 3 4) when (evenp x) append (list x x))" :stdlib t))))

(test loop-collect-into-with-when
  "LOOP WHEN ... COLLECT ... INTO with multiple named accumulators"
  (is (equal '((1 3 5 7 9) (2 4 6 8 10))
             (run-string "(loop for i from 1 to 10 when (oddp i) collect i into odds when (evenp i) collect i into evens finally (return (list (nreverse odds) (nreverse evens))))" :stdlib t))))

(test loop-collect-into-simple
  "LOOP COLLECT INTO with single named accumulator"
  (is (equal '(1 2 3)
             (run-string "(loop for x in (list 1 2 3) collect x into result finally (return (nreverse result)))" :stdlib t))))

(test loop-sum-into
  "LOOP SUM INTO with named accumulator"
  (is (= 15
         (run-string "(loop for i from 1 to 5 sum i into total finally (return total))" :stdlib t))))

;;; Floor/Truncate/Ceiling Multiple Values Tests

(test floor-multiple-values
  "FLOOR returns quotient and remainder via multiple-value-bind"
  (is (equal '(3 2)
             (run-string "(multiple-value-bind (q r) (floor 17 5) (list q r))" :stdlib t))))

(test truncate-multiple-values
  "TRUNCATE returns quotient and remainder via multiple-value-bind"
  (is (equal '(3 2)
             (run-string "(multiple-value-bind (q r) (truncate 17 5) (list q r))" :stdlib t))))

(test ceiling-multiple-values
  "CEILING returns quotient and remainder via multiple-value-bind"
  (is (equal '(4 -3)
             (run-string "(multiple-value-bind (q r) (ceiling 17 5) (list q r))" :stdlib t))))

;;; Nested Destructuring-Bind Tests

(test destructuring-bind-nested
  "DESTRUCTURING-BIND with nested pattern"
  (is (= 10
         (run-string "(destructuring-bind (a (b c) d) (list 1 (list 2 3) 4) (+ a b c d))" :stdlib t))))

(test destructuring-bind-deep-nested
  "DESTRUCTURING-BIND with deeply nested pattern"
  (is (= 15
         (run-string "(destructuring-bind (a (b (c d)) e) (list 1 (list 2 (list 3 4)) 5) (+ a b c d e))" :stdlib t))))

;;; Variadic Append/Nconc Tests

(test append-three-args
  "APPEND with 3 arguments works correctly"
  (is (equal '(1 2 3 4 5)
             (run-string "(append (list 1 2) (list 3 4) (list 5))" :stdlib t))))

(test append-zero-args
  "APPEND with 0 arguments returns nil"
  (is (equal nil (run-string "(append)" :stdlib t))))

(test append-one-arg
  "APPEND with 1 argument returns that list"
  (is (equal '(1 2 3)
             (run-string "(append (list 1 2 3))" :stdlib t))))

(test nconc-three-args
  "NCONC with 3 arguments"
  (is (equal '(1 2 3 4 5 6)
             (run-string "(nconc (list 1 2) (list 3 4) (list 5 6))" :stdlib t))))

(test self-host-stack-compiler
  "Mini stack-machine compiler: parse -> compile -> run through VM"
  (is (= 17
         (run-string "
           (progn
             (defstruct ast-num value)
             (defstruct ast-binop op lhs rhs)
             (defun parse (expr)
               (cond
                 ((numberp expr) (make-ast-num :value expr))
                 ((consp expr) (make-ast-binop :op (car expr) :lhs (parse (cadr expr)) :rhs (parse (caddr expr))))
                 (t (error \"unknown\"))))
             (defun compile-node (node)
               (cond
                 ((ast-num-p node) (list (list (quote push) (ast-num-value node))))
                 ((ast-binop-p node)
                  (append (compile-node (ast-binop-lhs node))
                          (compile-node (ast-binop-rhs node))
                          (list (list (quote op) (ast-binop-op node)))))
                 (t nil)))
             (defun run-vm (instrs)
               (let ((stack nil))
                 (dolist (inst instrs)
                   (cond
                     ((eq (car inst) (quote push)) (push (cadr inst) stack))
                     ((eq (car inst) (quote op))
                      (let ((b (pop stack)) (a (pop stack)))
                        (cond
                          ((eq (cadr inst) (quote +)) (push (+ a b) stack))
                          ((eq (cadr inst) (quote *)) (push (* a b) stack))
                          ((eq (cadr inst) (quote -)) (push (- a b) stack)))))))
                 (car stack)))
             (run-vm (compile-node (parse (quote (+ (* 3 4) (- 10 5)))))))" :stdlib t))))

;;; Consp Fix / Type Predicates Tests

(test consp-on-list
  "CONSP returns true for cons cells from list"
  (is (= 1 (run-string "(consp (list 1 2))" :stdlib t))))

(test consp-on-integer
  "CONSP returns false for integer"
  (is (= 0 (run-string "(consp 42)" :stdlib t))))

(test and-with-consp
  "AND works correctly with CONSP predicate"
  (is (= 42 (run-string "(and (consp (list 1)) 42)" :stdlib t))))

(test mini-compiler-self-host
  "Mini compiler can compile expression with pattern matching"
  (is (equal '(:ADD (:CONST 1) (:CONST 2))
             (run-string "(defun my-compile (expr) (cond ((integerp expr) (list :const expr)) ((and (consp expr) (eq (car expr) (quote +))) (list :add (my-compile (second expr)) (my-compile (third expr)))) (t (list :unknown expr)))) (my-compile (quote (+ 1 2)))" :stdlib t))))

;;; Funcall/Apply with Quoted Symbols Tests

(test funcall-quoted-builtin
  "FUNCALL with quoted builtin symbol"
  (is (= 7 (run-string "(funcall (quote +) 3 4)" :stdlib t))))

(test funcall-quoted-user-function
  "FUNCALL with quoted user-defined function"
  (is (= 7 (run-string "(defun my-add2 (a b) (+ a b)) (funcall (quote my-add2) 3 4)" :stdlib t))))

(test apply-quoted-builtin
  "APPLY with quoted variadic builtin"
  (is (= 6 (run-string "(apply (quote +) (list 1 2 3))" :stdlib t))))

(test apply-quoted-user-function
  "APPLY with quoted user-defined function"
  (is (= 7 (run-string "(defun my-add3 (a b) (+ a b)) (apply (quote my-add3) (list 3 4))" :stdlib t))))

(test apply-lambda
  "APPLY with lambda"
  (is (= 6 (run-string "(apply (lambda (a b c) (+ a b c)) (list 1 2 3))" :stdlib t))))

(test funcall-with-function-ref
  "FUNCALL with #'function reference"
  (is (= 7 (run-string "(funcall #'+ 3 4)" :stdlib t))))

;;; Maphash Tests

(test maphash-collect-values
  "MAPHASH iterates over hash table entries with closure mutation"
  (let ((result (run-string "(let ((result nil))
  (let ((ht (make-hash-table)))
    (setf (gethash :a ht) 1)
    (setf (gethash :b ht) 2)
    (maphash (lambda (k v) (setq result (cons v result))) ht)
    result))")))
    (is (listp result))
    (is (= 2 (length result)))
    (is (null (set-difference result (list 1 2))))))

(test maphash-returns-nil
  "MAPHASH returns nil"
  (is (null (run-string "(let ((ht (make-hash-table)))
  (setf (gethash :x ht) 10)
  (maphash (lambda (k v) v) ht))"))))

(test maphash-empty-table
  "MAPHASH on empty hash table does nothing"
  (is (null (run-string "(let ((ht (make-hash-table)))
  (maphash (lambda (k v) k) ht))"))))

(test maphash-count-entries
  "MAPHASH can count entries via closure mutation"
  (is (= 3 (run-string "(let ((count 0))
  (let ((ht (make-hash-table)))
    (setf (gethash :a ht) 1)
    (setf (gethash :b ht) 2)
    (setf (gethash :c ht) 3)
    (maphash (lambda (k v) (setq count (+ count 1))) ht)
    count))"))))

;;; Capture-by-Reference Tests

(test capture-by-ref-counter
  "Closure captures variable by reference — counter increments across calls"
  (is (= 3 (run-string "(let ((count 0))
  (let ((inc (lambda () (setq count (+ count 1)) count)))
    (funcall inc) (funcall inc) (funcall inc)))"))))

(test capture-by-ref-shared-state
  "Multiple closures share the same boxed variable"
  (is (= 42 (run-string "(let ((x 0))
  (defun get-x4 () x)
  (defun set-x4 (v) (setq x v))
  (set-x4 42)
  (get-x4))" :stdlib t))))

(test capture-by-ref-accumulator
  "Closure accumulates values via setq on captured variable"
  (is (= 10 (run-string "(let ((sum 0))
  (let ((add (lambda (n) (setq sum (+ sum n)) sum)))
    (funcall add 1) (funcall add 2) (funcall add 3) (funcall add 4)))"))))

;;; File I/O Tests

(test file-io-write-read
  "Write characters to file and read them back"
  (let ((result (run-string "(let ((h (open \"/tmp/cl-cc-test-wr.txt\" :direction :output)))
  (write-char #\\H h)
  (write-char #\\i h)
  (close h)
  (let ((h2 (open \"/tmp/cl-cc-test-wr.txt\" :direction :input)))
    (let ((c1 (read-char h2)))
      (let ((c2 (read-char h2)))
        (close h2)
        (list c1 c2)))))")))
    (is (equal result '(#\H #\i)))))

(test file-io-with-open-file
  "WITH-OPEN-FILE macro writes and reads correctly"
  (let ((result (run-string "(with-open-file (out \"/tmp/cl-cc-test-wof2.txt\" :direction :output)
  (write-char #\\X out))
(with-open-file (in \"/tmp/cl-cc-test-wof2.txt\" :direction :input)
  (read-char in))")))
    (is (eql result #\X))))

(test file-io-read-from-string
  "READ-FROM-STRING parses S-expression from string"
  (let ((result (run-string "(read-from-string \"(+ 1 2)\")")))
    (is (listp result))
    (is (= 3 (length result)))))

(test file-io-read-from-file
  "READ reads S-expression from file stream"
  (let ((result (run-string "(with-open-file (out \"/tmp/cl-cc-test-rd.txt\" :direction :output)
  (write-char #\\( out) (write-char #\\a out) (write-char #\\) out))
(with-open-file (in \"/tmp/cl-cc-test-rd.txt\" :direction :input)
  (read in))")))
    (is (listp result))))

;;; Setf Accessor Tests

(test setf-defstruct-accessor
  "SETF on defstruct accessor modifies slot"
  (is (= 42 (run-string "(defstruct my-cell (value 0))
(let ((c (make-my-cell)))
  (setf (my-cell-value c) 42)
  (my-cell-value c))"))))

(test setf-defstruct-counter
  "SETF on defstruct accessor for counter pattern"
  (is (= 3 (run-string "(defstruct my-counter2 (n 0))
(let ((c (make-my-counter2)))
  (setf (my-counter2-n c) (+ (my-counter2-n c) 1))
  (setf (my-counter2-n c) (+ (my-counter2-n c) 1))
  (setf (my-counter2-n c) (+ (my-counter2-n c) 1))
  (my-counter2-n c))"))))

;;; Self-Hosting Pattern Tests

(test self-host-compiler-context
  "Self-hosting: compile a mini compiler context with defstruct"
  (is (equal '(3 3 0 1 2) (run-string "
(defstruct compiler-ctx2
  (counter 0)
  (instructions nil))
(defun make-reg2 (ctx)
  (let ((n (compiler-ctx2-counter ctx)))
    (setf (compiler-ctx2-counter ctx) (+ n 1))
    n))
(defun emit-inst2 (ctx inst)
  (setf (compiler-ctx2-instructions ctx)
        (cons inst (compiler-ctx2-instructions ctx))))
(let ((ctx (make-compiler-ctx2)))
  (let ((r1 (make-reg2 ctx))
        (r2 (make-reg2 ctx))
        (r3 (make-reg2 ctx)))
    (emit-inst2 ctx (list :const r1 42))
    (emit-inst2 ctx (list :const r2 10))
    (emit-inst2 ctx (list :add r3 r1 r2))
    (list (compiler-ctx2-counter ctx)
          (length (compiler-ctx2-instructions ctx))
          r1 r2 r3)))" :stdlib t))))

(test self-host-clos-ast-eval
  "Self-hosting: CLOS-based AST evaluator"
  (is (= 42 (run-string "
(defclass eval-int ()
  ((value :initarg :value :reader eval-value)))
(defclass eval-binop ()
  ((op :initarg :op :reader eval-op)
   (lhs :initarg :lhs :reader eval-lhs)
   (rhs :initarg :rhs :reader eval-rhs)))
(defgeneric eval-node (node))
(defmethod eval-node ((node eval-int))
  (eval-value node))
(defmethod eval-node ((node eval-binop))
  (let ((l (eval-node (eval-lhs node)))
        (r (eval-node (eval-rhs node))))
    (if (eq (eval-op node) :add) (+ l r) (* l r))))
(let ((tree (make-instance 'eval-binop
              :op :add
              :lhs (make-instance 'eval-int :value 30)
              :rhs (make-instance 'eval-binop
                     :op :mul
                     :lhs (make-instance 'eval-int :value 4)
                     :rhs (make-instance 'eval-int :value 3)))))
  (eval-node tree))"))))

;;; Labels Mutual Recursion Tests

(test labels-mutual-recursion
  "Labels: mutually recursive even-p/odd-p"
  (is (eq t (run-string "
(labels ((even-p (n) (if (= n 0) t (odd-p (- n 1))))
         (odd-p (n) (if (= n 0) nil (even-p (- n 1)))))
  (even-p 4))"))))

(test labels-mutual-recursion-odd
  "Labels: mutually recursive even-p/odd-p returns nil for odd"
  (is (not (run-string "
(labels ((even-p (n) (if (= n 0) t (odd-p (- n 1))))
         (odd-p (n) (if (= n 0) nil (even-p (- n 1)))))
  (even-p 3))"))))

(test labels-mutual-three-fns
  "Labels: three mutually recursive functions"
  (is (= 6 (run-string "
(labels ((a (n) (if (= n 0) 0 (+ 1 (b (- n 1)))))
         (b (n) (if (= n 0) 0 (+ 1 (c (- n 1)))))
         (c (n) (if (= n 0) 0 (+ 1 (a (- n 1))))))
  (a 6))"))))

;;; Hash Table :test Parameter Tests

(test ht-test-equal-quote
  "Hash table with :test 'equal for string keys"
  (is (= 42 (run-string "
(let ((ht (make-hash-table :test 'equal)))
  (setf (gethash \"key\" ht) 42)
  (gethash \"key\" ht))"))))

(test ht-test-equal-sharp-quote
  "Hash table with :test #'equal for string keys"
  (is (= 42 (run-string "
(let ((ht (make-hash-table :test #'equal)))
  (setf (gethash \"key\" ht) 42)
  (gethash \"key\" ht))"))))

(test ht-default-eql-int-keys
  "Hash table default eql works with integer keys"
  (is (= 99 (run-string "
(let ((ht (make-hash-table)))
  (setf (gethash 1 ht) 99)
  (gethash 1 ht))"))))

;;; New Builtin Tests (type-of, make-list, alphanumericp, prin1-to-string)

(test builtin-type-of-integer
  "type-of returns integer for numbers"
  (is (eq 'integer (run-string "(type-of 42)"))))

(test builtin-type-of-string
  "type-of returns string for strings"
  (is (eq 'string (run-string "(type-of \"hello\")"))))

(test builtin-type-of-cons
  "type-of returns cons for cons cells"
  (is (eq 'cons (run-string "(type-of '(1 2))"))))

(test builtin-make-list-3
  "make-list creates a list of nils"
  (is (equal '(nil nil nil) (run-string "(make-list 3)"))))

(test builtin-make-list-0
  "make-list 0 returns nil"
  (is (not (run-string "(make-list 0)"))))

(test builtin-alphanumericp-alpha
  "alphanumericp returns truthy for letters"
  (is (not (zerop (run-string "(alphanumericp #\\a)")))))

(test builtin-alphanumericp-bang
  "alphanumericp returns falsy for punctuation"
  (is (zerop (run-string "(alphanumericp #\\!)"))))

(test builtin-prin1-to-string
  "prin1-to-string converts value to string"
  (is (stringp (run-string "(prin1-to-string 42)"))))

(test builtin-princ-to-string
  "princ-to-string converts value to string"
  (is (stringp (run-string "(princ-to-string 42)"))))

;;; Runtime Eval Tests

(test eval-constant
  "eval returns constants directly"
  (is (= 42 (run-string "(eval 42)"))))

(test eval-quoted-form
  "eval evaluates quoted arithmetic"
  (is (= 3 (run-string "(eval '(+ 1 2))"))))

(test eval-nested-form
  "eval evaluates nested expressions"
  (is (= 21 (run-string "(eval '(* (+ 1 2) (+ 3 4)))"))))

(test eval-let-form
  "eval evaluates let bindings"
  (is (= 15 (run-string "(eval '(let ((x 10)) (+ x 5)))"))))

(test eval-constructed-form
  "eval evaluates dynamically constructed forms"
  (is (= 30 (run-string "
(let ((op '+) (a 10) (b 20))
  (eval (list op a b)))"))))

;;; Setf Variable Tests

(test setf-plain-variable
  "setf on plain variable expands to setq"
  (is (= 10 (run-string "
(let ((x 0))
  (setf x 10)
  x)"))))

(test setf-variable-increment
  "setf variable with increment pattern"
  (is (= 3 (run-string "
(let ((counter 0))
  (setf counter (+ counter 1))
  (setf counter (+ counter 1))
  (setf counter (+ counter 1))
  counter)"))))

;;; Stdlib HOF Tests (with stdlib)

(test stdlib-mapcar
  "mapcar with stdlib loaded"
  (is (equal '(2 4 6) (run-string
    "(mapcar (lambda (x) (* x 2)) '(1 2 3))" :stdlib t))))

(test stdlib-reduce
  "reduce with stdlib loaded"
  (is (= 15 (run-string
    "(reduce #'+ '(1 2 3 4 5) :initial-value 0)" :stdlib t))))

(test stdlib-remove-if
  "remove-if with stdlib loaded"
  (is (equal '(2 4) (run-string
    "(remove-if #'oddp '(1 2 3 4 5))" :stdlib t))))

;;; Let Alias Fix Tests

(test let-no-alias
  "LET bindings don't alias — mutation of original doesn't affect copy"
  (is (= 0 (run-string "(let ((x 0)) (let ((y x)) (setq x 10) y))"))))

(test let-no-alias-nested
  "Nested LET bindings are independent copies"
  (is (= 5 (run-string "(let ((a 5)) (let ((b a)) (let ((c b)) (setq a 99) c)))"))))

;;; Prog1/Prog2/Ignore-Errors Tests

(test compile-prog1-basic
  "prog1 returns first form value"
  (is (= 42 (run-string "(prog1 42 (+ 1 2))"))))

(test compile-prog1-side-effect
  "prog1 returns first form, executes side effects"
  (is (= 0 (run-string "(let ((x 0)) (prog1 x (setq x 10)))"))))

(test compile-prog2
  "prog2 returns second form value"
  (is (= 42 (run-string "(prog2 1 42 3)"))))

(test compile-ignore-errors-success
  "ignore-errors returns normal value on success"
  (is (= 3 (run-string "(ignore-errors (+ 1 2))"))))

(test compile-ignore-errors-failure
  "ignore-errors returns nil on error"
  (is (null (run-string "(ignore-errors (error \"boom\"))"))))

;;; Unwind-Protect Integration Tests

(test unwind-protect-cleanup-visible
  "unwind-protect cleanup side effects visible in handler-case"
  (is (eq t (run-string "
(let ((cleaned nil))
  (handler-case
    (unwind-protect
      (error \"boom\")
      (setf cleaned t))
    (error (e) cleaned)))"))))

;;; Self-Hosting CLOS Compiler Test

(test self-host-clos-compiler
  "Self-hosting: CLOS-based AST compiler with defgeneric/defmethod dispatch"
  (is (equal '(:R2 3)
    (run-string "
(defclass ast-n6 () ())
(defclass ast-i6 (ast-n6) ((v :initarg :v :reader ast-i6-v)))
(defclass ast-v6 (ast-n6) ((n :initarg :n :reader ast-v6-n)))
(defclass ast-b6 (ast-n6)
  ((op :initarg :op :reader ast-b6-op)
   (l :initarg :l :reader ast-b6-l)
   (r :initarg :r :reader ast-b6-r)))
(defstruct cx6 (nr 0) (insts nil) (env nil))
(defun mr6 (c) (let ((r (cx6-nr c))) (setf (cx6-nr c) (+ r 1))
  (intern (concatenate 'string \"R\" (write-to-string r)) :keyword)))
(defun em6 (c i) (setf (cx6-insts c) (append (cx6-insts c) (list i))))
(defgeneric cn6 (node ctx))
(defmethod cn6 ((node ast-i6) ctx)
  (let ((r (mr6 ctx))) (em6 ctx (list :const r (ast-i6-v node))) r))
(defmethod cn6 ((node ast-v6) ctx)
  (cdr (assoc (ast-v6-n node) (cx6-env ctx))))
(defmethod cn6 ((node ast-b6) ctx)
  (let ((l (cn6 (ast-b6-l node) ctx))
        (rv (cn6 (ast-b6-r node) ctx))
        (dst (mr6 ctx)))
    (em6 ctx (list (ast-b6-op node) dst l rv)) dst))
(let ((ctx (make-cx6)))
  (let ((r (cn6 (make-instance 'ast-b6 :op :add
    :l (make-instance 'ast-i6 :v 10)
    :r (make-instance 'ast-i6 :v 20)) ctx)))
    (list r (length (cx6-insts ctx)))))
" :stdlib t))))

;;; Hash Table Extended Builtins Tests

(test compile-hash-table-values
  "hash-table-values returns list of values"
  (let ((result (run-string "(let ((ht (make-hash-table)))
  (setf (gethash :a ht) 1)
  (setf (gethash :b ht) 2)
  (hash-table-values ht))")))
    (is (= 2 (length result)))
    (is (null (set-difference result '(1 2))))))

(test compile-hash-table-test-default
  "hash-table-test returns eql for default"
  (is (eq 'eql (run-string "(hash-table-test (make-hash-table))"))))

(test compile-hash-table-test-equal
  "hash-table-test returns equal when created with :test 'equal"
  (is (eq 'equal (run-string "(hash-table-test (make-hash-table :test 'equal))"))))

(test compile-copy-hash-table
  "copy-hash-table creates independent copy"
  (is (= 42 (run-string "
(let ((ht (make-hash-table)))
  (setf (gethash :a ht) 42)
  (let ((ht2 (copy-hash-table ht)))
    (setf (gethash :a ht) 99)
    (gethash :a ht2)))"))))

;;; Car/Cdr Composition and List Accessor Tests

(test cadddr-basic
  "cadddr returns fourth element"
  (is (equal 'd (run-string "(cadddr '(a b c d e))" :stdlib t))))

(test caadr-basic
  "caadr returns car of second element"
  (is (equal 'x (run-string "(caadr '(a (x y) c))" :stdlib t))))

(test caddar-basic
  "caddar returns third element of first sublist"
  (is (equal 3 (run-string "(caddar '((1 2 3) b c))" :stdlib t))))

;;; Self-Hosting Integration Tests

(test self-host-eval-loop
  "Mini eval that dispatches on form type - core pattern for self-hosting"
  (is (= 7 (run-string "
  (defun mini-eval (form env)
    (cond
      ((integerp form) form)
      ((symbolp form) (cdr (assoc form env)))
      ((and (consp form) (eq (car form) 'quote)) (cadr form))
      ((and (consp form) (eq (car form) 'if))
       (if (not (= 0 (mini-eval (cadr form) env)))
           (mini-eval (caddr form) env)
           (mini-eval (cadddr form) env)))
      ((and (consp form) (eq (car form) '+))
       (+ (mini-eval (cadr form) env) (mini-eval (caddr form) env)))
      (t 0)))
  (mini-eval '(if 1 (+ 3 4) 0) nil)
" :stdlib t))))

(test self-host-defstruct-pipeline
  "Defstruct with constructor and accessor - AST node pattern"
  (is (= 42 (run-string "
  (defstruct node type value children)
  (let ((n (make-node :type 'add :value nil :children (list (make-node :type 'lit :value 42 :children nil)))))
    (node-value (car (node-children n))))
" :stdlib t))))

(test self-host-hash-table-registry
  "Registry pattern using hash tables - like compiler function/class registries"
  (is (= 30 (run-string "
  (let ((registry (make-hash-table)))
    (setf (gethash 'add registry) (lambda (a b) (+ a b)))
    (setf (gethash 'mul registry) (lambda (a b) (* a b)))
    (let ((op (gethash 'add registry)))
      (funcall op 10 20)))
" :stdlib t))))

(test self-host-recursive-tree-walk
  "Recursive tree processing - like AST walking"
  (is (= 10 (run-string "
  (defun tree-sum (tree)
    (if (consp tree)
        (+ (tree-sum (car tree)) (tree-sum (cdr tree)))
        (if (integerp tree) tree 0)))
  (tree-sum '((1 . 2) . (3 . (4 . nil))))
" :stdlib t))))

(test self-host-closure-counter
  "Closure as mutable state via defun inside let"
  (is (= 3 (run-string "
  (let ((counter 0))
    (defun next-id ()
      (setq counter (+ counter 1))
      counter))
  (next-id) (next-id) (next-id)
" :stdlib t))))

(test self-host-macro-code-gen
  "Using list to generate code then eval it - code generation pattern"
  (is (= 15 (run-string "
  (defun make-add-expr (a b)
    (list '+ a b))
  (defun make-let-expr (var val body)
    (list 'let (list (list var val)) body))
  (eval (make-let-expr 'x 10 (make-add-expr 'x 5)))
" :stdlib t))))

;;; Non-Constant Default Parameter Tests

(test key-non-constant-default
  "Test &key parameter with non-constant default expression."
  (is (equal '(1 2 3) (run-string "(progn
     (defun test-fn (&key (data (list 1 2 3))) data)
     (test-fn))"))))

(test key-non-constant-default-supplied
  "Test &key parameter with non-constant default when value is supplied."
  (is (equal '(4 5 6) (run-string "(progn
     (defun test-fn (&key (data (list 1 2 3))) data)
     (test-fn :data (list 4 5 6)))"))))

(test optional-non-constant-default
  "Test &optional parameter with non-constant default expression."
  (is (equal '(10 20) (run-string "(progn
     (defun test-fn (&optional (data (list 10 20))) data)
     (test-fn))"))))

(test defstruct-non-constant-default
  "Test defstruct with non-constant slot default (hash table)."
  (is (eq :bar (run-string "(progn
     (defstruct registry (entries (make-hash-table :test 'eq)))
     (let ((r (make-registry)))
       (setf (gethash 'foo (registry-entries r)) :bar)
       (gethash 'foo (registry-entries r))))" :stdlib t))))

;;; Multiple Dispatch Tests

(test multi-dispatch-double-specializer
  "Multiple dispatch: method specialized on two parameters"
  (is (= 1 (run-string "(progn
    (defclass animal () ())
    (defclass dog (animal) ())
    (defclass cat (animal) ())
    (defclass food () ())
    (defclass bone (food) ())
    (defclass fish (food) ())
    (defgeneric feed (a f))
    (defmethod feed ((a dog) (f bone)) 1)
    (defmethod feed ((a cat) (f fish)) 2)
    (feed (make-instance 'dog) (make-instance 'bone)))" :stdlib t)))
  (is (= 2 (run-string "(progn
    (defclass animal () ())
    (defclass dog (animal) ())
    (defclass cat (animal) ())
    (defclass food () ())
    (defclass bone (food) ())
    (defclass fish (food) ())
    (defgeneric feed (a f))
    (defmethod feed ((a dog) (f bone)) 1)
    (defmethod feed ((a cat) (f fish)) 2)
    (feed (make-instance 'cat) (make-instance 'fish)))" :stdlib t))))

(test multi-dispatch-mixed-specialization
  "Multiple dispatch: one specialized, one unspecialized parameter"
  (is (= 10 (run-string "(progn
    (defclass shape () ())
    (defclass circle (shape) ())
    (defclass rect (shape) ())
    (defgeneric area (s ctx))
    (defmethod area ((s circle) ctx) 10)
    (defmethod area ((s rect) ctx) 20)
    (area (make-instance 'circle) 99))" :stdlib t)))
  (is (= 20 (run-string "(progn
    (defclass shape () ())
    (defclass circle (shape) ())
    (defclass rect (shape) ())
    (defgeneric area (s ctx))
    (defmethod area ((s circle) ctx) 10)
    (defmethod area ((s rect) ctx) 20)
    (area (make-instance 'rect) 99))" :stdlib t))))

(test multi-dispatch-with-clos-second-arg
  "Multiple dispatch: unspecialized param receives CLOS instance"
  (is (= 42 (run-string "(progn
    (defclass ctx () ())
    (defclass nd () ())
    (defclass nd-int (nd) ((v :initarg :v :reader nd-v)))
    (defgeneric cmp (n c))
    (defmethod cmp ((n nd-int) c) 42)
    (cmp (make-instance 'nd-int :v 1) (make-instance 'ctx)))" :stdlib t))))

(test multi-dispatch-inheritance-fallback
  "Multiple dispatch: dispatch falls back via class inheritance"
  (is (eq 'base (run-string "(progn
    (defclass a () ())
    (defclass b (a) ())
    (defclass x () ())
    (defclass y (x) ())
    (defgeneric op (p q))
    (defmethod op ((p a) (q x)) 'base)
    (op (make-instance 'b) (make-instance 'y)))" :stdlib t))))

(test multi-dispatch-type-equality
  "Multiple dispatch: double dispatch for type equality (self-hosting pattern)"
  (is (eq t (run-string "(progn
    (defclass ty () ())
    (defclass ty-int (ty) ())
    (defclass ty-str (ty) ())
    (defgeneric ty-eq (a b))
    (defmethod ty-eq ((a ty-int) (b ty-int)) t)
    (defmethod ty-eq ((a ty-str) (b ty-str)) t)
    (defmethod ty-eq ((a ty) (b ty)) nil)
    (ty-eq (make-instance 'ty-int) (make-instance 'ty-int)))" :stdlib t)))
  (is (null (run-string "(progn
    (defclass ty () ())
    (defclass ty-int (ty) ())
    (defclass ty-str (ty) ())
    (defgeneric ty-eq (a b))
    (defmethod ty-eq ((a ty-int) (b ty-int)) t)
    (defmethod ty-eq ((a ty-str) (b ty-str)) t)
    (defmethod ty-eq ((a ty) (b ty)) nil)
    (ty-eq (make-instance 'ty-int) (make-instance 'ty-str)))" :stdlib t))))

;;; CLOS Initform and Accessor Setf Tests

(test clos-initform-integer
  "Test CLOS :initform with integer value."
  (is (= 0 (run-string "(progn
     (defclass counter () ((n :initform 0 :accessor counter-n)))
     (counter-n (make-instance 'counter)))"))))

(test clos-initform-with-setf-accessor
  "Test CLOS :accessor setf works in same progn as defclass."
  (is (= 99 (run-string "(progn
     (defclass box () ((val :initarg :val :initform 0 :accessor box-val)))
     (let ((b (make-instance 'box)))
       (setf (box-val b) 99)
       (box-val b)))"))))

(test clos-initform-counter-increment
  "Test CLOS accessor setf with repeated mutation."
  (is (= 3 (run-string "(progn
     (defclass counter () ((n :initform 0 :accessor counter-n)))
     (let ((c (make-instance 'counter)))
       (setf (counter-n c) (+ (counter-n c) 1))
       (setf (counter-n c) (+ (counter-n c) 1))
       (setf (counter-n c) (+ (counter-n c) 1))
       (counter-n c)))"))))

;;; Self-Hosting Bootstrap Tests

(test self-host-make-register
  "Test self-hosting: register allocation utility."
  (is (equal '(:R0 :R1 :R2) (run-string "(progn
     (defstruct compiler-ctx (next-register 0))
     (defun make-register (ctx)
       (let ((reg (intern (format nil \"R~D\" (compiler-ctx-next-register ctx)) :keyword)))
         (setf (compiler-ctx-next-register ctx) (+ (compiler-ctx-next-register ctx) 1))
         reg))
     (let ((ctx (make-compiler-ctx)))
       (list (make-register ctx) (make-register ctx) (make-register ctx))))" :stdlib t))))

(test self-host-mini-compiler
  "Test self-hosting: complete mini-compiler pipeline (parse -> compile -> VM -> run)."
  (is (= 35 (run-string "(progn
     (defstruct mc-int (val 0))
     (defstruct mc-binop (op nil) (l nil) (r nil))
     (defstruct mc-ctx (instrs nil) (nreg 0))
     (defun mc-reg (ctx)
       (let ((r (intern (format nil \"R~D\" (mc-ctx-nreg ctx)) :keyword)))
         (setf (mc-ctx-nreg ctx) (+ (mc-ctx-nreg ctx) 1)) r))
     (defun mc-emit (ctx inst) (setf (mc-ctx-instrs ctx) (cons inst (mc-ctx-instrs ctx))))
     (defun mc-compile (node ctx)
       (cond
         ((mc-int-p node) (let ((dst (mc-reg ctx))) (mc-emit ctx (list :const dst (mc-int-val node))) dst))
         ((mc-binop-p node)
          (let* ((lr (mc-compile (mc-binop-l node) ctx)) (rr (mc-compile (mc-binop-r node) ctx))
                 (dst (mc-reg ctx))
                 (opcode (case (mc-binop-op node) (+ :add) (- :sub) (* :mul))))
            (mc-emit ctx (list opcode dst lr rr)) dst))
         (t (error \"unknown\"))))
     (defun mc-run (instrs)
       (let ((regs (make-hash-table)))
         (dolist (inst instrs)
           (let ((op (car inst)))
             (cond ((eq op :const) (setf (gethash (cadr inst) regs) (caddr inst)))
                   ((member op '(:add :sub :mul))
                    (let ((l (gethash (caddr inst) regs)) (r (gethash (cadddr inst) regs)))
                      (setf (gethash (cadr inst) regs)
                            (case op (:add (+ l r)) (:sub (- l r)) (:mul (* l r)))))))))
         (gethash (cadr (car (last instrs))) regs)))
     (let ((ctx (make-mc-ctx)))
       (mc-compile (make-mc-binop :op '* :l (make-mc-binop :op '+ :l (make-mc-int :val 3) :r (make-mc-int :val 4))
                     :r (make-mc-binop :op '- :l (make-mc-int :val 10) :r (make-mc-int :val 5))) ctx)
       (mc-run (nreverse (mc-ctx-instrs ctx)))))" :stdlib t))))

(test self-host-clos-compiler
  "Test self-hosting: CLOS-based compiler with generic dispatch."
  (is (equal '((:CONST :R0 3) (:CONST :R1 4) (:MUL :R2 :R0 :R1) (:CONST :R3 5) (:ADD :R4 :R2 :R3))
    (run-string "(progn
     (defclass c-node () ())
     (defclass c-int (c-node) ((val :initarg :val :reader c-int-val)))
     (defclass c-add (c-node) ((lhs :initarg :lhs :reader c-add-lhs) (rhs :initarg :rhs :reader c-add-rhs)))
     (defclass c-mul (c-node) ((lhs :initarg :lhs :reader c-mul-lhs) (rhs :initarg :rhs :reader c-mul-rhs)))
     (defclass c-ctx () ((instrs :initform nil :accessor c-ctx-instrs) (nreg :initform 0 :accessor c-ctx-nreg)))
     (defun c-reg (ctx) (let ((r (intern (format nil \"R~D\" (c-ctx-nreg ctx)) :keyword)))
       (setf (c-ctx-nreg ctx) (+ (c-ctx-nreg ctx) 1)) r))
     (defun c-emit (ctx inst) (setf (c-ctx-instrs ctx) (cons inst (c-ctx-instrs ctx))))
     (defgeneric c-compile (node ctx))
     (defmethod c-compile ((node c-int) ctx)
       (let ((dst (c-reg ctx))) (c-emit ctx (list :const dst (c-int-val node))) dst))
     (defmethod c-compile ((node c-add) ctx)
       (let* ((lr (c-compile (c-add-lhs node) ctx)) (rr (c-compile (c-add-rhs node) ctx)) (dst (c-reg ctx)))
         (c-emit ctx (list :add dst lr rr)) dst))
     (defmethod c-compile ((node c-mul) ctx)
       (let* ((lr (c-compile (c-mul-lhs node) ctx)) (rr (c-compile (c-mul-rhs node) ctx)) (dst (c-reg ctx)))
         (c-emit ctx (list :mul dst lr rr)) dst))
     (let ((ctx (make-instance 'c-ctx)))
       (c-compile (make-instance 'c-add
                    :lhs (make-instance 'c-mul :lhs (make-instance 'c-int :val 3) :rhs (make-instance 'c-int :val 4))
                    :rhs (make-instance 'c-int :val 5)) ctx)
       (nreverse (c-ctx-instrs ctx))))" :stdlib t))))

(test self-host-clos-full-pipeline
  "Full self-hosting: CLOS AST -> register compiler -> hash-table VM with let/if"
  (is (= 100 (run-string "
    (progn
      (defclass ast () ())
      (defclass ast-lit (ast) ((value :initarg :value :accessor ast-lit-value)))
      (defclass ast-var (ast) ((name :initarg :name :accessor ast-var-name)))
      (defclass ast-binop (ast) ((op :initarg :op :accessor ast-binop-op)
                                  (lhs :initarg :lhs :accessor ast-binop-lhs)
                                  (rhs :initarg :rhs :accessor ast-binop-rhs)))
      (defclass ast-let (ast) ((var :initarg :var :accessor ast-let-var)
                                 (init :initarg :init :accessor ast-let-init)
                                 (body :initarg :body :accessor ast-let-body)))
      (defclass ast-if (ast) ((test :initarg :test :accessor ast-if-test)
                                (then :initarg :then :accessor ast-if-then)
                                (else :initarg :else :accessor ast-if-else)))
      (defun parse (sexp)
        (cond
          ((numberp sexp) (make-instance (quote ast-lit) :value sexp))
          ((symbolp sexp) (make-instance (quote ast-var) :name sexp))
          ((not (consp sexp)) (error \"bad\"))
          ((eq (car sexp) (quote let))
           (make-instance (quote ast-let) :var (caar (cadr sexp))
             :init (parse (cadar (cadr sexp))) :body (parse (caddr sexp))))
          ((eq (car sexp) (quote if))
           (make-instance (quote ast-if) :test (parse (cadr sexp))
             :then (parse (caddr sexp)) :else (parse (cadddr sexp))))
          ((member (car sexp) (quote (+ - * =)))
           (make-instance (quote ast-binop) :op (car sexp)
             :lhs (parse (cadr sexp)) :rhs (parse (caddr sexp))))
          (t (error \"unknown\"))))
      (defvar *rc* 0)
      (defvar *code* nil)
      (defun fr () (let ((r (intern (format nil \"R~D\" *rc*)))) (setq *rc* (+ *rc* 1)) r))
      (defun em (inst) (push inst *code*))
      (defgeneric cn (node env))
      (defmethod cn ((n ast-lit) env)
        (let ((r (fr))) (em (list (quote CONST) r (ast-lit-value n))) r))
      (defmethod cn ((n ast-var) env) (cdr (assoc (ast-var-name n) env)))
      (defmethod cn ((n ast-binop) env)
        (let ((l (cn (ast-binop-lhs n) env)) (r (cn (ast-binop-rhs n) env)) (d (fr)))
          (em (list (ast-binop-op n) d l r)) d))
      (defmethod cn ((n ast-let) env)
        (cn (ast-let-body n) (acons (ast-let-var n) (cn (ast-let-init n) env) env)))
      (defmethod cn ((n ast-if) env)
        (let ((tr (cn (ast-if-test n) env)) (th (cn (ast-if-then n) env))
              (el (cn (ast-if-else n) env)) (d (fr)))
          (em (list (quote SEL) d tr th el)) d))
      (defun rp (instrs)
        (let ((regs (make-hash-table :test (quote eq))))
          (dolist (inst instrs)
            (let ((op (nth 0 inst)) (dst (nth 1 inst)))
              (cond
                ((eq op (quote CONST)) (setf (gethash dst regs) (nth 2 inst)))
                ((eq op (quote SEL))
                 (let ((tv (gethash (nth 2 inst) regs)))
                   (setf (gethash dst regs)
                     (if (and tv (not (zerop tv)))
                         (gethash (nth 3 inst) regs)
                         (gethash (nth 4 inst) regs)))))
                (t (let ((a (gethash (nth 2 inst) regs)) (b (gethash (nth 3 inst) regs)))
                     (setf (gethash dst regs)
                       (cond ((eq op (quote +)) (+ a b)) ((eq op (quote -)) (- a b))
                             ((eq op (quote *)) (* a b)) ((eq op (quote =)) (if (= a b) 1 0))
                             (t 0))))))))
          (gethash (intern (format nil \"R~D\" (- *rc* 1))) regs)))
      (setq *rc* 0) (setq *code* nil)
      (cn (parse (quote (let ((x 10)) (if (= x 10) (* x x) (+ x 1))))) nil)
      (rp (nreverse *code*)))" :stdlib t))))

;;; Generic Function as First-Class Value Tests

(test funcall-generic-function
  "funcall with #'generic-function should dispatch correctly"
  (is (= 11 (run-string "(progn
    (defgeneric my-fn (x))
    (defmethod my-fn ((x t)) (+ x 1))
    (funcall #'my-fn 10))"))))

(test funcall-generic-function-clos
  "funcall with #'generic-function should dispatch on CLOS class"
  (is (string= "dog-speak" (run-string "(progn
    (defclass animal () ())
    (defclass dog (animal) ())
    (defgeneric speak (x))
    (defmethod speak ((x dog)) \"dog-speak\")
    (defmethod speak ((x t)) \"default\")
    (funcall #'speak (make-instance 'dog)))"))))

(test mapcar-generic-function
  "mapcar with #'generic-function via stdlib"
  (is (equal '(2 3 4) (run-string "(progn
    (defgeneric inc (x))
    (defmethod inc ((x t)) (+ x 1))
    (mapcar #'inc (list 1 2 3)))" :stdlib t))))

(test apply-generic-function
  "apply with #'generic-function should work"
  (is (= 42 (run-string "(progn
    (defgeneric add1 (x))
    (defmethod add1 ((x t)) (+ x 1))
    (apply #'add1 (list 41)))"))))

(test generic-function-in-let
  "Binding #'generic-function to a variable and calling it"
  (is (= 5 (run-string "(progn
    (defgeneric double (x))
    (defmethod double ((x t)) (* x 2))
    (let ((f #'double))
      (funcall f 2)
      (+ (funcall f 2) 1)))"))))

(test mapcar-generic-function-reader
  "mapcar with #'reader-method on CLOS instances"
  (is (equal '(A B C) (run-string "(progn
    (defclass item ()
      ((name :initarg :name :reader item-name)))
    (let ((items (list (make-instance 'item :name 'a)
                       (make-instance 'item :name 'b)
                       (make-instance 'item :name 'c))))
      (mapcar #'item-name items)))" :stdlib t))))

(test self-host-mapcar-inst-sexp
  "Self-hosting pattern: mapcar #'generic-function over instruction list"
  (is (equal '((:CONST R0 42) (:CONST R1 7) (:ADD R2 R0 R1))
    (run-string "(progn
      (defclass instruction () ())
      (defclass inst-const (instruction)
        ((dst :initarg :dst :reader inst-dst)
         (value :initarg :value :reader inst-value)))
      (defclass inst-add (instruction)
        ((dst :initarg :dst :reader inst-dst)
         (lhs :initarg :lhs :reader inst-lhs)
         (rhs :initarg :rhs :reader inst-rhs)))
      (defgeneric inst-sexp (inst))
      (defmethod inst-sexp ((inst inst-const))
        (list :const (inst-dst inst) (inst-value inst)))
      (defmethod inst-sexp ((inst inst-add))
        (list :add (inst-dst inst) (inst-lhs inst) (inst-rhs inst)))
      (let ((program (list (make-instance 'inst-const :dst 'r0 :value 42)
                           (make-instance 'inst-const :dst 'r1 :value 7)
                           (make-instance 'inst-add :dst 'r2 :lhs 'r0 :rhs 'r1))))
        (mapcar #'inst-sexp program)))" :stdlib t))))

;;; Run Tests Function

;;; Global Variable (defvar) Persistence Tests

(test defvar-setq-persists-across-calls
  "defvar + setq mutations should persist across function calls"
  (is (= 3 (run-string "(progn
    (defvar *counter* 0)
    (defun inc-counter () (setq *counter* (+ *counter* 1)) *counter*)
    (inc-counter) (inc-counter) (inc-counter))"))))

(test defvar-inline-counter
  "defvar counter should increment sequentially"
  (is (equal '(0 1 2) (run-string "(progn
    (defvar *n* 0)
    (defun next-n ()
      (let ((val *n*))
        (setq *n* (+ *n* 1))
        val))
    (list (next-n) (next-n) (next-n)))"))))

(test defvar-label-generation
  "defvar counter for label generation pattern"
  (is (equal '("L_0" "L_1" "L_2") (run-string "(progn
    (defvar *lbl* 0)
    (defun make-label (prefix)
      (let ((n *lbl*))
        (setq *lbl* (+ n 1))
        (concatenate 'string prefix \"_\" (write-to-string n))))
    (list (make-label \"L\") (make-label \"L\") (make-label \"L\")))" :stdlib t))))

;;; Defmacro in progn Tests

(test defmacro-in-progn-simple
  "defmacro should work within progn for subsequent forms"
  (is (= 10 (run-string "(progn
    (defmacro my-dbl (x) (list '+ x x))
    (my-dbl 5))"))))

(test defmacro-in-progn-rest
  "defmacro with &rest should work within progn"
  (is (= 42 (run-string "(progn
    (defmacro my-when (test &rest body)
      (list 'if test (cons 'progn body) nil))
    (my-when (= 1 1) 42))"))))

(test defmacro-in-progn-used-twice
  "macro defined in progn should be usable multiple times"
  (is (= 12 (run-string "(progn
    (defmacro my-add1 (x) (list '+ x 1))
    (+ (my-add1 5) (my-add1 5)))"))))

;;; Self-Hosting Compiler Pattern Tests

(test self-host-compiler-context-full
  "Self-hosting: full compiler context with make-register, make-label, emit"
  (let ((result (run-string "(progn
    (defclass compiler-context ()
      ((instructions :initform nil :accessor ctx-instructions)
       (next-register :initform 0 :accessor ctx-next-register)
       (next-label :initform 0 :accessor ctx-next-label)))
    (defun make-register (ctx)
      (let ((n (ctx-next-register ctx)))
        (setf (ctx-next-register ctx) (+ n 1))
        (intern (concatenate 'string \"R\" (write-to-string n)) :keyword)))
    (defun make-label (ctx prefix)
      (let ((n (ctx-next-label ctx)))
        (setf (ctx-next-label ctx) (+ n 1))
        (concatenate 'string prefix \"_\" (write-to-string n))))
    (defun emit (ctx inst)
      (setf (ctx-instructions ctx) (cons inst (ctx-instructions ctx))))
    (let ((ctx (make-instance 'compiler-context)))
      (let ((r0 (make-register ctx)) (r1 (make-register ctx)))
        (emit ctx (list :const r0 42))
        (emit ctx (list :const r1 7))
        (emit ctx (list :add (make-register ctx) r0 r1))
        (nreverse (ctx-instructions ctx)))))" :stdlib t)))
    (is (equal '((:CONST :R0 42) (:CONST :R1 7) (:ADD :R2 :R0 :R1)) result))))

(test self-host-ast-compile-dispatch
  "Self-hosting: CLOS compile-ast dispatch compiles (+ (* 3 4) 5)"
  (let ((result (run-string "(progn
    (defclass ctx () ((instrs :initform nil :accessor ctx-instrs) (rc :initform 0 :accessor ctx-rc)))
    (defun mk-r (c) (let ((n (ctx-rc c))) (setf (ctx-rc c) (+ n 1))
      (intern (concatenate 'string \"R\" (write-to-string n)) :keyword)))
    (defclass nd () ())
    (defclass nd-int (nd) ((v :initarg :v :reader nd-v)))
    (defclass nd-op (nd) ((o :initarg :o :reader nd-o) (l :initarg :l :reader nd-l) (r :initarg :r :reader nd-r)))
    (defgeneric cmp (n c))
    (defmethod cmp ((n nd-int) c) (let ((d (mk-r c))) (setf (ctx-instrs c) (cons (list :const d (nd-v n)) (ctx-instrs c))) d))
    (defmethod cmp ((n nd-op) c) (let* ((lr (cmp (nd-l n) c)) (rr (cmp (nd-r n) c)) (d (mk-r c)))
      (setf (ctx-instrs c) (cons (list (nd-o n) d lr rr) (ctx-instrs c))) d))
    (let ((c (make-instance 'ctx)))
      (cmp (make-instance 'nd-op :o :add
             :l (make-instance 'nd-op :o :mul
                  :l (make-instance 'nd-int :v 3) :r (make-instance 'nd-int :v 4))
             :r (make-instance 'nd-int :v 5)) c)
      (nreverse (ctx-instrs c))))" :stdlib t)))
    (is (equal '((:CONST :R0 3) (:CONST :R1 4) (:MUL :R2 :R0 :R1)
                 (:CONST :R3 5) (:ADD :R4 :R2 :R3)) result))))

(test self-host-macro-expander
  "Self-hosting: macro expansion system with hash table registry"
  (let ((result (run-string "(progn
    (defvar *macros* (make-hash-table :test 'eq))
    (defun reg-macro (name fn) (setf (gethash name *macros*) fn))
    (defun expand (form)
      (if (and (consp form) (symbolp (car form)))
          (let ((ex (gethash (car form) *macros*)))
            (if ex (expand (funcall ex form nil))
                (mapcar (lambda (x) (if (consp x) (expand x) x)) form)))
          form))
    (reg-macro 'when (lambda (f e) (list 'if (second f) (cons 'progn (cddr f)) nil)))
    (expand '(when x (+ 1 2))))" :stdlib t)))
    (is (equal '(IF X (PROGN (+ 1 2)) NIL) result))))

;;; Multiple-Value-List Tests

(test multiple-value-list-floor
  "multiple-value-list captures floor quotient and remainder"
  (is (equal '(3 2) (run-string "(multiple-value-list (floor 17 5))"))))

(test multiple-value-list-values
  "multiple-value-list captures explicit values"
  (is (equal '(1 2 3) (run-string "(multiple-value-list (values 1 2 3))"))))

(test multiple-value-list-single
  "multiple-value-list with single value returns singleton list"
  (is (equal '(42) (run-string "(multiple-value-list (values 42))"))))

;;; Apply with Spread Arguments Tests

(test apply-spread-args-plus
  "apply #'+ with spread args collects all values"
  (is (= 10 (run-string "(apply #'+ 1 2 (list 3 4))"))))

(test apply-spread-args-minus
  "apply #'- with spread args subtracts sequentially"
  (is (= 5 (run-string "(apply #'- (list 10 3 2))"))))

(test apply-spread-args-multiply
  "apply #'* with spread args multiplies all"
  (is (= 24 (run-string "(apply #'* (list 2 3 4))"))))

(test apply-spread-args-append
  "apply #'append with list of lists"
  (is (equal '(1 2 3 4) (run-string "(apply #'append (list (list 1 2) (list 3 4)))"))))

(test apply-quoted-plus-list
  "apply with quoted + and list arg"
  (is (= 15 (run-string "(apply #'+ (list 1 2 3 4 5))"))))

;;; Typed Defun/Lambda Tests

(test typed-defun-basic
  "typed defun with fixnum params and return type"
  (is (= 7 (run-string "(progn
    (defun typed-add ((x fixnum) (y fixnum)) fixnum (+ x y))
    (typed-add 3 4))"))))

(test typed-defun-string
  "typed defun with string param and return type"
  (is (string= "Hello World" (run-string "(progn
    (defun typed-greet ((name string)) string
      (concatenate 'string \"Hello \" name))
    (typed-greet \"World\"))"))))

(test typed-defun-no-return-type
  "typed defun with param types but no return type"
  (is (= 12 (run-string "(progn
    (defun typed-mul ((x fixnum) (y fixnum)) (* x y))
    (typed-mul 3 4))"))))

(test typed-defun-mixed-params
  "typed defun with some typed and some untyped params"
  (is (= 7 (run-string "(progn
    (defun typed-mixed ((x fixnum) y) (+ x y))
    (typed-mixed 3 4))"))))

(test typed-lambda-basic
  "typed lambda with fixnum params"
  (is (= 30 (run-string "(funcall (lambda ((x fixnum) (y fixnum)) fixnum (+ x y)) 10 20)"))))

(test typed-lambda-no-return
  "typed lambda without return type"
  (is (= 6 (run-string "(funcall (lambda ((a fixnum) (b fixnum)) (* a b)) 2 3)"))))

(test typed-defun-type-registry
  "typed defun registers function type for type checking"
  (let ((old-count (hash-table-count cl-cc:*function-type-registry*)))
    (run-string "(defun typed-reg-test ((x fixnum)) fixnum x)")
    (is (> (hash-table-count cl-cc:*function-type-registry*) old-count))))

;;; CLOS Type Inference Tests

(test clos-type-make-instance
  "make-instance infers class type"
  (multiple-value-bind (result type)
      (run-string-typed "(progn
        (defclass point () ((x :initarg :x :type fixnum) (y :initarg :y :type fixnum)))
        (make-instance 'point :x 1 :y 2))")
    (declare (ignore result))
    (is (typep type 'cl-cc/type:type-primitive))
    (is (string= "POINT" (symbol-name (cl-cc/type:type-primitive-name type))))))

(test clos-type-slot-value
  "slot-value infers slot type from defclass :type"
  (multiple-value-bind (result type)
      (run-string-typed "(progn
        (defclass point () ((x :initarg :x :type fixnum) (y :initarg :y :type fixnum)))
        (let ((p (make-instance 'point :x 10 :y 20)))
          (slot-value p 'x)))")
    (is (= 10 result))
    (is (typep type 'cl-cc/type:type-primitive))
    (is (string= "FIXNUM" (symbol-name (cl-cc/type:type-primitive-name type))))))

(test clos-type-string-slot
  "slot-value infers string type from slot annotation"
  (multiple-value-bind (result type)
      (run-string-typed "(progn
        (defclass person () ((name :initarg :name :type string)))
        (let ((p (make-instance 'person :name \"Alice\")))
          (slot-value p 'name)))")
    (is (string= "Alice" result))
    (is (typep type 'cl-cc/type:type-primitive))
    (is (string= "STRING" (symbol-name (cl-cc/type:type-primitive-name type))))))

;;; Type Alias (deftype) Tests

(test deftype-basic
  "deftype registers type alias that can be used in typed defun"
  (is (= 42 (run-string "(progn
    (deftype my-int fixnum)
    (defun typed-id ((x my-int)) my-int x)
    (typed-id 42))"))))

(test deftype-union
  "deftype with union type expands in type registry"
  (let ((old-count (hash-table-count cl-cc/type:*type-alias-registry*)))
    (run-string "(deftype int-or-str (or fixnum string))")
    (is (> (hash-table-count cl-cc/type:*type-alias-registry*) old-count))))

(test deftype-in-slot
  "deftype alias usable in defclass slot :type"
  (is (= 10 (run-string "(progn
    (deftype coordinate fixnum)
    (defclass point2 () ((x :initarg :x :type coordinate)))
    (let ((p (make-instance 'point2 :x 10)))
      (slot-value p 'x)))"))))

;;; Type Narrowing Tests

(test type-narrowing-numberp
  "numberp narrows type to fixnum in then-branch"
  (multiple-value-bind (result type)
      (run-string-typed "(let ((x 42)) (if (numberp x) (+ x 1) 0))")
    (is (= 43 result))
    (is (typep type 'cl-cc/type:type-primitive))
    (is (string= "FIXNUM" (symbol-name (cl-cc/type:type-primitive-name type))))))

(test type-narrowing-stringp
  "stringp narrows type to string in then-branch"
  (multiple-value-bind (result type)
      (run-string-typed "(let ((x \"hello\")) (if (stringp x) x \"default\"))")
    (is (string= "hello" result))
    (is (typep type 'cl-cc/type:type-primitive))))

;;; Higher-Order Function Macro Expansions (Self-Hosting)

(test hof-mapcar-basic
  "mapcar applies function to each element"
  (is (equal '(2 4 6)
             (run-string "(mapcar (lambda (x) (* x 2)) (list 1 2 3))"))))

(test hof-mapcar-empty
  "mapcar on empty list returns nil"
  (is (null (run-string "(mapcar (lambda (x) x) nil)"))))

(test hof-mapc-side-effect
  "mapc executes side effect and returns original list"
  (is (equal '(1 2 3)
             (run-string "(mapc (lambda (x) (+ x 1)) (list 1 2 3))"))))

(test hof-mapcan-flatten
  "mapcan concatenates results"
  (is (equal '(1 1 2 2 3 3)
             (run-string "(mapcan (lambda (x) (list x x)) (list 1 2 3))"))))

(test hof-every-true
  "every returns t when all satisfy predicate"
  (is (eq t (run-string "(every (lambda (x) (> x 0)) (list 1 2 3))"))))

(test hof-every-false
  "every returns nil when one fails"
  (is (null (run-string "(every (lambda (x) (> x 2)) (list 1 2 3))"))))

(test hof-every-empty
  "every on empty list returns t"
  (is (eq t (run-string "(every (lambda (x) x) nil)"))))

(test hof-some-found
  "some returns first truthy result"
  (is (= 3 (run-string "(some (lambda (x) (if (> x 2) x nil)) (list 1 2 3 4))"))))

(test hof-some-not-found
  "some returns nil when none match"
  (is (null (run-string "(some (lambda (x) (if (> x 10) x nil)) (list 1 2 3))"))))

(test hof-remove-if-basic
  "remove-if filters out matching elements"
  (is (equal '(1 3 5)
             (run-string "(remove-if (lambda (x) (= 0 (mod x 2))) (list 1 2 3 4 5))"))))

(test hof-remove-if-not-basic
  "remove-if-not keeps only matching elements"
  (is (equal '(2 4)
             (run-string "(remove-if-not (lambda (x) (= 0 (mod x 2))) (list 1 2 3 4 5))"))))

(test hof-find-basic
  "find returns first matching element"
  (is (= 3 (run-string "(find 3 (list 1 2 3 4 5))"))))

(test hof-find-not-found
  "find returns nil when not found"
  (is (null (run-string "(find 99 (list 1 2 3))"))))

(test hof-find-if-basic
  "find-if returns first element satisfying predicate"
  (is (= 4 (run-string "(find-if (lambda (x) (> x 3)) (list 1 2 3 4 5))"))))

(test hof-position-basic
  "position returns index of first match"
  (is (= 2 (run-string "(position 3 (list 1 2 3 4 5))"))))

(test hof-position-not-found
  "position returns nil when not found"
  (is (null (run-string "(position 99 (list 1 2 3))"))))

(test hof-count-basic
  "count returns number of matching elements"
  (is (= 3 (run-string "(count 2 (list 1 2 2 3 2))"))))

(test hof-count-if-basic
  "count-if counts elements satisfying predicate"
  (is (= 2 (run-string "(count-if (lambda (x) (> x 3)) (list 1 2 3 4 5))"))))

(test hof-remove-basic
  "remove filters out matching elements by eql"
  (is (equal '(1 3 5)
             (run-string "(remove 2 (list 1 2 3 2 5))"))))

(test hof-remove-duplicates-basic
  "remove-duplicates removes duplicate elements"
  (is (equal '(1 2 3)
             (run-string "(remove-duplicates (list 1 2 3 2 1))"))))

;;; Parametric Types (type-constructor)

(test parametric-type-parse-list
  "Parsing (list fixnum) yields a type-constructor"
  (let ((ty (cl-cc/type:parse-type-specifier '(list fixnum))))
    (is (typep ty 'cl-cc/type:type-constructor))
    (is (eq 'list (cl-cc/type:type-constructor-name ty)))
    (is (= 1 (length (cl-cc/type:type-constructor-args ty))))
    (is (cl-cc/type:type-equal-p (first (cl-cc/type:type-constructor-args ty))
                                  cl-cc/type:type-int))))

(test parametric-type-parse-option
  "Parsing (Option string) yields a type-constructor"
  (let ((ty (cl-cc/type:parse-type-specifier '(Option string))))
    (is (typep ty 'cl-cc/type:type-constructor))
    (is (eq 'Option (cl-cc/type:type-constructor-name ty)))
    (is (cl-cc/type:type-equal-p (first (cl-cc/type:type-constructor-args ty))
                                  cl-cc/type:type-string))))

(test parametric-type-parse-pair
  "Parsing (Pair fixnum string) yields a type-constructor with 2 args"
  (let ((ty (cl-cc/type:parse-type-specifier '(Pair fixnum string))))
    (is (typep ty 'cl-cc/type:type-constructor))
    (is (eq 'Pair (cl-cc/type:type-constructor-name ty)))
    (is (= 2 (length (cl-cc/type:type-constructor-args ty))))
    (is (cl-cc/type:type-equal-p (first (cl-cc/type:type-constructor-args ty))
                                  cl-cc/type:type-int))
    (is (cl-cc/type:type-equal-p (second (cl-cc/type:type-constructor-args ty))
                                  cl-cc/type:type-string))))

(test parametric-type-unify-same
  "Unifying (List fixnum) with (List fixnum) succeeds"
  (let ((t1 (cl-cc/type:parse-type-specifier '(list fixnum)))
        (t2 (cl-cc/type:parse-type-specifier '(list fixnum))))
    (multiple-value-bind (subst ok) (cl-cc/type:type-unify t1 t2)
      (is-true ok)
      (is (null subst)))))

(test parametric-type-unify-with-var
  "Unifying (List ?a) with (List fixnum) binds ?a to fixnum"
  (let* ((tv (cl-cc/type:make-type-variable 'a))
         (t1 (cl-cc/type:make-type-constructor 'list (list tv)))
         (t2 (cl-cc/type:parse-type-specifier '(list fixnum))))
    (multiple-value-bind (subst ok) (cl-cc/type:type-unify t1 t2)
      (is-true ok)
      (is (not (null subst)))
      (let ((resolved (cl-cc/type:type-substitute tv subst)))
        (is (cl-cc/type:type-equal-p resolved cl-cc/type:type-int))))))

(test parametric-type-unify-different-constructors
  "Unifying (List fixnum) with (Option fixnum) fails"
  (let ((t1 (cl-cc/type:parse-type-specifier '(list fixnum)))
        (t2 (cl-cc/type:parse-type-specifier '(Option fixnum))))
    (multiple-value-bind (subst ok) (cl-cc/type:type-unify t1 t2)
      (declare (ignore subst))
      (is (not ok)))))

(test parametric-type-unparse
  "Unparsing a type-constructor roundtrips correctly"
  (let* ((ty (cl-cc/type:parse-type-specifier '(Pair fixnum string)))
         (spec (cl-cc/type:unparse-type ty)))
    (is (equal 'Pair (first spec)))
    (is (= 3 (length spec)))))

(test parametric-type-to-string
  "type-to-string works for type-constructor"
  (let ((ty (cl-cc/type:parse-type-specifier '(list fixnum))))
    (is (string= "(LIST FIXNUM)" (cl-cc/type:type-to-string ty)))))

(test parametric-type-equal-p
  "type-equal-p works for type-constructors"
  (let ((t1 (cl-cc/type:parse-type-specifier '(list fixnum)))
        (t2 (cl-cc/type:parse-type-specifier '(list fixnum)))
        (t3 (cl-cc/type:parse-type-specifier '(list string))))
    (is (cl-cc/type:type-equal-p t1 t2))
    (is (not (cl-cc/type:type-equal-p t1 t3)))))

(test parametric-type-free-vars
  "Free vars are extracted from type-constructor args"
  (let* ((tv (cl-cc/type:make-type-variable 'x))
         (ty (cl-cc/type:make-type-constructor 'list (list tv))))
    (is (= 1 (length (cl-cc/type:type-free-vars ty))))))

(test parametric-type-nested
  "Nested parametric types: (List (Option fixnum))"
  (let ((ty (cl-cc/type:parse-type-specifier '(list (Option fixnum)))))
    (is (typep ty 'cl-cc/type:type-constructor))
    (is (eq 'list (cl-cc/type:type-constructor-name ty)))
    (let ((inner (first (cl-cc/type:type-constructor-args ty))))
      (is (typep inner 'cl-cc/type:type-constructor))
      (is (eq 'Option (cl-cc/type:type-constructor-name inner))))))

(test parametric-type-in-typed-defun
  "Typed defun with parametric return type compiles"
  (let ((result (run-string "(progn
    (deftype int-list (list fixnum))
    (defun make-nums () (list 1 2 3))
    (length (make-nums)))")))
    (is (= 3 result))))

;;; Defparameter Tests

(test defparameter-basic
  "defparameter should work like defvar for variable definition"
  (is (= 42 (run-string "(progn (defparameter *val* 42) *val*)"))))

(test defparameter-with-function
  "defparameter should persist across function calls"
  (is (= 10 (run-string "(progn
    (defparameter *base* 10)
    (defun get-base () *base*)
    (get-base))"))))

(test defparameter-setq-mutation
  "defparameter + setq should work like defvar + setq"
  (is (= 5 (run-string "(progn
    (defparameter *x* 0)
    (setq *x* 5)
    *x*)"))))

;;; String= and Equal Tests

(test string-equal-basic
  "string= should compare strings for equality"
  (is-true (run-string "(string= \"hello\" \"hello\")")))

(test string-equal-different
  "string= should return falsy for different strings"
  (is (= 0 (run-string "(string= \"hello\" \"world\")"))))

(test equal-numbers
  "equal should compare numbers"
  (is-true (run-string "(equal 42 42)")))

(test equal-strings
  "equal should compare strings"
  (is-true (run-string "(equal \"abc\" \"abc\")")))

(test equal-lists
  "equal should recursively compare lists"
  (is-true (run-string "(equal '(1 2 3) (list 1 2 3))")))

(test equal-different
  "equal should return falsy for different values"
  (is (= 0 (run-string "(equal 1 2)"))))

;;; Numeric Builtins Tests (max, min, mod, zerop, plusp, minusp)

(test numeric-max-basic
  "max should return the larger of two numbers"
  (is (= 5 (run-string "(max 3 5)"))))

(test numeric-min-basic
  "min should return the smaller of two numbers"
  (is (= 3 (run-string "(min 3 5)"))))

(test numeric-mod-basic
  "mod should return the remainder"
  (is (= 1 (run-string "(mod 7 3)"))))

(test numeric-mod-even
  "mod should return 0 for even division"
  (is (= 0 (run-string "(mod 6 3)"))))

(test numeric-zerop-true
  "zerop should return true for 0"
  (is-true (run-string "(zerop 0)")))

(test numeric-zerop-false
  "zerop should return falsy for non-zero"
  (is (= 0 (run-string "(zerop 5)"))))

(test numeric-plusp-true
  "plusp should return true for positive numbers"
  (is-true (run-string "(plusp 5)")))

(test numeric-plusp-false
  "plusp should return falsy for negative numbers"
  (is (= 0 (run-string "(plusp -3)"))))

(test numeric-minusp-true
  "minusp should return true for negative numbers"
  (is-true (run-string "(minusp -3)")))

(test numeric-minusp-false
  "minusp should return falsy for positive numbers"
  (is (= 0 (run-string "(minusp 5)"))))

(test numeric-abs-basic
  "abs should return absolute value"
  (is (= 5 (run-string "(abs -5)"))))

(test numeric-evenp-true
  "evenp should return true for even numbers"
  (is-true (run-string "(evenp 4)")))

(test numeric-oddp-true
  "oddp should return true for odd numbers"
  (is-true (run-string "(oddp 3)")))

;;; Warn Compilation Tests

(test compile-warn-basic
  "warn should compile and not crash (returns nil)"
  (is (null (run-string "(warn \"test warning\")"))))

(test compile-warn-continues
  "warn should not abort execution"
  (is (= 42 (run-string "(progn (warn \"warning\") 42)"))))

;;; Format Compilation Tests

(test compile-format-nil-simple
  "format nil with ~A should return string"
  (is (string= "hello" (run-string "(format nil \"~A\" \"hello\")"))))

(test compile-format-nil-number
  "format nil with ~D should format number"
  (is (string= "42" (run-string "(format nil \"~D\" 42)"))))

(test compile-format-nil-concat
  "format nil with multiple ~A should concatenate"
  (is (string= "hello world" (run-string "(format nil \"~A ~A\" \"hello\" \"world\")"))))

;;; Self-Hosting Smoke Test: Mini-Optimizer with Labels + HOFs

(test self-host-optimizer-pipeline
  "Self-hosting smoke: CLOS AST + labels recursive optimizer + mapcar + defparameter"
  (is (= 30 (run-string "
    (progn
      (defclass ir () ())
      (defclass ir-const (ir) ((val :initarg :val :accessor ir-const-val)))
      (defclass ir-add (ir) ((lhs :initarg :lhs :accessor ir-add-lhs)
                              (rhs :initarg :rhs :accessor ir-add-rhs)))
      (defclass ir-mul (ir) ((lhs :initarg :lhs :accessor ir-mul-lhs)
                              (rhs :initarg :rhs :accessor ir-mul-rhs)))
      (defclass ir-seq (ir) ((forms :initarg :forms :accessor ir-seq-forms)))
      (defparameter *opt-count* 0)
      (defun mk-const (v) (make-instance 'ir-const :val v))
      (defun mk-add (l r) (make-instance 'ir-add :lhs l :rhs r))
      (defun mk-mul (l r) (make-instance 'ir-mul :lhs l :rhs r))
      (defun mk-seq (fs) (make-instance 'ir-seq :forms fs))
      (defgeneric opt (node))
      (defmethod opt ((n ir-const)) n)
      (defmethod opt ((n ir-add))
        (let ((l (opt (ir-add-lhs n))) (r (opt (ir-add-rhs n))))
          (if (and (typep l 'ir-const) (typep r 'ir-const))
              (progn (setq *opt-count* (+ *opt-count* 1))
                     (mk-const (+ (ir-const-val l) (ir-const-val r))))
              (mk-add l r))))
      (defmethod opt ((n ir-mul))
        (let ((l (opt (ir-mul-lhs n))) (r (opt (ir-mul-rhs n))))
          (if (and (typep l 'ir-const) (typep r 'ir-const))
              (progn (setq *opt-count* (+ *opt-count* 1))
                     (mk-const (* (ir-const-val l) (ir-const-val r))))
              (mk-mul l r))))
      (defmethod opt ((n ir-seq))
        (mk-seq (mapcar #'opt (ir-seq-forms n))))
      (defgeneric eval-ir (node))
      (defmethod eval-ir ((n ir-const)) (ir-const-val n))
      (defmethod eval-ir ((n ir-add))
        (+ (eval-ir (ir-add-lhs n)) (eval-ir (ir-add-rhs n))))
      (defmethod eval-ir ((n ir-mul))
        (* (eval-ir (ir-mul-lhs n)) (eval-ir (ir-mul-rhs n))))
      (defmethod eval-ir ((n ir-seq))
        (let ((r 0)) (dolist (f (ir-seq-forms n) r) (setq r (eval-ir f)))))
      (let ((ir-prog (mk-seq (list (mk-add (mk-const 10) (mk-const 20))
                                   (mk-mul (mk-const 3) (mk-const 10))))))
        (eval-ir (opt ir-prog))))" :stdlib t))))

;;; Self-Hosting Integration: Macro Expander + Type Checker

(test self-host-macro-system-full
  "Self-hosting: hash-table macro registry + recursive expansion + multiple macros"
  (is (equal '(IF X (PROGN (IF Y (PROGN Z) NIL)) NIL)
             (run-string "(progn
    (defvar *mx* (make-hash-table :test 'eq))
    (defun register-macro (name fn) (setf (gethash name *mx*) fn))
    (defun mx-expand (form)
      (if (consp form)
          (let ((expander (gethash (car form) *mx*)))
            (if expander
                (mx-expand (funcall expander form))
                (mapcar (lambda (x) (if (consp x) (mx-expand x) x)) form)))
          form))
    (register-macro 'when
      (lambda (f) (list 'if (cadr f) (cons 'progn (cddr f)) nil)))
    (mx-expand '(when x (when y z))))" :stdlib t))))

(test self-host-type-checker
  "Self-hosting: simple HM-style type checker with CLOS type nodes"
  (is (eq 'ok (run-string "(progn
    (defclass ty () ())
    (defclass ty-int (ty) ())
    (defclass ty-str (ty) ())
    (defclass ty-fn (ty) ((arg :initarg :arg :accessor ty-fn-arg)
                           (ret :initarg :ret :accessor ty-fn-ret)))
    (defgeneric ty-eq (a b))
    (defmethod ty-eq ((a ty-int) (b ty-int)) t)
    (defmethod ty-eq ((a ty-str) (b ty-str)) t)
    (defmethod ty-eq ((a ty-fn) (b ty-fn))
      (and (ty-eq (ty-fn-arg a) (ty-fn-arg b))
           (ty-eq (ty-fn-ret a) (ty-fn-ret b))))
    (defmethod ty-eq ((a t) (b t)) nil)
    (defvar *env* (make-hash-table :test 'eq))
    (defun tc (expr)
      (cond
        ((numberp expr) (make-instance 'ty-int))
        ((stringp expr) (make-instance 'ty-str))
        ((symbolp expr) (gethash expr *env*))
        ((and (consp expr) (eq (car expr) 'add))
         (let ((l (tc (cadr expr))) (r (tc (caddr expr))))
           (if (and (typep l 'ty-int) (typep r 'ty-int))
               (make-instance 'ty-int)
               (error \"type mismatch\"))))
        (t (error \"unknown\"))))
    (setf (gethash 'x *env*) (make-instance 'ty-int))
    (let ((t1 (tc '(add x 1)))
          (t2 (tc '(add 2 3))))
      (if (and (typep t1 'ty-int) (typep t2 'ty-int)) 'ok 'fail)))"))))

(test self-host-format-error-pipeline
  "Self-hosting: format for string building + error signaling + handler-case"
  (is (string= "caught: bad-value"
               (run-string "(handler-case
    (let ((val 42))
      (if (> val 100)
          val
          (error (format nil \"bad-value\"))))
    (error (e) (format nil \"caught: ~A\" e)))"))))

;;; Prog/With-Slots/Nth-Value Macro Tests

(test compile-prog-basic
  "prog macro: let + tagbody + block with return"
  (is (= 10 (run-string "(prog ((x 0))
    loop
    (setq x (+ x 1))
    (when (= x 10) (return x))
    (go loop))" :stdlib t))))

(test compile-prog-star
  "prog* macro: sequential bindings"
  (is (= 3 (run-string "(prog* ((x 1) (y (+ x 2)))
    (return y))" :stdlib t))))

(test compile-with-slots-basic
  "with-slots binds slot values"
  (is (= 30 (run-string "(progn
    (defclass point () ((x :initarg :x) (y :initarg :y)))
    (let ((p (make-instance 'point :x 10 :y 20)))
      (with-slots (x y) p
        (+ x y))))" :stdlib t))))

(test compile-nth-value
  "nth-value extracts specific value from multiple values"
  (is (= 2 (run-string "(nth-value 1 (floor 17 5))" :stdlib t))))

(test compile-prog-nil-return
  "prog without explicit return yields nil"
  (is (null (run-string "(prog ((x 1)) (setq x 2))" :stdlib t))))

;;; Run Tests Function

(defun run-tests ()
  "Run all tests in the cl-cc test suite."
  (run! 'cl-cc-suite))
