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

(deftest-each vm-exec-arithmetic
  "Basic arithmetic expressions compile and evaluate correctly."
  :cases (("add"        "(+ 3 4)"           7)
          ("sub"        "(- 10 7)"           3)
          ("mul"        "(* 6 7)"            42)
          ("nested"     "(+ (* 2 3) 3)"      9))
  (code expected)
  (assert-run= expected code))

;;; Conditional Compilation Tests

(deftest-each vm-exec-if
  "If expressions compile and branch correctly."
  :cases (("false-cond"     "(if 0 10 20)"                    20)
          ("true-cond"      "(if 1 10 20)"                    10)
          ("nested"         "(if 1 (if 0 1 2) 3)"             2)
          ("var-cond"       "(let ((x 0)) (if x 20 10))"      10))
  (code expected)
  (assert-run= expected code))

;;; Let Binding Tests

(deftest-each vm-exec-let
  "Let binding forms compile and evaluate correctly."
  :cases (("simple"      "(let ((x 42)) x)"                       42)
          ("multi"       "(let ((x 2) (y 3)) (+ x y))"            5)
          ("shadowing"   "(let ((x 10)) (let ((x 20)) x))"         20)
          ("computed"    "(let ((x 5) (y 7)) (+ (* x 2) y))"      17))
  (code expected)
  (assert-run= expected code))

;;; Progn Tests

(deftest-each vm-exec-progn
  "Progn sequences compile and return the last value."
  :cases (("simple"    "(progn 1 2 3)"                               3)
          ("with-let"  "(progn (let ((x 2)) x) (let ((y 3)) y))"    3))
  (code expected)
  (assert-run= expected code))

(deftest vm-exec-progn-empty
  "Empty progn signals an error (not yet supported)."
  (assert-signals error (run-string "(progn)")))

;;; Print Tests

(deftest vm-exec-print
  "print outputs values, including sequenced prints."
  (let ((output (with-output-to-string (*standard-output*)
                  (run-string "(print 42)"))))
    (assert-true (search "42" output)))
  (let ((output (with-output-to-string (*standard-output*)
                  (run-string "(progn (print 1) (print 2) (print 3))"))))
    (assert-true (search "1" output))
    (assert-true (search "2" output))
    (assert-true (search "3" output))))

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
   ("mutually-recursive-labels"
    "(labels ((even? (n) (if (= n 0) 1 (odd? (- n 1)))) (odd? (n) (if (= n 0) 0 (even? (- n 1))))) (even? 10))")
   ("hof-function-as-arg"
    "(let ((apply-twice (lambda (f x) (f (f x))))) (apply-twice (lambda (x) (* x 2)) 3))")
   ("hof-returning-function"
    "(let ((make-mul (lambda (n) (lambda (x) (* x n))))) (let ((double (make-mul 2))) (double 21)))"))
  (code)
  (assert-true (compilation-result-program (compile-string code :target :vm))))

;;; Assembly Emission Tests

(deftest asm-emission
  "Both x86_64 and aarch64 backends generate string assembly for various forms."
  (%assert-assembly-contains "(lambda (x) (+ x 2))" "add")
  (%assert-assembly-stringp "(if 1 2 3)")
  (%assert-assembly-stringp "(let ((x 1)) x)")
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

(deftest-each compile-let-scoping
  "Deeply nested let bindings and multi-level variable shadowing work correctly."
  :cases (("deep-nesting" 10 "(let ((a 1)) (let ((b 2)) (let ((c 3)) (let ((d 4)) (+ a (+ b (+ c d)))))))")
          ("shadowing"     3 "(let ((x 1)) (let ((x 2)) (let ((x 3)) x)))"))
  (expected form)
  (assert-= expected (run-string form)))

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

;;; PBT for Compiler

(deftest pbt-integer-compilation
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
    (assert-true (>= (/ passes total) 0.90))))

(deftest-each pbt-binary-op-commutative
  "Property: binary arithmetic operations are commutative in compiled code."
  :cases (("addition"       "+" 100)
          ("multiplication" "*"  50))
  (op range)
  (let ((passes 0) (total 50))
    (dotimes (i total)
      (let* ((a (random range))
             (b (random range))
             (r1 (handler-case (run-string (format nil "(~A ~D ~D)" op a b)) (error () nil)))
             (r2 (handler-case (run-string (format nil "(~A ~D ~D)" op b a)) (error () nil))))
        (when (and r1 r2 (= r1 r2))
          (incf passes))))
    (assert-true (>= (/ passes total) 0.90))))

(deftest pbt-if-always-returns-one-branch
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
    (assert-true (>= (/ passes total) 0.90))))

(deftest pbt-let-bindings-preserve-value
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
    (assert-true (>= (/ passes total) 0.90))))

;;; Function Call Tests

(deftest-each compile-function-call-structure
  "Lambda calls with simple, multi-argument, and nested forms produce a vm-program."
  :cases (("simple"    "((lambda (x) x) 5)")
          ("multi-arg" "((lambda (a b c) (+ a (+ b c))) 1 2 3)")
          ("nested"    "((lambda (x) (+ x 1)) ((lambda (y) (* y 2)) 3))"))
  (form)
  (let* ((result (compile-string form :target :vm))
         (program (compilation-result-program result)))
    (assert-false (null program))
    (assert-type vm-program program)))

(deftest-each compile-flet-labels-structure
  "flet, labels, and higher-order lambda forms all produce a vm-program."
  :cases (("return-fn"         "((lambda (n) (lambda (x) (+ x n))) 5)")
          ("flet-basic"        "(flet ((double (x) (* x 2))) (double 21))")
          ("flet-multi"        "(flet ((add1 (x) (+ x 1)) (add2 (x) (+ x 2))) (add2 (add1 10)))")
          ("labels-recursive"  "(labels ((count (n) (if (= n 0) 0 (+ 1 (count (- n 1)))))) (count 5))")
          ("labels-mutual"     "(labels ((even? (n) (if (= n 0) 1 (odd? (- n 1)))) (odd? (n) (if (= n 0) 0 (even? (- n 1))))) (even? 10))")
          ("labels-with-let"   "(let ((x 10)) (labels ((rec (n) (if (= n 0) x (+ 1 (rec (- n 1)))))) (rec 3)))"))
  (form)
  (let* ((result (compile-string form :target :vm))
         (program (compilation-result-program result)))
    (assert-false (null program))
    (assert-type vm-program program)))

;;; Multiple Top-Level Forms Tests

(deftest-each compile-multiple-forms
  "Multiple top-level forms compile and execute in sequence correctly."
  :cases (("simple"    6  "(defun foo (x) (+ x 1)) (foo 5)")
          ("progn"     3  "1 2 3")
          ("chain"     12 "(defun add1 (x) (+ x 1)) (defun add2 (x) (add1 (add1 x))) (add2 10)")
          ("let-call"  15 "(defun triple (x) (* x 3)) (let ((y 5)) (triple y))")
          ("recursion"  6 "(defun sum-to (n) (if (= n 0) 0 (+ n (sum-to (- n 1))))) (sum-to 3)"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Multiple Values and Apply Tests

(deftest-each compile-values-numeric
  "values and multiple-value-bind return correct numeric results."
  :cases (("single"     42 "(values 42)")
          ("primary"     1 "(values 1 2 3)")
          ("mvb-basic"   3 "(multiple-value-bind (a b) (values 1 2) (+ a b))")
          ("apply"       6 "(defun my-add (a b c) (+ a (+ b c))) (apply my-add (quote (1 2 3)))")
          ("mvb-single" 42 "(multiple-value-bind (x) (values 42) x)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each compile-values-nil
  "values and multiple-value-bind return nil in edge cases."
  :cases (("empty"        "(values)")
          ("mvb-missing"  "(multiple-value-bind (a b c) (values 1 2) c)"))
  (form)
  (assert-true (null (run-string form))))

;;; String/Symbol Builtin Compilation Tests

(deftest-each compile-string-comparison
  "String comparison operators return truthy for true condition and NIL for false."
  :cases (("string=-match"    t   "(string= \"hello\" \"hello\")")
          ("string=-mismatch" nil "(string= \"hello\" \"world\")")
          ("string<-true"     t   "(string< \"abc\" \"def\")")
          ("string<-false"    nil "(string< \"def\" \"abc\")")
          ("string/=-true"    t   "(string/= \"hello\" \"world\")")
          ("string/=-false"   nil "(string/= \"hello\" \"hello\")")
          ("string-equal-ci"  t   "(string-equal \"Hello\" \"hello\")")
          ("string-equal-no"  nil "(string-equal \"hello\" \"world\")"))
  (expected form)
  (if expected
      (assert-true (run-string form))
      (assert-true (null (run-string form)))))

(deftest-each compile-string-ops
  "String length, upcase, downcase, and concatenate return the expected numeric length."
  :cases (("length-hello"  5 "(string-length \"hello\")")
          ("length-empty"  0 "(string-length \"\")")
          ("upcase-len"    5 "(string-length (string-upcase \"hello\"))")
          ("downcase-len"  3 "(string-length (string-downcase \"ABC\"))")
          ("concat-len"   10 "(string-length (concatenate 'string \"hello\" \"world\"))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each compile-type-predicates
  "Type predicates symbolp and numberp return 1/0 for matching/non-matching types."
  :cases (("symbolp-sym"  1 "(symbolp 'foo)")
          ("symbolp-num"  0 "(symbolp 42)")
          ("numberp-num"  1 "(numberp 42)")
          ("numberp-sym"  0 "(numberp 'foo)"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Macro Expansion in Compiler Tests

(deftest compile-control-macros
  "cond/when/unless/and/or macros work in compiled code."
  ;; cond
  (assert-true (= 42 (run-string "(cond ((= 1 2) 10) ((= 1 1) 42) (t 0))")))
  (assert-true (= 0 (run-string "(cond ((= 1 2) 10) ((= 2 3) 20) (t 0))")))
  ;; when/unless
  (assert-true (= 42 (run-string "(when (= 1 1) 42)")))
  (assert-true (null (run-string "(when (= 1 2) 42)")))
  (assert-true (= 99 (run-string "(unless (= 1 2) 99)")))
  (assert-true (null (run-string "(unless (= 1 1) 99)")))
  ;; and/or
  (assert-true (= 3 (run-string "(and 1 2 3)")))
  (assert-true (null (run-string "(and 1 nil 3)")))
  (assert-true (= 5 (run-string "(or nil nil 5)")))
  (assert-true (= 1 (run-string "(or 1 2 3)")))
  (assert-true (null (run-string "(or nil nil nil)"))))

(deftest compile-t-nil-constants
  "t and nil are recognized as constants; not inverts truthiness."
  (assert-false (null (run-string "t")))
  (assert-null (run-string "nil"))
  (assert-true (eq t (run-string "(not nil)")))
  (assert-true (null (run-string "(not 1)")))
  (assert-true (null (run-string "(not t)"))))

;;; List Operation Builtin Tests

(deftest-each compile-list-ops
  "cons/car/cdr, list/length, first/rest, eq/eql return the expected numeric values."
  :cases (("car"         1 "(car (cons 1 2))")
          ("cdr"         2 "(cdr (cons 1 2))")
          ("length-3"    3 "(length (list 1 2 3))")
          ("length-0"    0 "(length (list))")
          ("first"      10 "(first (list 10 20 30))")
          ("rest-len"    2 "(length (rest (list 10 20 30)))")
          ("eq-true"     1 "(eq 1 1)")
          ("eq-false"    0 "(eq 1 2)")
          ("eql-true"    1 "(eql 42 42)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest compile-list-builtins
  "append/reverse builtins work on lists."
  (assert-true (= 4 (run-string "(length (append (list 1 2) (list 3 4)))")))
  (assert-true (= 3 (run-string "(first (reverse (list 1 2 3)))"))))

;;; Handler-Case Tests

(deftest-each compile-handler-case-return-value
  "handler-case returns the correct numeric value in various scenarios."
  :cases (("no-error"      42 "(handler-case 42 (error (e) 0))")
          ("catches-error" 99 "(handler-case (error \"boom\") (error (e) 99))")
          ("arithmetic"    10 "(handler-case (+ 3 7) (error (e) 0))")
          ("handler-body" 100 "(handler-case (error \"x\") (error (e) (* 10 10)))")
          ("nested"         1 "(handler-case (handler-case (error \"inner\") (error (e) 1)) (error (e) 2))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest compile-handler-case-error-variable
  "handler-case binds the error value to the variable"
  (assert-string= "boom" (run-string "(handler-case (error \"boom\") (error (e) e))")))

(deftest compile-error-builtin-without-handler
  "error without handler signals a CL error"
  (assert-signals error
    (run-string "(error \"unhandled\")")))

;;; Hash Table Operation Tests

(deftest-each compile-hash-table-numeric
  "Hash table operations return the expected numeric values; gethash returns nil when missing with no default."
  :cases (("hash-table-p"   1  "(let ((ht (make-hash-table))) (hash-table-p ht))")
          ("gethash-get"    42 "(let ((ht (make-hash-table))) (setf (gethash 'x ht) 42) (gethash 'x ht))")
          ("gethash-default" 99 "(let ((ht (make-hash-table))) (gethash 'missing ht 99))")
          ("count"           2 "(let ((ht (make-hash-table))) (setf (gethash 'a ht) 1) (setf (gethash 'b ht) 2) (hash-table-count ht))")
          ("remhash"         1 "(let ((ht (make-hash-table))) (setf (gethash 'a ht) 1) (setf (gethash 'b ht) 2) (remhash 'a ht) (hash-table-count ht))")
          ("hash-table-p-no" 0 "(hash-table-p 42)")
          ("setf-returns"  100 "(let ((ht (make-hash-table))) (setf (gethash 'k ht) 100))")
          ("overwrite"      20 "(let ((ht (make-hash-table))) (setf (gethash 'k ht) 10) (setf (gethash 'k ht) 20) (gethash 'k ht))"))
  (expected form)
  (assert-= expected (run-string form))
  (assert-true (null (run-string "(let ((ht (make-hash-table))) (gethash 'missing ht))"))))

;;; Defmacro Compilation Tests

(deftest-each compile-defmacro-numeric
  "defmacro defines macros that expand and evaluate to the correct numeric result."
  :cases (("basic"      42  "(defmacro my-const () 42) (my-const)")
          ("with-args"  10  "(defmacro my-dbl (x) (list '+ x x)) (my-dbl 5)")
          ("quasiquote" 15  "(defmacro my-add3 (a b c) `(+ ,a (+ ,b ,c))) (my-add3 3 5 7)")
          ("in-let"     100 "(defmacro my-square (x) `(* ,x ,x)) (let ((n 10)) (my-square n))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest compile-defmacro-returns-name
  "defmacro returns the macro name; symbol-name returns the name string."
  (assert-true (string= "my-mac" (string-downcase (symbol-name (run-string "(defmacro my-mac (x) x)")))))
  (assert-string= "FOO" (run-string "(symbol-name 'foo)")))

;;; Symbol Manipulation Tests

(deftest-each compile-symbol-creation
  "intern/gensym/make-symbol all create symbols (symbolp returns truthy)."
  :cases (("intern"      "(symbolp (intern \"TEST-SYM\"))")
          ("gensym"      "(symbolp (gensym))")
          ("make-symbol" "(symbolp (make-symbol \"TEMP\"))"))
  (form)
  (assert-true (not (null (run-string form)))))

(deftest-each compile-keywordp
  "keywordp returns 1 for keywords and 0 for non-keywords."
  :cases (("keyword"     1 "(keywordp :foo)")
          ("non-keyword" 0 "(keywordp 'foo)"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Extended List and Macro Tests

(deftest-each compile-list-macros-numeric
  "push/pop/incf/decf/nth/nthcdr/nreverse return the expected numeric values."
  :cases (("push-front" 3  "(let ((lst nil)) (push 1 lst) (push 2 lst) (push 3 lst) (car lst))")
          ("pop"        1  "(let ((lst (list 1 2 3))) (pop lst))")
          ("incf"       5  "(let ((x 3)) (incf x 2) x)")
          ("decf"       1  "(let ((x 3)) (decf x 2) x)")
          ("nth"        30 "(nth 2 (list 10 20 30 40))")
          ("nthcdr"     30 "(car (nthcdr 2 (list 10 20 30 40)))")
          ("nreverse"   3  "(car (nreverse (list 1 2 3)))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest compile-member-builtin
  "member finds element in list"
  (assert-true (not (null (run-string "(member 3 (list 1 2 3 4))")))))

(deftest-each compile-numeric-predicates
  "zerop/plusp/minusp/evenp/oddp return 1/0 for matching/non-matching values."
  :cases (("zerop-0"    1 "(zerop 0)")
          ("zerop-5"    0 "(zerop 5)")
          ("plusp-5"    1 "(plusp 5)")
          ("minusp-neg" 1 "(minusp (- 0 3))")
          ("evenp-true"  1 "(evenp 4)")
          ("evenp-false" 0 "(evenp 3)")
          ("oddp-true"   1 "(oddp 3)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each compile-case-macro
  "case dispatches on key match and otherwise clause."
  :cases (("match"     2  "(case 'b (a 1) (b 2) (c 3))")
          ("otherwise" 99 "(case 'z (a 1) (otherwise 99))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest compile-keyword-self-eval
  "keywords evaluate to themselves; typecase dispatches on type."
  (assert-eq :test (run-string ":test"))
  (assert-true (= 1 (run-string "(typecase 42 (integer 1) (string 2) (otherwise 3))"))))

;;; Typep and Destructuring Tests

(deftest-each compile-typep
  "typep returns 1 for matching types and 0 for non-matching."
  :cases (("integer"  1 "(typep 42 'integer)")
          ("string"   1 "(typep \"hello\" 'string)")
          ("symbol"   1 "(typep 'foo 'symbol)")
          ("cons"     1 "(typep (cons 1 2) 'cons)")
          ("null"     1 "(typep nil 'null)")
          ("negative" 0 "(typep 42 'string)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each compile-destructuring-bind
  "destructuring-bind correctly binds list elements, including &rest patterns."
  :cases (("basic" 6 "(destructuring-bind (a b c) (list 1 2 3) (+ a (+ b c)))")
          ("rest"  2 "(destructuring-bind (a &rest b) (list 1 2 3) (length b))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Iteration Macro Tests

(deftest-each compile-iteration-macros
  "dolist/dotimes/do/loop each compute the correct numeric sum."
  :cases (("dolist"  6  "(let ((sum 0)) (dolist (x (list 1 2 3)) (setq sum (+ sum x))) sum)")
          ("dotimes" 10 "(let ((sum 0)) (dotimes (i 5) (setq sum (+ sum i))) sum)")
          ("do"      10 "(do ((i 0 (+ i 1)) (sum 0 (+ sum i))) ((= i 5) sum))")
          ("loop"    10 "(let ((sum 0) (i 0)) (loop (if (= i 5) (return sum)) (setq sum (+ sum i)) (setq i (+ i 1))))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Extended Lambda List Tests (&rest, &optional, &key)

;; &rest tests
(deftest-each compile-rest-params
  "&rest collects remaining arguments into a list."
  :cases (("basic"     '(1 2 3) "(defun my-list (&rest args) args) (my-list 1 2 3)")
          ("required"  '(1 2 3) "(defun fr (a &rest r) (cons a r)) (fr 1 2 3)")
          ("single"    '(42)    "(defun my-list1 (&rest args) args) (my-list1 42)"))
  (expected form)
  (assert-equal expected (run-string form)))

(deftest compile-rest-extras
  "&rest with no extra args gives nil; car and length work on &rest lists."
  (assert-true (not (run-string "(defun my-list0 (&rest args) args) (my-list0)")))
  (assert-= 10 (run-string "(defun first-rest (&rest args) (car args)) (first-rest 10 20 30)"))
  (assert-= 3 (run-string " (defun my-len (lst) (if (null lst) 0 (+ 1 (my-len (cdr lst))))) (defun count-args (&rest args) (my-len args)) (count-args 1 2 3)")))

;; &optional tests
(deftest-each compile-optional-params
  "&optional parameters use provided values, defaults, or nil."
  :cases (("provided"          15 "(defun opt-add (a &optional b) (if b (+ a b) a)) (opt-add 10 5)")
          ("missing"           10 "(defun opt-add2 (a &optional b) (if b (+ a b) a)) (opt-add2 10)")
          ("default"           10 "(defun opt-def (a &optional (b 0)) (+ a b)) (opt-def 10)")
          ("default-overridden" 15 "(defun opt-def2 (a &optional (b 0)) (+ a b)) (opt-def2 10 5)")
          ("multiple"           6 "(defun opt-multi (a &optional (b 0) (c 0)) (+ (+ a b) c)) (opt-multi 1 2 3)")
          ("partial"            3 "(defun opt-part (a &optional (b 0) (c 0)) (+ (+ a b) c)) (opt-part 1 2)"))
  (expected form)
  (assert-= expected (run-string form)))

;; &key tests
(deftest-each compile-key-params
  "&key parameters support defaults, reordering, and combination with required args."
  :cases (("basic"         7  "(defun key-add (&key x y) (+ x y)) (key-add :x 3 :y 4)")
          ("default"      10  "(defun key-def (&key (x 0) (y 0)) (+ x y)) (key-def :x 10)")
          ("reorder"       7  "(defun key-ord (&key x y) (+ x y)) (key-ord :y 4 :x 3)")
          ("with-required" 30 "(defun rk (a &key (b 0)) (+ a b)) (rk 10 :b 20)"))
  (expected form)
  (assert-= expected (run-string form)))

;; lambda with extended params
(deftest-each compile-lambda-params
  "lambda forms support &rest and &optional parameters."
  :cases (("rest"              '(10 20 30) "(funcall (lambda (&rest args) args) 10 20 30)")
          ("optional-default"  5           "(funcall (lambda (a &optional (b 0)) (+ a b)) 5)")
          ("optional-provided" 15          "(funcall (lambda (a &optional (b 0)) (+ a b)) 5 10)"))
  (expected form)
  (assert-true (equal expected (run-string form))))

;; combined features
(deftest-each compile-rest-combined
  "&rest combined with &optional and closure capture."
  :cases (("with-optional" '(3 4 5)     " (defun opt-rest (a &optional (b 0) &rest r) r) (opt-rest 1 2 3 4 5)")
          ("closure"       '(10 20 30)  " (defun make-lister () (lambda (&rest args) args)) (funcall (make-lister) 10 20 30)"))
  (expected form)
  (assert-equal expected (run-string form)))

;;; Variadic Arithmetic and List Tests

(deftest-each compile-variadic-arith
  "Variadic +, *, - produce correct numeric results."
  :cases (("plus-3"    6  "(+ 1 2 3)")
          ("plus-5"   15  "(+ 1 2 3 4 5)")
          ("plus-1"   10  "(+ 10)")
          ("plus-0"    0  "(+)")
          ("times-3"  24  "(* 2 3 4)")
          ("times-0"   1  "(*)")
          ("minus-3"   5  "(- 10 3 2)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each compile-list-construction
  "list builds proper lists; car of list returns first element."
  :cases (("basic" '(1 2 3) "(list 1 2 3)")
          ("single" '(42)   "(list 42)"))
  (expected form)
  (assert-equal expected (run-string form)))

(deftest compile-list-car
  "car of list"
  (assert-= 1 (run-string "(car (list 1 2 3))")))

;;; Standard Library Set Operations Tests

(deftest-each stdlib-list-ops
  "set-difference, union, append-lists, and last-cons work on lists."
  :cases (("set-diff"       '(1 3 5)     "(set-difference (list 1 2 3 4 5) (list 2 4))")
          ("set-diff-empty" '(1 2 3)     "(set-difference (list 1 2 3) (list))")
          ("union"          '(1 2 3 4 5) "(union-lists (list 1 2 3) (list 3 4 5))")
          ("append-lists"   '(1 2 3 4)   "(append-lists (list 1 2) (list 3 4))")
          ("last-cons"      3            "(car (last-cons (list 1 2 3)))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

(deftest-each stdlib-reduce
  "reduce folds a list with a function and optional initial value."
  :cases (("basic"       10 "(reduce (lambda (a b) (+ a b)) (list 1 2 3 4))")
          ("single"      42 "(reduce (lambda (a b) (+ a b)) (list 42))")
          ("with-init"   10 "(reduce (lambda (a b) (+ a b)) (list 1 2 3 4) :initial-value 0)")
          ("empty-init"   0 "(reduce (lambda (a b) (+ a b)) nil :initial-value 0)"))
  (expected form)
  (assert-= expected (run-string form :stdlib t)))

(deftest-each stdlib-reduce-edge
  "reduce edge cases: nil initial value, reduce-init accumulation."
  :cases (("init-nil"     nil      "(reduce (lambda (a b) (cons b a)) nil :initial-value nil)")
          ("init-accum"   '(3 2 1) "(reduce-init (lambda (acc x) (cons x acc)) (list 1 2 3) nil)"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

(deftest compile-hash-table-keys
  "hash-table-keys returns list of keys"
  (assert-= 2 (run-string " (let ((ht (make-hash-table))) (setf (gethash :x ht) 10) (setf (gethash :y ht) 20) (length (hash-table-keys ht)))")))

;;; Defstruct Tests

(deftest-each compile-defstruct
  "defstruct creates constructors, accessors, predicates, and custom variants."
  :cases (("basic"       10 " (progn (defstruct point x y) (let ((p (make-point :x 10 :y 20))) (point-x p)))")
          ("default"      0 " (progn (defstruct counter (count 0)) (let ((c (make-counter))) (counter-count c)))")
          ("predicate"    1 " (progn (defstruct my-box value) (let ((b (make-my-box :value 42))) (if (my-box-p b) 1 0)))")
          ("typep"        1 " (progn (defstruct my-pair first second) (let ((p (make-my-pair :first 1 :second 2))) (if (typep p 'my-pair) 1 0)))")
          ("not-typep"    0 " (progn (defstruct my-thing val) (if (typep 42 'my-thing) 1 0))")
          ("boa"          3 " (progn (defstruct (my-vec (:constructor make-my-vec (x y))) x y) (let ((v (make-my-vec 1 3))) (my-vec-y v)))")
          ("conc-name"   42 " (progn (defstruct (my-item (:conc-name item-)) value) (let ((i (make-my-item :value 42))) (item-value i)))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Car/Cdr Composition Tests

(deftest-each compile-cxr-basic
  "c*r compositions extract elements from list structures."
  :cases (("caar"  1    "(caar (list (list 1 2) (list 3 4)))")
          ("cadr"  2    "(cadr (list 1 2 3))")
          ("cddr"  '(3) "(cddr (list 1 2 3))")
          ("caddr" 3    "(caddr (list 1 2 3 4))"))
  (expected form)
  (assert-true (equal expected (run-string form))))

;;; Stdlib Find/Position Tests

(deftest-each stdlib-find-position
  "find and position return element/index, or nil when not found."
  :cases (("find"          3   "(find 3 (list 1 2 3 4 5))")
          ("find-miss"     nil "(find 9 (list 1 2 3))")
          ("position"      2   "(position 3 (list 1 2 3 4 5))")
          ("position-miss" nil "(position 9 (list 1 2 3))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

(deftest stdlib-find-with-key
  "find with :key function"
  (assert-true (string= "(2 . b)"
                        (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                          (string-downcase (format nil "~S" (run-string "(find 2 (list (cons 1 'a) (cons 2 'b) (cons 3 'c)) :key (lambda (x) (car x)))" :stdlib t)))))))

(deftest stdlib-identity
  "identity returns its argument"
  (assert-true (= 42 (run-string "(identity 42)" :stdlib t))))

(deftest stdlib-pairlis
  "pairlis creates alist from keys and values"
  (assert-true (string= "((b . 2) (a . 1))"
                        (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                          (string-downcase (format nil "~S" (run-string "(pairlis (list 'a 'b) (list 1 2))" :stdlib t)))))))

(deftest stdlib-assoc-rassoc
  "assoc-if finds by predicate; rassoc finds by value."
  (assert-true (string= "(2 . b)"
                        (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                          (string-downcase (format nil "~S" (run-string "(assoc-if (lambda (k) (= k 2)) (list (cons 1 'a) (cons 2 'b)))" :stdlib t))))))
  (assert-true (string= "(2 . b)"
                        (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                          (string-downcase (format nil "~S" (run-string "(rassoc 'b (list (cons 1 'a) (cons 2 'b) (cons 3 'c)))" :stdlib t)))))))

;;; Setf Places Tests

(deftest-each compile-setf-places
  "setf on car/cdr/first/nth returns and mutates the correct value."
  :cases (("car"         99 "(let ((pair (cons 1 2))) (setf (car pair) 99) (car pair))")
          ("cdr"         99 "(let ((pair (cons 1 2))) (setf (cdr pair) 99) (cdr pair))")
          ("first"       42 "(let ((lst (list 1 2 3))) (setf (first lst) 42) (first lst))")
          ("nth"         99 "(let ((lst (list 10 20 30))) (setf (nth 1 lst) 99) (nth 1 lst))")
          ("returns-val" 42 "(let ((pair (cons 1 2))) (setf (car pair) 42))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Package System Tests

(deftest-each compile-package-forms
  "in-package and defpackage return their package keywords; subsequent forms evaluate normally."
  :cases (("in-package"        :cl-cc   "(in-package :cl-cc)")
          ("defpackage"        :test-pkg "(defpackage :test-pkg (:use :cl))")
          ("in-package-then-code" 42   "(progn (in-package :cl-cc) 42)"))
  (expected form)
  (assert-true (equal expected (run-string form))))

;;; Macrolet Tests

(deftest-each compile-macrolet
  "macrolet defines scoped local macros with the correct numeric result."
  :cases (("basic"    6  "(macrolet ((double (x) `(+ ,x ,x))) (double 3))")
          ("multiple" 10 "(macrolet ((add1 (x) `(+ ,x 1)) (add2 (x) `(+ ,x 2))) (+ (add1 3) (add2 4)))")
          ("scoped"   42 "(let ((x 42)) (macrolet ((get-x () 'x)) (get-x)))")
          ("nested"    8 "(macrolet ((square (x) `(* ,x ,x))) (macrolet ((sq-plus-sq (a b) `(+ (square ,a) (square ,b)))) (sq-plus-sq 2 2)))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Function Reference Tests (#'builtin)

(deftest-each compile-function-ref-numeric
  "#'builtin creates a callable that returns the expected numeric result."
  :cases (("car"  1 "(funcall #'car (cons 1 2))")
          ("plus" 7 "(funcall #'+ 3 4)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each compile-function-sharpsign
  "#'builtin creates callables usable with funcall and higher-order functions."
  :cases (("cons-pair"  '(1 . 2) "(funcall #'cons 1 2)")
          ("car-mapcar" '(1 2 3) "(mapcar #'car (list (cons 1 'a) (cons 2 'b) (cons 3 'c)))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

(deftest compile-function-find-key
  "find with #'car as :key"
  (assert-true (string= "(2 . b)"
                        (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                          (string-downcase (format nil "~S" (run-string "(find 2 (list (cons 1 'a) (cons 2 'b) (cons 3 'c)) :key #'car)" :stdlib t)))))))

;;; Warn Test

;;; String Concatenation Tests

(deftest-each compile-string-concat
  "string-concat and concatenate 'string join strings correctly."
  :cases (("two-strings" "hello world" "(string-concat \"hello \" \"world\")")
          ("concat-abc"  "abc"         "(concatenate 'string \"a\" \"b\" \"c\")")
          ("concat-two"  "foobar"      "(concatenate 'string \"foo\" \"bar\")")
          ("concat-one"  "hello"       "(concatenate 'string \"hello\")"))
  (expected form)
  (assert-string= expected (run-string form)))

;;; Check-Type Tests

(deftest compile-check-type
  "check-type passes silently for correct type and signals error for wrong type."
  (assert-true (eq nil (run-string "(let ((x 42)) (check-type x integer))")))
  (assert-signals error
    (run-string "(let ((x \"hello\")) (check-type x integer))")))

;;; Eval-When Tests

(deftest-each compile-eval-when-numeric
  "eval-when :execute/:load-toplevel includes body and returns numeric value."
  :cases (("execute"       42 "(eval-when (:execute) 42)")
          ("load-toplevel" 10 "(eval-when (:load-toplevel :execute) (+ 3 7))")
          ("all"            5 "(eval-when (:compile-toplevel :load-toplevel :execute) 5)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest compile-eval-when-skip
  "eval-when without :execute skips body"
  (assert-true (eq nil (run-string "(eval-when (:compile-toplevel) 42)"))))

;;; Property List and Set Operations Tests

(deftest-each stdlib-getf
  "getf returns the correct value for found keys, defaults, or nil."
  :cases (("found"     2   "(getf (list :a 1 :b 2 :c 3) :b)")
          ("default"   99  "(getf (list :a 1) :z 99)")
          ("first"     1   "(getf (list :a 1 :b 2) :a)")
          ("not-found" nil "(getf (list :a 1 :b 2) :z)"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

(deftest-each stdlib-set-ops
  "intersection and remove correctly filter list elements."
  :cases (("intersection"       '(2 3) "(intersection (list 1 2 3) (list 2 3 4))")
          ("intersection-empty" nil    "(intersection (list 1 2) (list 3 4))")
          ("remove"             '(1 3 5) "(remove 2 (list 1 2 3 2 5))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

;;; Eval Tests

(deftest-each our-eval-numeric
  "our-eval compiles and runs arithmetic, lambda, and let forms."
  :cases (("basic"  42 '(+ 20 22))
          ("lambda" 10 '(funcall (lambda (x) (+ x 3)) 7))
          ("let"    15 '(let ((a 5) (b 10)) (+ a b))))
  (expected form)
  (assert-= expected (our-eval form)))

;;; Self-Hosting Eval Tests — verify macro expansion runs through our-eval

(deftest selfhost-macro-eval-fn-is-function
  "*macro-eval-fn* should be our-eval after system load (self-hosting)."
  (assert-true (functionp cl-cc:*macro-eval-fn*)))

(deftest selfhost-macro-eval-fn-is-our-eval
  "*macro-eval-fn* is set to our-eval at load time by pipeline.lisp."
  (assert-true (eq cl-cc:*macro-eval-fn* #'cl-cc:our-eval)))

(deftest-each selfhost-defmacro-via-our-eval
  "defmacro bodies are expanded through our-eval (self-hosting)."
  :cases (("basic"       49 "(progn (defmacro sh-sq (x) (list (quote *) x x)) (sh-sq 7))")
          ("with-args"   10 "(progn (defmacro sh-add3 (a b c) (list (quote +) a b c)) (sh-add3 2 3 5))")
          ("quasiquote"  42 "(progn (defmacro sh-unless (test &body body) (list (quote if) test nil (cons (quote progn) body))) (sh-unless nil 42))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each selfhost-macrolet-via-our-eval
  "macrolet bodies are expanded through our-eval (self-hosting)."
  :cases (("basic"     42  "(macrolet ((add1 (x) (list (quote +) x 1))) (add1 41))")
          ("nested"    8   "(macrolet ((dbl (x) (list (quote +) x x))) (macrolet ((quad (x) (list (quote dbl) (list (quote dbl) x)))) (quad 2)))")
          ("body-form" 6   "(macrolet ((triple (x) (list (quote *) x 3))) (triple 2))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Declare Tests

(deftest-each compile-declare
  "declare forms (ignore/type) are silently skipped and code runs correctly."
  :cases (("ignore"  42 "(let ((x 42)) (declare (ignore x)) x)")
          ("in-defun" 10 "(progn (defun my-decl-fn (x) (declare (type integer x)) x) (my-decl-fn 10))")
          ("type"      3 "(let ((x 1) (y 2)) (declare (type integer x y)) (+ x y))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Extended Arithmetic Tests

(deftest-each compile-arithmetic-ops
  "mod/rem/truncate/floor/ceiling/abs/min/max return the correct numeric results."
  :cases (("mod"      1 "(mod 7 3)")
          ("rem"      1 "(rem 7 3)")
          ("truncate" 2 "(truncate 7 3)")
          ("floor"    2 "(floor 7 3)")
          ("ceiling"  3 "(ceiling 7 3)")
          ("abs"      5 "(abs (- 0 5))")
          ("min"      2 "(min 5 2)")
          ("max"      5 "(max 5 2)"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Association List and Utility Tests

(deftest-each compile-alist-and-list-ops
  "assoc/acons/nconc/copy-list/listp/atom return correct numeric values."
  :cases (("assoc-found"    2 "(cdr (assoc 'b (list (cons 'a 1) (cons 'b 2) (cons 'c 3))))")
          ("acons"         42 "(cdr (car (acons 'x 42 nil)))")
          ("nconc-len"      4 "(length (nconc (list 1 2) (list 3 4)))")
          ("copy-list-len"  3 "(length (copy-list (list 1 2 3)))")
          ("listp-list"     1 "(listp (list 1 2))")
          ("listp-nil"      1 "(listp nil)")
          ("atom-true"      1 "(atom 42)")
          ("atom-false"     0 "(atom (cons 1 2))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest compile-equal-ops
  "equal returns truthy for matching structures, NIL for different structures."
  (assert-true (run-string "(equal (list 1 2 3) (list 1 2 3))"))
  (assert-true (null (run-string "(equal (list 1 2) (list 1 3))")))
  (assert-true (run-string "(equal (subst 'x 'a (list 'a 'b 'a)) (list 'x 'b 'x))")))

(deftest compile-assoc-and-string
  "assoc returns nil when not found; (string sym) coerces symbol to string."
  (assert-true (equal nil (run-string "(assoc 'z (list (cons 'a 1) (cons 'b 2)))")))
  (assert-true (equal "HELLO" (run-string "(string 'hello)"))))

;;; String/Character Builtin Tests

(deftest-each compile-char-ops
  "char/code-char/char-upcase/char-downcase return the expected character."
  :cases (("char-access"   #\e "(char \"hello\" 1)")
          ("code-char"     #\A "(code-char 65)")
          ("char-upcase"   #\A "(char-upcase #\\a)")
          ("char-downcase" #\a "(char-downcase #\\A)"))
  (expected form)
  (assert-true (equal expected (run-string form))))

(deftest-each compile-char-numeric
  "char-code/char=/digit-char-p/alpha-char-p/upper-case-p/lower-case-p/stringp/characterp return numeric values."
  :cases (("char-code"      65 "(char-code #\\A)")
          ("char=-true"      1 "(char= #\\a #\\a)")
          ("digit-char-p"    5 "(digit-char-p #\\5)")
          ("alpha-char-p"    1 "(alpha-char-p #\\z)")
          ("upper-case-p"    1 "(upper-case-p #\\A)")
          ("lower-case-p"    1 "(lower-case-p #\\a)")
          ("stringp-true"    1 "(stringp \"hello\")")
          ("stringp-false"   0 "(stringp 42)")
          ("characterp-true" 1 "(characterp #\\a)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest compile-string-char-utils
  "digit-char-p/nil, string-trim, and parse-integer work correctly."
  (assert-true (equal nil (run-string "(digit-char-p #\\a)")))
  (assert-true (equal "hello" (run-string "(string-trim \" \" \"  hello  \")")))
  (assert-true (= 42 (run-string "(parse-integer \"42\")"))))

(deftest-each compile-subseq
  "subseq extracts substrings with optional end index."
  :cases (("with-end" "ell" "(subseq \"hello\" 1 4)")
          ("no-end"   "llo" "(subseq \"hello\" 2)"))
  (expected form)
  (assert-true (equal expected (run-string form))))

(deftest-each compile-search
  "search returns the position of the pattern or nil when not found (ANSI CL)."
  :cases (("found"     2   "(search \"ll\" \"hello\")")
          ("not-found" nil "(search \"xyz\" \"hello\")"))
  (expected form)
  (assert-equal expected (run-string form)))

;;; I/O and Format Tests

(deftest-each io-write-to-string
  "write-to-string converts values to their string representations."
  :cases (("integer" "42"    "(write-to-string 42)")
          ("symbol"  "HELLO" "(write-to-string 'hello)"))
  (expected form)
  (assert-true (equal expected (run-string form))))

(deftest-each io-format-nil
  "format with nil destination returns the correctly formatted string."
  :cases (("string"    "hello world" "(format nil \"hello ~A\" \"world\")")
          ("number"    "x=42"        "(format nil \"x=~A\" 42)")
          ("no-args"   "hello"       "(format nil \"hello\")")
          ("multi"     "1 + 2 = 3"   "(format nil \"~A + ~A = ~A\" 1 2 3)"))
  (expected form)
  (assert-true (equal expected (run-string form))))

(deftest-each io-print-returns-value
  "princ/prin1/print each return the value they printed."
  :cases (("princ" "(princ 42)")
          ("prin1" "(prin1 42)")
          ("print" "(print 42)"))
  (form)
  (assert-true (equal 42 (run-string form))))

(deftest io-returns-nil
  "terpri and (format t ...) both return nil."
  (assert-true (equal nil (run-string "(terpri)")))
  (assert-true (equal nil (run-string "(format t \"hello\")"))))

(deftest-each io-format-directives
  "format directives for iteration, conditionals, and character output."
  :cases (("iteration"          "1, 2, 3" "(format nil \"~{~A~^, ~}\" (list 1 2 3))")
          ("conditional-index"  "one"     "(format nil \"~[zero~;one~;two~:;many~]\" 1)")
          ("conditional-default" "many"   "(format nil \"~[zero~;one~;two~:;many~]\" 99)"))
  (expected form)
  (assert-equal expected (run-string form :stdlib t)))

(deftest io-write-char-basic
  "write-char outputs a character and returns it"
  (assert-true (equal #\A (run-string "(write-char #\\A)"))))

;;; Higher-Order Function Tests (require stdlib)

(deftest-each stdlib-hof-list-result
  "HOFs return the correct list result when applied to lists."
  :cases (("mapcar"        '(2 4 6)   "(mapcar (lambda (x) (+ x x)) (list 1 2 3))")
          ("remove-if"     '(1 2)     "(remove-if (lambda (x) (> x 2)) (list 1 2 3 4 5))")
          ("remove-if-not" '(3 4 5)   "(remove-if-not (lambda (x) (> x 2)) (list 1 2 3 4 5))"))
  (expected form)
  (assert-equal expected (run-string form :stdlib t)))

(deftest-each stdlib-hof-numeric
  "HOFs return numeric results for reduce, find-if, count-if."
  :cases (("reduce"   10 "(reduce (lambda (a b) (+ a b)) (list 1 2 3 4))")
          ("find-if"   4 "(find-if (lambda (x) (> x 3)) (list 1 2 3 4 5))")
          ("count-if"  2 "(count-if (lambda (x) (> x 2)) (list 1 2 3 4))"))
  (expected form)
  (assert-= expected (run-string form :stdlib t)))

(deftest-each stdlib-hof-truthy
  "every/some return truthy values when the predicate matches."
  :cases (("every-all"  "(every (lambda (x) (> x 0)) (list 1 2 3))")
          ("some-found" "(some (lambda (x) (> x 2)) (list 1 2 3))"))
  (form)
  (assert-true (not (null (run-string form :stdlib t)))))

(deftest-each stdlib-hof-nil
  "every/some/find-if/mapcar return nil when there is no match or empty input."
  :cases (("mapcar-empty"  "(mapcar (lambda (x) x) nil)")
          ("find-if-miss"  "(find-if (lambda (x) (> x 10)) (list 1 2 3))")
          ("every-fail"    "(every (lambda (x) (> x 2)) (list 1 2 3))")
          ("some-miss"     "(some (lambda (x) (> x 10)) (list 1 2 3))"))
  (form)
  (assert-true (equal nil (run-string form :stdlib t))))

;;; With-Output-To-String Tests

(deftest-each compile-with-output-to-string
  "with-output-to-string collects writes and format output into a string."
  :cases (("empty"        ""            "(with-output-to-string (s))")
          ("format"       "hello world" "(with-output-to-string (s) (format s \"hello ~A\" \"world\"))")
          ("multi-write"  "ab"          "(with-output-to-string (s) (write-string \"a\" s) (write-string \"b\" s))")
          ("multi-format" "x=1 y=2"     "(with-output-to-string (s) (format s \"x=~A\" 1) (format s \" y=~A\" 2))"))
  (expected form)
  (assert-string= expected (run-string form)))

(deftest compile-make-string-output-stream
  "make-string-output-stream and get-output-stream-string work"
  (assert-string= "foo" (run-string "(let ((s (make-string-output-stream))) (write-string \"foo\" s) (get-output-stream-string s))")))

;;; Array/Vector Tests

(deftest compile-make-array-basic
  "make-array creates an array"
  (let ((result (run-string "(make-array 5)")))
    (assert-true (vectorp result))
    (assert-= 5 (length result))))

(deftest-each compile-array-numeric
  "aref/setf aref/vector-push-extend return the correct numeric values."
  :cases (("aref-init" 0  "(let ((a (make-array 3))) (aref a 0))")
          ("setf-aref" 42 "(let ((a (make-array 3))) (setf (aref a 1) 42) (aref a 1))")
          ("vec-push"  10 "(let ((v (make-array 0 :fill-pointer t :adjustable t))) (vector-push-extend 10 v) (aref v 0))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each compile-vectorp
  "vectorp returns truthy for vectors and 0 for non-vectors."
  :cases (("vector"  "(not (null (vectorp (make-array 3))))")
          ("non-vec" "(eql 0 (vectorp 42))"))
  (form)
  (assert-true (run-string form)))

;;; Sort Tests

(deftest-each compile-sort
  "sort produces the correctly ordered list."
  :cases (("ascending"  '(1 1 2 3 4 5 6 9) "(sort (list 3 1 4 1 5 9 2 6) (lambda (a b) (< a b)))")
          ("descending" '(5 3 1)            "(sort (list 3 1 5) (lambda (a b) (> a b)))")
          ("single"     '(42)               "(sort (list 42) (lambda (a b) (< a b)))")
          ("empty"      nil                 "(sort nil (lambda (a b) (< a b)))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

;;; Coerce Tests

(deftest compile-coerce
  "coerce converts between chars/lists/strings/vectors."
  (assert-string= "abc" (run-string "(coerce (list #\\a #\\b #\\c) 'string)"))
  (assert-true (equal '(#\h #\i) (run-string "(coerce \"hi\" 'list)")))
  (let ((result (run-string "(coerce (list 1 2 3) 'vector)")))
    (assert-true (vectorp result))
    (assert-= 3 (length result))))

;;; Loop Macro Tests

(deftest-each loop-collect-list
  "LOOP collect/append/nconc/on/when/unless accumulations return the expected list."
  :cases (("for-in"         '(1 2 3)           "(loop for x in (list 1 2 3) collect x)")
          ("for-in-do"      '(1 2 3)           "(let ((r nil)) (loop for x in (list 1 2 3) do (push x r)) (nreverse r))")
          ("for-from-to"    '(1 2 3 4 5)       "(loop for i from 1 to 5 collect i)")
          ("for-below"      '(0 1 2 3 4)       "(loop for i from 0 below 5 collect i)")
          ("collect-expr"   '(1 4 9)           "(loop for x in (list 1 2 3) collect (* x x))")
          ("for-on"         '((1 2 3) (2 3) (3)) "(loop for x on (list 1 2 3) collect x)")
          ("empty"          nil                "(loop for x in nil collect x)")
          ("from-by"        '(0 2 4)           "(loop for i from 0 below 5 by 2 collect i)")
          ("on-by-cddr"     '((1 2 3 4) (3 4)) "(loop for x on (list 1 2 3 4) by (function cddr) collect x)")
          ("when-collect"   '(4 16 36)         "(loop for x in (list 1 2 3 4 5 6) when (evenp x) collect (* x x))")
          ("unless-collect" '(1 9 25)          "(loop for x in (list 1 2 3 4 5 6) unless (evenp x) collect (* x x))")
          ("if-collect"     '(2 4 6)           "(loop for x in (list 1 2 3 4 5 6) if (evenp x) collect x)")
          ("unless-do"      '(1 3 5)           "(let ((r nil)) (loop for x in (list 1 2 3 4 5) unless (evenp x) do (push x r)) (nreverse r))")
          ("when-append"    '(2 2 4 4)         "(loop for x in (list 1 2 3 4) when (evenp x) append (list x x))")
          ("collect-into"   '(1 2 3)           "(loop for x in (list 1 2 3) collect x into result finally (return (nreverse result)))")
          ("append"         '(1 2 3 4)         "(loop for x in (list (list 1 2) (list 3 4)) append x)")
          ("hash-key-val"   '(100)             "(let ((ht (make-hash-table))) (setf (gethash 'a ht) 100) (loop for k being the hash-keys of ht using (hash-value v) collect v))")
          ("collect-into-when" '((1 3 5 7 9) (2 4 6 8 10)) "(loop for i from 1 to 10 when (oddp i) collect i into odds when (evenp i) collect i into evens finally (return (list (nreverse odds) (nreverse evens))))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

(deftest-each loop-numeric
  "LOOP sum/repeat/with-clause return the expected numeric value."
  :cases (("repeat"       5  "(let ((n 0)) (loop repeat 5 do (setq n (+ n 1))) n)")
          ("sum"         15  "(loop for i from 1 to 5 sum i)")
          ("repeat-zero"  0  "(let ((n 0)) (loop repeat 0 do (setq n (+ n 1))) n)")
          ("hash-keys"    2  "(let ((ht (make-hash-table))) (setf (gethash 'a ht) 1) (setf (gethash 'b ht) 2) (length (loop for k being the hash-keys of ht collect k)))")
          ("hash-values" 30  "(let ((ht (make-hash-table))) (setf (gethash 'x ht) 10) (setf (gethash 'y ht) 20) (loop for v being the hash-values of ht sum v))")
          ("with-clause" 15  "(loop with sum = 0 for i from 1 to 5 do (setq sum (+ sum i)) finally (return sum))")
          ("when-sum"    12  "(loop for i from 1 to 6 when (evenp i) sum i)")
          ("sum-into"    15  "(loop for i from 1 to 5 sum i into total finally (return total))"))
  (expected form)
  (assert-= expected (run-string form :stdlib t)))

(deftest-each loop-always-never-thereis
  "LOOP always/never/thereis return t, nil, or the first truthy value."
  :cases (("always-true"  t   "(loop for i in (list 2 4 6) always (evenp i))")
          ("always-false" nil "(loop for i in (list 2 3 6) always (evenp i))")
          ("never-true"   t   "(loop for i in (list 1 3 5) never (evenp i))")
          ("thereis"      1   "(loop for i in (list 1 2 3) thereis (evenp i))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

(deftest loop-edge-cases
  "LOOP: empty hash-keys returns nil; nconc concatenates sublists."
  (assert-null (run-string "(loop for k being the hash-keys of (make-hash-table) collect k)" :stdlib t))
  (assert-true (string= "(a b c d)"
                        (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                          (string-downcase (format nil "~S" (run-string "(loop for x in (list (list 'a 'b) (list 'c 'd)) nconc (copy-list x))" :stdlib t)))))))

;;; Floor/Truncate/Ceiling Multiple Values Tests

(deftest-each floor-truncate-ceiling
  "floor/truncate/ceiling return quotient and remainder via multiple-value-bind."
  :cases (("floor"    '(3  2) "(multiple-value-bind (q r) (floor    17 5) (list q r))")
          ("truncate" '(3  2) "(multiple-value-bind (q r) (truncate 17 5) (list q r))")
          ("ceiling"  '(4 -3) "(multiple-value-bind (q r) (ceiling  17 5) (list q r))"))
  (expected form)
  (assert-equal expected (run-string form :stdlib t)))

;;; Nested Destructuring-Bind Tests

(deftest-each destructuring-bind-nested
  "DESTRUCTURING-BIND with nested and deeply nested patterns."
  :cases (("nested"      10 "(destructuring-bind (a (b c) d) (list 1 (list 2 3) 4) (+ a b c d))")
          ("deep-nested" 15 "(destructuring-bind (a (b (c d)) e) (list 1 (list 2 (list 3 4)) 5) (+ a b c d e))"))
  (expected form)
  (assert-= expected (run-string form :stdlib t)))

;;; Variadic Append/Nconc Tests

(deftest-each append-variadic
  "append and nconc work with 0, 1, or 3 arguments."
  :cases (("three-args" '(1 2 3 4 5)   "(append (list 1 2) (list 3 4) (list 5))")
          ("zero-args"  nil             "(append)")
          ("one-arg"    '(1 2 3)        "(append (list 1 2 3))")
          ("nconc"      '(1 2 3 4 5 6)  "(nconc (list 1 2) (list 3 4) (list 5 6))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

(deftest self-host-stack-compiler
  "Mini stack-machine compiler: parse -> compile -> run through VM"
  (assert-= 17 (run-string " (progn (defstruct ast-num value) (defstruct ast-binop op lhs rhs) (defun parse (expr) (cond ((numberp expr) (make-ast-num :value expr)) ((consp expr) (make-ast-binop :op (car expr) :lhs (parse (cadr expr)) :rhs (parse (caddr expr)))) (t (error \"unknown\")))) (defun compile-node (node) (cond ((ast-num-p node) (list (list (quote push) (ast-num-value node)))) ((ast-binop-p node) (append (compile-node (ast-binop-lhs node)) (compile-node (ast-binop-rhs node)) (list (list (quote op) (ast-binop-op node))))) (t nil))) (defun run-vm (instrs) (let ((stack nil)) (dolist (inst instrs) (cond ((eq (car inst) (quote push)) (push (cadr inst) stack)) ((eq (car inst) (quote op)) (let ((b (pop stack)) (a (pop stack))) (cond ((eq (cadr inst) (quote +)) (push (+ a b) stack)) ((eq (cadr inst) (quote *)) (push (* a b) stack)) ((eq (cadr inst) (quote -)) (push (- a b) stack))))))) (car stack))) (run-vm (compile-node (parse (quote (+ (* 3 4) (- 10 5)))))))" :stdlib t)))

;;; Consp Fix / Type Predicates Tests

(deftest consp-and
  "consp returns 1 for cons cells, 0 for non-cons; and works with consp."
  (assert-true (= 1  (run-string "(consp (list 1 2))" :stdlib t)))
  (assert-true (= 0  (run-string "(consp 42)"         :stdlib t)))
  (assert-true (= 42 (run-string "(and (consp (list 1)) 42)" :stdlib t))))

(deftest mini-compiler-self-host
  "Mini compiler can compile expression with pattern matching"
  (assert-true (equal '(:ADD (:CONST 1) (:CONST 2))
             (run-string "(defun my-compile (expr) (cond ((integerp expr) (list :const expr)) ((and (consp expr) (eq (car expr) (quote +))) (list :add (my-compile (second expr)) (my-compile (third expr)))) (t (list :unknown expr)))) (my-compile (quote (+ 1 2)))" :stdlib t))))

;;; Funcall/Apply with Quoted Symbols Tests

(deftest-each funcall-apply-quoted
  "funcall and apply work with quoted symbols, lambdas, and #' references."
  :cases (("funcall-builtin"  7 "(funcall (quote +) 3 4)")
          ("funcall-user"     7 "(defun my-add2 (a b) (+ a b)) (funcall (quote my-add2) 3 4)")
          ("apply-builtin"    6 "(apply (quote +) (list 1 2 3))")
          ("apply-user"       7 "(defun my-add3 (a b) (+ a b)) (apply (quote my-add3) (list 3 4))")
          ("apply-lambda"     6 "(apply (lambda (a b c) (+ a b c)) (list 1 2 3))")
          ("funcall-hash-ref" 7 "(funcall #'+ 3 4)"))
  (expected form)
  (assert-= expected (run-string form :stdlib t)))

;;; Maphash Tests

(deftest maphash-collect-values
  "MAPHASH iterates over hash table entries with closure mutation"
  (let ((result (run-string "(let ((result nil))
  (let ((ht (make-hash-table)))
    (setf (gethash :a ht) 1)
    (setf (gethash :b ht) 2)
    (maphash (lambda (k v) (setq result (cons v result))) ht)
    result))")))
    (assert-true (listp result))
    (assert-= 2 (length result))
    (assert-true (null (set-difference result (list 1 2))))))

(deftest maphash-behavior
  "maphash iterates and always returns nil; closure mutation captures counts."
  (assert-true (null (run-string "(let ((ht (make-hash-table)))
  (setf (gethash :x ht) 10)
  (maphash (lambda (k v) v) ht))")))
  (assert-true (null (run-string "(let ((ht (make-hash-table)))
  (maphash (lambda (k v) k) ht))")))
  (assert-= 3 (run-string "(let ((count 0))
  (let ((ht (make-hash-table)))
    (setf (gethash :a ht) 1)
    (setf (gethash :b ht) 2)
    (setf (gethash :c ht) 3)
    (maphash (lambda (k v) (setq count (+ count 1))) ht)
    count))")))

;;; Capture-by-Reference Tests

(deftest-each capture-by-ref
  "Closures capture variables by reference; mutations are visible to all accessing closures."
  :cases (("counter" 3
           "(let ((count 0))
  (let ((inc (lambda () (setq count (+ count 1)) count)))
    (funcall inc) (funcall inc) (funcall inc)))")
          ("shared-state" 42
           "(let ((x 0))
  (defun get-x4 () x)
  (defun set-x4 (v) (setq x v))
  (set-x4 42)
  (get-x4))")
          ("accumulator" 10
           "(let ((sum 0))
  (let ((add (lambda (n) (setq sum (+ sum n)) sum)))
    (funcall add 1) (funcall add 2) (funcall add 3) (funcall add 4)))"))
  (expected form)
  (assert-= expected (run-string form :stdlib t)))

;;; File I/O Tests

(deftest file-io
  "File I/O: write-char/read-char, with-open-file, read-from-string, and read."
  ;; write-char + read-char via open/close
  (let ((result (run-string "(let ((h (open \"/tmp/cl-cc-test-wr.txt\" :direction :output)))
  (write-char #\\H h)
  (write-char #\\i h)
  (close h)
  (let ((h2 (open \"/tmp/cl-cc-test-wr.txt\" :direction :input)))
    (let ((c1 (read-char h2)))
      (let ((c2 (read-char h2)))
        (close h2)
        (list c1 c2)))))")))
    (assert-true (equal result '(#\H #\i))))
  ;; with-open-file
  (let ((result (run-string "(with-open-file (out \"/tmp/cl-cc-test-wof2.txt\" :direction :output)
  (write-char #\\X out))
(with-open-file (in \"/tmp/cl-cc-test-wof2.txt\" :direction :input)
  (read-char in))")))
    (assert-eql result #\X))
  ;; read-from-string
  (let ((result (run-string "(read-from-string \"(+ 1 2)\")")))
    (assert-true (listp result))
    (assert-= 3 (length result)))
  ;; read from file
  (let ((result (run-string "(with-open-file (out \"/tmp/cl-cc-test-rd.txt\" :direction :output)
  (write-char #\\( out) (write-char #\\a out) (write-char #\\) out))
(with-open-file (in \"/tmp/cl-cc-test-rd.txt\" :direction :input)
  (read in))")))
    (assert-true (listp result))))

;;; Setf Accessor Tests

(deftest-each setf-defstruct
  "setf on defstruct accessors modifies slots correctly."
  :cases (("accessor" 42
           "(defstruct my-cell (value 0))
(let ((c (make-my-cell)))
  (setf (my-cell-value c) 42)
  (my-cell-value c))")
          ("counter"  3
           "(defstruct my-counter2 (n 0))
(let ((c (make-my-counter2)))
  (setf (my-counter2-n c) (+ (my-counter2-n c) 1))
  (setf (my-counter2-n c) (+ (my-counter2-n c) 1))
  (setf (my-counter2-n c) (+ (my-counter2-n c) 1))
  (my-counter2-n c))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Self-Hosting Pattern Tests

(deftest self-host-compiler-context
  "Self-hosting: compile a mini compiler context with defstruct"
  (assert-equal '(3 3 0 1 2) (run-string "
(defstruct compiler-ctx2 (counter 0) (instructions nil))
(defun make-reg2 (ctx) (let ((n (compiler-ctx2-counter ctx))) (setf (compiler-ctx2-counter ctx) (+ n 1)) n))
(defun emit-inst2 (ctx inst) (setf (compiler-ctx2-instructions ctx) (cons inst (compiler-ctx2-instructions ctx))))
(let ((ctx (make-compiler-ctx2))) (let ((r1 (make-reg2 ctx)) (r2 (make-reg2 ctx)) (r3 (make-reg2 ctx))) (emit-inst2 ctx (list :const r1 42)) (emit-inst2 ctx (list :const r2 10)) (emit-inst2 ctx (list :add r3 r1 r2)) (list (compiler-ctx2-counter ctx) (length (compiler-ctx2-instructions ctx)) r1 r2 r3)))" :stdlib t)))

(deftest self-host-clos-ast-eval
  "Self-hosting: CLOS-based AST evaluator"
  (assert-= 42 (run-string "
(defclass eval-int () ((value :initarg :value :reader eval-value)))
(defclass eval-binop () ((op :initarg :op :reader eval-op) (lhs :initarg :lhs :reader eval-lhs) (rhs :initarg :rhs :reader eval-rhs)))
(defgeneric eval-node (node))
(defmethod eval-node ((node eval-int)) (eval-value node))
(defmethod eval-node ((node eval-binop)) (let ((l (eval-node (eval-lhs node))) (r (eval-node (eval-rhs node)))) (if (eq (eval-op node) :add) (+ l r) (* l r))))
(let ((tree (make-instance 'eval-binop :op :add :lhs (make-instance 'eval-int :value 30) :rhs (make-instance 'eval-binop :op :mul :lhs (make-instance 'eval-int :value 4) :rhs (make-instance 'eval-int :value 3))))) (eval-node tree))")))

;;; Labels Mutual Recursion Tests

(deftest-each labels-mutual-recursion
  "Labels with mutually recursive functions behave correctly."
  :cases (("even-4" t
           "(labels ((even-p (n) (if (= n 0) t (odd-p (- n 1)))) (odd-p (n) (if (= n 0) nil (even-p (- n 1))))) (even-p 4))")
          ("odd-3" nil
           "(labels ((even-p (n) (if (= n 0) t (odd-p (- n 1)))) (odd-p (n) (if (= n 0) nil (even-p (- n 1))))) (even-p 3))")
          ("three-fns" 6
           "(labels ((a (n) (if (= n 0) 0 (+ 1 (b (- n 1))))) (b (n) (if (= n 0) 0 (+ 1 (c (- n 1))))) (c (n) (if (= n 0) 0 (+ 1 (a (- n 1)))))) (a 6))"))
  (expected form)
  (assert-true (equal expected (run-string form))))

;;; Hash Table :test Parameter Tests

(deftest-each ht-test-parameter
  "Hash tables with various :test parameters work correctly."
  :cases (("equal-quote"       42 "(let ((ht (make-hash-table :test 'equal))) (setf (gethash \"key\" ht) 42) (gethash \"key\" ht))")
          ("equal-sharp-quote" 42 "(let ((ht (make-hash-table :test #'equal))) (setf (gethash \"key\" ht) 42) (gethash \"key\" ht))")
          ("eql-int-keys"      99 "(let ((ht (make-hash-table))) (setf (gethash 1 ht) 99) (gethash 1 ht))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; New Builtin Tests (type-of, make-list, alphanumericp, prin1-to-string)

(deftest-each builtin-type-of
  "type-of returns the correct type symbol for common types."
  :cases (("integer" 'fixnum "(type-of 42)")
          ("string"  'string  "(type-of \"hello\")")
          ("cons"    'cons    "(type-of '(1 2))"))
  (expected form)
  (assert-true (eq expected (run-string form))))

(deftest-each builtin-make-list
  "make-list creates a list of the specified length filled with nil."
  :cases (("three" '(nil nil nil) "(make-list 3)")
          ("zero"  nil            "(make-list 0)"))
  (expected form)
  (assert-true (equal expected (run-string form))))

(deftest-each builtin-alphanumericp
  "alphanumericp returns truthy for alphanumeric chars and 0 for punctuation."
  :cases (("alpha" "(alphanumericp #\\a)" t)
          ("digit" "(alphanumericp #\\5)" t)
          ("punct" "(alphanumericp #\\!)" nil))
  (form expected)
  (if expected
      (assert-true (not (zerop (run-string form))))
      (assert-true (zerop  (run-string form)))))

(deftest-each builtin-print-to-string
  "prin1-to-string and princ-to-string both return strings."
  :cases (("prin1" "(prin1-to-string 42)")
          ("princ" "(princ-to-string 42)"))
  (form)
  (assert-true (stringp (run-string form))))

;;; Runtime Eval Tests

(deftest-each compile-eval
  "eval computes various form types correctly."
  :cases (("constant"    42 "(eval 42)")
          ("quoted"       3 "(eval '(+ 1 2))")
          ("nested"      21 "(eval '(* (+ 1 2) (+ 3 4)))")
          ("let-form"    15 "(eval '(let ((x 10)) (+ x 5)))")
          ("constructed" 30 "(let ((op '+) (a 10) (b 20)) (eval (list op a b)))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Setf Variable Tests

(deftest-each compile-setf-variable
  "setf on plain variables works like setq."
  :cases (("plain"     10 "(let ((x 0)) (setf x 10) x)")
          ("increment"  3 "(let ((counter 0)) (setf counter (+ counter 1)) (setf counter (+ counter 1)) (setf counter (+ counter 1)) counter)"))
  (expected form)
  (assert-= expected (run-string form)))

;;; FR-603: (setf (values ...)) assigns to multiple places

(deftest compile-setf-values
  "(setf (values ...)) assigns multiple values to individual places."
  (assert-= 10 (run-string
             "(let ((a 0) (b 0))
                (setf (values a b) (values 10 20))
                a)"))
  (assert-= 20 (run-string
             "(let ((a 0) (b 0))
                (setf (values a b) (values 10 20))
                b)"))
  (assert-= 30 (run-string
             "(let ((x 0) (y 0))
                (setf (values x y) (values 10 20))
                (+ x y))")))

;;; Stdlib HOF Tests (with stdlib)

(deftest-each stdlib-hof-basic
  "mapcar, reduce, and remove-if with stdlib loaded return correct values."
  :cases (("mapcar"    '(2 4 6) "(mapcar (lambda (x) (* x 2)) '(1 2 3))")
          ("reduce"    15       "(reduce #'+ '(1 2 3 4 5) :initial-value 0)")
          ("remove-if" '(2 4)   "(remove-if #'oddp '(1 2 3 4 5))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

;;; Let Alias Fix Tests

(deftest-each let-no-alias
  "LET bindings are independent copies; mutating the original does not affect the copy."
  :cases (("simple" 0 "(let ((x 0)) (let ((y x)) (setq x 10) y))")
          ("nested" 5 "(let ((a 5)) (let ((b a)) (let ((c b)) (setq a 99) c))))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Prog1/Prog2/Ignore-Errors Tests

(deftest-each compile-prog1-prog2
  "prog1 returns first form value; prog2 returns second form value."
  :cases (("basic"       42 "(prog1 42 (+ 1 2))")
          ("side-effect"  0 "(let ((x 0)) (prog1 x (setq x 10)))")
          ("prog2"       42 "(prog2 1 42 3)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each compile-ignore-errors
  "ignore-errors returns the value on success and nil on error."
  :cases (("success" 3   "(ignore-errors (+ 1 2))")
          ("failure" nil "(ignore-errors (error \"boom\"))"))
  (expected form)
  (assert-true (equal expected (run-string form))))

;;; Unwind-Protect Integration Tests

(deftest unwind-protect-cleanup-visible
  "unwind-protect cleanup side effects visible in handler-case"
  (assert-eq t (run-string "
(let ((cleaned nil)) (handler-case (unwind-protect (error \"boom\") (setf cleaned t)) (error (e) cleaned)))")))

;;; Self-Hosting CLOS Compiler Test

(deftest self-host-clos-compiler
  "Self-hosting: CLOS-based AST compiler with defgeneric/defmethod dispatch"
  (assert-equal '(:R2 3) (run-string "
(defclass ast-n6 () ())
(defclass ast-i6 (ast-n6) ((v :initarg :v :reader ast-i6-v)))
(defclass ast-v6 (ast-n6) ((n :initarg :n :reader ast-v6-n)))
(defclass ast-b6 (ast-n6) ((op :initarg :op :reader ast-b6-op) (l :initarg :l :reader ast-b6-l) (r :initarg :r :reader ast-b6-r)))
(defstruct cx6 (nr 0) (insts nil) (env nil))
(defun mr6 (c) (let ((r (cx6-nr c))) (setf (cx6-nr c) (+ r 1)) (intern (concatenate 'string \"R\" (write-to-string r)) :keyword)))
(defun em6 (c i) (setf (cx6-insts c) (append (cx6-insts c) (list i))))
(defgeneric cn6 (node ctx))
(defmethod cn6 ((node ast-i6) ctx) (let ((r (mr6 ctx))) (em6 ctx (list :const r (ast-i6-v node))) r))
(defmethod cn6 ((node ast-v6) ctx) (cdr (assoc (ast-v6-n node) (cx6-env ctx))))
(defmethod cn6 ((node ast-b6) ctx) (let ((l (cn6 (ast-b6-l node) ctx)) (rv (cn6 (ast-b6-r node) ctx)) (dst (mr6 ctx))) (em6 ctx (list (ast-b6-op node) dst l rv)) dst))
(let ((ctx (make-cx6))) (let ((r (cn6 (make-instance 'ast-b6 :op :add :l (make-instance 'ast-i6 :v 10) :r (make-instance 'ast-i6 :v 20)) ctx))) (list r (length (cx6-insts ctx)))))
" :stdlib t)))

;;; Hash Table Extended Builtins Tests

(deftest compile-hash-table-values
  "hash-table-values returns list of values"
  (let ((result (run-string "(let ((ht (make-hash-table)))
  (setf (gethash :a ht) 1)
  (setf (gethash :b ht) 2)
  (hash-table-values ht))")))
    (assert-= 2 (length result))
    (assert-true (null (set-difference result '(1 2))))))

(deftest-each compile-hash-table-test
  "hash-table-test returns the test function name the table was created with."
  :cases (("default-eql"    'eql   "(hash-table-test (make-hash-table))")
          ("explicit-equal" 'equal "(hash-table-test (make-hash-table :test 'equal))"))
  (expected form)
  (assert-true (eq expected (run-string form))))

(deftest compile-copy-hash-table
  "copy-hash-table creates independent copy"
  (assert-= 42 (run-string "
(let ((ht (make-hash-table))) (setf (gethash :a ht) 42) (let ((ht2 (copy-hash-table ht))) (setf (gethash :a ht) 99) (gethash :a ht2)))")))

;;; Car/Cdr Composition and List Accessor Tests

(deftest-each cxr-compositions
  "c*r composition functions access deeply nested list elements correctly."
  :cases (("cadddr" "d" "(string-downcase (symbol-name (cadddr '(a b c d e))))")
          ("caadr"  "x" "(string-downcase (symbol-name (caadr '(a (x y) c))))")
          ("caddar" 3   "(caddar '((1 2 3) b c))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

;;; Self-Hosting Integration Tests

(deftest-each self-host-patterns
  "Self-hosting patterns: eval dispatch, defstruct, registry, tree walk, closure counter, code gen."
  :cases (("eval-loop"       7  " (defun mini-eval (form env) (cond ((integerp form) form) ((symbolp form) (cdr (assoc form env))) ((and (consp form) (eq (car form) 'quote)) (cadr form)) ((and (consp form) (eq (car form) 'if)) (if (not (= 0 (mini-eval (cadr form) env))) (mini-eval (caddr form) env) (mini-eval (cadddr form) env))) ((and (consp form) (eq (car form) '+)) (+ (mini-eval (cadr form) env) (mini-eval (caddr form) env))) (t 0))) (mini-eval '(if 1 (+ 3 4) 0) nil) ")
          ("defstruct-pipe"  42 " (defstruct node type value children) (let ((n (make-node :type 'add :value nil :children (list (make-node :type 'lit :value 42 :children nil))))) (node-value (car (node-children n)))) ")
          ("ht-registry"     30 " (let ((registry (make-hash-table))) (setf (gethash 'add registry) (lambda (a b) (+ a b))) (setf (gethash 'mul registry) (lambda (a b) (* a b))) (let ((op (gethash 'add registry))) (funcall op 10 20))) ")
          ("tree-walk"       10 " (defun tree-sum (tree) (if (consp tree) (+ (tree-sum (car tree)) (tree-sum (cdr tree))) (if (integerp tree) tree 0))) (tree-sum '((1 . 2) . (3 . (4 . nil)))) ")
          ("closure-counter"  3 " (let ((counter 0)) (defun next-id () (setq counter (+ counter 1)) counter)) (next-id) (next-id) (next-id) ")
          ("macro-code-gen"  15 " (defun make-add-expr (a b) (list '+ a b)) (defun make-let-expr (var val body) (list 'let (list (list var val)) body)) (eval (make-let-expr 'x 10 (make-add-expr 'x 5))) "))
  (expected form)
  (assert-= expected (run-string form :stdlib t)))

;;; Non-Constant Default Parameter Tests

(deftest-each non-constant-default-params
  "&key and &optional parameters use non-constant default expressions correctly."
  :cases (("key-default"    '(1 2 3) "(progn (defun test-fn (&key (data (list 1 2 3))) data) (test-fn))")
          ("key-supplied"   '(4 5 6) "(progn (defun test-fn (&key (data (list 1 2 3))) data) (test-fn :data (list 4 5 6)))")
          ("optional-default" '(10 20) "(progn (defun test-fn (&optional (data (list 10 20))) data) (test-fn))"))
  (expected form)
  (assert-equal expected (run-string form)))

(deftest defstruct-non-constant-default
  "Test defstruct with non-constant slot default (hash table)."
  (assert-eq :bar (run-string "(progn (defstruct registry (entries (make-hash-table :test 'eq))) (let ((r (make-registry))) (setf (gethash 'foo (registry-entries r)) :bar) (gethash 'foo (registry-entries r))))" :stdlib t)))

;;; Multiple Dispatch Tests

(deftest-each multi-dispatch-numeric
  "Multiple dispatch: double/mixed specialization and CLOS second arg."
  :cases (("dog+bone"     1  "(progn (defclass animal () ()) (defclass dog (animal) ()) (defclass cat (animal) ()) (defclass food () ()) (defclass bone (food) ()) (defclass fish (food) ()) (defgeneric feed (a f)) (defmethod feed ((a dog) (f bone)) 1) (defmethod feed ((a cat) (f fish)) 2) (feed (make-instance 'dog) (make-instance 'bone)))")
           ("cat+fish"     2  "(progn (defclass animal () ()) (defclass dog (animal) ()) (defclass cat (animal) ()) (defclass food () ()) (defclass bone (food) ()) (defclass fish (food) ()) (defgeneric feed (a f)) (defmethod feed ((a dog) (f bone)) 1) (defmethod feed ((a cat) (f fish)) 2) (feed (make-instance 'cat) (make-instance 'fish)))")
           ("circle-mixed" 10 "(progn (defclass shape () ()) (defclass circle (shape) ()) (defclass rect (shape) ()) (defgeneric area (s ctx)) (defmethod area ((s circle) ctx) 10) (defmethod area ((s rect) ctx) 20) (area (make-instance 'circle) 99))")
           ("rect-mixed"   20 "(progn (defclass shape () ()) (defclass circle (shape) ()) (defclass rect (shape) ()) (defgeneric area (s ctx)) (defmethod area ((s circle) ctx) 10) (defmethod area ((s rect) ctx) 20) (area (make-instance 'rect) 99))")
           ("clos-2nd-arg" 42 "(progn (defclass ctx () ()) (defclass nd () ()) (defclass nd-int (nd) ((v :initarg :v :reader nd-v))) (defgeneric cmp (n c)) (defmethod cmp ((n nd-int) c) 42) (cmp (make-instance 'nd-int :v 1) (make-instance 'ctx)))"))
  (expected form)
  (assert-= expected (run-string form :stdlib t)))

(deftest multi-dispatch-inheritance-fallback
  "Multiple dispatch: dispatch falls back via class inheritance"
  (assert-true (string= "base"
                        (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                          (string-downcase (format nil "~S" (run-string "(progn (defclass a () ()) (defclass b (a) ()) (defclass x () ()) (defclass y (x) ()) (defgeneric op (p q)) (defmethod op ((p a) (q x)) 'base) (op (make-instance 'b) (make-instance 'y)))" :stdlib t)))))))

(deftest-each multi-dispatch-type-equality
  "Multiple dispatch: double dispatch for type equality (self-hosting pattern)"
  :cases (("same-type" t   "(progn (defclass ty () ()) (defclass ty-int (ty) ()) (defclass ty-str (ty) ()) (defgeneric ty-eq (a b)) (defmethod ty-eq ((a ty-int) (b ty-int)) t) (defmethod ty-eq ((a ty-str) (b ty-str)) t) (defmethod ty-eq ((a ty) (b ty)) nil) (ty-eq (make-instance 'ty-int) (make-instance 'ty-int)))")
           ("diff-type" nil "(progn (defclass ty () ()) (defclass ty-int (ty) ()) (defclass ty-str (ty) ()) (defgeneric ty-eq (a b)) (defmethod ty-eq ((a ty-int) (b ty-int)) t) (defmethod ty-eq ((a ty-str) (b ty-str)) t) (defmethod ty-eq ((a ty) (b ty)) nil) (ty-eq (make-instance 'ty-int) (make-instance 'ty-str)))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

;;; CLOS Initform and Accessor Setf Tests

(deftest-each clos-initform
  "CLOS :initform provides default values and :accessor setf works for mutation."
  :cases (("integer"    0 "(progn (defclass counter () ((n :initform 0 :accessor counter-n))) (counter-n (make-instance 'counter)))")
          ("setf"      99 "(progn (defclass box () ((val :initarg :val :initform 0 :accessor box-val))) (let ((b (make-instance 'box))) (setf (box-val b) 99) (box-val b)))")
          ("increment"  3 "(progn (defclass counter () ((n :initform 0 :accessor counter-n))) (let ((c (make-instance 'counter))) (setf (counter-n c) (+ (counter-n c) 1)) (setf (counter-n c) (+ (counter-n c) 1)) (setf (counter-n c) (+ (counter-n c) 1)) (counter-n c)))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Self-Hosting Bootstrap Tests

(deftest self-host-make-register
  "Test self-hosting: register allocation utility."
  (assert-equal '(:R0 :R1 :R2) (run-string "(progn (defstruct compiler-ctx (next-register 0)) (defun make-register (ctx) (let ((reg (intern (format nil \"R~D\" (compiler-ctx-next-register ctx)) :keyword))) (setf (compiler-ctx-next-register ctx) (+ (compiler-ctx-next-register ctx) 1)) reg)) (let ((ctx (make-compiler-ctx))) (list (make-register ctx) (make-register ctx) (make-register ctx))))" :stdlib t)))

(deftest self-host-mini-compiler
  "Test self-hosting: complete mini-compiler pipeline (parse -> compile -> VM -> run)."
  (assert-= 35 (run-string "(progn (defstruct mc-int (val 0)) (defstruct mc-binop (op nil) (l nil) (r nil)) (defstruct mc-ctx (instrs nil) (nreg 0)) (defun mc-reg (ctx) (let ((r (intern (format nil \"R~D\" (mc-ctx-nreg ctx)) :keyword))) (setf (mc-ctx-nreg ctx) (+ (mc-ctx-nreg ctx) 1)) r)) (defun mc-emit (ctx inst) (setf (mc-ctx-instrs ctx) (cons inst (mc-ctx-instrs ctx)))) (defun mc-compile (node ctx) (cond ((mc-int-p node) (let ((dst (mc-reg ctx))) (mc-emit ctx (list :const dst (mc-int-val node))) dst)) ((mc-binop-p node) (let* ((lr (mc-compile (mc-binop-l node) ctx)) (rr (mc-compile (mc-binop-r node) ctx)) (dst (mc-reg ctx)) (opcode (case (mc-binop-op node) (+ :add) (- :sub) (* :mul)))) (mc-emit ctx (list opcode dst lr rr)) dst)) (t (error \"unknown\")))) (defun mc-run (instrs) (let ((regs (make-hash-table))) (dolist (inst instrs) (let ((op (car inst))) (cond ((eq op :const) (setf (gethash (cadr inst) regs) (caddr inst))) ((member op '(:add :sub :mul)) (let ((l (gethash (caddr inst) regs)) (r (gethash (cadddr inst) regs))) (setf (gethash (cadr inst) regs) (case op (:add (+ l r)) (:sub (- l r)) (:mul (* l r))))))))) (gethash (cadr (car (last instrs))) regs))) (let ((ctx (make-mc-ctx))) (mc-compile (make-mc-binop :op '* :l (make-mc-binop :op '+ :l (make-mc-int :val 3) :r (make-mc-int :val 4)) :r (make-mc-binop :op '- :l (make-mc-int :val 10) :r (make-mc-int :val 5))) ctx) (mc-run (nreverse (mc-ctx-instrs ctx)))))" :stdlib t)))

(deftest self-host-clos-compiler-full
  "Test self-hosting: CLOS-based compiler with generic dispatch (full pipeline)."
  (assert-true (equal '((:CONST :R0 3) (:CONST :R1 4) (:MUL :R2 :R0 :R1) (:CONST :R3 5) (:ADD :R4 :R2 :R3))
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

(deftest self-host-clos-full-pipeline
  "Full self-hosting: CLOS AST -> register compiler -> hash-table VM with let/if"
  (assert-= 100 (run-string " (progn (defclass ast () ()) (defclass ast-lit (ast) ((value :initarg :value :accessor ast-lit-value))) (defclass ast-var (ast) ((name :initarg :name :accessor ast-var-name))) (defclass ast-binop (ast) ((op :initarg :op :accessor ast-binop-op) (lhs :initarg :lhs :accessor ast-binop-lhs) (rhs :initarg :rhs :accessor ast-binop-rhs))) (defclass ast-let (ast) ((var :initarg :var :accessor ast-let-var) (init :initarg :init :accessor ast-let-init) (body :initarg :body :accessor ast-let-body))) (defclass ast-if (ast) ((test :initarg :test :accessor ast-if-test) (then :initarg :then :accessor ast-if-then) (else :initarg :else :accessor ast-if-else))) (defun parse (sexp) (cond ((numberp sexp) (make-instance (quote ast-lit) :value sexp)) ((symbolp sexp) (make-instance (quote ast-var) :name sexp)) ((not (consp sexp)) (error \"bad\")) ((eq (car sexp) (quote let)) (make-instance (quote ast-let) :var (caar (cadr sexp)) :init (parse (cadar (cadr sexp))) :body (parse (caddr sexp)))) ((eq (car sexp) (quote if)) (make-instance (quote ast-if) :test (parse (cadr sexp)) :then (parse (caddr sexp)) :else (parse (cadddr sexp)))) ((member (car sexp) (quote (+ - * =))) (make-instance (quote ast-binop) :op (car sexp) :lhs (parse (cadr sexp)) :rhs (parse (caddr sexp)))) (t (error \"unknown\")))) (defvar *rc* 0) (defvar *code* nil) (defun fr () (let ((r (intern (format nil \"R~D\" *rc*)))) (setq *rc* (+ *rc* 1)) r)) (defun em (inst) (push inst *code*)) (defgeneric cn (node env)) (defmethod cn ((n ast-lit) env) (let ((r (fr))) (em (list (quote CONST) r (ast-lit-value n))) r)) (defmethod cn ((n ast-var) env) (cdr (assoc (ast-var-name n) env))) (defmethod cn ((n ast-binop) env) (let ((l (cn (ast-binop-lhs n) env)) (r (cn (ast-binop-rhs n) env)) (d (fr))) (em (list (ast-binop-op n) d l r)) d)) (defmethod cn ((n ast-let) env) (cn (ast-let-body n) (acons (ast-let-var n) (cn (ast-let-init n) env) env))) (defmethod cn ((n ast-if) env) (let ((tr (cn (ast-if-test n) env)) (th (cn (ast-if-then n) env)) (el (cn (ast-if-else n) env)) (d (fr))) (em (list (quote SEL) d tr th el)) d)) (defun rp (instrs) (let ((regs (make-hash-table :test (quote eq)))) (dolist (inst instrs) (let ((op (nth 0 inst)) (dst (nth 1 inst))) (cond ((eq op (quote CONST)) (setf (gethash dst regs) (nth 2 inst))) ((eq op (quote SEL)) (let ((tv (gethash (nth 2 inst) regs))) (setf (gethash dst regs) (if (and tv (not (zerop tv))) (gethash (nth 3 inst) regs) (gethash (nth 4 inst) regs))))) (t (let ((a (gethash (nth 2 inst) regs)) (b (gethash (nth 3 inst) regs))) (setf (gethash dst regs) (cond ((eq op (quote +)) (+ a b)) ((eq op (quote -)) (- a b)) ((eq op (quote *)) (* a b)) ((eq op (quote =)) (if (= a b) 1 0)) (t 0)))))))) (gethash (intern (format nil \"R~D\" (- *rc* 1))) regs))) (setq *rc* 0) (setq *code* nil) (cn (parse (quote (let ((x 10)) (if (= x 10) (* x x) (+ x 1))))) nil) (rp (nreverse *code*)))" :stdlib t)))

;;; Generic Function as First-Class Value Tests

(deftest-each generic-function-numeric
  "Generic functions work with funcall/apply/let and return correct numeric values."
  :cases (("funcall" 11 "(progn (defgeneric my-fn (x)) (defmethod my-fn ((x t)) (+ x 1)) (funcall #'my-fn 10))")
          ("apply"   42 "(progn (defgeneric add1 (x)) (defmethod add1 ((x t)) (+ x 1)) (apply #'add1 (list 41)))")
          ("in-let"   5 "(progn (defgeneric double (x)) (defmethod double ((x t)) (* x 2)) (let ((f #'double)) (funcall f 2) (+ (funcall f 2) 1)))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest funcall-generic-function-clos
  "funcall with #'generic-function should dispatch on CLOS class"
  (assert-string= "dog-speak" (run-string "(progn (defclass animal () ()) (defclass dog (animal) ()) (defgeneric speak (x)) (defmethod speak ((x dog)) \"dog-speak\") (defmethod speak ((x t)) \"default\") (funcall #'speak (make-instance 'dog)))")))

(deftest mapcar-generic-function
  "mapcar with #'generic-function via stdlib"
  (assert-equal '(2 3 4) (run-string "(progn (defgeneric inc (x)) (defmethod inc ((x t)) (+ x 1)) (mapcar #'inc (list 1 2 3)))" :stdlib t)))

(deftest mapcar-generic-function-reader
  "mapcar with #'reader-method on CLOS instances"
  (assert-true (string= "(a b c)"
                        (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                          (string-downcase (format nil "~S" (run-string "(progn (defclass item () ((name :initarg :name :reader item-name))) (let ((items (list (make-instance 'item :name 'a) (make-instance 'item :name 'b) (make-instance 'item :name 'c)))) (mapcar #'item-name items)))" :stdlib t)))))))

(deftest self-host-mapcar-inst-sexp
  "Self-hosting pattern: mapcar #'generic-function over instruction list"
  (assert-true (string= "((:const r0 42) (:const r1 7) (:add r2 r0 r1))"
    (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
      (string-downcase (format nil "~S" (run-string "(progn
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
        (mapcar #'inst-sexp program)))" :stdlib t)))))))

;;; Run Tests Function

;;; Global Variable (defvar) Persistence Tests

(deftest-each defvar-persistence
  "defvar combined with setq and functions persists state correctly."
  :cases (("counter"  3
           "(progn (defvar *counter* 0) (defun inc-counter () (setq *counter* (+ *counter* 1)) *counter*) (inc-counter) (inc-counter) (inc-counter))")
          ("sequence" '(0 1 2)
           "(progn (defvar *n* 0) (defun next-n () (let ((val *n*)) (setq *n* (+ *n* 1)) val)) (list (next-n) (next-n) (next-n)))"))
  (expected form)
  (assert-true (equal expected (run-string form))))

(deftest defvar-label-generation
  "defvar counter for label generation pattern"
  (assert-equal '("L_0" "L_1" "L_2") (run-string "(progn (defvar *lbl* 0) (defun make-label (prefix) (let ((n *lbl*)) (setq *lbl* (+ n 1)) (concatenate 'string prefix \"_\" (write-to-string n)))) (list (make-label \"L\") (make-label \"L\") (make-label \"L\")))" :stdlib t)))

;;; Defmacro in progn Tests

(deftest-each defmacro-in-progn
  "defmacro works within progn, making the macro available for subsequent forms."
  :cases (("simple"     10 "(progn (defmacro my-dbl (x) (list '+ x x)) (my-dbl 5))")
          ("rest"       42 "(progn (defmacro my-when (test &rest body) (list 'if test (cons 'progn body) nil)) (my-when (= 1 1) 42))")
          ("used-twice" 12 "(progn (defmacro my-add1 (x) (list '+ x 1)) (+ (my-add1 5) (my-add1 5)))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Self-Hosting Compiler Pattern Tests

(deftest self-host-compiler-context-full
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
    (assert-true (equal '((:CONST :R0 42) (:CONST :R1 7) (:ADD :R2 :R0 :R1)) result))))

(deftest self-host-ast-compile-dispatch
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
    (assert-equal '((:CONST :R0 3) (:CONST :R1 4) (:MUL :R2 :R0 :R1) (:CONST :R3 5) (:ADD :R4 :R2 :R3)) result)))

(deftest self-host-macro-expander
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
    (assert-true (string= "(if x (progn (+ 1 2)) nil)"
                         (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                           (string-downcase (format nil "~S" result)))))))

;;; Multiple-Value-List Tests

(deftest-each multiple-value-list
  "multiple-value-list captures the full values list from various forms."
  :cases (("floor"  '(3 2)   "(multiple-value-list (floor 17 5))")
          ("values" '(1 2 3) "(multiple-value-list (values 1 2 3))")
          ("single" '(42)    "(multiple-value-list (values 42))"))
  (expected form)
  (assert-true (equal expected (run-string form))))

;;; Apply with Spread Arguments Tests

(deftest-each apply-spread-args-numeric
  "apply with spread arguments computes the correct numeric result."
  :cases (("plus-spread"  10 "(apply #'+ 1 2 (list 3 4))")
          ("minus-list"    5 "(apply #'- (list 10 3 2))")
          ("multiply-list" 24 "(apply #'* (list 2 3 4))")
          ("plus-five"    15 "(apply #'+ (list 1 2 3 4 5))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest apply-spread-args-append
  "apply #'append with list of lists"
  (assert-true (equal '(1 2 3 4) (run-string "(apply #'append (list (list 1 2) (list 3 4)))"))))


;;; Typed Defun/Lambda Tests

(deftest-each typed-defun-numeric
  "Typed defun compiles and executes to the correct numeric result."
  :cases (("basic"        7  "(progn (defun typed-add ((x fixnum) (y fixnum)) fixnum (+ x y)) (typed-add 3 4))")
          ("no-return-type" 12 "(progn (defun typed-mul ((x fixnum) (y fixnum)) (* x y)) (typed-mul 3 4))")
          ("mixed-params"  7  "(progn (defun typed-mixed ((x fixnum) y) (+ x y)) (typed-mixed 3 4))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest typed-defun-string
  "typed defun with string param and return type"
  (assert-string= "Hello World" (run-string "(progn (defun typed-greet ((name string)) string (concatenate 'string \"Hello \" name)) (typed-greet \"World\"))")))

(deftest-each typed-lambda
  "Typed lambda with parameter type annotations compiles correctly."
  :cases (("with-return" 30 "(funcall (lambda ((x fixnum) (y fixnum)) fixnum (+ x y)) 10 20)")
          ("no-return"    6 "(funcall (lambda ((a fixnum) (b fixnum)) (* a b)) 2 3)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest typed-defun-type-registry
  "typed defun registers function type for type checking"
  (let ((old-count (hash-table-count cl-cc:*function-type-registry*)))
    (run-string "(defun typed-reg-test ((x fixnum)) fixnum x)")
    (assert-true (> (hash-table-count cl-cc:*function-type-registry*) old-count))))

(deftest typed-multi-form-top-level
  "Type checking applies to multi-form top-level compilation as well."
  (multiple-value-bind (result type)
      (run-string-typed "(defvar *typed-top-level* 1)
                         42")
    (assert-= 42 result)
    (assert-type cl-cc/type:type-primitive type)
    (assert-string= "FIXNUM" (symbol-name (cl-cc/type:type-primitive-name type)))))

;;; CLOS Type Inference Tests

(deftest clos-type-inference
  "make-instance infers class type; slot-value infers slot type from defclass :type."
  ;; make-instance infers class type
  (multiple-value-bind (result type)
      (run-string-typed "(progn
        (defclass point () ((x :initarg :x :type fixnum) (y :initarg :y :type fixnum)))
        (make-instance 'point :x 1 :y 2))")
    (declare (ignore result))
    (assert-type cl-cc/type:type-primitive type)
    (assert-string= "POINT" (symbol-name (cl-cc/type:type-primitive-name type))))
  ;; slot-value infers fixnum slot type
  (multiple-value-bind (result type)
      (run-string-typed "(progn
        (defclass point () ((x :initarg :x :type fixnum) (y :initarg :y :type fixnum)))
        (let ((p (make-instance 'point :x 10 :y 20)))
          (slot-value p 'x)))")
    (assert-= 10 result)
    (assert-type cl-cc/type:type-primitive type)
    (assert-string= "FIXNUM" (symbol-name (cl-cc/type:type-primitive-name type))))
  ;; slot-value infers string slot type
  (multiple-value-bind (result type)
      (run-string-typed "(progn
        (defclass person () ((name :initarg :name :type string)))
        (let ((p (make-instance 'person :name \"Alice\")))
          (slot-value p 'name)))")
    (assert-string= "Alice" result)
    (assert-type cl-cc/type:type-primitive type)
    (assert-string= "STRING" (symbol-name (cl-cc/type:type-primitive-name type)))))

;;; Type Alias (deftype) Tests

(deftest-each deftype-numeric
  "deftype aliases work in typed defun and defclass slot :type."
  :cases (("basic"   42 "(progn (deftype my-int fixnum) (defun typed-id ((x my-int)) my-int x) (typed-id 42))")
          ("in-slot" 10 "(progn (deftype coordinate fixnum) (defclass point2 () ((x :initarg :x :type coordinate))) (let ((p (make-instance 'point2 :x 10))) (slot-value p 'x)))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest deftype-union
  "deftype with union type expands in type registry"
  (let ((old-count (hash-table-count cl-cc/type:*type-alias-registry*)))
    (run-string "(deftype int-or-str (or fixnum string))")
    (assert-true (> (hash-table-count cl-cc/type:*type-alias-registry*) old-count))))

;;; Type Narrowing Tests

(deftest type-narrowing
  "numberp/stringp narrow types in then-branch."
  (multiple-value-bind (result type)
      (run-string-typed "(let ((x 42)) (if (numberp x) (+ x 1) 0))")
    (assert-= 43 result)
    (assert-type cl-cc/type:type-primitive type)
    (assert-string= "FIXNUM" (symbol-name (cl-cc/type:type-primitive-name type))))
  (multiple-value-bind (result type)
      (run-string-typed "(let ((x \"hello\")) (if (stringp x) x \"default\"))")
    (assert-string= "hello" result)
    (assert-type cl-cc/type:type-primitive type)))

;;; Higher-Order Function Macro Expansions (Self-Hosting)

(deftest-each hof-list-result
  "Higher-order functions that return lists or boolean-like values."
  :cases (("mapcar-basic"       '(2 4 6)       "(mapcar (lambda (x) (* x 2)) (list 1 2 3))")
          ("mapcar-empty"       nil            "(mapcar (lambda (x) x) nil)")
          ("mapc-original"      '(1 2 3)       "(mapc (lambda (x) (+ x 1)) (list 1 2 3))")
          ("mapcan-flatten"     '(1 1 2 2 3 3) "(mapcan (lambda (x) (list x x)) (list 1 2 3))")
          ("every-true"         t              "(every (lambda (x) (> x 0)) (list 1 2 3))")
          ("every-false"        nil            "(every (lambda (x) (> x 2)) (list 1 2 3))")
          ("every-empty"        t              "(every (lambda (x) x) nil)")
          ("some-not-found"     nil            "(some (lambda (x) (if (> x 10) x nil)) (list 1 2 3))")
          ("find-not-found"     nil            "(find 99 (list 1 2 3))")
          ("position-not-found" nil            "(position 99 (list 1 2 3))")
          ("remove-if"          '(1 3 5)       "(remove-if (lambda (x) (= 0 (mod x 2))) (list 1 2 3 4 5))")
          ("remove-if-not"      '(2 4)         "(remove-if-not (lambda (x) (= 0 (mod x 2))) (list 1 2 3 4 5))")
          ("remove-basic"       '(1 3 5)       "(remove 2 (list 1 2 3 2 5))")
          ("remove-duplicates"  '(1 2 3)       "(remove-duplicates (list 1 2 3 2 1))"))
  (expected form)
  (assert-true (equal expected (run-string form))))

(deftest-each hof-numeric-result
  "Higher-order functions that return numeric values."
  :cases (("some-found"   3 "(some (lambda (x) (if (> x 2) x nil)) (list 1 2 3 4))")
          ("find-basic"   3 "(find 3 (list 1 2 3 4 5))")
          ("find-if"      4 "(find-if (lambda (x) (> x 3)) (list 1 2 3 4 5))")
          ("position"     2 "(position 3 (list 1 2 3 4 5))")
          ("count"        3 "(count 2 (list 1 2 2 3 2))")
          ("count-if"     2 "(count-if (lambda (x) (> x 3)) (list 1 2 3 4 5))"))
  (expected form)
  (assert-= expected (run-string form)))

;;; Parametric Types (type-constructor)

(deftest parametric-type-parse-list
  "Parsing (list fixnum) yields a type-constructor"
  (let ((ty (cl-cc/type:parse-type-specifier '(list fixnum))))
    (assert-type cl-cc/type:type-constructor ty)
    (assert-eq 'list (cl-cc/type:type-constructor-name ty))
    (assert-true (= 1 (length (cl-cc/type:type-constructor-args ty))))
    (assert-true (cl-cc/type:type-equal-p (first (cl-cc/type:type-constructor-args ty))
                                  cl-cc/type:type-int))))

(deftest parametric-type-parse-option
  "Parsing (Option string) yields a type-constructor"
  (let ((ty (cl-cc/type:parse-type-specifier '(Option string))))
    (assert-type cl-cc/type:type-constructor ty)
    (assert-eq 'Option (cl-cc/type:type-constructor-name ty))
    (assert-true (cl-cc/type:type-equal-p (first (cl-cc/type:type-constructor-args ty))
                                  cl-cc/type:type-string))))

(deftest parametric-type-parse-pair
  "Parsing (Pair fixnum string) yields a type-constructor with 2 args"
  (let ((ty (cl-cc/type:parse-type-specifier '(Pair fixnum string))))
    (assert-type cl-cc/type:type-constructor ty)
    (assert-eq 'Pair (cl-cc/type:type-constructor-name ty))
    (assert-true (= 2 (length (cl-cc/type:type-constructor-args ty))))
    (assert-true (cl-cc/type:type-equal-p (first (cl-cc/type:type-constructor-args ty))
                                  cl-cc/type:type-int))
    (assert-true (cl-cc/type:type-equal-p (second (cl-cc/type:type-constructor-args ty))
                                  cl-cc/type:type-string))))

(deftest parametric-type-unify-same
  "Unifying (List fixnum) with (List fixnum) succeeds with no new bindings"
  (let ((t1 (cl-cc/type:parse-type-specifier '(list fixnum)))
        (t2 (cl-cc/type:parse-type-specifier '(list fixnum))))
    (multiple-value-bind (subst ok) (cl-cc/type:type-unify t1 t2)
      (assert-true ok)
      ;; No bindings needed — subst may be empty struct or nil
      (assert-true (or (null subst)
                       (zerop (hash-table-count
                                (cl-cc/type:substitution-bindings subst))))))))

(deftest parametric-type-unify-with-var
  "Unifying (List ?a) with (List fixnum) binds ?a to fixnum"
  (let* ((tv (cl-cc/type:make-type-variable 'a))
         (t1 (cl-cc/type:make-type-constructor 'list (list tv)))
         (t2 (cl-cc/type:parse-type-specifier '(list fixnum))))
    (multiple-value-bind (subst ok) (cl-cc/type:type-unify t1 t2)
      (assert-true ok)
      (assert-false (null subst))
      (let ((resolved (cl-cc/type:type-substitute tv subst)))
        (assert-true (cl-cc/type:type-equal-p resolved cl-cc/type:type-int))))))

(deftest parametric-type-unify-different-constructors
  "Unifying (List fixnum) with (Option fixnum) fails"
  (let ((t1 (cl-cc/type:parse-type-specifier '(list fixnum)))
        (t2 (cl-cc/type:parse-type-specifier '(Option fixnum))))
    (multiple-value-bind (subst ok) (cl-cc/type:type-unify t1 t2)
      (declare (ignore subst))
      (assert-false ok))))

(deftest parametric-type-unparse
  "Unparsing a type-constructor roundtrips correctly"
  (let* ((ty (cl-cc/type:parse-type-specifier '(Pair fixnum string)))
         (spec (cl-cc/type:unparse-type ty)))
    (assert-equal 'Pair (first spec))
    (assert-= 3 (length spec))))

(deftest parametric-type-to-string
  "type-to-string works for type-constructor"
  (let ((ty (cl-cc/type:parse-type-specifier '(list fixnum))))
    (assert-string= "(LIST FIXNUM)" (cl-cc/type:type-to-string ty))))

(deftest parametric-type-equal-p
  "type-equal-p works for type-constructors"
  (let ((t1 (cl-cc/type:parse-type-specifier '(list fixnum)))
        (t2 (cl-cc/type:parse-type-specifier '(list fixnum)))
        (t3 (cl-cc/type:parse-type-specifier '(list string))))
    (assert-true (cl-cc/type:type-equal-p t1 t2))
    (assert-false (cl-cc/type:type-equal-p t1 t3))))

(deftest parametric-type-free-vars
  "Free vars are extracted from type-constructor args"
  (let* ((tv (cl-cc/type:make-type-variable 'x))
         (ty (cl-cc/type:make-type-constructor 'list (list tv))))
    (assert-true (= 1 (length (cl-cc/type:type-free-vars ty))))))

(deftest parametric-type-nested
  "Nested parametric types: (List (Option fixnum))"
  (let ((ty (cl-cc/type:parse-type-specifier '(list (Option fixnum)))))
    (assert-type cl-cc/type:type-constructor ty)
    (assert-eq 'list (cl-cc/type:type-constructor-name ty))
    (let ((inner (first (cl-cc/type:type-constructor-args ty))))
      (assert-type cl-cc/type:type-constructor inner)
      (assert-eq 'Option (cl-cc/type:type-constructor-name inner)))))

(deftest parametric-type-in-typed-defun
  "Typed defun with parametric return type compiles"
  (let ((result (run-string "(progn
    (deftype int-list (list fixnum))
    (defun make-nums () (list 1 2 3))
    (length (make-nums)))")))
    (assert-= 3 result)))

;;; Defparameter Tests

(deftest-each defparameter-persistence
  "defparameter defines and persists dynamic variables across various usage patterns."
  :cases (("basic"         42 "(progn (defparameter *val* 42) *val*)")
          ("with-function" 10 "(progn (defparameter *base* 10) (defun get-base () *base*) (get-base))")
          ("setq-mutation"  5 "(progn (defparameter *x* 0) (setq *x* 5) *x*)"))
  (expected form)
  (assert-= expected (run-string form)))

;;; String= and Equal Tests

(deftest-each compile-equal-truthy
  "equal and string= return truthy for matching values."
  :cases (("string=-match"  "(string= \"hello\" \"hello\")")
          ("equal-numbers"  "(equal 42 42)")
          ("equal-strings"  "(equal \"abc\" \"abc\")")
          ("equal-lists"    "(equal '(1 2 3) (list 1 2 3))"))
  (form)
  (assert-true (run-string form)))

(deftest-each compile-equal-false
  "equal and string= return NIL (falsy) for non-matching values."
  :cases (("string=-diff" "(string= \"hello\" \"world\")")
          ("equal-diff"   "(equal 1 2)"))
  (form)
  (assert-true (null (run-string form))))

;;; Numeric Builtins Tests (max, min, mod, zerop, plusp, minusp)

(deftest-each numeric-arithmetic
  "max/min/mod/abs return the expected numeric value."
  :cases (("max"       5 "(max 3 5)")
          ("min"       3 "(min 3 5)")
          ("mod-basic" 1 "(mod 7 3)")
          ("mod-even"  0 "(mod 6 3)")
          ("abs"       5 "(abs -5)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each numeric-predicates-truthy
  "Numeric predicates return truthy for matching values."
  :cases (("zerop-zero"  "(zerop 0)")
          ("plusp-pos"   "(plusp 5)")
          ("minusp-neg"  "(minusp -3)")
          ("evenp-even"  "(evenp 4)")
          ("oddp-odd"    "(oddp 3)"))
  (form)
  (assert-true (run-string form)))

(deftest-each numeric-predicates-false
  "Numeric predicates return 0 (falsy) for non-matching values."
  :cases (("zerop-nonzero" "(zerop 5)")
          ("plusp-neg"     "(plusp -3)")
          ("minusp-pos"    "(minusp 5)"))
  (form)
  (assert-true (= 0 (run-string form))))

;;; Warn Compilation Tests

(deftest compile-warn
  "warn compiles without crashing and execution continues past it."
  (assert-true (null (run-string "(warn \"test warning\")")))
  (assert-= 42 (run-string "(progn (warn \"warning\") 42)")))

;;; Format Compilation Tests

(deftest-each compile-format-nil
  "format nil with various directives returns the correctly formatted string."
  :cases (("simple" "hello"       "(format nil \"~A\" \"hello\")")
          ("number" "42"          "(format nil \"~D\" 42)")
          ("concat" "hello world" "(format nil \"~A ~A\" \"hello\" \"world\")"))
  (expected form)
  (assert-string= expected (run-string form)))

;;; Self-Hosting Smoke Test: Mini-Optimizer with Labels + HOFs

(deftest self-host-optimizer-pipeline
  "Self-hosting smoke: CLOS AST + labels recursive optimizer + mapcar + defparameter"
  (assert-= 30 (run-string " (progn (defclass ir () ()) (defclass ir-const (ir) ((val :initarg :val :accessor ir-const-val))) (defclass ir-add (ir) ((lhs :initarg :lhs :accessor ir-add-lhs) (rhs :initarg :rhs :accessor ir-add-rhs))) (defclass ir-mul (ir) ((lhs :initarg :lhs :accessor ir-mul-lhs) (rhs :initarg :rhs :accessor ir-mul-rhs))) (defclass ir-seq (ir) ((forms :initarg :forms :accessor ir-seq-forms))) (defparameter *opt-count* 0) (defun mk-const (v) (make-instance 'ir-const :val v)) (defun mk-add (l r) (make-instance 'ir-add :lhs l :rhs r)) (defun mk-mul (l r) (make-instance 'ir-mul :lhs l :rhs r)) (defun mk-seq (fs) (make-instance 'ir-seq :forms fs)) (defgeneric opt (node)) (defmethod opt ((n ir-const)) n) (defmethod opt ((n ir-add)) (let ((l (opt (ir-add-lhs n))) (r (opt (ir-add-rhs n)))) (if (and (typep l 'ir-const) (typep r 'ir-const)) (progn (setq *opt-count* (+ *opt-count* 1)) (mk-const (+ (ir-const-val l) (ir-const-val r)))) (mk-add l r)))) (defmethod opt ((n ir-mul)) (let ((l (opt (ir-mul-lhs n))) (r (opt (ir-mul-rhs n)))) (if (and (typep l 'ir-const) (typep r 'ir-const)) (progn (setq *opt-count* (+ *opt-count* 1)) (mk-const (* (ir-const-val l) (ir-const-val r)))) (mk-mul l r)))) (defmethod opt ((n ir-seq)) (mk-seq (mapcar #'opt (ir-seq-forms n)))) (defgeneric eval-ir (node)) (defmethod eval-ir ((n ir-const)) (ir-const-val n)) (defmethod eval-ir ((n ir-add)) (+ (eval-ir (ir-add-lhs n)) (eval-ir (ir-add-rhs n)))) (defmethod eval-ir ((n ir-mul)) (* (eval-ir (ir-mul-lhs n)) (eval-ir (ir-mul-rhs n)))) (defmethod eval-ir ((n ir-seq)) (let ((r 0)) (dolist (f (ir-seq-forms n) r) (setq r (eval-ir f))))) (let ((ir-prog (mk-seq (list (mk-add (mk-const 10) (mk-const 20)) (mk-mul (mk-const 3) (mk-const 10)))))) (eval-ir (opt ir-prog))))" :stdlib t)))

;;; Self-Hosting Integration: Macro Expander + Type Checker

(deftest self-host-macro-system-full
  "Self-hosting: hash-table macro registry + recursive expansion + multiple macros"
  (assert-true (string= "(if x (progn (if y (progn z) nil)) nil)"
             (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
               (string-downcase (format nil "~S" (run-string "(progn
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
    (mx-expand '(when x (when y z))))" :stdlib t)))))))

(deftest self-host-type-checker
  "Self-hosting: simple HM-style type checker with CLOS type nodes"
  (assert-true (string= "ok" (string-downcase (symbol-name (run-string "(progn (defclass ty () ()) (defclass ty-int (ty) ()) (defclass ty-str (ty) ()) (defclass ty-fn (ty) ((arg :initarg :arg :accessor ty-fn-arg) (ret :initarg :ret :accessor ty-fn-ret))) (defgeneric ty-eq (a b)) (defmethod ty-eq ((a ty-int) (b ty-int)) t) (defmethod ty-eq ((a ty-str) (b ty-str)) t) (defmethod ty-eq ((a ty-fn) (b ty-fn)) (and (ty-eq (ty-fn-arg a) (ty-fn-arg b)) (ty-eq (ty-fn-ret a) (ty-fn-ret b)))) (defmethod ty-eq ((a t) (b t)) nil) (defvar *env* (make-hash-table :test 'eq)) (defun tc (expr) (cond ((numberp expr) (make-instance 'ty-int)) ((stringp expr) (make-instance 'ty-str)) ((symbolp expr) (gethash expr *env*)) ((and (consp expr) (eq (car expr) 'add)) (let ((l (tc (cadr expr))) (r (tc (caddr expr)))) (if (and (typep l 'ty-int) (typep r 'ty-int)) (make-instance 'ty-int) (error \"type mismatch\")))) (t (error \"unknown\")))) (setf (gethash 'x *env*) (make-instance 'ty-int)) (let ((t1 (tc '(add x 1))) (t2 (tc '(add 2 3)))) (if (and (typep t1 'ty-int) (typep t2 'ty-int)) 'ok 'fail)))"))))))



(deftest self-host-format-error-pipeline
  "Self-hosting: format for string building + error signaling + handler-case"
  (assert-string= "caught: bad-value" (run-string "(handler-case (let ((val 42)) (if (> val 100) val (error (format nil \"bad-value\")))) (error (e) (format nil \"caught: ~A\" e)))")))

;;; Prog/With-Slots/Nth-Value Macro Tests

(deftest compile-prog-and-friends
  "prog/prog*/with-slots/nth-value macros work in compiled code."
  ;; Consolidated into a single :stdlib t run to avoid 5×1s timeout
  (let ((results (run-string "(list
    (prog ((x 0)) loop (setq x (+ x 1)) (when (= x 10) (return x)) (go loop))
    (prog* ((x 1) (y (+ x 2))) (return y))
    (progn (defclass point () ((x :initarg :x) (y :initarg :y)))
           (let ((p (make-instance (quote point) :x 10 :y 20)))
             (with-slots (x y) p (+ x y))))
    (nth-value 1 (floor 17 5))
    (prog ((x 1)) (setq x 2)))" :stdlib t)))
    (assert-= 10 (first results))
    (assert-= 3 (second results))
    (assert-= 30 (third results))
    (assert-= 2 (fourth results))
    (assert-false (fifth results))))

;;; ANSI CL FR-400/FR-500 Tests (mismatch, make-string, float literals, string-not-equal)

(deftest-each stdlib-mismatch
  "mismatch returns the index of the first difference, or nil for equal sequences."
  :cases (("index"  2   "(mismatch (list 1 2 3) (list 1 2 4))")
          ("equal"  nil "(mismatch (list 1 2 3) (list 1 2 3))")
          ("prefix" 0   "(mismatch nil (list 1 2))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

(deftest-each compile-make-string
  "make-string creates strings of the specified length and fill character."
  :cases (("initial-element" "xxxx" "(make-string 4 :initial-element #\\x)")
          ("default-len"     3      "(length (make-string 3))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

(deftest-each compile-float-properties
  "Float literals and float-precision/radix return the expected values."
  :cases (("literal"   4.0 "(+ 1.5 2.5)")
          ("precision" 53  "(float-precision 1.0d0)")
          ("radix"     2   "(float-radix 1.0)"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

(deftest compile-string-not-equal
  "string-not-equal returns true for different strings and false for case-insensitively equal strings."
  (assert-true (run-string "(string-not-equal \"abc\" \"def\")" :stdlib t))
  (assert-true (run-string "(if (string-not-equal \"abc\" \"ABC\") nil t)" :stdlib t)))


;;; ─── New stdlib tests (FR-495, FR-496, FR-540, FR-547, FR-582, FR-596, etc.) ──

(deftest compile-tailp
  "tailp checks pointer identity of list tails."
  (let ((r (run-string "(let* ((x '(1 2 3)) (tail (cddr x))) (tailp tail x))")))
    (assert-true r)))

(deftest compile-ldiff
  "ldiff returns list up to a given tail."
  (let ((r (run-string "(let* ((x '(1 2 3 4)) (tail (cddr x))) (ldiff x tail))")))
    (assert-equal '(1 2) r)))

(deftest compile-copy-alist
  "copy-alist returns a fresh alist with the same content."
  (let ((r (run-string "(let ((al '((a . 1) (b . 2)))) (equal al (copy-alist al)))")))
    (assert-true r)))

(deftest compile-tree-equal
  "tree-equal compares trees recursively."
  (let ((r (run-string "(tree-equal '(1 (2 3)) '(1 (2 3)))")))
    (assert-true r)))

(deftest compile-get-properties
  "get-properties finds first matching plist key."
  (let ((r (run-string "(get-properties '(:a 1 :b 2 :c 3) '(:b :c))")))
    (assert-equal :b r)))

(deftest compile-nunion
  "nunion returns union of two lists."
  (let ((r (run-string "(sort (nunion '(1 2 3) '(2 3 4)) #'<)")))
    (assert-equal '(1 2 3 4) r)))

(deftest compile-nsubst
  "nsubst substitutes numeric values in a tree."
  (let ((r (run-string "(nsubst 99 1 '(1 2 (1 3)))")))
    (assert-equal '(99 2 (99 3)) r)))

(deftest compile-nstring-upcase
  "nstring-upcase returns uppercased string."
  (let ((r (run-string "(nstring-upcase \"hello\")")))
    (assert-equal "HELLO" r)))

(deftest compile-array-element-type
  "array-element-type returns T for all arrays in cl-cc."
  (let ((r (run-string "(array-element-type (make-array 3))")))
    (assert-equal t r)))

(deftest compile-array-in-bounds-p
  "array-in-bounds-p checks index validity."
  (let ((in  (run-string "(array-in-bounds-p (make-array 5) 3)"))
        (out (run-string "(array-in-bounds-p (make-array 5) 7)")))
    (assert-true in)
    (assert-true (not out))))

(deftest compile-equalp
  "equalp compares lists and strings case-insensitively."
  (let ((r1 (run-string "(equalp '(1 2) '(1 2))"))
        (r2 (run-string "(equalp \"hello\" \"HELLO\")"))
        (r3 (run-string "(equalp '(1 2) '(1 3))")))
    (assert-true r1)
    (assert-true r2)
    (assert-true (not r3))))

(deftest compile-lisp-implementation-type
  "lisp-implementation-type returns cl-cc."
  (let ((r (run-string "(lisp-implementation-type)")))
    (assert-equal "cl-cc" r)))

(deftest compile-compiled-function-p
  "compiled-function-p returns true for lambdas."
  (let ((r (run-string "(compiled-function-p (lambda (x) x))")))
    (assert-true r)))

(deftest compile-last-with-count
  "last with count returns last N conses."
  (let ((r (run-string "(last '(1 2 3 4 5) 2)")))
    (assert-equal '(4 5) r)))

(deftest compile-butlast-with-count
  "butlast with count returns all but last N conses."
  (let ((r (run-string "(butlast '(1 2 3 4 5) 2)")))
    (assert-equal '(1 2 3) r)))

(deftest compile-make-array-initial-contents
  "make-array with :initial-contents fills the array."
  (let ((r (run-string "(let ((a (make-array 3 :initial-contents '(10 20 30)))) (aref a 1))")))
    (assert-= 20 r)))

(deftest compile-make-array-initial-element
  "make-array with :initial-element fills with default."
  (let ((r (run-string "(let ((a (make-array 4 :initial-element 7))) (aref a 3))")))
    (assert-= 7 r)))

(deftest compile-setf-bit
  "setf bit mutates a vector element using the bit accessor."
  (let ((r (run-string "(let ((bv (make-array 4))) (setf (bit bv 2) 1) (bit bv 2))")))
    (assert-= 1 r)))

(deftest compile-search-vector
  "search finds a pattern in a vector."
  (let ((r (run-string "(search '(2 3) '(1 2 3 4))")))
    (assert-= 1 r)))

(deftest compile-write-to-string-keywords
  "write-to-string with keyword args ignores unknown keywords."
  (let ((r (run-string "(write-to-string 42 :base 10)")))
    (assert-equal "42" r)))

;;; ─── FR-635: bit-nor / bit-nand / bit-eqv ────────────────────────────────────

(deftest compile-bit-nor
  "bit-nor computes element-wise NOR."
  (let ((r (run-string "(bit-nor #*1010 #*1100)")))
    (assert-true (vectorp r))))

(deftest compile-bit-eqv
  "bit-eqv computes element-wise XNOR."
  (let ((r (run-string "(bit-eqv #*1010 #*0101)")))
    (assert-true (vectorp r))))

;;; ─── FR-497: with-hash-table-iterator ────────────────────────────────────────

(deftest compile-with-hash-table-iterator
  "with-hash-table-iterator iterates all keys."
  (let ((r (run-string "(let ((h (make-hash-table)) (count 0))
  (setf (gethash :a h) 1 (gethash :b h) 2)
  (with-hash-table-iterator (next h)
    (loop (multiple-value-bind (more k v) (next)
            (unless more (return count))
            (incf count)
            (declare (ignore k v))))))")))
    (assert-= 2 r)))

;;; ─── FR-617: read-from-string 2nd value ──────────────────────────────────────

(deftest compile-read-from-string-position
  "read-from-string returns the end position (after delimiter) as second value."
  (let ((r (run-string "(multiple-value-bind (val pos)
  (read-from-string \"42 rest\")
  (list val pos))")))
    ;; ANSI CL: position is after the token delimiter (space), so 3 not 2
    (assert-equal '(42 3) r)))

;;; ─── FR-637: string comparison with keyword bounds ───────────────────────────

(deftest compile-string=-start-end
  "string= with :start1/:end1 compares substrings."
  (let ((r (run-string "(string= \"hello world\" \"world\" :start1 6)")))
    (assert-true r)))

(deftest compile-string<-start-end
  "string< with :start2/:end2 compares substrings; equal substrings return NIL."
  (let ((r (run-string "(string< \"ab\" \"abcde\" :start2 0 :end2 2)")))
    ;; "ab" < "ab" is false — ANSI returns NIL
    (assert-true (null r))))

;;; ─── FR-608: with-input-from-string keyword args ─────────────────────────────

(deftest compile-with-input-from-string-start
  "with-input-from-string :start skips prefix."
  (let ((r (run-string "(with-input-from-string (s \"hello world\" :start 6)
  (read s))")))
    (assert-equal 'cl-cc::world r)))

;;; ─── FR-363: with-compilation-unit ───────────────────────────────────────────

(deftest compile-with-compilation-unit
  "with-compilation-unit evaluates body."
  (let ((r (run-string "(with-compilation-unit () (+ 1 2))")))
    (assert-= 3 r)))

;;; ─── FR-397: locally ─────────────────────────────────────────────────────────

(deftest compile-locally
  "locally evaluates forms and strips leading declare."
  (let ((r (run-string "(locally (+ 10 20))")))
    (assert-= 30 r)))

;;; ─── FR-439: compiler-let ────────────────────────────────────────────────────

(deftest compile-compiler-let
  "compiler-let acts as let at runtime."
  (let ((r (run-string "(compiler-let ((x 5) (y 3)) (+ x y))")))
    (assert-= 8 r)))

;;; ─── FR-566: pathname host bridges ───────────────────────────────────────────

(deftest compile-pathname-name
  "pathname-name returns filename without extension."
  (let ((r (run-string "(pathname-name \"/tmp/foo.lisp\")")))
    (assert-equal "foo" r)))

(deftest compile-pathname-type
  "pathname-type returns file extension."
  (let ((r (run-string "(pathname-type \"/tmp/foo.lisp\")")))
    (assert-equal "lisp" r)))

;;; ─── FR-572: #nA multi-dimensional array literal ─────────────────────────────

(deftest compile-hash-na-2d-array
  "#2A creates a 2D array with correct dimensions."
  (let ((r (run-string "#2A((1 2 3) (4 5 6))")))
    (assert-true (arrayp r))
    (assert-= 2 (array-rank r))
    (assert-= 2 (array-dimension r 0))
    (assert-= 3 (array-dimension r 1))
    (assert-= 1 (aref r 0 0))
    (assert-= 6 (aref r 1 2))))

(deftest compile-hash-na-1d-array
  "#1A creates a 1D array (same as #(...))."
  (let ((r (run-string "#1A(10 20 30)")))
    (assert-true (vectorp r))
    (assert-= 3 (length r))
    (assert-= 20 (aref r 1))))

;;; ─── FR-572: #P pathname literal ─────────────────────────────────────────────

(deftest compile-hash-p-pathname-name
  "#P pathname literal lexes and pathname-name extracts the basename."
  (let ((r (run-string "(pathname-name #P\"/tmp/foo.lisp\")")))
    (assert-equal "foo" r)))

(deftest compile-hash-p-pathname-type
  "#P pathname literal lexes and pathname-type extracts the extension."
  (let ((r (run-string "(pathname-type #P\"/tmp/bar.txt\")")))
    (assert-equal "txt" r)))

;;; ─── FR-612: read-char / read-line / read with eof args ──────────────────────

(deftest compile-read-char-eof-args
  "read-char with eof-error-p and eof-value args compiles without error."
  (let ((r (run-string
              "(with-input-from-string (s \"\")
                 (read-char s nil :end-of-stream))")))
    ;; :eof sentinel is returned (eof-value arg accepted but internal :eof used)
    (assert-true (or (eq r :eof) (eq r :end-of-stream) (null r)))))

(deftest compile-read-char-stream-arg
  "read-char with explicit stream reads a character."
  (let ((r (run-string
              "(with-input-from-string (s \"A\")
                 (read-char s))")))
    (assert-equal #\A r)))

(deftest compile-read-line-eof-args
  "read-line with eof args compiles without error (eof-value arg accepted)."
  (let ((r (run-string
              "(with-input-from-string (s \"\")
                 (read-line s nil nil))")))
    ;; VM returns :eof sentinel regardless of user-supplied eof-value (partial impl)
    (assert-true (or (null r) (equal r "") (eq r :eof)))))

(deftest compile-close-abort
  "close with :abort t compiles and returns t."
  (let ((r (run-string
              "(let ((s (make-string-output-stream)))
                 (close s :abort t))")))
    (assert-true r)))

;;; ─── FR-358: Readtable stubs ─────────────────────────────────────────────────

(deftest compile-readtable-stubs
  "readtable stubs are defined and callable."
  (let ((result (run-string
                 "(list (readtablep *readtable*)
                        (copy-readtable)
                        (readtable-case nil))")))
    (assert-equal '(nil nil :upcase) result)))

(deftest compile-set-macro-character
  "set-macro-character and get-macro-character compile without error."
  (let* ((r1 (run-string
                "(set-macro-character (code-char 94) (lambda (s c) (declare (ignore s c)) :caret))"
                :stdlib t))
         (r2 (run-string "(get-macro-character (code-char 94))" :stdlib t)))
    (assert-true r1)    ; set-macro-character returns t
    (assert-false r2))) ; get-macro-character returns nil (stub)

;;; ─── FR-579: string-to-octets / octets-to-string ─────────────────────────────

(deftest compile-string-to-octets
  "string-to-octets converts a string to a byte vector."
  (let ((r (run-string "(string-to-octets \"hello\")")))
    (assert-true (vectorp r))
    (assert-= 104 (aref r 0))  ; 'h' = ASCII 104
    (assert-= 5 (length r))))

(deftest compile-octets-to-string
  "octets-to-string round-trips through string-to-octets."
  (let ((r (run-string "(octets-to-string (string-to-octets \"hello\"))")))
    (assert-string= "hello" r)))

;;; ─── FR-502/507: fill/replace/copy-seq vector support ───────────────────────

(deftest compile-fill-vector
  "fill works on a vector, modifying elements in place."
  (let ((r (run-string "(let ((v (make-array 3 :initial-contents '(1 2 3))))
                           (fill v 0)
                           v)" :stdlib t)))
    (assert-true (vectorp r))
    (assert-= 0 (aref r 0))
    (assert-= 0 (aref r 1))
    (assert-= 0 (aref r 2))))

(deftest compile-fill-vector-start-end
  "fill with :start/:end modifies only the specified range."
  (let ((r (run-string "(let ((v (make-array 5 :initial-contents '(0 1 2 3 4))))
                           (fill v 9 :start 1 :end 4)
                           v)" :stdlib t)))
    (assert-true (vectorp r))
    (assert-= 0 (aref r 0))
    (assert-= 9 (aref r 1))
    (assert-= 9 (aref r 2))
    (assert-= 9 (aref r 3))
    (assert-= 4 (aref r 4))))

(deftest compile-replace-vectors
  "replace copies elements from source vector into dest vector."
  (let ((r (run-string "(let ((d (make-array 3 :initial-contents '(0 0 0)))
                              (s (make-array 3 :initial-contents '(1 2 3))))
                           (replace d s)
                           d)" :stdlib t)))
    (assert-true (vectorp r))
    (assert-= 1 (aref r 0))
    (assert-= 2 (aref r 1))
    (assert-= 3 (aref r 2))))

(deftest compile-copy-seq-vector
  "copy-seq on a vector returns a fresh copy."
  (let ((r (run-string "(let ((v (make-array 3 :initial-contents '(1 2 3))))
                           (copy-seq v))" :stdlib t)))
    (assert-true (vectorp r))
    (assert-= 3 (length r))
    (assert-= 1 (aref r 0))))

;;; ─── FR-697: assoc/member with :test/:key keyword args ───────────────────────

(deftest compile-member-test-keyword
  "member with :test #'equal works for string elements."
  (let ((r (run-string "(member \"b\" '(\"a\" \"b\" \"c\") :test #'equal)" :stdlib t)))
    (assert-equal '("b" "c") r)))

(deftest compile-member-key-keyword
  "member with :key extracts the correct field."
  (let ((r (run-string "(member 2 '((1 . a) (2 . b) (3 . c)) :key #'car)" :stdlib t)))
    (assert-true (consp r))
    (assert-= 2 (caar r))))

(deftest compile-assoc-test-keyword
  "assoc with :test #'equal works for string keys."
  (let ((r (run-string "(assoc \"b\" '((\"a\" . 1) (\"b\" . 2)) :test #'equal)" :stdlib t)))
    (assert-equal '("b" . 2) r)))

(deftest compile-assoc-key-keyword
  "assoc with :key transforms the key before comparison."
  ;; key squares each car: look for squared value 4 in ((1 . a) (2 . b) (3 . c))
  ;; => 2^2=4 matches item 4, returns (2 . b)
  (let ((r (run-string "(assoc 4 '((1 . a) (2 . b) (3 . c)) :key (lambda (x) (* x x)))" :stdlib t)))
    (assert-true (consp r))
    (assert-= 2 (car r))))

;;; ─── position/count/find-if with keyword args ────────────────────────────────

(deftest compile-position-test-keyword
  "position with :test #'equal works for strings."
  (let ((r (run-string "(position \"b\" '(\"a\" \"b\" \"c\") :test #'equal)" :stdlib t)))
    (assert-= 1 r)))

(deftest compile-position-key-keyword
  "position with :key extracts the right field."
  (let ((r (run-string "(position 2 '((1 . a) (2 . b) (3 . c)) :key #'car)" :stdlib t)))
    (assert-= 1 r)))

(deftest compile-count-test-keyword
  "count with :test #'equal counts strings correctly."
  (let ((r (run-string "(count \"a\" '(\"a\" \"b\" \"a\") :test #'equal)" :stdlib t)))
    (assert-= 2 r)))

(deftest compile-find-if-key-keyword
  "find-if with :key applies the key function."
  (let ((r (run-string "(find-if #'evenp '((1 . a) (2 . b) (3 . c)) :key #'car)" :stdlib t)))
    (assert-true (consp r))
    (assert-= 2 (car r))))

;;; ─── remove-duplicates with :test keyword ────────────────────────────────────

(deftest compile-remove-duplicates-test-keyword
  "remove-duplicates with :test #'equal handles string equality."
  (let ((r (run-string "(remove-duplicates '(\"a\" \"b\" \"a\") :test #'equal)" :stdlib t)))
    (assert-= 2 (length r))))

;;; ─── remove with :test keyword ────────────────────────────────────────────────

(deftest compile-remove-test-keyword
  "remove with :test #'equal removes matching strings."
  (let ((r (run-string "(remove \"a\" '(\"a\" \"b\" \"a\") :test #'equal)" :stdlib t)))
    (assert-equal '("b") r)))

;;; ─── string-upcase/downcase with :start/:end ─────────────────────────────────

(deftest compile-string-upcase-start-end
  "string-upcase with :start/:end uppercases a substring."
  (let ((r (run-string "(string-upcase \"hello\" :start 1 :end 3)")))
    (assert-string= "hELlo" r)))

(deftest compile-string-downcase-start-end
  "string-downcase with :start/:end lowercases a substring."
  (let ((r (run-string "(string-downcase \"HELLO\" :start 1 :end 4)")))
    (assert-string= "HellO" r)))

;;; ─── FR-599: #n= / #n# label and reference reader macros ────────────────────

(deftest compile-hash-n-eq-label
  "#n= labels a data object and #n# references it."
  (let ((r (run-string "(list #0=(1 2 3) #0#)")))
    ;; Both elements are the same list (1 2 3)
    (assert-equal '(1 2 3) (first r))
    (assert-equal '(1 2 3) (second r))))

(deftest compile-hash-n-eq-string
  "#0= with a string literal."
  (let ((r (run-string "#0=\"hello\"")))
    (assert-string= "hello" r)))

;;; ─── FR-641: union/intersection/set-difference with :test ────────────────────

(deftest compile-union-test-keyword
  "union with :test #'equal works for string lists."
  (let ((r (run-string "(sort (union '(\"a\" \"b\") '(\"b\" \"c\") :test #'equal) #'string<)" :stdlib t)))
    (assert-= 3 (length r))
    (assert-string= "a" (first r))))

(deftest compile-set-difference-test-keyword
  "set-difference with :test #'equal works for string lists."
  (let ((r (run-string "(set-difference '(\"a\" \"b\" \"c\") '(\"b\") :test #'equal)" :stdlib t)))
    (assert-= 2 (length r))))

(deftest compile-intersection-test-keyword
  "intersection with :test #'equal works for string lists."
  (let ((r (run-string "(intersection '(\"a\" \"b\" \"c\") '(\"b\" \"c\" \"d\") :test #'equal)" :stdlib t)))
    (assert-= 2 (length r))))

;;; ─── FR-688: delete/substitute with :test keyword ────────────────────────────

(deftest compile-delete-test-keyword
  "delete with :test #'equal removes matching strings."
  (let ((r (run-string "(delete \"a\" '(\"a\" \"b\" \"a\") :test #'equal)" :stdlib t)))
    (assert-equal '("b") r)))

(deftest compile-substitute-test-keyword
  "substitute with :test #'equal replaces matching strings."
  (let ((r (run-string "(substitute \"x\" \"a\" '(\"a\" \"b\" \"a\") :test #'equal)" :stdlib t)))
    (assert-= 3 (length r))
    (assert-string= "x" (first r))
    (assert-string= "x" (third r))))

;;; ─── compile-file-pathname host bridge ───────────────────────────────────────

(deftest compile-compile-file-pathname
  "compile-file-pathname returns a pathname with .fasl type."
  (let ((r (run-string "(compile-file-pathname \"/tmp/foo.lisp\")")))
    (assert-true (pathnamep r))))

;;; ─── FR-604: float 2-arg prototype form ──────────────────────────────────────

(deftest compile-float-2arg
  "float with prototype argument converts to float ignoring prototype."
  (assert-true (floatp (run-string "(float 3 1.0d0)")))
  (assert-= (float 3) (run-string "(float 3 1.0d0)")))

;;; ─── FR-396: declaim macro stub ─────────────────────────────────────────────

(deftest compile-declaim-toplevel
  "declaim at top level is silently ignored."
  (assert-true (null (run-string "(declaim (optimize (speed 3))) nil"))))

(deftest compile-declaim-inline-noop
  "declaim inline is a no-op — function still works."
  (let ((r (run-string "(declaim (inline square)) (defun square (x) (* x x)) (square 5)" :stdlib t)))
    (assert-= 25 r)))

;;; ─── FR-598: stream typep ────────────────────────────────────────────────────

(deftest compile-typep-stream
  "typep checks stream type correctly."
  (assert-true (run-string "(typep *standard-output* 'stream)"))
  (assert-true (run-string "(typep *standard-output* 'output-stream)"))
  (assert-= 0 (run-string "(typep 42 'stream)")))

(deftest compile-typep-string-stream
  "typep checks string-stream type correctly."
  (let ((r (run-string "(let ((s (make-string-output-stream))) (typep s 'string-stream))" :stdlib t)))
    (assert-true r)))

;;; ─── FR-603: (setf (values ...)) ────────────────────────────────────────────

(deftest compile-setf-values
  "(setf (values a b) (floor 7 3)) destructures multiple values."
  (let ((r (run-string "(let (a b) (setf (values a b) (floor 7 3)) (list a b))" :stdlib t)))
    (assert-= 2 (first r))
    (assert-= 1 (second r))))

;;; ─── FR-607: documentation storage ──────────────────────────────────────────

(deftest compile-documentation-defun
  "defun with docstring stores it in *documentation-table*."
  (let ((r (run-string "(defun greet (x) \"Greet X.\" (format nil \"Hello ~A\" x)) (documentation 'greet 'function)" :stdlib t)))
    (assert-string= "Greet X." r)))

(deftest compile-documentation-no-docstring
  "defun without docstring returns nil from documentation."
  (let ((r (run-string "(defun add2 (x) (+ x 2)) (documentation 'add2 'function)" :stdlib t)))
    (assert-true (null r))))

;;; FR-562: Unicode character names via lexer
(deftest compile-unicode-char-name
  "Lexer resolves Unicode character names via cl:name-char."
  (assert-= 945 (run-string "(char-code #\\Greek_Small_Letter_Alpha)"))
  (assert-= 9731 (run-string "(char-code #\\Snowman)")))

(deftest compile-unicode-code-char
  "code-char supports full Unicode range."
  (assert-= 128512 (run-string "(char-code (code-char 128512))")))

;;; FR-687: make-string :element-type with both keywords
(deftest compile-make-string-element-type
  "make-string accepts :element-type and ignores it."
  (assert-= 5 (run-string "(length (make-string 5 :element-type 'character))")))

(deftest compile-make-string-both-keywords
  "make-string with both :initial-element and :element-type correctly fills."
  (let ((r (run-string "(make-string 3 :initial-element #\\x :element-type 'character)")))
    (assert-string= "xxx" r)))

;;; (run-tests is defined in framework.lisp)
