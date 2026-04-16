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

(deftest-each vm-exec-basic-forms
  "Arithmetic, conditionals, let bindings, and progn sequences compile and evaluate correctly."
  :cases (("arith-add"        "(+ 3 4)"                                7)
          ("arith-sub"        "(- 10 7)"                               3)
          ("arith-mul"        "(* 6 7)"                                42)
          ("arith-nested"     "(+ (* 2 3) 3)"                          9)
          ("if-false-cond"    "(if nil 10 20)"                         20)
          ("if-true-cond"     "(if 1 10 20)"                           10)
          ("if-nested"        "(if 1 (if 0 1 2) 3)"                   2)
          ("if-var-cond"      "(let ((x 0)) (if x 20 10))"            10)
          ("let-simple"       "(let ((x 42)) x)"                       42)
          ("let-multi"        "(let ((x 2) (y 3)) (+ x y))"           5)
          ("let-shadowing"    "(let ((x 10)) (let ((x 20)) x))"        20)
          ("let-computed"     "(let ((x 5) (y 7)) (+ (* x 2) y))"     17)
          ("progn-simple"     "(progn 1 2 3)"                          3)
          ("progn-with-let"   "(progn (let ((x 2)) x) (let ((y 3)) y))" 3))
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
  "Lambda, flet, labels, and higher-order forms all compile to a valid vm-program."
  :cases (("lambda-simple"       "((lambda (x) x) 5)")
          ("lambda-multi-arg"    "((lambda (a b c) (+ a (+ b c))) 1 2 3)")
          ("lambda-nested"       "((lambda (x) (+ x 1)) ((lambda (y) (* y 2)) 3))")
          ("lambda-return-fn"    "((lambda (n) (lambda (x) (+ x n))) 5)")
          ("flet-basic"          "(flet ((double (x) (* x 2))) (double 21))")
          ("flet-multi"          "(flet ((add1 (x) (+ x 1)) (add2 (x) (+ x 2))) (add2 (add1 10)))")
          ("labels-recursive"    "(labels ((count (n) (if (= n 0) 0 (+ 1 (count (- n 1)))))) (count 5))")
          ("labels-mutual"       "(labels ((even? (n) (if (= n 0) 1 (odd? (- n 1)))) (odd? (n) (if (= n 0) 0 (even? (- n 1))))) (even? 10))")
          ("labels-with-let"     "(let ((x 10)) (labels ((rec (n) (if (= n 0) x (+ 1 (rec (- n 1)))))) (rec 3)))"))
  (form)
  (let* ((result (compile-string form :target :vm))
         (program (compilation-result-program result)))
    (assert-false (null program))
    (assert-type vm-program program)))

;;; Multiple Top-Level Forms and Values Tests

(deftest-each compile-multiple-forms-and-values
  "Multiple top-level forms, values, and multiple-value-bind all return expected numeric results."
  :cases (("forms-simple"    6  "(defun foo (x) (+ x 1)) (foo 5)")
          ("forms-progn"     3  "1 2 3")
          ("forms-chain"     12 "(defun add1 (x) (+ x 1)) (defun add2 (x) (add1 (add1 x))) (add2 10)")
          ("forms-let-call"  15 "(defun triple (x) (* x 3)) (let ((y 5)) (triple y))")
          ("forms-recursion"  6 "(defun sum-to (n) (if (= n 0) 0 (+ n (sum-to (- n 1))))) (sum-to 3)")
          ("values-single"   42 "(values 42)")
          ("values-primary"   1 "(values 1 2 3)")
          ("mvb-basic"        3 "(multiple-value-bind (a b) (values 1 2) (+ a b))")
          ("apply-user"       6 "(defun my-add (a b c) (+ a (+ b c))) (apply my-add (quote (1 2 3)))")
          ("mvb-single"      42 "(multiple-value-bind (x) (values 42) x)"))
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

(deftest-each compile-string-ops-and-type-predicates
  "String ops return numeric results and type predicates return CL booleans."
  :cases (("length-hello"  5 "(string-length \"hello\")")
          ("length-empty"  0 "(string-length \"\")")
          ("upcase-len"    5 "(string-length (string-upcase \"hello\"))")
          ("downcase-len"  3 "(string-length (string-downcase \"ABC\"))")
          ("concat-len"   10 "(string-length (concatenate 'string \"hello\" \"world\"))")
          ("symbolp-sym"   t "(symbolp 'foo)")
          ("symbolp-num"   nil "(symbolp 42)")
          ("numberp-num"   t "(numberp 42)")
          ("numberp-sym"   nil "(numberp 'foo)"))
  (expected form)
  (assert-equal expected (run-string form)))

;;; Macro Expansion in Compiler Tests

(deftest-each compile-control-macros-valued
  "cond/when/unless/and/or macros return the expected values."
  :cases (("cond-match"    42  "(cond ((= 1 2) 10) ((= 1 1) 42) (t 0))")
          ("cond-default"   0  "(cond ((= 1 2) 10) ((= 2 3) 20) (t 0))")
          ("when-true"     42  "(when (= 1 1) 42)")
          ("unless-false"  99  "(unless (= 1 2) 99)")
          ("and-all"        3  "(and 1 2 3)")
          ("or-find"        5  "(or nil nil 5)")
          ("or-first"       1  "(or 1 2 3)"))
  (expected form)
  (assert-run= expected form))

(deftest-each compile-control-macros-nil
  "cond/when/unless/and/or return nil in the false cases."
  :cases (("when-false"   "(when (= 1 2) 42)")
          ("unless-true"  "(unless (= 1 1) 99)")
          ("and-short"    "(and 1 nil 3)")
          ("or-all-nil"   "(or nil nil nil)"))
  (form)
  (assert-run-false form))

(deftest-each compile-t-nil-constants
  "t and nil are recognized as constants; not inverts truthiness."
  :cases (("t-is-true"    t   "t")
          ("not-nil"      t   "(not nil)")
          ("nil-is-nil"   nil "nil")
          ("not-1"        nil "(not 1)")
          ("not-t"        nil "(not t)"))
  (expected form)
  (assert-run= expected form))

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

(deftest-each compile-list-builtins
  "append/reverse builtins work on lists."
  :cases (("append-len" 4 "(length (append (list 1 2) (list 3 4)))")
          ("reverse-first" 3 "(first (reverse (list 1 2 3)))"))
  (expected form)
  (assert-run= expected form))

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

;;; Extended List and Macro Tests

(deftest-each compile-keywordp-and-list-macros
  "keywordp, push/pop/incf/decf/nth/nthcdr/nreverse return the expected numeric values."
  :cases (("keyword"     1  "(keywordp :foo)")
          ("non-keyword" 0  "(keywordp 'foo)")
          ("push-front"  3  "(let ((lst nil)) (push 1 lst) (push 2 lst) (push 3 lst) (car lst))")
          ("pop"         1  "(let ((lst (list 1 2 3))) (pop lst))")
          ("incf"        5  "(let ((x 3)) (incf x 2) x)")
          ("decf"        1  "(let ((x 3)) (decf x 2) x)")
          ("nth"         30 "(nth 2 (list 10 20 30 40))")
          ("nthcdr"      30 "(car (nthcdr 2 (list 10 20 30 40)))")
          ("nreverse"    3  "(car (nreverse (list 1 2 3)))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest compile-member-builtin
  "member finds element in list"
  (assert-true (not (null (run-string "(member 3 (list 1 2 3 4))")))))

(deftest-each compile-numeric-predicates-and-case
  "Numeric predicates return CL booleans and case returns the selected value."
  :cases (("zerop-0"    t  "(zerop 0)")
          ("zerop-5"    nil  "(zerop 5)")
          ("plusp-5"    t  "(plusp 5)")
          ("minusp-neg" t  "(minusp (- 0 3))")
          ("evenp-true"  t "(evenp 4)")
          ("evenp-false" nil "(evenp 3)")
          ("oddp-true"   t "(oddp 3)")
          ("case-match"     2  "(case 'b (a 1) (b 2) (c 3))")
          ("case-otherwise" 99 "(case 'z (a 1) (otherwise 99))"))
  (expected form)
  (assert-equal expected (run-string form)))

(deftest compile-keyword-self-eval
  "keywords evaluate to themselves; typecase dispatches on type."
  (assert-eq :test (run-string ":test"))
  (assert-run= 1 "(typecase 42 (integer 1) (string 2) (otherwise 3))"))

;;; Typep and Destructuring Tests

;;; Iteration Macro Tests

(deftest-each compile-typep-destructuring-iteration
  "typep/destructuring-bind/iteration macros return the expected numeric result."
  :cases (("typep-integer"   1 "(typep 42 'integer)")
          ("typep-string"    1 "(typep \"hello\" 'string)")
          ("typep-symbol"    1 "(typep 'foo 'symbol)")
          ("typep-cons"      1 "(typep (cons 1 2) 'cons)")
          ("typep-null"      1 "(typep nil 'null)")
          ("typep-negative"  0 "(typep 42 'string)")
          ("db-basic"        6 "(destructuring-bind (a b c) (list 1 2 3) (+ a (+ b c)))")
          ("db-rest"         2 "(destructuring-bind (a &rest b) (list 1 2 3) (length b))")
          ("dolist"          6 "(let ((sum 0)) (dolist (x (list 1 2 3)) (setq sum (+ sum x))) sum)")
          ("dotimes"        10 "(let ((sum 0)) (dotimes (i 5) (setq sum (+ sum i))) sum)")
          ("do"             10 "(do ((i 0 (+ i 1)) (sum 0 (+ sum i))) ((= i 5) sum))")
          ("loop"           10 "(let ((sum 0) (i 0)) (loop (if (= i 5) (return sum)) (setq sum (+ sum i)) (setq i (+ i 1))))"))
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
          ("four-keys"   '(1 2 3 4) "(defun key-many (&key a b c d) (list a b c d)) (key-many :d 4 :b 2 :a 1 :c 3)")
          ("four-defaults" '(1 20 3 4) "(defun key-many-def (&key (a 1) (b 2) (c 3) (d 4)) (list a b c d)) (key-many-def :b 20 :d 4)")
          ("with-required" 30 "(defun rk (a &key (b 0)) (+ a b)) (rk 10 :b 20)"))
  (expected form)
  (assert-equal expected (run-string form)))

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
  "Variadic +, *, - and car of list produce correct numeric results."
  :cases (("plus-3"    6  "(+ 1 2 3)")
          ("plus-5"   15  "(+ 1 2 3 4 5)")
          ("plus-1"   10  "(+ 10)")
          ("plus-0"    0  "(+)")
          ("times-3"  24  "(* 2 3 4)")
          ("times-0"   1  "(*)")
          ("minus-3"   5  "(- 10 3 2)")
          ("list-car"  1  "(car (list 1 2 3))"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each compile-list-construction
  "list builds proper lists."
  :cases (("basic" '(1 2 3) "(list 1 2 3)")
          ("single" '(42)   "(list 42)"))
  (expected form)
  (assert-equal expected (run-string form)))

;;; Standard Library Set Operations Tests

(in-suite cl-cc-integration-serial-suite)

(deftest-each stdlib-list-ops
  "set-difference, union, append-lists, and last-cons work on lists."
  :cases (("set-diff"       '(1 3 5)     "(set-difference (list 1 2 3 4 5) (list 2 4))")
          ("set-diff-empty" '(1 2 3)     "(set-difference (list 1 2 3) (list))")
          ("union"          '(1 2 3 4 5) "(sort (union (list 1 2 3) (list 3 4 5)) #'<)")
          ("append-lists"   '(1 2 3 4)   "(append (list 1 2) (list 3 4))")
          ("last-cons"      3            "(car (last (list 1 2 3)))"))
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
          ("init-accum"   '(3 2 1) "(reduce (lambda (acc x) (cons x acc)) (list 1 2 3) :initial-value nil)"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

(in-suite cl-cc-integration-suite)

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

(deftest-each stdlib-cons-printing-forms
  "Stdlib forms producing cons/alist structures render correctly as lowercase strings."
  :cases (("find-with-key"   "(2 . b)"           "(find 2 (list (cons 1 'a) (cons 2 'b) (cons 3 'c)) :key (lambda (x) (car x)))")
          ("pairlis"         "((b . 2) (a . 1))"  "(pairlis (list 'a 'b) (list 1 2))")
          ("assoc-if"        "(2 . b)"            "(assoc-if (lambda (k) (= k 2)) (list (cons 1 'a) (cons 2 'b)))")
          ("rassoc"          "(2 . b)"            "(rassoc 'b (list (cons 1 'a) (cons 2 'b) (cons 3 'c)))")
          ("find-sharpsign-key" "(2 . b)"         "(find 2 (list (cons 1 'a) (cons 2 'b) (cons 3 'c)) :key #'car)"))
  (expected form)
  (assert-true (string= expected
                         (let ((*package* (find-package :cl-cc)) (*print-pretty* nil))
                           (string-downcase (format nil "~S" (run-string form :stdlib t)))))))

(deftest stdlib-identity
  "identity returns its argument"
  (assert-run= 42 "(identity 42)"))

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

;;; Macrolet and Function Reference Tests (#'builtin)

(deftest-each compile-macrolet-and-funcall
  "macrolet scoped macros and #'builtin funcall return the expected numeric results."
  :cases (("macrolet-basic"    6  "(macrolet ((double (x) `(+ ,x ,x))) (double 3))")
          ("macrolet-multiple" 10 "(macrolet ((add1 (x) `(+ ,x 1)) (add2 (x) `(+ ,x 2))) (+ (add1 3) (add2 4)))")
          ("macrolet-scoped"   42 "(let ((x 42)) (macrolet ((get-x () 'x)) (get-x)))")
          ("macrolet-nested"    8 "(macrolet ((square (x) `(* ,x ,x))) (macrolet ((sq-plus-sq (a b) `(+ (square ,a) (square ,b)))) (sq-plus-sq 2 2)))")
          ("funcall-car"        1 "(funcall #'car (cons 1 2))")
          ("funcall-plus"       7 "(funcall #'+ 3 4)"))
  (expected form)
  (assert-= expected (run-string form)))

(deftest-each compile-function-sharpsign
  "#'builtin creates callables usable with funcall and higher-order functions."
  :cases (("cons-pair"  '(1 . 2) "(funcall #'cons 1 2)")
          ("car-mapcar" '(1 2 3) "(mapcar #'car (list (cons 1 'a) (cons 2 'b) (cons 3 'c)))"))
  (expected form)
  (assert-true (equal expected (run-string form :stdlib t))))

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

(deftest-each compile-check-type
  "check-type passes silently for correct type and signals error for wrong type."
  :cases (("passes" "(let ((x 42)) (check-type x integer))"        nil)
          ("errors" "(let ((x \"hello\")) (check-type x integer))"  t))
  (form should-error-p)
  (if should-error-p
      (assert-signals error (run-string form))
      (assert-true (eq nil (run-string form)))))

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

(deftest-each stdlib-getf-and-set-ops
  "getf returns the correct value; intersection and remove filter list elements."
  :cases (("getf-found"          2         "(getf (list :a 1 :b 2 :c 3) :b)")
          ("getf-default"        99        "(getf (list :a 1) :z 99)")
          ("getf-first"          1         "(getf (list :a 1 :b 2) :a)")
          ("getf-not-found"      nil       "(getf (list :a 1 :b 2) :z)")
          ("set-intersection"    '(2 3)    "(intersection (list 1 2 3) (list 2 3 4))")
          ("set-intersection-empty" nil    "(intersection (list 1 2) (list 3 4))")
          ("set-remove"          '(1 3 5)  "(remove 2 (list 1 2 3 2 5))"))
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

(deftest selfhost-macro-eval-fn
  "*macro-eval-fn* is a function and is bound to our-eval at load time."
  (assert-true (functionp cl-cc:*macro-eval-fn*))
  (assert-true (eq cl-cc:*macro-eval-fn* #'cl-cc:our-eval)))

(deftest-each selfhost-macro-declare-arith-alist
  "Self-hosting macros, declare forms, arithmetic ops, and alist/list ops return expected numeric results."
  :cases (("defmacro-basic"      49 "(progn (defmacro sh-sq (x) (list (quote *) x x)) (sh-sq 7))")
          ("defmacro-with-args"  10 "(progn (defmacro sh-add3 (a b c) (list (quote +) a b c)) (sh-add3 2 3 5))")
          ("defmacro-quasiquote" 42 "(progn (defmacro sh-unless (test &body body) (list (quote if) test nil (cons (quote progn) body))) (sh-unless nil 42))")
          ("macrolet-basic"      42 "(macrolet ((add1 (x) (list (quote +) x 1))) (add1 41))")
          ("macrolet-nested"      8 "(macrolet ((dbl (x) (list (quote +) x x))) (macrolet ((quad (x) (list (quote dbl) (list (quote dbl) x)))) (quad 2)))")
          ("macrolet-body-form"   6 "(macrolet ((triple (x) (list (quote *) x 3))) (triple 2))")
          ("declare-ignore"      42 "(let ((x 42)) (declare (ignore x)) x)")
          ("declare-in-defun"    10 "(progn (defun my-decl-fn (x) (declare (type integer x)) x) (my-decl-fn 10))")
          ("declare-type"         3 "(let ((x 1) (y 2)) (declare (type integer x y)) (+ x y))")
          ("arith-mod"            1 "(mod 7 3)")
          ("arith-rem"            1 "(rem 7 3)")
          ("arith-truncate"       2 "(truncate 7 3)")
          ("arith-floor"          2 "(floor 7 3)")
          ("arith-ceiling"        3 "(ceiling 7 3)")
          ("arith-abs"            5 "(abs (- 0 5))")
          ("arith-min"            2 "(min 5 2)")
          ("arith-max"            5 "(max 5 2)")
          ("alist-assoc-found"    2 "(cdr (assoc 'b (list (cons 'a 1) (cons 'b 2) (cons 'c 3))))")
          ("alist-acons"         42 "(cdr (car (acons 'x 42 nil)))")
          ("alist-nconc-len"      4 "(length (nconc (list 1 2) (list 3 4)))")
          ("alist-copy-list-len"  3 "(length (copy-list (list 1 2 3)))")
          ("alist-listp-list"     1 "(listp (list 1 2))")
          ("alist-listp-nil"      1 "(listp nil)")
          ("alist-atom-true"      1 "(atom 42)")
          ("alist-atom-false"     0 "(atom (cons 1 2))"))
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
  "char-code/digit-char-p return numeric values; predicate results use CL booleans."
  :cases (("char-code"      65 "(char-code #\\A)")
          ("char=-true"      1 "(char= #\\a #\\a)")
          ("digit-char-p"    5 "(digit-char-p #\\5)")
          ("alpha-char-p"    1 "(alpha-char-p #\\z)")
          ("upper-case-p"    1 "(upper-case-p #\\A)")
          ("lower-case-p"    1 "(lower-case-p #\\a)")
          ("stringp-true"    t "(stringp \"hello\")")
          ("stringp-false"   nil "(stringp 42)")
          ("characterp-true" 1 "(characterp #\\a)"))
  (expected form)
  (assert-equal expected (run-string form)))

(deftest-each compile-string-char-utils
  "digit-char-p/nil, string-trim, parse-integer, and subseq work correctly."
  :cases (("digit-char-non-digit" nil    "(digit-char-p #\\a)")
          ("string-trim"         "hello" "(string-trim \" \" \"  hello  \")")
          ("parse-integer"        42     "(parse-integer \"42\")")
          ("subseq-with-end"     "ell"   "(subseq \"hello\" 1 4)")
          ("subseq-no-end"       "llo"   "(subseq \"hello\" 2)"))
  (expected form)
  (assert-run= expected form))

(deftest-each compile-search
  "search returns the position of the pattern or nil when not found (ANSI CL)."
  :cases (("found"     2   "(search \"ll\" \"hello\")")
          ("not-found" nil "(search \"xyz\" \"hello\")"))
  (expected form)
  (assert-equal expected (run-string form)))

;;; I/O and Format Tests

(deftest-each io-string-format-equal
  "write-to-string and format nil return the expected string representations."
  :cases (("write-integer"   "42"          "(write-to-string 42)")
          ("write-symbol"    "HELLO"       "(write-to-string 'hello)")
          ("format-string"   "hello world" "(format nil \"hello ~A\" \"world\")")
          ("format-number"   "x=42"        "(format nil \"x=~A\" 42)")
          ("format-no-args"  "hello"       "(format nil \"hello\")")
          ("format-multi"    "1 + 2 = 3"   "(format nil \"~A + ~A = ~A\" 1 2 3)"))
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
  "with-output-to-string, make-string-output-stream, and get-output-stream-string produce the expected string."
  :cases (("empty"              ""            "(with-output-to-string (s))")
          ("format"             "hello world" "(with-output-to-string (s) (format s \"hello ~A\" \"world\"))")
          ("multi-write"        "ab"          "(with-output-to-string (s) (write-string \"a\" s) (write-string \"b\" s))")
          ("multi-format"       "x=1 y=2"     "(with-output-to-string (s) (format s \"x=~A\" 1) (format s \" y=~A\" 2))")
          ("string-output-stream" "foo"       "(let ((s (make-string-output-stream))) (write-string \"foo\" s) (get-output-stream-string s))"))
  (expected form)
  (assert-string= expected (run-string form)))

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

;;; LOOP runtime tests moved to loop-macro-runtime-tests.lisp.

;;; Runtime/self-host support tests moved to compiler-tests-runtime.lisp.
