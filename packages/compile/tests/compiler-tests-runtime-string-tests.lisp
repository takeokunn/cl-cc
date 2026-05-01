;;;; compiler-tests-runtime-string-tests.lisp — String/Symbol, macro expansion, list ops, handler-case, hash-table, defmacro, stdlib HOF
(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

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
  (assert-equal expected (not (null (run-string form)))))

(deftest-compile compile-string-ops-and-type-predicates
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
  )

;;; Macro Expansion in Compiler Tests

(deftest-compile compile-control-macros-valued
  "cond/when/unless/and/or macros return the expected values."
  :cases (("cond-match"    42  "(cond ((= 1 2) 10) ((= 1 1) 42) (t 0))")
          ("cond-default"   0  "(cond ((= 1 2) 10) ((= 2 3) 20) (t 0))")
          ("when-true"     42  "(when (= 1 1) 42)")
          ("unless-false"  99  "(unless (= 1 2) 99)")
          ("and-all"        3  "(and 1 2 3)")
          ("or-find"        5  "(or nil nil 5)")
          ("or-first"       1  "(or 1 2 3)")))

(deftest-each compile-control-macros-nil
  "cond/when/unless/and/or return nil in the false cases."
  :cases (("when-false"   "(when (= 1 2) 42)")
          ("unless-true"  "(unless (= 1 1) 99)")
          ("and-short"    "(and 1 nil 3)")
          ("or-all-nil"   "(or nil nil nil)"))
  (form)
  (assert-run-false form))

(deftest-compile compile-t-nil-constants
  "t and nil are recognized as constants; not inverts truthiness."
  :cases (("t-is-true"    t   "t")
          ("not-nil"      t   "(not nil)")
          ("nil-is-nil"   nil "nil")
          ("not-1"        nil "(not 1)")
          ("not-t"        nil "(not t)")))

;;; List Operation Builtin Tests

(deftest-compile compile-list-ops
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
  )

(deftest-compile compile-list-builtins
  "append/reverse builtins work on lists."
  :cases (("append-len" 4 "(length (append (list 1 2) (list 3 4)))")
          ("reverse-first" 3 "(first (reverse (list 1 2 3)))")))

;;; Handler-Case Tests

(deftest-compile compile-handler-case-return-value
  "handler-case returns the correct numeric value in various scenarios."
  :cases (("no-error"      42 "(handler-case 42 (error (e) 0))")
          ("catches-error" 99 "(handler-case (error \"boom\") (error (e) 99))")
          ("arithmetic"    10 "(handler-case (+ 3 7) (error (e) 0))")
          ("handler-body" 100 "(handler-case (error \"x\") (error (e) (* 10 10)))")
          ("nested"         1 "(handler-case (handler-case (error \"inner\") (error (e) 1)) (error (e) 2))"))
  )

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

(deftest-compile compile-defmacro-numeric
  "defmacro defines macros that expand and evaluate to the correct numeric result."
  :cases (("basic"      42  "(defmacro my-const () 42) (my-const)")
          ("with-args"  10  "(defmacro my-dbl (x) (list '+ x x)) (my-dbl 5)")
          ("quasiquote" 15  "(defmacro my-add3 (a b c) `(+ ,a (+ ,b ,c))) (my-add3 3 5 7)")
          ("in-let"     100 "(defmacro my-square (x) `(* ,x ,x)) (let ((n 10)) (my-square n))")))

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

;;; Typep and Destructuring Tests

(deftest-compile compile-typep-destructuring-iteration
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
  )

;;; Runtime Eval and Setf Variable Tests

(deftest-compile compile-eval-and-setf-variable
  "eval and setf on plain variables return the expected numeric results."
  :cases (("eval-constant"    42 "(eval 42)")
          ("eval-quoted"       3 "(eval '(+ 1 2))")
          ("eval-nested"      21 "(eval '(* (+ 1 2) (+ 3 4)))")
          ("eval-let-form"    15 "(eval '(let ((x 10)) (+ x 5)))")
          ("eval-constructed" 30 "(let ((op '+) (a 10) (b 20)) (eval (list op a b)))")
          ("setf-plain"       10 "(let ((x 0)) (setf x 10) x)")
          ("setf-increment"    3 "(let ((counter 0)) (setf counter (+ counter 1)) (setf counter (+ counter 1)) (setf counter (+ counter 1)) counter)"))
  )

;;; FR-603: (setf (values ...)) assigns to multiple places

(deftest-compile compile-setf-values
  "(setf (values ...)) assigns multiple values to individual places."
  :cases (("reads-a"    10 "(let ((a 0) (b 0)) (setf (values a b) (values 10 20)) a)")
          ("reads-b"    20 "(let ((a 0) (b 0)) (setf (values a b) (values 10 20)) b)")
          ("reads-both" 30 "(let ((x 0) (y 0)) (setf (values x y) (values 10 20)) (+ x y))")))

(deftest-compile compile-obsolete-set
  "SET is available as a builtin and assigns through SYMBOL-VALUE without stdlib loading."
  :cases (("returns-value" 42 "(progn (defparameter fr586-set-var 0) (set 'fr586-set-var 42))")
          ("reads-binding" 42 "(progn (defparameter fr586-set-var 0) (set 'fr586-set-var 42) (symbol-value 'fr586-set-var))")
          ("updates-value" 99 "(progn (defparameter fr586-set-var 0) (set 'fr586-set-var 42) (set 'fr586-set-var 99) (symbol-value 'fr586-set-var))")))
