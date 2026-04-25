;;;; compiler-tests-stdlib-io.lisp — Self-hosting eval, string/char, I/O, HOF, array, sort, coerce tests
(in-package :cl-cc/test)

(in-suite cl-cc-integration-suite)

;;; Self-Hosting Eval Tests — verify macro expansion runs through our-eval

(deftest selfhost-macro-eval-fn
  "*macro-eval-fn* is a function and is bound to our-eval at load time."
  (assert-true (functionp cl-cc:*macro-eval-fn*))
  (assert-false (eq cl-cc:*macro-eval-fn* #'eval))
  (assert-eql 3 (funcall cl-cc:*macro-eval-fn* '(+ 1 2))))

(deftest-compile-each selfhost-macro-declare-arith-alist
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
  )

(deftest-compile-each compile-equal-ops
  "equal returns truthy for matching structures, NIL for different structures."
  :cases (("list-match"   t   "(equal (list 1 2 3) (list 1 2 3))")
          ("list-no-match" nil "(equal (list 1 2) (list 1 3))")
          ("subst-match"  t   "(equal (subst 'x 'a (list 'a 'b 'a)) (list 'x 'b 'x))"))
  )

(deftest-compile-each compile-assoc-and-string-coerce
  "assoc-miss returns nil; string coerces a symbol to its name string."
  :cases (("assoc-miss"    nil     "(assoc 'z (list (cons 'a 1) (cons 'b 2)))")
          ("string-coerce" "HELLO" "(string 'hello)"))
  )

;;; String/Character Builtin Tests

(deftest-compile-each compile-char-ops
  "char/code-char/char-upcase/char-downcase return the expected character."
  :cases (("char-access"   #\e "(char \"hello\" 1)")
          ("code-char"     #\A "(code-char 65)")
          ("char-upcase"   #\A "(char-upcase #\\a)")
          ("char-downcase" #\a "(char-downcase #\\A)"))
  )

(deftest-compile-each compile-char-numeric
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
  )

(deftest-compile-each compile-string-char-utils
  "digit-char-p/nil, string-trim, parse-integer, and subseq work correctly."
  :cases (("digit-char-non-digit" nil    "(digit-char-p #\\a)")
          ("string-trim"         "hello" "(string-trim \" \" \"  hello  \")")
          ("parse-integer"        42     "(parse-integer \"42\")")
          ("subseq-with-end"     "ell"   "(subseq \"hello\" 1 4)")
          ("subseq-no-end"       "llo"   "(subseq \"hello\" 2)"))
  )

(deftest-compile-each compile-search
  "search returns the position of the pattern or nil when not found (ANSI CL)."
  :cases (("found"     2   "(search \"ll\" \"hello\")")
          ("not-found" nil "(search \"xyz\" \"hello\")"))
  )

;;; I/O and Format Tests

(deftest-compile-each io-string-format-equal
  "write-to-string and format nil return the expected string representations."
  :cases (("write-integer"   "42"          "(write-to-string 42)")
          ("write-symbol"    "HELLO"       "(write-to-string 'hello)")
          ("format-string"   "hello world" "(format nil \"hello ~A\" \"world\")")
          ("format-number"   "x=42"        "(format nil \"x=~A\" 42)")
          ("format-no-args"  "hello"       "(format nil \"hello\")")
          ("format-multi"    "1 + 2 = 3"   "(format nil \"~A + ~A = ~A\" 1 2 3)"))
  )

(deftest-compile-each io-print-returns-value
  "princ/prin1/print each return the value they printed."
  :cases (("princ" 42 "(princ 42)")
          ("prin1" 42 "(prin1 42)")
          ("print" 42 "(print 42)")))

(deftest-compile-each io-returns-nil
  "terpri and (format t ...) both return nil."
  :cases (("terpri"   nil "(terpri)")
          ("format-t" nil "(format t \"hello\")")))

(deftest-compile-each io-format-directives
  "format directives for iteration, conditionals, and character output."
  :cases (("iteration"          "1, 2, 3" "(format nil \"~{~A~^, ~}\" (list 1 2 3))")
          ("conditional-index"  "one"     "(format nil \"~[zero~;one~;two~:;many~]\" 1)")
          ("conditional-default" "many"   "(format nil \"~[zero~;one~;two~:;many~]\" 99)"))
  :stdlib t)

(deftest io-write-char-basic
  "write-char outputs a character and returns it"
  (assert-true (equal #\A (run-string "(write-char #\\A)"))))

;;; Higher-Order Function Tests (require stdlib)

(deftest-compile-each stdlib-hof-list-result
  "HOFs return the correct list result when applied to lists."
  :cases (("mapcar"        '(2 4 6)   "(mapcar (lambda (x) (+ x x)) (list 1 2 3))")
          ("remove-if"     '(1 2)     "(remove-if (lambda (x) (> x 2)) (list 1 2 3 4 5))")
          ("remove-if-not" '(3 4 5)   "(remove-if-not (lambda (x) (> x 2)) (list 1 2 3 4 5))"))
  :stdlib t)

(deftest-compile-each stdlib-hof-numeric
  "HOFs return numeric results for reduce, find-if, count-if."
  :cases (("reduce"   10 "(reduce (lambda (a b) (+ a b)) (list 1 2 3 4))")
           ("find-if"   4 "(find-if (lambda (x) (> x 3)) (list 1 2 3 4 5))")
           ("count-if"  2 "(count-if (lambda (x) (> x 2)) (list 1 2 3 4))"))
  :stdlib t)

(deftest-each stdlib-hof-truthy
  "every/some return truthy values when the predicate matches."
  :cases (("every-all"  "(every (lambda (x) (> x 0)) (list 1 2 3))")
          ("some-found" "(some (lambda (x) (> x 2)) (list 1 2 3))"))
  (form)
  (assert-true (not (null (run-string form :stdlib t)))))

(deftest-compile-each stdlib-hof-nil
  "every/some/find-if/mapcar return nil when there is no match or empty input."
  :cases (("mapcar-empty"  nil "(mapcar (lambda (x) x) nil)")
           ("find-if-miss"  nil "(find-if (lambda (x) (> x 10)) (list 1 2 3))")
           ("every-fail"    nil "(every (lambda (x) (> x 2)) (list 1 2 3))")
           ("some-miss"     nil "(some (lambda (x) (> x 10)) (list 1 2 3))"))
  :stdlib t)

;;; With-Output-To-String Tests

(deftest-compile-each compile-with-output-to-string
  "with-output-to-string, make-string-output-stream, and get-output-stream-string produce the expected string."
  :cases (("empty"              ""            "(with-output-to-string (s))")
          ("format"             "hello world" "(with-output-to-string (s) (format s \"hello ~A\" \"world\"))")
          ("multi-write"        "ab"          "(with-output-to-string (s) (write-string \"a\" s) (write-string \"b\" s))")
          ("multi-format"       "x=1 y=2"     "(with-output-to-string (s) (format s \"x=~A\" 1) (format s \" y=~A\" 2))")
          ("string-output-stream" "foo"       "(let ((s (make-string-output-stream))) (write-string \"foo\" s) (get-output-stream-string s))"))
  )

;;; Array/Vector Tests

(deftest compile-make-array-basic
  "make-array creates an array"
  (let ((result (run-string "(make-array 5)")))
    (assert-true (vectorp result))
    (assert-= 5 (length result))))

(deftest-compile-each compile-array-numeric
  "aref/setf aref/vector-push-extend return the correct numeric values."
  :cases (("aref-init" 0  "(let ((a (make-array 3))) (aref a 0))")
          ("setf-aref" 42 "(let ((a (make-array 3))) (setf (aref a 1) 42) (aref a 1))")
          ("vec-push"  10 "(let ((v (make-array 0 :fill-pointer t :adjustable t))) (vector-push-extend 10 v) (aref v 0))"))
  )

(deftest-compile-each compile-vectorp
  "vectorp returns truthy for vectors and 0 for non-vectors."
  :cases (("vector"  t "(not (null (vectorp (make-array 3))))")
          ("non-vec" t "(eql 0 (vectorp 42))")))

;;; Sort Tests

(deftest-compile-each compile-sort
  "sort produces the correctly ordered list."
  :cases (("ascending"  '(1 1 2 3 4 5 6 9) "(sort (list 3 1 4 1 5 9 2 6) (lambda (a b) (< a b)))")
           ("descending" '(5 3 1)            "(sort (list 3 1 5) (lambda (a b) (> a b)))")
           ("single"     '(42)               "(sort (list 42) (lambda (a b) (< a b)))")
           ("empty"      nil                 "(sort nil (lambda (a b) (< a b)))"))
  :stdlib t)

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
