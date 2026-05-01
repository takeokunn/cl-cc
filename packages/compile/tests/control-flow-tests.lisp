(in-package :cl-cc/test)

(defsuite control-flow-tests
  :description "Tests for control flow forms: block, return-from, tagbody, go, catch, throw, unwind-protect, setq, quote, the."
  :parent cl-cc-integration-suite)

(in-suite control-flow-tests)

(defun is-compile-string (source &key (target :x86_64))
  "Helper: verify that SOURCE compiles without error."
  (handler-case
      (progn (compile-string source :target target) t)
    (error () nil)))

;; ----------------------------------------------------------------------------
;; setq tests
;; ----------------------------------------------------------------------------

(deftest-each setq-forms
  "setq assignment compiles and updates variable bindings correctly."
  :cases (("basic"    "(let ((x 1)) (setq x 42) x)"              42)
          ("multiple" "(let ((x 1) (y 2)) (setq x (+ x y)) x)"    3))
  (source expected)
  (assert-true (is-compile-string source :target :vm))
  (assert-= expected (run-string source)))

;; ----------------------------------------------------------------------------
;; quote tests
;; ----------------------------------------------------------------------------

(deftest quote-integer
  (assert-true (is-compile-string "(quote 42)" :target :x86_64))
  (let ((result (run-string "(quote 42)")))
    (assert-= result 42)))

(deftest-each quote-compile-forms
  "Symbol, list, and nested-list quote forms compile without error."
  :cases (("symbol" "(quote foo)")
          ("list"   "(quote (1 2 3))")
          ("nested" "(quote (a (b c) d))"))
  (source)
  (assert-true (is-compile-string source :target :x86_64)))

;; ----------------------------------------------------------------------------
;; the tests
;; ----------------------------------------------------------------------------

(deftest-each the-forms
  "The type annotation forms compile and evaluate correctly."
  :cases (("integer"       "(the integer 42)"       42)
          ("in-expression" "(the integer (+ 1 2))"   3))
  (source expected)
  (assert-true (is-compile-string source :target :vm))
  (assert-= expected (run-string source)))

(deftest-each boolean-predicate-branches
  "Numeric predicate results still behave as false in branch forms."
  :cases (("if-false-branch"  20 "(if (= 1 2) 10 20)")
          ("cond-second-arm"  42 "(cond ((= 1 2) 10) ((= 2 2) 42) (t 0))")
          ("labels-base-case"  1 "(labels ((f (n) (if (= n 0) 1 (f (- n 1))))) (f 0))"))
  (expected source)
  (assert-= expected (run-string source)))

;; ----------------------------------------------------------------------------
;; block/return-from tests
;; ----------------------------------------------------------------------------

(deftest-each block-forms
  "Block normal return, early return-from, and nested blocks evaluate correctly."
  :cases (("normal-return" "(block foo 42)"                                             42)
          ("early-return"  "(block foo (return-from foo 10) 20)"                        10)
          ("nested"        "(block outer (block inner (return-from inner 5) 10) 20)"    20))
  (source expected)
  (assert-true (is-compile-string source :target :x86_64))
  (assert-= expected (run-string source)))

;; ----------------------------------------------------------------------------
;; tagbody/go tests
;; ----------------------------------------------------------------------------

(deftest tagbody-simple
  (assert-true (is-compile-string "(tagbody start (print 1) (go end) middle (print 2) end (print 3))" :target :vm)))

(deftest tagbody-preserves-following-body-forms
  "A tagbody used as a non-final form must not swallow the following body forms."
  (assert-= 6 (run-string "(let ((acc 6)) (tagbody start (go end) end) acc)")))

;; ----------------------------------------------------------------------------
;; catch/throw tests
;; ----------------------------------------------------------------------------

(deftest-each compile-catch-forms
  "catch compiles and evaluates to its body value."
  :cases (("basic"     42 "(catch 'foo 42)")
          ("with-throw" 99 "(catch 'done (throw 'done 99) 42)"))
  (expected source)
  (assert-true (is-compile-string source :target :vm))
  (assert-= expected (run-string source)))

(deftest-each compile-nested-catch
  "Nested catch forms route throws to the matching tag."
  :cases (("inner-tag" 10 "(catch 'outer (catch 'inner (throw 'inner 10)))")
          ("outer-tag" 20 "(catch 'outer (catch 'inner (throw 'outer 20)))"))
  (expected source)
  (assert-= expected (run-string source)))

(deftest-each compile-unwind-protect
  "unwind-protect compiles and returns its protected form's value."
  :cases (("primary-value" 42 "(unwind-protect 42 (print 0))"))
  (expected source)
  (assert-true (is-compile-string source :target :vm))
  (assert-= expected (run-string source)))

(deftest-each compile-multiple-value-prog1
  "multiple-value-prog1 compiles and returns its first form's primary value."
  :cases (("primary-value" 42 "(multiple-value-prog1 42 (print 1) (print 2))"))
  (expected source)
  (assert-true (is-compile-string source :target :vm))
  (assert-= expected (run-string source)))

;; ----------------------------------------------------------------------------
;; Run control flow tests
;; ----------------------------------------------------------------------------

(defun run-control-flow-tests ()
  (run-suite 'control-flow-tests))
