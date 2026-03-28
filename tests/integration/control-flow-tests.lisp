(in-package :cl-cc/test)

(defsuite control-flow-tests
  :description "Tests for control flow forms: block, return-from, tagbody, go, catch, throw, unwind-protect, setq, quote, the."
  :parent cl-cc-suite)

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
  (assert-true (is-compile-string source :target :x86_64))
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
  (assert-true (is-compile-string source :target :x86_64))
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

;; ----------------------------------------------------------------------------
;; catch/throw tests
;; ----------------------------------------------------------------------------

(deftest control-flow-constructs
  "catch, unwind-protect, and multiple-value-prog1 compile and evaluate to their primary value."
  (assert-true (is-compile-string "(catch 'foo 42)" :target :vm))
  (assert-= 42 (run-string "(catch 'foo 42)"))
  ;; catch with throw
  (assert-= 99 (run-string "(catch 'done (throw 'done 99) 42)"))
  ;; nested catch — inner tag
  (assert-= 10 (run-string "(catch 'outer (catch 'inner (throw 'inner 10)))"))
  ;; nested catch — outer tag
  (assert-= 20 (run-string "(catch 'outer (catch 'inner (throw 'outer 20)))"))
  (assert-true (is-compile-string "(unwind-protect 42 (print 0))" :target :vm))
  (assert-= 42 (run-string "(unwind-protect 42 (print 0))"))
  (assert-true (is-compile-string "(multiple-value-prog1 42 (print 1) (print 2))" :target :vm))
  (assert-= 42 (run-string "(multiple-value-prog1 42 (print 1) (print 2))")))

;; ----------------------------------------------------------------------------
;; Run control flow tests
;; ----------------------------------------------------------------------------

(defun run-control-flow-tests ()
  (run-suite 'control-flow-tests))
