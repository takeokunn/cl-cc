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

(deftest setq-basic
  (assert-true (is-compile-string "(let ((x 1)) (setq x 42) x)" :target :x86_64))
  (let ((result (run-string "(let ((x 1)) (setq x 42) x)")))
    (assert-= result 42)))

(deftest setq-multiple
  (assert-true (is-compile-string "(let ((x 1) (y 2)) (setq x (+ x y)) x)" :target :x86_64))
  (let ((result (run-string "(let ((x 1) (y 2)) (setq x (+ x y)) x)")))
    (assert-= result 3)))

;; ----------------------------------------------------------------------------
;; quote tests
;; ----------------------------------------------------------------------------

(deftest quote-integer
  (assert-true (is-compile-string "(quote 42)" :target :x86_64))
  (let ((result (run-string "(quote 42)")))
    (assert-= result 42)))

(deftest quote-symbol
  (assert-true (is-compile-string "(quote foo)" :target :x86_64)))

(deftest quote-list
  (assert-true (is-compile-string "(quote (1 2 3))" :target :x86_64)))

(deftest quote-nested
  (assert-true (is-compile-string "(quote (a (b c) d))" :target :x86_64)))

;; ----------------------------------------------------------------------------
;; the tests
;; ----------------------------------------------------------------------------

(deftest the-integer
  (assert-true (is-compile-string "(the integer 42)" :target :x86_64))
  (let ((result (run-string "(the integer 42)")))
    (assert-= result 42)))

(deftest the-in-expression
  (assert-true (is-compile-string "(the integer (+ 1 2))" :target :x86_64))
  (let ((result (run-string "(the integer (+ 1 2))")))
    (assert-= result 3)))

;; ----------------------------------------------------------------------------
;; block/return-from tests
;; ----------------------------------------------------------------------------

(deftest block-normal-return
  (assert-true (is-compile-string "(block foo 42)" :target :x86_64))
  (let ((result (run-string "(block foo 42)")))
    (assert-= result 42)))

(deftest block-early-return
  (assert-true (is-compile-string "(block foo (return-from foo 10) 20)" :target :x86_64))
  (let ((result (run-string "(block foo (return-from foo 10) 20)")))
    (assert-= result 10)))

(deftest block-nested
  (assert-true (is-compile-string "(block outer (block inner (return-from inner 5) 10) 20)" :target :x86_64))
  (let ((result (run-string "(block outer (block inner (return-from inner 5) 10) 20)")))
    (assert-= result 20)))

;; ----------------------------------------------------------------------------
;; tagbody/go tests
;; ----------------------------------------------------------------------------

(deftest tagbody-simple
  (assert-true (is-compile-string "(tagbody start (print 1) (go end) middle (print 2) end (print 3))" :target :vm)))

;; ----------------------------------------------------------------------------
;; catch/throw tests
;; ----------------------------------------------------------------------------

(deftest catch-no-throw
  (assert-true (is-compile-string "(catch 'foo 42)" :target :x86_64))
  (let ((result (run-string "(catch 'foo 42)")))
    (assert-= result 42)))

(deftest unwind-protect-normal
  (assert-true (is-compile-string "(unwind-protect 42 (print 0))" :target :vm))
  (let ((result (run-string "(unwind-protect 42 (print 0))")))
    (assert-= result 42)))

(deftest multiple-value-prog1
  (assert-true (is-compile-string "(multiple-value-prog1 42 (print 1) (print 2))" :target :vm))
  (let ((result (run-string "(multiple-value-prog1 42 (print 1) (print 2))")))
    (assert-= result 42)))

;; ----------------------------------------------------------------------------
;; Run control flow tests
;; ----------------------------------------------------------------------------

(defun run-control-flow-tests ()
  (run-suite 'control-flow-tests))
