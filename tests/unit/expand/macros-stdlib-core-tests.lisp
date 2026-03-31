;;;; tests/unit/expand/macros-stdlib-core-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib.lisp

(in-package :cl-cc/test)

(defsuite macros-stdlib-core-suite
  :description "Tests for macros-stdlib.lisp: core arithmetic and return forms"
  :parent cl-cc-suite)

(in-suite macros-stdlib-core-suite)

(deftest-each 1+-1--expansion
  "1+ and 1- are shorthand for (+ n 1) and (- n 1)."
  :cases (("1+" '(1+ n) '(+ n 1))
          ("1-" '(1- n) '(- n 1)))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))

(deftest signum-expansion
  "SIGNUM wraps n in a LET (avoids double evaluation) and dispatches via COND."
  (let* ((result (our-macroexpand-1 '(signum n)))
         (body   (caddr result)))
    (assert-eq (car result) 'let)
    (assert-eq (car body)   'cond)))

(deftest-each return-expansion
  "return expands to (return-from nil ...) with optional value."
  :cases (("with-value" '(return v)  '(return-from nil v))
          ("no-value"   '(return)    '(return-from nil nil)))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))
