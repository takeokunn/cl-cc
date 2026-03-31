;;;; tests/unit/expand/expander-tail-tests.lisp — Tail-form expander tests

(in-package :cl-cc/test)

(defsuite expander-tail-suite
  :description "Tail-form expander unit tests"
  :parent cl-cc-suite)

(in-suite expander-tail-suite)

(deftest expander-tail-round-normalizes-arity
  "Single-argument rounding forms expand to a two-argument call."
  (let ((result (cl-cc::compiler-macroexpand-all '(round x))))
    (assert-eq 'round (car result))
    (assert-equal 1 (third result))))

(deftest expander-tail-error-and-warn-expand-format-strings
  "error and warn with format args expand through FORMAT."
  (let ((result (cl-cc::compiler-macroexpand-all '(error "oops" 1))))
    (assert-eq 'error (car result))
    (assert-eq 'format (car (second result)))))
