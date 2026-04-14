;;;; tests/unit/expand/expander-control-helpers-tests.lisp — Control helper tests

(in-package :cl-cc/test)

(defsuite expander-control-helpers-suite
  :description "Control-form helper unit tests"
  :parent cl-cc-unit-suite)

(in-suite expander-control-helpers-suite)

(deftest expand-let-binding-expands-value
  "expand-let-binding macro-expands the value while preserving the binding name."
  (assert-equal '(x (+ 1 2)) (cl-cc::expand-let-binding '(x (+ 1 2)))))

(deftest expand-flet-labels-binding-expands-body
  "expand-flet-labels-binding expands only the body forms."
  (let ((result (cl-cc::expand-flet-labels-binding '(foo (x) (1+ x) (1- x)))))
    (assert-eq 'foo (first result))
    (assert-eq '(x) (second result))
    (assert-equal '((+ x 1) (- x 1)) (cddr result))))
