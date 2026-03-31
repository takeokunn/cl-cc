;;;; tests/unit/expand/expander-binding-tests.lisp — Binding helper tests

(in-package :cl-cc/test)

(defsuite expander-binding-suite :description "Binding helper unit tests")

(deftest let-binding-symbol-value
  "expand-let-binding expands the value form, preserves name."
  (let ((b (cl-cc::expand-let-binding '(x 42))))
    (assert-equal 'x (first b))
    (assert-equal 42 (second b))))

(deftest let-binding-bare-symbol
  "expand-let-binding passes through a bare symbol."
  (assert-equal 'x (cl-cc::expand-let-binding 'x)))

(deftest expand-flet-labels-binding
  "expand-flet-labels-binding: full form expands body; short form passes through."
  (let ((full  (cl-cc::expand-flet-labels-binding '(foo (x) (+ x 1))))
        (short (cl-cc::expand-flet-labels-binding '(foo (x)))))
    (assert-equal 'foo    (first full))
    (assert-equal '(x)   (second full))
    (assert-true (consp  (third full)))
    (assert-equal '(foo (x)) short)))
