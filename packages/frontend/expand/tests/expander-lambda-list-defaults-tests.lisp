;;;; tests/unit/expand/expander-lambda-list-defaults-tests.lisp — Lambda-list default expander tests
;;;;
;;;; Tests for expand-lambda-list-defaults.

(in-package :cl-cc/test)

(defsuite expander-lambda-list-defaults-suite :description "Lambda-list default expander unit tests"
  :parent cl-cc-unit-suite)


(in-suite expander-lambda-list-defaults-suite)
(deftest lambda-list-no-defaults
  "expand-lambda-list-defaults passes through simple params."
  (assert-equal '(x y z) (cl-cc/expand::expand-lambda-list-defaults '(x y z))))

(deftest lambda-list-optional-with-default
  "expand-lambda-list-defaults expands optional default values."
  (let ((result (cl-cc/expand::expand-lambda-list-defaults '(x &optional (y 42)))))
    (assert-equal 'x (first result))
    (assert-equal '&optional (second result))
    ;; y's default should be expanded (42 stays as 42)
    (assert-true (consp (third result)))))
