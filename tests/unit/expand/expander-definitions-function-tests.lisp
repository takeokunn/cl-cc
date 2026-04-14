;;;; tests/unit/expand/expander-definitions-function-tests.lisp — Definition-form function tests

(in-package :cl-cc/test)

(defsuite expander-definitions-function-suite :description "Definition-form function unit tests"
  :parent cl-cc-unit-suite)


(in-suite expander-definitions-function-suite)
(deftest-each expander-defun-lambda-preserve-structure
  "Plain defun and lambda preserve their structure after macro expansion."
  :cases (("defun"  '(defun triple (x) (* 3 x)) 'defun  2 '(x))
          ("lambda" '(lambda (x y) (+ x y))     'lambda 1 '(x y)))
  (form expected-head params-pos expected-params)
  (let ((result (cl-cc::compiler-macroexpand-all form)))
    (assert-eq    expected-head   (car result))
    (assert-equal expected-params (nth params-pos result))))

(deftest expander-lambda-optional-default-expanded
  "compiler-macroexpand-all: lambda &optional default value is expanded."
  (let ((result (cl-cc::compiler-macroexpand-all
                 '(lambda (x &optional (y (+ 1 1))) (+ x y)))))
    (assert-eq 'lambda (car result))
    (let* ((params (second result))
           (opt-param (third params)))
      (assert-equal 'y (first opt-param))
      (assert-equal '(+ 1 1) (second opt-param)))))
