;;;; tests/unit/expand/expander-definitions-forms-tests.lisp — Definition-form expander tests

(in-package :cl-cc/test)

(defsuite expander-definitions-forms-suite
  :description "Definition-form expander unit tests"
  :parent cl-cc-unit-suite)

(in-suite expander-definitions-forms-suite)

(deftest expander-defun-expands-body
  "defun expansion keeps the definition head and expands the body."
  (let ((result (cl-cc::compiler-macroexpand-all '(defun foo (x) (1+ x)))))
    (assert-eq 'defun (car result))
    (assert-eq 'foo (second result))))

(deftest expander-lambda-expands-typed-params
  "lambda expansion dispatches through the typed/untyped helper path."
  (let ((result (format nil "~S"
                        (cl-cc::compiler-macroexpand-all
                         '(lambda ((x integer)) (1+ x))))))
    (assert-true (search "LAMBDA" result))
    (assert-true (search "TYPEP" result))))

(deftest expander-defclass-and-deftype-expands
  "defclass registers slots and deftype returns the quoted type name."
  (let ((class-result (cl-cc::compiler-macroexpand-all
                       '(defclass sample () ((slot :initform (1+ 2))))))
        (type-result (cl-cc::compiler-macroexpand-all
                      '(deftype sample-type () 'integer))))
    (assert-eq 'defclass (car class-result))
    (assert-eq 'quote (car type-result))
    (assert-eq 'sample-type (second type-result))))
