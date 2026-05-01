;;;; tests/unit/expand/expander-definitions-constant-tests.lisp — Definition-form constant tests

(in-package :cl-cc/test)

(defsuite expander-definitions-constant-suite :description "Definition-form constant unit tests"
  :parent cl-cc-unit-suite)


(in-suite expander-definitions-constant-suite)
(deftest expander-defconstant-to-defparameter
  "defconstant (with or without docstring) expands to defparameter."
  (let ((basic   (cl-cc/expand::compiler-macroexpand-all '(defconstant +my-const+ 42)))
        (with-doc (cl-cc/expand::compiler-macroexpand-all '(defconstant +pi+ 3.14159 "Pi constant"))))
    (assert-eq    'defparameter (car basic))
    (assert-equal 42            (third basic))
    (assert-eq    'defparameter (car with-doc))
    (assert-eq    '+pi+         (second with-doc))))
