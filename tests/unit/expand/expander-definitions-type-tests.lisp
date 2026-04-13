;;;; tests/unit/expand/expander-definitions-type-tests.lisp — Definition-form type tests

(in-package :cl-cc/test)

(defsuite expander-definitions-type-suite :description "Definition-form type unit tests"
  :parent cl-cc-suite)


(in-suite expander-definitions-type-suite)
(deftest expander-deftype-registers-alias
  "compiler-macroexpand-all: (deftype foo fixnum) registers the alias and returns (quote foo)."
  (let ((result (cl-cc::compiler-macroexpand-all '(deftype my-index-type fixnum))))
    (assert-eq 'quote (car result))
    (assert-eq 'my-index-type (second result))))
