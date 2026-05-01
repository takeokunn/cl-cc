;;;; tests/unit/expand/expander-sequence-tests.lisp — Sequence expander tests

(in-package :cl-cc/test)

(defsuite expander-sequence-suite
  :description "Sequence expander unit tests"
  :parent cl-cc-unit-suite)

(in-suite expander-sequence-suite)

(deftest-each expander-sequence-predicates-expand-to-let
  "mapcar, every, and some expand to loop-based LET forms."
  :cases (("mapcar" '(mapcar #'1+ '(1 2)))
          ("every"  '(every #'evenp '(1 2 3)))
          ("some"   '(some #'oddp '(1 2 3))))
  (form)
  (assert-eq 'let (car (cl-cc/expand::compiler-macroexpand-all form))))
