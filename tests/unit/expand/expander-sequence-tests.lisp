;;;; tests/unit/expand/expander-sequence-tests.lisp — Sequence expander tests

(in-package :cl-cc/test)

(defsuite expander-sequence-suite
  :description "Sequence expander unit tests"
  :parent cl-cc-suite)

(in-suite expander-sequence-suite)

(deftest expander-sequence-mapcar-expands-to-let
  "mapcar expands to a loop-based LET form."
  (assert-eq 'let (car (cl-cc::compiler-macroexpand-all '(mapcar #'1+ '(1 2))))))

(deftest expander-sequence-every-and-some-expand
  "every and some expand to loop-based LET forms for multi-list dispatch."
  (assert-eq 'let (car (cl-cc::compiler-macroexpand-all '(every #'evenp '(1 2 3)))))
  (assert-eq 'let (car (cl-cc::compiler-macroexpand-all '(some #'oddp '(1 2 3))))))
