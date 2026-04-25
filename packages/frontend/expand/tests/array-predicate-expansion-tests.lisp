;;;; tests/unit/expand/array-predicate-expansion-tests.lisp

(in-package :cl-cc/test)

(defsuite array-predicate-expansion-suite
  :description "Tests for direct array predicate expansion"
  :parent cl-cc-unit-suite)

(in-suite array-predicate-expansion-suite)

(deftest-each array-predicate-remains-direct-call
  "Array capability predicates are no longer expanded through compatibility stubs."
  :cases (("adjustable-array-p" '(adjustable-array-p arr))
          ("array-has-fill-pointer-p" '(array-has-fill-pointer-p arr)))
  (form)
  (assert-no-expansion form))
