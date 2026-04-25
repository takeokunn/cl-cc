;;;; tests/unit/expand/macros-introspection-tests.lisp — Introspection helper tests

(in-package :cl-cc/test)

(defsuite macros-introspection-suite
  :description "Introspection helper unit tests"
  :parent cl-cc-unit-suite)

(in-suite macros-introspection-suite)

(deftest macros-introspection-equalp-expands
  "equalp expands into the recursive labels-based comparer."
  (assert-eq 'labels (car (our-macroexpand-1 '(equalp 1 1)))))
