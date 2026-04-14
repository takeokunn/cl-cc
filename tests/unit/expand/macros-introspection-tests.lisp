;;;; tests/unit/expand/macros-introspection-tests.lisp — Introspection helper tests

(in-package :cl-cc/test)

(defsuite macros-introspection-suite
  :description "Introspection helper unit tests"
  :parent cl-cc-unit-suite)

(in-suite macros-introspection-suite)

(deftest macros-introspection-equalp-expands
  "equalp expands into the recursive labels-based comparer."
  (assert-eq 'labels (car (our-macroexpand-1 '(equalp 1 1)))))

(deftest macros-introspection-implementation-stubs-expand
  "Implementation-identification stubs expand to quoted constants."
  (assert-equal "cl-cc" (our-macroexpand-1 '(lisp-implementation-type)))
  (assert-equal "unknown" (our-macroexpand-1 '(machine-type)))
  (assert-eq 'functionp (car (our-macroexpand-1 '(compiled-function-p x)))))

(deftest macros-introspection-proclaim-expands
  "proclaim is a stub that expands to progn."
  (assert-eq 'progn (car (our-macroexpand-1 '(proclaim (special x))))))
