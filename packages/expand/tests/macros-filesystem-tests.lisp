;;;; tests/unit/expand/macros-filesystem-tests.lisp
;;;; Coverage tests for src/expand/macros-filesystem.lisp

(in-package :cl-cc/test)

(defsuite macros-filesystem-suite
  :description "Tests for macros-filesystem.lisp"
  :parent cl-cc-unit-suite)

(in-suite macros-filesystem-suite)

(deftest time-expansion
  "TIME expands to a LET* that measures elapsed universal time."
  (let ((result (our-macroexpand-1 '(time (+ 1 2)))))
    (assert-eq 'let* (car result))
    (assert-eq 'get-universal-time (car (second (first (second result)))))
    (assert-eq 'format (car (third result)))))
