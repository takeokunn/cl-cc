;;;; tests/unit/expand/macros-basic-list-tests.lisp
;;;; Coverage tests for src/expand/macros-basic.lisp

(in-package :cl-cc/test)

(defsuite macros-basic-list-suite
  :description "Tests for macros-basic.lisp: list"
  :parent cl-cc-suite)

(in-suite macros-basic-list-suite)

(deftest list-empty-is-nil
  "(list) expands to nil — no cons allocation at all"
  (assert-equal (our-macroexpand-1 '(list)) nil))

(deftest-each list-expands-to-nested-cons
  "LIST expands to right-associative nested cons cells terminating in nil."
  :cases (("one-element"    '(list x)     '(cons x nil))
          ("two-elements"   '(list a b)   '(cons a (cons b nil)))
          ("three-elements" '(list a b c) '(cons a (cons b (cons c nil))))
          ("literals"       '(list 1 2 3) '(cons 1 (cons 2 (cons 3 nil)))))
  (form expected)
  (assert-equal (our-macroexpand-1 form) expected))
