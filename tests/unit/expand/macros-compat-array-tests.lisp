;;;; tests/unit/expand/macros-compat-array-tests.lisp

(in-package :cl-cc/test)

(defsuite macros-compat-array-suite
  :description "Tests for array wrappers in macros-compat.lisp"
  :parent cl-cc-suite)

(in-suite macros-compat-array-suite)

(deftest-each array-predicate-stub-expansion
  "adjustable-array-p and array-has-fill-pointer-p are LET stubs returning T and NIL respectively."
  :cases (("adjustable-array-p"       '(adjustable-array-p arr)       t)
          ("array-has-fill-pointer-p" '(array-has-fill-pointer-p arr) nil))
  (form expected-return)
  (let* ((result (our-macroexpand-1 form))
         (body   (cddr result)))
    (assert-eq 'let (car result))
    (assert-equal expected-return (car (last body)))))
