;;;; tests/unit/expand/macros-compat-array-tests.lisp

(in-package :cl-cc/test)

(defsuite macros-compat-array-suite
  :description "Tests for array wrappers in macros-compat.lisp"
  :parent cl-cc-suite)

(in-suite macros-compat-array-suite)

(deftest adjustable-array-p-expansion
  "ADJUSTABLE-ARRAY-P expands to a LET that returns T."
  (let* ((result (our-macroexpand-1 '(adjustable-array-p arr)))
         (body   (cddr result)))
    (assert-eq 'let (car result))
    (assert-eq t (car (last body)))))

(deftest array-has-fill-pointer-p-expansion
  "ARRAY-HAS-FILL-POINTER-P expands to a LET that returns NIL."
  (let* ((result (our-macroexpand-1 '(array-has-fill-pointer-p arr)))
         (body   (cddr result)))
    (assert-eq 'let (car result))
    (assert-equal nil (car (last body)))))
