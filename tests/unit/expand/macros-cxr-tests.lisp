;;;; tests/unit/expand/macros-cxr-tests.lisp — CXR accessor macro tests

(in-package :cl-cc/test)

(defsuite macros-cxr-suite
  :description "CXR accessor macro tests"
  :parent cl-cc-suite)

(in-suite macros-cxr-suite)

(deftest macros-cxr-cadr-expands
  "CADR expands to CAR of CDR."
  (assert-equal '(car (cdr x)) (our-macroexpand-1 '(cadr x))))

(deftest macros-cxr-caddr-and-cdddr-expand
  "CADDR and CDDDR expand through the generated accessor registry."
  (assert-equal '(car (cdr (cdr x))) (our-macroexpand-1 '(caddr x)))
  (assert-equal '(cdr (cdr (cdr x))) (our-macroexpand-1 '(cdddr x))))
