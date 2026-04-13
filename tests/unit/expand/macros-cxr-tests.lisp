;;;; tests/unit/expand/macros-cxr-tests.lisp — CXR accessor macro tests

(in-package :cl-cc/test)

(defsuite macros-cxr-suite
  :description "CXR accessor macro tests"
  :parent cl-cc-suite)

(in-suite macros-cxr-suite)

(deftest-each macros-cxr-expansion
  "CXR accessor macros expand to the equivalent nested CAR/CDR calls."
  :cases (("cadr"  '(cadr x)  '(car (cdr x)))
          ("caddr" '(caddr x) '(car (cdr (cdr x))))
          ("cdddr" '(cdddr x) '(cdr (cdr (cdr x)))))
  (form expected)
  (assert-equal expected (our-macroexpand-1 form)))
