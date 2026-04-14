;;;; tests/unit/vm/package-tests.lisp — package export smoke tests

(in-package :cl-cc/test)

(defsuite package-suite
  :description "Package export smoke tests"
  :parent cl-cc-unit-suite)

(in-suite package-suite)

(deftest cl-cc-package-exists
  "The cl-cc package should be available at load time."
  (assert-true (find-package :cl-cc)))

(deftest cl-cc-package-exports-representatives
  "Representative exports stay external in the package surface."
  (dolist (name '("CST-NODE" "RUN-STRING" "QUERY-ONE" "OUR-MACROEXPAND-ALL" "AST-INT"))
    (multiple-value-bind (sym status)
        (find-symbol name :cl-cc)
      (assert-true sym)
      (assert-eq :external status))))
