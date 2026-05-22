;;;; tests/native-advanced-evidence-tests.lisp
;;;; Minimal evidence-suite anchor required by cl-cc-test.asd.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest native-advanced-evidence-suite-loads
  "Evidence test module is present so the native-advanced-evidence ASDF module loads."
  (assert-true t))
