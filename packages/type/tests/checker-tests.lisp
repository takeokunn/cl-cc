;;;; tests/unit/type/checker-tests.lisp — checker module boundary tests

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest checker-interface-ready
  "checker.lisp exposes a testable readiness predicate for the bidirectional checker boundary."
  (assert-true (cl-cc/type::checker-interface-ready-p)))

(deftest checker-rigid-constructor-available
  "checker boundary exposes the canonical rigid constructor path."
  (let ((sk (cl-cc/type:fresh-rigid-var 'alpha)))
    (assert-true (cl-cc/type:type-rigid-p sk))
    (assert-eq 'alpha (cl-cc/type:type-rigid-name sk))))
