;;;; tests/unit/type/checker-tests.lisp — checker module boundary tests

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest checker-interface-ready
  "checker.lisp exposes a testable readiness predicate for the bidirectional checker boundary."
  (assert-true (cl-cc/type::checker-interface-ready-p)))

(deftest checker-skolem-constructor-available
  "checker boundary still exposes the legacy skolem constructor path documented by checker.lisp."
  (let ((sk (cl-cc/type:make-type-skolem 'alpha)))
    (assert-true (cl-cc/type:type-skolem-p sk))
    (assert-eq 'alpha (cl-cc/type:type-skolem-name sk))))
