;;;; tests/unit/expand/macros-basic-check-type-tests.lisp
;;;; Coverage tests for src/expand/macros-basic.lisp

(in-package :cl-cc/test)

(defsuite macros-basic-check-type-suite
  :description "Tests for macros-basic.lisp: check-type"
  :parent cl-cc-unit-suite)

(in-suite macros-basic-check-type-suite)

(deftest check-type-expansion
  "CHECK-TYPE: TYPEP guard signals TYPE-ERROR inside a STORE-VALUE restart."
  (let* ((result (our-macroexpand-1 '(check-type x integer)))
         (guard-form (third result))
         (restart-form (third guard-form))
         (restart-clause (third restart-form)))
    (assert-eq 'tagbody (car result))
    (assert-eq 'unless (car guard-form))
    (assert-equal '(typep x 'integer) (second guard-form))
    (assert-eq 'restart-case (car restart-form))
    (assert-eq 'store-value (car restart-clause))))
