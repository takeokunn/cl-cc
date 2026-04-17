;;;; tests/unit/expand/macros-basic-check-type-tests.lisp
;;;; Coverage tests for src/expand/macros-basic.lisp

(in-package :cl-cc/test)

(defsuite macros-basic-check-type-suite
  :description "Tests for macros-basic.lisp: check-type"
  :parent cl-cc-unit-suite)

(in-suite macros-basic-check-type-suite)

(deftest check-type-expansion
  "CHECK-TYPE: UNLESS with typep test, body signals type-error via make-condition."
  (let* ((result (our-macroexpand-1 '(check-type x integer)))
         (error-form (caddr result))
         (make-cond (cadr error-form)))
    (assert-eq 'unless (car result))
    (assert-equal '(typep x 'integer) (cadr result))
    (assert-eq 'error (car error-form))
    (assert-eq 'cl-cc/expand::%make-type-error (car make-cond))
    (assert-equal '(quote integer) (third make-cond))))
