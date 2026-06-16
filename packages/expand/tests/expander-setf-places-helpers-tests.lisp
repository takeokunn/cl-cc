;;;; tests/unit/expand/expander-setf-places-helpers-tests.lisp — Setf-place helper tests

(in-package :cl-cc/test)

(defsuite expander-setf-places-helpers-suite
  :description "Setf-place helper unit tests"
  :parent cl-cc-unit-suite)

(in-suite expander-setf-places-helpers-suite)

(deftest expand-setf-cons-place-car-and-cdr
  "expand-setf-cons-place uses rplaca/rplacd for CAR and CDR accessors."
  (let ((car-result (cl-cc/expand::expand-setf-cons-place '(car x) 'v))
        (cdr-result (cl-cc/expand::expand-setf-cons-place '(cdr x) 'v)))
    (assert-form-string-contains car-result "RPLACA")
    (assert-form-string-contains cdr-result "RPLACD")))

(deftest expand-setf-cons-place-nth
  "expand-setf-cons-place handles NTH with NTHCDR."
  (let ((result (cl-cc/expand::expand-setf-cons-place '(nth 2 x) 'v)))
    (assert-form-string-contains result "NTHCDR")))

(deftest expand-setf-cons-place-deep-cxr
  "expand-setf-cons-place handles deep CXR accessors such as CDDDR."
  (let ((result (cl-cc/expand::expand-setf-cons-place '(cdddr method-entry) 'v)))
    (assert-form-string-contains result "RPLACD")
    (assert-form-string-contains result "CDR")))
