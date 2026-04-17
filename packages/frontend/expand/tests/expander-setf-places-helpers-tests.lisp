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
    (assert-true (search "RPLACA" (format nil "~S" car-result)))
    (assert-true (search "RPLACD" (format nil "~S" cdr-result)))))

(deftest expand-setf-cons-place-nth
  "expand-setf-cons-place handles NTH with NTHCDR."
  (let ((result (cl-cc/expand::expand-setf-cons-place '(nth 2 x) 'v)))
    (assert-true (search "NTHCDR" (format nil "~S" result)))))
