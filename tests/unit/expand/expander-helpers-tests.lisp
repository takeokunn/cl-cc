;;;; tests/unit/expand/expander-helpers-tests.lisp — Expander helper tests

(in-package :cl-cc/test)

(defsuite expander-helpers-suite
  :description "Expander helper unit tests"
  :parent cl-cc-suite)

(in-suite expander-helpers-suite)

(deftest expand-make-array-form-initial-element
  "expand-make-array-form turns :initial-element into a filling loop."
  (let ((result (cl-cc::expand-make-array-form 3 '(:initial-element 9))))
    (assert-eq 'let (car result))
    (assert-true (search "TAGBODY" (format nil "~S" result)))))

(deftest expand-make-array-form-fill-pointer
  "expand-make-array-form promotes adjustable arrays when fill-pointer is present."
  (let ((result (cl-cc::expand-make-array-form 3 '(:fill-pointer t))))
    (assert-true (member (car result) '(let* cl-cc::make-adjustable-vector))))
  (assert-eq 'cl-cc::make-adjustable-vector
             (car (cl-cc::expand-make-array-form 3 '(:adjustable t)))))

(deftest expand-setf-accessor-falls-back-to-slot-value
  "expand-setf-accessor falls back to slot-value when no accessor mapping exists."
  (let ((result (cl-cc::expand-setf-accessor '(foo obj) 'value)))
    (assert-eq 'setf (car result))
    (assert-eq 'slot-value (caadr result))))
