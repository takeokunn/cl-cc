;;;; tests/unit/expand/expander-helpers-tests.lisp — Expander helper tests

(in-package :cl-cc/test)

(defsuite expander-helpers-suite
  :description "Expander helper unit tests"
  :parent cl-cc-unit-suite)

(in-suite expander-helpers-suite)

(deftest expand-make-array-form-initial-element
  "expand-make-array-form preserves :initial-element for backend lowering."
  (let ((result (cl-cc/expand::expand-make-array-form 3 '(:initial-element 9))))
    (assert-eq 'make-array (car result))
    (assert-true (search "INITIAL-ELEMENT" (format nil "~S" result)))))

(deftest expand-make-array-form-fill-pointer
  "expand-make-array-form leaves adjustable keywords for codegen-phase2."
  (let ((result (cl-cc/expand::expand-make-array-form 3 '(:adjustable t))))
    (assert-eq 'make-array (car result))
    (assert-true (search "ADJUSTABLE" (format nil "~S" result)))))

(deftest expand-setf-accessor-falls-back-to-slot-value
  "expand-setf-accessor falls back to the runtime slot writer when no accessor mapping exists."
  (let ((result (cl-cc/expand::expand-setf-accessor '(foo obj) 'value)))
    (assert-eq 'cl-cc/bootstrap:rt-slot-set (car result))
    (assert-eq 'obj (second result))))
