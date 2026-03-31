;;;; tests/unit/expand/expander-array-tests.lisp — Array expander tests

(in-package :cl-cc/test)

(defsuite expander-array-suite :description "Array expander unit tests")

(deftest-each expand-make-array-adjustable-promotes
  "expand-make-array-form uses make-adjustable-vector for :adjustable t or :fill-pointer t."
  :cases (("adjustable"   '(:adjustable t))
          ("fill-pointer" '(:fill-pointer t)))
  (kwargs)
  (assert-true (search "ADJUSTABLE"
                       (format nil "~S" (cl-cc::expand-make-array-form 10 kwargs)))))

(deftest expand-make-array-simple
  "expand-make-array-form without adjustable keywords expands to (make-array SIZE)."
  (let ((result (cl-cc::expand-make-array-form 10 nil)))
    (assert-true result)))
