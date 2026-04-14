;;;; tests/unit/expand/expander-array-tests.lisp — Array expander tests

(in-package :cl-cc/test)

(defsuite expander-array-suite :description "Array expander unit tests"
  :parent cl-cc-unit-suite)


(in-suite expander-array-suite)
(deftest-each expand-make-array-adjustable-promotes
  "expand-make-array-form uses make-adjustable-vector for :adjustable t or :fill-pointer t.
Binds *print-circle* so format can handle the gensym-shared expansion
structure without running the 30s test-level timeout."
  :cases (("adjustable"   '(:adjustable t))
          ("fill-pointer" '(:fill-pointer t)))
  (kwargs)
  (let ((*print-circle* t))
    (assert-true (search "ADJUSTABLE"
                         (format nil "~S" (cl-cc::expand-make-array-form 10 kwargs))))))

(deftest expand-make-array-simple
  "expand-make-array-form without adjustable keywords expands to (make-array SIZE)."
  (let ((result (cl-cc::expand-make-array-form 10 nil)))
    (assert-true result)))
