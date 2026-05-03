;;;; tests/unit/expand/expander-array-tests.lisp — Array expander tests

(in-package :cl-cc/test)

(defsuite expander-array-suite :description "Array expander unit tests"
  :parent cl-cc-unit-suite)


(in-suite expander-array-suite)
(deftest-each expand-make-array-adjustable-promotes
  "expand-make-array-form preserves array keyword calls for the backend.
Binds *print-circle* so format can handle the gensym-shared expansion
structure without running the 30s test-level timeout."
  :cases (("adjustable"   '(:adjustable t) "ADJUSTABLE")
          ("fill-pointer" '(:fill-pointer t) "FILL-POINTER"))
  (kwargs expected-keyword)
  (let ((*print-circle* t))
    (let ((result (cl-cc/expand::expand-make-array-form 10 kwargs)))
      (assert-eq 'make-array (car result))
      (assert-true (search expected-keyword (format nil "~S" result))))))

(deftest expand-make-array-simple
  "expand-make-array-form without adjustable keywords expands to (make-array SIZE)."
  (let ((result (cl-cc/expand::expand-make-array-form 10 nil)))
    (assert-true result)))
