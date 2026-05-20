;;;; tests/unit/expand/expander-test-support.lisp — Shared expansion test helpers

(in-package :cl-cc/test)

(defmacro assert-list-head (form expected-head)
  `(let ((result ,form))
     (assert-eq ,expected-head (car result))
     result))

(defmacro assert-expansion-equal (form expected)
  `(assert-equal ,expected (cl-cc/expand:compiler-macroexpand-all ,form)))

(defmacro assert-expansion-head (form expected-head)
  `(let ((result (cl-cc/expand:compiler-macroexpand-all ,form)))
     (assert-eq ,expected-head (car result))
     result))

(defmacro assert-expansion-contains (form needle)
  `(let ((result (cl-cc/expand:compiler-macroexpand-all ,form)))
     (assert-true (search ,needle (format nil "~S" result)))
     result))

(defmacro assert-no-expansion (form)
  `(assert-equal ,form (our-macroexpand-1 ,form)))

(defmacro assert-printed-contains (form needle)
  `(assert-true (search ,needle (format nil "~S" ,form))))

(defun %tree-contains-head-p (head form)
  "True if FORM contains any cons whose CAR is HEAD."
  (cond ((consp form)
         (or (eq (car form) head)
             (%tree-contains-head-p head (car form))
             (%tree-contains-head-p head (cdr form))))
        (t nil)))
