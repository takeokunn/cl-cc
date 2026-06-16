;;;; tests/unit/expand/expander-test-support.lisp — Shared expansion test helpers

(in-package :cl-cc/test)

(defmacro assert-expansion-head (form expected-head)
  `(let ((result (cl-cc/expand:compiler-macroexpand-all ,form)))
     (assert-eq ,expected-head (car result))
     result))

(defmacro assert-no-expansion (form)
  `(assert-equal ,form (our-macroexpand-1 ,form)))

(defmacro assert-form-string-contains (form expected-substring)
  `(assert-true (search ,expected-substring (format nil "~S" ,form))))

(defmacro assert-expanded-string-contains (form expected-substring)
  `(assert-form-string-contains (cl-cc/expand:compiler-macroexpand-all ,form)
     ,expected-substring))

(defmacro assert-form-string-not-contains (form unexpected-substring)
  `(assert-false (search ,unexpected-substring (format nil "~S" ,form))))

(defmacro assert-expanded-string-not-contains (form unexpected-substring)
  `(assert-form-string-not-contains
       (cl-cc/expand:compiler-macroexpand-all ,form)
       ,unexpected-substring))

(defun %tree-contains-head-p (head form)
  "True if FORM contains any cons whose CAR is HEAD."
  (cond ((consp form)
         (or (eq (car form) head)
             (%tree-contains-head-p head (car form))
             (%tree-contains-head-p head (cdr form))))
        (t nil)))
