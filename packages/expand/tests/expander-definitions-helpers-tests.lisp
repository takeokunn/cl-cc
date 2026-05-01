;;;; tests/unit/expand/expander-definitions-helpers-tests.lisp — Definition helper tests

(in-package :cl-cc/test)

(defsuite expander-definitions-helpers-suite
  :description "Definition helper unit tests"
  :parent cl-cc-unit-suite)

(in-suite expander-definitions-helpers-suite)

(deftest expand-lambda-list-defaults-expands-defaults
  "Default initforms inside &optional and &key positions are macro-expanded."
  (assert-equal '(x &optional (y (+ 2 1) y-p) &key ((:z z) (+ 3 1) z-p))
                (cl-cc/expand::expand-lambda-list-defaults
                 '(x &optional (y (1+ 2) y-p) &key ((:z z) (1+ 3) z-p)))))

(deftest expand-lambda-list-defaults-keeps-plain-params
  "Required parameters and lambda-list keywords are preserved."
  (assert-equal '(a b &rest args)
                (cl-cc/expand::expand-lambda-list-defaults '(a b &rest args))))
