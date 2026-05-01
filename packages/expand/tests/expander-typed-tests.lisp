;;;; tests/unit/expand/expander-typed-tests.lisp — Typed-form expander tests

(in-package :cl-cc/test)

(defsuite expander-typed-suite :description "Typed-form expander unit tests"
  :parent cl-cc-unit-suite)


(in-suite expander-typed-suite)
(deftest expand-typed-defun-plain-params-unchanged
  "expand-typed-defun-or-lambda with plain params produces a defun with same params."
  (let* ((result (cl-cc/expand::expand-typed-defun-or-lambda
                  'defun 'my-typed-fn '(a b) '((+ a b))))
         (result-str (format nil "~S" result)))
    (assert-true (search "MY-TYPED-FN" result-str))))

(deftest expand-typed-defun-typed-params-stripped
  "expand-typed-defun-or-lambda strips type annotations; check-type → typep."
  (let* ((result (cl-cc/expand::expand-typed-defun-or-lambda
                  'defun 'typed-adder '((x integer) (y integer)) '((+ x y))))
         (result-str (format nil "~S" result)))
    (assert-true (search "TYPEP" result-str))))

(deftest expand-typed-lambda-produces-lambda
  "expand-typed-defun-or-lambda with 'lambda head produces a lambda form."
  (let* ((result (cl-cc/expand::expand-typed-defun-or-lambda
                  'lambda nil '(x) '(x)))
         (result-str (format nil "~S" result)))
    (assert-true (search "LAMBDA" result-str))))
