(in-package :cl-cc/test)

(in-suite cl-cc-suite)

(deftest prolog-unification-simple
  (let ((env (cl-cc:unify '?x 42 nil)))
    (assert-false (null env))
    (assert-= (cl-cc:substitute-variables '?x env) 42)))

(deftest prolog-unification-fails
  (assert-null (cl-cc:unify 1 2 nil)))

(deftest prolog-logic-var-p
  (assert-true (cl-cc:logic-var-p '?x))
  (assert-false (cl-cc:logic-var-p 'x))
  (assert-false (cl-cc:logic-var-p 42)))

(deftest prolog-substitute-nested
  (let ((env (cl-cc:unify '?x '(a b c) nil)))
    (assert-false (null env))
    (assert-equal (cl-cc:substitute-variables '(?x ?y) env) '((a b c) ?y))))
