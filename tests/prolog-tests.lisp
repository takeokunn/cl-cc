(in-package :cl-cc/test)

(in-suite cl-cc-suite)

(test prolog-unification-simple
  (let ((env (cl-cc:unify '?x 42 nil)))
    (is (not (null env)))
    (is (= (cl-cc:substitute-variables '?x env) 42))))

(test prolog-unification-fails
  (is (null (cl-cc:unify 1 2 nil))))

(test prolog-logic-var-p
  (is (cl-cc:logic-var-p '?x))
  (is (not (cl-cc:logic-var-p 'x)))
  (is (not (cl-cc:logic-var-p 42))))

(test prolog-substitute-nested
  (let ((env (cl-cc:unify '?x '(a b c) nil)))
    (is (not (null env)))
    (is (equal (cl-cc:substitute-variables '(?x ?y) env) '((a b c) ?y)))))
