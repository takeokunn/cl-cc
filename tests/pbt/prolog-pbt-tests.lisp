;;;; tests/pbt/prolog-pbt-tests.lisp - Property-Based Tests for Prolog Engine
;;;
;;; This module provides property-based tests for the Prolog unification engine.

(in-package :cl-cc/pbt)

(in-suite cl-cc-pbt-suite)

;; ----------------------------------------------------------------------------
;; Unification Properties
;; ----------------------------------------------------------------------------

(defproperty unify-variable-with-integer
    (n (gen-integer :min -1000 :max 1000))
  (let ((result (unify '?x n)))
    (and (not (null result))
         (equal (substitute-variables '?x result) n))))

(defproperty unify-variable-with-symbol
    (s (gen-symbol :prefix "SYM"))
  (let ((result (unify '?x s)))
    (and (not (null result))
         (eq (substitute-variables '?x result) s))))

(defproperty unify-two-variables-binds
    (n (gen-integer :min -1000 :max 1000))
  (let* ((env1 (unify '?x '?y))
         (env2 (unify '?x n env1)))
    (and (not (null env1))
         (not (null env2))
         (equal (substitute-variables '?y env2) n))))

(defproperty unify-fails-on-different-integers
    (a (gen-integer :min 0 :max 500)
     b (gen-integer :min 501 :max 1000))
  (null (unify a b)))

(defproperty substitute-preserves-non-variables
    (n (gen-integer :min -1000 :max 1000))
  (= (substitute-variables n nil) n))

(defproperty unify-variable-with-list
    (a (gen-integer :min -100 :max 100)
     b (gen-integer :min -100 :max 100))
  (let ((result (unify '?x (list a b))))
    (and (not (null result))
         (equal (substitute-variables '?x result) (list a b)))))

(defproperty logic-var-p-works
    (n (gen-integer :min -1000 :max 1000))
  (and (logic-var-p '?x)
       (logic-var-p '?foo)
       (not (logic-var-p 'x))
       (not (logic-var-p n))))
