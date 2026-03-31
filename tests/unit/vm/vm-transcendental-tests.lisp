;;;; tests/unit/vm/vm-transcendental-tests.lisp — VM transcendental instruction tests

(in-package :cl-cc/test)

(defsuite vm-transcendental-suite
  :description "Unit tests for src/vm/vm-transcendental.lisp"
  :parent cl-cc-suite)

(in-suite vm-transcendental-suite)

(defun %vm-trans-unary (ctor-fn src-val)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src-val)
    (exec1 (funcall ctor-fn :dst 0 :src 1) s)
    (cl-cc:vm-reg-get s 0)))

(defun %vm-trans-binary (ctor-fn lhs rhs)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 lhs)
    (cl-cc:vm-reg-set s 2 rhs)
    (exec1 (funcall ctor-fn :dst 0 :lhs 1 :rhs 2) s)
    (cl-cc:vm-reg-get s 0)))

(deftest-each vm-transcendental-unary
  "Unary transcendental functions produce correct results at known points."
  :cases (("sqrt-4"   #'cl-cc::make-vm-sqrt     4.0  2.0)
          ("exp-0"    #'cl-cc::make-vm-exp-inst  0.0  1.0)
          ("log-1"    #'cl-cc::make-vm-log-inst  1.0  0.0)
          ("sin-0"    #'cl-cc::make-vm-sin-inst  0.0  0.0)
          ("cos-0"    #'cl-cc::make-vm-cos-inst  0.0  1.0)
          ("tan-0"    #'cl-cc::make-vm-tan-inst  0.0  0.0)
          ("sinh-0"   #'cl-cc::make-vm-sinh-inst 0.0  0.0)
          ("cosh-0"   #'cl-cc::make-vm-cosh-inst 0.0  1.0)
          ("tanh-0"   #'cl-cc::make-vm-tanh-inst 0.0  0.0))
  (ctor input expected)
  (assert-= expected (%vm-trans-unary ctor input)))

(deftest vm-atan2
  "vm-atan2-inst computes atan(0, 1) = 0."
  (assert-= 0.0 (%vm-trans-binary #'cl-cc::make-vm-atan2-inst 0.0 1.0)))
