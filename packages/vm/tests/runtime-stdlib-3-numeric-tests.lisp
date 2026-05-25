;;; runtime-stdlib-3-numeric-tests.lisp — FR-952/955/956 native VM number tower

(in-package :cl-cc/test)

(defsuite runtime-stdlib-3-numeric-suite
  :description "FR-952 bignum, FR-955 rational, FR-956 complex number tower tests"
  :parent cl-cc-unit-suite)

(in-suite runtime-stdlib-3-numeric-suite)

(deftest fr-952-bignum-expt-2-1000
  "vm-bignum-expt computes 2^1000 without delegating to host arithmetic."
  (let ((value (cl-cc/vm::vm-bignum-expt 2 1000)))
    (assert-true (cl-cc/vm::vm-bignum-p value))
    (assert-equal (write-to-string (expt 2 1000))
                  (cl-cc/vm::vm-bignum-to-string value 10))))

(deftest fr-952-bignum-large-multiplication
  "vm-bignum-mul handles large multi-limb products."
  (let* ((lhs (1- (expt 2 257)))
         (rhs (+ (expt 2 193) 12345))
         (product (cl-cc/vm::vm-bignum-mul lhs rhs)))
    (assert-true (cl-cc/vm::vm-bignum-p product))
    (assert-= (* lhs rhs) (cl-cc/vm::vm-bignum-to-integer product))))

(deftest fr-952-bignum-gcd
  "vm-bignum-gcd uses binary GCD for large values."
  (let* ((a (* (expt 2 180) 45))
         (b (* (expt 2 120) 75))
         (g (cl-cc/vm::vm-bignum-gcd a b)))
    (assert-true (cl-cc/vm::vm-bignum-p g))
    (assert-= (gcd a b) (cl-cc/vm::vm-bignum-to-integer g))))

(deftest fr-955-rational-addition-normalizes
  "1/3 + 1/6 normalizes to 1/2."
  (let ((result (cl-cc/vm::vm-rational-add
                 (cl-cc/vm::vm-make-ratio 1 3)
                 (cl-cc/vm::vm-make-ratio 1 6))))
    (assert-true (cl-cc/vm::vm-ratio-p result))
    (assert-= 1 (cl-cc/vm::vm-ratio-numerator result))
    (assert-= 2 (cl-cc/vm::vm-ratio-denominator result))))

(deftest fr-955-rationalize-decimal
  "vm-rationalize maps 0.1 to the friendly rational 1/10."
  (let ((result (cl-cc/vm::vm-rationalize 0.1d0)))
    (assert-true (cl-cc/vm::vm-ratio-p result))
    (assert-= 1 (cl-cc/vm::vm-ratio-numerator result))
    (assert-= 10 (cl-cc/vm::vm-ratio-denominator result))))

(deftest fr-955-vm-floor-rational-remainder
  "vm-floor returns quotient plus normalized rational remainder."
  (multiple-value-bind (q r)
      (cl-cc/vm::vm-floor (cl-cc/vm::vm-make-ratio 7 3))
    (assert-= 2 q)
    (assert-true (cl-cc/vm::vm-ratio-p r))
    (assert-= 1 (cl-cc/vm::vm-ratio-numerator r))
    (assert-= 3 (cl-cc/vm::vm-ratio-denominator r))))

(deftest fr-956-complex-sqrt-minus-one
  "sqrt(-1) is represented as #C(0 1)."
  (let ((result (cl-cc/vm::vm-complex-sqrt -1)))
    (assert-true (cl-cc/vm::vm-complex-p result))
    (assert-= 0 (cl-cc/vm::vm-realpart result))
    (assert-= 1 (cl-cc/vm::vm-imagpart result))))

(deftest fr-956-complex-euler-identity
  "exp(#C(0 pi)) is approximately #C(-1 0)."
  (let ((result (cl-cc/vm::vm-complex-exp (cl-cc/vm::vm-complex-make 0 pi))))
    (assert-true (cl-cc/vm::vm-complex-p result))
    (assert-true (< (abs (+ 1 (cl-cc/vm::vm-realpart result))) 1.0d-12))
    (assert-true (< (abs (cl-cc/vm::vm-imagpart result)) 1.0d-12))))

(deftest fr-956-complex-print-format
  "Native VM complex values print in #C(real imag) format."
  (assert-equal "#C(3 4)"
                (write-to-string (cl-cc/vm::vm-complex-make 3 4))))
