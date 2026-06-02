;;; vm-bignum-tests.lisp — Unit tests for the VM native bignum/rational/complex tower
;;;
;;; Covers: vm-bignum-{add,sub,negate,div,mul}, vm-bignum-to-string with non-decimal
;;; radix, vm-integer->bignum roundtrip, vm-rational-{sub,mul,div}, vm-complex-
;;; {add,sub,mul,div,conjugate,abs,phase,log}, and
;;; vm-bignum-burnikel-ziegler-divide rounding modes.

(in-package :cl-cc/test)

(defsuite vm-bignum-suite
  :description "Native VM bignum, rational, and complex number tower unit tests"
  :parent cl-cc-unit-suite)

(in-suite vm-bignum-suite)

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 1: vm-integer->bignum roundtrip
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each bignum-integer-roundtrip
  "vm-integer->bignum and vm-bignum-to-integer are inverse operations."
  :cases (("zero"          0)
          ("positive"      12345)
          ("negative"      -99999)
          ("power-of-2"    (expt 2 128))
          ("large-neg"     (- (expt 2 200))))
  (n)
  (let ((bn (cl-cc/vm::vm-integer->bignum n)))
    (assert-true (cl-cc/vm::vm-bignum-p bn))
    (assert-= n (cl-cc/vm::vm-bignum-to-integer bn))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 2: vm-bignum-add
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each bignum-add
  "vm-bignum-add produces the correct signed sum."
  :cases (("pos+pos"   (expt 2 128) (expt 2 64) (+ (expt 2 128) (expt 2 64)))
          ("pos+neg"   (expt 2 64)  (- (expt 2 32))  (- (expt 2 64) (expt 2 32)))
          ("neg+neg"   (- (expt 2 64)) (- (expt 2 64)) (- (expt 2 65)))
          ("sum-zero"  (expt 2 64)  (- (expt 2 64))  0))
  (a b expected)
  (let ((result (cl-cc/vm::vm-bignum-add a b)))
    (assert-= expected (cl-cc/vm::vm-bignum-to-integer result))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 3: vm-bignum-sub and vm-bignum-negate
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each bignum-sub
  "vm-bignum-sub produces the correct signed difference."
  :cases (("large-sub"  (expt 2 200) (expt 2 100) (- (expt 2 200) (expt 2 100)))
          ("neg-result" (expt 2 64)  (expt 2 128) (- (expt 2 64) (expt 2 128)))
          ("sub-self"   (expt 2 64)  (expt 2 64)  0))
  (a b expected)
  (assert-= expected (cl-cc/vm::vm-bignum-to-integer (cl-cc/vm::vm-bignum-sub a b))))

(deftest-each bignum-negate
  "vm-bignum-negate flips the sign of a bignum value."
  :cases (("negate-positive" (expt 2 100)  (- (expt 2 100)))
          ("negate-negative" (- (expt 2 100)) (expt 2 100))
          ("negate-zero"     0                0))
  (n expected)
  (assert-= expected
            (cl-cc/vm::vm-bignum-to-integer (cl-cc/vm::vm-bignum-negate n))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 4: vm-bignum-div
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each bignum-div
  "vm-bignum-div returns truncated quotient and remainder with correct signs."
  :cases (("pos-div-pos"  (expt 2 128) (expt 2 64)  (expt 2 64)       0)
          ("large-modulo" (* (expt 2 100) 7) 7  (expt 2 100)          0)
          ("non-exact"    (+ (* (expt 2 64) 5) 3) 5  (expt 2 64)      3))
  (dividend divisor expected-q expected-r)
  (multiple-value-bind (q r) (cl-cc/vm::vm-bignum-div dividend divisor)
    (assert-= expected-q (cl-cc/vm::vm-bignum-to-integer q))
    (assert-= expected-r (cl-cc/vm::vm-bignum-to-integer r))))

(deftest bignum-div-by-zero-signals-error
  "vm-bignum-div signals an error when the divisor is zero."
  (assert-signals error
    (cl-cc/vm::vm-bignum-div (expt 2 64) 0)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 5: vm-bignum-to-string with non-decimal radix
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each bignum-to-string-radix
  "vm-bignum-to-string renders the bignum correctly in non-decimal bases."
  :cases (("hex-255"       255       16  "FF")
          ("binary-10"     10        2   "1010")
          ("octal-8"       8         8   "10")
          ("zero"          0         16  "0")
          ("negative-hex"  -255      16  "-FF"))
  (n radix expected)
  (assert-equal expected (cl-cc/vm::vm-bignum-to-string n radix)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 6: vm-bignum-burnikel-ziegler-divide rounding modes
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each bignum-bz-divide-rounding-modes
  "vm-bignum-burnikel-ziegler-divide applies the requested rounding mode."
  :cases (("truncate-pos"  7  2 :truncate  3  1)
          ("truncate-neg"  -7 2 :truncate  -3 -1)
          ("floor-pos"     7  2 :floor     3  1)
          ("floor-neg"     -7 2 :floor     -4 1)
          ("ceiling-pos"   7  2 :ceiling   4  -1)
          ("ceiling-neg"   -7 2 :ceiling   -3 -1)
          ("round-tie-up"  5  2 :round     2  1)
          ("round-exact"   6  2 :round     3  0))
  (dividend divisor rounding expected-q expected-r)
  (multiple-value-bind (q r)
      (cl-cc/vm::vm-bignum-burnikel-ziegler-divide dividend divisor :rounding rounding)
    (assert-= expected-q q)
    (assert-= expected-r r)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 7: vm-rational-sub, vm-rational-mul, vm-rational-div
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each rational-sub
  "vm-rational-sub produces the correct normalized difference."
  :cases (("half-minus-third"    1 2  1 3   1 6)
          ("third-minus-half"    1 3  1 2   -1 6)
          ("same-value"          3 4  3 4   0 1))
  (an ad bn bd expected-n expected-d)
  (let ((result (cl-cc/vm::vm-rational-sub (cl-cc/vm::vm-make-ratio an ad)
                                            (cl-cc/vm::vm-make-ratio bn bd))))
    (assert-= expected-n (cl-cc/vm::vm-ratio-numerator result))
    (assert-= expected-d (cl-cc/vm::vm-ratio-denominator result))))

(deftest-each rational-mul
  "vm-rational-mul produces the correct normalized product."
  :cases (("half-times-third"   1 2  1 3   1 6)
          ("two-thirds-times-3" 2 3  3 1   2 1)
          ("neg-product"        -1 2  1 3  -1 6))
  (an ad bn bd expected-n expected-d)
  (let ((result (cl-cc/vm::vm-rational-mul (cl-cc/vm::vm-make-ratio an ad)
                                            (cl-cc/vm::vm-make-ratio bn bd))))
    (assert-= expected-n (cl-cc/vm::vm-ratio-numerator result))
    (assert-= expected-d (cl-cc/vm::vm-ratio-denominator result))))

(deftest-each rational-div
  "vm-rational-div produces the correct normalized quotient."
  :cases (("half-div-third"   1 2  1 3   3 2)
          ("third-div-neg"    1 3  -1 2  -2 3)
          ("integer-result"   2 3  2 9   3 1))
  (an ad bn bd expected-n expected-d)
  (let ((result (cl-cc/vm::vm-rational-div (cl-cc/vm::vm-make-ratio an ad)
                                            (cl-cc/vm::vm-make-ratio bn bd))))
    (assert-= expected-n (cl-cc/vm::vm-ratio-numerator result))
    (assert-= expected-d (cl-cc/vm::vm-ratio-denominator result))))

(deftest rational-div-by-zero-signals-error
  "vm-rational-div signals an error when the divisor numerator is zero."
  (assert-signals error
    (cl-cc/vm::vm-rational-div (cl-cc/vm::vm-make-ratio 1 2)
                                (cl-cc/vm::vm-make-ratio 0 1))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 8: vm-complex-add, vm-complex-sub, vm-complex-mul, vm-complex-div
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each complex-add
  "vm-complex-add sums real and imaginary components independently."
  :cases (("pos-plus-pos"  1 2  3 4  4  6)
          ("cancel-imag"   2 5  1 -5 3  0)
          ("zero-plus-z"   0 0  3 4  3  4))
  (ar ai br bi expected-r expected-i)
  (let ((result (cl-cc/vm::vm-complex-add (cl-cc/vm::vm-complex-make ar ai)
                                          (cl-cc/vm::vm-complex-make br bi))))
    (assert-= expected-r (cl-cc/vm::vm-realpart result))
    (assert-= expected-i (cl-cc/vm::vm-imagpart result))))

(deftest-each complex-sub
  "vm-complex-sub subtracts real and imaginary components independently."
  :cases (("simple-sub"   5 7  2 3  3 4)
          ("neg-result"   1 1  3 4  -2 -3)
          ("sub-self"     3 4  3 4  0  0))
  (ar ai br bi expected-r expected-i)
  (let ((result (cl-cc/vm::vm-complex-sub (cl-cc/vm::vm-complex-make ar ai)
                                          (cl-cc/vm::vm-complex-make br bi))))
    (assert-= expected-r (cl-cc/vm::vm-realpart result))
    (assert-= expected-i (cl-cc/vm::vm-imagpart result))))

(deftest-each complex-mul
  "vm-complex-mul applies the (a+bi)(c+di) = (ac-bd) + (ad+bc)i formula."
  :cases (("i-squared"     0 1  0 1   -1 0)
          ("real-times-z"  3 0  2 5   6  15)
          ("standard"      1 2  3 4   -5 10))
  (ar ai br bi expected-r expected-i)
  (let ((result (cl-cc/vm::vm-complex-mul (cl-cc/vm::vm-complex-make ar ai)
                                          (cl-cc/vm::vm-complex-make br bi))))
    (assert-= expected-r (cl-cc/vm::vm-realpart result))
    (assert-= expected-i (cl-cc/vm::vm-imagpart result))))

(deftest-each complex-div
  "vm-complex-div applies conjugate-denominator division."
  :cases (("div-by-real"   6 4  2 0   3 2)
          ("div-by-i"      0 4  0 2   2 0))
  (ar ai br bi expected-r expected-i)
  (let ((result (cl-cc/vm::vm-complex-div (cl-cc/vm::vm-complex-make ar ai)
                                          (cl-cc/vm::vm-complex-make br bi))))
    (assert-= expected-r (cl-cc/vm::vm-realpart result))
    (assert-= expected-i (cl-cc/vm::vm-imagpart result))))

(deftest complex-div-by-zero-signals-error
  "vm-complex-div signals an error when dividing by the zero complex number."
  (assert-signals error
    (cl-cc/vm::vm-complex-div (cl-cc/vm::vm-complex-make 1 2)
                               (cl-cc/vm::vm-complex-make 0 0))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 9: vm-complex-conjugate, vm-complex-abs, vm-complex-phase,
;;;            vm-complex-log
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest complex-conjugate-negates-imaginary
  "vm-complex-conjugate returns the complex conjugate (negated imaginary part)."
  (let ((result (cl-cc/vm::vm-complex-conjugate (cl-cc/vm::vm-complex-make 3 4))))
    (assert-= 3 (cl-cc/vm::vm-realpart result))
    (assert-= -4 (cl-cc/vm::vm-imagpart result))))

(deftest complex-abs-3-4-is-5
  "The absolute value of #C(3 4) is the classic 3-4-5 Pythagorean triple."
  (assert-= 5.0 (cl-cc/vm::vm-complex-abs (cl-cc/vm::vm-complex-make 3 4))))

(deftest complex-phase-pure-imaginary-is-half-pi
  "The phase of a purely imaginary positive number is pi/2."
  (let ((phase (cl-cc/vm::vm-complex-phase (cl-cc/vm::vm-complex-make 0 1))))
    (assert-true (< (abs (- phase (/ pi 2))) 1.0d-12))))

(deftest complex-log-of-e-to-the-i-pi
  "log(exp(i*pi)) recovers #C(0 pi) to double-float precision."
  (let* ((z (cl-cc/vm::vm-complex-exp (cl-cc/vm::vm-complex-make 0 pi)))
         (log-z (cl-cc/vm::vm-complex-log z)))
    (assert-true (< (abs (cl-cc/vm::vm-realpart log-z)) 1.0d-12))
    (assert-true (< (abs (- (cl-cc/vm::vm-imagpart log-z) pi)) 1.0d-12))))
