;;;; tests/conformance/number-conformance-tests.lisp
;;;; ANSI CL Number Tower Conformance Tests (expected-fail for known gaps)
;;;;
;;;; Tests numeric operations that should work per ANSI CL but are
;;;; fixnum-only in the native x86-64 backend or rely on host SBCL
;;;; for bignum/ratio/complex arithmetic.

(in-package :cl-cc/test)

(defsuite ansi-conformance-number-suite
  :description "ANSI CL Number Tower Conformance Tests"
  :parent cl-cc-conformance-suite
  :parallel nil)

(in-suite ansi-conformance-number-suite)

;;; ──────────────────────────────────────────────────────────────────────
;;; Helper
;;; ──────────────────────────────────────────────────────────────────────

(defun num-run (form-string)
  "Run a numeric FORM-STRING through cl-cc and return the result."
  (cl-cc:run-string form-string))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Bignum Operations (fixnum overflow → bignum promotion)
;;; ──────────────────────────────────────────────────────────────────────

(defexpected num-bignum-add-overflow
  "Adding beyond most-positive-fixnum should produce a bignum."
  :timeout 30
  :tags '(:number :bignum :overflow :e2e)
  (let ((result (num-run "(+ most-positive-fixnum 1)")))
    (assert-true (> result most-positive-fixnum))))

(defexpected num-bignum-mul-overflow
  "Multiplying beyond fixnum range should produce a bignum."
  :timeout 30
  :tags '(:number :bignum :overflow :e2e)
  (let ((result (num-run "(* most-positive-fixnum 2)")))
    (assert-true (> result most-positive-fixnum))))

(defexpected num-bignum-sub-overflow
  "Subtracting below most-negative-fixnum should produce a bignum."
  :timeout 30
  :tags '(:number :bignum :overflow :e2e)
  (let ((result (num-run "(- most-negative-fixnum 1)")))
    (assert-true (< result most-negative-fixnum))))

(defexpected num-bignum-expt-large
  "Large exponentiation should produce a bignum."
  :timeout 30
  :tags '(:number :bignum :expt :e2e)
  (let ((result (num-run "(expt 2 100)")))
    (assert-true (> result 0))
    (assert-= 1267650600228229401496703205376 result)))

(defexpected num-bignum-gcd-large
  "GCD of large integers should work correctly."
  :timeout 30
  :tags '(:number :bignum :gcd :e2e)
  (let ((result (num-run "(gcd 1234567890123456789 9876543210987654321)")))
    (assert-= 9 result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Ratio Operations
;;; ──────────────────────────────────────────────────────────────────────

(defexpected num-ratio-division
  "Integer division should produce a ratio, not truncate."
  :timeout 30
  :tags '(:number :ratio :division :e2e)
  (let ((result (num-run "(/ 1 3)")))
    (assert-equal 1/3 result)))

(defexpected num-ratio-add
  "Adding ratios should produce correct exact result."
  :timeout 30
  :tags '(:number :ratio :addition :e2e)
  (let ((result (num-run "(+ 1/2 1/3)")))
    (assert-equal 5/6 result)))

(defexpected num-ratio-mul
  "Multiplying ratios should produce correct exact result."
  :timeout 30
  :tags '(:number :ratio :multiplication :e2e)
  (let ((result (num-run "(* 2/3 3/4)")))
    (assert-equal 1/2 result)))

(defexpected num-ratio-typep
  "Ratios should satisfy typep 'ratio."
  :timeout 30
  :tags '(:number :ratio :typep :e2e)
  (let ((result (num-run "(typep (/ 1 3) 'ratio)")))
    (assert-true result)))

(defexpected num-rational-function
  "rational should convert float to exact ratio."
  :timeout 30
  :tags '(:number :rational :e2e)
  (let ((result (num-run "(rational 0.5)")))
    (assert-equal 1/2 result)))

(defexpected num-rationalize-function
  "rationalize should find simplest rational approximation."
  :timeout 30
  :tags '(:number :rationalize :e2e)
  (let ((result (num-run "(rationalize 1/3)")))
    (assert-equal 1/3 result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Complex Number Operations
;;; ──────────────────────────────────────────────────────────────────────

(defexpected num-complex-constructor
  "complex should construct complex numbers."
  :timeout 30
  :tags '(:number :complex :e2e)
  (let ((result (num-run "(complex 3 4)")))
    (assert-equal 3 (num-run "(realpart (complex 3 4))"))
    (assert-equal 4 (num-run "(imagpart (complex 3 4))"))))

(defexpected num-complex-add
  "Adding complex numbers should work."
  :timeout 30
  :tags '(:number :complex :addition :e2e)
  (let ((result (num-run "(+ #c(1 2) #c(3 4))")))
    (assert-equal 4 (num-run "(realpart (+ #c(1 2) #c(3 4)))"))
    (assert-equal 6 (num-run "(imagpart (+ #c(1 2) #c(3 4)))"))))

(defexpected num-complex-mul
  "Multiplying complex numbers should work."
  :timeout 30
  :tags '(:number :complex :multiplication :e2e)
  (let ((result (num-run "(* #c(1 2) #c(3 4))")))
    (assert-equal -5 (num-run "(realpart (* #c(1 2) #c(3 4)))"))
    (assert-equal 10 (num-run "(imagpart (* #c(1 2) #c(3 4)))"))))

(defexpected num-complex-conjugate
  "conjugate should negate imaginary part."
  :timeout 30
  :tags '(:number :complex :conjugate :e2e)
  (let ((result (num-run "(conjugate #c(3 4))")))
    (assert-equal 3 (num-run "(realpart (conjugate #c(3 4)))"))
    (assert-equal -4 (num-run "(imagpart (conjugate #c(3 4)))"))))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Numeric Contagion Rules
;;; ──────────────────────────────────────────────────────────────────────

(defexpected num-contagion-integer-ratio
  "Integer + ratio should produce ratio."
  :timeout 30
  :tags '(:number :contagion :ratio :e2e)
  (let ((result (num-run "(+ 1 1/2)")))
    (assert-equal 3/2 result)))

(defexpected num-contagion-ratio-float
  "Ratio + float should produce float."
  :timeout 30
  :tags '(:number :contagion :float :e2e)
  (let ((result (num-run "(+ 1/2 0.5)")))
    (assert-= 1.0 result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Numeric Predicates on Extended Types
;;; ──────────────────────────────────────────────────────────────────────

(defexpected num-bignump-predicate
  "bignump should return T for bignums."
  :timeout 30
  :tags '(:number :bignump :typep :e2e)
  (let ((result (num-run "(bignump (expt 2 100))")))
    (assert-true result)))

(defexpected num-float-type-hierarchy
  "Floats should satisfy floatp but not integerp."
  :timeout 30
  :tags '(:number :floatp :type-hierarchy :e2e)
  (let ((result (num-run "(and (floatp 3.14) (not (integerp 3.14)))")))
    (assert-true result)))

(defexpected num-real-type-hierarchy
  "All numbers should satisfy realp."
  :timeout 30
  :tags '(:number :realp :type-hierarchy :e2e)
  (let ((result (num-run "(and (realp 42) (realp 3.14) (realp 1/2))")))
    (assert-true result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Native x86-64 Fixnum Boundary
;;; ──────────────────────────────────────────────────────────────────────

(defexpected num-native-fixnum-add-overflow
  "Native-compiled code should handle fixnum overflow correctly."
  :timeout 60
  :tags '(:number :native :fixnum :overflow :e2e)
  ;; This test compiles a function that adds at the fixnum boundary
  (let ((result (num-run
                 "(let ((x most-positive-fixnum))
                    (list x (+ x 1) (type-of (+ x 1))))")))
    (assert-true result)))

(defexpected num-native-bignum-identity
  "Large integer literals should survive compilation roundtrip."
  :timeout 60
  :tags '(:number :native :bignum :identity :e2e)
  (let ((result (num-run "1267650600228229401496703205376")))
    (assert-= 1267650600228229401496703205376 result)))

;;; ──────────────────────────────────────────────────────────────────────
;;; Expected-Fail: Arithmetic Functions on Extended Types
;;; ──────────────────────────────────────────────────────────────────────

(defexpected num-isqrt-large
  "isqrt should work on bignums."
  :timeout 30
  :tags '(:number :isqrt :bignum :e2e)
  (let ((result (num-run "(isqrt (expt 2 100))")))
    (assert-= (isqrt (expt 2 100)) result)))

(defexpected num-floor-large
  "floor should work on bignums."
  :timeout 30
  :tags '(:number :floor :bignum :e2e)
  (let ((result (num-run "(floor (expt 2 100) 3)")))
    (assert-= (floor (expt 2 100) 3) result)))
