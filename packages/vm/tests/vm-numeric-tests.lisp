;;;; tests/unit/vm/vm-numeric-tests.lisp — VM numeric tower instruction tests

(in-package :cl-cc/test)

(defsuite vm-numeric-suite
  :description "Unit tests for src/vm/vm-numeric.lisp"
  :parent cl-cc-unit-suite)

(in-suite vm-numeric-suite)

(defun %vm-num-unary (ctor-fn src-val)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src-val)
    (exec1 (funcall ctor-fn :dst 0 :src 1) s)
    (cl-cc:vm-reg-get s 0)))

(defun %vm-num-binary (ctor-fn lhs rhs)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 lhs)
    (cl-cc:vm-reg-set s 2 rhs)
    (exec1 (funcall ctor-fn :dst 0 :lhs 1 :rhs 2) s)
    (cl-cc:vm-reg-get s 0)))

(deftest-each vm-type-of
  "vm-type-of returns the correct type symbol."
  :cases (("nil"    nil            'null)
          ("fixnum" 42             'fixnum)
          ("bignum"  (+ most-positive-fixnum 1) 'bignum)
          ("ratio"   1/2           'ratio)
          ("float"  1.0f0          'single-float)
          ("str"    "hello"      'string)
          ("char"   #\a            'character)
          ("sym"    'foo           'symbol)
          ("cons"   '(a)           'cons)
          ("pathname" #p"/tmp"     'pathname)
          ("random-state" (make-random-state t) 'random-state))
  (src expected)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (cl-cc:make-vm-type-of :dst 0 :src 1) s)
    (assert-equal expected (cl-cc:vm-reg-get s 0))))

(deftest-each vm-float-convert-and-scale
  "Float conversion and scale operations produce the expected results."
  :cases (("convert" #'cl-cc:make-vm-float-inst 42 42.0)
          ("scale"   #'cl-cc:make-vm-scale-float 1.0 8.0))
  (ctor src expected)
  (if (eq ctor #'cl-cc:make-vm-scale-float)
      (assert-= expected (%vm-num-binary ctor src 3))
      (assert-= expected (%vm-num-unary ctor src))))

(deftest-each vm-float-introspection
  "Float inspection constructors execute and return the host Common Lisp results."
  :cases (("float-precision" #'cl-cc:make-vm-float-precision 1.0 (float-precision 1.0))
          ("float-radix" #'cl-cc:make-vm-float-radix 1.0 (float-radix 1.0))
          ("float-sign" #'cl-cc:make-vm-float-sign -2.5 (float-sign -2.5))
          ("float-digits" #'cl-cc:make-vm-float-digits 1.0 (float-digits 1.0)))
  (ctor src expected)
  (assert-equal expected (%vm-num-unary ctor src)))

(deftest-each vm-float-decode-values
  "vm-decode-float and vm-integer-decode-float both store exactly 3 multiple values."
  :cases (("decode-float"         #'cl-cc:make-vm-decode-float)
          ("integer-decode-float" #'cl-cc:make-vm-integer-decode-float))
  (ctor)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 1.0)
    (exec1 (funcall ctor :dst 0 :src 1) s)
    (assert-= 3 (length (cl-cc:vm-values-list s)))))

(deftest vm-bignum-digit-vector-splits-little-endian-digits
  "vm-bignum-digit-vector exposes a host-independent little-endian digit representation."
  (let ((base cl-cc/vm::+vm-bignum-digit-base+))
    (multiple-value-bind (digits sign)
        (cl-cc/vm::vm-bignum-digit-vector (+ base 42))
      (assert-= 1 sign)
      (assert-= 2 (length digits))
      (assert-= 42 (aref digits 0))
      (assert-= 1 (aref digits 1)))))

(deftest vm-bignum-schoolbook-multiply-digits-computes-product-digits
  "vm-bignum-schoolbook-multiply-digits multiplies little-endian digit vectors."
  (let ((digits (cl-cc/vm::vm-bignum-schoolbook-multiply-digits
                 #(3 2 1) #(6 5 4) 10)))
    (assert-equal '(8 8 0 6 5) (coerce digits 'list))))

(deftest-each vm-bignum-multiplication-strategy-selects-thresholded-plan
  "vm-bignum-multiplication-strategy distinguishes fixnum, schoolbook, and Karatsuba plans."
  :cases (("fixnum" 21 2 4 :fixnum)
          ("schoolbook" most-positive-fixnum 2 64 :schoolbook)
          ("karatsuba" (expt cl-cc/vm::+vm-bignum-digit-base+ 5)
           (expt cl-cc/vm::+vm-bignum-digit-base+ 5) 4 :karatsuba))
  (lhs rhs threshold expected)
  (assert-eq expected
             (cl-cc/vm::vm-bignum-multiplication-strategy
              lhs rhs :threshold threshold)))

(deftest vm-bignum-multiply-plan-records-digits-sign-and-strategy
  "vm-bignum-multiply-plan records operand digit vectors, sign, and selected strategy."
  (let ((plan (cl-cc/vm::vm-bignum-multiply-plan (- most-positive-fixnum) 2 :threshold 64)))
    (assert-eq :schoolbook (getf plan :strategy))
    (assert-= -1 (getf plan :sign))
    (assert-true (vectorp (getf plan :lhs-digits)))
    (assert-true (vectorp (getf plan :rhs-digits)))))

(deftest-each vm-float-rounding
  "Float rounding operations store quotient and set values-list."
  :cases (("ffloor"     #'cl-cc:make-vm-ffloor)
          ("fceiling"   #'cl-cc:make-vm-fceiling)
          ("ftruncate"  #'cl-cc:make-vm-ftruncate)
          ("fround"     #'cl-cc:make-vm-fround))
  (ctor)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 7.0)
    (cl-cc:vm-reg-set s 2 2.0)
    (exec1 (funcall ctor :dst 0 :lhs 1 :rhs 2) s)
    (assert-true (numberp (cl-cc:vm-reg-get s 0)))
    (assert-= 2 (length (cl-cc:vm-values-list s)))))

(deftest vm-rational
  "vm-rational converts 0.5 to 1/2."
  (assert-equal 1/2 (%vm-num-unary #'cl-cc:make-vm-rational 0.5)))

(deftest-each vm-rational-parts
  "vm-numerator/denominator extract the correct part of 3/4."
  :cases (("numerator"   #'cl-cc:make-vm-numerator   3)
          ("denominator" #'cl-cc:make-vm-denominator 4))
  (make-inst expected)
  (assert-= expected (%vm-num-unary make-inst 3/4)))

(deftest-each vm-gcd-lcm
  "vm-gcd and vm-lcm compute correct results for known inputs."
  :cases (("gcd" #'cl-cc:make-vm-gcd 12 8  4)
          ("lcm" #'cl-cc:make-vm-lcm  4 6 12))
  (make-inst a b expected)
  (assert-= expected (%vm-num-binary make-inst a b)))

(deftest vm-complex-construct
  "vm-complex constructs #C(3 4)."
  (assert-equal #C(3 4) (%vm-num-binary #'cl-cc:make-vm-complex 3 4)))

(deftest-each vm-complex-parts
  "vm-realpart/imagpart extract the correct component of #C(3 4)."
  :cases (("realpart" #'cl-cc:make-vm-realpart 3)
          ("imagpart" #'cl-cc:make-vm-imagpart 4))
  (make-inst expected)
  (assert-= expected (%vm-num-unary make-inst #C(3 4))))

(deftest vm-conjugate
  "vm-conjugate of #C(3 4) is #C(3 -4)."
  (assert-equal #C(3 -4) (%vm-num-unary #'cl-cc:make-vm-conjugate #C(3 4))))

(deftest vm-complex-unbox-plan-splits-local-complex
  "vm-complex-unbox-plan exposes a split real/imag representation for local complex values."
  (let ((plan (cl-cc/vm::vm-complex-unbox-plan #C(3 4) :local-p t)))
    (assert-eq :split-registers (getf plan :representation))
    (assert-= 3 (getf plan :real))
    (assert-= 4 (getf plan :imag))))

(deftest vm-complex-unbox-plan-keeps-escaping-complex-boxed
  "vm-complex-unbox-plan falls back to boxed representation when the value escapes."
  (let ((plan (cl-cc/vm::vm-complex-unbox-plan #C(3 4) :local-p nil)))
    (assert-eq :boxed (getf plan :representation))
    (assert-equal #C(3 4) (getf plan :value))))

(deftest vm-complex-unboxed-add-plan-adds-components
  "vm-complex-unboxed-add-plan rewrites local complex addition into component addition."
  (let ((plan (cl-cc/vm::vm-complex-unboxed-add-plan #C(1 2) #C(3 4) :local-p t)))
    (assert-eq :split-registers (getf plan :representation))
    (assert-= 4 (getf plan :real))
    (assert-= 6 (getf plan :imag))))
