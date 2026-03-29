;;;; tests/unit/vm/primitives-tests.lisp — VM Primitive Instruction Tests
;;;
;;; Tests for execute-instruction on type predicates, comparisons,
;;; arithmetic extensions, boolean ops, bit ops, transcendentals,
;;; float ops, rational/complex ops, environment predicates,
;;; time functions, symbol plists, progv, and generic arithmetic.
;;;
;;; Relies on make-test-vm / exec1 helpers from list-tests.lisp.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Helpers: construct pred1/pred2/unary/binary instructions from defstructs
;;; ═══════════════════════════════════════════════════════════════════════════

(defun %make-pred1 (ctor-fn src-val)
  "Run a unary predicate instruction (pred1). Returns DST register value."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src-val)
    (exec1 (funcall ctor-fn :dst 0 :src 1) s)
    (cl-cc:vm-reg-get s 0)))

(defun %make-pred2 (ctor-fn lhs rhs)
  "Run a binary predicate instruction (pred2). Returns DST register value."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 lhs)
    (cl-cc:vm-reg-set s 2 rhs)
    (exec1 (funcall ctor-fn :dst 0 :lhs 1 :rhs 2) s)
    (cl-cc:vm-reg-get s 0)))

(defun %make-unary (ctor-fn src-val)
  "Run a unary instruction. Returns DST register value."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src-val)
    (exec1 (funcall ctor-fn :dst 0 :src 1) s)
    (cl-cc:vm-reg-get s 0)))

(defun %make-binary (ctor-fn lhs rhs)
  "Run a binary instruction. Returns DST register value."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 lhs)
    (cl-cc:vm-reg-set s 2 rhs)
    (exec1 (funcall ctor-fn :dst 0 :lhs 1 :rhs 2) s)
    (cl-cc:vm-reg-get s 0)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 1: Type Predicates (pred1 pattern — return 1/0)
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each prim-type-pred
  "Type predicates return 1 for matching types, 0 otherwise."
  :cases (("cons-p-true"     #'cl-cc::make-vm-cons-p     '(a . b)   1)
          ("cons-p-false"    #'cl-cc::make-vm-cons-p     42         0)
          ("null-p-true"     #'cl-cc::make-vm-null-p     nil         1)
          ("null-p-false"    #'cl-cc::make-vm-null-p     'x          0)
          ("symbol-p-true"   #'cl-cc::make-vm-symbol-p   'hello      1)
          ("symbol-p-false"  #'cl-cc::make-vm-symbol-p   42          0)
          ("number-p-true"   #'cl-cc::make-vm-number-p   42          1)
          ("number-p-false"  #'cl-cc::make-vm-number-p   "hello"     0)
          ("integer-p-true"  #'cl-cc::make-vm-integer-p  42          1)
          ("integer-p-false" #'cl-cc::make-vm-integer-p  3.14        0)
          ("evenp-true"      #'cl-cc::make-vm-evenp      4           1)
          ("evenp-false"     #'cl-cc::make-vm-evenp      3           0)
          ("oddp-true"       #'cl-cc::make-vm-oddp       3           1)
          ("oddp-false"      #'cl-cc::make-vm-oddp       4           0))
  (ctor src-val expected)
  (assert-= expected (%make-pred1 ctor src-val)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 2: EQL comparison (pred2 — return 1/0)
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each prim-eq
  "vm-eq returns 1 for eql values, 0 otherwise."
  :cases (("equal"     42 42 1)
          ("not-equal" 42 99 0))
  (lhs rhs expected)
  (assert-= expected (%make-pred2 #'cl-cc::make-vm-eq lhs rhs)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 3: Numeric Comparisons (pred2 — return 1/0)
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each prim-comparison
  "Numeric comparison instructions return 1/0."
  :cases (("lt-true"      #'cl-cc::make-vm-lt     3 5  1)
          ("lt-false"     #'cl-cc::make-vm-lt     5 3  0)
          ("lt-equal"     #'cl-cc::make-vm-lt     3 3  0)
          ("gt-true"      #'cl-cc::make-vm-gt     5 3  1)
          ("gt-false"     #'cl-cc::make-vm-gt     3 5  0)
          ("le-true-lt"   #'cl-cc::make-vm-le     3 5  1)
          ("le-true-eq"   #'cl-cc::make-vm-le     3 3  1)
          ("le-false"     #'cl-cc::make-vm-le     5 3  0)
          ("ge-true-gt"   #'cl-cc::make-vm-ge     5 3  1)
          ("ge-true-eq"   #'cl-cc::make-vm-ge     3 3  1)
          ("ge-false"     #'cl-cc::make-vm-ge     3 5  0)
          ("num-eq-true"  #'cl-cc::make-vm-num-eq 7 7  1)
          ("num-eq-false" #'cl-cc::make-vm-num-eq 7 8  0))
  (ctor lhs rhs expected)
  (assert-= expected (%make-pred2 ctor lhs rhs)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 4: Unary Arithmetic
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each prim-unary-arith
  "Unary arithmetic instructions compute correct results."
  :cases (("neg-pos"   #'cl-cc::make-vm-neg  5    -5)
          ("neg-neg"   #'cl-cc::make-vm-neg  -3   3)
          ("neg-zero"  #'cl-cc::make-vm-neg  0    0)
          ("abs-pos"   #'cl-cc::make-vm-abs  5    5)
          ("abs-neg"   #'cl-cc::make-vm-abs  -7   7)
          ("abs-zero"  #'cl-cc::make-vm-abs  0    0)
          ("inc-pos"   #'cl-cc::make-vm-inc  5    6)
          ("inc-neg"   #'cl-cc::make-vm-inc  -1   0)
          ("dec-pos"   #'cl-cc::make-vm-dec  5    4)
          ("dec-zero"  #'cl-cc::make-vm-dec  0    -1))
  (ctor src expected)
  (assert-= expected (%make-unary ctor src)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 5: Binary Arithmetic
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each prim-binary-arith
  "Binary arithmetic instructions compute correct results."
  :cases (("div-10/3"  #'cl-cc::make-vm-div  10 3  3)
          ("div-neg"   #'cl-cc::make-vm-div  -7 2  -4)
          ("mod-10/3"  #'cl-cc::make-vm-mod  10 3  1)
          ("mod-neg"   #'cl-cc::make-vm-mod  -7 2  1)
          ("min-lhs"   #'cl-cc::make-vm-min  3  5  3)
          ("min-rhs"   #'cl-cc::make-vm-min  5  3  3)
          ("max-lhs"   #'cl-cc::make-vm-max  5  3  5)
          ("max-rhs"   #'cl-cc::make-vm-max  3  5  5)
          ("rem-10/3"  #'cl-cc::make-vm-rem  10 3  1)
          ("rem-neg"   #'cl-cc::make-vm-rem  -7 2  -1))
  (ctor lhs rhs expected)
  (assert-= expected (%make-binary ctor lhs rhs)))

;;; Division by zero errors

(deftest-each prim-div-by-zero
  "vm-div and vm-mod both signal error on division by zero."
  :cases (("div" #'cl-cc::make-vm-div)
          ("mod" #'cl-cc::make-vm-mod))
  (make-inst)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 10)
    (cl-cc:vm-reg-set s 2 0)
    (assert-signals error
      (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) s))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 6: Multiple-Value Division (truncate/floor/ceiling/round)
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each prim-rounding-behavior
  "Rounding instructions (7 / 2): quotient in dst and (quotient remainder) in vm-values-list.
Round is excluded from the values-list check (nil means skip)."
  :cases (("truncate" #'cl-cc::make-vm-truncate     3 '(3  1))
          ("floor"    #'cl-cc::make-vm-floor-inst   3 '(3  1))
          ("ceiling"  #'cl-cc::make-vm-ceiling-inst 4 '(4 -1))
          ("round"    #'cl-cc::make-vm-round-inst   4 nil))
  (make-inst expected-q expected-vals)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 7)
    (cl-cc:vm-reg-set s 2 2)
    (exec1 (funcall make-inst :dst 0 :lhs 1 :rhs 2) s)
    (assert-= expected-q (cl-cc:vm-reg-get s 0))
    (when expected-vals
      (assert-equal expected-vals (cl-cc:vm-values-list s)))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 7: Boolean Operations
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each prim-not-cases
  "vm-not: nil/0 are falsey → t; truthy → nil."
  :cases (("nil-is-false"    nil  t)
          ("zero-is-false"   0    t)
          ("truthy-is-true"  42   nil))
  (input expected)
  (if expected
      (assert-equal t   (%make-unary #'cl-cc::make-vm-not input))
      (assert-null      (%make-unary #'cl-cc::make-vm-not input))))

(deftest-each prim-and-cases
  "vm-and: both truthy → t; any nil → nil."
  :cases (("both-true"  1   2   t)
          ("one-false"  1   nil nil))
  (lhs rhs expected)
  (if expected
      (assert-equal t (%make-binary #'cl-cc::make-vm-and lhs rhs))
      (assert-null    (%make-binary #'cl-cc::make-vm-and lhs rhs))))

(deftest-each prim-or-cases
  "vm-or: both nil → nil; any truthy → t."
  :cases (("both-false"  nil nil nil)
          ("one-true"    nil 42  t))
  (lhs rhs expected)
  (if expected
      (assert-equal t (%make-binary #'cl-cc::make-vm-or lhs rhs))
      (assert-null    (%make-binary #'cl-cc::make-vm-or lhs rhs))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 8: Bitwise Operations
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each prim-bitwise-binary
  "Bitwise binary operations compute correct results."
  :cases (("logand"    #'cl-cc::make-vm-logand  #xFF #x0F  #x0F)
          ("logior"    #'cl-cc::make-vm-logior  #xF0 #x0F  #xFF)
          ("logxor"    #'cl-cc::make-vm-logxor  #xFF #x0F  #xF0)
          ("logeqv"    #'cl-cc::make-vm-logeqv  #xFF #xFF  -1)
          ("ash-left"  #'cl-cc::make-vm-ash     1    4     16)
          ("ash-right" #'cl-cc::make-vm-ash     16   -2    4)
          ("expt-2^10" #'cl-cc::make-vm-expt    2    10    1024))
  (ctor lhs rhs expected)
  (assert-= expected (%make-binary ctor lhs rhs)))

(deftest-each prim-bitwise-unary
  "Bitwise unary operations compute correct results."
  :cases (("lognot-0"    #'cl-cc::make-vm-lognot         0    -1)
          ("lognot-ff"   #'cl-cc::make-vm-lognot         #xFF  (lognot #xFF))
          ("logcount-7"  #'cl-cc::make-vm-logcount        7    3)
          ("logcount-0"  #'cl-cc::make-vm-logcount        0    0)
          ("intlen-0"    #'cl-cc::make-vm-integer-length  0    0)
          ("intlen-255"  #'cl-cc::make-vm-integer-length  255  8))
  (ctor src expected)
  (assert-= expected (%make-unary ctor src)))

(deftest-each prim-bitwise-pred
  "Bitwise predicate instructions return 1/0."
  :cases (("logtest-true"    #'cl-cc::make-vm-logtest   #xFF #x01  1)
          ("logtest-false"   #'cl-cc::make-vm-logtest   #xF0 #x0F  0)
          ("logbitp-set"     #'cl-cc::make-vm-logbitp   0    1      1)
          ("logbitp-clear"   #'cl-cc::make-vm-logbitp   1    1      0))
  (ctor lhs rhs expected)
  (assert-= expected (%make-pred2 ctor lhs rhs)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 9: Typep General Predicate
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each prim-typep
  "vm-typep checks various type names."
  :cases (("integer"    42         'integer    1)
          ("string"     "hello"    'string     1)
          ("symbol"     'foo       'symbol     1)
          ("cons"       '(a)       'cons       1)
          ("null"       nil        'null       1)
          ("list-cons"  '(a)       'list       1)
          ("list-nil"   nil        'list       1)
          ("char"       #\a        'character  1)
          ("atom-num"   42         'atom       1)
          ("int-wrong"  "hello"    'integer    0)
          ("refine-true" 42        '(refine fixnum plusp) 1)
          ("refine-false" -1       '(refine fixnum plusp) 0))
  (src type-sym expected)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (cl-cc::make-vm-typep :dst 0 :src 1 :type-name type-sym) s)
    (assert-= expected (cl-cc:vm-reg-get s 0))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 10: Type-of Instruction
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each prim-type-of
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
          ("random-state" (make-random-state t) 'random-state)
          ("readtable" *readtable* 'readtable))
  (src expected)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (cl-cc::make-vm-type-of :dst 0 :src 1) s)
    (assert-equal expected (cl-cc:vm-reg-get s 0))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 11: Transcendental Functions
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each prim-transcendental-unary
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
  (assert-= expected (%make-unary ctor input)))

(deftest prim-atan2
  "vm-atan2-inst computes atan(0, 1) = 0."
  (assert-= 0.0 (%make-binary #'cl-cc::make-vm-atan2-inst 0.0 1.0)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 12: Float Operations
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest prim-float-convert
  "vm-float-inst converts integer to float."
  (assert-= 42.0 (%make-unary #'cl-cc::make-vm-float-inst 42)))

(deftest prim-scale-float
  "vm-scale-float scales 1.0 by 2^3 = 8.0."
  (assert-= 8.0 (%make-binary #'cl-cc::make-vm-scale-float 1.0 3)))

(deftest-each prim-float-decode-values
  "vm-decode-float and vm-integer-decode-float both store exactly 3 multiple values."
  :cases (("decode-float"         #'cl-cc::make-vm-decode-float)
          ("integer-decode-float" #'cl-cc::make-vm-integer-decode-float))
  (ctor)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 1.0)
    (exec1 (funcall ctor :dst 0 :src 1) s)
    (assert-= 3 (length (cl-cc:vm-values-list s)))))

;;; Float rounding operations

(deftest-each prim-float-rounding
  "Float rounding operations store quotient and set values-list."
  :cases (("ffloor"     #'cl-cc::make-vm-ffloor)
          ("fceiling"   #'cl-cc::make-vm-fceiling)
          ("ftruncate"  #'cl-cc::make-vm-ftruncate)
          ("fround"     #'cl-cc::make-vm-fround))
  (ctor)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 7.0)
    (cl-cc:vm-reg-set s 2 2.0)
    (exec1 (funcall ctor :dst 0 :lhs 1 :rhs 2) s)
    (assert-true (numberp (cl-cc:vm-reg-get s 0)))
    (assert-= 2 (length (cl-cc:vm-values-list s)))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 13: Rational / Complex Number Functions
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest prim-rational
  "vm-rational converts 0.5 to 1/2."
  (assert-equal 1/2 (%make-unary #'cl-cc::make-vm-rational 0.5)))

(deftest-each prim-rational-parts
  "vm-numerator/denominator extract the correct part of 3/4."
  :cases (("numerator"   #'cl-cc::make-vm-numerator   3)
          ("denominator" #'cl-cc::make-vm-denominator 4))
  (make-inst expected)
  (assert-= expected (%make-unary make-inst 3/4)))

(deftest-each prim-gcd-lcm
  "vm-gcd and vm-lcm compute correct results for known inputs."
  :cases (("gcd" #'cl-cc::make-vm-gcd 12 8  4)
          ("lcm" #'cl-cc::make-vm-lcm  4 6 12))
  (make-inst a b expected)
  (assert-= expected (%make-binary make-inst a b)))

(deftest prim-complex-construct
  "vm-complex constructs #C(3 4)."
  (assert-equal #C(3 4) (%make-binary #'cl-cc::make-vm-complex 3 4)))

(deftest-each prim-complex-parts
  "vm-realpart/imagpart extract the correct component of #C(3 4)."
  :cases (("realpart" #'cl-cc::make-vm-realpart 3)
          ("imagpart" #'cl-cc::make-vm-imagpart 4))
  (make-inst expected)
  (assert-= expected (%make-unary make-inst #C(3 4))))

(deftest prim-conjugate
  "vm-conjugate of #C(3 4) is #C(3 -4)."
  (assert-equal #C(3 -4) (%make-unary #'cl-cc::make-vm-conjugate #C(3 4))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 14: Environment Predicates (boundp/fboundp/makunbound/fmakunbound)
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest prim-boundp
  "vm-boundp returns nil for unbound symbol and t after binding."
  (assert-null (%make-unary #'cl-cc::make-vm-boundp 'test-prim-var))
  (let ((s (make-test-vm)))
    (setf (gethash 'test-prim-var (cl-cc:vm-global-vars s)) 42)
    (cl-cc:vm-reg-set s 1 'test-prim-var)
    (exec1 (cl-cc::make-vm-boundp :dst 0 :src 1) s)
    (assert-equal t (cl-cc:vm-reg-get s 0))))

(deftest prim-makunbound
  "vm-makunbound removes global binding."
  (let ((s (make-test-vm)))
    (setf (gethash 'test-prim-var (cl-cc:vm-global-vars s)) 42)
    (cl-cc:vm-reg-set s 1 'test-prim-var)
    (exec1 (cl-cc::make-vm-makunbound :dst 0 :src 1) s)
    (assert-false (nth-value 1 (gethash 'test-prim-var (cl-cc:vm-global-vars s))))))

(deftest prim-fboundp-unbound
  "vm-fboundp returns nil for unbound function."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'test-prim-fn)
    (exec1 (cl-cc::make-vm-fboundp :dst 0 :src 1) s)
    (assert-null (cl-cc:vm-reg-get s 0))))

(deftest prim-fmakunbound
  "vm-fmakunbound removes function binding."
  (let ((s (make-test-vm)))
    (setf (gethash 'test-prim-fn (cl-cc:vm-function-registry s)) :some-closure)
    (cl-cc:vm-reg-set s 1 'test-prim-fn)
    (exec1 (cl-cc::make-vm-fmakunbound :dst 0 :src 1) s)
    (assert-false (nth-value 1 (gethash 'test-prim-fn (cl-cc:vm-function-registry s))))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 15: Random Number Generation
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest prim-random-in-range
  "vm-random returns a number in [0, 100)."
  (let ((result (%make-unary #'cl-cc::make-vm-random 100)))
    (assert-true (integerp result))
    (assert-true (>= result 0))
    (assert-true (< result 100))))

(deftest prim-make-random-state
  "vm-make-random-state with t creates a fresh random state."
  (assert-true (random-state-p (%make-unary #'cl-cc::make-vm-make-random-state t))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 16: Time Functions
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each prim-get-time
  "Time query instructions return non-negative integers."
  :cases (("universal-time"    #'cl-cc::make-vm-get-universal-time     #'>)
          ("internal-realtime" #'cl-cc::make-vm-get-internal-real-time #'>=))
  (ctor cmp)
  (let ((s (make-test-vm)))
    (exec1 (funcall ctor :dst 0) s)
    (assert-true (funcall cmp (cl-cc:vm-reg-get s 0) 0))))

(deftest prim-decode-universal-time-values
  "vm-decode-universal-time stores 9 values."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 (get-universal-time))
    (exec1 (cl-cc::make-vm-decode-universal-time :dst 0 :src 1) s)
    (assert-= 9 (length (cl-cc:vm-values-list s)))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 17: Symbol Property Lists
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest prim-symbol-get-default
  "vm-symbol-get returns default when property absent."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'prim-test-sym)
    (cl-cc:vm-reg-set s 2 :color)
    (cl-cc:vm-reg-set s 3 :none)
    (exec1 (cl-cc::make-vm-symbol-get :dst 0 :sym 1
                                       :indicator 2 :default 3) s)
    (assert-equal :none (cl-cc:vm-reg-get s 0))))

(deftest prim-symbol-set-and-get
  "vm-symbol-set then vm-symbol-get round-trips."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'prim-test-sym)
    (cl-cc:vm-reg-set s 2 :color)
    (cl-cc:vm-reg-set s 3 'red)
    (cl-cc:vm-reg-set s 4 nil)
    ;; set
    (exec1 (cl-cc::make-vm-symbol-set :dst 0 :sym 1
                                       :indicator 2 :value 3) s)
    ;; get
    (exec1 (cl-cc::make-vm-symbol-get :dst 5 :sym 1
                                       :indicator 2 :default 4) s)
    (assert-equal 'red (cl-cc:vm-reg-get s 5))))

(deftest prim-remprop
  "vm-remprop removes a property and returns t."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'prim-test-sym)
    (cl-cc:vm-reg-set s 2 :color)
    (cl-cc:vm-reg-set s 3 'red)
    (exec1 (cl-cc::make-vm-symbol-set :dst 0 :sym 1
                                       :indicator 2 :value 3) s)
    (exec1 (cl-cc::make-vm-remprop :dst 0 :sym 1 :indicator 2) s)
    (assert-equal t (cl-cc:vm-reg-get s 0))))

(deftest prim-symbol-plist-empty
  "vm-symbol-plist returns nil for symbol with no properties."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 'prim-test-fresh-sym)
    (exec1 (cl-cc::make-vm-symbol-plist :dst 0 :src 1) s)
    (assert-null (cl-cc:vm-reg-get s 0))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 18: PROGV — Dynamic Variable Binding
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest prim-progv-enter-exit
  "vm-progv-enter binds globals and vm-progv-exit restores them."
  (let ((s (make-test-vm)))
    (setf (gethash 'prim-pv-x (cl-cc:vm-global-vars s)) 10)
    (cl-cc:vm-reg-set s 1 '(prim-pv-x prim-pv-y))
    (cl-cc:vm-reg-set s 2 '(99 100))
    ;; Enter
    (exec1 (cl-cc::make-vm-progv-enter :dst 0 :syms 1 :vals 2) s)
    (assert-= 99 (gethash 'prim-pv-x (cl-cc:vm-global-vars s)))
    (assert-= 100 (gethash 'prim-pv-y (cl-cc:vm-global-vars s)))
    ;; Exit — restore
    (exec1 (cl-cc::make-vm-progv-exit :saved 0) s)
    (assert-= 10 (gethash 'prim-pv-x (cl-cc:vm-global-vars s)))
    (assert-false (nth-value 1 (gethash 'prim-pv-y (cl-cc:vm-global-vars s))))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 19: Generic (Polymorphic) Arithmetic
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each prim-generic-arith
  "Generic arithmetic instructions compute correct results."
  :cases (("add"  #'cl-cc::make-vm-generic-add  10 3   13)
          ("sub"  #'cl-cc::make-vm-generic-sub  10 3   7)
          ("mul"  #'cl-cc::make-vm-generic-mul  10 3   30)
          ("div"  #'cl-cc::make-vm-generic-div  10 3   3))
  (ctor lhs rhs expected)
  (assert-= expected (%make-binary ctor lhs rhs)))

(deftest-each prim-generic-comparison
  "Generic comparison instructions: eq/lt/gt over known values."
  :cases (("eq-equal"    #'cl-cc::make-vm-generic-eq "hello" "hello" t)
          ("eq-unequal"  #'cl-cc::make-vm-generic-eq "hello" "world" nil)
          ("lt-true"     #'cl-cc::make-vm-generic-lt 3       5       t)
          ("gt-false"    #'cl-cc::make-vm-generic-gt 3       5       nil))
  (ctor lhs rhs expected)
  (if expected
      (assert-equal t  (%make-binary ctor lhs rhs))
      (assert-null     (%make-binary ctor lhs rhs))))

(deftest prim-generic-div-by-zero
  "vm-generic-div signals error on division by zero."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 10)
    (cl-cc:vm-reg-set s 2 0)
    (assert-signals error
      (exec1 (cl-cc::make-vm-generic-div :dst 0 :lhs 1 :rhs 2) s))))
