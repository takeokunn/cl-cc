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

(deftest prim-eq-equal
  "vm-eq returns 1 for eql values."
  (assert-= 1 (%make-pred2 #'cl-cc::make-vm-eq 42 42)))

(deftest prim-eq-not-equal
  "vm-eq returns 0 for non-eql values."
  (assert-= 0 (%make-pred2 #'cl-cc::make-vm-eq 42 99)))

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

(deftest prim-div-by-zero
  "vm-div signals error on division by zero."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 10)
    (cl-cc:vm-reg-set s 2 0)
    (assert-signals error
      (exec1 (cl-cc::make-vm-div :dst 0 :lhs 1 :rhs 2) s))))

(deftest prim-mod-by-zero
  "vm-mod signals error on division by zero."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 10)
    (cl-cc:vm-reg-set s 2 0)
    (assert-signals error
      (exec1 (cl-cc::make-vm-mod :dst 0 :lhs 1 :rhs 2) s))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 6: Multiple-Value Division (truncate/floor/ceiling/round)
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest prim-truncate-quotient
  "vm-truncate stores quotient in dst."
  (assert-= 3 (%make-binary #'cl-cc::make-vm-truncate 7 2)))

(deftest prim-truncate-values
  "vm-truncate stores (quotient remainder) in vm-values-list."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 7)
    (cl-cc:vm-reg-set s 2 2)
    (exec1 (cl-cc::make-vm-truncate :dst 0 :lhs 1 :rhs 2) s)
    (assert-equal '(3 1) (cl-cc:vm-values-list s))))

(deftest prim-floor-quotient
  "vm-floor-inst stores floor quotient in dst."
  (assert-= 3 (%make-binary #'cl-cc::make-vm-floor-inst 7 2)))

(deftest prim-floor-values
  "vm-floor-inst stores (quotient remainder) in vm-values-list."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 7)
    (cl-cc:vm-reg-set s 2 2)
    (exec1 (cl-cc::make-vm-floor-inst :dst 0 :lhs 1 :rhs 2) s)
    (assert-equal '(3 1) (cl-cc:vm-values-list s))))

(deftest prim-ceiling-quotient
  "vm-ceiling-inst stores ceiling quotient in dst."
  (assert-= 4 (%make-binary #'cl-cc::make-vm-ceiling-inst 7 2)))

(deftest prim-ceiling-values
  "vm-ceiling-inst stores (quotient remainder) in vm-values-list."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 7)
    (cl-cc:vm-reg-set s 2 2)
    (exec1 (cl-cc::make-vm-ceiling-inst :dst 0 :lhs 1 :rhs 2) s)
    (assert-equal '(4 -1) (cl-cc:vm-values-list s))))

(deftest prim-round-quotient
  "vm-round-inst stores rounded quotient in dst (banker's rounding)."
  (assert-= 4 (%make-binary #'cl-cc::make-vm-round-inst 7 2)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 7: Boolean Operations
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest prim-not-nil
  "vm-not on nil returns t."
  (assert-equal t (%make-unary #'cl-cc::make-vm-not nil)))

(deftest prim-not-zero
  "vm-not on 0 returns t (0 is falsey in vm-falsep)."
  (assert-equal t (%make-unary #'cl-cc::make-vm-not 0)))

(deftest prim-not-truthy
  "vm-not on a truthy value returns nil."
  (assert-null (%make-unary #'cl-cc::make-vm-not 42)))

(deftest prim-and-both-true
  "vm-and with both truthy returns t."
  (assert-equal t (%make-binary #'cl-cc::make-vm-and 1 2)))

(deftest prim-and-one-false
  "vm-and with one nil returns nil."
  (assert-null (%make-binary #'cl-cc::make-vm-and 1 nil)))

(deftest prim-or-both-false
  "vm-or with both nil returns nil."
  (assert-null (%make-binary #'cl-cc::make-vm-or nil nil)))

(deftest prim-or-one-true
  "vm-or with one truthy returns t."
  (assert-equal t (%make-binary #'cl-cc::make-vm-or nil 42)))

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
          ("int-wrong"  "hello"    'integer    0))
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
  :cases (("nil"    nil      'null)
          ("int"    42       'integer)
          ("str"    "hello"  'string)
          ("char"   #\a      'character)
          ("sym"    'foo     'symbol)
          ("cons"   '(a)     'cons))
  (src expected)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (cl-cc::make-vm-type-of :dst 0 :src 1) s)
    (assert-equal expected (cl-cc:vm-reg-get s 0))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 11: Transcendental Functions
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest prim-sqrt
  "vm-sqrt computes square root."
  (assert-= 2.0 (%make-unary #'cl-cc::make-vm-sqrt 4.0)))

(deftest prim-exp
  "vm-exp-inst computes e^0 = 1."
  (assert-= 1.0 (%make-unary #'cl-cc::make-vm-exp-inst 0.0)))

(deftest prim-log
  "vm-log-inst computes ln(1) = 0."
  (assert-= 0.0 (%make-unary #'cl-cc::make-vm-log-inst 1.0)))

(deftest prim-sin-zero
  "vm-sin-inst of 0 is 0."
  (assert-= 0.0 (%make-unary #'cl-cc::make-vm-sin-inst 0.0)))

(deftest prim-cos-zero
  "vm-cos-inst of 0 is 1."
  (assert-= 1.0 (%make-unary #'cl-cc::make-vm-cos-inst 0.0)))

(deftest prim-tan-zero
  "vm-tan-inst of 0 is 0."
  (assert-= 0.0 (%make-unary #'cl-cc::make-vm-tan-inst 0.0)))

(deftest prim-atan2
  "vm-atan2-inst computes atan(0, 1) = 0."
  (assert-= 0.0 (%make-binary #'cl-cc::make-vm-atan2-inst 0.0 1.0)))

(deftest prim-sinh-zero
  "vm-sinh-inst of 0 is 0."
  (assert-= 0.0 (%make-unary #'cl-cc::make-vm-sinh-inst 0.0)))

(deftest prim-cosh-zero
  "vm-cosh-inst of 0 is 1."
  (assert-= 1.0 (%make-unary #'cl-cc::make-vm-cosh-inst 0.0)))

(deftest prim-tanh-zero
  "vm-tanh-inst of 0 is 0."
  (assert-= 0.0 (%make-unary #'cl-cc::make-vm-tanh-inst 0.0)))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 12: Float Operations
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest prim-float-convert
  "vm-float-inst converts integer to float."
  (assert-= 42.0 (%make-unary #'cl-cc::make-vm-float-inst 42)))

(deftest prim-scale-float
  "vm-scale-float scales 1.0 by 2^3 = 8.0."
  (assert-= 8.0 (%make-binary #'cl-cc::make-vm-scale-float 1.0 3)))

(deftest prim-decode-float-values
  "vm-decode-float stores 3 multiple values."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 1.0)
    (exec1 (cl-cc::make-vm-decode-float :dst 0 :src 1) s)
    (assert-= 3 (length (cl-cc:vm-values-list s)))))

(deftest prim-integer-decode-float-values
  "vm-integer-decode-float stores 3 multiple values."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 1.0)
    (exec1 (cl-cc::make-vm-integer-decode-float :dst 0 :src 1) s)
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

(deftest prim-numerator
  "vm-numerator returns numerator of 3/4."
  (assert-= 3 (%make-unary #'cl-cc::make-vm-numerator 3/4)))

(deftest prim-denominator
  "vm-denominator returns denominator of 3/4."
  (assert-= 4 (%make-unary #'cl-cc::make-vm-denominator 3/4)))

(deftest prim-gcd
  "vm-gcd of 12 and 8 is 4."
  (assert-= 4 (%make-binary #'cl-cc::make-vm-gcd 12 8)))

(deftest prim-lcm
  "vm-lcm of 4 and 6 is 12."
  (assert-= 12 (%make-binary #'cl-cc::make-vm-lcm 4 6)))

(deftest prim-complex-construct
  "vm-complex constructs #C(3 4)."
  (assert-equal #C(3 4) (%make-binary #'cl-cc::make-vm-complex 3 4)))

(deftest prim-realpart
  "vm-realpart of #C(3 4) is 3."
  (assert-= 3 (%make-unary #'cl-cc::make-vm-realpart #C(3 4))))

(deftest prim-imagpart
  "vm-imagpart of #C(3 4) is 4."
  (assert-= 4 (%make-unary #'cl-cc::make-vm-imagpart #C(3 4))))

(deftest prim-conjugate
  "vm-conjugate of #C(3 4) is #C(3 -4)."
  (assert-equal #C(3 -4) (%make-unary #'cl-cc::make-vm-conjugate #C(3 4))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 14: Environment Predicates (boundp/fboundp/makunbound/fmakunbound)
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest prim-boundp-unbound
  "vm-boundp returns nil for unbound symbol."
  (assert-null (%make-unary #'cl-cc::make-vm-boundp 'test-prim-var)))

(deftest prim-boundp-bound
  "vm-boundp returns t for bound symbol."
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

(deftest prim-get-universal-time
  "vm-get-universal-time returns a positive integer."
  (let ((s (make-test-vm)))
    (exec1 (cl-cc::make-vm-get-universal-time :dst 0) s)
    (assert-true (> (cl-cc:vm-reg-get s 0) 0))))

(deftest prim-get-internal-real-time
  "vm-get-internal-real-time returns a non-negative integer."
  (let ((s (make-test-vm)))
    (exec1 (cl-cc::make-vm-get-internal-real-time :dst 0) s)
    (assert-true (>= (cl-cc:vm-reg-get s 0) 0))))

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

(deftest prim-generic-eq-true
  "vm-generic-eq uses EQUAL and returns t for equal values."
  (assert-equal t (%make-binary #'cl-cc::make-vm-generic-eq "hello" "hello")))

(deftest prim-generic-eq-false
  "vm-generic-eq returns nil for non-equal values."
  (assert-null (%make-binary #'cl-cc::make-vm-generic-eq "hello" "world")))

(deftest prim-generic-lt
  "vm-generic-lt returns t when lhs < rhs."
  (assert-equal t (%make-binary #'cl-cc::make-vm-generic-lt 3 5)))

(deftest prim-generic-gt
  "vm-generic-gt returns nil when lhs < rhs."
  (assert-null (%make-binary #'cl-cc::make-vm-generic-gt 3 5)))

(deftest prim-generic-div-by-zero
  "vm-generic-div signals error on division by zero."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 10)
    (cl-cc:vm-reg-set s 2 0)
    (assert-signals error
      (exec1 (cl-cc::make-vm-generic-div :dst 0 :lhs 1 :rhs 2) s))))
