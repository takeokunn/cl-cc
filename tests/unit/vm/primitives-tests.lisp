;;;; tests/unit/vm/primitives-tests.lisp — VM Primitive Instruction Tests
;;;
;;; Tests for execute-instruction on type predicates, comparisons,
;;; arithmetic extensions, boolean ops, and vm-typep.
;;;
;;; Relies on make-test-vm / exec1 helpers from list-tests.lisp.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Helpers: construct pred1/pred2 instructions from defstructs
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
