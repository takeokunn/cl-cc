;;;; tests/unit/runtime/runtime-tests-2.lisp — Runtime Library Unit Tests (Part 2)
;;;;
;;;; Continuation of runtime-tests.lisp:
;;;; list ops, array ops, arithmetic helpers, numeric predicates, and comparisons.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── List Operations ───────────────────────────────────────────────────────

(deftest rt-cons-creation-and-mutation
  "rt-cons/car/cdr create cons; rt-rplaca/rt-rplacd mutate in place."
  (let ((c (cl-cc/runtime:rt-cons 1 2)))
    (assert-= 1 (cl-cc/runtime:rt-car c))
    (assert-= 2 (cl-cc/runtime:rt-cdr c))
    (cl-cc/runtime:rt-rplaca c 10)
    (cl-cc/runtime:rt-rplacd c 20)
    (assert-= 10 (cl-cc/runtime:rt-car c))
    (assert-= 20 (cl-cc/runtime:rt-cdr c))))

(deftest rt-list-push-pop
  "rt-pop-list returns head+tail; rt-push-list conses onto front."
  (multiple-value-bind (head tail)
      (cl-cc/runtime:rt-pop-list '(a b c))
    (assert-eq 'a head)
    (assert-equal '(b c) tail))
  (assert-equal '(x a b) (cl-cc/runtime:rt-push-list 'x '(a b))))

(deftest-each rt-endp-convention
  "rt-endp: 1 for nil (end of list); 0 for non-empty list."
  :cases (("nil-end"   nil  1)
          ("non-empty" '(1) 0))
  (input expected)
  (assert-= expected (cl-cc/runtime:rt-endp input)))

(deftest-each rt-equal-convention
  "rt-equal: 1 when structurally equal; 0 when not."
  :cases (("equal"     '(1 2) '(1 2) 1)
          ("not-equal" '(1 2) '(1 3) 0))
  (a b expected)
  (assert-= expected (cl-cc/runtime:rt-equal a b)))

;;; ─── Array Operations ─────────────────────────────────────────────────────

(deftest rt-make-array-creation
  "rt-make-array: correct length; :initial-element initializes all slots."
  (let ((a5 (cl-cc/runtime:rt-make-array 5))
        (a3 (cl-cc/runtime:rt-make-array 3 :initial-element 0)))
    (assert-= 5 (cl-cc/runtime:rt-array-length a5))
    (assert-= 0 (cl-cc/runtime:rt-aref a3 0))))

(deftest rt-array-mutation-ops
  "rt-aset/rt-aref, rt-svref/rt-svset, rt-bit-set/rt-bit-access all mutate correctly."
  (let ((a (cl-cc/runtime:rt-make-array 3 :initial-element 0)))
    (cl-cc/runtime:rt-aset a 1 42)
    (assert-= 42 (cl-cc/runtime:rt-aref a 1)))
  (let ((v (vector 1 2 3)))
    (assert-= 2 (cl-cc/runtime:rt-svref v 1))
    (cl-cc/runtime:rt-svset v 1 99)
    (assert-= 99 (cl-cc/runtime:rt-svref v 1)))
  (let ((bv (make-array 4 :element-type 'bit :initial-element 0)))
    (cl-cc/runtime:rt-bit-set bv 2 1)
    (assert-= 1 (cl-cc/runtime:rt-bit-access bv 2))
    (assert-= 0 (cl-cc/runtime:rt-bit-access bv 0))))

;;; ─── Arithmetic Helpers ────────────────────────────────────────────────────

(deftest-each rt-basic-arithmetic
  "rt-add/sub/mul/div/mod/rem: binary arithmetic operations."
  :cases (("add" #'cl-cc/runtime:rt-add 3  4   7)
          ("sub" #'cl-cc/runtime:rt-sub 3  4  -1)
          ("mul" #'cl-cc/runtime:rt-mul 3  4  12)
          ("div" #'cl-cc/runtime:rt-div 5  2   5/2)
          ("mod" #'cl-cc/runtime:rt-mod 7  3   1)
          ("rem" #'cl-cc/runtime:rt-rem 7  3   1))
  (fn a b expected)
  (assert-= expected (funcall fn a b)))

(deftest-each rt-unary-arithmetic
  "rt-neg/abs/inc/dec/lognot: unary arithmetic and bitwise operations."
  :cases (("neg"    #'cl-cc/runtime:rt-neg     5   -5)
          ("abs"    #'cl-cc/runtime:rt-abs    -5    5)
          ("inc"    #'cl-cc/runtime:rt-inc     5    6)
          ("dec"    #'cl-cc/runtime:rt-dec     5    4)
          ("lognot" #'cl-cc/runtime:rt-lognot  42  -43))
  (fn input expected)
  (assert-= expected (funcall fn input)))

(deftest-each rt-not-convention
  "rt-not: 1 for falsy (nil); 0 for truthy (t, integer)."
  :cases (("nil"     nil  1)
          ("true"    t    0)
          ("integer" 42   0))
  (input expected)
  (assert-= expected (cl-cc/runtime:rt-not input)))

(deftest-each rt-numeric-predicates
  "Numeric predicates return 1/0."
  :cases (("evenp-t"  #'cl-cc/runtime:rt-evenp  4  1)
          ("evenp-f"  #'cl-cc/runtime:rt-evenp  3  0)
          ("oddp-t"   #'cl-cc/runtime:rt-oddp   3  1)
          ("oddp-f"   #'cl-cc/runtime:rt-oddp   4  0)
          ("zerop-t"  #'cl-cc/runtime:rt-zerop  0  1)
          ("zerop-f"  #'cl-cc/runtime:rt-zerop  1  0)
          ("plusp-t"  #'cl-cc/runtime:rt-plusp   5  1)
          ("plusp-f"  #'cl-cc/runtime:rt-plusp  -1  0)
          ("minusp-t" #'cl-cc/runtime:rt-minusp -1  1)
          ("minusp-f" #'cl-cc/runtime:rt-minusp  1  0))
  (pred-fn input expected)
  (assert-= expected (funcall pred-fn input)))

;;; ─── Comparisons ───────────────────────────────────────────────────────────

(deftest-each rt-comparisons
  "Comparison helpers return 1/0."
  :cases (("lt-t"     #'cl-cc/runtime:rt-lt     1 2 1)
          ("lt-f"     #'cl-cc/runtime:rt-lt     2 1 0)
          ("gt-t"     #'cl-cc/runtime:rt-gt     2 1 1)
          ("gt-f"     #'cl-cc/runtime:rt-gt     1 2 0)
          ("le-eq"    #'cl-cc/runtime:rt-le     2 2 1)
          ("ge-eq"    #'cl-cc/runtime:rt-ge     2 2 1)
          ("num-eq-t" #'cl-cc/runtime:rt-num-eq 5 5 1)
          ("num-eq-f" #'cl-cc/runtime:rt-num-eq 5 6 0)
          ("eq-t"     #'cl-cc/runtime:rt-eq     :a :a 1)
          ("eq-f"     #'cl-cc/runtime:rt-eq     :a :b 0)
          ("eql-t"    #'cl-cc/runtime:rt-eql    42 42 1)
          ("eql-f"    #'cl-cc/runtime:rt-eql    42 43 0))
  (cmp-fn a b expected)
  (assert-= expected (funcall cmp-fn a b)))
