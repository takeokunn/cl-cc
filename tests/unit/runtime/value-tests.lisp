;;;; tests/unit/runtime/value-tests.lisp - NaN-Boxing Value Representation Tests
;;;
;;; Tests for cl-cc/runtime NaN-boxing: type predicates, encode/decode
;;; round-trips, singleton constants, and edge-case bit patterns.

(in-package :cl-cc/test)

;;; ------------------------------------------------------------
;;; Suite
;;; ------------------------------------------------------------

(defsuite value-suite
  :description "NaN-boxing value representation tests"
  :parent cl-cc-suite)

(in-suite value-suite)

;;; ------------------------------------------------------------
;;; Singleton constants
;;; ------------------------------------------------------------

(deftest-each value-singleton-constants
  "NaN-boxing singleton constants have expected 64-bit bit patterns."
  :cases (("nil"     cl-cc/runtime:+val-nil+     #x7FFF000000000000)
          ("t"       cl-cc/runtime:+val-t+        #x7FFF000000000001)
          ("unbound" cl-cc/runtime:+val-unbound+  #x7FFF000000000002))
  (const expected-bits)
  (assert-= expected-bits const))

;;; ------------------------------------------------------------
;;; val-nil-p / val-t-p / val-unbound-p
;;; ------------------------------------------------------------

(deftest-each value-sentinel-predicates
  "val-nil-p, val-t-p, val-unbound-p each recognize their own sentinel and reject others."
  :cases (("nil-recognizes-nil"  #'cl-cc/runtime:val-nil-p     cl-cc/runtime:+val-nil+     t)
          ("nil-rejects-t"       #'cl-cc/runtime:val-nil-p     cl-cc/runtime:+val-t+       nil)
          ("t-recognizes-t"      #'cl-cc/runtime:val-t-p       cl-cc/runtime:+val-t+       t)
          ("t-rejects-nil"       #'cl-cc/runtime:val-t-p       cl-cc/runtime:+val-nil+     nil)
          ("unbound-recognizes"  #'cl-cc/runtime:val-unbound-p  cl-cc/runtime:+val-unbound+ t)
          ("unbound-rejects-nil" #'cl-cc/runtime:val-unbound-p  cl-cc/runtime:+val-nil+    nil))
  (pred-fn val expected)
  (if expected
      (assert-true  (funcall pred-fn val))
      (assert-false (funcall pred-fn val))))

;;; ------------------------------------------------------------
;;; Fixnum encode/decode round-trip
;;; ------------------------------------------------------------

(deftest-each value-fixnum-round-trip
  "encode-fixnum / decode-fixnum round-trips for representative integer values."
  :cases (("zero"           0)
          ("positive"       42)
          ("negative"      -1)
          ("large-positive" (1- (expt 2 50)))
          ("large-negative" (- (expt 2 50))))
  (n)
  (assert-= n (cl-cc/runtime:decode-fixnum (cl-cc/runtime:encode-fixnum n))))

;;; ------------------------------------------------------------
;;; val-fixnum-p
;;; ------------------------------------------------------------

(deftest-each value-fixnum-p-recognises
  "val-fixnum-p recognises encoded fixnums of all sign classes."
  :cases (("zero"     0)
          ("positive" 100)
          ("negative" -100))
  (n)
  (assert-true (cl-cc/runtime:val-fixnum-p (cl-cc/runtime:encode-fixnum n))))

(deftest value-fixnum-p-rejects-sentinels
  "val-fixnum-p rejects +val-nil+ and +val-t+."
  (assert-false (cl-cc/runtime:val-fixnum-p cl-cc/runtime:+val-nil+))
  (assert-false (cl-cc/runtime:val-fixnum-p cl-cc/runtime:+val-t+)))

;;; ------------------------------------------------------------
;;; Character encode/decode round-trip
;;; ------------------------------------------------------------

(deftest-each value-char-round-trip
  "encode-char / decode-char round-trips for ASCII, NUL, and Unicode codepoints."
  :cases (("ascii"   #\A)
          ("nul"     (code-char 0))
          ("unicode" (code-char #x1F600)))
  (c)
  (assert-equal c (cl-cc/runtime:decode-char (cl-cc/runtime:encode-char c))))

;;; ------------------------------------------------------------
;;; val-char-p
;;; ------------------------------------------------------------

(deftest value-char-p-cases
  "val-char-p: recognizes encoded char; rejects +val-nil+ and encoded fixnum."
  (assert-true  (cl-cc/runtime:val-char-p (cl-cc/runtime:encode-char #\x)))
  (assert-false (cl-cc/runtime:val-char-p cl-cc/runtime:+val-nil+))
  (assert-false (cl-cc/runtime:val-char-p (cl-cc/runtime:encode-fixnum 65))))

;;; ------------------------------------------------------------
;;; Double encode/decode round-trip
;;; ------------------------------------------------------------

#+sbcl
(deftest value-double-encode-zero
  "encode-double 0.0d0 decodes back."
  (assert-equal 0.0d0 (cl-cc/runtime:decode-double (cl-cc/runtime:encode-double 0.0d0))))

#+sbcl
(deftest value-double-encode-positive
  "encode-double positive double round-trips (mantissa bits avoid fixnum collision)."
  (assert-equal 3.14d0
                (cl-cc/runtime:decode-double (cl-cc/runtime:encode-double 3.14d0))))

#+sbcl
(deftest value-double-encode-negative
  "encode-double negative double round-trips."
  (assert-equal -0.1d0
                (cl-cc/runtime:decode-double (cl-cc/runtime:encode-double -0.1d0))))

;;; ------------------------------------------------------------
;;; val-double-p
;;; ------------------------------------------------------------

#+sbcl
(deftest value-double-p-true
  "val-double-p recognises an encoded double with non-zero mantissa low bits.
   NOTE: doubles like 1.5d0 whose low 13 mantissa bits are zero are
   indistinguishable from fixnums by bit pattern alone; use 0.1d0 instead."
  (assert-true (cl-cc/runtime:val-double-p (cl-cc/runtime:encode-double 0.1d0))))

(deftest-each value-double-p-false-for-non-doubles
  "val-double-p rejects non-double values."
  :cases (("fixnum" (cl-cc/runtime:encode-fixnum 42))
          ("nil"    cl-cc/runtime:+val-nil+))
  (val)
  (assert-false (cl-cc/runtime:val-double-p val)))

;;; ------------------------------------------------------------
;;; Pointer encode/decode
;;; ------------------------------------------------------------

(deftest-each value-pointer-round-trip
  "encode-pointer / decode-pointer round-trips for each pointer tag."
  :cases (("object"   #x0000DEADBEEF cl-cc/runtime:+tag-object+)
          ("cons"     #x0000CAFE1234 cl-cc/runtime:+tag-cons+)
          ("function" #x0000000100FF cl-cc/runtime:+tag-function+))
  (addr tag)
  (let ((v (cl-cc/runtime:encode-pointer addr tag)))
    (assert-= addr (cl-cc/runtime:decode-pointer v))))

;;; ------------------------------------------------------------
;;; val-pointer-p / sub-tag predicates
;;; ------------------------------------------------------------

(deftest-each value-pointer-p-recognises
  "val-pointer-p recognises all pointer-tagged values."
  :cases (("object" cl-cc/runtime:+tag-object+)
          ("cons"   cl-cc/runtime:+tag-cons+)
          ("string" cl-cc/runtime:+tag-string+))
  (tag)
  (assert-true (cl-cc/runtime:val-pointer-p (cl-cc/runtime:encode-pointer #x1000 tag))))

(deftest value-pointer-p-false-for-nil
  "val-pointer-p rejects +val-nil+ (special, not pointer)."
  (assert-false (cl-cc/runtime:val-pointer-p cl-cc/runtime:+val-nil+)))

(deftest value-pointer-p-false-for-char
  "val-pointer-p rejects an encoded character (char range, not pointer range)."
  (assert-false (cl-cc/runtime:val-pointer-p (cl-cc/runtime:encode-char #\A))))

(deftest-each value-sub-tag-predicates
  "Each sub-tag predicate recognises its own tag and rejects others."
  :cases (("object"   cl-cc/runtime:+tag-object+   #'cl-cc/runtime:val-object-p)
          ("cons"     cl-cc/runtime:+tag-cons+     #'cl-cc/runtime:val-cons-p)
          ("symbol"   cl-cc/runtime:+tag-symbol+   #'cl-cc/runtime:val-symbol-p)
          ("function" cl-cc/runtime:+tag-function+ #'cl-cc/runtime:val-function-p)
          ("string"   cl-cc/runtime:+tag-string+   #'cl-cc/runtime:val-string-p))
  (tag pred)
  (let ((v (cl-cc/runtime:encode-pointer #x1000 tag)))
    (assert-true (funcall pred v))))

;;; ------------------------------------------------------------
;;; No collisions between types
;;; ------------------------------------------------------------

(deftest value-no-collision-nil-vs-char
  "nil constant and char base are distinct bit patterns."
  (assert-true (/= cl-cc/runtime:+val-nil+ cl-cc/runtime:+tag-char+)))

(deftest value-no-collision-cons-tag-vs-char
  "cons pointer upper-16 bits (#x7FFA) != char upper-16 bits (#x7FFE)."
  (let ((cons-v (cl-cc/runtime:encode-pointer 0 cl-cc/runtime:+tag-cons+))
        (char-v (cl-cc/runtime:encode-char (code-char 0))))
    (assert-true (/= (ash cons-v -48) (ash char-v -48)))))

(deftest value-no-collision-function-tag-vs-char
  "function pointer upper-16 bits (#x7FFC) != char upper-16 bits (#x7FFE)."
  (let ((fn-v   (cl-cc/runtime:encode-pointer 0 cl-cc/runtime:+tag-function+))
        (char-v (cl-cc/runtime:encode-char (code-char 0))))
    (assert-true (/= (ash fn-v -48) (ash char-v -48)))))

;;; ------------------------------------------------------------
;;; encode-bool
;;; ------------------------------------------------------------

(deftest-each value-encode-bool
  "encode-bool maps CL truthiness to val-t or val-nil."
  :cases (("t"      t   cl-cc/runtime:+val-t+)
          ("nil"    nil cl-cc/runtime:+val-nil+)
          ("truthy" 42  cl-cc/runtime:+val-t+))
  (cl-val expected-tag)
  (assert-= expected-tag (cl-cc/runtime:encode-bool cl-val)))

;;; ------------------------------------------------------------
;;; cl-value->val / val->cl-value round-trips
;;; ------------------------------------------------------------

(deftest value-interop-fixnum
  "cl-value->val / val->cl-value round-trip for fixnum."
  (assert-= 99 (cl-cc/runtime:val->cl-value (cl-cc/runtime:cl-value->val 99))))

(deftest value-interop-nil
  "cl-value->val nil gives +val-nil+; val->cl-value returns nil."
  (assert-= cl-cc/runtime:+val-nil+ (cl-cc/runtime:cl-value->val nil))
  (assert-equal nil (cl-cc/runtime:val->cl-value cl-cc/runtime:+val-nil+)))

(deftest value-interop-t
  "cl-value->val t gives +val-t+; val->cl-value returns t."
  (assert-= cl-cc/runtime:+val-t+ (cl-cc/runtime:cl-value->val t))
  (assert-equal t (cl-cc/runtime:val->cl-value cl-cc/runtime:+val-t+)))

(deftest value-interop-char
  "cl-value->val character round-trips."
  (assert-equal #\Z (cl-cc/runtime:val->cl-value (cl-cc/runtime:cl-value->val #\Z))))

#+sbcl
(deftest value-interop-double
  "cl-value->val double-float round-trips."
  (assert-equal 2.71828d0
                (cl-cc/runtime:val->cl-value (cl-cc/runtime:cl-value->val 2.71828d0))))
