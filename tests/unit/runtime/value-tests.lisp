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

(deftest value-nil-constant
  "NIL sentinel has expected bit pattern."
  (assert-= #x7FFF000000000000 cl-cc/runtime:+val-nil+))

(deftest value-t-constant
  "T sentinel has expected bit pattern."
  (assert-= #x7FFF000000000001 cl-cc/runtime:+val-t+))

(deftest value-unbound-constant
  "Unbound sentinel has expected bit pattern."
  (assert-= #x7FFF000000000002 cl-cc/runtime:+val-unbound+))

;;; ------------------------------------------------------------
;;; val-nil-p / val-t-p / val-unbound-p
;;; ------------------------------------------------------------

(deftest value-nil-p-true
  "val-nil-p recognises +val-nil+."
  (assert-true (cl-cc/runtime:val-nil-p cl-cc/runtime:+val-nil+)))

(deftest value-nil-p-false-for-t
  "val-nil-p rejects +val-t+."
  (assert-false (cl-cc/runtime:val-nil-p cl-cc/runtime:+val-t+)))

(deftest value-t-p-true
  "val-t-p recognises +val-t+."
  (assert-true (cl-cc/runtime:val-t-p cl-cc/runtime:+val-t+)))

(deftest value-t-p-false-for-nil
  "val-t-p rejects +val-nil+."
  (assert-false (cl-cc/runtime:val-t-p cl-cc/runtime:+val-nil+)))

(deftest value-unbound-p-true
  "val-unbound-p recognises +val-unbound+."
  (assert-true (cl-cc/runtime:val-unbound-p cl-cc/runtime:+val-unbound+)))

(deftest value-unbound-p-false-for-nil
  "val-unbound-p rejects +val-nil+."
  (assert-false (cl-cc/runtime:val-unbound-p cl-cc/runtime:+val-nil+)))

;;; ------------------------------------------------------------
;;; Fixnum encode/decode round-trip
;;; ------------------------------------------------------------

(deftest value-fixnum-encode-zero
  "encode-fixnum 0 decodes back to 0."
  (assert-= 0 (cl-cc/runtime:decode-fixnum (cl-cc/runtime:encode-fixnum 0))))

(deftest value-fixnum-encode-positive
  "encode-fixnum positive integer round-trips."
  (assert-= 42 (cl-cc/runtime:decode-fixnum (cl-cc/runtime:encode-fixnum 42))))

(deftest value-fixnum-encode-negative
  "encode-fixnum negative integer round-trips."
  (assert-= -1 (cl-cc/runtime:decode-fixnum (cl-cc/runtime:encode-fixnum -1))))

(deftest value-fixnum-encode-large-positive
  "encode-fixnum near max 51-bit positive round-trips."
  (let ((n (1- (expt 2 50))))
    (assert-= n (cl-cc/runtime:decode-fixnum (cl-cc/runtime:encode-fixnum n)))))

(deftest value-fixnum-encode-large-negative
  "encode-fixnum near min 51-bit negative round-trips."
  (let ((n (- (expt 2 50))))
    (assert-= n (cl-cc/runtime:decode-fixnum (cl-cc/runtime:encode-fixnum n)))))

;;; ------------------------------------------------------------
;;; val-fixnum-p
;;; ------------------------------------------------------------

(deftest value-fixnum-p-zero
  "val-fixnum-p recognises encoded 0."
  (assert-true (cl-cc/runtime:val-fixnum-p (cl-cc/runtime:encode-fixnum 0))))

(deftest value-fixnum-p-positive
  "val-fixnum-p recognises encoded positive."
  (assert-true (cl-cc/runtime:val-fixnum-p (cl-cc/runtime:encode-fixnum 100))))

(deftest value-fixnum-p-negative
  "val-fixnum-p recognises encoded negative."
  (assert-true (cl-cc/runtime:val-fixnum-p (cl-cc/runtime:encode-fixnum -100))))

(deftest value-fixnum-p-rejects-nil
  "val-fixnum-p rejects +val-nil+."
  (assert-false (cl-cc/runtime:val-fixnum-p cl-cc/runtime:+val-nil+)))

(deftest value-fixnum-p-rejects-t
  "val-fixnum-p rejects +val-t+."
  (assert-false (cl-cc/runtime:val-fixnum-p cl-cc/runtime:+val-t+)))

;;; ------------------------------------------------------------
;;; Character encode/decode round-trip
;;; ------------------------------------------------------------

(deftest value-char-encode-a
  "encode-char #\\A decodes back to #\\A."
  (assert-equal #\A (cl-cc/runtime:decode-char (cl-cc/runtime:encode-char #\A))))

(deftest value-char-encode-nul
  "encode-char NUL (codepoint 0) decodes back."
  (assert-equal (code-char 0)
                (cl-cc/runtime:decode-char (cl-cc/runtime:encode-char (code-char 0)))))

(deftest value-char-encode-unicode
  "encode-char Unicode codepoint round-trips."
  (let ((c (code-char #x1F600)))          ; emoji codepoint
    (assert-equal c (cl-cc/runtime:decode-char (cl-cc/runtime:encode-char c)))))

;;; ------------------------------------------------------------
;;; val-char-p
;;; ------------------------------------------------------------

(deftest value-char-p-true
  "val-char-p recognises an encoded character."
  (assert-true (cl-cc/runtime:val-char-p (cl-cc/runtime:encode-char #\x))))

(deftest value-char-p-false-for-nil
  "val-char-p rejects +val-nil+."
  (assert-false (cl-cc/runtime:val-char-p cl-cc/runtime:+val-nil+)))

(deftest value-char-p-false-for-fixnum
  "val-char-p rejects an encoded fixnum."
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

(deftest value-double-p-false-for-fixnum
  "val-double-p rejects an encoded fixnum."
  (assert-false (cl-cc/runtime:val-double-p (cl-cc/runtime:encode-fixnum 42))))

(deftest value-double-p-false-for-nil
  "val-double-p rejects +val-nil+."
  (assert-false (cl-cc/runtime:val-double-p cl-cc/runtime:+val-nil+)))

;;; ------------------------------------------------------------
;;; Pointer encode/decode
;;; ------------------------------------------------------------

(deftest value-pointer-encode-object
  "encode-pointer with +tag-object+ round-trips address."
  (let ((addr #x0000DEADBEEF))
    (let ((v (cl-cc/runtime:encode-pointer addr cl-cc/runtime:+tag-object+)))
      (assert-= addr (cl-cc/runtime:decode-pointer v)))))

(deftest value-pointer-encode-cons
  "encode-pointer with +tag-cons+ round-trips address."
  (let ((addr #x0000CAFE1234))
    (let ((v (cl-cc/runtime:encode-pointer addr cl-cc/runtime:+tag-cons+)))
      (assert-= addr (cl-cc/runtime:decode-pointer v)))))

(deftest value-pointer-encode-function
  "encode-pointer with +tag-function+ round-trips address."
  (let ((addr #x0000000100FF))
    (let ((v (cl-cc/runtime:encode-pointer addr cl-cc/runtime:+tag-function+)))
      (assert-= addr (cl-cc/runtime:decode-pointer v)))))

;;; ------------------------------------------------------------
;;; val-pointer-p / sub-tag predicates
;;; ------------------------------------------------------------

(deftest value-pointer-p-object
  "val-pointer-p recognises object-tagged pointer."
  (let ((v (cl-cc/runtime:encode-pointer #x1000 cl-cc/runtime:+tag-object+)))
    (assert-true (cl-cc/runtime:val-pointer-p v))))

(deftest value-pointer-p-cons
  "val-pointer-p recognises cons-tagged pointer."
  (let ((v (cl-cc/runtime:encode-pointer #x1000 cl-cc/runtime:+tag-cons+)))
    (assert-true (cl-cc/runtime:val-pointer-p v))))

(deftest value-pointer-p-string
  "val-pointer-p recognises string-tagged pointer."
  (let ((v (cl-cc/runtime:encode-pointer #x1000 cl-cc/runtime:+tag-string+)))
    (assert-true (cl-cc/runtime:val-pointer-p v))))

(deftest value-pointer-p-false-for-nil
  "val-pointer-p rejects +val-nil+ (special, not pointer)."
  (assert-false (cl-cc/runtime:val-pointer-p cl-cc/runtime:+val-nil+)))

(deftest value-pointer-p-false-for-char
  "val-pointer-p rejects an encoded character (char range, not pointer range)."
  (assert-false (cl-cc/runtime:val-pointer-p (cl-cc/runtime:encode-char #\A))))

(deftest value-object-p-true
  "val-object-p recognises +tag-object+."
  (let ((v (cl-cc/runtime:encode-pointer #x1000 cl-cc/runtime:+tag-object+)))
    (assert-true (cl-cc/runtime:val-object-p v))))

(deftest value-object-p-false-for-cons
  "val-object-p rejects +tag-cons+."
  (let ((v (cl-cc/runtime:encode-pointer #x1000 cl-cc/runtime:+tag-cons+)))
    (assert-false (cl-cc/runtime:val-object-p v))))

(deftest value-cons-p-true
  "val-cons-p recognises +tag-cons+."
  (let ((v (cl-cc/runtime:encode-pointer #x1000 cl-cc/runtime:+tag-cons+)))
    (assert-true (cl-cc/runtime:val-cons-p v))))

(deftest value-symbol-p-true
  "val-symbol-p recognises +tag-symbol+."
  (let ((v (cl-cc/runtime:encode-pointer #x1000 cl-cc/runtime:+tag-symbol+)))
    (assert-true (cl-cc/runtime:val-symbol-p v))))

(deftest value-function-p-true
  "val-function-p recognises +tag-function+."
  (let ((v (cl-cc/runtime:encode-pointer #x1000 cl-cc/runtime:+tag-function+)))
    (assert-true (cl-cc/runtime:val-function-p v))))

(deftest value-string-p-true
  "val-string-p recognises +tag-string+."
  (let ((v (cl-cc/runtime:encode-pointer #x1000 cl-cc/runtime:+tag-string+)))
    (assert-true (cl-cc/runtime:val-string-p v))))

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

(deftest value-encode-bool-true
  "encode-bool on true CL value gives +val-t+."
  (assert-= cl-cc/runtime:+val-t+ (cl-cc/runtime:encode-bool t)))

(deftest value-encode-bool-false
  "encode-bool on nil gives +val-nil+."
  (assert-= cl-cc/runtime:+val-nil+ (cl-cc/runtime:encode-bool nil)))

(deftest value-encode-bool-truthy
  "encode-bool on any non-nil value gives +val-t+."
  (assert-= cl-cc/runtime:+val-t+ (cl-cc/runtime:encode-bool 42)))

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
