;;;; src/runtime/value.lisp - NaN-Boxing Value Representation
;;;
;;; Simulates 64-bit NaN-boxing on top of SBCL using (unsigned-byte 64) integers.
;;; All VM instructions input and output boxed values (type VAL = (unsigned-byte 64)).
;;;
;;; Bit layout:
;;;
;;;   Fixnum:    bits[63:13] sign-extended integer, bits[12:0] = 0 (tag = 0000)
;;;              Encoding: (ash integer 13)  — 51-bit signed integers
;;;
;;;   Double:    Any bit pattern that is NOT a quiet NaN with our tag flag set.
;;;              Decoded by reinterpreting bits as IEEE 754 double via LDB.
;;;
;;;   Pointer:   Quiet NaN base (bits[62:51] = #x7FF, bit[50] = 1) + 3-bit tag in bits[50:48]
;;;              + 48-bit address in bits[47:0].
;;;              Base mask: #x7FF8000000000000
;;;              Tag in bits [50:48]:
;;;                001 = Object   (general heap object)
;;;                010 = Cons     (cons cell)
;;;                011 = Symbol
;;;                100 = Function (closure)
;;;                101 = String
;;;
;;;   Character: Quiet NaN base #x7FFE000000000000 + 21-bit codepoint in bits[20:0]
;;;
;;;   Nil:       #x7FFF000000000000
;;;   T:         #x7FFF000000000001
;;;   Unbound:   #x7FFF000000000002

(in-package :cl-cc/runtime)

(declaim (optimize (speed 3) (safety 1)))

;;; ------------------------------------------------------------
;;; Core bit-pattern constants
;;; ------------------------------------------------------------

;;; Quiet NaN base: exponent all-ones (bits[62:52]=7FF) + quiet bit (bit[51]=1).
;;; We use bit[50] as the "tagged pointer" discriminator — when set, this is a
;;; pointer value; when clear (but still NaN), it is a character or special.
(defconstant +nan-boxing-quiet-nan+  #x7FF8000000000000
  "IEEE 754 quiet NaN canonical bit pattern (positive, no payload).")

;;; Pointer values use +nan-boxing-quiet-nan+ as the base, with bits[50:48] = 000.
;;; OR-ing one of the 3-bit sub-tags (001..101) into bits[50:48] gives each kind
;;; a distinct upper-16-bit pattern:
;;;   object=001  → upper16=#x7FF9
;;;   cons=010    → upper16=#x7FFA
;;;   symbol=011  → upper16=#x7FFB
;;;   function=100→ upper16=#x7FFC
;;;   string=101  → upper16=#x7FFD
;;; Character (#x7FFE) and nil/t/unbound (#x7FFF) therefore cannot collide.
(defconstant +ptr-base+     #x7FF8000000000000
  "Base bit pattern for pointer-tagged values (= quiet NaN, bits[50:48]=000).")

(defconstant +ptr-mask+     #x7FFF000000000000
  "Mask covering the NaN prefix + all tag bits (bits[63:48]).")

(defconstant +tag-mask+     #x0007000000000000
  "Isolates the 3-bit sub-tag from a pointer value (bits[50:48]).")

(defconstant +addr-mask+    #x0000FFFFFFFFFFFF
  "Isolates the 48-bit address payload from a pointer value.")

;;; 3-bit pointer sub-tags (shifted to bits[50:48])
(defconstant +tag-object+   #x0001000000000000  "Heap object sub-tag.")
(defconstant +tag-cons+     #x0002000000000000  "Cons cell sub-tag.")
(defconstant +tag-symbol+   #x0003000000000000  "Symbol sub-tag.")
(defconstant +tag-function+ #x0004000000000000  "Function/closure sub-tag.")
(defconstant +tag-string+   #x0005000000000000  "String sub-tag.")

;;; Character: quiet NaN with bit[50] clear but bit[49] set.
;;; Base = #x7FFE_... gives bit[49]=1, bit[50]=0 → distinct from pointers.
(defconstant +tag-char+     #x7FFE000000000000
  "Base bit pattern for character immediate values.")

;;; Special singleton values (quiet NaN space, bits[50:49]=11).
(defconstant +val-nil+      #x7FFF000000000000  "Boxed NIL.")
(defconstant +val-t+        #x7FFF000000000001  "Boxed T.")
(defconstant +val-unbound+  #x7FFF000000000002  "Unbound-slot sentinel.")

;;; Fixnum: tag = 0 (bits[12:0] = 0), integer in bits[63:13].
;;; A valid boxed fixnum has its low 13 bits all zero.
(defconstant +fixnum-tag+   0)
(defconstant +fixnum-mask+  #x1FFF  "Low 13 bits; zero means fixnum.")
(defconstant +fixnum-shift+ 13      "Bits to shift integer for fixnum encoding.")

;;; Double detection: a double is any (unsigned-byte 64) that is NOT in our
;;; tagged NaN space.  Our tagged NaN space starts at #x7FF8000000000000
;;; with bit[50] or bit[49] set.  The simplest discriminant: if
;;; bits[62:49] = #x3FFE (i.e., upper 15 bits after bit63 = 0111_1111_1111_110x)
;;; then it is a pointer or special.  We detect doubles as "not NaN-tagged".
(defconstant +nan-tag-base+ #x7FF8000000000000
  "Minimum value whose upper bits indicate our NaN-tagged space.")

;;; ------------------------------------------------------------
;;; Type predicates  (inline for performance)
;;; ------------------------------------------------------------

(declaim (ftype (function ((unsigned-byte 64)) boolean)
                val-fixnum-p val-double-p val-pointer-p
                val-nil-p val-t-p val-char-p val-unbound-p
                val-object-p val-cons-p val-symbol-p val-function-p val-string-p))

(declaim (inline val-fixnum-p val-double-p val-pointer-p
                 val-nil-p val-t-p val-char-p val-unbound-p
                 val-object-p val-cons-p val-symbol-p val-function-p val-string-p))

(defun val-fixnum-p (v)
  "True if V is a boxed fixnum: low 13 bits all zero AND not in NaN-tagged space.

   The NaN-tagged space occupies [+nan-tag-base+, #x8000000000000000) — a narrow
   positive range with exponent bits all-ones.  Negative fixnums have bit 63 = 1
   (value >= #x8000000000000000) and therefore fall ABOVE the NaN-tagged range;
   they must not be rejected by the +nan-tag-base+ guard."
  (declare (type (unsigned-byte 64) v))
  (and (zerop (logand v +fixnum-mask+))
       ;; Exclude only the NaN-tagged range.  It is entirely positive (bit 63 = 0),
       ;; so values with bit 63 = 1 (negative fixnums) are never in NaN space.
       (not (and (zerop (ldb (byte 1 63) v))    ; positive (bit 63 = 0)
                 (>= v +nan-tag-base+)))))

(defun val-double-p (v)
  "True if V encodes an IEEE 754 double (not in our NaN-tagged space)."
  (declare (type (unsigned-byte 64) v))
  ;; A value is a double if it is NOT in our special NaN space.
  ;; Our NaN space: bits[62:51] = #x7FF and bit[50..49] != 00
  ;; (fixnums have low bits clear so they escape this).
  ;; Practical check: v >= +nan-tag-base+ and NOT fixnum and NOT char/ptr/special.
  (and (not (val-fixnum-p v))
       (< v +nan-tag-base+)))

(defun val-pointer-p (v)
  "True if V is any pointer-tagged value (object/cons/symbol/function/string).
   Pointer upper-16-bits range: #x7FF9 (object/tag=001) to #x7FFD (string/tag=101)."
  (declare (type (unsigned-byte 64) v))
  ;; Extract upper 16 bits and check they fall in the pointer sub-tag range.
  (let ((h (ash v -48)))
    (declare (type (unsigned-byte 16) h))
    (and (>= h #x7FF9) (<= h #x7FFD))))

(declaim (inline %val-ptr-tag))
(defun %val-ptr-tag (v)
  "Extract the 3-bit sub-tag from a pointer value."
  (declare (type (unsigned-byte 64) v))
  (logand v +tag-mask+))

(defun val-object-p (v)
  (declare (type (unsigned-byte 64) v))
  (= (logand v +ptr-mask+) (logior +ptr-base+ +tag-object+)))

(defun val-cons-p (v)
  (declare (type (unsigned-byte 64) v))
  (= (logand v +ptr-mask+) (logior +ptr-base+ +tag-cons+)))

(defun val-symbol-p (v)
  (declare (type (unsigned-byte 64) v))
  (= (logand v +ptr-mask+) (logior +ptr-base+ +tag-symbol+)))

(defun val-function-p (v)
  (declare (type (unsigned-byte 64) v))
  (= (logand v +ptr-mask+) (logior +ptr-base+ +tag-function+)))

(defun val-string-p (v)
  (declare (type (unsigned-byte 64) v))
  (= (logand v +ptr-mask+) (logior +ptr-base+ +tag-string+)))

(defun val-char-p (v)
  "True if V is a boxed character."
  (declare (type (unsigned-byte 64) v))
  (= (logand v #xFFFF000000000000) +tag-char+))

(defun val-nil-p (v)
  (declare (type (unsigned-byte 64) v))
  (= v +val-nil+))

(defun val-t-p (v)
  (declare (type (unsigned-byte 64) v))
  (= v +val-t+))

(defun val-unbound-p (v)
  (declare (type (unsigned-byte 64) v))
  (= v +val-unbound+))

;;; ------------------------------------------------------------
;;; Fixnum encode / decode
;;; ------------------------------------------------------------

(declaim (inline encode-fixnum decode-fixnum))

(defun encode-fixnum (n)
  "Box the signed integer N as a NaN-boxed fixnum.
   N must fit in 51 signed bits (range -2^50 .. 2^50-1)."
  (declare (type fixnum n))
  ;; Shift left by 13; mask to 64 bits to discard sign extension artifacts.
  (logand (ash n +fixnum-shift+) #xFFFFFFFFFFFFFFFF))

(defun decode-fixnum (v)
  "Unbox the NaN-boxed fixnum V to a signed CL integer."
  (declare (type (unsigned-byte 64) v))
  ;; Arithmetic right-shift: sign-extend from bit 63 down.
  (let ((raw (ash v (- +fixnum-shift+))))
    ;; raw is a non-negative integer (ash on unsigned); sign-extend bit 50.
    (if (logbitp 50 raw)
        (- raw (ash 1 51))
        raw)))

;;; ------------------------------------------------------------
;;; Double encode / decode
;;; ------------------------------------------------------------

(declaim (inline encode-double decode-double))

(defun encode-double (f)
  "Box the double-float F as a NaN-boxed value.
   Returns the IEEE 754 bit pattern as (unsigned-byte 64)."
  (declare (type double-float f))
  #+sbcl
  (logand (sb-kernel:double-float-bits f) #xFFFFFFFFFFFFFFFF)
  #-sbcl
  (let ((sign-bit (if (minusp (float-sign f)) #x8000000000000000 0)))
    (cond
      ((not (= f f)) #xFFF8000000000000)
      ((> f most-positive-double-float) (logior sign-bit #x7FF0000000000000))
      ((< f (- most-positive-double-float)) #xFFF0000000000000)
      ((zerop f) sign-bit)
      (t
       (multiple-value-bind (significand exponent sig-sign)
           (integer-decode-float f)
         (declare (ignore sig-sign))
         (let* ((biased-exp (+ exponent 52 1023)))
           (if (>= biased-exp 1)
               (logior sign-bit (ash biased-exp 52) (- significand (ash 1 52)))
               (logior sign-bit (ash significand (- biased-exp 1))))))))))

(defun decode-double (v)
  "Unbox NaN-boxed value V to a double-float."
  (declare (type (unsigned-byte 64) v))
  #+sbcl
  (let* ((high (ldb (byte 32 32) v))
         (high-signed (if (logbitp 31 high) (- high #x100000000) high)))
    (sb-kernel:make-double-float high-signed (ldb (byte 32 0) v)))
  #-sbcl
  (let* ((sign (ldb (byte 1 63) v))
         (biased-exp (ldb (byte 11 52) v))
         (mantissa (ldb (byte 52 0) v))
         (sign-factor (if (zerop sign) 1.0d0 -1.0d0)))
    (cond
      ((and (= biased-exp #x7FF) (not (zerop mantissa))) (sqrt -1.0d0))
      ((and (= biased-exp #x7FF) (zerop mantissa)) (* sign-factor (expt 2.0d0 1025)))
      ((and (zerop biased-exp) (zerop mantissa)) (* sign-factor 0.0d0))
      ((zerop biased-exp) (* sign-factor (scale-float (float mantissa 0.0d0) -1074)))
      (t (let ((significand (logior mantissa (ash 1 52))))
           (* sign-factor (scale-float (float significand 0.0d0) (- biased-exp 1075))))))))

;;; ------------------------------------------------------------
;;; Pointer encode / decode
;;; ------------------------------------------------------------

(declaim (inline encode-pointer decode-pointer pointer-tag))

(defun encode-pointer (addr tag)
  "Box the 48-bit heap ADDRESS with the given TAG constant.
   TAG must be one of +tag-object+, +tag-cons+, +tag-symbol+,
   +tag-function+, or +tag-string+."
  (declare (type (unsigned-byte 64) addr tag))
  (logior +ptr-base+ tag (logand addr +addr-mask+)))

(defun decode-pointer (v)
  "Extract the 48-bit heap address from a pointer-tagged NaN-boxed value."
  (declare (type (unsigned-byte 64) v))
  (logand v +addr-mask+))

(defun pointer-tag (v)
  "Return the 3-bit sub-tag word from a pointer-tagged NaN-boxed value."
  (declare (type (unsigned-byte 64) v))
  (logand v +tag-mask+))

;;; ------------------------------------------------------------
;;; Character encode / decode
;;; ------------------------------------------------------------

(declaim (inline encode-char decode-char))

(defun encode-char (c)
  "Box the CL character C as a NaN-boxed character immediate."
  (declare (type character c))
  (logior +tag-char+ (char-code c)))

(defun decode-char (v)
  "Unbox NaN-boxed character V to a CL character."
  (declare (type (unsigned-byte 64) v))
  (code-char (logand v #x1FFFFF)))

;;; ------------------------------------------------------------
;;; Boolean encode
;;; ------------------------------------------------------------

(declaim (inline encode-bool))

(defun encode-bool (x)
  "Convert a CL generalised boolean to +val-t+ or +val-nil+."
  (if x +val-t+ +val-nil+))

;;; ------------------------------------------------------------
;;; CL value <-> NaN-boxed val interop
;;; ------------------------------------------------------------

(defun cl-value->val (x)
  "Convert a host CL value X to its NaN-boxed representation.

   Supported types:
     fixnum        → encode-fixnum
     double-float  → encode-double
     single-float  → encode-double (promoted)
     character     → encode-char
     null (nil)    → +val-nil+
     (eql t)       → +val-t+
     integer       → encode-fixnum (if in range), else error
     otherwise     → encode-pointer with +tag-object+ using
                     (sxhash x) as a placeholder address.
                     (Real use: pass the GC-managed address instead.)"
  (etypecase x
    (null    +val-nil+)
    ((eql t) +val-t+)
    (fixnum  (encode-fixnum x))
    (integer
     ;; Arbitrary-precision integers: only support 51-bit range here.
     (if (and (>= x (- (expt 2 50))) (< x (expt 2 50)))
         (encode-fixnum x)
         (error "cl-value->val: integer ~A out of 51-bit fixnum range" x)))
    (double-float (encode-double x))
    (single-float (encode-double (float x 1.0d0)))
    (character    (encode-char x))
    ;; For heap objects (strings, conses, etc.) we cannot know the GC address
    ;; at this layer; callers should use encode-pointer directly.
    (t
     (error "cl-value->val: cannot box value of type ~S (use encode-pointer for heap objects)"
            (type-of x)))))

(defun val->cl-value (v)
  "Convert a NaN-boxed value V back to a host CL value.

   Pointer types return the raw 48-bit integer address (the caller must
   dereference through the GC heap)."
  (declare (type (unsigned-byte 64) v))
  (cond
    ((= v +val-nil+)     nil)
    ((= v +val-t+)       t)
    ((= v +val-unbound+) (error "val->cl-value: unbound slot"))
    ((val-fixnum-p v)    (decode-fixnum v))
    ((val-char-p v)      (decode-char v))
    ((val-double-p v)    (decode-double v))
    ((val-pointer-p v)   (decode-pointer v))
    (t
     (error "val->cl-value: unrecognised bit pattern #x~16,'0X" v))))
