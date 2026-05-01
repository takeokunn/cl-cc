;;;; packages/runtime/src/value-codec.lisp - NaN-Boxing Encode/Decode Codecs
;;;
;;; Extracted from value.lisp.
;;; Contains: fixnum/double/pointer/character encode+decode,
;;;           encode-bool, cl-value<->val interop.
;;;
;;; Depends on value.lisp (constants: +fixnum-shift+, +fixnum-mask+,
;;;   +nan-tag-base+, +ptr-base+, +tag-mask+, +addr-mask+,
;;;   +tag-char+, +val-nil+, +val-t+, +val-unbound+;
;;;   predicates: val-fixnum-p, val-char-p, val-double-p, val-pointer-p).
;;; Load order: immediately after value.lisp.

(in-package :cl-cc/runtime)

(declaim (optimize (speed 3) (safety 1)))

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
  (logand (sb-kernel:double-float-bits f) #xFFFFFFFFFFFFFFFF))

(defun decode-double (v)
  "Unbox NaN-boxed value V to a double-float."
  (declare (type (unsigned-byte 64) v))
  (let* ((high (ldb (byte 32 32) v))
         (high-signed (if (logbitp 31 high) (- high #x100000000) high)))
    (sb-kernel:make-double-float high-signed (ldb (byte 32 0) v))))

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
  "Convert a host CL value X to its NaN-boxed representation."
  (etypecase x
    (null    +val-nil+)
    ((eql t) +val-t+)
    (fixnum  (encode-fixnum x))
    (integer
     (if (and (>= x (- (expt 2 50))) (< x (expt 2 50)))
         (encode-fixnum x)
         (error "cl-value->val: integer ~A out of 51-bit fixnum range" x)))
    (double-float (encode-double x))
    (single-float (encode-double (float x 1.0d0)))
    (character    (encode-char x))
    (t
     (error "cl-value->val: cannot box value of type ~S (use encode-pointer for heap objects)"
            (type-of x)))))

(defun val->cl-value (v)
  "Convert a NaN-boxed value V back to a host CL value.
   Pointer types return the raw 48-bit integer address."
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
