;;;; packages/compile/src/nan-boxing.lisp — FR-674 NaN Boxing
;;;; Encode pointers and small values into IEEE 754 NaN payload space.
;;;; JavaScriptCore / LuaJIT / V8 NaN-boxing equivalent.

(in-package :cl-cc/compile)

;; NaN boxing: IEEE 754 double has 2^53-1 NaN values.
;; We use the NaN payload (51 bits) to store pointers, fixnums, etc.
;; Only actual NaN floats use the full representation.

(defconstant +nan-box-mask+ #x7FF8000000000000
  "NaN tag mask: exponent all 1s, quiet NaN bit set.")

(defconstant +nan-box-tag-float+ 0
  "Tag: genuine float (NaN with payload 0).")

(defconstant +nan-box-tag-fixnum+ 1
  "Tag: fixnum encoded in payload.")

(defconstant +nan-box-tag-ptr+ 2
  "Tag: heap pointer encoded in payload.")

(defun nan-box-float (float-val)
  "Box a float value for NaN-boxed representation.
Uses portable IEEE 754 bit extraction via integer-decode-float arithmetic."
  (multiple-value-bind (significand exponent sign)
      (integer-decode-float (coerce float-val 'double-float))
    (let* ((biased-exp (+ exponent 52 1023))
           (sign-bit   (if (minusp sign) 1 0))
           (exp-bits   (max 0 (min 2047 biased-exp)))
           (frac-bits  (logand significand (1- (ash 1 52)))))
      (logior (ash sign-bit 63)
              (ash exp-bits 52)
              frac-bits))))

(defun nan-box-fixnum (fixnum-val)
  "Box a fixnum into NaN space."
  (logior +nan-box-mask+
          (ash +nan-box-tag-fixnum+ 48)
          (logand fixnum-val #xFFFFFFFFFFFF)))

(defun nan-box-pointer (ptr)
  "Box a heap pointer into NaN space."
  (logior +nan-box-mask+
          (ash +nan-box-tag-ptr+ 48)
          (logand ptr #xFFFFFFFFFFFF)))

(defun nan-box-unbox (boxed)
  "Unbox a NaN-boxed value. Returns (values tag payload)."
  (if (= (logand boxed +nan-box-mask+) +nan-box-mask+)
      (let ((tag (ldb (byte 3 48) boxed)))
        (values tag (logand boxed #xFFFFFFFFFFFF)))
      (values :raw boxed)))
