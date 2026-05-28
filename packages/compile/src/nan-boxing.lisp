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
  "Box a float value for NaN-boxed representation."
  #+sbcl
  (sb-kernel:double-float-bits float-val)
  #-sbcl
  (error "NAN-BOX-FLOAT requires SBCL double-float bit access: ~S" float-val))

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
