;;;; packages/compile/tests/nan-boxing-tests.lisp
;;;; Unit tests for NaN-boxing value encoding in nan-boxing.lisp
;;;;
;;;; Covers: nan-box-fixnum, nan-box-pointer, nan-box-unbox round-trips,
;;;;   boundary cases (48-bit payload mask), tag discrimination.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest nan-box-fixnum-round-trips-zero
  "nan-box-fixnum on 0 encodes the fixnum tag and a zero payload."
  (multiple-value-bind (tag payload)
      (cl-cc/compile::nan-box-unbox (cl-cc/compile::nan-box-fixnum 0))
    (assert-= cl-cc/compile::+nan-box-tag-fixnum+ tag)
    (assert-= 0 payload)))

(deftest nan-box-fixnum-round-trips-positive
  "nan-box-fixnum on 42 recovers tag=fixnum and payload=42."
  (multiple-value-bind (tag payload)
      (cl-cc/compile::nan-box-unbox (cl-cc/compile::nan-box-fixnum 42))
    (assert-= cl-cc/compile::+nan-box-tag-fixnum+ tag)
    (assert-= 42 payload)))

(deftest nan-box-pointer-embeds-ptr-tag
  "nan-box-pointer encodes the pointer tag and preserves the low 48-bit address."
  (let ((ptr #x1234567890AB))
    (multiple-value-bind (tag payload)
        (cl-cc/compile::nan-box-unbox (cl-cc/compile::nan-box-pointer ptr))
      (assert-= cl-cc/compile::+nan-box-tag-ptr+ tag)
      (assert-= ptr payload))))

(deftest nan-box-float-preserves-ieee754-bits
  "nan-box-float on 1.0d0 returns the IEEE 754 bit pattern for 1.0."
  (let ((boxed (cl-cc/compile::nan-box-float 1.0d0)))
    (assert-= #x3FF0000000000000 boxed)))

(deftest nan-box-fixnum-payload-masked-to-48-bits
  "nan-box-fixnum masks the payload to 48 bits, discarding higher bits."
  (let* ((big-val (+ #xFFFFFFFFFFFF 1))
         (boxed (cl-cc/compile::nan-box-fixnum big-val)))
    (multiple-value-bind (tag payload)
        (cl-cc/compile::nan-box-unbox boxed)
      (assert-= cl-cc/compile::+nan-box-tag-fixnum+ tag)
      (assert-= (logand big-val #xFFFFFFFFFFFF) payload))))

(deftest nan-box-unbox-distinguishes-fixnum-from-pointer
  "nan-box-unbox returns different tags for fixnum vs pointer encodings."
  (multiple-value-bind (fixnum-tag ignore-f)
      (cl-cc/compile::nan-box-unbox (cl-cc/compile::nan-box-fixnum 1))
    (declare (ignore ignore-f))
    (multiple-value-bind (ptr-tag ignore-p)
        (cl-cc/compile::nan-box-unbox (cl-cc/compile::nan-box-pointer 1))
      (declare (ignore ignore-p))
      (assert-false (= fixnum-tag ptr-tag)))))
