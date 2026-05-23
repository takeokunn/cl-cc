;;;; packages/vm/src/numeric-fp-141.lisp — Phase 141: Numeric/FP Extension
;;;; FR-788 FMA, FR-789 Fast-Math, FR-790 BF16, FR-791 Interval Arithmetic

(in-package :cl-cc/vm)

;; FR-788: FMA
(defvar *fuse-fma* t)
(defun fma (a b c) (+ (* a b) c))

;; FR-789: Fast-Math
(defvar *fast-math* nil)
(defvar *associative-math* nil)

;; FR-790: Half-Precision / BF16
(defstruct float16 (bits 0 :type (unsigned-byte 16)))
(defstruct bfloat16 (bits 0 :type (unsigned-byte 16)))

;; FR-791: Interval Arithmetic
(defstruct interval (lo 0.0d0 :type double-float) (hi 0.0d0 :type double-float))
(defun interval-add (a b)
  (make-interval :lo (+ (interval-lo a) (interval-lo b))
                 :hi (+ (interval-hi a) (interval-hi b))))

(export '(*fuse-fma* fma *fast-math* *associative-math*
          float16 make-float16 bfloat16 make-bfloat16
          interval make-interval interval-add))
