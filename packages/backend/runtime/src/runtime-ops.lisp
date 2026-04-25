;;;; runtime-ops.lisp — Array, arithmetic, bitwise, comparison, and math rt-* wrappers
;;;
;;; Extracted from runtime.lisp.
;;; Contains: Arrays/Vectors, Arithmetic, Bitwise, Comparisons, Math sections.
;;;
;;; Depends on runtime.lisp (define-rt-predicate, define-rt-binary-predicate).
;;; Load order: immediately after runtime.lisp.

(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; Arrays / Vectors
;;; ------------------------------------------------------------

(defun rt-make-array (dims &key (element-type t) initial-element fill-pointer adjustable)
  (if initial-element
      (make-array dims :element-type element-type :initial-element initial-element
                  :fill-pointer fill-pointer :adjustable adjustable)
      (make-array dims :element-type element-type
                  :fill-pointer fill-pointer :adjustable adjustable)))

(defun rt-aref (a &rest indices) (apply #'aref a indices))
(defun rt-aset (a &rest indices-then-value)
  (let ((indices (butlast indices-then-value))
        (val (car (last indices-then-value))))
    (apply #'(setf aref) val a indices)))
(defun rt-array-length (a) (length a))
(defun rt-array-rank (a) (array-rank a))
(defun rt-array-dimension (a axis) (array-dimension a axis))
(defun rt-array-dimensions (a) (array-dimensions a))
(defun rt-array-total-size (a) (array-total-size a))
(defun rt-row-major-aref (a idx) (row-major-aref a idx))
(defun rt-array-row-major-index (a &rest indices) (apply #'array-row-major-index a indices))
(defun rt-vector-push (val vec) (vector-push val vec))
(defun rt-vector-push-extend (val vec) (vector-push-extend val vec))
(defun rt-vector-pop (vec) (vector-pop vec))
(defun rt-fill-pointer (vec) (fill-pointer vec))
(defun rt-set-fill-pointer (vec n) (setf (fill-pointer vec) n))
(define-rt-predicate rt-array-has-fill-pointer-p array-has-fill-pointer-p)
(define-rt-predicate rt-array-adjustable-p       adjustable-array-p)
(defun rt-adjust-array (arr dims &rest kwargs) (apply #'adjust-array arr dims kwargs))
(defun rt-array-displacement (arr) (array-displacement arr))
(defun rt-svref (vec idx) (svref vec idx))
(defun rt-svset (vec idx val) (setf (svref vec idx) val))

;; Bit arrays
(defun rt-bit-access (bv idx) (bit bv idx))
(defun rt-bit-set (bv idx val) (setf (bit bv idx) val))
(defun rt-bit-and (a b) (bit-and a b))
(defun rt-bit-or (a b) (bit-ior a b))
(defun rt-bit-xor (a b) (bit-xor a b))
(defun rt-bit-not (a) (bit-not a))
(defun rt-sbit (sv idx) (sbit sv idx))

;;; ------------------------------------------------------------
;;; Arithmetic
;;; ------------------------------------------------------------

(defun rt-add (a b) (+ a b))
(defun rt-sub (a b) (- a b))
(defun rt-mul (a b) (* a b))
(defun rt-div (a b) (/ a b))
(defun rt-mod (a b) (mod a b))
(defun rt-rem (a b) (rem a b))
(defun rt-neg (a) (- a))
(defun rt-abs (a) (abs a))
(defun rt-inc (a) (1+ a))
(defun rt-dec (a) (1- a))
(defun rt-cl-and (a b) (and a b))
(defun rt-cl-or (a b) (or a b))
(defun rt-not (x) (if x 0 1))
(define-rt-predicate rt-evenp  evenp)
(define-rt-predicate rt-oddp   oddp)
(define-rt-predicate rt-zerop  zerop)
(define-rt-predicate rt-plusp  plusp)
(define-rt-predicate rt-minusp minusp)

;;; ------------------------------------------------------------
;;; Bitwise
;;; ------------------------------------------------------------

(defun rt-ash (n count) (ash n count))
(defun rt-logand (a b) (logand a b))
(defun rt-logior (a b) (logior a b))
(defun rt-logxor (a b) (logxor a b))
(defun rt-logeqv (a b) (logeqv a b))
(defun rt-lognot (a) (lognot a))
(defun rt-logtest (a b) (if (logtest a b) 1 0))
(defun rt-logbitp (idx n) (if (logbitp idx n) 1 0))
(defun rt-logcount (n) (logcount n))
(defun rt-integer-length (n) (integer-length n))

;;; ------------------------------------------------------------
;;; Comparisons
;;; ------------------------------------------------------------

(define-rt-binary-predicate rt-eq       eq)
(define-rt-binary-predicate rt-eql      eql)
(define-rt-binary-predicate rt-equal-fn equal)
(define-rt-binary-predicate rt-lt       <)
(define-rt-binary-predicate rt-gt       >)
(define-rt-binary-predicate rt-le       <=)
(define-rt-binary-predicate rt-ge       >=)
(define-rt-binary-predicate rt-num-eq   =)

;;; ------------------------------------------------------------
;;; Math
;;; ------------------------------------------------------------

(defun rt-expt (base exp) (expt base exp))
(defun rt-sqrt (x) (sqrt x))
(defun rt-exp (x) (exp x))
(defun rt-log (x) (log x))
(defun rt-sin (x) (sin x))
(defun rt-cos (x) (cos x))
(defun rt-tan (x) (tan x))
(defun rt-asin (x) (asin x))
(defun rt-acos (x) (acos x))
(defun rt-atan (x) (atan x))
(defun rt-atan2 (y x) (atan y x))
(defun rt-sinh (x) (sinh x))
(defun rt-cosh (x) (cosh x))
(defun rt-tanh (x) (tanh x))
(defun rt-floor (x) (floor x))
(defun rt-ceiling (x) (ceiling x))
(defun rt-truncate (x) (truncate x))
(defun rt-round (x) (round x))
(defun rt-ffloor (x) (ffloor x))
(defun rt-fceiling (x) (fceiling x))
(defun rt-ftruncate (x) (ftruncate x))
(defun rt-fround (x) (fround x))
(defun rt-float (x) (float x))
(defun rt-float-precision (x) (float-precision x))
(defun rt-float-radix (x) (float-radix x))
(defun rt-float-sign (x) (float-sign x))
(defun rt-float-digits (x) (float-digits x))
(defun rt-decode-float (x) (decode-float x))
(defun rt-integer-decode-float (x) (integer-decode-float x))
(defun rt-scale-float (x k) (scale-float x k))
(defun rt-rational (x) (rational x))
(defun rt-rationalize (x) (rationalize x))
(defun rt-numerator (x) (numerator x))
(defun rt-denominator (x) (denominator x))
(defun rt-realpart (x) (realpart x))
(defun rt-imagpart (x) (imagpart x))
(defun rt-conjugate (x) (conjugate x))
(defun rt-phase (x) (phase x))
(defun rt-complex (r i) (complex r i))
(defun rt-gcd (a b) (gcd a b))
(defun rt-lcm (a b) (lcm a b))
