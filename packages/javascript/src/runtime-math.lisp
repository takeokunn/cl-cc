;;;; packages/javascript/src/runtime-math.lisp — JS Math built-ins
;;;;
;;;; All Math.* functions. Extracted from runtime-string.lisp so that
;;;; string and numeric concerns are separated (SRP).

(in-package :cl-cc/javascript)

;;; ─── Rounding ─────────────────────────────────────────────────────────────────
;;;
;;; Pass NaN / Infinity through unchanged; otherwise coerce to double-float.

(defmacro define-js-math-rounding (name cl-fn)
  `(defun ,name (x)
     (let ((n (%js-to-number x)))
       (if (or (%js-float-nan-p n) (%js-float-infinity-p n))
           n
           (coerce (,cl-fn n) 'double-float)))))

(define-js-math-rounding %js-math-floor  floor)
(define-js-math-rounding %js-math-ceil   ceiling)
(define-js-math-rounding %js-math-trunc  truncate)

;;; Math.round rounds half away from -Infinity: (floor (+ n 0.5)), not CL's round.
(defun %js-math-round (x)
  (let ((n (%js-to-number x)))
    (if (or (%js-float-nan-p n) (%js-float-infinity-p n))
        n
        (coerce (floor (+ n 0.5d0)) 'double-float))))

;;; ─── Trigonometry ─────────────────────────────────────────────────────────────

(defmacro define-js-math-trig (name cl-fn)
  `(defun ,name (x) (,cl-fn (%js-to-number x))))

(define-js-math-trig %js-math-sin  sin)
(define-js-math-trig %js-math-cos  cos)
(define-js-math-trig %js-math-tan  tan)
(define-js-math-trig %js-math-asin asin)
(define-js-math-trig %js-math-acos acos)
(define-js-math-trig %js-math-atan atan)

(defun %js-math-atan2 (y x)
  (atan (%js-to-number y) (%js-to-number x)))

;;; ─── Numeric utilities ────────────────────────────────────────────────────────

(defun %js-math-abs (x)
  (let ((n (%js-to-number x)))
    (if (%js-float-nan-p n) n (abs n))))

(defun %js-math-sign (x)
  (let ((n (%js-to-number x)))
    (cond ((%js-float-nan-p n) n)
          ((> n 0)  1.0d0)
          ((< n 0) -1.0d0)
          (t         0.0d0))))

;;; ─── Variadic min/max ─────────────────────────────────────────────────────────

(defun %js-math-max (&rest args)
  (if (null args)
      *js-neg-inf-float*
      (reduce (lambda (a b)
                (let ((na (%js-to-number a))
                      (nb (%js-to-number b)))
                  (if (or (%js-float-nan-p na) (%js-float-nan-p nb))
                      *js-nan-float*
                      (max na nb))))
              args)))

(defun %js-math-min (&rest args)
  (if (null args)
      *js-inf-float*
      (reduce (lambda (a b)
                (let ((na (%js-to-number a))
                      (nb (%js-to-number b)))
                  (if (or (%js-float-nan-p na) (%js-float-nan-p nb))
                      *js-nan-float*
                      (min na nb))))
              args)))

;;; ─── Power / roots / logs ─────────────────────────────────────────────────────

(defun %js-math-pow  (base exp) (expt (%js-to-number base) (%js-to-number exp)))
(defun %js-math-sqrt (x) (let ((n (%js-to-number x))) (if (< n 0) *js-nan-float* (sqrt n))))
(defun %js-math-exp  (x) (exp (%js-to-number x)))
(defun %js-math-log  (x) (let ((n (%js-to-number x))) (if (< n 0) *js-nan-float* (log n))))
(defun %js-math-log2 (x) (let ((n (%js-to-number x))) (if (< n 0) *js-nan-float* (log n 2.0d0))))
(defun %js-math-log10(x) (let ((n (%js-to-number x))) (if (< n 0) *js-nan-float* (log n 10.0d0))))

(defun %js-math-hypot (&rest args)
  (sqrt (reduce #'+ (mapcar (lambda (x) (let ((n (%js-to-number x))) (* n n))) args)
                :initial-value 0.0d0)))

;;; ─── Bit-level / integer ──────────────────────────────────────────────────────

(defun %js-math-random () (random 1.0d0))

(defun %js-math-clz32 (x)
  "Count leading zeros in the 32-bit representation."
  (let* ((n (logand (truncate (%js-to-number x)) #xFFFFFFFF))
         (count 0))
    (if (zerop n)
        32
        (progn
          (loop for bit from 31 downto 0
                while (zerop (logand n (ash 1 bit)))
                do (incf count))
          count))))

(defun %js-math-fround (x)
  (coerce (coerce (%js-to-number x) 'single-float) 'double-float))

(defun %js-round-half-even (x)
  (multiple-value-bind (whole frac) (floor x)
    (cond ((< frac 0.5d0) whole)
          ((> frac 0.5d0) (1+ whole))
          ((evenp whole) whole)
          (t (1+ whole)))))

(defun %js-f16round-number (x)
  (let* ((n (%js-to-number x))
         (d (coerce n 'double-float)))
    (cond
      ((%js-float-nan-p d) d)
      ((%js-float-infinity-p d) d)
      ((zerop d) d)
      (t
       (let* ((sign (if (minusp d) -1.0d0 1.0d0))
              (abs-n (abs d))
              (rounded
                (if (< abs-n (scale-float 1.0d0 -14))
                    (* (%js-round-half-even (/ abs-n (scale-float 1.0d0 -24)))
                       (scale-float 1.0d0 -24))
                    (let* ((exp (floor (log abs-n 2.0d0)))
                           (step (scale-float 1.0d0 (- exp 10))))
                      (* (%js-round-half-even (/ abs-n step)) step)))))
         (cond ((zerop rounded) (* sign 0.0d0))
               ((> rounded 65504.0d0) (if (plusp sign) *js-inf-float* *js-neg-inf-float*))
               (t (* sign rounded))))))))

(defun %js-math-f16round (x)
  (%js-f16round-number x))

(defun %js-math-imul (a b)
  "32-bit integer multiply, sign-extended."
  (let* ((ia (logand (truncate (%js-to-number a)) #xFFFFFFFF))
         (ib (logand (truncate (%js-to-number b)) #xFFFFFFFF))
         (result (logand (* ia ib) #xFFFFFFFF)))
    (if (logbitp 31 result)
        (- result #x100000000)
        result)))
