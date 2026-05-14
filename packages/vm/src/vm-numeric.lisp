(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Numeric Tower Extensions
;;;
;;; Contains: round (FR-301), float operations (FR-305), float decode/inspect,
;;; float rounding (ffloor/fceiling/ftruncate/fround), environment predicates
;;; (FR-1202), random (FR-1205), universal-time ops (FR-1204),
;;; rational numbers (FR-306), complex numbers (FR-307).
;;;
;;; Bitwise operations (FR-303) → packages/vm/src/vm-bitwise.lisp
;;; Transcendental functions (FR-304) → packages/vm/src/vm-transcendental.lisp
;;;
;;; Load order: after vm-transcendental.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ------------------------------------------------------------
;;; Phase 2: Numeric Tower (FR-301 to FR-305)
;;; ------------------------------------------------------------

(defparameter +vm-bignum-digit-base+ 1000000000
  "Base used by VM bignum planning helpers; each digit fits comfortably in a fixnum.")

(defparameter +vm-bignum-karatsuba-threshold+ 64
  "Digit-count threshold where bignum multiplication planning selects Karatsuba.")

(defun vm-bignum-digit-vector (integer &optional (base +vm-bignum-digit-base+))
  "Return INTEGER as a little-endian digit vector and sign using BASE."
  (check-type integer integer)
  (let ((n (abs integer))
        (digits nil))
    (if (zerop n)
        (values (vector 0) 0)
        (progn
          (loop while (> n 0)
                do (multiple-value-bind (q r) (floor n base)
                     (push r digits)
                     (setf n q)))
          (values (coerce (nreverse digits) 'vector)
                  (if (minusp integer) -1 1))))))

(defun %vm-trim-bignum-digits (digits)
  (let ((end (1- (length digits))))
    (loop while (and (> end 0) (zerop (aref digits end)))
          do (decf end))
    (subseq digits 0 (1+ end))))

(defun vm-bignum-schoolbook-multiply-digits
    (lhs rhs &optional (base +vm-bignum-digit-base+))
  "Multiply little-endian digit vectors LHS and RHS with schoolbook arithmetic."
  (let ((result (make-array (+ (length lhs) (length rhs)) :initial-element 0)))
    (dotimes (i (length lhs))
      (let ((carry 0))
        (dotimes (j (length rhs))
          (let* ((index (+ i j))
                 (total (+ (aref result index)
                           (* (aref lhs i) (aref rhs j))
                           carry)))
            (multiple-value-bind (q r) (floor total base)
              (setf (aref result index) r
                    carry q))))
        (loop for index from (+ i (length rhs))
              while (> carry 0)
              do (let ((total (+ (aref result index) carry)))
                   (multiple-value-bind (q r) (floor total base)
                     (setf (aref result index) r
                           carry q))))))
    (%vm-trim-bignum-digits result)))

(defun vm-bignum-multiplication-strategy
    (lhs rhs &key (threshold +vm-bignum-karatsuba-threshold+))
  "Return the planned multiplication strategy for integer operands LHS and RHS."
  (check-type lhs integer)
  (check-type rhs integer)
  (if (typep (* lhs rhs) 'fixnum)
      :fixnum
      (multiple-value-bind (lhs-digits lhs-sign) (vm-bignum-digit-vector lhs)
        (declare (ignore lhs-sign))
        (multiple-value-bind (rhs-digits rhs-sign) (vm-bignum-digit-vector rhs)
          (declare (ignore rhs-sign))
          (if (>= (max (length lhs-digits) (length rhs-digits)) threshold)
              :karatsuba
              :schoolbook)))))

(defun vm-bignum-multiply-plan
    (lhs rhs &key (threshold +vm-bignum-karatsuba-threshold+))
  "Describe a bignum multiplication plan without depending on host bignum internals."
  (multiple-value-bind (lhs-digits lhs-sign) (vm-bignum-digit-vector lhs)
    (multiple-value-bind (rhs-digits rhs-sign) (vm-bignum-digit-vector rhs)
      (list :strategy (vm-bignum-multiplication-strategy lhs rhs :threshold threshold)
            :sign (* lhs-sign rhs-sign)
            :lhs-digits lhs-digits
            :rhs-digits rhs-digits))))

(defun vm-complex-unbox-plan (value &key local-p)
  "Describe whether VALUE can be represented as split real/imag registers."
  (if (and local-p (complexp value))
      (list :representation :split-registers
            :real (realpart value)
            :imag (imagpart value))
      (list :representation :boxed
            :value value)))

(defun vm-complex-unboxed-add-plan (lhs rhs &key local-p)
  "Return a split-register addition plan for local complex operands, or NIL."
  (let ((lhs-plan (vm-complex-unbox-plan lhs :local-p local-p))
        (rhs-plan (vm-complex-unbox-plan rhs :local-p local-p)))
    (when (and (eq (getf lhs-plan :representation) :split-registers)
               (eq (getf rhs-plan :representation) :split-registers))
      (list :representation :split-registers
            :real (+ (getf lhs-plan :real) (getf rhs-plan :real))
            :imag (+ (getf lhs-plan :imag) (getf rhs-plan :imag))))))

;;; FR-301: round (binary, returns quotient + remainder as multiple values)

(define-vm-instruction vm-round-inst (vm-instruction)
  "Round number to nearest integer (banker's rounding)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :round)
  (:sexp-slots dst lhs rhs))

(defmethod execute-instruction ((inst vm-round-inst) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (q r)
      (round (vm-reg-get state (vm-lhs inst))
             (vm-reg-get state (vm-rhs inst)))
    (vm-reg-set state (vm-dst inst) q)
    (setf (vm-values-list state) (list q r)))
  (values (1+ pc) nil nil))


;;; FR-305: Float Operations
;; define-vm-unary-instruction / define-vm-binary-instruction defined in vm.lisp.

(define-vm-unary-instruction vm-float-inst       :float           "Convert number to float.")
(define-vm-unary-instruction vm-float-precision  :float-precision "Number of significant radix digits in a float.")
(define-vm-unary-instruction vm-float-radix      :float-radix     "Radix of the float representation.")
(define-vm-unary-instruction vm-float-sign       :float-sign      "Sign of a float as float.")
(define-vm-unary-instruction vm-float-digits     :float-digits    "Number of radix digits in the float mantissa.")
(define-vm-binary-instruction vm-scale-float     :scale-float     "Scale a float by a power of the radix.")

(define-simple-instruction vm-float-inst      :unary float)
(define-simple-instruction vm-float-precision :unary float-precision)
(define-simple-instruction vm-float-radix     :unary float-radix)
(define-simple-instruction vm-float-sign      :unary float-sign)
(define-simple-instruction vm-float-digits    :unary float-digits)
(define-simple-instruction vm-scale-float     :binary scale-float)

(define-vm-unary-instruction vm-decode-float :decode-float "Decode float into significand, exponent, sign (3 multiple values).")

(defmethod execute-instruction ((inst vm-decode-float) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (sig exp sign)
      (decode-float (vm-reg-get state (vm-src inst)))
    (vm-reg-set state (vm-dst inst) sig)
    (setf (vm-values-list state) (list sig exp sign)))
  (values (1+ pc) nil nil))

(define-vm-unary-instruction vm-integer-decode-float :integer-decode-float "Decode float into integer significand, exponent, sign (3 multiple values).")

(defmethod execute-instruction ((inst vm-integer-decode-float) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (sig exp sign)
      (integer-decode-float (vm-reg-get state (vm-src inst)))
    (vm-reg-set state (vm-dst inst) sig)
    (setf (vm-values-list state) (list sig exp sign)))
  (values (1+ pc) nil nil))

;;; Environment predicates (vm-boundp, vm-fboundp, etc.) are in vm-environment.lisp (loads after).
