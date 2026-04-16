(in-package :cl-cc/vm)

;;; VM Bit Array, adjust-array, and displacement Instructions (FR-605, FR-606, FR-648)
;;;
;;; Depends on array.lisp (vm-arr/vm-idx/vm-val accessors, base array instructions).
;;; Load order: immediately after array.lisp.

;;; ─── FR-606: Bit array operations ────────────────────────────────────────────

(define-vm-instruction vm-bit-access (vm-instruction)
  "Access element of bit array: (bit array index)."
  (dst nil :reader vm-dst) (arr nil :reader vm-arr) (idx nil :reader vm-idx)
  (:sexp-tag :bit-access) (:sexp-slots dst arr idx))
(defmethod execute-instruction ((inst vm-bit-access) state pc labels)
  (declare (ignore labels))
  (let ((arr (vm-reg-get state (vm-arr inst)))
        (idx (vm-reg-get state (vm-idx inst))))
    (vm-reg-set state (vm-dst inst)
                (if (bit-vector-p arr) (bit arr idx) (aref arr idx))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-bit-set (vm-instruction)
  "Set element of bit array: (setf (bit array index) val)."
  (dst nil :reader vm-dst) (arr nil :reader vm-arr) (idx nil :reader vm-idx)
  (val nil :reader vm-val)
  (:sexp-tag :bit-set) (:sexp-slots dst arr idx val))
(defmethod execute-instruction ((inst vm-bit-set) state pc labels)
  (declare (ignore labels))
  (let ((arr (vm-reg-get state (vm-arr inst)))
        (idx (vm-reg-get state (vm-idx inst)))
        (v   (vm-reg-get state (vm-val inst))))
    (if (bit-vector-p arr)
        (setf (bit arr idx) v)
        (setf (aref arr idx) v))
    (vm-reg-set state (vm-dst inst) v)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-bit-and (vm-instruction)
  "Element-wise AND of two bit arrays."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :bit-and) (:sexp-slots dst lhs rhs))
(define-simple-instruction vm-bit-and :binary bit-and)

(define-vm-instruction vm-bit-or (vm-instruction)
  "Element-wise OR of two bit arrays."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :bit-or) (:sexp-slots dst lhs rhs))
;; ANSI CL uses bit-ior (not bit-or); cl-cc exposes it as bit-or for symmetry
(define-simple-instruction vm-bit-or :binary bit-ior)

(define-vm-instruction vm-bit-xor (vm-instruction)
  "Element-wise XOR of two bit arrays."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :bit-xor) (:sexp-slots dst lhs rhs))
(define-simple-instruction vm-bit-xor :binary bit-xor)

(define-vm-instruction vm-bit-not (vm-instruction)
  "Element-wise NOT of a bit array."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :bit-not) (:sexp-slots dst src))
(define-simple-instruction vm-bit-not :unary bit-not)

(define-vm-instruction vm-sbit (vm-instruction)
  "Access element of simple bit-vector: (sbit svec index)."
  (dst nil :reader vm-dst) (arr nil :reader vm-arr) (idx nil :reader vm-idx)
  (:sexp-tag :sbit) (:sexp-slots dst arr idx))
(defmethod execute-instruction ((inst vm-sbit) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (sbit (vm-reg-get state (vm-arr inst))
                    (vm-reg-get state (vm-idx inst))))
  (values (1+ pc) nil nil))

;;; ─── FR-605: adjust-array and array-displacement ─────────────────────────────

(define-vm-instruction vm-adjust-array (vm-instruction)
  "Adjust dimensions of an adjustable array."
  (dst nil :reader vm-dst) (arr nil :reader vm-arr) (dims nil :reader vm-dims)
  (:sexp-tag :adjust-array) (:sexp-slots dst arr dims))
(defmethod execute-instruction ((inst vm-adjust-array) state pc labels)
  (declare (ignore labels))
  (let* ((arr (vm-reg-get state (vm-arr inst)))
         (dims (vm-reg-get state (vm-dims inst)))
         (new-dims (if (listp dims) dims (list dims))))
    (vm-reg-set state (vm-dst inst) (adjust-array arr new-dims))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-array-displacement (vm-instruction)
  "Return displaced-to array and offset (both nil for non-displaced arrays)."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :array-displacement) (:sexp-slots dst src))
(defmethod execute-instruction ((inst vm-array-displacement) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (displaced-to offset)
      (array-displacement (vm-reg-get state (vm-src inst)))
    (vm-reg-set state (vm-dst inst) displaced-to)
    (setf (vm-values-list state) (list displaced-to offset))
    (values (1+ pc) nil nil)))

;;; ─── simple-vector-p (FR-648) ────────────────────────────────────────────────
(define-vm-unary-instruction vm-simple-vector-p :simple-vector-p "Test if value is a simple-vector. Returns 1/0.")
(define-simple-instruction vm-simple-vector-p :pred1 simple-vector-p)
