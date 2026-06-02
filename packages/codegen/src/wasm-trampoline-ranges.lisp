;;;; packages/codegen/src/wasm-trampoline-ranges.lisp - FR-209/FR-145 fixnum range analysis
;;;
;;; Contains the i31ref fixnum range analysis and boxing/unboxing helpers used
;;; by the WASM trampoline to avoid redundant box/unbox pairs and constant
;;; i31ref materializations (FR-209: i31ref native boxing, FR-145: integer range annotations).

(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-209: i31ref fixnum native boxing/range analysis helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defconstant +wasm-i31-min+ (- (expt 2 30))
  "Minimum signed integer representable by Wasm GC i31ref.")

(defconstant +wasm-i31-max+ (1- (expt 2 30))
  "Maximum signed integer representable by Wasm GC i31ref.")

(defun wasm-i31-range-p (value)
  "Return true when integer VALUE fits in signed Wasm i31ref payload bits."
  (and (integerp value)
       (<= +wasm-i31-min+ value +wasm-i31-max+)))

(defun reg-record-fixnum-range (reg-map reg range)
  "Record RANGE for REG and mark REG as an i31ref fixnum when FR-209 is enabled."
  (when *wasm-i31ref-optimize-enabled*
    (reg-record-type reg-map reg :i31ref)
    (let ((ranges (wasm-reg-map-fixnum-ranges reg-map)))
      (when ranges (setf (gethash reg ranges) range)))))

(defun reg-known-fixnum-range (reg-map reg)
  "Return the known fixnum range for REG, or NIL when unknown."
  (let ((ranges (wasm-reg-map-fixnum-ranges reg-map)))
    (and ranges (gethash reg ranges))))

(defun wasm-range-binop (lhs-range rhs-range op)
  "Conservatively compute a result range for integer OP over LHS/RHS ranges."
  (when (and (consp lhs-range) (consp rhs-range))
    (let ((a (car lhs-range)) (b (cdr lhs-range))
          (c (car rhs-range)) (d (cdr rhs-range)))
      (cond
        ((string= op "i64.add") (cons (+ a c) (+ b d)))
        ((string= op "i64.sub") (cons (- a d) (- b c)))
        ((string= op "i64.mul")
         (let ((values (list (* a c) (* a d) (* b c) (* b d))))
           (cons (apply #'min values) (apply #'max values))))
        ((member op '("i64.and" "i64.or" "i64.xor") :test #'string=)
         (cons +wasm-i31-min+ +wasm-i31-max+))
        (t nil)))))

(defun wasm-range-unary (src-range format-string)
  "Conservatively compute a result range for a unary fixnum format string."
  (when (consp src-range)
    (cond
      ((string= format-string "(i64.add ~A (i64.const 1))")
       (cons (1+ (car src-range)) (1+ (cdr src-range))))
      ((string= format-string "(i64.sub ~A (i64.const 1))")
       (cons (1- (car src-range)) (1- (cdr src-range))))
      ((string= format-string "(i64.sub (i64.const 0) ~A)")
       (cons (- (cdr src-range)) (- (car src-range))))
      ((or (search "popcnt" format-string)
           (search "clz" format-string)
           (search "ctz" format-string))
       (cons 0 64))
      (t nil))))

(defun wasm-range-i31-or-unknown (range)
  "Return RANGE if it fits i31ref, otherwise the conservative i31 range."
  (if (and (consp range)
           (wasm-i31-range-p (car range))
           (wasm-i31-range-p (cdr range)))
      range
      (cons +wasm-i31-min+ +wasm-i31-max+)))

(defun wasm-i64-const-wat-value (wat)
  "Return integer N for WAT of the form (i64.const N), else NIL."
  (when (stringp wat)
    (let ((prefix "(i64.const "))
      (when (and (>= (length wat) (+ (length prefix) 2))
                 (string= wat prefix :end1 (length prefix))
                 (char= (char wat (1- (length wat))) #\)))
        (ignore-errors
          (parse-integer wat :start (length prefix) :end (1- (length wat))))))))

(defun wasm-i64-extended-i31-source (wat)
  "Return X from WAT shaped as (i64.extend_i32_s (i31.get_s X)), else NIL."
  (when (stringp wat)
    (let ((prefix "(i64.extend_i32_s (i31.get_s "))
      (when (and (>= (length wat) (+ (length prefix) 2))
                 (string= wat prefix :end1 (length prefix)))
        (subseq wat (length prefix) (- (length wat) 2))))))

(defun wasm-fixnum-unbox (reg-map reg &key (result-type :i64))
  "Unbox a native i31ref fixnum.

RESULT-TYPE selects the consumer width.  :I64 preserves the historical fixnum
arithmetic path with i64.extend_i32_s; :I32 emits direct i31.get_s for Wasm
consumers that already accept i32, skipping the extend/wrap pair."
  (let* ((ref (reg-local-ref reg-map reg))
         (known (and *wasm-i31ref-optimize-enabled* (reg-known-type reg-map reg)))
         (i32-wat (format nil "(i31.get_s ~A)" ref)))
    (case result-type
      (:i32 i32-wat)
      (:i64 (if (and *wasm-i31ref-optimize-enabled* (eq known :i64-unboxed))
                ref
                (format nil "(i64.extend_i32_s ~A)" i32-wat)))
      (otherwise (error "Unsupported wasm-fixnum-unbox result type: ~S" result-type)))))

(defun wasm-fixnum-box (i64-wat)
  "Box an integer expression as an i31ref fixnum.

FR-209 optimizes constant i31 values to direct i32.const and removes dead
box/unbox pairs by returning the original i31ref expression when boxing a value
that was just unboxed from i31ref."
  (if *wasm-i31ref-optimize-enabled*
      (let ((const (wasm-i64-const-wat-value i64-wat))
            (inner-i31 (wasm-i64-extended-i31-source i64-wat)))
        (cond
          ((and const (wasm-i31-range-p const))
           (format nil "(ref.i31 (i32.const ~D))" const))
          (inner-i31 inner-i31)
          (t (format nil "(ref.i31 (i32.wrap_i64 ~A))" i64-wat))))
      (format nil "(ref.i31 (i32.wrap_i64 ~A))" i64-wat)))

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-145: Integer Range Annotation — fixnum unboxed register tracking
;;; ─────────────────────────────────────────────────────────────────────────────

(defvar *wasm-fixnum-unboxed-regs* nil
  "Dynamic binding: hash table mapping VM register keyword to integer constant value
   when the register holds a known fixnum constant that can be used as raw i64.
   Set by vm-const emit when the const value is an integer (FR-145).")

(defun wasm-fixnum-unboxed-reg-p (reg-map reg)
  "Return the integer constant REG holds, or NIL if not a known fixnum constant.
FR-145: Checks *wasm-fixnum-unboxed-regs* table for the register."
  (declare (ignore reg-map))
  (and *wasm-fixnum-unboxed-regs*
       (gethash reg *wasm-fixnum-unboxed-regs*)))

(defun wasm-mark-reg-unboxed-fixnum (reg value)
  "Mark REG as holding an unboxed i64 constant VALUE (FR-145)."
  (when *wasm-fixnum-unboxed-regs*
    (setf (gethash reg *wasm-fixnum-unboxed-regs*) value)))

(defun wasm-clear-reg-unboxed-fixnum (reg)
  "Clear the unboxed-fixnum mark for REG (FR-145)."
  (when *wasm-fixnum-unboxed-regs*
    (remhash reg *wasm-fixnum-unboxed-regs*)))
))
