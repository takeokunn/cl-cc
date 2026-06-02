(in-package :cl-cc/vm)

;;; VM Numeric Primitives Extension
;;;
;;; This file contains higher-level numeric facilities that build on the
;;; core primitive instructions defined in primitives.lisp:
;;;
;;;   FR-842  Kahan compensated summation and pairwise summation
;;;   FR-843  Floating-point exception trap control (SBCL-native)
;;;   FR-844  Double-double 128-bit extended precision
;;;   FR-860  ANSI numeric contagion table
;;;   FR-861  Inline arithmetic dispatch table and instruction
;;;   FR-829  Fixnum overflow detection and bignum promotion

;;; ─── FR-842 Kahan Summation ──────────────────────────────────────────────────
;;;
;;; Error-compensated floating-point summation using the Kahan 1965 algorithm.
;;; Provides both an accumulator API and a convenience function, plus a
;;; divide-and-conquer pairwise sum with O(log n) error growth.

(defstruct (kahan-accumulator (:constructor %make-kahan-accumulator))
  "Error-compensated summation accumulator using the Kahan 1965 algorithm.
The struct maintains a running SUM and a COMPENSATION term that tracks
the low-order bits lost during each addition."
  (sum 0.0d0 :type double-float)
  (compensation 0.0d0 :type double-float))

(defun make-kahan-accumulator (&optional (initial 0.0d0))
  "Create a fresh Kahan accumulator seeded with INITIAL (default 0.0)."
  (%make-kahan-accumulator :sum (float initial 1.0d0)))

(defun kahan-add! (acc val)
  "Add VAL to the Kahan accumulator ACC, tracking the rounding error.
Returns ACC (modified)."
  (let* ((y (- (float val 1.0d0) (kahan-accumulator-compensation acc)))
         (t-sum (+ (kahan-accumulator-sum acc) y)))
    (setf (kahan-accumulator-compensation acc) (- (- t-sum (kahan-accumulator-sum acc)) y)
          (kahan-accumulator-sum acc) t-sum))
  acc)

(defun kahan-result (acc)
  "Return the current compensated sum from accumulator ACC."
  (kahan-accumulator-sum acc))

(defun kahan-sum (sequence)
  "Return the Kahan-compensated sum of the numbers in SEQUENCE.
More accurate than (+ a b c ...) for float sequences with large magnitude
differences."
  (let ((acc (make-kahan-accumulator)))
    (map nil (lambda (val) (kahan-add! acc val)) sequence)
    (kahan-result acc)))

(defun pairwise-sum (sequence)
  "Return the pairwise (divide-and-conquer) sum of SEQUENCE.
Performs O(n) additions with a balanced binary tree reduction, keeping
error growth O(log n) instead of O(n).  Coerces list arguments to a vector
for O(1) random access."
  (let ((vec (if (typep sequence 'vector) sequence (coerce sequence 'vector))))
    (labels ((rec (start end)
               (case (- end start)
                 (0 0.0d0)
                 (1 (float (aref vec start) 1.0d0))
                 (otherwise
                  (let ((mid (floor (+ start end) 2)))
                    (+ (rec start mid) (rec mid end)))))))
      (rec 0 (length vec)))))

;;; FR-843 float exception control forms have been moved to
;;; primitives-numeric-fp-traps.lisp (loaded before this file).

;;; ─── FR-844 / FR-860 / FR-861 / FR-829 Numeric Runtime Extensions ─────────
;;;
;;; Extended precision arithmetic, ANSI numeric contagion metadata, inline
;;; arithmetic dispatch, and explicit overflow-detection helpers.

(defparameter *numeric-precision* 53
  "Current requested arithmetic precision in bits.  WITH-PRECISION binds this
value in an MPFR-compatible style; host arithmetic remains the execution engine
unless callers opt into double-double helpers explicitly.")

(defmacro with-precision (bits &body body)
  "Evaluate BODY with *NUMERIC-PRECISION* dynamically bound to BITS.

The macro mirrors MPFR-style dynamic precision blocks while keeping this VM
portable.  Double-double operations are available through DD+ and DD* when
callers need more than the host double-float mantissa."
  `(let ((*numeric-precision* ,bits))
     ,@body))

(defstruct (double-double
            (:constructor %make-double-double (hi lo))
            (:copier nil))
  "A 128-bit extended precision number represented as HI+LO double-floats.
The operations below use Dekker/Knuth error-free transforms for double-double
addition and multiplication."
  (hi 0.0d0 :type double-float)
  (lo 0.0d0 :type double-float))

(defun make-double-double (value &optional (low 0.0d0))
  "Coerce VALUE and LOW into a normalized DOUBLE-DOUBLE value."
  (%make-double-double (float value 1.0d0) (float low 1.0d0)))

(declaim (inline %dd-coerce %dd-quick-two-sum %dd-two-sum %dd-split %dd-two-prod))

(defun %dd-coerce (value)
  (if (double-double-p value)
      value
      (make-double-double value)))

(defun %dd-quick-two-sum (a b)
  "Return an unevaluated exact sum A+B as two double-floats, assuming |A|>=|B|."
  (let* ((s (+ a b))
         (e (- b (- s a))))
    (values s e)))

(defun %dd-two-sum (a b)
  "Knuth two-sum transform: return S,E where S+E is exactly A+B."
  (let* ((s (+ a b))
         (bb (- s a))
         (e (+ (- a (- s bb)) (- b bb))))
    (values s e)))

(defun %dd-split (a)
  "Dekker split of A into high and low halves for IEEE double precision."
  (let* ((c (* 134217729.0d0 a)) ; 2^27 + 1
         (hi (- c (- c a)))
         (lo (- a hi)))
    (values hi lo)))

(defun %dd-two-prod (a b)
  "Dekker product transform: return P,E where P+E is exactly A*B modulo IEEE ops."
  (let ((p (* a b)))
    (multiple-value-bind (ah al) (%dd-split a)
      (multiple-value-bind (bh bl) (%dd-split b)
        (values p (+ (- (- (- (* ah bh) p) (* al bh)) (* ah bl)) (* al bl)))))))

(defun dd+ (left right)
  "Return LEFT+RIGHT as a normalized DOUBLE-DOUBLE value."
  (let ((a (%dd-coerce left))
        (b (%dd-coerce right)))
    (multiple-value-bind (s e)
        (%dd-two-sum (double-double-hi a) (double-double-hi b))
      (let* ((e (+ e (double-double-lo a) (double-double-lo b))))
        (multiple-value-bind (hi lo) (%dd-quick-two-sum s e)
          (%make-double-double hi lo))))))

(defun dd* (left right)
  "Return LEFT*RIGHT as a normalized DOUBLE-DOUBLE value."
  (let ((a (%dd-coerce left))
        (b (%dd-coerce right)))
    (multiple-value-bind (p e)
        (%dd-two-prod (double-double-hi a) (double-double-hi b))
      (let ((e (+ e
                  (* (double-double-hi a) (double-double-lo b))
                  (* (double-double-lo a) (double-double-hi b)))))
        (multiple-value-bind (hi lo) (%dd-quick-two-sum p e)
          (%make-double-double hi lo))))))

(defun %dd-rational-value (dd)
  (+ (rational (double-double-hi dd))
     (rational (double-double-lo dd))))

(defun %decimal-rational-string (value digits)
  "Render rational VALUE with DIGITS digits after the decimal point."
  (check-type digits (integer 0 *))
  (let* ((negative-p (minusp value))
         (abs-value (abs value))
         (scale (expt 10 digits))
         (scaled (round (* abs-value scale)))
         (integer-part (floor scaled scale))
         (fractional-part (mod scaled scale)))
    (if (zerop digits)
        (format nil "~:[~;-~]~D" negative-p integer-part)
        (format nil "~:[~;-~]~D.~v,'0D" negative-p integer-part digits fractional-part))))

(defun dd-to-string (dd digits)
  "Return DD as a decimal string with DIGITS fractional digits."
  (%decimal-rational-string (%dd-rational-value (%dd-coerce dd)) digits))

;;; ─── FR-860 ANSI Numeric Contagion ───────────────────────────────────────────

(defparameter +numeric-contagion-types+
  #(integer rational single-float double-float complex)
  "ANSI CL §12.1.4 numeric contagion hierarchy used by the VM.")

(defparameter *numeric-contagion-table*
  (let* ((types +numeric-contagion-types+)
         (size (length types))
         (table (make-array (list size size))))
    (dotimes (i size table)
      (dotimes (j size)
        (setf (aref table i j) (aref types (max i j))))))
  "Type×type matrix for ANSI numeric contagion.  Indices follow
+NUMERIC-CONTAGION-TYPES+: integer < rational < single-float < double-float
< complex.")

(defun numeric-contagion-rank (type)
  "Return TYPE's numeric contagion rank, or NIL for an unsupported type."
  (position type +numeric-contagion-types+ :test #'eq))

(defun numeric-contagion-type-of (value)
  "Return VALUE's contagion type symbol."
  (cond
    ((complexp value) 'complex)
    ((typep value 'double-float) 'double-float)
    ((typep value 'single-float) 'single-float)
    ((integerp value) 'integer)
    ((rationalp value) 'rational)
    ((floatp value) 'double-float)
    (t (error "Not a VM numeric value: ~S" value))))

(defun numeric-contagion-result-type (left-type right-type)
  "Infer the ANSI numeric result type for LEFT-TYPE and RIGHT-TYPE."
  (let ((left-rank (numeric-contagion-rank left-type))
        (right-rank (numeric-contagion-rank right-type)))
    (unless (and left-rank right-rank)
      (error "Unsupported numeric contagion types: ~S × ~S" left-type right-type))
    (aref *numeric-contagion-table* left-rank right-rank)))

(defun infer-numeric-result-type (left right)
  "Compile-time friendly numeric result inference.
LEFT and RIGHT may be either type symbols or literal numeric values."
  (numeric-contagion-result-type
   (if (symbolp left) left (numeric-contagion-type-of left))
   (if (symbolp right) right (numeric-contagion-type-of right))))

;;; ─── FR-861 Inline Arithmetic Dispatch ───────────────────────────────────────

(defconstant +arith-op-count+ 4)
(defconstant +arith-tag-count+ 5)
(defconstant +arith-dispatch-table-size+ (* +arith-op-count+ +arith-tag-count+ +arith-tag-count+))

(defparameter +arith-operation-tags+ #(+ - * /))
(defparameter +arith-type-tags+ #(fixnum integer rational float complex))

(defmacro define-arith-dispatch-fns (&rest prefix-op-groups)
  "Generate inline arithmetic dispatch functions for each (PREFIX OPS...) group.
Each PREFIX-OP-GROUP is (prefix op1 op2 ...).  For each combination a function
named %PREFIX-OP is emitted that calls CL's OP directly on its two arguments.
All generated functions are declared inline.

Example: (define-arith-dispatch-fns (ff + - * /) (fi + - * /) (dd + - * /))"
  (let ((names (loop for (prefix . ops) in prefix-op-groups
                     append (loop for op in ops
                                  collect (intern (format nil "%~A~A" prefix op))))))
    `(progn
       (declaim (inline ,@names))
       ,@(loop for (prefix . ops) in prefix-op-groups
               append (loop for op in ops
                            for name = (intern (format nil "%~A~A" prefix op))
                            collect `(defun ,name (a b) (,op a b)))))))

(define-arith-dispatch-fns
  (ff + - * /)
  (fi + - * /)
  (dd + - * /))

(defun arithmetic-op-tag (op)
  "Return the inline dispatch operation tag for OP."
  (or (position op +arith-operation-tags+ :test #'eq)
      (error "Unsupported arithmetic dispatch operation: ~S" op)))

(defun arithmetic-type-tag (value)
  "Extract VALUE's arithmetic dispatch type tag."
  (cond
    ((typep value 'fixnum) 0)
    ((integerp value) 1)
    ((rationalp value) 2)
    ((floatp value) 3)
    ((complexp value) 4)
    (t (error "Unsupported arithmetic dispatch operand: ~S" value))))

(defun arithmetic-dispatch-index (op-tag left-tag right-tag)
  "Flatten OP-TAG, LEFT-TAG, RIGHT-TAG into *ARITH-DISPATCH-TABLE* index."
  (+ (* op-tag +arith-tag-count+ +arith-tag-count+)
     (* left-tag +arith-tag-count+)
     right-tag))

(defun %set-arith-dispatch (table op left-tag right-tag function)
  (setf (aref table (arithmetic-dispatch-index (arithmetic-op-tag op) left-tag right-tag))
        (cons (list (aref +arith-type-tags+ left-tag)
                    (aref +arith-type-tags+ right-tag))
              function)))

(defparameter *arith-dispatch-table*
  (let ((table (make-array +arith-dispatch-table-size+ :initial-element nil)))
    (dolist (spec '((+ %ff+ %fi+ %dd+)
                    (- %ff- %fi- %dd-)
                    (* %ff* %fi* %dd*)
                    (/ %ff/ %fi/ %dd/)))
      (destructuring-bind (op ff fi dd) spec
        (%set-arith-dispatch table op 0 0 (symbol-function ff))
        (%set-arith-dispatch table op 0 3 (symbol-function fi))
        (%set-arith-dispatch table op 3 0 (symbol-function fi))
        (%set-arith-dispatch table op 3 3 (symbol-function dd))))
    table)
  "Flattened inline arithmetic dispatch table.  Each populated entry is
((LEFT-TYPE RIGHT-TYPE) . FUNCTION), so tag extraction plus index computation
selects a direct function jump for +, -, *, and /.")

(defun arithmetic-dispatch-entry (op left right)
  "Return the inline dispatch table entry selected by OP, LEFT, and RIGHT."
  (aref *arith-dispatch-table*
        (arithmetic-dispatch-index (arithmetic-op-tag op)
                                   (arithmetic-type-tag left)
                                   (arithmetic-type-tag right))))

(defun inline-arithmetic-dispatch (op left right)
  "Execute OP on LEFT and RIGHT through the inline arithmetic dispatch table.
Missing specialized entries intentionally fall back to host generic arithmetic."
  (let ((entry (arithmetic-dispatch-entry op left right)))
    (if entry
        (funcall (cdr entry) left right)
        (funcall (ecase op
                   (+ #'+) (- #'-) (* #'*) (/ #'/))
                 left right))))

(define-vm-instruction vm-arith-dispatch (vm-binop)
  "Inline arithmetic dispatch instruction.  OP is one of +, -, *, /."
  (op '+ :reader vm-arith-dispatch-op)
  (:sexp-tag :arith-dispatch)
  (:sexp-slots dst lhs rhs op))

(defmethod execute-instruction ((inst vm-arith-dispatch) state pc labels)
  (declare (ignore labels))
  (let* ((lhs (vm-reg-get state (vm-lhs inst)))
         (rhs (vm-reg-get state (vm-rhs inst)))
         (result (inline-arithmetic-dispatch (vm-arith-dispatch-op inst) lhs rhs)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; ─── FR-829 Fixnum Overflow Detection ────────────────────────────────────────

(defparameter *vm-arithmetic-safety* 1
  "Runtime arithmetic safety level.  A value of 0 skips explicit overflow checks.")

(defun vm-fixnum-overflow-detected-p (value)
  "Return T when VALUE overflows the VM fixnum representation."
  (and (integerp value)
       (fboundp 'vm-fixnum51-overflow-p)
       (vm-fixnum51-overflow-p value)))

(defun vm-fixnum-add-with-overflow-detection (left right &key (safety *vm-arithmetic-safety*))
  "Add fixnums with x86-64 JO-style overflow detection and bignum promotion.
When SAFETY is 0 the explicit overflow branch is skipped."
  (let ((result (+ left right)))
    (if (and (plusp safety) (vm-fixnum-overflow-detected-p result))
        (vm-bignum-add-integers left right)
        result)))

(defun vm-fixnum-sub-with-overflow-detection (left right &key (safety *vm-arithmetic-safety*))
  "Subtract fixnums with explicit overflow detection unless SAFETY is 0."
  (let ((result (- left right)))
    (if (and (plusp safety) (vm-fixnum-overflow-detected-p result))
        (vm-bignum-subtract-integers left right)
        result)))

(defun vm-fixnum-mul-with-overflow-detection (left right &key (safety *vm-arithmetic-safety*))
  "Multiply fixnums with explicit overflow detection unless SAFETY is 0."
  (let ((result (* left right)))
    (if (and (plusp safety) (vm-fixnum-overflow-detected-p result))
        (vm-bignum-multiply-integers left right)
        result)))

(export '(with-precision
          *numeric-precision*
          double-double
          double-double-p
          double-double-hi
          double-double-lo
          make-double-double
          dd+
          dd*
          dd-to-string
          +numeric-contagion-types+
          *numeric-contagion-table*
          numeric-contagion-rank
          numeric-contagion-type-of
          numeric-contagion-result-type
          infer-numeric-result-type
          +arith-operation-tags+
          +arith-type-tags+
          *arith-dispatch-table*
          arithmetic-op-tag
          arithmetic-type-tag
          arithmetic-dispatch-index
          arithmetic-dispatch-entry
          inline-arithmetic-dispatch
          vm-arith-dispatch
          make-vm-arith-dispatch
          vm-arith-dispatch-op
          *vm-arithmetic-safety*
          vm-fixnum-overflow-detected-p
          vm-fixnum-add-with-overflow-detection
          vm-fixnum-sub-with-overflow-detection
          vm-fixnum-mul-with-overflow-detection))
