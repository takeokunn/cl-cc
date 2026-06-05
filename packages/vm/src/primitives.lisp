(in-package :cl-cc/vm)

;;; VM Primitive Instructions
;;;
;;; This file extends the VM with primitive type predicates, comparisons,
;;; arithmetic extensions, and boolean operations.
;;;

;;; Type Predicates
;; define-vm-unary-instruction / define-vm-binary-instruction defined in vm.lisp.

(define-vm-binary-instruction vm-eq        :eq        "EQL comparison. Returns 1 if LHS equals RHS, 0 otherwise.")
(define-vm-unary-instruction  vm-cons-p    :cons-p    "Type predicate for cons cells. Returns 1 if SRC is a cons cell, 0 otherwise.")
(define-vm-unary-instruction  vm-null-p    :null-p    "Type predicate for nil. Returns 1 if SRC is nil, 0 otherwise.")
(define-vm-unary-instruction  vm-symbol-p  :symbol-p  "Type predicate for symbols. Returns 1 if SRC is a symbol, 0 otherwise.")

;;; Fast path for immediate symbols: no hash-table lookup needed.
(defmethod execute-instruction :around ((inst vm-symbol-p) state pc labels)
  (declare (ignore labels))
  (let ((value (vm-reg-get state (vm-src inst))))
    (if (vm-immediate-symbol-p value)
        (progn (vm-reg-set state (vm-dst inst) 1) (values (1+ pc) nil nil))
        (call-next-method))))

(define-vm-unary-instruction  vm-number-p  :number-p  "Type predicate for numbers. Returns 1 if SRC is a number, 0 otherwise.")
(define-vm-unary-instruction  vm-integer-p :integer-p "Type predicate for integers. Returns 1 if SRC is an integer, 0 otherwise.")
(define-vm-unary-instruction  vm-function-p :function-p "Type predicate for functions/closures. Returns 1 if SRC is a function, 0 otherwise.")

;;; Comparison Operations

(define-vm-binary-instruction vm-lt     :lt     "Less than comparison. Returns 1 if LHS < RHS, 0 otherwise.")
(define-vm-binary-instruction vm-gt     :gt     "Greater than comparison. Returns 1 if LHS > RHS, 0 otherwise.")
(define-vm-binary-instruction vm-le     :le     "Less than or equal comparison. Returns 1 if LHS <= RHS, 0 otherwise.")
(define-vm-binary-instruction vm-ge     :ge     "Greater than or equal comparison. Returns 1 if LHS >= RHS, 0 otherwise.")
(define-vm-binary-instruction vm-num-eq :num-eq "Numeric equality comparison. Returns 1 if LHS = RHS, 0 otherwise.")

;;; Arithmetic Extensions

(define-vm-binary-instruction vm-div          :div      "Integer division. DST = floor(LHS / RHS).")
(define-vm-binary-instruction vm-cl-div       :cl-div   "CL division. DST = LHS / RHS (rational-preserving).")
(define-vm-binary-instruction vm-mod          :mod      "Modulo operation. DST = LHS mod RHS.")
(define-vm-unary-instruction  vm-neg          :neg      "Negation. DST = -SRC.")
(define-vm-unary-instruction  vm-abs          :abs      "Absolute value. DST = |SRC|.")
(define-vm-unary-instruction  vm-inc          :inc      "Increment. DST = SRC + 1.")
(define-vm-unary-instruction  vm-dec          :dec      "Decrement. DST = SRC - 1.")
(define-vm-binary-instruction vm-min          :min      "Minimum. DST = min(LHS, RHS).")
(define-vm-binary-instruction vm-max          :max      "Maximum. DST = max(LHS, RHS).")
(define-vm-binary-instruction vm-truncate     :truncate "Truncate division. DST = truncate(LHS / RHS).")
(define-vm-binary-instruction vm-floor-inst   :floor    "Floor division. DST = floor(LHS / RHS).")
(define-vm-binary-instruction vm-ceiling-inst :ceiling  "Ceiling division. DST = ceiling(LHS / RHS).")
(define-vm-binary-instruction vm-round-inst   :round    "Round division (banker's rounding). DST = round(LHS / RHS).")
(define-vm-binary-instruction vm-rem          :rem      "Remainder. DST = rem(LHS, RHS).")
(define-vm-unary-instruction  vm-evenp        :evenp    "Even predicate. Returns 1 if SRC is even, 0 otherwise.")
(define-vm-unary-instruction  vm-oddp         :oddp     "Odd predicate. Returns 1 if SRC is odd, 0 otherwise.")

;;; Boolean Operations

(define-vm-unary-instruction  vm-not :not "Logical not. Returns 1 if SRC is 0, 0 otherwise.")
(define-vm-binary-instruction vm-and :and "Logical and. Returns 1 if both LHS and RHS are non-zero, 0 otherwise.")
(define-vm-binary-instruction vm-or  :or  "Logical or. Returns 1 if either LHS or RHS is non-zero, 0 otherwise.")

;;; General Type Predicate

(define-vm-instruction vm-typep (vm-instruction)
  "General type check. Returns 1 if SRC is of TYPE-NAME, 0 otherwise.
TYPE-NAME is a symbol like INTEGER, STRING, SYMBOL, CONS, NULL, LIST, etc."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (type-name nil :reader vm-type-name)
  (:sexp-tag :typep)
  (:sexp-slots dst src type-name))

;;; Instruction Execution - Type Predicates
;;; (vm-typep data tables + check function → see primitives-typep.lisp)

(declaim (inline %vm-decode-immediate-symbol-for-eql))

(defun %vm-decode-immediate-symbol-for-eql (value)
  (if (vm-immediate-symbol-p value)
      (vm-decode-symbol value)
      value))

(defmethod execute-instruction ((inst vm-eq) state pc labels)
  (declare (ignore labels))
  (let* ((lhs (%vm-decode-immediate-symbol-for-eql
               (vm-reg-get state (vm-lhs inst))))
         (rhs (%vm-decode-immediate-symbol-for-eql
               (vm-reg-get state (vm-rhs inst))))
         (result (if (eql lhs rhs) 1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))
(define-simple-instruction vm-cons-p    :pred1 consp)
(define-simple-instruction vm-null-p    :pred1 null)
(define-simple-instruction vm-symbol-p  :pred1 symbolp)
(define-simple-instruction vm-number-p  :pred1 numberp)
(define-simple-instruction vm-integer-p :pred1 integerp)

(defmethod execute-instruction ((inst vm-function-p) state pc labels)
  (declare (ignore labels))
  (let* ((value (vm-reg-get state (vm-src inst)))
         (result (if (typep value 'vm-closure-object) 1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; Instruction Execution - Comparison Operations

(define-simple-instruction vm-lt     :pred2 <)
(define-simple-instruction vm-gt     :pred2 >)
(define-simple-instruction vm-le     :pred2 <=)
(define-simple-instruction vm-ge     :pred2 >=)
(define-simple-instruction vm-num-eq :pred2 =)

;;; Instruction Execution - Arithmetic Extensions

(defun %vm-cl-div-fast-path (lhs rhs)
  "Return LHS/RHS and the selected rational arithmetic path.
The path value is diagnostic and keeps the runtime specialization testable."
  (cond
    ((and (typep lhs 'fixnum) (typep rhs 'fixnum))
      ;; Standard CL / already produces the correct ratio for fixnum division.
      (values (/ lhs rhs) :fixnum))
    ((and (%vm-fixnum-rational-p lhs) (%vm-fixnum-rational-p rhs))
      (values (/ (* (numerator lhs) (denominator rhs))
                 (* (denominator lhs) (numerator rhs)))
             :fixnum-rational))
    ((and (typep lhs 'fixnum) (%vm-fixnum-rational-p rhs))
     (values (/ (* lhs (denominator rhs))
                (numerator rhs))
             :fixnum-rational))
    ((and (%vm-fixnum-rational-p lhs) (typep rhs 'fixnum))
     (values (/ (numerator lhs)
                (* (denominator lhs) rhs))
             :fixnum-rational))
    (t
     (values (/ lhs rhs) :generic))))

(defmethod execute-instruction ((inst vm-div) state pc labels)
  (declare (ignore labels))
  (let* ((lhs (vm-reg-get state (vm-lhs inst)))
         (divisor (vm-reg-get state (vm-rhs inst))))
    (if (zerop divisor)
        (error "vm-div: Division by zero")
        (let ((result (if (or (typep lhs 'bignum) (typep divisor 'bignum))
                          (nth-value 0 (vm-bignum-burnikel-ziegler-divide lhs divisor :rounding :floor))
                          (floor lhs divisor))))
          (vm-reg-set state (vm-dst inst) result)
          (values (1+ pc) nil nil)))))

(defmethod execute-instruction ((inst vm-mul) state pc labels)
  (declare (ignore labels))
  (let* ((lhs (vm-reg-get state (vm-lhs inst)))
         (rhs (vm-reg-get state (vm-rhs inst)))
         (result (%vm-mul-with-overflow-fallback lhs rhs)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-cl-div) state pc labels)
  (declare (ignore labels))
  (let ((divisor (vm-reg-get state (vm-rhs inst))))
    (if (zerop divisor)
        (error "Division by zero")
        (let ((result (%vm-cl-div-fast-path (vm-reg-get state (vm-lhs inst)) divisor)))
          (vm-reg-set state (vm-dst inst) result)
          (values (1+ pc) nil nil)))))

(define-vm-instruction vm-float-div (vm-cl-div)
  (:sexp-tag :fdiv)
  (:sexp-slots dst lhs rhs))

(defmethod execute-instruction ((inst vm-mod) state pc labels)
  (declare (ignore labels))
  (let ((divisor (vm-reg-get state (vm-rhs inst))))
    (if (zerop divisor)
        (error "vm-mod: Division by zero")
        (let ((result (mod (vm-reg-get state (vm-lhs inst)) divisor)))
          (vm-reg-set state (vm-dst inst) result)
          (values (1+ pc) nil nil)))))

(define-simple-instruction vm-neg :unary  -)
(define-simple-instruction vm-abs :unary  abs)
(define-simple-instruction vm-inc :unary  1+)
(define-simple-instruction vm-dec :unary  1-)
(define-simple-instruction vm-min :binary min)
(define-simple-instruction vm-max :binary max)

;;; Division family: data-driven via macro (truncate, floor, ceiling share identical structure)

(defmacro define-vm-division-executors (&rest specs)
  "Generate execute-instruction methods for integer division variants.
Each SPEC is (instruction-type host-function)."
  `(progn
     ,@(loop for (inst-type host-fn) in specs
             collect `(defmethod execute-instruction ((inst ,inst-type) state pc labels)
                         (declare (ignore labels))
                         (let* ((lhs (vm-reg-get state (vm-lhs inst)))
                                (rhs (vm-reg-get state (vm-rhs inst))))
                           (when (zerop rhs)
                             (error "Division by zero"))
                           (multiple-value-bind (q r)
                               (cond
                                 ((and (typep lhs 'fixnum) (typep rhs 'fixnum))
                                  (,host-fn lhs rhs))
                                 ((or (typep lhs 'bignum) (typep rhs 'bignum))
                                  (vm-bignum-burnikel-ziegler-divide
                                   lhs rhs
                                   :rounding ,(case host-fn
                                                (truncate :truncate)
                                                (floor :floor)
                                                (ceiling :ceiling)
                                                (round :round)
                                                (otherwise :truncate))))
                                 (t
                                  (,host-fn lhs rhs)))
                             (vm-reg-set state (vm-dst inst) q)
                             (setf (vm-values-list state) (list q r))))
                         (values (1+ pc) nil nil)))))


(define-vm-division-executors
  (vm-truncate     truncate)
  (vm-floor-inst   floor)
  (vm-ceiling-inst ceiling)
  (vm-round-inst   round))

(define-simple-instruction vm-rem   :binary rem)
(define-simple-instruction vm-evenp :pred1  evenp)
(define-simple-instruction vm-oddp  :pred1  oddp)

;;; FR-885: Float rounding (ffloor/fceiling/ftruncate/fround)
;;; Return float quotient + integer remainder. Map to host CL float rounding.

(define-vm-binary-instruction vm-ffloor :ffloor "Float floor: returns float quotient and float remainder.")
(define-vm-binary-instruction vm-fceiling :fceiling "Float ceiling: returns float quotient and float remainder.")
(define-vm-binary-instruction vm-ftruncate :ftruncate "Float truncate: returns float quotient and float remainder.")
(define-vm-binary-instruction vm-fround :fround "Float round: returns float quotient and float remainder.")

(defmacro define-vm-float-rounding-executors (&rest specs)
  "Generate execute-instruction methods for float rounding variants.
Each SPEC is (instruction-type host-function)."
  `(progn
     ,@(loop for (inst-type host-fn) in specs
             collect `(defmethod execute-instruction ((inst ,inst-type) state pc labels)
                        (declare (ignore labels))
                        (let* ((lhs (vm-reg-get state (vm-lhs inst)))
                               (rhs (vm-reg-get state (vm-rhs inst))))
                          (when (zerop rhs)
                            (error "Division by zero"))
                          (multiple-value-bind (q r)
                              (,host-fn lhs rhs)
                            (vm-reg-set state (vm-dst inst) q)
                            (setf (vm-values-list state) (list q r))))
                        (values (1+ pc) nil nil)))))

(define-vm-float-rounding-executors
  (vm-ffloor ffloor)
  (vm-fceiling fceiling)
  (vm-ftruncate ftruncate)
  (vm-fround fround))

;;; Instruction Execution - Boolean Operations

(defmethod execute-instruction ((inst vm-not) state pc labels)
  (declare (ignore labels))
  (let ((result (if (vm-falsep (vm-reg-get state (vm-src inst))) t nil)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-and) state pc labels)
  (declare (ignore labels))
  (let ((result (if (and (not (vm-falsep (vm-reg-get state (vm-lhs inst))))
                         (not (vm-falsep (vm-reg-get state (vm-rhs inst)))))
                    t nil)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-or) state pc labels)
  (declare (ignore labels))
  (let ((result (if (or (not (vm-falsep (vm-reg-get state (vm-lhs inst))))
                        (not (vm-falsep (vm-reg-get state (vm-rhs inst)))))
                    t nil)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; Type-Of Instruction

(defvar *vm-type-of-dispatch*)

;;; FR-2501 / FR-2502: Dynamic / TypeRep runtime values

(defstruct (vm-type-rep (:constructor %make-vm-type-rep))
  "Runtime representation of a VM-visible type designator."
  (specifier 't))

(defun make-vm-type-rep (specifier)
  "Construct a runtime type representation for SPECIFIER."
  (%make-vm-type-rep :specifier specifier))

(defun vm-type-rep (specifier)
  "Coerce SPECIFIER into a vm-type-rep object."
  (if (vm-type-rep-p specifier)
      specifier
      (make-vm-type-rep specifier)))

(defun vm-type-rep-equal (left right)
  "Return T when LEFT and RIGHT represent the same runtime type."
  (equal (vm-type-rep-specifier (vm-type-rep left))
         (vm-type-rep-specifier (vm-type-rep right))))

(defun %vm-type-rep-value-compatible-p (value expected-type)
  "Return T when VALUE satisfies EXPECTED-TYPE's runtime type specifier."
  (vm-typep-check value (vm-type-rep-specifier (vm-type-rep expected-type))))

(defun %vm-type-rep-compatible-p (stored-type value expected-type)
  "Return T when STORED-TYPE exactly matches EXPECTED-TYPE or VALUE satisfies it."
  (or (vm-type-rep-equal stored-type expected-type)
      (%vm-type-rep-value-compatible-p value expected-type)))

(defstruct (vm-dynamic (:constructor %make-vm-dynamic))
  "A runtime value paired with its TypeRep."
  (type-rep (make-vm-type-rep 't) :type vm-type-rep)
  (value nil))

(defun make-vm-dynamic (type-rep value)
  "Construct a typed runtime Dynamic wrapper."
  (%make-vm-dynamic :type-rep (vm-type-rep type-rep) :value value))

(defun vm-runtime-type-of (value)
  "Return the VM-visible runtime type designator for VALUE."
  (or (cdr (assoc-if (lambda (pred) (funcall pred value))
                     *vm-type-of-dispatch*))
      (and (hash-table-p value)
           (gethash :__class__ value)
           (let ((class-ht (gethash :__class__ value)))
             (and (hash-table-p class-ht)
                  (or (gethash :__name__ class-ht) 't))))
      't))

(defun vm-type-rep-of (value)
  "Return the runtime TypeRep of VALUE."
  (make-vm-type-rep (vm-runtime-type-of value)))

(defun vm-wrap-dynamic (value &optional type-specifier)
  "Wrap VALUE in a Dynamic with TYPE-SPECIFIER or the inferred runtime type."
  (make-vm-dynamic (or type-specifier (vm-type-rep-of value)) value))

(defun vm-unwrap-dynamic (dynamic expected-type)
  "Return (values value t) when DYNAMIC matches EXPECTED-TYPE, else (values nil nil)."
  (let ((dynamic-value (if (vm-dynamic-p dynamic)
                            dynamic
                            (vm-wrap-dynamic dynamic))))
    (let ((payload (vm-dynamic-value dynamic-value)))
      (if (%vm-type-rep-compatible-p (vm-dynamic-type-rep dynamic-value)
                                     payload
                                     expected-type)
          (values payload t)
          (values nil nil)))))

(defun vm-cast-with-type-rep (value expected-type)
  "Attempt to cast VALUE to EXPECTED-TYPE using runtime TypeRep compatibility."
  (if (vm-dynamic-p value)
      (vm-unwrap-dynamic value expected-type)
      (let ((expected-rep (vm-type-rep expected-type)))
        (if (%vm-type-rep-compatible-p (vm-type-rep-of value)
                                       value
                                       expected-rep)
            (values value t)
            (values nil nil)))))

(define-vm-unary-instruction vm-type-of :type-of "Get the type of an object.")

(defmethod execute-instruction ((inst vm-type-of) state pc labels)
  (declare (ignore labels))
  (let* ((value  (vm-reg-get state (vm-src inst)))
         (result (vm-runtime-type-of value)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; Runtime Eval — Meta-circular evaluation

(define-vm-unary-instruction vm-eval :eval
  "Evaluate a form at runtime by compiling and running it in a fresh VM.
This enables meta-circular self-hosting: compiled code can call eval.")

(defmethod execute-instruction ((inst vm-eval) state pc labels)
  (declare (ignore labels))
  (let* ((form (vm-reg-get state (vm-src inst)))
         (result (funcall *vm-eval-hook* form)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; FR-631: Macro expansion at runtime

(define-vm-unary-instruction vm-macroexpand-1-inst :macroexpand-1
  "Expand a macro form once. Returns the (possibly unexpanded) form.")

(defmethod execute-instruction ((inst vm-macroexpand-1-inst) state pc labels)
  (declare (ignore labels))
  (let* ((form (vm-reg-get state (vm-src inst)))
         (result (funcall *vm-macroexpand-1-hook* form)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-vm-unary-instruction vm-macroexpand-inst :macroexpand
  "Fully expand a macro form. Returns the expanded form.")

(defmethod execute-instruction ((inst vm-macroexpand-inst) state pc labels)
  (declare (ignore labels))
  (let* ((form (vm-reg-get state (vm-src inst)))
         (result (funcall *vm-macroexpand-hook* form)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; FR-498: Hash code computation

(define-vm-unary-instruction vm-sxhash :sxhash "Compute hash code for an object.")
(define-simple-instruction vm-sxhash :unary sxhash)

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

;;; vm-numeric.lisp — Numeric tower (round, bit ops, transcendentals, float, rational, complex)
;;; vm-extensions.lisp — Char comparisons, symbol plist, PROGV, generic arith

;;; ─── FR-844 / FR-860 / FR-861 / FR-829 Numeric Runtime Extensions ─────────
;;;
;;; This section is intentionally append-only.  Several feature branches touch
;;; earlier primitive arithmetic methods; the new API below exposes extended
;;; precision arithmetic, ANSI numeric contagion metadata, inline arithmetic
;;; dispatch, and explicit overflow-detection helpers without editing those
;;; methods in place.

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

(defconstant +arith-op-count+ 4)
(defconstant +arith-tag-count+ 5)
(defconstant +arith-dispatch-table-size+ (* +arith-op-count+ +arith-tag-count+ +arith-tag-count+))

(defparameter +arith-operation-tags+ #(+ - * /))
(defparameter +arith-type-tags+ #(fixnum integer rational float complex))


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
    (dolist (spec `((+ ,#'+ ,#'+ ,#'+)
                    (- ,#'- ,#'- ,#'-)
                    (* ,#'* ,#'* ,#'*)
                    (/ ,#'/ ,#'/ ,#'/)))
      (destructuring-bind (op ff fi dd) spec
        (%set-arith-dispatch table op 0 0 ff)
        (%set-arith-dispatch table op 0 3 fi)
        (%set-arith-dispatch table op 3 0 fi)
        (%set-arith-dispatch table op 3 3 dd)))
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
