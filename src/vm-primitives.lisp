(in-package :cl-cc)

;;; VM Primitive Instructions
;;;
;;; This file extends the VM with primitive type predicates, comparisons,
;;; arithmetic extensions, and boolean operations.
;;;

;;; Type Predicates

(define-vm-instruction vm-eq (vm-instruction)
  "EQL comparison. Returns 1 if LHS equals RHS, 0 otherwise."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :eq)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-cons-p (vm-instruction)
  "Type predicate for cons cells. Returns 1 if SRC is a cons cell, 0 otherwise."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :cons-p)
  (:sexp-slots dst src))

(define-vm-instruction vm-null-p (vm-instruction)
  "Type predicate for nil. Returns 1 if SRC is nil, 0 otherwise."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :null-p)
  (:sexp-slots dst src))

(define-vm-instruction vm-symbol-p (vm-instruction)
  "Type predicate for symbols. Returns 1 if SRC is a symbol, 0 otherwise."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :symbol-p)
  (:sexp-slots dst src))

(define-vm-instruction vm-number-p (vm-instruction)
  "Type predicate for numbers. Returns 1 if SRC is a number, 0 otherwise."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :number-p)
  (:sexp-slots dst src))

(define-vm-instruction vm-integer-p (vm-instruction)
  "Type predicate for integers. Returns 1 if SRC is an integer, 0 otherwise."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :integer-p)
  (:sexp-slots dst src))

(define-vm-instruction vm-function-p (vm-instruction)
  "Type predicate for functions/closures. Returns 1 if SRC is a function, 0 otherwise."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :function-p)
  (:sexp-slots dst src))

;;; Comparison Operations

(define-vm-instruction vm-lt (vm-instruction)
  "Less than comparison. Returns 1 if LHS < RHS, 0 otherwise."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :lt)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-gt (vm-instruction)
  "Greater than comparison. Returns 1 if LHS > RHS, 0 otherwise."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :gt)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-le (vm-instruction)
  "Less than or equal comparison. Returns 1 if LHS <= RHS, 0 otherwise."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :le)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-ge (vm-instruction)
  "Greater than or equal comparison. Returns 1 if LHS >= RHS, 0 otherwise."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :ge)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-num-eq (vm-instruction)
  "Numeric equality comparison. Returns 1 if LHS = RHS, 0 otherwise."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :num-eq)
  (:sexp-slots dst lhs rhs))

;;; Arithmetic Extensions

(define-vm-instruction vm-div (vm-instruction)
  "Integer division. DST = floor(LHS / RHS)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :div)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-mod (vm-instruction)
  "Modulo operation. DST = LHS mod RHS."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :mod)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-neg (vm-instruction)
  "Negation. DST = -SRC."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :neg)
  (:sexp-slots dst src))

(define-vm-instruction vm-abs (vm-instruction)
  "Absolute value. DST = |SRC|."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :abs)
  (:sexp-slots dst src))

(define-vm-instruction vm-inc (vm-instruction)
  "Increment. DST = SRC + 1."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :inc)
  (:sexp-slots dst src))

(define-vm-instruction vm-dec (vm-instruction)
  "Decrement. DST = SRC - 1."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :dec)
  (:sexp-slots dst src))

(define-vm-instruction vm-min (vm-instruction)
  "Minimum. DST = min(LHS, RHS)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :min)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-max (vm-instruction)
  "Maximum. DST = max(LHS, RHS)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :max)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-truncate (vm-instruction)
  "Truncate division. DST = truncate(LHS / RHS)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :truncate)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-floor-inst (vm-instruction)
  "Floor division. DST = floor(LHS / RHS)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :floor)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-ceiling-inst (vm-instruction)
  "Ceiling division. DST = ceiling(LHS / RHS)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :ceiling)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-rem (vm-instruction)
  "Remainder. DST = rem(LHS, RHS)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :rem)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-evenp (vm-instruction)
  "Even predicate. Returns 1 if SRC is even, 0 otherwise."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :evenp)
  (:sexp-slots dst src))

(define-vm-instruction vm-oddp (vm-instruction)
  "Odd predicate. Returns 1 if SRC is odd, 0 otherwise."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :oddp)
  (:sexp-slots dst src))

;;; Boolean Operations

(define-vm-instruction vm-not (vm-instruction)
  "Logical not. Returns 1 if SRC is 0, 0 otherwise."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :not)
  (:sexp-slots dst src))

(define-vm-instruction vm-and (vm-instruction)
  "Logical and. Returns 1 if both LHS and RHS are non-zero, 0 otherwise."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :and)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-or (vm-instruction)
  "Logical or. Returns 1 if either LHS or RHS is non-zero, 0 otherwise."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :or)
  (:sexp-slots dst lhs rhs))

;;; General Type Predicate

(define-vm-instruction vm-typep (vm-instruction)
  "General type check. Returns 1 if SRC is of TYPE-NAME, 0 otherwise.
TYPE-NAME is a symbol like INTEGER, STRING, SYMBOL, CONS, NULL, LIST, etc."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (type-name nil :reader vm-type-name)
  (:sexp-tag :typep)
  (:sexp-slots dst src type-name))

;;; Instruction Execution - General Type Predicate

(defun vm-typep-check (value type-sym)
  "Check if VALUE is of TYPE-SYM. Handles both host CL types and VM CLOS types."
  (case type-sym
    ((integer fixnum) (integerp value))
    ((string) (stringp value))
    ((symbol) (symbolp value))
    ((cons) (consp value))
    ((null) (null value))
    ((list) (listp value))
    ((number) (numberp value))
    ((character) (characterp value))
    ((atom) (atom value))
    ((function) (functionp value))
    ((hash-table) (hash-table-p value))
    (otherwise
     (if (and (hash-table-p value)
              (gethash :__class__ value))
         (let* ((class-ht (gethash :__class__ value))
                (class-name (when (hash-table-p class-ht)
                              (gethash :__name__ class-ht)))
                (cpl (when (hash-table-p class-ht)
                       (gethash :__cpl__ class-ht))))
           (or (eq class-name type-sym)
               (member type-sym cpl)))
         (ignore-errors (typep value type-sym))))))

(defmethod execute-instruction ((inst vm-typep) state pc labels)
  (declare (ignore labels))
  (let* ((value (vm-reg-get state (vm-src inst)))
         (type-sym (vm-type-name inst))
         (result (if (vm-typep-check value type-sym) 1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; Instruction Execution - Type Predicates

(define-simple-instruction vm-eq :pred2 eql)

(define-simple-instruction vm-cons-p :pred1 consp)

(define-simple-instruction vm-null-p :pred1 null)

(define-simple-instruction vm-symbol-p :pred1 symbolp)

(define-simple-instruction vm-number-p :pred1 numberp)

(define-simple-instruction vm-integer-p :pred1 integerp)

(defmethod execute-instruction ((inst vm-function-p) state pc labels)
  (declare (ignore labels))
  (let* ((value (vm-reg-get state (vm-src inst)))
         (result (if (typep value 'vm-closure-object) 1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; Instruction Execution - Comparison Operations

(define-simple-instruction vm-lt :pred2 <)

(define-simple-instruction vm-gt :pred2 >)

(define-simple-instruction vm-le :pred2 <=)

(define-simple-instruction vm-ge :pred2 >=)

(define-simple-instruction vm-num-eq :pred2 =)

;;; Instruction Execution - Arithmetic Extensions

(defmethod execute-instruction ((inst vm-div) state pc labels)
  (declare (ignore labels))
  (let ((divisor (vm-reg-get state (vm-rhs inst))))
    (if (zerop divisor)
        (error "vm-div: Division by zero")
        (let ((result (floor (vm-reg-get state (vm-lhs inst)) divisor)))
          (vm-reg-set state (vm-dst inst) result)
          (values (1+ pc) nil nil)))))

(defmethod execute-instruction ((inst vm-mod) state pc labels)
  (declare (ignore labels))
  (let ((divisor (vm-reg-get state (vm-rhs inst))))
    (if (zerop divisor)
        (error "vm-mod: Division by zero")
        (let ((result (mod (vm-reg-get state (vm-lhs inst)) divisor)))
          (vm-reg-set state (vm-dst inst) result)
          (values (1+ pc) nil nil)))))

(define-simple-instruction vm-neg :unary -)

(define-simple-instruction vm-abs :unary abs)

(define-simple-instruction vm-inc :unary 1+)

(define-simple-instruction vm-dec :unary 1-)

(define-simple-instruction vm-min :binary min)

(define-simple-instruction vm-max :binary max)

(defmethod execute-instruction ((inst vm-truncate) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (q r)
      (truncate (vm-reg-get state (vm-lhs inst))
                (vm-reg-get state (vm-rhs inst)))
    (vm-reg-set state (vm-dst inst) q)
    (setf (vm-values-list state) (list q r)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-floor-inst) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (q r)
      (floor (vm-reg-get state (vm-lhs inst))
             (vm-reg-get state (vm-rhs inst)))
    (vm-reg-set state (vm-dst inst) q)
    (setf (vm-values-list state) (list q r)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-ceiling-inst) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (q r)
      (ceiling (vm-reg-get state (vm-lhs inst))
               (vm-reg-get state (vm-rhs inst)))
    (vm-reg-set state (vm-dst inst) q)
    (setf (vm-values-list state) (list q r)))
  (values (1+ pc) nil nil))

(define-simple-instruction vm-rem :binary rem)

(define-simple-instruction vm-evenp :pred1 evenp)

(define-simple-instruction vm-oddp :pred1 oddp)

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

(define-vm-instruction vm-type-of (vm-instruction)
  "Get the type of an object."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :type-of)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-type-of) state pc labels)
  (declare (ignore labels))
  (let* ((value (vm-reg-get state (vm-src inst)))
         (result (cond
                   ((null value) 'null)
                   ((integerp value) 'integer)
                   ((stringp value) 'string)
                   ((characterp value) 'character)
                   ((symbolp value) 'symbol)
                   ((consp value) 'cons)
                   ((vectorp value) 'vector)
                   ((hash-table-p value) 'hash-table)
                   (t 't))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; Runtime Eval — Meta-circular evaluation

(define-vm-instruction vm-eval (vm-instruction)
  "Evaluate a form at runtime by compiling and running it in a fresh VM.
This enables meta-circular self-hosting: compiled code can call eval."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :eval)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-eval) state pc labels)
  (declare (ignore labels))
  (let* ((form (vm-reg-get state (vm-src inst)))
         (result (our-eval form)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; ------------------------------------------------------------
;;; Phase 2: Numeric Tower (FR-301 to FR-305)
;;; ------------------------------------------------------------

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

;;; FR-303: Bit Operations

(define-vm-instruction vm-ash (vm-instruction)
  "Arithmetic shift: (ash integer count). Positive count = left shift."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :ash)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-ash :binary ash)

(define-vm-instruction vm-logand (vm-instruction)
  "Bitwise AND."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :logand)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-logand :binary logand)

(define-vm-instruction vm-logior (vm-instruction)
  "Bitwise inclusive OR."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :logior)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-logior :binary logior)

(define-vm-instruction vm-logxor (vm-instruction)
  "Bitwise exclusive OR."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :logxor)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-logxor :binary logxor)

(define-vm-instruction vm-logeqv (vm-instruction)
  "Bitwise equivalence (XNOR)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :logeqv)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-logeqv :binary logeqv)

(define-vm-instruction vm-lognot (vm-instruction)
  "Bitwise complement."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :lognot)
  (:sexp-slots dst src))

(define-simple-instruction vm-lognot :unary lognot)

(define-vm-instruction vm-logtest (vm-instruction)
  "Test if any bits are set in common: (logtest j k) => t/nil."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :logtest)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-logtest :pred2 logtest)

(define-vm-instruction vm-logbitp (vm-instruction)
  "Test if bit INDEX is set in INTEGER: (logbitp index integer) => t/nil."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :logbitp)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-logbitp :pred2 logbitp)

(define-vm-instruction vm-logcount (vm-instruction)
  "Count set bits in integer."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :logcount)
  (:sexp-slots dst src))

(define-simple-instruction vm-logcount :unary logcount)

(define-vm-instruction vm-integer-length (vm-instruction)
  "Number of bits needed to represent integer."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :integer-length)
  (:sexp-slots dst src))

(define-simple-instruction vm-integer-length :unary integer-length)

;;; FR-304: Transcendental Functions

(define-vm-instruction vm-expt (vm-instruction)
  "Exponentiation: (expt base power)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :expt)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-expt :binary expt)

(define-vm-instruction vm-sqrt (vm-instruction)
  "Square root."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :sqrt)
  (:sexp-slots dst src))

(define-simple-instruction vm-sqrt :unary sqrt)

(define-vm-instruction vm-exp-inst (vm-instruction)
  "e raised to the power x."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :exp)
  (:sexp-slots dst src))

(define-simple-instruction vm-exp-inst :unary exp)

(define-vm-instruction vm-log-inst (vm-instruction)
  "Natural logarithm."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :log)
  (:sexp-slots dst src))

(define-simple-instruction vm-log-inst :unary log)

(define-vm-instruction vm-sin-inst (vm-instruction)
  "Sine (radians)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :sin)
  (:sexp-slots dst src))

(define-simple-instruction vm-sin-inst :unary sin)

(define-vm-instruction vm-cos-inst (vm-instruction)
  "Cosine (radians)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :cos)
  (:sexp-slots dst src))

(define-simple-instruction vm-cos-inst :unary cos)

(define-vm-instruction vm-tan-inst (vm-instruction)
  "Tangent (radians)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :tan)
  (:sexp-slots dst src))

(define-simple-instruction vm-tan-inst :unary tan)

(define-vm-instruction vm-asin-inst (vm-instruction)
  "Arc sine."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :asin)
  (:sexp-slots dst src))

(define-simple-instruction vm-asin-inst :unary asin)

(define-vm-instruction vm-acos-inst (vm-instruction)
  "Arc cosine."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :acos)
  (:sexp-slots dst src))

(define-simple-instruction vm-acos-inst :unary acos)

(define-vm-instruction vm-atan-inst (vm-instruction)
  "Arc tangent (1-arg: atan y)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :atan)
  (:sexp-slots dst src))

(define-simple-instruction vm-atan-inst :unary atan)

(define-vm-instruction vm-atan2-inst (vm-instruction)
  "Arc tangent (2-arg: atan y x)."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :atan2)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-atan2-inst :binary atan)

(define-vm-instruction vm-sinh-inst (vm-instruction)
  "Hyperbolic sine."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :sinh)
  (:sexp-slots dst src))

(define-simple-instruction vm-sinh-inst :unary sinh)

(define-vm-instruction vm-cosh-inst (vm-instruction)
  "Hyperbolic cosine."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :cosh)
  (:sexp-slots dst src))

(define-simple-instruction vm-cosh-inst :unary cosh)

(define-vm-instruction vm-tanh-inst (vm-instruction)
  "Hyperbolic tangent."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :tanh)
  (:sexp-slots dst src))

(define-simple-instruction vm-tanh-inst :unary tanh)

;;; FR-305: Float Operations

(define-vm-instruction vm-float-inst (vm-instruction)
  "Convert number to float."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :float)
  (:sexp-slots dst src))

(define-simple-instruction vm-float-inst :unary float)

(define-vm-instruction vm-float-precision (vm-instruction)
  "Number of significant radix digits in a float."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :float-precision)
  (:sexp-slots dst src))

(define-simple-instruction vm-float-precision :unary float-precision)

(define-vm-instruction vm-float-radix (vm-instruction)
  "Radix of the float representation."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :float-radix)
  (:sexp-slots dst src))

(define-simple-instruction vm-float-radix :unary float-radix)

(define-vm-instruction vm-float-sign (vm-instruction)
  "Sign of a float as float."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :float-sign)
  (:sexp-slots dst src))

(define-simple-instruction vm-float-sign :unary float-sign)

(define-vm-instruction vm-float-digits (vm-instruction)
  "Number of radix digits in the float mantissa."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :float-digits)
  (:sexp-slots dst src))

(define-simple-instruction vm-float-digits :unary float-digits)

(define-vm-instruction vm-scale-float (vm-instruction)
  "Scale a float by a power of the radix."
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs)
  (:sexp-tag :scale-float)
  (:sexp-slots dst lhs rhs))

(define-simple-instruction vm-scale-float :binary scale-float)

(define-vm-instruction vm-decode-float (vm-instruction)
  "Decode float into significand, exponent, sign (3 multiple values)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :decode-float)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-decode-float) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (sig exp sign)
      (decode-float (vm-reg-get state (vm-src inst)))
    (vm-reg-set state (vm-dst inst) sig)
    (setf (vm-values-list state) (list sig exp sign)))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-integer-decode-float (vm-instruction)
  "Decode float into integer significand, exponent, sign (3 multiple values)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :integer-decode-float)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-integer-decode-float) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (sig exp sign)
      (integer-decode-float (vm-reg-get state (vm-src inst)))
    (vm-reg-set state (vm-dst inst) sig)
    (setf (vm-values-list state) (list sig exp sign)))
  (values (1+ pc) nil nil))

;;; Phase 3+: FR-1202 Environment Predicates and FR-1205 Random

;; FR-1202: boundp — test if a symbol has a global variable binding
(define-vm-instruction vm-boundp (vm-instruction)
  "Test if symbol has a global variable value. Returns T/nil."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :boundp)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-boundp) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-src inst)))
         (result (nth-value 1 (gethash sym (vm-global-vars state)))))
    (vm-reg-set state (vm-dst inst) (if result t nil))
    (values (1+ pc) nil nil)))

;; FR-1202: fboundp — test if a symbol names a function
(define-vm-instruction vm-fboundp (vm-instruction)
  "Test if symbol names a function in the VM function table. Returns T/nil."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :fboundp)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-fboundp) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-src inst)))
         (result (gethash sym (vm-function-registry state))))
    (vm-reg-set state (vm-dst inst) (if result t nil))
    (values (1+ pc) nil nil)))

;; FR-1202: makunbound — remove a global variable binding
(define-vm-instruction vm-makunbound (vm-instruction)
  "Remove global variable binding for SYM. Returns SYM."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :makunbound)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-makunbound) state pc labels)
  (declare (ignore labels))
  (let ((sym (vm-reg-get state (vm-src inst))))
    (remhash sym (vm-global-vars state))
    (vm-reg-set state (vm-dst inst) sym)
    (values (1+ pc) nil nil)))

;; FR-1202: fmakunbound — remove a function binding
(define-vm-instruction vm-fmakunbound (vm-instruction)
  "Remove function binding for SYM. Returns SYM."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :fmakunbound)
  (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-fmakunbound) state pc labels)
  (declare (ignore labels))
  (let ((sym (vm-reg-get state (vm-src inst))))
    (remhash sym (vm-function-registry state))
    (vm-reg-set state (vm-dst inst) sym)
    (values (1+ pc) nil nil)))

;; FR-1205: random — generate a random number
(define-vm-instruction vm-random (vm-instruction)
  "Generate a random number in [0, LIMIT)."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :random)
  (:sexp-slots dst src))

(define-simple-instruction vm-random :unary random)

;; FR-1205: make-random-state
(define-vm-instruction vm-make-random-state (vm-instruction)
  "Create a random state. If SRC is nil, copy current *random-state*; if t, generate fresh."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :make-random-state) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-make-random-state) state pc labels)
  (declare (ignore labels))
  (let ((arg (vm-reg-get state (vm-src inst))))
    (vm-reg-set state (vm-dst inst)
                (cond ((eq arg t)  (make-random-state t))
                      ((eq arg nil) (make-random-state))
                      (t (make-random-state arg))))
    (values (1+ pc) nil nil)))

;; FR-1204: get-universal-time — seconds since 1900-01-01
(define-vm-instruction vm-get-universal-time (vm-instruction)
  "Return current time as universal time integer."
  (dst nil :reader vm-dst)
  (:sexp-tag :get-universal-time)
  (:sexp-slots dst))

(defmethod execute-instruction ((inst vm-get-universal-time) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (get-universal-time))
  (values (1+ pc) nil nil))

;; FR-1204: get-internal-real-time
(define-vm-instruction vm-get-internal-real-time (vm-instruction)
  "Return internal real time as integer."
  (dst nil :reader vm-dst)
  (:sexp-tag :get-internal-real-time)
  (:sexp-slots dst))

(defmethod execute-instruction ((inst vm-get-internal-real-time) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (get-internal-real-time))
  (values (1+ pc) nil nil))

;; FR-1204: get-internal-run-time
(define-vm-instruction vm-get-internal-run-time (vm-instruction)
  "Return internal run time (CPU time) as integer."
  (dst nil :reader vm-dst)
  (:sexp-tag :get-internal-run-time)
  (:sexp-slots dst))

(defmethod execute-instruction ((inst vm-get-internal-run-time) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (get-internal-run-time))
  (values (1+ pc) nil nil))

;; FR-1204: decode-universal-time — returns 9 values via vm-values-list
(define-vm-instruction vm-decode-universal-time (vm-instruction)
  "Decode universal time into (sec min hour day month year day-of-week dst-p tz)."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :decode-universal-time) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-decode-universal-time) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (sec min hour date month year day dst-p zone)
      (decode-universal-time (vm-reg-get state (vm-src inst)))
    (setf (vm-values-list state) (list sec min hour date month year day dst-p zone))
    (vm-reg-set state (vm-dst inst) sec)
    (values (1+ pc) nil nil)))

;; FR-1204: encode-universal-time — takes 6 required + optional tz in a list
(define-vm-instruction vm-encode-universal-time (vm-instruction)
  "Encode (sec min hour date month year &optional zone) as universal time."
  (dst nil :reader vm-dst) (args-reg nil :reader vm-args-reg)
  (:sexp-tag :encode-universal-time) (:sexp-slots dst args-reg))

(defmethod execute-instruction ((inst vm-encode-universal-time) state pc labels)
  (declare (ignore labels))
  (let* ((args (vm-reg-get state (vm-args-reg inst)))
         (sec   (nth 0 args))
         (min   (nth 1 args))
         (hour  (nth 2 args))
         (date  (nth 3 args))
         (month (nth 4 args))
         (year  (nth 5 args))
         (zone  (nth 6 args))
         (result (if zone
                     (encode-universal-time sec min hour date month year zone)
                     (encode-universal-time sec min hour date month year))))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; FR-301: Float rounding functions (ffloor, fceiling, ftruncate, fround)

(define-vm-instruction vm-ffloor (vm-instruction)
  "Float floor: returns float quotient and float remainder."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :ffloor) (:sexp-slots dst lhs rhs))

(defmethod execute-instruction ((inst vm-ffloor) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (q r) (ffloor (vm-reg-get state (vm-lhs inst))
                                      (vm-reg-get state (vm-rhs inst)))
    (vm-reg-set state (vm-dst inst) q)
    (setf (vm-values-list state) (list q r))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-fceiling (vm-instruction)
  "Float ceiling: returns float quotient and float remainder."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :fceiling) (:sexp-slots dst lhs rhs))

(defmethod execute-instruction ((inst vm-fceiling) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (q r) (fceiling (vm-reg-get state (vm-lhs inst))
                                        (vm-reg-get state (vm-rhs inst)))
    (vm-reg-set state (vm-dst inst) q)
    (setf (vm-values-list state) (list q r))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-ftruncate (vm-instruction)
  "Float truncate: returns float quotient and float remainder."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :ftruncate) (:sexp-slots dst lhs rhs))

(defmethod execute-instruction ((inst vm-ftruncate) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (q r) (ftruncate (vm-reg-get state (vm-lhs inst))
                                         (vm-reg-get state (vm-rhs inst)))
    (vm-reg-set state (vm-dst inst) q)
    (setf (vm-values-list state) (list q r))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-fround (vm-instruction)
  "Float round: returns float quotient and float remainder."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :fround) (:sexp-slots dst lhs rhs))

(defmethod execute-instruction ((inst vm-fround) state pc labels)
  (declare (ignore labels))
  (multiple-value-bind (q r) (fround (vm-reg-get state (vm-lhs inst))
                                      (vm-reg-get state (vm-rhs inst)))
    (vm-reg-set state (vm-dst inst) q)
    (setf (vm-values-list state) (list q r))
    (values (1+ pc) nil nil)))

;;; FR-306: Rational number functions

(define-vm-instruction vm-rational (vm-instruction)
  "Convert number to rational."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :rational) (:sexp-slots dst src))
(defmethod execute-instruction ((inst vm-rational) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (rational (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-rationalize (vm-instruction)
  "Rationalize a float to closest rational."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :rationalize) (:sexp-slots dst src))
(defmethod execute-instruction ((inst vm-rationalize) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (rationalize (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-numerator (vm-instruction)
  "Return numerator of rational."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :numerator) (:sexp-slots dst src))
(defmethod execute-instruction ((inst vm-numerator) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (numerator (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-denominator (vm-instruction)
  "Return denominator of rational."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :denominator) (:sexp-slots dst src))
(defmethod execute-instruction ((inst vm-denominator) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (denominator (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-gcd (vm-instruction)
  "Return greatest common divisor."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :gcd) (:sexp-slots dst lhs rhs))
(defmethod execute-instruction ((inst vm-gcd) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (gcd (vm-reg-get state (vm-lhs inst)) (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-lcm (vm-instruction)
  "Return least common multiple."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :lcm) (:sexp-slots dst lhs rhs))
(defmethod execute-instruction ((inst vm-lcm) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (lcm (vm-reg-get state (vm-lhs inst)) (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

;;; FR-307: Complex number functions

(define-vm-instruction vm-realpart (vm-instruction)
  "Return real part of number."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :realpart) (:sexp-slots dst src))
(defmethod execute-instruction ((inst vm-realpart) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (realpart (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-imagpart (vm-instruction)
  "Return imaginary part of number."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :imagpart) (:sexp-slots dst src))
(defmethod execute-instruction ((inst vm-imagpart) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (imagpart (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-conjugate (vm-instruction)
  "Return complex conjugate."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :conjugate) (:sexp-slots dst src))
(defmethod execute-instruction ((inst vm-conjugate) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (conjugate (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-phase (vm-instruction)
  "Return phase angle of complex number."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :phase) (:sexp-slots dst src))
(defmethod execute-instruction ((inst vm-phase) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (phase (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(define-vm-instruction vm-complex (vm-instruction)
  "Construct a complex number from real and imaginary parts."
  (dst nil :reader vm-dst) (lhs nil :reader vm-lhs) (rhs nil :reader vm-rhs)
  (:sexp-tag :complex) (:sexp-slots dst lhs rhs))
(defmethod execute-instruction ((inst vm-complex) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (complex (vm-reg-get state (vm-lhs inst)) (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

;;; FR-407: Case-insensitive char comparisons (remaining two)

(define-vm-instruction vm-char-not-greaterp (vm-instruction)
  "Case-insensitive CHAR<=: T if CHAR1 <= CHAR2 ignoring case."
  (dst nil :reader vm-dst) (char1 nil :reader vm-char1) (char2 nil :reader vm-char2)
  (:sexp-tag :char-not-greaterp) (:sexp-slots dst char1 char2))
(define-simple-instruction vm-char-not-greaterp :pred2 char-not-greaterp :lhs vm-char1 :rhs vm-char2)

(define-vm-instruction vm-char-not-lessp (vm-instruction)
  "Case-insensitive CHAR>=: T if CHAR1 >= CHAR2 ignoring case."
  (dst nil :reader vm-dst) (char1 nil :reader vm-char1) (char2 nil :reader vm-char2)
  (:sexp-tag :char-not-lessp) (:sexp-slots dst char1 char2))
(define-simple-instruction vm-char-not-lessp :pred2 char-not-lessp :lhs vm-char1 :rhs vm-char2)

;;; FR-1201: Symbol property list operations

(define-vm-instruction vm-symbol-get (vm-instruction)
  "Get property INDICATOR from symbol SYM's plist. Returns default if absent."
  (dst nil :reader vm-dst) (sym nil :reader vm-sym)
  (indicator nil :reader vm-indicator) (default nil :reader vm-default)
  (:sexp-tag :symbol-get) (:sexp-slots dst sym indicator default))

(defmethod execute-instruction ((inst vm-symbol-get) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-sym inst)))
         (indicator (vm-reg-get state (vm-indicator inst)))
         (default (vm-reg-get state (vm-default inst)))
         (plist (gethash sym (vm-symbol-plists state) nil))
         (result (getf plist indicator default)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-symbol-set (vm-instruction)
  "Set property INDICATOR on symbol SYM's plist to VALUE."
  (dst nil :reader vm-dst) (sym nil :reader vm-sym)
  (indicator nil :reader vm-indicator) (value nil :reader vm-value)
  (:sexp-tag :symbol-set) (:sexp-slots dst sym indicator value))

(defmethod execute-instruction ((inst vm-symbol-set) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-sym inst)))
         (indicator (vm-reg-get state (vm-indicator inst)))
         (val (vm-reg-get state (vm-value inst)))
         (plist (gethash sym (vm-symbol-plists state) nil)))
    (setf (getf plist indicator) val)
    (setf (gethash sym (vm-symbol-plists state)) plist)
    (vm-reg-set state (vm-dst inst) val)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-remprop (vm-instruction)
  "Remove property INDICATOR from symbol SYM's plist."
  (dst nil :reader vm-dst) (sym nil :reader vm-sym)
  (indicator nil :reader vm-indicator)
  (:sexp-tag :remprop) (:sexp-slots dst sym indicator))

(defmethod execute-instruction ((inst vm-remprop) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-sym inst)))
         (indicator (vm-reg-get state (vm-indicator inst)))
         (plist (gethash sym (vm-symbol-plists state) nil))
         (found-p (not (eq (getf plist indicator :__not-found__) :__not-found__))))
    (remf plist indicator)
    (if plist
        (setf (gethash sym (vm-symbol-plists state)) plist)
        (remhash sym (vm-symbol-plists state)))
    (vm-reg-set state (vm-dst inst) (if found-p t nil))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-symbol-plist (vm-instruction)
  "Return entire property list of SYM."
  (dst nil :reader vm-dst) (src nil :reader vm-src)
  (:sexp-tag :symbol-plist) (:sexp-slots dst src))

(defmethod execute-instruction ((inst vm-symbol-plist) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-src inst)))
         (plist (gethash sym (vm-symbol-plists state) nil)))
    (vm-reg-set state (vm-dst inst) plist)
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-set-symbol-plist (vm-instruction)
  "Set the entire property list of SYM to PLIST."
  (dst nil :reader vm-dst) (sym nil :reader vm-sym) (plist-reg nil :reader vm-plist-reg)
  (:sexp-tag :set-symbol-plist) (:sexp-slots dst sym plist-reg))

(defmethod execute-instruction ((inst vm-set-symbol-plist) state pc labels)
  (declare (ignore labels))
  (let* ((sym (vm-reg-get state (vm-sym inst)))
         (new-plist (vm-reg-get state (vm-plist-reg inst))))
    (if new-plist
        (setf (gethash sym (vm-symbol-plists state)) new-plist)
        (remhash sym (vm-symbol-plists state)))
    (vm-reg-set state (vm-dst inst) new-plist)
    (values (1+ pc) nil nil)))

;;; FR-102: PROGV — dynamic variable binding

(define-vm-instruction vm-progv-enter (vm-instruction)
  "Save current global bindings for SYMS-REG, then bind each to the
   corresponding value in VALS-REG.  Store the saved alist in DST so
   vm-progv-exit can restore it."
  (dst nil :reader vm-dst)
  (syms nil :reader vm-syms-reg)
  (vals nil :reader vm-vals-reg)
  (:sexp-tag :progv-enter)
  (:sexp-slots dst syms vals))

(defmethod execute-instruction ((inst vm-progv-enter) state pc labels)
  (declare (ignore labels))
  (let* ((syms (vm-reg-get state (vm-syms-reg inst)))
         (vals (vm-reg-get state (vm-vals-reg inst)))
         (global-vars (vm-global-vars state))
         (saved nil))
    ;; Save old bindings (or :unbound sentinel)
    (dolist (sym syms)
      (multiple-value-bind (old-val found-p) (gethash sym global-vars)
        (push (cons sym (if found-p old-val :unbound)) saved)))
    ;; Bind new values
    (let ((val-ptr vals))
      (dolist (sym syms)
        (if val-ptr
            (progn
              (setf (gethash sym global-vars) (car val-ptr))
              (setf val-ptr (cdr val-ptr)))
            (setf (gethash sym global-vars) nil))))
    ;; Return saved alist so vm-progv-exit can restore
    (vm-reg-set state (vm-dst inst) (nreverse saved))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-progv-exit (vm-instruction)
  "Restore global bindings from the saved alist produced by vm-progv-enter."
  (saved nil :reader vm-saved-reg)
  (:sexp-tag :progv-exit)
  (:sexp-slots saved))

(defmethod execute-instruction ((inst vm-progv-exit) state pc labels)
  (declare (ignore labels))
  (let ((saved (vm-reg-get state (vm-saved-reg inst)))
        (global-vars (vm-global-vars state)))
    (dolist (pair saved)
      (let ((sym (car pair))
            (old-val (cdr pair)))
        (if (eq old-val :unbound)
            (remhash sym global-vars)
            (setf (gethash sym global-vars) old-val))))
    (values (1+ pc) nil nil)))
