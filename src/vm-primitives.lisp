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
