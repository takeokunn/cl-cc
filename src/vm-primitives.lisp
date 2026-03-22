(in-package :cl-cc)

;;; ----------------------------------------------------------------------------
;;; VM Primitive Instructions
;;; ----------------------------------------------------------------------------
;;;
;;; This file extends the VM with primitive type predicates, comparisons,
;;; arithmetic extensions, and boolean operations.
;;;

;;; ----------------------------------------------------------------------------
;;; Type Predicates
;;; ----------------------------------------------------------------------------

(defclass vm-eq (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs))
  (:documentation "EQL comparison. Returns 1 if LHS equals RHS, 0 otherwise."))

(defclass vm-cons-p (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Type predicate for cons cells. Returns 1 if SRC is a cons cell, 0 otherwise."))

(defclass vm-null-p (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Type predicate for nil. Returns 1 if SRC is nil, 0 otherwise."))

(defclass vm-symbol-p (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Type predicate for symbols. Returns 1 if SRC is a symbol, 0 otherwise."))

(defclass vm-number-p (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Type predicate for numbers. Returns 1 if SRC is a number, 0 otherwise."))

(defclass vm-integer-p (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Type predicate for integers. Returns 1 if SRC is an integer, 0 otherwise."))

(defclass vm-function-p (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Type predicate for functions/closures. Returns 1 if SRC is a function, 0 otherwise."))

;;; ----------------------------------------------------------------------------
;;; Comparison Operations
;;; ----------------------------------------------------------------------------

(defclass vm-lt (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs))
  (:documentation "Less than comparison. Returns 1 if LHS < RHS, 0 otherwise."))

(defclass vm-gt (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs))
  (:documentation "Greater than comparison. Returns 1 if LHS > RHS, 0 otherwise."))

(defclass vm-le (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs))
  (:documentation "Less than or equal comparison. Returns 1 if LHS <= RHS, 0 otherwise."))

(defclass vm-ge (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs))
  (:documentation "Greater than or equal comparison. Returns 1 if LHS >= RHS, 0 otherwise."))

(defclass vm-num-eq (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs))
  (:documentation "Numeric equality comparison. Returns 1 if LHS = RHS, 0 otherwise."))

;;; ----------------------------------------------------------------------------
;;; Arithmetic Extensions
;;; ----------------------------------------------------------------------------

(defclass vm-div (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs))
  (:documentation "Integer division. DST = floor(LHS / RHS)."))

(defclass vm-mod (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs))
  (:documentation "Modulo operation. DST = LHS mod RHS."))

(defclass vm-neg (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Negation. DST = -SRC."))

(defclass vm-abs (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Absolute value. DST = |SRC|."))

(defclass vm-inc (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Increment. DST = SRC + 1."))

(defclass vm-dec (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Decrement. DST = SRC - 1."))

(defclass vm-min (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs))
  (:documentation "Minimum. DST = min(LHS, RHS)."))

(defclass vm-max (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs))
  (:documentation "Maximum. DST = max(LHS, RHS)."))

(defclass vm-truncate (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs))
  (:documentation "Truncate division. DST = truncate(LHS / RHS)."))

(defclass vm-floor-inst (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs))
  (:documentation "Floor division. DST = floor(LHS / RHS)."))

(defclass vm-ceiling-inst (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs))
  (:documentation "Ceiling division. DST = ceiling(LHS / RHS)."))

(defclass vm-rem (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs))
  (:documentation "Remainder. DST = rem(LHS, RHS)."))

(defclass vm-evenp (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Even predicate. Returns 1 if SRC is even, 0 otherwise."))

(defclass vm-oddp (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Odd predicate. Returns 1 if SRC is odd, 0 otherwise."))

;;; ----------------------------------------------------------------------------
;;; Boolean Operations
;;; ----------------------------------------------------------------------------

(defclass vm-not (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Logical not. Returns 1 if SRC is 0, 0 otherwise."))

(defclass vm-and (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs))
  (:documentation "Logical and. Returns 1 if both LHS and RHS are non-zero, 0 otherwise."))

(defclass vm-or (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs))
  (:documentation "Logical or. Returns 1 if either LHS or RHS is non-zero, 0 otherwise."))

;;; ----------------------------------------------------------------------------
;;; General Type Predicate
;;; ----------------------------------------------------------------------------

(defclass vm-typep (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src)
   (type-name :initarg :type-name :reader vm-type-name))
  (:documentation "General type check. Returns 1 if SRC is of TYPE-NAME, 0 otherwise.
TYPE-NAME is a symbol like INTEGER, STRING, SYMBOL, CONS, NULL, LIST, etc."))

;;; ----------------------------------------------------------------------------
;;; Instruction -> S-expression Conversion
;;; ----------------------------------------------------------------------------

;; Type predicates
(defmethod instruction->sexp ((inst vm-typep))
  (list :typep (vm-dst inst) (vm-src inst) (vm-type-name inst)))

(defmethod instruction->sexp ((inst vm-eq))
  (list :eq (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))

(defmethod instruction->sexp ((inst vm-cons-p))
  (list :cons-p (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-null-p))
  (list :null-p (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-symbol-p))
  (list :symbol-p (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-number-p))
  (list :number-p (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-integer-p))
  (list :integer-p (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-function-p))
  (list :function-p (vm-dst inst) (vm-src inst)))

;; Comparison operations
(defmethod instruction->sexp ((inst vm-lt))
  (list :lt (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))

(defmethod instruction->sexp ((inst vm-gt))
  (list :gt (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))

(defmethod instruction->sexp ((inst vm-le))
  (list :le (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))

(defmethod instruction->sexp ((inst vm-ge))
  (list :ge (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))

(defmethod instruction->sexp ((inst vm-num-eq))
  (list :num-eq (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))

;; Arithmetic extensions
(defmethod instruction->sexp ((inst vm-div))
  (list :div (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))

(defmethod instruction->sexp ((inst vm-mod))
  (list :mod (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))

(defmethod instruction->sexp ((inst vm-neg))
  (list :neg (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-abs))
  (list :abs (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-inc))
  (list :inc (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-dec))
  (list :dec (vm-dst inst) (vm-src inst)))

;; Boolean operations
(defmethod instruction->sexp ((inst vm-not))
  (list :not (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-and))
  (list :and (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))

(defmethod instruction->sexp ((inst vm-or))
  (list :or (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))

;;; ----------------------------------------------------------------------------
;;; S-expression -> Instruction Conversion (Extended)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Instruction Execution - General Type Predicate
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;; Instruction Execution - Type Predicates
;;; ----------------------------------------------------------------------------

(defmethod execute-instruction ((inst vm-eq) state pc labels)
  (declare (ignore labels))
  (let ((result (if (eql (vm-reg-get state (vm-lhs inst))
                         (vm-reg-get state (vm-rhs inst)))
                    1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-cons-p) state pc labels)
  (declare (ignore labels))
  (let* ((value (vm-reg-get state (vm-src inst)))
         (result (if (consp value) 1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-null-p) state pc labels)
  (declare (ignore labels))
  (let ((result (if (null (vm-reg-get state (vm-src inst))) 1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-symbol-p) state pc labels)
  (declare (ignore labels))
  (let ((result (if (symbolp (vm-reg-get state (vm-src inst))) 1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-number-p) state pc labels)
  (declare (ignore labels))
  (let ((result (if (numberp (vm-reg-get state (vm-src inst))) 1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-integer-p) state pc labels)
  (declare (ignore labels))
  (let ((result (if (integerp (vm-reg-get state (vm-src inst))) 1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-function-p) state pc labels)
  (declare (ignore labels))
  (let* ((value (vm-reg-get state (vm-src inst)))
         (result (if (typep value 'vm-closure-object) 1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; ----------------------------------------------------------------------------
;;; Instruction Execution - Comparison Operations
;;; ----------------------------------------------------------------------------

(defmethod execute-instruction ((inst vm-lt) state pc labels)
  (declare (ignore labels))
  (let ((result (if (< (vm-reg-get state (vm-lhs inst))
                       (vm-reg-get state (vm-rhs inst)))
                    1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-gt) state pc labels)
  (declare (ignore labels))
  (let ((result (if (> (vm-reg-get state (vm-lhs inst))
                       (vm-reg-get state (vm-rhs inst)))
                    1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-le) state pc labels)
  (declare (ignore labels))
  (let ((result (if (<= (vm-reg-get state (vm-lhs inst))
                        (vm-reg-get state (vm-rhs inst)))
                    1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-ge) state pc labels)
  (declare (ignore labels))
  (let ((result (if (>= (vm-reg-get state (vm-lhs inst))
                        (vm-reg-get state (vm-rhs inst)))
                    1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-num-eq) state pc labels)
  (declare (ignore labels))
  (let ((result (if (= (vm-reg-get state (vm-lhs inst))
                       (vm-reg-get state (vm-rhs inst)))
                    1 0)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; ----------------------------------------------------------------------------
;;; Instruction Execution - Arithmetic Extensions
;;; ----------------------------------------------------------------------------

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

(defmethod execute-instruction ((inst vm-neg) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (- (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-abs) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (abs (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-inc) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (1+ (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-dec) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (1- (vm-reg-get state (vm-src inst))))
  (values (1+ pc) nil nil))

;;; ----------------------------------------------------------------------------
;;; Instruction Execution - Extended Arithmetic
;;; ----------------------------------------------------------------------------

(defmethod instruction->sexp ((inst vm-min))
  (list :min (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))
(defmethod instruction->sexp ((inst vm-max))
  (list :max (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))
(defmethod instruction->sexp ((inst vm-truncate))
  (list :truncate (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))
(defmethod instruction->sexp ((inst vm-floor-inst))
  (list :floor (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))
(defmethod instruction->sexp ((inst vm-ceiling-inst))
  (list :ceiling (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))
(defmethod instruction->sexp ((inst vm-rem))
  (list :rem (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))
(defmethod instruction->sexp ((inst vm-evenp))
  (list :evenp (vm-dst inst) (vm-src inst)))
(defmethod instruction->sexp ((inst vm-oddp))
  (list :oddp (vm-dst inst) (vm-src inst)))

(defmethod execute-instruction ((inst vm-min) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (min (vm-reg-get state (vm-lhs inst))
                   (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-max) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (max (vm-reg-get state (vm-lhs inst))
                   (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

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

(defmethod execute-instruction ((inst vm-rem) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (rem (vm-reg-get state (vm-lhs inst))
                   (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-evenp) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (if (evenp (vm-reg-get state (vm-src inst))) 1 0))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-oddp) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst)
              (if (oddp (vm-reg-get state (vm-src inst))) 1 0))
  (values (1+ pc) nil nil))

;;; ----------------------------------------------------------------------------
;;; Instruction Execution - Boolean Operations
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;; Type-Of Instruction
;;; ----------------------------------------------------------------------------

(defclass vm-type-of (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src))
  (:documentation "Get the type of an object."))

(defmethod instruction->sexp ((inst vm-type-of))
  (list :type-of (vm-dst inst) (vm-src inst)))

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

;;; ----------------------------------------------------------------------------
;;; Runtime Eval — Meta-circular evaluation
;;; ----------------------------------------------------------------------------

(defclass vm-eval (vm-instruction)
  ((dst :initarg :dst :reader vm-dst
        :documentation "Register to store the evaluation result")
   (src :initarg :src :reader vm-src
        :documentation "Register containing the form to evaluate"))
  (:documentation "Evaluate a form at runtime by compiling and running it in a fresh VM.
This enables meta-circular self-hosting: compiled code can call eval."))

(defmethod instruction->sexp ((inst vm-eval))
  (list :eval (vm-dst inst) (vm-src inst)))

(defmethod execute-instruction ((inst vm-eval) state pc labels)
  (declare (ignore labels))
  (let* ((form (vm-reg-get state (vm-src inst)))
         (result (our-eval form)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

