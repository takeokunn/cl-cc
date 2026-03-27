(in-package :cl-cc)

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

(define-simple-instruction vm-eq        :pred2 eql)
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

(define-simple-instruction vm-neg :unary  -)
(define-simple-instruction vm-abs :unary  abs)
(define-simple-instruction vm-inc :unary  1+)
(define-simple-instruction vm-dec :unary  1-)
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

(define-simple-instruction vm-rem   :binary rem)
(define-simple-instruction vm-evenp :pred1  evenp)
(define-simple-instruction vm-oddp  :pred1  oddp)

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

(define-vm-unary-instruction vm-type-of :type-of "Get the type of an object.")

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

(define-vm-unary-instruction vm-eval :eval
  "Evaluate a form at runtime by compiling and running it in a fresh VM.
This enables meta-circular self-hosting: compiled code can call eval.")

(defmethod execute-instruction ((inst vm-eval) state pc labels)
  (declare (ignore labels))
  (let* ((form (vm-reg-get state (vm-src inst)))
         (result (our-eval form)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; vm-numeric.lisp — Numeric tower (round, bit ops, transcendentals, float, rational, complex)
;;; vm-extensions.lisp — Char comparisons, symbol plist, PROGV, generic arith
