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

(defun %vm-fixnum-rational-p (value)
  "Return T when VALUE is a rational with fixnum numerator and denominator."
  (and (rationalp value)
       (typep (numerator value) 'fixnum)
       (typep (denominator value) 'fixnum)))

(defun %vm-cl-div-fast-path (lhs rhs)
  "Return LHS/RHS and the selected rational arithmetic path.
The path value is diagnostic and keeps the runtime specialization testable."
  (cond
    ((and (typep lhs 'fixnum) (typep rhs 'fixnum))
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
  (let ((divisor (vm-reg-get state (vm-rhs inst))))
    (if (zerop divisor)
        (error "vm-div: Division by zero")
        (let ((result (floor (vm-reg-get state (vm-lhs inst)) divisor)))
          (vm-reg-set state (vm-dst inst) result)
          (values (1+ pc) nil nil)))))

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
                        (multiple-value-bind (q r)
                            (,host-fn (vm-reg-get state (vm-lhs inst))
                                      (vm-reg-get state (vm-rhs inst)))
                          (vm-reg-set state (vm-dst inst) q)
                          (setf (vm-values-list state) (list q r)))
                         (values (1+ pc) nil nil)))))

(define-vm-division-executors
  (vm-truncate     truncate)
  (vm-floor-inst   floor)
  (vm-ceiling-inst ceiling))

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

;;; vm-numeric.lisp — Numeric tower (round, bit ops, transcendentals, float, rational, complex)
;;; vm-extensions.lisp — Char comparisons, symbol plist, PROGV, generic arith
