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

;;; Instruction Execution - General Type Predicate

;;; Data table: maps type symbols to single-argument predicate functions.
(defparameter *vm-primitive-type-predicates*
  `((integer          . ,#'integerp)
    (fixnum           . ,#'integerp)
    (float            . ,#'floatp)
    (single-float     . ,#'floatp)
    (double-float     . ,#'floatp)
    (short-float      . ,#'floatp)
    (long-float       . ,#'floatp)
    (real             . ,#'realp)
    (rational         . ,#'rationalp)
    (complex          . ,#'complexp)
    (string           . ,#'stringp)
    (symbol           . ,#'symbolp)
    (keyword          . ,#'keywordp)
    (cons             . ,#'consp)
    (null             . ,#'null)
    (list             . ,#'listp)
    (number           . ,#'numberp)
    (character        . ,#'characterp)
    (base-char        . ,#'characterp)
    (standard-char    . ,#'characterp)
    (atom             . ,#'atom)
    (function         . ,#'functionp)
    (vector           . ,#'vectorp)
    (package          . ,#'packagep)
    (stream           . ,#'streamp)
    (input-stream     . ,#'input-stream-p)
    (output-stream    . ,#'output-stream-p)
    (bit-vector       . ,(lambda (v) (typep v 'bit-vector)))
    (simple-vector    . ,(lambda (v) (typep v 'simple-vector)))
    (simple-array     . ,(lambda (v) (typep v 'simple-array)))
    (array            . ,#'arrayp)
    (file-stream      . ,(lambda (v) (typep v 'file-stream)))
    (string-stream    . ,(lambda (v) (typep v 'string-stream)))
    (broadcast-stream . ,(lambda (v) (typep v 'broadcast-stream)))
    (two-way-stream   . ,(lambda (v) (typep v 'two-way-stream)))
    (echo-stream      . ,(lambda (v) (typep v 'echo-stream)))
    (concatenated-stream . ,(lambda (v) (typep v 'concatenated-stream)))
    (synonym-stream   . ,(lambda (v) (typep v 'synonym-stream)))
    (hash-table       . ,(lambda (v) (typep v 'vm-hash-table-object)))
    (bit              . ,(lambda (v) (or (eql v 0) (eql v 1)))))
  "Alist mapping type-symbol to single-argument predicate function for vm-typep-check.")

;;; Ordered dispatch list for vm-type-of: first matching predicate wins.
(defparameter *vm-type-of-dispatch*
  `((,#'null                                . null)
    (,(lambda (v) (typep v 'fixnum))        . fixnum)
    (,(lambda (v) (typep v 'bignum))        . bignum)
    (,(lambda (v) (typep v 'ratio))         . ratio)
    (,(lambda (v) (typep v 'double-float))  . double-float)
    (,(lambda (v) (typep v 'long-float))    . long-float)
    (,(lambda (v) (or (typep v 'single-float)
                      (typep v 'short-float)
                      (floatp v)))          . single-float)
    (,(lambda (v) (typep v 'bit-vector))    . bit-vector)
    (,#'stringp                             . string)
    (,(lambda (v) (typep v 'simple-vector)) . simple-vector)
    (,(lambda (v) (typep v 'simple-array))  . simple-array)
    (,#'characterp                          . character)
    (,#'symbolp                             . symbol)
    (,#'pathnamep                           . pathname)
    (,(lambda (v) (typep v 'random-state))  . random-state)
    (,#'readtablep                          . readtable)
    (,#'packagep                            . package)
    (,#'consp                               . cons)
    (,#'complexp                            . complex)
    (,(lambda (v) (or (functionp v) (typep v 'vm-closure-object))) . function)
    (,#'vectorp                             . vector)
    (,#'arrayp                              . array))
  "Ordered dispatch list for vm-type-of: first matching predicate wins.")

(defun %vm-typep-normalize-sym (type-sym)
  "Normalize TYPE-SYM: unwrap HM type primitives and intern into CL package."
  (let* ((type-pkg (find-package "CL-CC/TYPE"))
         (primitive-sym (and type-pkg (find-symbol "TYPE-PRIMITIVE" type-pkg)))
         (name-sym      (and type-pkg (find-symbol "NAME" type-pkg)))
         (primitive-class (and primitive-sym (find-class primitive-sym nil))))
    (when (and primitive-class name-sym (typep type-sym primitive-class))
      (setf type-sym (slot-value type-sym name-sym))))
  (when (symbolp type-sym)
    (let ((cl-sym (find-symbol (symbol-name type-sym) :cl)))
      (when cl-sym (setf type-sym cl-sym))))
  type-sym)

(defun %vm-typep-call-predicate (value predicate)
  "Invoke PREDICATE on VALUE, supporting function/symbol/lambda forms."
  (cond
    ((functionp predicate)
     (ignore-errors (funcall predicate value)))
    ((and (symbolp predicate) (fboundp predicate))
     (ignore-errors (funcall (symbol-function predicate) value)))
    ((and (consp predicate) (eq (car predicate) 'lambda))
     (let ((fn (ignore-errors (eval predicate))))
       (and fn (ignore-errors (funcall fn value)))))
    (t nil)))

;;; Data table: maps compound type-specifier head symbols to handler functions.
;;; Each handler is (value type-sym) → boolean.
;;; Note: lambdas here call vm-typep-check by symbol — resolved at call time, not load time.
(defparameter *vm-compound-type-handlers*
  (let ((ht (make-hash-table :test #'eq)))
    (flet ((reg (sym fn) (setf (gethash sym ht) fn)))
      (reg 'refine    (lambda (v ts) (and (vm-typep-check v (second ts))
                                           (%vm-typep-call-predicate v (third ts)))))
      (reg 'satisfies (lambda (v ts) (ignore-errors (funcall (second ts) v))))
      (reg 'or        (lambda (v ts) (some  (lambda (t2) (vm-typep-check v t2)) (cdr ts))))
      (reg 'and       (lambda (v ts) (every (lambda (t2) (vm-typep-check v t2)) (cdr ts))))
      (reg 'not       (lambda (v ts) (not (vm-typep-check v (second ts)))))
      (reg 'member    (lambda (v ts) (member v (cdr ts) :test #'eql)))
      (reg 'eql       (lambda (v ts) (eql v (second ts))))
      (reg 'values    (lambda (v ts) (declare (ignore v ts)) t))
      (reg 'function  (lambda (v ts) (declare (ignore ts))
                         (or (typep v 'vm-closure-object) (functionp v)))))
    ht)
  "Hash table mapping compound type-specifier head symbols to handlers (value type-sym) → boolean.
Used by vm-typep-check for compound forms: (or ...), (and ...), (not ...), etc.")

(defun vm-typep-check (value type-sym)
  "Check if VALUE is of TYPE-SYM using table dispatch for primitive and compound types."
  (setf type-sym (%vm-typep-normalize-sym type-sym))
  (let ((pred (and (symbolp type-sym)
                   (cdr (assoc type-sym *vm-primitive-type-predicates*)))))
    (cond
      ;; Fast table lookup for all primitive types
      (pred
       (funcall pred value))
      ;; Compound type specifiers: dispatch via *vm-compound-type-handlers*
      ((consp type-sym)
       (let ((handler (gethash (car type-sym) *vm-compound-type-handlers*)))
         (if handler
             (funcall handler value type-sym)
             (ignore-errors (typep value type-sym)))))
      ;; Structural refinement type objects
      ((type-refinement-p type-sym)
       (and (vm-typep-check value (type-refinement-base type-sym))
            (%vm-typep-call-predicate value (type-refinement-predicate type-sym))))
      ;; VM CLOS instances — check class name / CPL
      ((and (hash-table-p value) (gethash :__class__ value))
       (let* ((class-ht   (gethash :__class__ value))
              (class-name (and (hash-table-p class-ht) (gethash :__name__ class-ht)))
              (cpl        (and (hash-table-p class-ht) (gethash :__cpl__ class-ht))))
         (or (eq class-name type-sym) (member type-sym cpl))))
      ;; Fallback to host typep
      (t (ignore-errors (typep value type-sym))))))

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

(defmethod execute-instruction ((inst vm-cl-div) state pc labels)
  (declare (ignore labels))
  (let ((divisor (vm-reg-get state (vm-rhs inst))))
    (if (zerop divisor)
        (error "Division by zero")
        (let ((result (/ (vm-reg-get state (vm-lhs inst)) divisor)))
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
  (let* ((value  (vm-reg-get state (vm-src inst)))
         (result (or (cdr (assoc-if (lambda (pred) (funcall pred value))
                                    *vm-type-of-dispatch*))
                     ;; VM CLOS instances — return class name
                     (and (hash-table-p value)
                          (gethash :__class__ value)
                          (let ((class-ht (gethash :__class__ value)))
                            (and (hash-table-p class-ht)
                                 (or (gethash :__name__ class-ht) 't))))
                     't)))
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

;;; FR-631: Macro expansion at runtime

(define-vm-unary-instruction vm-macroexpand-1-inst :macroexpand-1
  "Expand a macro form once. Returns the (possibly unexpanded) form.")

(defmethod execute-instruction ((inst vm-macroexpand-1-inst) state pc labels)
  (declare (ignore labels))
  (let* ((form (vm-reg-get state (vm-src inst)))
         (result (our-macroexpand-1 form)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

(define-vm-unary-instruction vm-macroexpand-inst :macroexpand
  "Fully expand a macro form. Returns the expanded form.")

(defmethod execute-instruction ((inst vm-macroexpand-inst) state pc labels)
  (declare (ignore labels))
  (let* ((form (vm-reg-get state (vm-src inst)))
         (result (our-macroexpand form)))
    (vm-reg-set state (vm-dst inst) result)
    (values (1+ pc) nil nil)))

;;; FR-498: Hash code computation

(define-vm-unary-instruction vm-sxhash :sxhash "Compute hash code for an object.")
(define-simple-instruction vm-sxhash :unary sxhash)

;;; vm-numeric.lisp — Numeric tower (round, bit ops, transcendentals, float, rational, complex)
;;; vm-extensions.lisp — Char comparisons, symbol plist, PROGV, generic arith
