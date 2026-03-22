(in-package :cl-cc)

;;; ----------------------------------------------------------------------------
;;; VM Heap Object Base Class
;;; ----------------------------------------------------------------------------

(defclass vm-heap-object ()
  ()
  (:documentation "Base class for all objects that can be stored on the VM heap.
Provides a common supertype for heap-allocated VM objects like cons cells,
closures, and reader states."))

;;; ----------------------------------------------------------------------------
;;; VM Heap Address Wrapper
;;; ----------------------------------------------------------------------------

(defclass vm-heap-address ()
  ((address :initarg :address :reader vm-heap-address-value
            :type integer :documentation "The heap address (index) of this object."))
  (:documentation "Wrapper for heap addresses to distinguish them from regular integers."))

;;; ----------------------------------------------------------------------------
;;; VM Closure Object
;;; ----------------------------------------------------------------------------

(defclass vm-closure-object (vm-heap-object)
  ((entry-label :initarg :entry-label :reader vm-closure-entry-label
                :documentation "Label where function code begins")
   (params :initarg :params :reader vm-closure-params
           :documentation "List of required parameter register names")
   (optional-params :initarg :optional-params :initform nil :reader vm-closure-optional-params
                    :documentation "List of (register default-value) for &optional")
   (rest-param :initarg :rest-param :initform nil :reader vm-closure-rest-param
               :documentation "Register name for &rest parameter, or nil")
   (key-params :initarg :key-params :initform nil :reader vm-closure-key-params
               :documentation "List of (keyword register default-value) for &key")
   (captured-values :initarg :captured-values :initform nil :reader vm-closure-captured-values
        :documentation "Captured lexical environment values as alist"))
  (:documentation "Represents a closure with code and captured environment."))

;;; ----------------------------------------------------------------------------
;;; VM Cons Cell (Heap-based)
;;; ----------------------------------------------------------------------------

(defclass vm-cons-cell (vm-heap-object)
  ((car :initarg :car :accessor vm-cons-cell-car
        :documentation "The car (first) element of the cons cell")
   (cdr :initarg :cdr :accessor vm-cons-cell-cdr
        :documentation "The cdr (second) element of the cons cell"))
  (:documentation "Heap-allocated cons cell for VM list operations."))

;;; ----------------------------------------------------------------------------
;;; VM Instructions
;;; ----------------------------------------------------------------------------

(defclass vm-instruction () ())

(defclass vm-const (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (value :initarg :value :reader vm-value)))

(defclass vm-move (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src :initarg :src :reader vm-src)))

(defclass vm-binop (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (lhs :initarg :lhs :reader vm-lhs)
   (rhs :initarg :rhs :reader vm-rhs)))

(defclass vm-add (vm-binop) ())
(defclass vm-sub (vm-binop) ())
(defclass vm-mul (vm-binop) ())

(defclass vm-label (vm-instruction)
  ((name :initarg :name :reader vm-name)))

(defclass vm-jump (vm-instruction)
  ((label :initarg :label :reader vm-label-name)))

(defclass vm-jump-zero (vm-instruction)
  ((reg :initarg :reg :reader vm-reg)
   (label :initarg :label :reader vm-label-name)))

(defclass vm-print (vm-instruction)
  ((reg :initarg :reg :reader vm-reg)))

(defclass vm-halt (vm-instruction)
  ((reg :initarg :reg :reader vm-reg)))

;;; Function support instructions
(defclass vm-closure (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (label :initarg :label :reader vm-label-name)
   (params :initarg :params :initform nil :reader vm-closure-params
    :documentation "List of required parameter register names (e.g., (:PARAM_0 :PARAM_1))")
   (optional-params :initarg :optional-params :initform nil :reader vm-closure-optional-params
    :documentation "List of (register default-value) for &optional params")
   (rest-param :initarg :rest-param :initform nil :reader vm-closure-rest-param
    :documentation "Register name for &rest parameter, or nil")
   (key-params :initarg :key-params :initform nil :reader vm-closure-key-params
    :documentation "List of (keyword register default-value) for &key params")
   (captured :initarg :captured :initform nil :reader vm-captured-vars
    :documentation "List of (symbol . register) pairs for captured variables")))

(defclass vm-make-closure (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (label :initarg :label :reader vm-label-name)
   (params :initarg :params :initform nil :reader vm-make-closure-params)
   (env-regs :initarg :env-regs :initform nil :reader vm-env-regs))
  (:documentation "Create a closure from LABEL with PARAMS, capturing ENV-REGS values on heap."))

(defclass vm-closure-ref-idx (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (closure :initarg :closure :reader vm-closure-reg)
   (index :initarg :index :reader vm-closure-index))
  (:documentation "Access captured value at INDEX from closure in CLOSURE register."))

(defclass vm-call (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (func :initarg :func :reader vm-func-reg)
   (args :initarg :args :reader vm-args)))

(defclass vm-ret (vm-instruction)
  ((reg :initarg :reg :reader vm-reg)))

(defclass vm-func-ref (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (label :initarg :label :reader vm-label-name)))

;;; Multiple values and apply instructions
(defclass vm-values (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (src-regs :initarg :src-regs :reader vm-src-regs))
  (:documentation "Multiple values. DST gets primary value, values-list stores all."))

(defclass vm-mv-bind (vm-instruction)
  ((dst-regs :initarg :dst-regs :reader vm-dst-regs))
  (:documentation "Bind multiple values from values-list to registers."))

(defclass vm-values-to-list (vm-instruction)
  ((dst :initarg :dst :reader vm-dst))
  (:documentation "Convert vm-values-list to a list in DST register."))

(defclass vm-apply (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (func :initarg :func :reader vm-func-reg)
   (args :initarg :args :reader vm-args))
  (:documentation "Apply function with spread arguments. Last arg is a list to spread."))

(defclass vm-register-function (vm-instruction)
  ((name :initarg :name :reader vm-func-name)
   (src :initarg :src :reader vm-src))
  (:documentation "Register a closure in the global function registry by name."))

(defclass vm-set-global (vm-instruction)
  ((name :initarg :name :reader vm-global-name)
   (src :initarg :src :reader vm-src))
  (:documentation "Store a value in the global variable store (persists across function calls)."))

(defclass vm-get-global (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (name :initarg :name :reader vm-global-name))
  (:documentation "Load a value from the global variable store into a register."))

(defclass vm-program ()
  ((instructions :initarg :instructions :reader vm-program-instructions)
   (result-register :initarg :result-register :reader vm-program-result-register)))

(defclass vm-state ()
  ((registers :initform (make-hash-table :test #'equal) :reader vm-state-registers)
   (output-stream :initarg :output-stream :reader vm-output-stream)
   (call-stack :initform nil :accessor vm-call-stack
               :documentation "Stack of (return-pc . saved-env) for function calls")
   (closure-env :initform nil :accessor vm-closure-env
                :documentation "Current closure's captured environment")
   (heap :initform (make-hash-table :test #'eql) :reader vm-state-heap
         :documentation "Heap storage for allocated objects keyed by address")
   (heap-counter :initform 0 :accessor vm-heap-counter
                 :documentation "Counter for allocating heap addresses")
   (class-registry :initform (make-hash-table :test #'eq) :reader vm-class-registry
                   :documentation "Global registry mapping class names to class descriptor HTs")
   (values-list :initform nil :accessor vm-values-list
                :documentation "List of multiple return values from last VALUES call")
   (handler-stack :initform nil :accessor vm-handler-stack
                  :documentation "Stack of active error handlers for handler-case.
Each entry is (handler-label result-reg error-type saved-call-stack saved-registers).")
   (function-registry :initform (make-hash-table :test #'eq) :reader vm-function-registry
                      :documentation "Global registry mapping function names (symbols) to closures.
Used for resolving (funcall 'name ...) and (apply 'name ...).")
   (global-vars :initform (make-hash-table :test #'eq) :reader vm-global-vars
                :documentation "Global variable store for defvar/defparameter.
Values here persist across function calls (not subject to register save/restore)."))
  (:documentation "VM execution state with registers, call stack, and heap."))

;;; ----------------------------------------------------------------------------
;;; VM Heap Operations
;;; ----------------------------------------------------------------------------

(defun vm-heap-alloc (state object)
  "Allocate OBJECT on the heap, return its address."
  (let ((addr (incf (vm-heap-counter state))))
    (setf (gethash addr (vm-state-heap state)) object)
    addr))

(defun vm-heap-get (state address)
  "Get object from heap at ADDRESS."
  (gethash address (vm-state-heap state)))

(defun vm-heap-set (state address object)
  "Set OBJECT at heap ADDRESS."
  (setf (gethash address (vm-state-heap state)) object))

(defun vm-heap-car (cons-cell)
  "Get the car of a vm-cons-cell."
  (vm-cons-cell-car cons-cell))

(defun vm-heap-cdr (cons-cell)
  "Get the cdr of a vm-cons-cell."
  (vm-cons-cell-cdr cons-cell))

(defun vm-build-list (state values)
  "Build a native CL list from VALUES for use as a &rest parameter.
Uses native cons cells (same as vm-cons instruction)."
  (declare (ignore state))
  (copy-list values))

(defun get-vm-heap (state)
  "Get the heap hash-table from VM state for direct access."
  (vm-state-heap state))

(defun vm-heap-address (object)
  "Get heap address from an object. For cons cells and closures."
  (etypecase object
    (integer object)
    (vm-heap-address (vm-heap-address-value object))
    (null nil)))

(defgeneric instruction->sexp (instruction))

(defmethod instruction->sexp ((inst vm-const))
  (list :const (vm-dst inst) (vm-value inst)))

(defmethod instruction->sexp ((inst vm-move))
  (list :move (vm-dst inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-add))
  (list :add (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))

(defmethod instruction->sexp ((inst vm-sub))
  (list :sub (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))

(defmethod instruction->sexp ((inst vm-mul))
  (list :mul (vm-dst inst) (vm-lhs inst) (vm-rhs inst)))

(defmethod instruction->sexp ((inst vm-label))
  (list :label (vm-name inst)))

(defmethod instruction->sexp ((inst vm-jump))
  (list :jump (vm-label-name inst)))

(defmethod instruction->sexp ((inst vm-jump-zero))
  (list :jump-zero (vm-reg inst) (vm-label-name inst)))

(defmethod instruction->sexp ((inst vm-print))
  (list :print (vm-reg inst)))

(defmethod instruction->sexp ((inst vm-halt))
  (list :halt (vm-reg inst)))

(defmethod instruction->sexp ((inst vm-closure))
  (list :closure (vm-dst inst) (vm-label-name inst) (vm-closure-params inst)
        :optional (vm-closure-optional-params inst)
        :rest (vm-closure-rest-param inst)
        :key (vm-closure-key-params inst)
        :captured (vm-captured-vars inst)))

(defmethod instruction->sexp ((inst vm-call))
  (list :call (vm-dst inst) (vm-func-reg inst) (vm-args inst)))

(defmethod instruction->sexp ((inst vm-ret))
  (list :ret (vm-reg inst)))

(defmethod instruction->sexp ((inst vm-func-ref))
  (list :func-ref (vm-dst inst) (vm-label-name inst)))

(defmethod instruction->sexp ((inst vm-make-closure))
  (list* :make-closure (vm-dst inst) (vm-label-name inst) (vm-make-closure-params inst) (vm-env-regs inst)))

(defmethod instruction->sexp ((inst vm-closure-ref-idx))
  (list :closure-ref-idx (vm-dst inst) (vm-closure-reg inst) (vm-closure-index inst)))

(defmethod instruction->sexp ((inst vm-values))
  (list :values (vm-dst inst) (vm-src-regs inst)))

(defmethod instruction->sexp ((inst vm-values-to-list))
  (list :values-to-list (vm-dst inst)))

(defmethod instruction->sexp ((inst vm-mv-bind))
  (list :mv-bind (vm-dst-regs inst)))

(defmethod instruction->sexp ((inst vm-apply))
  (list :apply (vm-dst inst) (vm-func-reg inst) (vm-args inst)))

(defmethod instruction->sexp ((inst vm-register-function))
  (list :register-function (vm-func-name inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-set-global))
  (list :set-global (vm-global-name inst) (vm-src inst)))

(defmethod instruction->sexp ((inst vm-get-global))
  (list :get-global (vm-dst inst) (vm-global-name inst)))

(defgeneric sexp->instruction (sexp))

(defmethod sexp->instruction ((sexp cons))
  (case (car sexp)
    ;; Core instructions (vm.lisp)
    (:const (make-instance 'vm-const :dst (second sexp) :value (third sexp)))
    (:move (make-instance 'vm-move :dst (second sexp) :src (third sexp)))
    (:add (make-instance 'vm-add :dst (second sexp) :lhs (third sexp) :rhs (fourth sexp)))
    (:sub (make-instance 'vm-sub :dst (second sexp) :lhs (third sexp) :rhs (fourth sexp)))
    (:mul (make-instance 'vm-mul :dst (second sexp) :lhs (third sexp) :rhs (fourth sexp)))
    (:label (make-instance 'vm-label :name (second sexp)))
    (:jump (make-instance 'vm-jump :label (second sexp)))
    (:jump-zero (make-instance 'vm-jump-zero :reg (second sexp) :label (third sexp)))
    (:print (make-instance 'vm-print :reg (second sexp)))
    (:halt (make-instance 'vm-halt :reg (second sexp)))
    (:closure (make-instance 'vm-closure :dst (second sexp) :label (third sexp) :params (fourth sexp) :captured (fifth sexp)))
    (:call (make-instance 'vm-call :dst (second sexp) :func (third sexp) :args (fourth sexp)))
    (:ret (make-instance 'vm-ret :reg (second sexp)))
    (:func-ref (make-instance 'vm-func-ref :dst (second sexp) :label (third sexp)))
    ;; New closure instructions
    (:make-closure (make-instance 'vm-make-closure
                     :dst (second sexp)
                     :label (third sexp)
                     :params (fourth sexp)
                     :env-regs (nthcdr 4 sexp)))
    (:closure-ref-idx (make-instance 'vm-closure-ref-idx
                        :dst (second sexp)
                        :closure (third sexp)
                        :index (fourth sexp)))
    ;; Type predicates (vm-primitives.lisp)
    (:eq (make-instance 'vm-eq :dst (second sexp) :lhs (third sexp) :rhs (fourth sexp)))
    (:cons-p (make-instance 'vm-cons-p :dst (second sexp) :src (third sexp)))
    (:null-p (make-instance 'vm-null-p :dst (second sexp) :src (third sexp)))
    (:symbol-p (make-instance 'vm-symbol-p :dst (second sexp) :src (third sexp)))
    (:number-p (make-instance 'vm-number-p :dst (second sexp) :src (third sexp)))
    (:integer-p (make-instance 'vm-integer-p :dst (second sexp) :src (third sexp)))
    (:function-p (make-instance 'vm-function-p :dst (second sexp) :src (third sexp)))
    ;; Comparison operations (vm-primitives.lisp)
    (:lt (make-instance 'vm-lt :dst (second sexp) :lhs (third sexp) :rhs (fourth sexp)))
    (:gt (make-instance 'vm-gt :dst (second sexp) :lhs (third sexp) :rhs (fourth sexp)))
    (:le (make-instance 'vm-le :dst (second sexp) :lhs (third sexp) :rhs (fourth sexp)))
    (:ge (make-instance 'vm-ge :dst (second sexp) :lhs (third sexp) :rhs (fourth sexp)))
    (:num-eq (make-instance 'vm-num-eq :dst (second sexp) :lhs (third sexp) :rhs (fourth sexp)))
    ;; Arithmetic extensions (vm-primitives.lisp)
    (:div (make-instance 'vm-div :dst (second sexp) :lhs (third sexp) :rhs (fourth sexp)))
    (:mod (make-instance 'vm-mod :dst (second sexp) :lhs (third sexp) :rhs (fourth sexp)))
    (:neg (make-instance 'vm-neg :dst (second sexp) :src (third sexp)))
    (:abs (make-instance 'vm-abs :dst (second sexp) :src (third sexp)))
    (:inc (make-instance 'vm-inc :dst (second sexp) :src (third sexp)))
    (:dec (make-instance 'vm-dec :dst (second sexp) :src (third sexp)))
    ;; Boolean operations (vm-primitives.lisp)
    (:not (make-instance 'vm-not :dst (second sexp) :src (third sexp)))
    (:and (make-instance 'vm-and :dst (second sexp) :lhs (third sexp) :rhs (fourth sexp)))
    (:or (make-instance 'vm-or :dst (second sexp) :lhs (third sexp) :rhs (fourth sexp)))
    ;; Condition system (vm-conditions.lisp)
    (:signal (make-instance 'vm-signal
                            :condition-reg (second sexp)))
    (:vm-error (make-instance 'vm-error-instruction
                              :condition-reg (second sexp)))
    (:cerror (make-instance 'vm-cerror
                            :continue-message (second sexp)
                            :condition-reg (third sexp)))
    (:warn (make-instance 'vm-warn
                          :condition-reg (second sexp)))
    (:push-handler (make-instance 'vm-push-handler
                                  :type (second sexp)
                                  :handler-label (third sexp)
                                  :result-reg (fourth sexp)))
    (:pop-handler (make-instance 'vm-pop-handler))
    (:bind-restart (make-instance 'vm-bind-restart
                                  :name (second sexp)
                                  :restart-label (third sexp)))
    (:invoke-restart (make-instance 'vm-invoke-restart
                                    :name (second sexp)
                                    :value-reg (third sexp)))
    ;; List operations (vm-list.lisp)
    (:cons (make-instance 'vm-cons
             :dst (second sexp)
             :car-src (third sexp)
             :cdr-src (fourth sexp)))
    (:car (make-instance 'vm-car
            :dst (second sexp)
            :src (third sexp)))
    (:cdr (make-instance 'vm-cdr
            :dst (second sexp)
            :src (third sexp)))
    (:list (make-instance 'vm-list
             :dst (second sexp)
             :count (third sexp)
             :src-regs (cdddr sexp)))
    (:length (make-instance 'vm-length
               :dst (second sexp)
               :src (third sexp)))
    (:reverse (make-instance 'vm-reverse
                :dst (second sexp)
                :src (third sexp)))
    (:append (make-instance 'vm-append
               :dst (second sexp)
               :src1 (third sexp)
               :src2 (fourth sexp)))
    (:member (make-instance 'vm-member
               :dst (second sexp)
               :item (third sexp)
               :list (fourth sexp)))
    (:nth (make-instance 'vm-nth
            :dst (second sexp)
            :index (third sexp)
            :list (fourth sexp)))
    (:nthcdr (make-instance 'vm-nthcdr
               :dst (second sexp)
               :index (third sexp)
               :list (fourth sexp)))
    (:first (make-instance 'vm-first
              :dst (second sexp)
              :src (third sexp)))
    (:second (make-instance 'vm-second
               :dst (second sexp)
               :src (third sexp)))
    (:third (make-instance 'vm-third
              :dst (second sexp)
              :src (third sexp)))
    (:fourth (make-instance 'vm-fourth
               :dst (second sexp)
               :src (third sexp)))
    (:fifth (make-instance 'vm-fifth
              :dst (second sexp)
              :src (third sexp)))
    (:rest (make-instance 'vm-rest
             :dst (second sexp)
             :src (third sexp)))
    (:last (make-instance 'vm-last
             :dst (second sexp)
             :src (third sexp)))
    (:butlast (make-instance 'vm-butlast
                :dst (second sexp)
                :src (third sexp)))
    (:rplaca (make-instance 'vm-rplaca
               :cons (second sexp)
               :val (third sexp)))
    (:rplacd (make-instance 'vm-rplacd
               :cons (second sexp)
               :val (third sexp)))
    (:list-length (make-instance 'vm-list-length
                    :dst (second sexp)
                    :src (third sexp)))
    (:endp (make-instance 'vm-endp
             :dst (second sexp)
             :src (third sexp)))
    (:null (make-instance 'vm-null
             :dst (second sexp)
             :src (third sexp)))
    (:push (make-instance 'vm-push
             :dst (second sexp)
             :item (third sexp)
             :list (fourth sexp)))
    (:pop (make-instance 'vm-pop
            :dst (second sexp)
            :list (third sexp)))
    ;; String operations (vm-strings.lisp)
    (:string= (make-instance 'vm-string= :dst (second sexp) :str1 (third sexp) :str2 (fourth sexp)))
    (:string< (make-instance 'vm-string< :dst (second sexp) :str1 (third sexp) :str2 (fourth sexp)))
    (:string> (make-instance 'vm-string> :dst (second sexp) :str1 (third sexp) :str2 (fourth sexp)))
    (:string<= (make-instance 'vm-string<= :dst (second sexp) :str1 (third sexp) :str2 (fourth sexp)))
    (:string>= (make-instance 'vm-string>= :dst (second sexp) :str1 (third sexp) :str2 (fourth sexp)))
    (:string-equal (make-instance 'vm-string-equal :dst (second sexp) :str1 (third sexp) :str2 (fourth sexp)))
    (:string-lessp (make-instance 'vm-string-lessp :dst (second sexp) :str1 (third sexp) :str2 (fourth sexp)))
    (:string-greaterp (make-instance 'vm-string-greaterp :dst (second sexp) :str1 (third sexp) :str2 (fourth sexp)))
    (:string-not-equal (make-instance 'vm-string-not-equal :dst (second sexp) :str1 (third sexp) :str2 (fourth sexp)))
    (:string-length (make-instance 'vm-string-length :dst (second sexp) :src (third sexp)))
    (:char (make-instance 'vm-char :dst (second sexp) :string (third sexp) :index (fourth sexp)))
    (:char-code (make-instance 'vm-char-code :dst (second sexp) :src (third sexp)))
    (:code-char (make-instance 'vm-code-char :dst (second sexp) :src (third sexp)))
    (:char= (make-instance 'vm-char= :dst (second sexp) :char1 (third sexp) :char2 (fourth sexp)))
    (:char< (make-instance 'vm-char< :dst (second sexp) :char1 (third sexp) :char2 (fourth sexp)))
    (:subseq (make-instance 'vm-subseq :dst (second sexp) :string (third sexp) :start (fourth sexp) :end (fifth sexp)))
    (:concatenate (make-instance 'vm-concatenate :dst (second sexp) :str1 (third sexp) :str2 (fourth sexp)))
    (:string-upcase (make-instance 'vm-string-upcase :dst (second sexp) :src (third sexp)))
    (:string-downcase (make-instance 'vm-string-downcase :dst (second sexp) :src (third sexp)))
    (:string-capitalize (make-instance 'vm-string-capitalize :dst (second sexp) :src (third sexp)))
    (:string-trim (make-instance 'vm-string-trim :dst (second sexp) :char-bag (third sexp) :string (fourth sexp)))
    (:string-left-trim (make-instance 'vm-string-left-trim :dst (second sexp) :char-bag (third sexp) :string (fourth sexp)))
    (:string-right-trim (make-instance 'vm-string-right-trim :dst (second sexp) :char-bag (third sexp) :string (fourth sexp)))
    (:search-string (make-instance 'vm-search-string :dst (second sexp) :pattern (third sexp) :string (fourth sexp) :start (fifth sexp)))
    ;; Hash table operations (vm-hash.lisp)
    (:make-hash-table
     (if (third sexp)
         (make-instance 'vm-make-hash-table
                        :dst (second sexp)
                        :test (third sexp))
         (make-instance 'vm-make-hash-table
                        :dst (second sexp))))
    (:gethash
     (make-instance 'vm-gethash
                    :dst (second sexp)
                    :key (third sexp)
                    :table (fourth sexp)
                    :found-dst (getf (cdddr sexp) :found-dst)
                    :default (getf (cdddr sexp) :default)))
    (:sethash
     (make-instance 'vm-sethash
                    :key (second sexp)
                    :value (third sexp)
                    :table (fourth sexp)))
    (:remhash
     (make-instance 'vm-remhash
                    :key (second sexp)
                    :table (third sexp)))
    (:clrhash
     (make-instance 'vm-clrhash
                    :table (second sexp)))
    (:hash-table-count
     (make-instance 'vm-hash-table-count
                    :dst (second sexp)
                    :table (third sexp)))
    (:hash-table-p
     (make-instance 'vm-hash-table-p
                    :dst (second sexp)
                    :src (third sexp)))
    (:maphash
     (make-instance 'vm-maphash
                    :fn (second sexp)
                    :table (third sexp)))
    (:hash-table-keys
     (make-instance 'vm-hash-table-keys
                    :dst (second sexp)
                    :table (third sexp)))
    (:hash-table-values
     (make-instance 'vm-hash-table-values
                    :dst (second sexp)
                    :table (third sexp)))
    ;; CLOS instructions
    (:class-def (make-instance 'vm-class-def
                  :dst (second sexp)
                  :class-name (third sexp)
                  :superclasses (fourth sexp)
                  :slot-names (fifth sexp)
                  :slot-initargs (sixth sexp)
                  :slot-initform-regs (seventh sexp)))
    (:make-obj (make-instance 'vm-make-obj
                 :dst (second sexp)
                 :class-reg (third sexp)
                 :initarg-regs (fourth sexp)))
    (:slot-read (make-instance 'vm-slot-read
                  :dst (second sexp)
                  :obj-reg (third sexp)
                  :slot-name (fourth sexp)))
    (:slot-write (make-instance 'vm-slot-write
                   :obj-reg (second sexp)
                   :slot-name (third sexp)
                   :value-reg (fourth sexp)))
    (:register-method (make-instance 'vm-register-method
                        :gf-reg (second sexp)
                        :specializer (third sexp)
                        :method-reg (fourth sexp)))
    (:generic-call (make-instance 'vm-generic-call
                     :dst (second sexp)
                     :gf-reg (third sexp)
                     :args (fourth sexp)))
    ;; Handler-case instructions
    (:establish-handler (make-instance 'vm-establish-handler
                          :handler-label (second sexp)
                          :result-reg (third sexp)
                          :error-type (fourth sexp)))
    (:remove-handler (make-instance 'vm-remove-handler))
    (:signal-error (make-instance 'vm-signal-error
                     :error-reg (second sexp)))
    ;; Multiple values and apply
    (:values (make-instance 'vm-values
               :dst (second sexp)
               :src-regs (third sexp)))
    (:mv-bind (make-instance 'vm-mv-bind
                :dst-regs (second sexp)))
    (:values-to-list (make-instance 'vm-values-to-list
                       :dst (second sexp)))
    (:apply (make-instance 'vm-apply
              :dst (second sexp)
              :func (third sexp)
              :args (fourth sexp)))
    (:register-function (make-instance 'vm-register-function
                          :name (second sexp)
                          :src (third sexp)))
    (:set-global (make-instance 'vm-set-global
                   :name (second sexp)
                   :src (third sexp)))
    (:get-global (make-instance 'vm-get-global
                   :dst (second sexp)
                   :name (third sexp)))
    (otherwise (error "Unknown instruction sexp: ~S" sexp))))

(defgeneric execute-instruction (instruction state pc labels))

(defun vm-reg-get (state reg)
  (gethash reg (vm-state-registers state) 0))

(defun vm-reg-set (state reg value)
  (setf (gethash reg (vm-state-registers state)) value)
  value)

(defun vm-generic-function-p (value)
  "Return T if VALUE is a generic function dispatch table (hash table with :__methods__)."
  (and (hash-table-p value)
       (gethash :__methods__ value)
       t))

(defun vm-resolve-function (state value)
  "Resolve VALUE to a closure or generic function dispatch table.
If VALUE is already a closure, return it.
If VALUE is a hash table with :__methods__, return it (generic function).
If VALUE is a symbol, look it up in the function registry."
  (cond
    ((typep value 'vm-closure-object) value)
    ((vm-generic-function-p value) value)
    ((symbolp value)
     (let ((entry (gethash value (vm-function-registry state))))
       (if entry
           entry
           (error "Undefined function: ~S" value))))
    (t (error "Invalid function designator: ~S" value))))

(defun vm-closure-ref (state var-name)
  "Look up a variable in the current closure's captured environment."
  (let ((closure-env (vm-closure-env state)))
    (let ((entry (assoc var-name closure-env)))
      (if entry
          (cdr entry)
          (error "Unbound closure variable: ~S" var-name)))))

(defmethod execute-instruction ((inst vm-const) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (vm-value inst))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-move) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (vm-reg-get state (vm-src inst)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-add) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state
              (vm-dst inst)
              (+ (vm-reg-get state (vm-lhs inst))
                 (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-sub) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state
              (vm-dst inst)
              (- (vm-reg-get state (vm-lhs inst))
                 (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-mul) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state
              (vm-dst inst)
              (* (vm-reg-get state (vm-lhs inst))
                 (vm-reg-get state (vm-rhs inst))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-label) state pc labels)
  (declare (ignore state labels))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-jump) state pc labels)
  (declare (ignore state pc))
  (values (gethash (vm-label-name inst) labels) nil nil))

(defun vm-falsep (value)
  "Return T if VALUE is falsy (nil, 0, or false)."
  (or (null value) (eql value 0)))

(defmethod execute-instruction ((inst vm-jump-zero) state pc labels)
  (if (vm-falsep (vm-reg-get state (vm-reg inst)))
      (values (gethash (vm-label-name inst) labels) nil nil)
      (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-print) state pc labels)
  (declare (ignore labels))
  (format (vm-output-stream state) "~A~%" (vm-reg-get state (vm-reg inst)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-halt) state pc labels)
  (declare (ignore pc labels))
  (values nil t (vm-reg-get state (vm-reg inst))))

(defmethod execute-instruction ((inst vm-closure) state pc labels)
  (declare (ignore labels))
  ;; captured is a list of (symbol . register) pairs from the compiler
  ;; We store values keyed by REGISTER name so vm-call restores to the correct register
  (let* ((dst-reg (vm-dst inst))
         (captured-vals (mapcar (lambda (binding)
                                  (cons (cdr binding) (vm-reg-get state (cdr binding))))
                                (vm-captured-vars inst)))
         (closure (make-instance 'vm-closure-object
                                 :entry-label (vm-label-name inst)
                                 :params (vm-closure-params inst)
                                 :optional-params (vm-closure-optional-params inst)
                                 :rest-param (vm-closure-rest-param inst)
                                 :key-params (vm-closure-key-params inst)
                                 :captured-values captured-vals)))
    (vm-reg-set state dst-reg closure)
    ;; Fix self-references for recursive labels: if a captured register
    ;; is the same as dst-reg, the closure captures itself (not yet created at lookup time)
    (dolist (cv-pair captured-vals)
      (when (eql (car cv-pair) dst-reg)
        (setf (cdr cv-pair) closure)))
    (values (1+ pc) nil nil)))

(defun vm-dispatch-generic-call (gf-ht state pc arg-regs dst-reg labels)
  "Dispatch a generic function call. GF-HT is the generic function dispatch table.
Supports multiple dispatch by passing all argument values for composite key lookup.
Returns (values next-pc halt-p result) like execute-instruction."
  (let* ((all-arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs))
         (first-arg (car all-arg-values))
         (method-closure (vm-resolve-gf-method gf-ht state first-arg all-arg-values))
         (entry-label (vm-closure-entry-label method-closure))
         (params (vm-closure-params method-closure))
         (captured (vm-closure-captured-values method-closure))
         (return-pc (1+ pc))
         (old-closure-env (vm-closure-env state))
         (saved-regs (let ((copy (make-hash-table :test (hash-table-test (vm-state-registers state)))))
                       (maphash (lambda (k v) (setf (gethash k copy) v))
                                (vm-state-registers state))
                       copy))
         (arg-values (mapcar (lambda (arg-reg) (vm-reg-get state arg-reg)) arg-regs)))
    (push (list return-pc dst-reg old-closure-env saved-regs) (vm-call-stack state))
    (loop for var-binding in captured
          do (vm-reg-set state (car var-binding) (cdr var-binding)))
    (loop for param in params
          for arg-val in arg-values
          do (vm-reg-set state param arg-val))
    (when captured
      (setf (vm-closure-env state) captured))
    (values (gethash entry-label labels) nil nil)))

(defmethod execute-instruction ((inst vm-call) state pc labels)
  (let* ((raw-func (vm-reg-get state (vm-func-reg inst)))
         (func (vm-resolve-function state raw-func))
         (arg-regs (vm-args inst))
         (dst-reg (vm-dst inst)))
    ;; If it's a generic function dispatch table, route to generic dispatch
    (when (vm-generic-function-p func)
      (return-from execute-instruction
        (vm-dispatch-generic-call func state pc arg-regs dst-reg labels)))
    ;; Normal closure call
    (let* ((closure func)
           (entry-label (vm-closure-entry-label closure))
           (params (vm-closure-params closure))
           (captured (vm-closure-captured-values closure))
           (return-pc (1+ pc))
           (old-closure-env (vm-closure-env state))
           ;; Save caller's register state for restoration on return
           (saved-regs (let ((copy (make-hash-table :test (hash-table-test (vm-state-registers state)))))
                         (maphash (lambda (k v) (setf (gethash k copy) v))
                                  (vm-state-registers state))
                         copy))
           ;; Read argument values BEFORE overwriting registers
           (arg-values (mapcar (lambda (arg-reg) (vm-reg-get state arg-reg)) arg-regs)))
      ;; Push call frame with saved registers
      (push (list return-pc dst-reg old-closure-env saved-regs) (vm-call-stack state))
      ;; Restore captured variable values into registers
      (loop for var-binding in captured
           do (vm-reg-set state (car var-binding) (cdr var-binding)))
      ;; Bind required arguments to parameters using pre-saved values
      (loop for param in params
            for arg-val in arg-values
            do (vm-reg-set state param arg-val))
      ;; Handle &optional parameters
      (let ((opt-params (vm-closure-optional-params closure))
            (remaining-args (nthcdr (length params) arg-values)))
        (when opt-params
          (loop for (opt-reg default-val) in opt-params
                for i from 0
                do (vm-reg-set state opt-reg
                               (if (< i (length remaining-args))
                                   (nth i remaining-args)
                                   default-val)))
          (setf remaining-args (nthcdr (length opt-params) remaining-args)))
        ;; Handle &rest parameter
        (let ((rest-param (vm-closure-rest-param closure)))
          (when rest-param
            (let ((rest-start (+ (length params) (length opt-params))))
              (vm-reg-set state rest-param
                          (vm-build-list state (nthcdr rest-start arg-values))))))
        ;; Handle &key parameters
        (let ((key-params (vm-closure-key-params closure)))
          (when key-params
            (let ((key-args (nthcdr (+ (length params) (length opt-params)) arg-values)))
              (loop for (keyword key-reg default-val) in key-params
                    do (let ((pos (position keyword key-args)))
                         (vm-reg-set state key-reg
                                     (if pos
                                         (nth (1+ pos) key-args)
                                         default-val))))))))
      ;; Set up closure's captured environment for nested closures
      (when captured
        (setf (vm-closure-env state) captured))
      ;; Jump to function entry
      (values (gethash entry-label labels) nil nil))))

(defmethod execute-instruction ((inst vm-ret) state pc labels)
  (declare (ignore pc labels))
  (let ((result (vm-reg-get state (vm-reg inst))))
    (if (vm-call-stack state)
        (destructuring-bind (return-pc dst-reg old-closure-env saved-regs)
            (pop (vm-call-stack state))
          ;; Restore caller's register state
          (when saved-regs
            (clrhash (vm-state-registers state))
            (maphash (lambda (k v) (setf (gethash k (vm-state-registers state)) v))
                     saved-regs))
          ;; Write the return value into the destination register
          (vm-reg-set state dst-reg result)
          (when old-closure-env
            (setf (vm-closure-env state) old-closure-env))
          (values return-pc nil nil))
        (values nil t result))))

(defmethod execute-instruction ((inst vm-func-ref) state pc labels)
  (declare (ignore pc labels))
  (vm-reg-set state (vm-dst inst)
              (make-instance 'vm-closure-object
                             :entry-label (vm-label-name inst)
                             :params nil
                             :captured-values nil))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-make-closure) state pc labels)
  (declare (ignore labels))
  (let* ((captured-values (mapcar (lambda (reg) (vm-reg-get state reg)) (vm-env-regs inst)))
         (closure (make-instance 'vm-closure-object
                                 :entry-label (vm-label-name inst)
                                 :params (vm-make-closure-params inst)
                                 :captured-values captured-values))
         (addr (vm-heap-alloc state closure)))
    (vm-reg-set state (vm-dst inst) addr)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-closure-ref-idx) state pc labels)
  (declare (ignore labels))
  (let* ((addr (vm-reg-get state (vm-closure-reg inst)))
         (closure (vm-heap-get state addr))
         (idx (vm-closure-index inst))
         (values-list (vm-closure-captured-values closure)))
    (when (>= idx (length values-list))
      (error "Closure ref index ~D out of bounds (captured ~D values)" idx (length values-list)))
    (vm-reg-set state (vm-dst inst) (nth idx values-list))
    (values (1+ pc) nil nil)))

;;; ----------------------------------------------------------------------------
;;; Multiple Values and Apply Execution
;;; ----------------------------------------------------------------------------

(defmethod execute-instruction ((inst vm-values) state pc labels)
  (declare (ignore labels))
  (let* ((src-regs (vm-src-regs inst))
         (all-values (mapcar (lambda (reg) (vm-reg-get state reg)) src-regs)))
    ;; Store all values in state
    (setf (vm-values-list state) all-values)
    ;; Primary value goes to dst
    (vm-reg-set state (vm-dst inst) (if all-values (first all-values) nil))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-mv-bind) state pc labels)
  (declare (ignore labels))
  (let ((vals (vm-values-list state))
        (dst-regs (vm-dst-regs inst)))
    (loop for reg in dst-regs
          for i from 0
          do (vm-reg-set state reg (if (< i (length vals))
                                       (nth i vals)
                                       nil)))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-values-to-list) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (copy-list (vm-values-list state)))
  (values (1+ pc) nil nil))

(defun vm-list-to-lisp-list (state value)
  "Convert a VM list (possibly using vm-cons-cell heap objects) to a Lisp list."
  (cond
    ((null value) nil)
    ((consp value) value)
    ((typep value 'vm-cons-cell)
     (cons (vm-cons-cell-car value)
           (vm-list-to-lisp-list state (vm-cons-cell-cdr value))))
    ((integerp value)
     ;; Could be a heap address
     (let ((obj (vm-heap-get state value)))
       (if (typep obj 'vm-cons-cell)
           (cons (vm-cons-cell-car obj)
                 (vm-list-to-lisp-list state (vm-cons-cell-cdr obj)))
           (list value))))
    (t (list value))))

(defun vm-classify-arg (arg state)
  "Determine the class name of an argument for generic dispatch."
  (if (hash-table-p arg)
      (let ((class-ht (gethash :__class__ arg)))
        (if class-ht
            (gethash :__name__ class-ht)
            t))
      (typecase arg
        (integer 'integer)
        (string 'string)
        (symbol 'symbol)
        (t t))))

(defun vm-resolve-gf-method (gf-ht state first-arg &optional all-args)
  "Resolve the applicable method closure for generic function GF-HT.
Supports multiple dispatch: if ALL-ARGS is provided, builds a composite key
from all argument classes. Falls back to single dispatch on FIRST-ARG."
  (let* ((methods-ht (gethash :__methods__ gf-ht))
         ;; Check if methods table uses composite keys (lists) or single keys (symbols)
         (uses-composite-keys (block check
                                (maphash (lambda (k v)
                                           (declare (ignore v))
                                           (when (listp k)
                                             (return-from check t)))
                                         methods-ht)
                                nil)))
    (if (and uses-composite-keys all-args)
        ;; Multiple dispatch: build composite key from all args
        (let* ((arg-classes (mapcar (lambda (arg) (vm-classify-arg arg state))
                                   all-args))
               (method-closure
                 (or
                   ;; Exact match
                   (gethash arg-classes methods-ht)
                   ;; Try with inheritance on each position
                   (vm-resolve-multi-dispatch methods-ht state arg-classes)
                   ;; All-t fallback
                   (gethash (make-list (length arg-classes) :initial-element t) methods-ht))))
          (unless method-closure
            (error "No applicable method for generic function ~S on classes ~S"
                   (gethash :__name__ gf-ht) arg-classes))
          method-closure)
        ;; Single dispatch (backward compatible)
        (let* ((class-name (vm-classify-arg first-arg state))
               (method-closure
                 (or
                   (gethash class-name methods-ht)
                   (let ((class-ht (gethash class-name (vm-class-registry state))))
                     (when class-ht
                       (let ((cpl (gethash :__cpl__ class-ht)))
                         (loop for ancestor in (cdr cpl)
                               for m = (gethash ancestor methods-ht)
                               when m return m))))
                   (gethash t methods-ht))))
          (unless method-closure
            (error "No applicable method for generic function ~S on class ~S"
                   (gethash :__name__ gf-ht) class-name))
          method-closure))))

(defun vm-resolve-multi-dispatch (methods-ht state arg-classes)
  "Try to find a method by substituting ancestor classes in each position.
Uses class precedence lists for inheritance-based fallback."
  ;; Build CPLs for each argument position, always ending with T
  (let ((cpls (mapcar (lambda (class-name)
                        (let* ((class-ht (gethash class-name (vm-class-registry state)))
                               (cpl (if class-ht
                                        (gethash :__cpl__ class-ht)
                                        (list class-name))))
                          ;; Ensure T is always at the end of the CPL
                          (if (member t cpl)
                              cpl
                              (append cpl (list t)))))
                      arg-classes)))
    ;; Try all combinations, preferring earlier positions
    (vm-try-dispatch-combinations methods-ht cpls (length arg-classes))))

(defun vm-try-dispatch-combinations (methods-ht cpls n)
  "Try dispatch key combinations from CPLs, most-specific first."
  (when (= n 0)
    (return-from vm-try-dispatch-combinations (gethash nil methods-ht)))
  ;; Simple strategy: try substituting each position one at a time
  (let ((first-cpl (car cpls))
        (rest-cpls (cdr cpls)))
    (dolist (class first-cpl)
      (if (= n 1)
          (let ((m (gethash (list class) methods-ht)))
            (when m (return-from vm-try-dispatch-combinations m)))
          ;; Recursively try remaining positions
          (let ((sub-result (vm-try-dispatch-sub methods-ht rest-cpls (list class))))
            (when sub-result
              (return-from vm-try-dispatch-combinations sub-result))))))
  nil)

(defun vm-try-dispatch-sub (methods-ht cpls prefix)
  "Recursive helper for multi-dispatch combination search."
  (if (null cpls)
      (gethash prefix methods-ht)
      (let ((first-cpl (car cpls))
            (rest-cpls (cdr cpls)))
        (dolist (class first-cpl)
          (let ((result (vm-try-dispatch-sub methods-ht rest-cpls (append prefix (list class)))))
            (when result
              (return-from vm-try-dispatch-sub result))))
        nil)))

(defmethod execute-instruction ((inst vm-apply) state pc labels)
  (let* ((raw-func (vm-reg-get state (vm-func-reg inst)))
         (func (vm-resolve-function state raw-func))
         (arg-regs (vm-args inst))
         (dst-reg (vm-dst inst)))
    ;; Build argument list: all args except last are normal, last is a list to spread
    (let* ((arg-values (mapcar (lambda (reg) (vm-reg-get state reg)) arg-regs))
           (spread-args (if arg-values
                            (let ((normal (butlast arg-values))
                                  (last-arg (car (last arg-values))))
                              (append normal (vm-list-to-lisp-list state last-arg)))
                            nil))
           ;; If generic function, resolve to the applicable method
           (closure (if (vm-generic-function-p func)
                        (vm-resolve-gf-method func state (car spread-args) spread-args)
                        func))
           (entry-label (vm-closure-entry-label closure))
           (params (vm-closure-params closure))
           (captured (vm-closure-captured-values closure))
           (return-pc (1+ pc))
           (old-closure-env (vm-closure-env state))
           (saved-regs (let ((copy (make-hash-table :test (hash-table-test (vm-state-registers state)))))
                         (maphash (lambda (k v) (setf (gethash k copy) v))
                                  (vm-state-registers state))
                         copy)))
      ;; Push call frame
      (push (list return-pc dst-reg old-closure-env saved-regs) (vm-call-stack state))
      ;; Restore captured
      (loop for var-binding in captured
            do (vm-reg-set state (car var-binding) (cdr var-binding)))
      ;; Bind spread arguments to parameters
      (loop for param in params
            for arg-val in spread-args
            do (vm-reg-set state param arg-val))
      ;; Set up closure environment
      (when captured
        (setf (vm-closure-env state) captured))
      ;; Jump to function entry
      (values (gethash entry-label labels) nil nil))))

;;; ----------------------------------------------------------------------------
;;; VM Environment Reference Instruction
;;; ----------------------------------------------------------------------------

(defclass vm-env-ref (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (var-name :initarg :var-name :reader vm-var-name))
  (:documentation "Access variable from closure's captured environment."))

(defmethod instruction->sexp ((inst vm-env-ref))
  (list :env-ref (vm-dst inst) (vm-var-name inst)))

(defmethod execute-instruction ((inst vm-env-ref) state pc labels)
  (declare (ignore labels))
  (let* ((var-name (vm-var-name inst))
         (value (vm-closure-ref state var-name)))
    (vm-reg-set state (vm-dst inst) value)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-register-function) state pc labels)
  (declare (ignore labels))
  (let ((name (vm-func-name inst))
        (closure (vm-reg-get state (vm-src inst))))
    (setf (gethash name (vm-function-registry state)) closure)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-set-global) state pc labels)
  (declare (ignore labels))
  (let ((name (vm-global-name inst))
        (value (vm-reg-get state (vm-src inst))))
    (setf (gethash name (vm-global-vars state)) value)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-get-global) state pc labels)
  (declare (ignore labels))
  (let ((name (vm-global-name inst)))
    (multiple-value-bind (value found-p) (gethash name (vm-global-vars state))
      (unless found-p
        (error "Unbound global variable: ~S" name))
      (vm-reg-set state (vm-dst inst) value)
      (values (1+ pc) nil nil))))

;;; ----------------------------------------------------------------------------
;;; CLOS VM Instructions
;;; ----------------------------------------------------------------------------

(defclass vm-class-def (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (class-name :initarg :class-name :reader vm-class-name-sym)
   (superclasses :initarg :superclasses :initform nil :reader vm-superclasses)
   (slot-names :initarg :slot-names :reader vm-slot-names)
   (slot-initargs :initarg :slot-initargs :initform nil :reader vm-slot-initargs)
   (slot-initform-regs :initarg :slot-initform-regs :initform nil :reader vm-slot-initform-regs
                       :documentation "Alist of (slot-name . register) for slots with :initform values"))
  (:documentation "Define a class. Creates a class descriptor hash table."))

(defclass vm-make-obj (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (class-reg :initarg :class-reg :reader vm-class-reg)
   (initarg-regs :initarg :initarg-regs :initform nil :reader vm-initarg-regs))
  (:documentation "Create an instance of a class. INITARG-REGS is an alist of (initarg-keyword . register)."))

(defclass vm-slot-read (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (obj-reg :initarg :obj-reg :reader vm-obj-reg)
   (slot-name :initarg :slot-name :reader vm-slot-name-sym))
  (:documentation "Read a slot value from an object."))

(defclass vm-slot-write (vm-instruction)
  ((obj-reg :initarg :obj-reg :reader vm-obj-reg)
   (slot-name :initarg :slot-name :reader vm-slot-name-sym)
   (value-reg :initarg :value-reg :reader vm-value-reg))
  (:documentation "Write a value to an object slot."))

(defclass vm-register-method (vm-instruction)
  ((gf-reg :initarg :gf-reg :reader vm-gf-reg)
   (specializer :initarg :specializer :reader vm-method-specializer)
   (method-reg :initarg :method-reg :reader vm-method-reg))
  (:documentation "Register a method closure on a generic function dispatch table."))

(defclass vm-generic-call (vm-instruction)
  ((dst :initarg :dst :reader vm-dst)
   (gf-reg :initarg :gf-reg :reader vm-gf-reg)
   (args :initarg :args :reader vm-args))
  (:documentation "Dispatch and call a generic function method based on first argument's class."))

;;; CLOS instruction serialization

(defmethod instruction->sexp ((inst vm-class-def))
  (list :class-def (vm-dst inst) (vm-class-name-sym inst)
        (vm-superclasses inst) (vm-slot-names inst) (vm-slot-initargs inst)
        (vm-slot-initform-regs inst)))

(defmethod instruction->sexp ((inst vm-make-obj))
  (list :make-obj (vm-dst inst) (vm-class-reg inst) (vm-initarg-regs inst)))

(defmethod instruction->sexp ((inst vm-slot-read))
  (list :slot-read (vm-dst inst) (vm-obj-reg inst) (vm-slot-name-sym inst)))

(defmethod instruction->sexp ((inst vm-slot-write))
  (list :slot-write (vm-obj-reg inst) (vm-slot-name-sym inst) (vm-value-reg inst)))

(defmethod instruction->sexp ((inst vm-register-method))
  (list :register-method (vm-gf-reg inst) (vm-method-specializer inst) (vm-method-reg inst)))

(defmethod instruction->sexp ((inst vm-generic-call))
  (list :generic-call (vm-dst inst) (vm-gf-reg inst) (vm-args inst)))

;;; CLOS instruction execution

(defun collect-inherited-slots (superclasses registry)
  "Collect slot names from superclasses in MRO order (depth-first, left-to-right).
   Returns a list of slot names without duplicates, preserving order."
  (let ((seen (make-hash-table :test #'eq))
        (result nil))
    (labels ((walk (class-names)
               (dolist (cname class-names)
                 (let ((super-ht (gethash cname registry)))
                   (when super-ht
                     ;; Recurse into super's supers first
                     (walk (gethash :__superclasses__ super-ht))
                     ;; Add this super's slots
                     (dolist (slot (gethash :__slots__ super-ht))
                       (unless (gethash slot seen)
                         (setf (gethash slot seen) t)
                         (push slot result))))))))
      (walk superclasses))
    (nreverse result)))

(defun collect-inherited-initargs (superclasses registry)
  "Collect initarg mappings from superclasses. Later entries override earlier ones."
  (let ((result nil))
    (labels ((walk (class-names)
               (dolist (cname class-names)
                 (let ((super-ht (gethash cname registry)))
                   (when super-ht
                     (walk (gethash :__superclasses__ super-ht))
                     (dolist (entry (gethash :__initargs__ super-ht))
                       (unless (assoc (car entry) result)
                         (push entry result))))))))
      (walk superclasses))
    (nreverse result)))

(defun compute-class-precedence-list (class-name registry)
  "Compute the class precedence list for CLASS-NAME (depth-first, left-to-right).
   Returns a list of class names including CLASS-NAME itself."
  (let ((seen (make-hash-table :test #'eq))
        (result nil))
    (labels ((walk (name)
               (unless (gethash name seen)
                 (setf (gethash name seen) t)
                 (push name result)
                 (let ((class-ht (gethash name registry)))
                   (when class-ht
                     (dolist (super (gethash :__superclasses__ class-ht))
                       (walk super)))))))
      (walk class-name))
    (nreverse result)))

(defmethod execute-instruction ((inst vm-class-def) state pc labels)
  (declare (ignore labels))
  (let* ((class-ht (make-hash-table :test #'eq))
         (registry (vm-class-registry state))
         (supers (vm-superclasses inst))
         ;; Merge inherited slots with own slots
         (inherited-slots (collect-inherited-slots supers registry))
         (own-slots (vm-slot-names inst))
         (all-slots (append inherited-slots
                           (remove-if (lambda (s) (member s inherited-slots)) own-slots)))
         ;; Merge inherited initargs with own
         (inherited-initargs (collect-inherited-initargs supers registry))
         (own-initargs (vm-slot-initargs inst))
         (all-initargs (append inherited-initargs
                               (remove-if (lambda (e) (assoc (car e) inherited-initargs))
                                          own-initargs)))
         ;; Build initform values: read from registers at class definition time
         (initform-regs (vm-slot-initform-regs inst))
         (initform-values (loop for (slot-name . reg) in initform-regs
                                collect (cons slot-name (vm-reg-get state reg)))))
    (setf (gethash :__name__ class-ht) (vm-class-name-sym inst))
    (setf (gethash :__superclasses__ class-ht) supers)
    (setf (gethash :__slots__ class-ht) all-slots)
    (setf (gethash :__initargs__ class-ht) all-initargs)
    (setf (gethash :__methods__ class-ht) (make-hash-table :test #'equal))
    (setf (gethash :__initforms__ class-ht) initform-values)
    ;; Register in global class registry BEFORE computing CPL,
    ;; so the walk can find this class's superclasses entry
    (setf (gethash (vm-class-name-sym inst) registry) class-ht)
    (setf (gethash :__cpl__ class-ht)
          (compute-class-precedence-list (vm-class-name-sym inst) registry))
    (vm-reg-set state (vm-dst inst) class-ht)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-make-obj) state pc labels)
  (declare (ignore labels))
  (let* ((class-ht (vm-reg-get state (vm-class-reg inst)))
         (obj-ht (make-hash-table :test #'eq))
         (slot-names (gethash :__slots__ class-ht))
         (initarg-map (gethash :__initargs__ class-ht))
         (initform-values (gethash :__initforms__ class-ht))
         (initarg-regs (vm-initarg-regs inst)))
    ;; Set the class reference
    (setf (gethash :__class__ obj-ht) class-ht)
    ;; Initialize all slots: use initform values if available, otherwise nil
    (dolist (slot-name slot-names)
      (let ((initform-entry (assoc slot-name initform-values)))
        (setf (gethash slot-name obj-ht)
              (if initform-entry (cdr initform-entry) nil))))
    ;; Apply initargs: match each provided initarg to its slot (overrides initform)
    (loop for (initarg-key . value-reg) in initarg-regs
          for value = (vm-reg-get state value-reg)
          do (let ((slot-entry (assoc initarg-key initarg-map)))
               (when slot-entry
                 (setf (gethash (cdr slot-entry) obj-ht) value))))
    (vm-reg-set state (vm-dst inst) obj-ht)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-slot-read) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst)))
    (multiple-value-bind (value found-p) (gethash slot-name obj-ht)
      (unless found-p
        (error "Slot ~S not found in object" slot-name))
      (vm-reg-set state (vm-dst inst) value)
      (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-slot-write) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst))
         (value (vm-reg-get state (vm-value-reg inst))))
    (setf (gethash slot-name obj-ht) value)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-register-method) state pc labels)
  (declare (ignore labels))
  (let* ((gf-ht (vm-reg-get state (vm-gf-reg inst)))
         (methods-ht (gethash :__methods__ gf-ht))
         (specializer (vm-method-specializer inst))
         (method-closure (vm-reg-get state (vm-method-reg inst))))
    (setf (gethash specializer methods-ht) method-closure)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-generic-call) state pc labels)
  (let* ((gf-ht (vm-reg-get state (vm-gf-reg inst)))
         (arg-regs (vm-args inst))
         (dst-reg (vm-dst inst)))
    (vm-dispatch-generic-call gf-ht state pc arg-regs dst-reg labels)))

;;; ----------------------------------------------------------------------------
;;; Handler-Case VM Instructions
;;; ----------------------------------------------------------------------------

(defclass vm-establish-handler (vm-instruction)
  ((handler-label :initarg :handler-label :reader vm-handler-label
                  :documentation "Label to jump to when error is caught")
   (result-reg :initarg :result-reg :reader vm-handler-result-reg
               :documentation "Register to store the error value in the handler")
   (error-type :initarg :error-type :initform 'error :reader vm-error-type
               :documentation "Error type this handler catches (symbol)"))
  (:documentation "Push a handler entry onto the handler stack for handler-case."))

(defclass vm-remove-handler (vm-instruction)
  ()
  (:documentation "Pop the top handler from the handler stack."))

(defclass vm-sync-handler-regs (vm-instruction)
  ()
  (:documentation "Update all remaining handlers' saved-regs with the current register state.
Used by unwind-protect to propagate cleanup modifications before re-signaling."))

(defclass vm-signal-error (vm-instruction)
  ((error-reg :initarg :error-reg :reader vm-error-reg
              :documentation "Register containing the error value to signal"))
  (:documentation "Signal an error: walk the handler stack to find a matching handler.
If no handler is found, signal a CL error."))

(defmethod instruction->sexp ((inst vm-establish-handler))
  (list :establish-handler
        (vm-handler-label inst)
        (vm-handler-result-reg inst)
        (vm-error-type inst)))

(defmethod instruction->sexp ((inst vm-remove-handler))
  (list :remove-handler))

(defmethod instruction->sexp ((inst vm-sync-handler-regs))
  (list :sync-handler-regs))

(defmethod instruction->sexp ((inst vm-signal-error))
  (list :signal-error (vm-error-reg inst)))

(defmethod execute-instruction ((inst vm-establish-handler) state pc labels)
  (declare (ignore labels))
  ;; Save the current call-stack and register state so we can unwind
  (let ((saved-regs (let ((copy (make-hash-table :test (hash-table-test (vm-state-registers state)))))
                      (maphash (lambda (k v) (setf (gethash k copy) v))
                               (vm-state-registers state))
                      copy)))
    (push (list (vm-handler-label inst)
                (vm-handler-result-reg inst)
                (vm-error-type inst)
                (copy-list (vm-call-stack state))
                saved-regs)
          (vm-handler-stack state)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-remove-handler) state pc labels)
  (declare (ignore labels))
  (when (vm-handler-stack state)
    (pop (vm-handler-stack state)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-sync-handler-regs) state pc labels)
  (declare (ignore labels))
  ;; Update all remaining handlers' saved-regs with current register state
  ;; so that cleanup modifications propagate through re-signal
  (dolist (entry (vm-handler-stack state))
    (let ((saved-regs (fifth entry)))
      (maphash (lambda (k v) (setf (gethash k saved-regs) v))
               (vm-state-registers state))))
  (values (1+ pc) nil nil))

(defun vm-error-type-matches-p (error-value handler-type)
  "Check if ERROR-VALUE matches HANDLER-TYPE for handler-case dispatch.
String errors (from VM error instruction) match error/condition/t but not subtypes.
CL condition objects use typep."
  (cond
    ;; Catch-all types always match
    ((member handler-type '(error condition serious-condition t)) t)
    ;; For host CL condition objects, use typep
    ((typep error-value 'condition)
     (ignore-errors (typep error-value handler-type)))
    ;; String/other VM errors don't match specific subtypes
    (t nil)))

(defmethod execute-instruction ((inst vm-signal-error) state pc labels)
  (let ((error-value (vm-reg-get state (vm-error-reg inst))))
    ;; Walk handler stack to find first matching handler by error type
    (let ((matching-handler nil)
          (handlers-to-skip 0))
      (dolist (entry (vm-handler-stack state))
        (let ((error-type (third entry)))
          (if (vm-error-type-matches-p error-value error-type)
              (progn (setf matching-handler entry) (return))
              (incf handlers-to-skip))))
      (if matching-handler
          (progn
            ;; Remove handlers up to and including the matching one
            (dotimes (i (1+ handlers-to-skip))
              (pop (vm-handler-stack state)))
            (destructuring-bind (handler-label result-reg error-type saved-call-stack saved-regs)
                matching-handler
              (declare (ignore error-type))
              (setf (vm-call-stack state) saved-call-stack)
              (clrhash (vm-state-registers state))
              (maphash (lambda (k v) (setf (gethash k (vm-state-registers state)) v))
                       saved-regs)
              (vm-reg-set state result-reg error-value)
              (values (gethash handler-label labels) nil nil)))
          (error "Unhandled error in VM: ~S" error-value)))))

;;; ----------------------------------------------------------------------------
;;; Label Table Building
;;; ----------------------------------------------------------------------------

(defun build-label-table (instructions)
  (let ((labels (make-hash-table :test #'equal)))
    (loop for inst in instructions
          for pc from 0
          do (when (typep inst 'vm-label)
               (setf (gethash (vm-name inst) labels) pc)))
    labels))

(defun run-compiled (program &key (output-stream *standard-output*))
  (let* ((instructions (vm-program-instructions program))
         (labels (build-label-table instructions))
         (state (make-instance 'vm-io-state :output-stream output-stream)))
    (loop with pc = 0
          while (< pc (length instructions))
          do (multiple-value-bind (next-pc halted result)
                 (execute-instruction (nth pc instructions) state pc labels)
               (when halted
                 (return result))
               (setf pc next-pc))
          finally (return nil))))
