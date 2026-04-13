(in-package :cl-cc)

;;; VM Heap Object Base Class

(defclass vm-heap-object ()
  ()
  (:documentation "Base class for all objects that can be stored on the VM heap.
Provides a common supertype for heap-allocated VM objects like cons cells,
closures, and reader states."))

;;; VM Heap Address Wrapper

(defstruct vm-heap-address
  "Wrapper for heap addresses to distinguish them from regular integers."
  (value 0 :type integer))

;;; VM Closure Object

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
   (rest-stack-alloc-p :initarg :rest-stack-alloc-p :initform nil :reader vm-closure-rest-stack-alloc-p
                       :documentation "T when the &rest list may be stack-allocated")
   (captured-values :initarg :captured-values :initform #() :reader vm-closure-captured-values
                    :documentation "Captured lexical environment values as a vector of (register . value) pairs"))
  (:documentation "Represents a closure with code and captured environment."))

;;; VM Cons Cell (Heap-based)

(defclass vm-cons-cell (vm-heap-object)
  ((car :initarg :car :accessor vm-cons-cell-car
        :documentation "The car (first) element of the cons cell")
   (cdr :initarg :cdr :accessor vm-cons-cell-cdr
        :documentation "The cdr (second) element of the cons cell"))
  (:documentation "Heap-allocated cons cell for VM list operations."))

;;; VM Instruction DSL (Phase 8/9)

(defstruct vm-instruction
  "Base struct for all VM instructions.")

(defvar *instruction-constructors* (make-hash-table :test 'eq)
  "Hash table mapping sexp tags (keywords) to constructor functions for sexp->instruction.")

(defgeneric instruction->sexp (instruction)
  (:documentation "Convert a VM instruction to its sexp representation."))

(defgeneric sexp->instruction (sexp)
  (:documentation "Convert a sexp to a VM instruction."))

(defmethod sexp->instruction ((sexp cons))
  (let ((constructor (gethash (car sexp) *instruction-constructors*)))
    (if constructor
        (funcall constructor sexp)
        (error "Unknown instruction sexp: ~S" sexp))))

(defmacro define-vm-instruction (name (parent) &body body)
  "Define a VM instruction as a defstruct with auto-generated sexp serialization.
NAME is the struct name. PARENT is the parent struct to :include.
BODY contains slot definitions, an optional docstring, and options.

Slot definition: (slot-name initform &key reader type)
Options:
  (:sexp-tag :keyword) - keyword for instruction->sexp / sexp->instruction
  (:sexp-slots s1 s2 ...) - slot names in sexp field order (default: all own slots)"
  (let (docstring slots sexp-tag sexp-slots conc-name-override)
    (dolist (form body)
      (cond
        ((stringp form) (setf docstring form))
        ((and (consp form) (eq (car form) :sexp-tag))
         (setf sexp-tag (cadr form)))
        ((and (consp form) (eq (car form) :sexp-slots))
         (setf sexp-slots (cdr form)))
        ((and (consp form) (eq (car form) :conc-name))
         (setf conc-name-override (cadr form)))
        ((consp form) (push form slots))
        (t (error "Invalid form in define-vm-instruction: ~S" form))))
    (setf slots (nreverse slots))
    (unless sexp-slots
      (setf sexp-slots (mapcar #'car slots)))
    (let ((prefix (if conc-name-override
                      (string conc-name-override)
                      (concatenate 'string (symbol-name name) "-"))))
      (labels ((struct-acc (slot)
                 (intern (format nil "~A~A" prefix (symbol-name slot))))
               (ctor ()
                 (intern (format nil "MAKE-~A" (symbol-name name))))
               (pos-form (n)
                 (case n
                   (1 '(second sexp)) (2 '(third sexp)) (3 '(fourth sexp))
                   (4 '(fifth sexp)) (5 '(sixth sexp)) (6 '(seventh sexp))
                   (t `(nth ,n sexp)))))
        `(progn
           (defstruct (,name (:include ,parent)
                            ,@(when conc-name-override
                                `((:conc-name ,conc-name-override))))
             ,@(when docstring `(,docstring))
             ,@(mapcar (lambda (slot)
                         (let ((sname (car slot))
                               (init (cadr slot))
                               (type (getf (cddr slot) :type)))
                           (if type `(,sname ,init :type ,type) `(,sname ,init))))
                       slots))
           ,@(loop for slot in slots
                   for sname = (car slot)
                   for reader = (getf (cddr slot) :reader)
                   when (and reader (not (eq reader (struct-acc sname))))
                   collect `(eval-when (:compile-toplevel :load-toplevel :execute)
                              (unless (fboundp ',reader)
                                (defgeneric ,reader (inst)))
                              (defmethod ,reader ((inst ,name))
                                (,(struct-acc sname) inst))))
           ,@(when sexp-tag
               `((defmethod instruction->sexp ((inst ,name))
                   (list ,sexp-tag
                         ,@(mapcar (lambda (s) `(,(struct-acc s) inst))
                                   sexp-slots)))
                 (setf (gethash ,sexp-tag *instruction-constructors*)
                       (lambda (sexp)
                         ,@(unless sexp-slots '((declare (ignore sexp))))
                         (,(ctor)
                          ,@(loop for s in sexp-slots
                                  for i from 1
                                  append `(,(intern (symbol-name s) :keyword)
                                           ,(pos-form i)))))))))))))

;;; Simple Instruction Method Generator

(defmacro define-simple-instruction (name shape cl-func &key (src 'vm-src) (dst 'vm-dst) (lhs 'vm-lhs) (rhs 'vm-rhs))
  "Generate a simple execute-instruction method.
SHAPE is one of:
  :unary  — (CL-FUNC (vm-reg-get state (SRC inst))) → dst
  :binary — (CL-FUNC (vm-reg-get state (LHS inst)) (vm-reg-get state (RHS inst))) → dst
  :pred1  — (if (CL-FUNC (vm-reg-get state (SRC inst))) 1 0) → dst
  :pred2  — (if (CL-FUNC (vm-reg-get state (LHS inst)) (vm-reg-get state (RHS inst))) 1 0) → dst"
  (let ((inst (gensym "INST"))
        (state (gensym "STATE"))
        (pc (gensym "PC")))
    `(defmethod execute-instruction ((,inst ,name) ,state ,pc labels)
       (declare (ignore labels))
       (vm-reg-set ,state (,dst ,inst)
                   ,(ecase shape
                      (:unary `(,cl-func (vm-reg-get ,state (,src ,inst))))
                      (:binary `(,cl-func (vm-reg-get ,state (,lhs ,inst))
                                          (vm-reg-get ,state (,rhs ,inst))))
                      (:pred1 `(if (,cl-func (vm-reg-get ,state (,src ,inst))) 1 0))
                      (:pred2 `(if (,cl-func (vm-reg-get ,state (,lhs ,inst))
                                             (vm-reg-get ,state (,rhs ,inst)))
                                   1 0))))
       (values (1+ ,pc) nil nil))))

;;; Instruction Structure Shorthand Macros

(defmacro define-vm-unary-instruction (name tag docstring)
  "Define a VM instruction with a single (dst src) slot pair."
  `(define-vm-instruction ,name (vm-instruction)
     ,docstring
     (dst nil :reader vm-dst)
     (src nil :reader vm-src)
     (:sexp-tag ,tag)
     (:sexp-slots dst src)))

(defmacro define-vm-binary-instruction (name tag docstring)
  "Define a VM instruction with (dst lhs rhs) slots for binary operations."
  `(define-vm-instruction ,name (vm-instruction)
     ,docstring
     (dst nil :reader vm-dst)
     (lhs nil :reader vm-lhs)
     (rhs nil :reader vm-rhs)
     (:sexp-tag ,tag)
     (:sexp-slots dst lhs rhs)))

(defmacro define-vm-char-comparison (name tag docstring)
  "Define a VM instruction with (dst char1 char2) slots for binary char comparisons."
  `(define-vm-instruction ,name (vm-instruction)
     ,docstring
     (dst nil :reader vm-dst)
     (char1 nil :reader vm-char1)
     (char2 nil :reader vm-char2)
     (:sexp-tag ,tag)
     (:sexp-slots dst char1 char2)))

(defstruct vm-program
  (instructions nil :type list)
  (result-register nil)
  (leaf-p nil :type boolean))

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
Values here persist across function calls (not subject to register save/restore).")
   (symbol-plists :initform (make-hash-table :test #'eq) :reader vm-symbol-plists
                  :documentation "Property lists for symbols: sym -> plist.
Used for get/setf-get/remprop/symbol-plist operations.")
   (method-call-stack :initform nil :accessor vm-method-call-stack
                       :documentation "Parallel stack to call-stack tracking CLOS method context.
Each frame is either NIL (regular call) or (gf-ht methods-list all-args) for generic dispatch.
Used by call-next-method and next-method-p.")
   (profile-enabled-p :initform nil :accessor vm-profile-enabled-p
                      :documentation "T when lightweight VM stack sampling is enabled.")
   (profile-call-stack :initform nil :accessor vm-profile-call-stack
                       :documentation "Current sampled call stack as a list of function labels, leaf first.")
   (profile-samples :initform (make-hash-table :test #'equal) :accessor vm-profile-samples
                    :documentation "Collapsed stack string -> sample count for lightweight flamegraphs."))
  (:documentation "VM execution state with registers, call stack, and heap."))

;;; VM state initialization, profiling, heap ops, and execute-instruction generic
;;; are in vm-state-init.lisp (loaded immediately after this file).
;;;
;;; vm-bridge.lisp  — host function bridge + CLOS slot-definition helpers
;;; vm-execute.lisp — execute-instruction methods for core instructions
;;; vm-clos.lisp    — CLOS instruction defstructs + execute-instruction methods
;;; vm-run.lisp     — Handler-case, label table, run-vm, vm2-state
