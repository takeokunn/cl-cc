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
   (captured-values :initarg :captured-values :initform nil :reader vm-closure-captured-values
        :documentation "Captured lexical environment values as alist"))
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

;;; VM Instructions

(define-vm-instruction vm-const (vm-instruction)
  (dst nil :reader vm-dst)
  (value nil :reader vm-value)
  (:sexp-tag :const))

(define-vm-instruction vm-move (vm-instruction)
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :move))

(define-vm-instruction vm-binop (vm-instruction)
  (dst nil :reader vm-dst)
  (lhs nil :reader vm-lhs)
  (rhs nil :reader vm-rhs))

(define-vm-instruction vm-add (vm-binop)
  (:sexp-tag :add)
  (:sexp-slots dst lhs rhs))
(define-vm-instruction vm-sub (vm-binop)
  (:sexp-tag :sub)
  (:sexp-slots dst lhs rhs))
(define-vm-instruction vm-mul (vm-binop)
  (:sexp-tag :mul)
  (:sexp-slots dst lhs rhs))

(define-vm-instruction vm-label (vm-instruction)
  (name nil :reader vm-name)
  (:sexp-tag :label)
  (:conc-name vm-lbl-))

(define-vm-instruction vm-jump (vm-instruction)
  (label nil :reader vm-label-name)
  (:sexp-tag :jump))

(define-vm-instruction vm-jump-zero (vm-instruction)
  (reg nil :reader vm-reg)
  (label nil :reader vm-label-name)
  (:sexp-tag :jump-zero))

(define-vm-instruction vm-print (vm-instruction)
  (reg nil :reader vm-reg)
  (:sexp-tag :print))

(define-vm-instruction vm-halt (vm-instruction)
  (reg nil :reader vm-reg)
  (:sexp-tag :halt))

;;; Function support instructions
(define-vm-instruction vm-closure (vm-instruction)
  (dst nil :reader vm-dst)
  (label nil :reader vm-label-name)
  (params nil :reader vm-closure-params)
  (optional-params nil :reader vm-closure-optional-params)
  (rest-param nil :reader vm-closure-rest-param)
  (key-params nil :reader vm-closure-key-params)
  (captured nil :reader vm-captured-vars)
  (:sexp-tag :closure)
  (:sexp-slots dst label params optional-params rest-param key-params captured)
  (:conc-name vm-closure-inst-))

;; vm-make-closure uses list* for instruction->sexp (custom sexp handling)
(define-vm-instruction vm-make-closure (vm-instruction)
  "Create a closure from LABEL with PARAMS, capturing ENV-REGS values on heap."
  (dst nil :reader vm-dst)
  (label nil :reader vm-label-name)
  (params nil :reader vm-make-closure-params)
  (env-regs nil :reader vm-env-regs))

(defmethod instruction->sexp ((inst vm-make-closure))
  (list* :make-closure (vm-make-closure-dst inst) (vm-make-closure-label inst)
         (vm-make-closure-params inst) (vm-make-closure-env-regs inst)))

(setf (gethash :make-closure *instruction-constructors*)
      (lambda (sexp)
        (make-vm-make-closure :dst (second sexp) :label (third sexp)
                              :params (fourth sexp) :env-regs (nthcdr 4 sexp))))

(define-vm-instruction vm-closure-ref-idx (vm-instruction)
  "Access captured value at INDEX from closure in CLOSURE register."
  (dst nil :reader vm-dst)
  (closure nil :reader vm-closure-reg)
  (index nil :reader vm-closure-index)
  (:sexp-tag :closure-ref-idx))

(define-vm-instruction vm-call (vm-instruction)
  (dst nil :reader vm-dst)
  (func nil :reader vm-func-reg)
  (args nil :reader vm-args)
  (:sexp-tag :call))

(define-vm-instruction vm-ret (vm-instruction)
  (reg nil :reader vm-reg)
  (:sexp-tag :ret))

(define-vm-instruction vm-func-ref (vm-instruction)
  (dst nil :reader vm-dst)
  (label nil :reader vm-label-name)
  (:sexp-tag :func-ref))

;;; Multiple values and apply instructions
(define-vm-instruction vm-values (vm-instruction)
  "Multiple values. DST gets primary value, values-list stores all."
  (dst nil :reader vm-dst)
  (src-regs nil :reader vm-src-regs)
  (:sexp-tag :values))

(define-vm-instruction vm-mv-bind (vm-instruction)
  "Bind multiple values from values-list to registers."
  (dst-regs nil :reader vm-dst-regs)
  (:sexp-tag :mv-bind))

(define-vm-instruction vm-values-to-list (vm-instruction)
  "Convert vm-values-list to a list in DST register."
  (dst nil :reader vm-dst)
  (:sexp-tag :values-to-list))

(define-vm-instruction vm-spread-values (vm-instruction)
  "Spread a list from SRC register as multiple values. DST gets primary value."
  (dst nil :reader vm-dst)
  (src nil :reader vm-src)
  (:sexp-tag :spread-values))

(define-vm-instruction vm-clear-values (vm-instruction)
  "Clear vm-values-list to nil (reset multiple-values buffer before a form)."
  (:sexp-tag :clear-values))

(define-vm-instruction vm-ensure-values (vm-instruction)
  "If vm-values-list is nil, set it to (list (reg-get SRC)).
   Used after compiling a form for multiple-value-call to normalise
   plain expressions that do not emit vm-values."
  (src nil :reader vm-src)
  (:sexp-tag :ensure-values))

(define-vm-instruction vm-call-next-method (vm-instruction)
  "Call the next applicable method from the current method dispatch context.
   ARGS-REG holds the args list (nil means use original args).
   DST receives the return value."
  (dst nil :reader vm-dst)
  (args-reg nil :reader vm-cnm-args-reg)
  (:sexp-tag :call-next-method))

(define-vm-instruction vm-next-method-p (vm-instruction)
  "Return T if there is a next applicable method in the current dispatch context."
  (dst nil :reader vm-dst)
  (:sexp-tag :next-method-p))

(define-vm-instruction vm-apply (vm-instruction)
  "Apply function with spread arguments. Last arg is a list to spread."
  (dst nil :reader vm-dst)
  (func nil :reader vm-func-reg)
  (args nil :reader vm-args)
  (:sexp-tag :apply))

(define-vm-instruction vm-register-function (vm-instruction)
  "Register a closure in the global function registry by name."
  (name nil :reader vm-func-name)
  (src nil :reader vm-src)
  (:sexp-tag :register-function))

(define-vm-instruction vm-set-global (vm-instruction)
  "Store a value in the global variable store."
  (name nil :reader vm-global-name)
  (src nil :reader vm-src)
  (:sexp-tag :set-global))

(define-vm-instruction vm-get-global (vm-instruction)
  "Load a value from the global variable store into a register."
  (dst nil :reader vm-dst)
  (name nil :reader vm-global-name)
  (:sexp-tag :get-global))

(defstruct vm-program
  (instructions nil :type list)
  (result-register nil))

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
Used by call-next-method and next-method-p."))
  (:documentation "VM execution state with registers, call stack, and heap."))

;;; VM State Initialization — pre-populate standard global variables

(defmethod initialize-instance :after ((state vm-state) &key &allow-other-keys)
  "Initialize standard ANSI CL global variables in the VM."
  (let ((gv (vm-global-vars state)))
    ;; FR-1206: *features* and *modules*
    (setf (gethash '*features* gv) '(:common-lisp :cl-cc))
    (setf (gethash '*modules* gv) nil)
    (setf (gethash '*active-restarts* gv) nil)
    ;; FR-1204: time constants
    (setf (gethash 'internal-time-units-per-second gv) internal-time-units-per-second)
    ;; FR-1205: random state
    (setf (gethash '*random-state* gv) *random-state*)
    ;; Standard I/O vars
    (setf (gethash '*standard-output* gv) *standard-output*)
    (setf (gethash '*standard-input*  gv) *standard-input*)
    (setf (gethash '*error-output*    gv) *error-output*)
    (setf (gethash '*trace-output*    gv) *trace-output*)
    (setf (gethash '*debug-io*        gv) *debug-io*)
    (setf (gethash '*query-io*        gv) *query-io*)
    ;; Standard print vars
    (setf (gethash '*print-base*   gv) 10)
    (setf (gethash '*print-radix*  gv) nil)
    (setf (gethash '*print-circle* gv) nil)
    (setf (gethash '*print-pretty* gv) nil)
    (setf (gethash '*print-level*  gv) nil)
    (setf (gethash '*print-length* gv) nil)
    (setf (gethash '*print-escape* gv) t)
    (setf (gethash '*print-readably* gv) nil)
    (setf (gethash '*print-gensym* gv) t)))

;;; VM Heap Operations

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


(defun vm-build-list (state values)
  "Build a native CL list from VALUES for use as a &rest parameter.
Uses native cons cells (same as vm-cons instruction)."
  (declare (ignore state))
  (copy-list values))


(defun vm-heap-address (object)
  "Get heap address from an object. For cons cells and closures."
  (etypecase object
    (integer object)
    (vm-heap-address (vm-heap-address-value object))
    (null nil)))

;;; Old instruction->sexp and sexp->instruction removed — auto-generated by define-vm-instruction.

;;; instruction->sexp / sexp->instruction now table-driven via define-vm-instruction.

(defgeneric execute-instruction (instruction state pc labels))

(defun vm-generic-function-p (value)
  "Return T if VALUE is a generic function dispatch table (hash table with :__methods__)."
  (and (hash-table-p value)
       (gethash :__methods__ value)
       t))

;;; Host Function Bridge — whitelist of host CL functions callable from VM
;;;
;;; Only functions explicitly registered here can be resolved via fboundp.
;;; This prevents the VM from accidentally routing calls like `funcall` or
;;; `mapcar` to host CL (which would receive vm-closure-objects it can't handle).

(defvar *vm-host-bridge-functions* (make-hash-table :test #'eq)
  "Set of symbols whose host CL functions may be called directly from the VM.
Only functions that accept non-closure arguments (strings, numbers, symbols,
lists of data) should be registered here.")

(defun vm-register-host-bridge (sym)
  "Register SYM as a host-bridgeable function for the VM."
  (setf (gethash sym *vm-host-bridge-functions*) t))

;;; Runtime helpers for setf expansion
(defun rt-plist-put (plist indicator value)
  "Return a new plist with INDICATOR set to VALUE. Non-destructive."
  (let ((result nil) (found nil) (p plist))
    (loop while p do
          (let ((k (car p)))
            (if (eq k indicator)
                (progn (push indicator result) (push value result) (setf found t))
                (progn (push k result) (push (cadr p) result)))
            (setf p (cddr p))))
    (unless found
      (push value result) (push indicator result))
    (nreverse result)))

;; Register self-hosting functions: these take strings/forms, not closures
(dolist (sym '(run-string run-string-repl our-eval our-load
              compile-expression compile-string
              parse-all-forms
              ;; Macro expansion support
              generate-lambda-bindings register-macro
              ;; CL functions needed by self-hosting code
              find-package symbol-function intern
              ;; Runtime helpers for setf expansion
              rt-plist-put))
  (vm-register-host-bridge sym))

(defun vm-resolve-function (state value)
  "Resolve VALUE to a closure, generic function, or host bridge function.
If VALUE is already a closure, return it.
If VALUE is a hash table with :__methods__, return it (generic function).
If VALUE is a symbol, look it up in the function registry first, then
check the host bridge whitelist."
  (cond
    ((typep value 'vm-closure-object) value)
    ((vm-generic-function-p value) value)
    ((functionp value) value)
    ((symbolp value)
     (let ((entry (gethash value (vm-function-registry state))))
       (cond
         (entry entry)
         ;; Only bridge whitelisted host functions
         ((and (gethash value *vm-host-bridge-functions*)
               (fboundp value))
          (symbol-function value))
         (t (error "Undefined function: ~S" value)))))
    (t (error "Invalid function designator: ~S" value))))

(defmethod execute-instruction ((inst vm-const) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (vm-value inst))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-move) state pc labels)
  (declare (ignore labels))
  (vm-reg-set state (vm-dst inst) (vm-reg-get state (vm-src inst)))
  (values (1+ pc) nil nil))

(define-simple-instruction vm-add :binary +)
(define-simple-instruction vm-sub :binary -)
(define-simple-instruction vm-mul :binary *)

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

(defun vm-get-all-applicable-methods (gf-ht state all-args)
  "Return list of all applicable method closures for GF-HT and ALL-ARGS, most-specific first."
  (let* ((methods-ht (gethash :__methods__ gf-ht))
         (first-arg (car all-args))
         (class-name (vm-classify-arg first-arg state))
         (class-ht (gethash class-name (vm-class-registry state)))
         (cpl (if class-ht
                  (let ((c (gethash :__cpl__ class-ht)))
                    (if (member t c) c (append c (list t))))
                  (list class-name t)))
         (result nil))
    ;; Collect methods in CPL order (most-specific first)
    (dolist (ancestor cpl)
      (let ((m (or (gethash (list ancestor) methods-ht)
                   (gethash ancestor methods-ht))))
        (when m (push m result))))
    ;; Fallback: t-specializer (if not already collected)
    (let ((t-method (gethash t methods-ht)))
      (when (and t-method (not (member t-method result)))
        (push t-method result)))
    (nreverse result)))

(defun vm-dispatch-generic-call (gf-ht state pc arg-regs dst-reg labels)
  "Dispatch a generic function call. GF-HT is the generic function dispatch table.
Supports multiple dispatch by passing all argument values for composite key lookup.
Returns (values next-pc halt-p result) like execute-instruction."
  (let* ((all-arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs))
         (first-arg (car all-arg-values))
         ;; Compute all applicable methods (for call-next-method support)
         (all-methods (vm-get-all-applicable-methods gf-ht state all-arg-values))
         (method-closure (or (car all-methods)
                             (vm-resolve-gf-method gf-ht state first-arg all-arg-values)))
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
    ;; Push method context for call-next-method support
    (push (list gf-ht all-methods all-arg-values) (vm-method-call-stack state))
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
    ;; Host CL function (from whitelist bridge) — apply directly
    (when (functionp func)
      (let ((arg-values (mapcar (lambda (r) (vm-reg-get state r)) arg-regs)))
        (vm-reg-set state dst-reg (apply func arg-values))
        (return-from execute-instruction (values (1+ pc) nil nil))))
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
      ;; Push nil method-call-stack frame (regular function, not generic dispatch)
      (push nil (vm-method-call-stack state))
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
          ;; Pop method-call-stack in sync with call-stack
          (when (vm-method-call-stack state)
            (pop (vm-method-call-stack state)))
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
  ;; Try the function registry first — user-defined functions (defun) register
  ;; closures with proper entry-label, params, and captured-values.
  ;; Fall back to a bare closure for local labels in the same compilation unit.
  (let* ((label-str (vm-label-name inst))
         (sym (find-symbol label-str :cl-cc))
         (registered (when sym (gethash sym (vm-function-registry state)))))
    (vm-reg-set state (vm-dst inst)
                (or registered
                    (make-instance 'vm-closure-object
                                   :entry-label label-str
                                   :params nil
                                   :captured-values nil))))
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

;;; Multiple Values and Apply Execution

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

(defmethod execute-instruction ((inst vm-spread-values) state pc labels)
  (declare (ignore labels))
  (let ((lst (vm-reg-get state (vm-src inst))))
    (setf (vm-values-list state) (if (listp lst) lst (list lst)))
    (vm-reg-set state (vm-dst inst) (if (listp lst) (first lst) lst))
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-clear-values) state pc labels)
  (declare (ignore labels inst))
  (setf (vm-values-list state) nil)
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-ensure-values) state pc labels)
  (declare (ignore labels))
  (unless (vm-values-list state)
    (setf (vm-values-list state) (list (vm-reg-get state (vm-src inst)))))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-next-method-p) state pc labels)
  (declare (ignore labels))
  ;; Look for the innermost method context in method-call-stack
  (let ((ctx (car (vm-method-call-stack state))))
    (vm-reg-set state (vm-dst inst)
                (if (and ctx (cddr ctx) (cadr ctx) (cdadr ctx))
                    t
                    nil)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-call-next-method) state pc labels)
  ;; Find current method context (top of method-call-stack)
  (let ((ctx (car (vm-method-call-stack state))))
    (unless ctx
      (error "call-next-method called outside of a generic function method"))
    (let* ((gf-ht (first ctx))
           (methods-list (second ctx))    ; [current-method next1 next2 ...]
           (orig-args (third ctx))
           (next-method (cadr methods-list)))
      (unless next-method
        (error "No next method for ~S" (gethash :__name__ gf-ht)))
      ;; Determine arguments: use supplied args or original args
      (let* ((args-reg (vm-cnm-args-reg inst))
             (call-args (if args-reg
                            (vm-reg-get state args-reg)
                            orig-args))
             (dst-reg (vm-dst inst))
             (entry-label (vm-closure-entry-label next-method))
             (params (vm-closure-params next-method))
             (captured (vm-closure-captured-values next-method))
             (return-pc (1+ pc))
             (old-closure-env (vm-closure-env state))
             (saved-regs (let ((copy (make-hash-table :test (hash-table-test (vm-state-registers state)))))
                           (maphash (lambda (k v) (setf (gethash k copy) v))
                                    (vm-state-registers state))
                           copy)))
        ;; Push call frame for next method
        (push (list return-pc dst-reg old-closure-env saved-regs) (vm-call-stack state))
        ;; Push updated method context (remaining methods starting from next-method)
        (push (list gf-ht (cdr methods-list) call-args) (vm-method-call-stack state))
        ;; Set up next method's parameters
        (loop for var-binding in captured
              do (vm-reg-set state (car var-binding) (cdr var-binding)))
        (loop for param in params
              for arg-val in call-args
              do (vm-reg-set state param arg-val))
        (when captured
          (setf (vm-closure-env state) captured))
        (values (gethash entry-label labels) nil nil)))))

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
                            nil)))
      ;; Host CL function (from whitelist bridge) — apply directly
      (when (functionp func)
        (vm-reg-set state dst-reg (apply func spread-args))
        (return-from execute-instruction (values (1+ pc) nil nil)))
      (let* (;; If generic function, resolve to the applicable method
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
      ;; Push nil method-call-stack frame (apply is not a generic dispatch)
      (push nil (vm-method-call-stack state))
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
      (values (gethash entry-label labels) nil nil)))))

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

;;; CLOS VM Instructions

(define-vm-instruction vm-class-def (vm-instruction)
  "Define a class. Creates a class descriptor hash table."
  (dst nil :reader vm-dst)
  (class-name nil :reader vm-class-name-sym)
  (superclasses nil :reader vm-superclasses)
  (slot-names nil :reader vm-slot-names)
  (slot-initargs nil :reader vm-slot-initargs)
  (slot-initform-regs nil :reader vm-slot-initform-regs)
  (:sexp-tag :class-def))

(define-vm-instruction vm-make-obj (vm-instruction)
  "Create an instance of a class."
  (dst nil :reader vm-dst)
  (class-reg nil :reader vm-class-reg)
  (initarg-regs nil :reader vm-initarg-regs)
  (:sexp-tag :make-obj))

(define-vm-instruction vm-slot-read (vm-instruction)
  "Read a slot value from an object."
  (dst nil :reader vm-dst)
  (obj-reg nil :reader vm-obj-reg)
  (slot-name nil :reader vm-slot-name-sym)
  (:sexp-tag :slot-read))

(define-vm-instruction vm-slot-write (vm-instruction)
  "Write a value to an object slot."
  (obj-reg nil :reader vm-obj-reg)
  (slot-name nil :reader vm-slot-name-sym)
  (value-reg nil :reader vm-value-reg)
  (:sexp-tag :slot-write))

(define-vm-instruction vm-register-method (vm-instruction)
  "Register a method closure on a generic function dispatch table."
  (gf-reg nil :reader vm-gf-reg)
  (specializer nil :reader vm-method-specializer)
  (method-reg nil :reader vm-method-reg)
  (:sexp-tag :register-method))

(define-vm-instruction vm-generic-call (vm-instruction)
  "Dispatch and call a generic function method."
  (dst nil :reader vm-dst)
  (gf-reg nil :reader vm-gf-reg)
  (args nil :reader vm-args)
  (:sexp-tag :generic-call))

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

;;; FR-1002: CLOS slot predicates and slot manipulation

(define-vm-instruction vm-slot-boundp (vm-instruction)
  "Test if slot SLOT-NAME is bound in object OBJ."
  (dst nil :reader vm-dst)
  (obj-reg nil :reader vm-obj-reg)
  (slot-name-sym nil :reader vm-slot-name-sym)
  (:sexp-tag :slot-boundp)
  (:sexp-slots dst obj-reg slot-name-sym))

(defmethod execute-instruction ((inst vm-slot-boundp) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst)))
    (multiple-value-bind (value found-p) (gethash slot-name obj-ht)
      (declare (ignore value))
      (vm-reg-set state (vm-dst inst) (if found-p t nil))
      (values (1+ pc) nil nil))))

(define-vm-instruction vm-slot-makunbound (vm-instruction)
  "Remove slot SLOT-NAME from object OBJ. Returns OBJ."
  (dst nil :reader vm-dst)
  (obj-reg nil :reader vm-obj-reg)
  (slot-name-sym nil :reader vm-slot-name-sym)
  (:sexp-tag :slot-makunbound)
  (:sexp-slots dst obj-reg slot-name-sym))

(defmethod execute-instruction ((inst vm-slot-makunbound) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst)))
    (remhash slot-name obj-ht)
    (vm-reg-set state (vm-dst inst) (vm-reg-get state (vm-obj-reg inst)))
    (values (1+ pc) nil nil)))

(define-vm-instruction vm-slot-exists-p (vm-instruction)
  "Test if class of OBJ has a slot named SLOT-NAME."
  (dst nil :reader vm-dst)
  (obj-reg nil :reader vm-obj-reg)
  (slot-name-sym nil :reader vm-slot-name-sym)
  (:sexp-tag :slot-exists-p)
  (:sexp-slots dst obj-reg slot-name-sym))

(defmethod execute-instruction ((inst vm-slot-exists-p) state pc labels)
  (declare (ignore labels))
  (let* ((obj-ht (vm-reg-get state (vm-obj-reg inst)))
         (slot-name (vm-slot-name-sym inst))
         (class-ht (when (hash-table-p obj-ht) (gethash :__class__ obj-ht)))
         (slots (when class-ht (gethash :__slots__ class-ht))))
    (vm-reg-set state (vm-dst inst)
                (if (and slots (member slot-name slots)) t nil))
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

;;; Handler-Case VM Instructions

(define-vm-instruction vm-establish-handler (vm-instruction)
  "Push a handler entry onto the handler stack for handler-case."
  (handler-label nil :reader vm-handler-label)
  (result-reg nil :reader vm-handler-result-reg)
  (error-type nil :reader vm-error-type)
  (:sexp-tag :establish-handler))

(define-vm-instruction vm-remove-handler (vm-instruction)
  "Pop the top handler from the handler stack."
  (:sexp-tag :remove-handler))

(define-vm-instruction vm-sync-handler-regs (vm-instruction)
  "Update all handlers' saved-regs with current register state."
  (:sexp-tag :sync-handler-regs))

(define-vm-instruction vm-signal-error (vm-instruction)
  "Signal an error: walk the handler stack to find a matching handler."
  (error-reg nil :reader vm-error-reg)
  (:sexp-tag :signal-error))

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
                saved-regs
                (copy-list (vm-method-call-stack state)))
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
            (destructuring-bind (handler-label result-reg error-type saved-call-stack saved-regs
                                 &optional saved-method-call-stack)
                matching-handler
              (declare (ignore error-type))
              (setf (vm-call-stack state) saved-call-stack)
              (setf (vm-method-call-stack state) (or saved-method-call-stack nil))
              (clrhash (vm-state-registers state))
              (maphash (lambda (k v) (setf (gethash k (vm-state-registers state)) v))
                       saved-regs)
              (vm-reg-set state result-reg error-value)
              (values (gethash handler-label labels) nil nil)))
          (error "Unhandled error in VM: ~S" error-value)))))

;;; Label Table Building

(defun build-label-table (instructions)
  (let ((labels (make-hash-table :test #'equal)))
    (loop for inst in instructions
          for pc from 0
          do (when (typep inst 'vm-label)
               (setf (gethash (vm-name inst) labels) pc)))
    labels))

(defun run-program-slice (instructions labels start-pc state)
  "Execute INSTRUCTIONS (a vector) from START-PC to end, using LABELS for
label resolution and STATE for VM state.  Returns the halted result value,
or NIL if execution falls off the end without a halt instruction.
Used by run-string-repl for incremental REPL execution against a shared pool."
  (loop with pc = start-pc
        while (< pc (length instructions))
        do (multiple-value-bind (next-pc halted result)
               (execute-instruction (aref instructions pc) state pc labels)
             (when halted
               (return result))
             (setf pc next-pc))
        finally (return nil)))

(defun run-compiled (program &key (output-stream *standard-output*) state)
  "Run a compiled VM program.
If STATE is provided, execute using that existing vm-io-state (for REPL persistence).
Otherwise a fresh state is created from OUTPUT-STREAM."
  (let* ((instructions (vm-program-instructions program))
         (labels (build-label-table instructions))
         (state (or state (make-instance 'vm-io-state :output-stream output-stream))))
    (loop with pc = 0
          while (< pc (length instructions))
          do (multiple-value-bind (next-pc halted result)
                 (execute-instruction (nth pc instructions) state pc labels)
               (when halted
                 (return result))
               (setf pc next-pc))
          finally (return nil))))

;;; ─── Phase A: defopcode dispatch table + vm-state defstruct + run-vm ────────

;;; Opcode dispatch tables (256-slot vectors)
(defparameter *opcode-dispatch-table*
  (make-array 256 :initial-element nil)
  "Vector mapping opcode integer → handler function (lambda (state code pc) ...).")

(defparameter *opcode-name-table*
  (make-array 256 :initial-element nil)
  "Vector mapping opcode integer → symbol name.")

(defparameter *opcode-encoder-table*
  (make-hash-table :test 'eq)
  "Hash table mapping opcode symbol name → opcode integer.")

;;; Opcode counter (auto-incremented by defopcode)
;;; eval-when ensures this is bound at compile time (needed by defconstant in defopcode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *next-opcode* 0
    "Next available opcode integer; incremented by each defopcode form."))

(defmacro defopcode (name &body body)
  "Define a bytecode opcode NAME.
Assigns it the next available opcode number, registers it in all three
dispatch tables, and defines a constant +OP2-NAME+ for the opcode number.

The BODY must contain exactly one form: (lambda (state code pc) ...) which
returns the next PC value."
  ;; Capture and increment at macro-expansion time so defconstant gets a literal.
  (let ((op-sym (intern (format nil "+OP2-~A+" (symbol-name name))))
        (op-num *next-opcode*))
    (incf *next-opcode*)
    `(progn
       (defconstant ,op-sym ,op-num)
       (setf (aref *opcode-dispatch-table* ,op-sym) ,@body)
       (setf (aref *opcode-name-table*     ,op-sym) ',name)
       (setf (gethash ',name *opcode-encoder-table*) ,op-sym)
       ',name)))

;;; Register file size constant
(defconstant +vm-register-count+ 256
  "Number of integer-indexed registers in the vm2-state register file.")

;;; vm2-state defstruct with integer-indexed register file
;;; Distinct from vm-state (defclass, CLOS execute-instruction system).
;;; This is the flat-vector interpreter state for run-vm.
(defstruct (vm2-state
             (:constructor make-vm2-state
               (&key (output-stream *standard-output*)
                &aux (registers (make-array +vm-register-count+ :initial-element nil))
                     (global-vars (let ((ht (make-hash-table)))
                                    (setf (gethash '*features* ht) '(:cl-cc))
                                    (setf (gethash '*active-restarts* ht) nil)
                                    (setf (gethash '*standard-output* ht) *standard-output*)
                                    (setf (gethash '*standard-input* ht) *standard-input*)
                                    (setf (gethash '*error-output* ht) *error-output*)
                                    (setf (gethash '*trace-output* ht) *trace-output*)
                                    (setf (gethash '*debug-io* ht) *debug-io*)
                                    (setf (gethash '*query-io* ht) *query-io*)
                                    ht)))))
  "VM2 state for the flat-vector run-vm interpreter.
REGISTERS: simple-vector of 256 integer-indexed slots (fast svref access).
GLOBAL-VARS: hash table for global variable bindings.
OUTPUT-STREAM: stream for I/O."
  (registers    nil :type simple-vector)
  (global-vars  nil :type hash-table)
  (output-stream *standard-output*))

;;; Integer-indexed register accessors for vm2-state
(defun vm2-reg-get (state reg)
  "Get register REG (integer) from vm2-state STATE."
  (svref (vm2-state-registers state) reg))

(defun vm2-reg-set (state reg value)
  "Set register REG (integer) to VALUE in vm2-state STATE. Returns VALUE."
  (setf (svref (vm2-state-registers state) reg) value)
  value)

;;; Bytecode layout: flat simple-vector of 4-word groups
;;;   word 0: opcode (fixnum)
;;;   word 1: dst register (fixnum or nil)
;;;   word 2: src1/immediate (any CL object)
;;;   word 3: src2 register (fixnum or nil)

;;; Opcode definitions

(defopcode const
  (lambda (state code pc regs)
    (declare (ignore state))
    ;; CONST dst immediate → regs[dst] := immediate
    (let ((dst (svref code (+ pc 1)))
          (imm (svref code (+ pc 2))))
      (setf (svref regs dst) imm))
    (+ pc 4)))

(defopcode move
  (lambda (state code pc regs)
    (declare (ignore state))
    ;; MOVE dst src → regs[dst] := regs[src]
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2))))
      (setf (svref regs dst) (svref regs src)))
    (+ pc 4)))

(defopcode add2
  (lambda (state code pc regs)
    (declare (ignore state))
    ;; ADD2 dst src1 src2 → regs[dst] := regs[src1] + regs[src2]
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (+ (svref regs src1) (svref regs src2))))
    (+ pc 4)))

(defopcode sub2
  (lambda (state code pc regs)
    (declare (ignore state))
    ;; SUB2 dst src1 src2 → regs[dst] := regs[src1] - regs[src2]
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (- (svref regs src1) (svref regs src2))))
    (+ pc 4)))

(defopcode mul2
  (lambda (state code pc regs)
    (declare (ignore state))
    ;; MUL2 dst src1 src2 → regs[dst] := regs[src1] * regs[src2]
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (* (svref regs src1) (svref regs src2))))
    (+ pc 4)))

(defopcode halt2
  (lambda (state code pc regs)
    ;; HALT2 result-reg → return regs[result-reg] from run-vm
    ;; We signal halt by calling a non-local exit; run-vm catches it.
    (declare (ignore state))
    (let ((result-reg (svref code (+ pc 1))))
      (throw 'vm-halt (svref regs result-reg)))))

;;; Redefine run-vm to handle halt via catch/throw

(defun run-vm (code state)
  "Run bytecode CODE (a simple-vector) using STATE (a vm2-state struct).
Returns the value in the result register when halt2 executes."
  (declare (type simple-vector code)
           (type vm2-state state))
  (let ((regs (vm2-state-registers state))
        (len  (length code))
        (pc   0))
    (catch 'vm-halt
      (loop while (< pc len)
            do (let ((op (svref code pc)))
                 (let ((handler (aref *opcode-dispatch-table* op)))
                   (setf pc (funcall handler state code pc regs))))
            finally (return nil)))))

;;; ─── vm2-state compatibility shims ──────────────────────────────────────────
;;;
;;; These make vm2-state accessible via the same API as vm-state, allowing
;;; tests to use make-vm-state, vm-state-registers, vm-reg-get,
;;; vm-reg-set, vm-output-stream, and vm-global-vars uniformly.

(defun make-vm-state (&key (output-stream *standard-output*))
  "Create a vm2-state with an integer-indexed register file.
This is the PREFERRED constructor for new code; vm-io-state is for the
legacy CLOS execute-instruction pipeline."
  (make-vm2-state :output-stream output-stream))


;;; Extend existing CLOS generic functions to dispatch on vm2-state
(defmethod vm-state-registers ((s vm2-state))
  "Return the simple-vector register file of a vm2-state."
  (vm2-state-registers s))

(defmethod vm-output-stream ((s vm2-state))
  "Return the output stream of a vm2-state."
  (vm2-state-output-stream s))

(defmethod vm-global-vars ((s vm2-state))
  "Return the global-vars hash table of a vm2-state."
  (vm2-state-global-vars s))

;;; Redefine vm-reg-get/vm-reg-set to dispatch on vm2-state vs vm-state
;;; For vm2-state: use integer-indexed svref (registers = simple-vector)
;;; For vm-state class: use gethash with keyword or integer key
(defun vm-reg-get (state reg)
  (if (vm2-state-p state)
      (svref (vm2-state-registers state) reg)
      (gethash reg (slot-value state 'registers) 0)))

(defun vm-reg-set (state reg value)
  (if (vm2-state-p state)
      (setf (svref (vm2-state-registers state) reg) value)
      (setf (gethash reg (slot-value state 'registers)) value))
  value)
