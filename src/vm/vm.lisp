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

;;; Data table: (symbol . initial-value-thunk-or-value)
;;; Thunks (lambdas) are called at init time so each vm-state gets a fresh value.
(defparameter *vm-initial-globals*
  `(;; Feature / module flags (FR-1206)
    (*features*                    (:common-lisp :cl-cc))
    (*modules*                     nil)
    (*active-restarts*             nil)
    ;; Time (FR-1204) and random state (FR-1205)
    (internal-time-units-per-second internal-time-units-per-second)
    (*random-state*                *random-state*)
    ;; Standard I/O streams — bound to host streams at init
    (*standard-output*  *standard-output*)
    (*standard-input*   *standard-input*)
    (*error-output*     *error-output*)
    (*trace-output*     *trace-output*)
    (*debug-io*         *debug-io*)
    (*query-io*         *query-io*)
    ;; Print-control variables (ANSI CL defaults)
    (*print-base*       10)
    (*print-radix*      nil)
    (*print-circle*     nil)
    (*print-pretty*     nil)
    (*print-level*      nil)
    (*print-length*     nil)
    (*print-escape*     t)
    (*print-readably*   nil)
    (*print-gensym*     t)
    (*readtable*        nil)
    ;; Package system — bind *package* to host CL-USER
    (*package*          ,(find-package :cl-user))
    ;; Condition/restart system
    (*%condition-handlers* nil)
    (*%active-restarts*    nil)
    ;; Documentation table (FR-607): (name . type) → docstring
    (*documentation-table* nil))
  "Initial bindings for standard ANSI CL global variables in each vm-state.")

;;; CL-level declaration so (boundp '*documentation-table*) is T in the compiler
;;; Initialized as a real hash-table so the defun expander can gethash/setf into it
(defvar *documentation-table* (make-hash-table :test #'equal))

(defmethod initialize-instance :after ((state vm-state) &key &allow-other-keys)
  "Initialize standard ANSI CL global variables in the VM."
  (let ((gv (vm-global-vars state)))
    (dolist (entry *vm-initial-globals*)
      (setf (gethash (car entry) gv)
            (let ((value (second entry)))
              (cond
                ((functionp value) (funcall value))
                ((and (symbolp value) (boundp value)) (symbol-value value))
                (t value)))))))

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

;;; Package introspection helpers for do-symbols/do-external-symbols/do-all-symbols
(defun %package-symbols (package)
  "Return a list of all symbols accessible in PACKAGE."
  (let ((result nil))
    (do-symbols (s package) (push s result))
    (nreverse result)))

(defun %package-external-symbols (package)
  "Return a list of all external symbols in PACKAGE."
  (let ((result nil))
    (do-external-symbols (s package) (push s result))
    (nreverse result)))

(defun %all-symbols ()
  "Return a list of all symbols in all packages."
  (let ((result nil))
    (do-all-symbols (s) (push s result))
    (nreverse result)))

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
      (push indicator result) (push value result))
    (nreverse result)))

;; Register self-hosting functions: these take strings/forms, not closures
(dolist (sym '(run-string run-string-repl our-eval our-load
              compile-expression compile-string
              parse-all-forms
              ;; Macro expansion support
              generate-lambda-bindings register-macro
              ;; CL functions needed by self-hosting code
              find-package symbol-function intern gensym
              ;; FR-647/FR-655/FR-428/FR-558: symbol-value, find-symbol, macro-function, symbol-package
              symbol-value find-symbol macro-function symbol-package
              ;; FR-624: subtypep — delegate to host CL for standard type hierarchy
              subtypep
              ;; FR-512: compile — delegate to host CL
              compile
              ;; FR-437/FR-438: Package introspection — delegate to host CL
              list-all-packages package-name package-nicknames
              package-use-list package-used-by-list package-shadowing-symbols
              make-package rename-package delete-package
              export import unexport shadow shadowing-import
              use-package unuse-package
              ;; FR-361: Package symbol iteration helpers
              %package-symbols %package-external-symbols %all-symbols
              ;; Runtime helpers for setf expansion
              rt-plist-put
              ;; FR-479: File system operations
              probe-file rename-file delete-file file-write-date file-author
              directory ensure-directories-exist
              ;; FR-566: Pathname constructors + accessors
              make-pathname
              pathname pathname-name pathname-type pathname-host
              pathname-device pathname-directory pathname-version
              truename parse-namestring merge-pathnames namestring
              file-namestring directory-namestring host-namestring
              enough-namestring wild-pathname-p pathname-match-p
              translate-pathname
              ;; FR-659: Reader — read-delimited-list
              read-delimited-list
              ;; FR-435: Apropos
              apropos apropos-list
              ;; FR-393: Composite streams
              make-synonym-stream make-broadcast-stream
              make-two-way-stream make-echo-stream
              make-concatenated-stream
              ;; FR-567: Composite stream accessors
              broadcast-stream-streams two-way-stream-input-stream
              two-way-stream-output-stream echo-stream-input-stream
              echo-stream-output-stream concatenated-stream-streams
              synonym-stream-symbol
               ;; Generic sequence access (used by search, write-sequence, etc.)
               elt append
               ;; FR-566: compile-file-pathname
               compile-file-pathname
              ;; FR-579: Encoding/decoding (SBCL sb-ext wrappers)
              string-to-octets octets-to-string
              ;; FR-607: Documentation retrieval from CL-level table
              %get-documentation))
  (vm-register-host-bridge sym))

;;; FR-579: String encoding helpers (delegate to SBCL's sb-ext)
(defun string-to-octets (string &key (encoding :utf-8) (external-format :default)
                                      (start 0) end)
  "Convert STRING to a byte vector using ENCODING (default UTF-8)."
  (declare (ignore external-format))
  (let ((format (case encoding (:utf-8 :utf-8) (:ascii :ascii) (t :utf-8))))
    (sb-ext:string-to-octets string :external-format format
                                    :start start :end (or end (length string)))))

(defun octets-to-string (octets &key (encoding :utf-8) (external-format :default)
                                       (start 0) end)
  "Convert byte vector OCTETS to a string using ENCODING (default UTF-8)."
  (declare (ignore external-format))
  (let ((format (case encoding (:utf-8 :utf-8) (:ascii :ascii) (t :utf-8))))
    (sb-ext:octets-to-string octets :external-format format
                                    :start start :end (or end (length octets)))))

;;; FR-607: Documentation retrieval (CL-level, registered by defun expander)
(defun %get-documentation (name doc-type)
  "Return documentation string for (NAME DOC-TYPE) from *documentation-table*, or nil."
  (when (hash-table-p *documentation-table*)
    (gethash (list name doc-type) *documentation-table*)))

;;; vm-execute.lisp — execute-instruction methods for core instructions (loaded next)
;;; vm-clos.lisp    — CLOS instruction defstructs + execute-instruction methods
;;; vm-run.lisp     — Handler-case, label table, run-vm, vm2-state
