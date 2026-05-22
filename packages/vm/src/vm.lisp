(in-package :cl-cc/vm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow '(lisp-implementation-type lisp-implementation-version
            machine-type machine-version machine-instance
            software-type software-version room apropos apropos-list)))

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

(defparameter *vm-current-compilation-tier* 0
  "Compilation tier associated with closures allocated by the currently running program.")

(defclass vm-closure-object (vm-heap-object)
  ((entry-label :initarg :entry-label :reader vm-closure-entry-label
                :documentation "Label where function code begins")
   (dispatch-tag :initarg :dispatch-tag :initform nil :accessor vm-closure-dispatch-tag
                 :documentation "Optional defunctionalization dispatch tag for known closures.")
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
    (captured-regs :initarg :captured-regs :initform #() :accessor vm-closure-captured-regs
                   :documentation "Captured lexical environment register names as a flat vector.")
    (captured-vals :initarg :captured-vals :initform #() :accessor vm-closure-captured-vals
                   :documentation "Captured lexical environment values as a flat vector parallel to CAPTURED-REGS.")
    (captured-values :initarg :captured-values :initform nil :accessor %vm-closure-legacy-captured-values
                      :documentation "Legacy serialized captured environment payload, normalized after initialization.")
    (deopt-info :initarg :deopt-info :initform nil :accessor vm-closure-deopt-info
                :documentation "PC -> deoptimization reconstruction metadata for optimized closures.")
    (osr-entry-points :initarg :osr-entry-points :initform nil :accessor vm-closure-osr-entry-points
                      :documentation "Loop back-edge OSR entry metadata for this closure.")
    (tier1-entry-points :initarg :tier1-entry-points :initform nil :accessor vm-closure-tier1-entry-points
                        :documentation "Optional Tier-1 entry metadata used by the OSR stub.")
    (invocation-count :initarg :invocation-count :initform 0 :accessor vm-closure-invocation-count
                      :documentation "Dynamic invocation count used by tiered compilation.")
    (compilation-tier :initarg :compilation-tier :initform *vm-current-compilation-tier* :accessor vm-closure-compilation-tier
                      :documentation "Compilation tier for this closure: 0 fast, 1 optimized.")
    (program-flat :initarg :program-flat :initform nil :accessor vm-closure-program-flat
                  :documentation "Optional flat instruction vector that owns this closure's entry label.")
   (label-table :initarg :label-table :initform nil :accessor vm-closure-label-table
                :documentation "Optional label table paired with PROGRAM-FLAT for cross-program calls."))
  (:documentation "Represents a closure with code and captured environment."))

(defun %vm-normalize-captured-values (captured-values)
  "Return two vectors, captured registers and captured values, from legacy CAPTURED-VALUES.
Legacy bytecode used either a vector/list of (register . value) pairs for normal
closures or a vector/list of raw values for indexed heap closures."
  (let ((payload (cond
                   ((null captured-values) nil)
                   ((vectorp captured-values) (coerce captured-values 'list))
                   ((listp captured-values) captured-values)
                   (t (list captured-values)))))
    (if (and payload (every #'consp payload))
        (values (coerce (mapcar #'car payload) 'vector)
                (coerce (mapcar #'cdr payload) 'vector))
        (values #() (coerce (or payload nil) 'vector)))))

(defmethod initialize-instance :after ((closure vm-closure-object) &key captured-values captured-regs captured-vals)
  "Normalize legacy closure captures into flat parallel vectors."
  (declare (ignore captured-regs captured-vals))
  (when captured-values
    (multiple-value-bind (regs vals) (%vm-normalize-captured-values captured-values)
      (setf (vm-closure-captured-regs closure) regs
            (vm-closure-captured-vals closure) vals))))

(defun vm-closure-captured-values (closure)
  "Return CLOSURE's legacy captured payload when present, otherwise values.
Runtime restore uses VM-CLOSURE-CAPTURED-REGS/VALS; this reader remains for
serialized bytecode/tests that constructed closures with :CAPTURED-VALUES."
  (or (%vm-closure-legacy-captured-values closure)
      (vm-closure-captured-vals closure)))

(defparameter *tier-upgrade-threshold* 50
  "Number of calls after which a Tier-0 closure is upgraded to Tier-1.")

(defvar *vm-recompile-function-hook* nil
  "Optional function called as (HOOK CLOSURE TARGET-TIER) for runtime tier upgrades.")

(defvar *vm-current-state* nil
  "Dynamically bound to the VM state while invoking host bridge callables.")

(defvar *hot-reload-history* nil
  "Newest-first list of hot reload events.
Each entry is a plist containing at least :TIME, :PATH, :FUNCTIONS, and :RESULT.")

(defvar *hot-reload-function-sources* (make-hash-table :test #'eq)
  "Map function symbols to the last source file that defined them for HOT-RELOAD.")

(defun %vm-copy-function-registry (registry &optional replacement-name replacement-value)
  "Return a shallow copy of REGISTRY, optionally replacing one function entry."
  (let ((copy (make-hash-table :test (hash-table-test registry)
                               :size (+ (hash-table-count registry) 8))))
    (maphash (lambda (key value)
               (setf (gethash key copy) value))
             registry)
    (when replacement-name
      (setf (gethash replacement-name copy) replacement-value))
    copy))

(defun vm-register-function-atomic (state name closure)
  "Atomically publish NAME -> CLOSURE in STATE's function registry using CAS.
The registry table is replaced rather than mutated, so calls that already hold
an old closure continue on old code while later symbolic calls see the new code."
  (check-type name symbol)
  (loop
    for old-registry = (slot-value state 'function-registry)
    for new-registry = (%vm-copy-function-registry old-registry name closure)
    do #+sbcl
       (when (eq (sb-ext:compare-and-swap (slot-value state 'function-registry)
                                          old-registry
                                          new-registry)
                 old-registry)
         (return closure))
       #-sbcl
       (progn
         (setf (slot-value state 'function-registry) new-registry)
         (return closure))))

(defun %hot-reload-program (compiled)
  "Return a VM program from COMPILED, accepting either a program or compilation result."
  (cond
    ((let ((predicate (and (fboundp 'vm-program-p)
                           (symbol-function 'vm-program-p))))
       (and predicate (funcall predicate compiled)))
     compiled)
    (t
     (let ((accessor (or (let ((package (find-package :cl-cc/compile)))
                           (and package
                                (multiple-value-bind (symbol status)
                                    (find-symbol "COMPILATION-RESULT-PROGRAM" package)
                                  (and status (fboundp symbol) (symbol-function symbol)))))
                         (let ((package (find-package :cl-cc)))
                           (and package
                                (multiple-value-bind (symbol status)
                                    (find-symbol "COMPILATION-RESULT-PROGRAM" package)
                                  (and status (fboundp symbol) (symbol-function symbol))))))))
       (if accessor
           (funcall accessor compiled)
           (error "Cannot extract VM program from hot reload result: ~S" compiled))))))

(defun %hot-reload-read-forms (source)
  "Read top-level forms from SOURCE for lightweight defun/source tracking."
  (or (and *vm-parse-forms-hook*
           (ignore-errors (funcall *vm-parse-forms-hook* source)))
      (with-input-from-string (in source)
        (loop for form = (read in nil :eof)
              until (eq form :eof)
              collect form))))

(defun %hot-reload-function-names (source)
  "Return DEFUN names found in SOURCE."
  (let ((names nil))
    (dolist (form (%hot-reload-read-forms source) (nreverse names))
      (when (and (consp form)
                 (symbolp (first form))
                 (string= (symbol-name (first form)) "DEFUN")
                 (symbolp (second form)))
        (pushnew (second form) names :test #'eq)))))

(defun %hot-reload-record (path functions result)
  "Record a hot reload event and update function-to-source mappings."
  (dolist (function functions)
    (setf (gethash function *hot-reload-function-sources*) path))
  (push (list :time (cl:get-universal-time)
              :path path
              :functions functions
              :result result)
        *hot-reload-history*)
  result)

(defun hot-reload-file (path &optional (state *vm-current-state*))
  "Compile and load PATH into STATE, atomically replacing registered functions."
  (unless state
    (error "HOT-RELOAD-FILE requires an active VM state"))
  (unless *vm-compile-string-hook*
    (error "HOT-RELOAD-FILE requires *VM-COMPILE-STRING-HOOK*"))
  (let* ((truename (namestring (truename path)))
         (source (with-open-file (in truename :direction :input)
                   (let ((text (make-string (file-length in))))
                     (read-sequence text in)
                     text)))
         (functions (%hot-reload-function-names source))
         (compiled (funcall *vm-compile-string-hook* source))
         (program (%hot-reload-program compiled))
         (result (run-compiled program :state state)))
    (%hot-reload-record truename functions result)))

(defun hot-reload (function-name &optional (state *vm-current-state*))
  "Reload the source file that most recently defined FUNCTION-NAME."
  (let ((path (gethash function-name *hot-reload-function-sources*)))
    (unless path
      (error "No hot reload source is known for function: ~S" function-name))
    (hot-reload-file path state)))

(defun vm-closure-note-invocation (closure)
  "Increment CLOSURE's invocation counter and return the new count."
  (incf (vm-closure-invocation-count closure)))

(defun vm-maybe-tier-upgrade-closure (closure &optional (target-tier 1))
  "Upgrade hot Tier-0 CLOSURE to TARGET-TIER using *VM-RECOMPILE-FUNCTION-HOOK*."
  (when (and (typep closure 'vm-closure-object)
             (< (vm-closure-compilation-tier closure) target-tier)
             (> (vm-closure-invocation-count closure) *tier-upgrade-threshold*))
    (when *vm-recompile-function-hook*
      (funcall *vm-recompile-function-hook* closure target-tier))
    (setf (vm-closure-compilation-tier closure) target-tier))
  closure)

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

;;; FR-140: VM-local helpers for the runtime's immediate-symbol bit patterns.
;;; Kept here (rather than depending on cl-cc/runtime) because cl-cc-vm is built
;;; as an independent ASDF system.
(defconstant +vm-immediate-symbol-base+ #x7FFF000000000100)
(defconstant +vm-immediate-symbol-mask+ #xFFFFFFFFFFFFFF00)
(defconstant +vm-immediate-symbol-index-mask+ #xFF)

(defparameter *vm-immediate-symbol-table*
  #(:key :value :test :test-not :start :end :from-end :count :initial-value
    :element-type :initial-element :allow-other-keys :adjustable :fill-pointer
    quote lambda function declare setq setf if progn let let* block return-from
    tagbody go catch throw unwind-protect flet labels macrolet symbol-macrolet
    the values multiple-value-bind multiple-value-call eval-when locally and or
    cond case typecase ecase ccase loop do do* dolist dotimes defun defmacro
    defvar defparameter defconstant defclass defmethod defgeneric car cdr cons
    list append apply funcall))

(defparameter *vm-immediate-symbol-indexes*
  (let ((table (make-hash-table :test #'eq)))
    (loop for sym across *vm-immediate-symbol-table*
          for i from 0
          do (setf (gethash sym table) i))
    table))

(declaim (inline vm-immediate-symbol-p vm-decode-symbol vm-encode-common-symbol))

(defun vm-immediate-symbol-p (value)
  (and (typep value '(unsigned-byte 64))
       (= (logand value +vm-immediate-symbol-mask+) +vm-immediate-symbol-base+)))

(defun vm-decode-symbol (value)
  (if (vm-immediate-symbol-p value)
      (svref *vm-immediate-symbol-table*
             (logand value +vm-immediate-symbol-index-mask+))
      value))

(defun vm-encode-common-symbol (symbol)
  (or (let ((index (and (symbolp symbol)
                        (gethash symbol *vm-immediate-symbol-indexes*))))
        (and index (logior +vm-immediate-symbol-base+ index)))
      symbol))

(defun vm-immediate-intern-enabled-p ()
  "Avoid immediate symbols while compiling CL-CC's own build-time macro code."
  (let* ((package (or *package* (find-package :cl-user)))
         (name (and package (package-name package))))
    (not (and name
              (or (string= name "CL-CC")
                  (and (>= (length name) 6)
                       (string= name "CL-CC/" :end1 6)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %vm-instruction-sexp-position-form (position)
    "Return the SEXP accessor form for the 1-based instruction operand POSITION."
    (case position
      (1 '(second sexp))
      (2 '(third sexp))
      (3 '(fourth sexp))
      (4 '(fifth sexp))
      (5 '(sixth sexp))
      (6 '(seventh sexp))
      (t `(nth ,position sexp))))

  (defun %vm-instruction-constructor-args (slot-names &optional (position 1))
    "Build keyword/value constructor arguments for SLOT-NAMES from an instruction SEXP."
    (if (null slot-names)
        nil
        (append (list (intern (symbol-name (car slot-names)) :keyword)
                      (%vm-instruction-sexp-position-form position))
                (%vm-instruction-constructor-args (cdr slot-names) (+ position 1))))))

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
       (flet ((struct-acc (slot)
                (intern (format nil "~A~A" prefix (symbol-name slot))))
              (ctor ()
                (intern (format nil "MAKE-~A" (symbol-name name)))))
         (let* ((struct-options (append (list name (list :include parent))
                                        (when conc-name-override
                                          (list (list :conc-name conc-name-override)))))
                (struct-slots
                  (mapcar (lambda (slot)
                            (let ((sname (car slot))
                                  (init (cadr slot))
                                  (type (getf (cddr slot) :type)))
                              (if type
                                  (list sname init :type type)
                                  (list sname init))))
                          slots))
                (struct-form
                  (append (list 'defstruct struct-options)
                          (when docstring (list docstring))
                          struct-slots))
                (reader-forms
                  (loop for slot in slots
                        for sname = (car slot)
                        for reader = (getf (cddr slot) :reader)
                        when (and reader (not (eq reader (struct-acc sname))))
                        collect (list 'eval-when
                                      '(:compile-toplevel :load-toplevel :execute)
                                      (list 'unless
                                            (list 'fboundp (list 'quote reader))
                                            (list 'defgeneric reader '(inst)))
                                      (list 'defmethod reader
                                            (list (list 'inst name))
                                            (list (struct-acc sname) 'inst)))))
                 (sexp-forms
                   (when sexp-tag
                     (let ((ctor-args
                             (%vm-instruction-constructor-args sexp-slots)))
                       (list
                        (list 'defmethod 'instruction->sexp
                              (list (list 'inst name))
                              (cons 'list
                                    (cons sexp-tag
                                          (mapcar (lambda (s)
                                                    (list (struct-acc s) 'inst))
                                                  sexp-slots))))
                         (list 'setf
                               (list 'gethash sexp-tag '*instruction-constructors*)
                               (append (list 'lambda '(sexp))
                                       (unless sexp-slots
                                         (list '(declare (ignore sexp))))
                                       (list (cons (ctor) ctor-args))))))))
                 (immediate-symbol-forms
                   (case name
                     (vm-intern-symbol
                      `((defmethod execute-instruction :around ((inst ,name) state pc labels)
                          (multiple-value-bind (next-pc value-1 value-2) (call-next-method)
                            (when (vm-immediate-intern-enabled-p)
                              (let ((value (vm-reg-get state (vm-dst inst))))
                                (when (symbolp value)
                                  (vm-reg-set state (vm-dst inst)
                                              (vm-encode-common-symbol value)))))
                            (values next-pc value-1 value-2)))))
                     (vm-symbol-name
                      `((defmethod execute-instruction :around ((inst ,name) state pc labels)
                          (declare (ignore labels))
                          (let ((value (vm-reg-get state (vm-src inst))))
                            (if (vm-immediate-symbol-p value)
                                (progn
                                  (vm-reg-set state (vm-dst inst)
                                              (rt-string-intern
                                                (symbol-name (vm-decode-symbol value))))
                                  (values (1+ pc) nil nil))
                                (call-next-method))))))
                     (vm-keywordp
                      `((defmethod execute-instruction :around ((inst ,name) state pc labels)
                          (declare (ignore labels))
                          (let ((value (vm-reg-get state (vm-src inst))))
                            (if (vm-immediate-symbol-p value)
                                (progn
                                  (vm-reg-set state (vm-dst inst)
                                              (if (keywordp (vm-decode-symbol value)) 1 0))
                                  (values (1+ pc) nil nil))
                                (call-next-method))))))
                     (vm-symbol-p
                      `((defmethod execute-instruction :around ((inst ,name) state pc labels)
                          (declare (ignore labels))
                          (let ((value (vm-reg-get state (vm-src inst))))
                            (if (vm-immediate-symbol-p value)
                                (progn
                                  (vm-reg-set state (vm-dst inst) 1)
                                  (values (1+ pc) nil nil))
                                (call-next-method)))))))))
            (cons 'progn
                  (append (list struct-form)
                          reader-forms
                          sexp-forms
                          immediate-symbol-forms)))))))

;;; FR-155: Deoptimization / OSR metadata and checkpoint instructions.

(defstruct vm-deopt-info
  "Interpreter-state reconstruction metadata for one optimized-code PC."
  (pc nil)
  (label nil)
  (live-regs nil)
  (env nil)
  (description nil))

(defstruct vm-deopt-frame
  "Runtime snapshot captured when a type guard or checkpoint deoptimizes."
  (pc nil)
  (reason nil)
  (registers nil)
  (call-stack nil)
  (closure-env nil)
  (values-list nil)
  (info nil))

(defvar *vm-current-program-deopt-info* nil
  "Dynamically bound PC -> deoptimization metadata for the currently running program.")

(define-vm-instruction vm-type-check (vm-instruction)
  "Runtime type guard for optimized code.  On failure, deoptimizes to LABEL."
  (src nil :reader vm-src)
  (type-name nil :reader vm-type-name)
  (label nil :reader vm-type-check-deopt-label)
  (id nil :reader vm-type-check-deopt-id)
  (:sexp-tag :type-check)
  (:sexp-slots src type-name label id))

(define-vm-instruction vm-deopt (vm-instruction)
  "Explicit deoptimization checkpoint.  Saves VM state and resumes in interpreter code."
  (label nil :reader vm-deopt-label)
  (id nil :reader vm-deopt-id)
  (reason :checkpoint :reader vm-deopt-reason)
  (:sexp-tag :deopt)
  (:sexp-slots label id reason))

(define-vm-instruction vm-osr-entry (vm-instruction)
  "Loop back-edge marker eligible for on-stack replacement into Tier-1 code."
  (label nil :reader vm-osr-label)
  (id nil :reader vm-osr-id)
  (:sexp-tag :osr-entry)
  (:sexp-slots label id))

;;; Shorthand macros (define-simple-instruction, define-vm-unary-instruction,
;;; define-vm-binary-instruction, define-vm-char-comparison), vm-program,
;;; and vm-state are in vm-dsl.lisp (loaded immediately after this file).
;;;
;;; VM state initialization, profiling, heap ops, and execute-instruction generic
;;; are in vm-state-init.lisp (loaded after vm-dsl).
;;;
;;; vm-bridge.lisp  — host function bridge + CLOS slot-definition helpers
;;; vm-execute.lisp — execute-instruction methods for core instructions
;;; vm-clos.lisp    — CLOS instruction defstructs + execute-instruction methods
;;; vm-run.lisp     — Handler-case, label table, run-vm, vm2-state

;;; ─── FR-612: Environment Introspection API ──────────────────────────────────

(defun lisp-implementation-type ()
  "CL-CC")

(defun lisp-implementation-version ()
  "0.1.0")

(defun machine-type () (cl:machine-type))
(defun machine-version () (cl:machine-version))
(defun machine-instance () (cl:machine-instance))
(defun software-type () (cl:software-type))
(defun software-version () (cl:software-version))

(defun room (&optional (stream cl:*standard-output*))
  (cl:format stream "; CL-CC ~A ~A on ~A ~A~%"
             (lisp-implementation-type) (lisp-implementation-version)
             (machine-type) (software-type)))

(defun apropos-list (string-designator &optional (package nil package-supplied-p))
  (let ((result nil)
        (string (cl:string string-designator))
        (packages (cl:if package-supplied-p
                         (cl:list (cl:find-package package))
                         (cl:list-all-packages))))
    (cl:dolist (pkg packages result)
      (cl:do-symbols (sym pkg)
        (cl:when (cl:search string (cl:symbol-name sym) :test #'cl:char-equal)
          (cl:pushnew sym result))))))

(defun apropos (string-designator &optional package)
  (cl:dolist (sym (apropos-list string-designator package))
    (cl:format cl:t "~A~%" sym))
  (cl:values))

;;; ─── FR-607: Multiple Readtables ─────────────────────────────────────────────

(defun %make-vm-readtable (&key (case :upcase) host macro-chars dispatch-chars)
  "Create the VM readtable representation used by reader bridges."
  (let ((table (make-hash-table :test #'eq)))
    (setf (gethash :readtable table) t
          (gethash :case table) case
          (gethash :host table) (or host (cl:copy-readtable nil))
          (gethash :macro-chars table) (or macro-chars (make-hash-table :test #'eql))
          (gethash :dispatch-chars table) (or dispatch-chars (make-hash-table :test #'equal)))
    table))

(defun %vm-readtable-p (object)
  (and (hash-table-p object) (gethash :readtable object)))

(defun %vm-readtable-host (readtable)
  (cond
    ((null readtable) (cl:copy-readtable nil))
    ((%vm-readtable-p readtable) (gethash :host readtable))
    (t readtable)))

(defun %copy-hash-table (table &key (test #'eql))
  (let ((copy (make-hash-table :test test)))
    (maphash (lambda (key value) (setf (gethash key copy) value)) table)
    copy))

(defvar *readtable* (%make-vm-readtable)
  "Current VM readtable.")

(defun make-readtable (&key (case :upcase))
  "Return a fresh VM readtable with READTABLE-CASE set to CASE."
  (let ((readtable (%make-vm-readtable :case case)))
    (setf (cl:readtable-case (gethash :host readtable)) case)
    readtable))

(defun copy-readtable (&optional (from-readtable *readtable*) to-readtable)
  "Copy FROM-READTABLE into TO-READTABLE or a fresh VM readtable."
  (let* ((source (or from-readtable *readtable*))
         (source-host (%vm-readtable-host source))
         (target (or to-readtable (%make-vm-readtable)))
         (target-host (cl:copy-readtable source-host (%vm-readtable-host target))))
    (when (%vm-readtable-p target)
      (setf (gethash :host target) target-host
            (gethash :case target) (cl:readtable-case target-host)
            (gethash :macro-chars target)
            (if (%vm-readtable-p source)
                (%copy-hash-table (gethash :macro-chars source) :test #'eql)
                (make-hash-table :test #'eql))
            (gethash :dispatch-chars target)
            (if (%vm-readtable-p source)
                (%copy-hash-table (gethash :dispatch-chars source) :test #'equal)
                (make-hash-table :test #'equal))))
    target))

(defun readtable-case (readtable)
  "Return READTABLE's case conversion mode."
  (if (%vm-readtable-p readtable)
      (gethash :case readtable)
      (cl:readtable-case readtable)))

(defun (setf readtable-case) (case readtable)
  "Set READTABLE's case conversion mode."
  (check-type case (member :upcase :downcase :preserve :invert))
  (if (%vm-readtable-p readtable)
      (setf (gethash :case readtable) case
            (cl:readtable-case (gethash :host readtable)) case)
      (setf (cl:readtable-case readtable) case))
  case)

(defun set-macro-character (char function &optional non-terminating-p
                                  (readtable *readtable*))
  "Install FUNCTION as CHAR's reader macro in READTABLE."
  (let ((char (character char)))
    (when (%vm-readtable-p readtable)
      (setf (gethash char (gethash :macro-chars readtable))
            (cons function non-terminating-p)))
    (when (functionp function)
      (cl:set-macro-character char function non-terminating-p
                              (%vm-readtable-host readtable))))
  t)

(defun get-macro-character (char &optional (readtable *readtable*))
  "Return the reader macro function and non-terminating flag for CHAR."
  (let ((char (character char)))
    (if (%vm-readtable-p readtable)
        (let ((entry (gethash char (gethash :macro-chars readtable))))
          (if entry
              (values (car entry) (cdr entry))
              (cl:get-macro-character char (gethash :host readtable))))
        (cl:get-macro-character char readtable))))

(defun %dispatch-key (disp-char sub-char)
  (cons (char-upcase (character disp-char))
        (char-upcase (character sub-char))))

(defun set-dispatch-macro-character (disp-char sub-char function
                                     &optional (readtable *readtable*))
  "Install FUNCTION for DISP-CHAR/SUB-CHAR in READTABLE."
  (let ((key (%dispatch-key disp-char sub-char)))
    (when (%vm-readtable-p readtable)
      (setf (gethash key (gethash :dispatch-chars readtable)) function))
    (when (functionp function)
      (cl:set-dispatch-macro-character (car key) (cdr key) function
                                       (%vm-readtable-host readtable))))
  t)

(defun get-dispatch-macro-character (disp-char sub-char
                                     &optional (readtable *readtable*))
  "Return the dispatch macro function for DISP-CHAR/SUB-CHAR."
  (let ((key (%dispatch-key disp-char sub-char)))
    (if (%vm-readtable-p readtable)
        (or (gethash key (gethash :dispatch-chars readtable))
            (cl:get-dispatch-macro-character (car key) (cdr key)
                                             (gethash :host readtable)))
        (cl:get-dispatch-macro-character (car key) (cdr key) readtable))))

;; FR-622: Package locks
(defvar *vm-package-locks* (make-hash-table :test #'eq))

(defun vm-lock-package (package)
  (setf (gethash package *vm-package-locks*) t) package)

(defun vm-unlock-package (package)
  (remhash package *vm-package-locks*) package)

(defun vm-package-locked-p (package)
  (gethash package *vm-package-locks*))

(define-condition vm-package-locked-error (error)
  ((package :initarg :package :reader vm-package-locked-error-package))
  (:report (lambda (c s) (format s "Package ~S is locked" (vm-package-locked-error-package c)))))
