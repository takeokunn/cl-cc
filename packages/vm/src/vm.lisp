(in-package :cl-cc/vm)

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
                     :documentation "Captured lexical environment values as a vector of (register . value) pairs")
   (program-flat :initarg :program-flat :initform nil :accessor vm-closure-program-flat
                 :documentation "Optional flat instruction vector that owns this closure's entry label.")
   (label-table :initarg :label-table :initform nil :accessor vm-closure-label-table
                :documentation "Optional label table paired with PROGRAM-FLAT for cross-program calls."))
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
       (flet ((struct-acc (slot)
                (intern (format nil "~A~A" prefix (symbol-name slot))))
              (ctor ()
                (intern (format nil "MAKE-~A" (symbol-name name))))
              (pos-form (n)
                (case n
                  (1 '(second sexp)) (2 '(third sexp)) (3 '(fourth sexp))
                  (4 '(fifth sexp)) (5 '(sixth sexp)) (6 '(seventh sexp))
                  (t `(nth ,n sexp)))))
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
                            (loop for s in sexp-slots
                                  for i from 1
                                  append (list (intern (symbol-name s) :keyword)
                                               (pos-form i)))))
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
                                      (list (cons (ctor) ctor-args)))))))))
           (cons 'progn
                 (append (list struct-form)
                         reader-forms
                         sexp-forms)))))))

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
