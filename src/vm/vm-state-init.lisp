(in-package :cl-cc/vm)

;;; VM State Initialization and Runtime Helpers
;;;
;;; Profiling support, *vm-initial-globals* data table, initialize-instance,
;;; heap operations, argument slots, and the execute-instruction generic function.
;;;
;;; Depends on vm.lisp (vm-state class, vm-reg-set/get).
;;; Loaded immediately after vm.lisp and before vm-bridge.lisp.

;;; ─── VM Profiling ────────────────────────────────────────────────────────────

(defun vm-profile-enter-call (state label &key tail-p)
  "Record entry into LABEL for lightweight sampling.

When TAIL-P is true, replace the current leaf frame instead of pushing a new one."
  (when (and (vm-profile-enabled-p state) label)
    (if tail-p
        (if (vm-profile-call-stack state)
            (setf (first (vm-profile-call-stack state)) label)
            (push label (vm-profile-call-stack state)))
        (push label (vm-profile-call-stack state)))))

(defun vm-profile-return (state)
  "Record return from the current sampled function frame."
  (when (and (vm-profile-enabled-p state)
             (vm-profile-call-stack state)
             (> (length (vm-profile-call-stack state)) 1))
    (pop (vm-profile-call-stack state))))

(defun vm-profile-sample (state)
  "Take one lightweight stack sample for STATE."
  (when (vm-profile-enabled-p state)
    (let* ((stack (or (reverse (vm-profile-call-stack state))
                      (list "<toplevel>")))
           (key (format nil "~{~A~^;~}" stack)))
      (incf (gethash key (vm-profile-samples state) 0)))))

;;; ─── VM State Initialization ─────────────────────────────────────────────────

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

;;; ─── VM Heap Operations ──────────────────────────────────────────────────────

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

(defun vm-build-list (state values &key stack-allocate-p)
  "Build a native CL list from VALUES for use as a &rest parameter.
Uses native cons cells (same as vm-cons instruction)."
  (declare (ignore state)
           (dynamic-extent values))
  (if stack-allocate-p
      values
      (copy-list values)))

;;; ─── Argument Slot Helpers ───────────────────────────────────────────────────

(defparameter +vm-arg-slot-count+ 8
  "Number of reserved VM argument slots exposed by the helper API.")

(defun vm-arg-slot-name (index)
  "Return the reserved argument slot keyword for INDEX.

Valid indices are 0 through +VM-ARG-SLOT-COUNT+-1."
  (check-type index (integer 0 7))
  (intern (format nil "ARG~D" index) :keyword))

(defun vm-bind-arg-slots (state args)
  "Bind ARGS into reserved :ARG0.. slots on STATE and return the bound slot list."
  (loop for arg in args
        for index from 0 below +vm-arg-slot-count+
        for slot = (vm-arg-slot-name index)
        do (vm-reg-set state slot arg)
        collect slot))

(defun vm-heap-address (object)
  "Get heap address from an object. For cons cells and closures."
  (etypecase object
    (integer object)
    (vm-heap-address (vm-heap-address-value object))
    (null nil)))

;;; ─── Execute-instruction Generic + Utilities ─────────────────────────────────

(defgeneric execute-instruction (instruction state pc labels))

(defun vm-generic-function-p (value)
  "Return T if VALUE is a generic function dispatch table (hash table with :__methods__)."
  (and (hash-table-p value)
       (gethash :__methods__ value)
       t))

;;; Host function bridge and CLOS slot-definition helpers live in vm-bridge.lisp
;;; (loaded immediately after this file in the ASDF module sequence).
