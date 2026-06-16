;;;; vm-dsl.lisp - VM Instruction Shorthand Macros and State Definition
;;;;
;;; Depends on vm.lisp being loaded first (vm-instruction base struct,
;;; define-vm-instruction macro, vm-heap-object, vm-closure-object, etc.).

(in-package :cl-cc/vm)

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
    (let ((value-form
            (ecase shape
              (:unary
               (list cl-func (list 'vm-reg-get state (list src inst))))
              (:binary
               (list cl-func
                     (list 'vm-reg-get state (list lhs inst))
                     (list 'vm-reg-get state (list rhs inst))))
              (:pred1
               (list 'if
                     (list cl-func (list 'vm-reg-get state (list src inst)))
                     1
                     0))
              (:pred2
               (list 'if
                     (list cl-func
                           (list 'vm-reg-get state (list lhs inst))
                           (list 'vm-reg-get state (list rhs inst)))
                     1
                     0)))))
      (list 'defmethod 'execute-instruction
            (list (list inst name) state pc 'labels)
            (list 'declare (list 'ignore 'labels))
            (list 'vm-reg-set state (list dst inst) value-form)
            (list 'values (list '1+ pc) nil nil)))))

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
  (leaf-p nil :type boolean)
  (calling-convention :external :type keyword)
  (function-conventions nil)
  (deopt-info nil)
  (osr-entry-points nil)
  (tier1-entry-points nil)
  (load-time-value-cells nil)
  (compilation-tier 0 :type integer))

(defclass vm-state ()
   ((registers :initform (make-hash-table :test #'equal) :reader vm-state-registers)
   (output-stream :initarg :output-stream :reader vm-output-stream)
   (call-stack :initform nil :accessor vm-call-stack
               :documentation "Stack of (return-pc . saved-env) for function calls")
   (closure-env :initform nil :accessor vm-closure-env
                :documentation "Current closure's captured environment")
   (current-closure-stack :initform nil :accessor vm-current-closure-stack
                          :documentation "Stack of (call-depth . closure) entries for currently executing closures.")
   (heap :initform (make-hash-table :test #'eql) :reader vm-state-heap
         :documentation "Heap storage for allocated objects keyed by address")
   (heap-counter :initform 0 :accessor vm-heap-counter
                 :documentation "Counter for allocating heap addresses")
   (class-registry :initform (make-hash-table :test #'eq) :reader vm-class-registry
                   :documentation "Global registry mapping class names to class descriptor HTs")
    (values-list :initform nil :accessor vm-values-list
                 :documentation "List of multiple return values from last VALUES call")
    (mv-buffer :initform (make-array +maximum-multiple-values+ :initial-element nil)
               :accessor vm-mv-buffer
               :documentation "Fixed-size multiple-values buffer for allocation-free MV transfer.")
    (mv-count :initform 0 :accessor vm-mv-count
              :documentation "Number of valid entries in MV-BUFFER.")
    (load-time-values :initform (make-hash-table :test #'eql)
                      :reader vm-load-time-values
                      :documentation "Load-time-value cell id -> resolved value table.")
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
                   :documentation "User-visible symbol property storage keyed by symbol.
Entries stay as short linear plists until they exceed the promotion threshold,
after which they are stored in an internal hashed representation.")
    (system-symbol-plists :initform (make-hash-table :test #'eq)
                          :reader vm-system-symbol-plists
                          :documentation "Dedicated VM-only system property storage keyed by symbol.
Used for runtime/compiler metadata such as :function and :type without exposing
those properties through user-visible symbol-plist operations.")
    (symbol-plist-lock :initform (cl-cc/runtime:rt-make-lock "cl-cc/vm symbol plist lock")
                       :reader vm-symbol-plist-lock
                       :documentation "Mutex protecting atomic access to VM symbol plist tables.")
    (symbol-plist-read-barrier :initform 0 :accessor vm-symbol-plist-read-barrier
                               :documentation "Monotonic barrier incremented on VM symbol property writes.")
    (method-call-stack :initform nil :accessor vm-method-call-stack
                        :documentation "Parallel stack to call-stack tracking CLOS method context.
Each frame is either NIL (regular call) or (gf-ht methods-list all-args) for generic dispatch.
Used by call-next-method and next-method-p.")
   (profile-enabled-p :initform nil :accessor vm-profile-enabled-p
                      :documentation "T when lightweight VM stack sampling is enabled.")
    (profile-call-stack :initform nil :accessor vm-profile-call-stack
                        :documentation "Current sampled call stack as a list of function labels, leaf first.")
    (profile-call-start-times :initform nil :accessor vm-profile-call-start-times
                              :documentation "Current sampled call start times in nanoseconds, leaf first.")
   (profile-samples :initform (make-hash-table :test #'equal) :accessor vm-profile-samples
                    :documentation "Collapsed stack string -> sample count for lightweight flamegraphs.")
     (profile-bb-counts :initform (make-hash-table :test #'eql) :accessor vm-profile-bb-counts
                        :documentation "Program-counter/basic-block hit counts collected during execution.")
     (profile-inst-counts :initform (make-hash-table :test #'eq) :accessor vm-profile-inst-counts
                          :documentation "VM instruction type frequency counters collected during execution.")
     (profile-branch-counts :initform (make-hash-table :test #'equal) :accessor vm-profile-branch-counts
                            :documentation "Branch edge counters keyed by (kind from-pc to-pc).")
    (profile-call-counts :initform (make-hash-table :test #'equal) :accessor vm-profile-call-counts
                          :documentation "Function label -> dynamic call count collected for PGO.")
     (profile-call-times :initform (make-hash-table :test #'equal) :accessor vm-profile-call-times
                         :documentation "Function label -> cumulative elapsed time in nanoseconds collected for PGO.")
     (profile-type-feedback :initform (make-hash-table :test #'equal) :accessor vm-profile-type-feedback
                            :documentation "Register/type -> observation count collected for PGO.")
     (current-deopt-frame :initform nil :accessor vm-current-deopt-frame
                          :documentation "Most recent deoptimization snapshot.")
     (deopt-history :initform nil :accessor vm-deopt-history
                    :documentation "List of deoptimization snapshots, newest first.")
      (tier1-code :initform (make-hash-table :test #'equal) :accessor vm-tier1-code
                  :documentation "OSR id/label -> simple Tier-1 entry metadata used by the OSR stub.")
      (current-stack-segment :initform nil :accessor vm-current-stack-segment
                             :documentation "Current logical VM stack segment for segmented-stack growth.")
      (continuation-prompts :initform nil :accessor vm-continuation-prompts
                            :documentation "Stack of active delimited-continuation prompt frames.")
      (stack-depth :initform 0 :accessor vm-stack-depth
                   :documentation "O(1) logical VM call-stack depth counter.")
     (tail-call-optimization-enabled-p :initform t :accessor vm-tail-call-optimization-enabled-p
                                       :documentation "When true, vm-tail-call reuses the current frame.")
     (clos-shadow-stack :initform nil :accessor vm-clos-shadow-stack
                        :documentation "Shadow stack for CLOS call-next-method/method-combination chains."))
  (:documentation "VM execution state with registers, call stack, and heap."))

;;; VM state initialization, profiling, heap ops, and execute-instruction generic
;;; are in vm-state-init.lisp (loaded immediately after vm.lisp and this file).
;;;
;;; vm-bridge.lisp  — host function bridge + CLOS slot-definition helpers
;;; vm-execute.lisp — execute-instruction methods for core instructions
;;; vm-clos.lisp    — CLOS instruction defstructs + execute-instruction methods
;;; vm-run.lisp     — Handler-case, label table, run-vm, vm2-state
