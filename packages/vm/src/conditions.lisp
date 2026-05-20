(in-package :cl-cc/vm)

;;; VM Condition System — Protocol Layer
;;;
;;; Provides condition classes, handler stack management, and restart bindings.
;;; VM instruction definitions, execute-instruction methods, and condition constructors
;;; are in conditions-instructions.lisp (loaded immediately after this file).

;;; ─── VM Condition Classes ────────────────────────────────────────────────────

(define-condition vm-condition (condition)
  ((vm-state :initarg :vm-state :reader vm-condition-state
             :documentation "The VM state when condition was signaled.")
   (error-code :initarg :error-code :initform nil :reader vm-condition-error-code
               :documentation "Machine-readable diagnostic code for this VM condition.")
    (vm-fix-it :initarg :fix-it :initform nil :reader vm-condition-fix-it
               :documentation "Optional structured fix-it suggestion for this VM condition."))
  (:documentation "Base class for all VM conditions."))

(define-condition vm-error (vm-condition error)
  ()
  (:documentation "Base class for VM errors. These are serious conditions that
typically require intervention to continue execution."))

(define-condition vm-warning (vm-condition warning)
  ()
  (:documentation "Base class for VM warnings. These indicate potential issues
but don't interrupt normal execution."))

(define-condition vm-type-error (vm-error type-error)
  ()
  (:documentation "Type mismatch error - raised when a value doesn't match
the expected type.  Inherits from CL's TYPE-ERROR so user code can catch it
via (handler-case ... (type-error (c) ...)).")
  (:report (lambda (condition stream)
             (format stream "VM Type Error: expected ~A, got ~S"
                     (type-error-expected-type condition)
                     (type-error-datum condition)))))

(define-condition vm-unbound-variable (vm-error unbound-variable)
  ()
  (:documentation "Error raised when accessing an undefined variable.
Inherits from CL's UNBOUND-VARIABLE so user code can catch it via
(handler-case ... (unbound-variable (c) ...)).")
  (:report (lambda (condition stream)
             (format stream "VM Unbound Variable: ~S"
                     (cell-error-name condition)))))

(define-condition vm-undefined-function (vm-error undefined-function)
  ()
  (:documentation "Error raised when calling an undefined function.
Inherits from CL's UNDEFINED-FUNCTION so user code can catch it via
(handler-case ... (undefined-function (c) ...)).")
  (:report (lambda (condition stream)
             (format stream "VM Undefined Function: ~S"
                     (cell-error-name condition)))))

(define-condition vm-division-by-zero (vm-error division-by-zero)
  ((dividend :initarg :dividend :reader vm-dividend
             :documentation "The value being divided (extra context beyond ANSI operands)."))
  (:documentation "Error raised when attempting to divide by zero.
Inherits from CL's DIVISION-BY-ZERO so user code can catch it via
(handler-case ... (division-by-zero (c) ...)).")
  (:report (lambda (condition stream)
             (format stream "VM Division By Zero: attempted to divide ~S by zero"
                     (vm-dividend condition)))))


;;; ─── VM Handler Stack ────────────────────────────────────────────────────────
;;;
;;; Since we cannot modify vm-state, we use a hash table to associate
;;; handler stacks with VM states. This is managed by the VM condition
;;; instructions.

(defvar *vm-handler-stacks* (make-hash-table :test #'eq :weakness :key)
  "Hash table mapping VM states to their handler stacks.
Uses weak keys so handlers are GC'd when the VM state is collected.")

(defvar *vm-restart-bindings* (make-hash-table :test #'eq :weakness :key)
  "Hash table mapping VM states to their restart bindings.
Uses weak keys for the same reason as *vm-handler-stacks*.")

;;; Structure representing a condition handler in the VM.
;;; TYPE - Condition type this handler matches.
;;; HANDLER-FN - Function to call when condition is signaled.
(defstruct (vm-handler (:constructor make-vm-handler (type handler-fn)))
  type
  handler-fn)

;;; Structure representing a restart in the VM.
;;; NAME - Name of the restart.
;;; RESTART-FN - Function to invoke the restart.
(defstruct (vm-restart (:constructor make-vm-restart (name restart-fn)))
  name
  restart-fn)

(defun vm-get-handler-stack (vm-state)
  "Get the handler stack for VM-STATE, creating one if necessary."
  (or (gethash vm-state *vm-handler-stacks*)
      (setf (gethash vm-state *vm-handler-stacks*) nil)))

(defun vm-push-handler-to-stack (vm-state type handler-fn)
  "Push a new handler onto the handler stack for VM-STATE."
  (push (make-vm-handler type handler-fn)
        (gethash vm-state *vm-handler-stacks*)))

(defun vm-pop-handler-from-stack (vm-state)
  "Pop the top handler from the handler stack for VM-STATE.
Returns the popped handler or NIL if stack is empty."
  (let ((stack (vm-get-handler-stack vm-state)))
    (when stack
      (let ((handler (pop stack)))
        (setf (gethash vm-state *vm-handler-stacks*) stack)
        handler))))

(defun vm-find-handler (vm-state condition)
  "Find a handler for CONDITION in VM-STATE's handler stack.
Returns the first matching handler or NIL if none found."
  (let ((stack (vm-get-handler-stack vm-state)))
    (find-if (lambda (handler)
               (typep condition (vm-handler-type handler)))
             stack)))

(defun vm-get-restarts (vm-state)
  "Get the available restarts for VM-STATE."
  (gethash vm-state *vm-restart-bindings*))

(defun vm-add-restart (vm-state name restart-fn)
  "Add a restart binding for VM-STATE."
  (push (make-vm-restart name restart-fn)
        (gethash vm-state *vm-restart-bindings*)))

(defun vm-find-restart (vm-state name)
  "Find a restart by name in VM-STATE's restart bindings."
  (let ((restarts (vm-get-restarts vm-state)))
    (find name restarts :key #'vm-restart-name :test #'eq)))

(defun vm-clear-condition-context (vm-state)
  "Clear handler stack and restarts for VM-STATE (cleanup function)."
  (remhash vm-state *vm-handler-stacks*)
  (remhash vm-state *vm-restart-bindings*))

;;; ─── Zero-Cost Exception Tables ─────────────────────────────────────────────
;;;
;;; FR-138 stores handler metadata outside the hot instruction stream.  The
;;; compiler registers a vector of PC ranges for each VM program; label-table
;;; construction then associates the freshly built label table with that vector
;;; so condition signaling can find a handler from the current PC without
;;; executing establish/remove instructions on protected-form entry/exit.

(defstruct (vm-exception-entry
            (:constructor make-vm-exception-entry
                (start-pc end-pc handler-pc condition-type result-reg order)))
  start-pc
  end-pc
  handler-pc
  condition-type
  result-reg
  order)

(defvar *vm-program-exception-tables* (make-hash-table :test #'eq :weakness :key)
  "Weak map from VM program objects to their zero-cost exception table.")

(defvar *vm-instruction-exception-tables* (make-hash-table :test #'eq :weakness :key)
  "Weak map from VM program instruction lists to their exception table.")

(defvar *vm-label-exception-tables* (make-hash-table :test #'eq :weakness :key)
  "Weak map from per-run label tables to their exception table.")

(defun vm-register-program-exception-table (program exception-table)
  "Attach EXCEPTION-TABLE to PROGRAM and its instruction list.

EXCEPTION-TABLE is a vector of VM-EXCEPTION-ENTRY records with concrete PCs.
An empty table is ignored so programs without handlers keep the old shape."
  (let ((table (and exception-table
                    (if (vectorp exception-table)
                        exception-table
                        (coerce exception-table 'vector)))))
    (when (and program table (plusp (length table)))
      (setf (gethash program *vm-program-exception-tables*) table)
      (setf (gethash (vm-program-instructions program) *vm-instruction-exception-tables*) table))
    program))

(defun %vm-exception-table-for-labels (labels)
  "Return the exception table associated with LABELS, if any."
  (and labels (gethash labels *vm-label-exception-tables*)))

(defun %vm-exception-entry-active-p (entry pc condition)
  "Return true when ENTRY covers PC and accepts CONDITION."
  (and (<= (vm-exception-entry-start-pc entry) pc)
       (< pc (vm-exception-entry-end-pc entry))
       (vm-error-type-matches-p condition (vm-exception-entry-condition-type entry))))

(defun %vm-exception-entry-span (entry)
  (- (vm-exception-entry-end-pc entry)
     (vm-exception-entry-start-pc entry)))

(defun vm-find-exception-table-entry (labels pc condition)
  "Find the innermost exception-table handler for CONDITION at PC.

For identical ranges, preserve source clause order.  For nested protected
forms, prefer the smallest covering range."
  (let ((best nil))
    (let ((table (%vm-exception-table-for-labels labels)))
      (when table
        (loop for entry across table
              when (%vm-exception-entry-active-p entry pc condition)
                do (when (or (null best)
                             (< (%vm-exception-entry-span entry)
                                (%vm-exception-entry-span best))
                             (and (= (%vm-exception-entry-span entry)
                                     (%vm-exception-entry-span best))
                                  (< (vm-exception-entry-order entry)
                                     (vm-exception-entry-order best))))
                     (setf best entry)))))
    best))

(defun %vm-jump-to-exception-entry (state entry condition)
  "Store CONDITION for ENTRY's handler and jump to its handler PC."
  (vm-reg-set state (vm-exception-entry-result-reg entry) condition)
  (values (vm-exception-entry-handler-pc entry) nil nil))

(defun %vm-restore-frame-for-exception-propagation (state frame)
  "Restore one caller frame while propagating an exception-table lookup."
  (destructuring-bind (return-pc _dst-reg old-closure-env saved-regs) frame
    (declare (ignore _dst-reg))
    (vm-restore-registers state saved-regs)
    (when old-closure-env
      (setf (vm-closure-env state) old-closure-env))
    (when (vm-method-call-stack state)
      (pop (vm-method-call-stack state)))
    return-pc))

(defun vm-dispatch-exception-table (state labels pc condition &key error-p)
  "Resolve CONDITION via the PC→handler side table, unwinding callers as needed.

Returns normal EXECUTE-INSTRUCTION values when a handler is found.  If no entry
matches in the current frame, restore one call frame and retry at its return PC;
this models dynamic propagation without a runtime handler stack."
  (if (%vm-exception-table-for-labels labels)
      (loop with probe-pc = pc
            do (let ((entry (vm-find-exception-table-entry labels probe-pc condition)))
                 (when entry
                   (return (%vm-jump-to-exception-entry state entry condition)))
                 (if (vm-call-stack state)
                     (setf probe-pc
                           (%vm-restore-frame-for-exception-propagation
                            state (pop (vm-call-stack state))))
                     (progn
                       (when error-p
                         (vm-print-backtrace state :labels labels)
                         (if (typep condition 'condition)
                             (error condition)
                             (error "Unhandled error in VM: ~S" condition)))
                       (return (values nil nil nil))))))
      (values nil nil nil)))

(defun build-label-table (instructions)
  "Build an integer-keyed label table and associate any exception side table."
  (let ((labels (make-hash-table :test #'eql)))
    (loop for inst in instructions
          for pc from 0
          do (when (typep inst 'vm-label)
               (vm-label-table-store labels (vm-name inst) pc)))
    (let ((exception-table (gethash instructions *vm-instruction-exception-tables*)))
      (when exception-table
        (setf (gethash labels *vm-label-exception-tables*) exception-table)))
    labels))

(defmethod execute-instruction ((inst vm-signal-error) state pc labels)
  "Signal an error using the zero-cost exception table before legacy stacks."
  (let ((error-value (vm-reg-get state (vm-error-reg inst))))
    (multiple-value-bind (next-pc handled-p value)
        (vm-dispatch-exception-table state labels pc error-value :error-p nil)
      (declare (ignore value))
      (if next-pc
          (values next-pc handled-p nil)
          (let ((matching-handler nil)
                (handlers-to-skip 0))
            (dolist (entry (vm-handler-stack state))
              (if (vm-error-type-matches-p error-value (third entry))
                  (progn (setf matching-handler entry) (return))
                  (incf handlers-to-skip)))
            (if matching-handler
                (progn
                  (dotimes (i (1+ handlers-to-skip)) (pop (vm-handler-stack state)))
                  (destructuring-bind (handler-label result-reg _type saved-call-stack saved-regs
                                       &optional saved-method-call-stack)
                      matching-handler
                    (declare (ignore _type))
                    (%vm-unwind-to-handler state labels handler-label result-reg
                                           saved-call-stack saved-regs saved-method-call-stack
                                           error-value)))
                (progn
                  (vm-print-backtrace state :labels labels)
                  (if (typep error-value 'condition)
                      (error error-value)
                      (error "Unhandled error in VM: ~S" error-value)))))))))
