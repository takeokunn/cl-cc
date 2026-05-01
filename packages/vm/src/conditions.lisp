(in-package :cl-cc/vm)

;;; VM Condition System — Protocol Layer
;;;
;;; Provides condition classes, handler stack management, and restart bindings.
;;; VM instruction definitions, execute-instruction methods, and condition constructors
;;; are in conditions-instructions.lisp (loaded immediately after this file).

;;; ─── VM Condition Classes ────────────────────────────────────────────────────

(define-condition vm-condition (condition)
  ((vm-state :initarg :vm-state :reader vm-condition-state
             :documentation "The VM state when condition was signaled."))
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
