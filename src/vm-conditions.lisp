(in-package :cl-cc)

;;; ----------------------------------------------------------------------------
;;; VM Condition System
;;; ----------------------------------------------------------------------------
;;;
;;; This module implements a condition system for VM error handling.
;;; It provides:
;;; - Condition classes for various VM errors
;;; - VM instructions for signaling conditions
;;; - Handler stack management
;;; - Restart support
;;;
;;; The design follows Common Lisp condition system patterns but keeps
;;; the implementation minimal for VM use.

;;; ----------------------------------------------------------------------------
;;; VM Condition Classes
;;; ----------------------------------------------------------------------------

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

(define-condition vm-type-error (vm-error)
  ((expected-type :initarg :expected-type :reader vm-expected-type
                  :documentation "The type that was expected.")
   (datum :initarg :datum :reader vm-datum
          :documentation "The actual value that was received."))
  (:documentation "Type mismatch error - raised when a value doesn't match
the expected type.")
  (:report (lambda (condition stream)
             (format stream "VM Type Error: expected ~A, got ~S"
                     (vm-expected-type condition)
                     (vm-datum condition)))))

(define-condition vm-unbound-variable (vm-error)
  ((variable-name :initarg :variable-name :reader vm-variable-name
                  :documentation "The name of the unbound variable."))
  (:documentation "Error raised when accessing an undefined variable.")
  (:report (lambda (condition stream)
             (format stream "VM Unbound Variable: ~S"
                     (vm-variable-name condition)))))

(define-condition vm-undefined-function (vm-error)
  ((function-name :initarg :function-name :reader vm-function-name
                  :documentation "The name of the undefined function."))
  (:documentation "Error raised when calling an undefined function.")
  (:report (lambda (condition stream)
             (format stream "VM Undefined Function: ~S"
                     (vm-function-name condition)))))

(define-condition vm-arg-count-error (vm-error)
  ((function-name :initarg :function-name :reader vm-arg-count-function-name
                  :documentation "The name of the function being called.")
   (expected-count :initarg :expected-count :reader vm-expected-arg-count
                   :documentation "The expected number of arguments.")
   (actual-count :initarg :actual-count :reader vm-actual-arg-count
                 :documentation "The actual number of arguments provided."))
  (:documentation "Error raised when a function is called with the wrong
number of arguments.")
  (:report (lambda (condition stream)
             (format stream "VM Argument Count Error: ~S expects ~A argument~:P, got ~A"
                     (vm-arg-count-function-name condition)
                     (vm-expected-arg-count condition)
                     (vm-actual-arg-count condition)))))

(define-condition vm-control-error (vm-error)
  ((context :initarg :context :reader vm-control-context
            :documentation "Additional context about the control flow error."))
  (:documentation "Error raised for invalid control flow operations, such as
returning from a block that doesn't exist or invalid tagbody jumps.")
  (:report (lambda (condition stream)
             (format stream "VM Control Error: ~A"
                     (vm-control-context condition)))))

(define-condition vm-reader-error (vm-error)
  ((position :initarg :position :reader vm-reader-position
             :documentation "Position in the input where the error occurred.")
   (message :initarg :message :reader vm-reader-message
            :documentation "Description of the reader error."))
  (:documentation "Error raised during parsing/reading of input.")
  (:report (lambda (condition stream)
             (format stream "VM Reader Error at position ~A: ~A"
                     (vm-reader-position condition)
                     (vm-reader-message condition)))))

(define-condition vm-division-by-zero (vm-error)
  ((dividend :initarg :dividend :reader vm-dividend
             :documentation "The value being divided."))
  (:documentation "Error raised when attempting to divide by zero.")
  (:report (lambda (condition stream)
             (format stream "VM Division By Zero: attempted to divide ~S by zero"
                     (vm-dividend condition)))))

;;; ----------------------------------------------------------------------------
;;; VM Handler Stack
;;; ----------------------------------------------------------------------------
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

;;; ----------------------------------------------------------------------------
;;; VM Condition Instructions
;;; ----------------------------------------------------------------------------

(defclass vm-signal (vm-instruction)
  ((condition-reg :initarg :condition-reg :reader vm-condition-reg
                  :documentation "Register containing the condition to signal."))
  (:documentation "Signal a condition. Handlers will be searched and invoked
if a matching handler is found."))

(defclass vm-error-instruction (vm-instruction)
  ((condition-reg :initarg :condition-reg :reader vm-condition-reg
                  :documentation "Register containing the error condition to signal."))
  (:documentation "Signal an error condition. Unlike vm-signal, this will
propagate if no handler is found."))

(defclass vm-cerror (vm-instruction)
  ((continue-message :initarg :continue-message :reader vm-continue-message
                     :documentation "Message for the continue restart.")
   (condition-reg :initarg :condition-reg :reader vm-condition-reg
                  :documentation "Register containing the condition."))
  (:documentation "Signal a correctable error with a continue restart."))

(defclass vm-warn (vm-instruction)
  ((condition-reg :initarg :condition-reg :reader vm-condition-reg
                  :documentation "Register containing the warning to signal."))
  (:documentation "Signal a warning. Warnings don't interrupt execution
unless a handler explicitly handles them."))

;;; ----------------------------------------------------------------------------
;;; VM Handler Management Instructions
;;; ----------------------------------------------------------------------------

(defclass vm-push-handler (vm-instruction)
  ((handler-type :initarg :type :reader vm-push-handler-type
                 :documentation "Condition type this handler handles.")
   (handler-label :initarg :handler-label :reader vm-handler-label
                  :documentation "Label to jump to when handler is invoked.")
   (result-reg :initarg :result-reg :reader vm-handler-result-reg
               :documentation "Register to store condition when handler is called."))
  (:documentation "Push a condition handler onto the handler stack."))

(defclass vm-pop-handler (vm-instruction)
  ()
  (:documentation "Pop the top handler from the handler stack."))

;;; ----------------------------------------------------------------------------
;;; VM Restart Instructions
;;; ----------------------------------------------------------------------------

(defclass vm-bind-restart (vm-instruction)
  ((name :initarg :name :reader vm-restart-name-inst
         :documentation "Name of the restart.")
   (restart-label :initarg :restart-label :reader vm-restart-label
                  :documentation "Label to jump to when restart is invoked."))
  (:documentation "Bind a restart for the current dynamic extent."))

(defclass vm-invoke-restart (vm-instruction)
  ((name :initarg :name :reader vm-invoke-restart-name
         :documentation "Name of the restart to invoke.")
   (value-reg :initarg :value-reg :reader vm-invoke-value-reg
              :initform nil :documentation "Optional register containing value to pass to restart."))
  (:documentation "Invoke a restart by name."))

;;; ----------------------------------------------------------------------------
;;; Instruction -> S-expression Conversion
;;; ----------------------------------------------------------------------------

(defmethod instruction->sexp ((inst vm-signal))
  (list :signal (vm-condition-reg inst)))

(defmethod instruction->sexp ((inst vm-error-instruction))
  (list :vm-error (vm-condition-reg inst)))

(defmethod instruction->sexp ((inst vm-cerror))
  (list :cerror (vm-continue-message inst) (vm-condition-reg inst)))

(defmethod instruction->sexp ((inst vm-warn))
  (list :warn (vm-condition-reg inst)))

(defmethod instruction->sexp ((inst vm-push-handler))
  (list :push-handler
        (vm-push-handler-type inst)
        (vm-handler-label inst)
        (vm-handler-result-reg inst)))

(defmethod instruction->sexp ((inst vm-pop-handler))
  (list :pop-handler))

(defmethod instruction->sexp ((inst vm-bind-restart))
  (list :bind-restart
        (vm-restart-name-inst inst)
        (vm-restart-label inst)))

(defmethod instruction->sexp ((inst vm-invoke-restart))
  (if (vm-invoke-value-reg inst)
      (list :invoke-restart
            (vm-invoke-restart-name inst)
            (vm-invoke-value-reg inst))
      (list :invoke-restart (vm-invoke-restart-name inst))))

;;; ----------------------------------------------------------------------------
;;; S-expression -> Instruction Conversion
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Instruction Execution
;;; ----------------------------------------------------------------------------

(defun vm-signal-condition (condition vm-state &key (error-p nil))
  "Signal a CONDITION in the context of VM-STATE.
If ERROR-P is true, signal an error if no handler is found.
Returns (values handler-found-p handler-info) or signals error."
  (let ((handler (vm-find-handler vm-state condition)))
    (if handler
        (values t handler)
        (if error-p
            (error condition)
            (values nil nil)))))

(defmethod execute-instruction ((inst vm-signal) state pc labels)
  (let* ((condition (vm-reg-get state (vm-condition-reg inst)))
         (handler-found (vm-signal-condition condition state)))
    (if handler-found
        ;; Handler found - transfer control to handler
        (let* ((handler (vm-find-handler state condition))
               (handler-label (vm-handler-label handler))
               (result-reg (vm-handler-result-reg handler)))
          ;; Store condition in result register for handler
          (vm-reg-set state result-reg condition)
          ;; Jump to handler
          (values (gethash handler-label labels) nil nil))
        ;; No handler - continue execution
        (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-error-instruction) state pc labels)
  (let* ((condition (vm-reg-get state (vm-condition-reg inst)))
         (handler-found (vm-signal-condition condition state :error-p t)))
    (if handler-found
        (let* ((handler (vm-find-handler state condition))
               (handler-label (vm-handler-label handler))
               (result-reg (vm-handler-result-reg handler)))
          (vm-reg-set state result-reg condition)
          (values (gethash handler-label labels) nil nil))
        ;; Error was already signaled by vm-signal-condition
        (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-cerror) state pc labels)
  ;; Create a continue restart and signal the condition
  ;; The restart allows continuing after the error
  (let ((condition (vm-reg-get state (vm-condition-reg inst))))
    ;; Add a continue restart
    (vm-add-restart state 'continue
                    (list :label nil
                     :continue-message (vm-continue-message inst)))
    ;; Signal the condition
    (multiple-value-bind (handler-found handler)
        (vm-signal-condition condition state :error-p t)
      (declare (ignore handler-found))
      (when handler
        (let* ((handler-info (vm-handler-handler-fn handler))
               (handler-label (getf handler-info :label))
               (result-reg (getf handler-info :result-reg))
               (new-pc (gethash handler-label labels)))
          (vm-reg-set state result-reg condition)
          (return-from execute-instruction
            (values new-pc nil nil))))
    ;; If no handler, return to next instruction (continue restart)
    (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-warn) state pc labels)
  (declare (ignore labels))
  (let ((condition (vm-reg-get state (vm-condition-reg inst))))
    ;; Warnings don't interrupt execution by default
    ;; But handlers can still process them
    (vm-signal-condition condition state)
    ;; Output warning message if no handler handled it
    (format (vm-output-stream state) "; VM Warning: ~A~%" condition)
    (values (1+ pc) nil nil)))

(defmethod execute-instruction ((inst vm-push-handler) state pc labels)
  (declare (ignore labels pc))
  (vm-push-handler-to-stack state
                             (vm-push-handler-type inst)
                             ;; Store handler info for later lookup
                             (list :label (vm-handler-label inst)
                                   :result-reg (vm-handler-result-reg inst)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-pop-handler) state pc labels)
  (declare (ignore labels pc))
  (vm-pop-handler-from-stack state)
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-bind-restart) state pc labels)
  (declare (ignore labels pc))
  (vm-add-restart state
                  (vm-restart-name-inst inst)
                  (list :label (vm-restart-label inst)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-invoke-restart) state pc labels)
  (let* ((restart-name (vm-invoke-restart-name inst))
         (restart (vm-find-restart state restart-name)))
    (if restart
        (let ((restart-info (vm-restart-restart-fn restart)))
          (if (listp restart-info)
              ;; It's a label-based restart
              (let ((label (getf restart-info :label))
                    (value-reg (vm-invoke-value-reg inst)))
                (when (and value-reg
                           (vm-invoke-value-reg inst))
                  ;; Store value in a special register for the restart
                  (vm-reg-set state :restart-value
                              (vm-reg-get state value-reg)))
                (values (gethash label labels) nil nil))
              ;; It's a function-based restart
              (funcall restart-info)))
        (error 'vm-error
               :vm-state state
               :format-control "No restart named ~S found"
               :format-arguments (list restart-name)))))

;;; ----------------------------------------------------------------------------
;;; Condition Construction Helpers
;;; ----------------------------------------------------------------------------

(defun make-vm-type-error (vm-state expected-type datum)
  "Construct a vm-type-error condition."
  (make-condition 'vm-type-error
                  :vm-state vm-state
                  :expected-type expected-type
                  :datum datum))

(defun make-vm-unbound-variable (vm-state variable-name)
  "Construct a vm-unbound-variable condition."
  (make-condition 'vm-unbound-variable
                  :vm-state vm-state
                  :variable-name variable-name))

(defun make-vm-undefined-function (vm-state function-name)
  "Construct a vm-undefined-function condition."
  (make-condition 'vm-undefined-function
                  :vm-state vm-state
                  :function-name function-name))

(defun make-vm-arg-count-error (vm-state function-name expected actual)
  "Construct a vm-arg-count-error condition."
  (make-condition 'vm-arg-count-error
                  :vm-state vm-state
                  :function-name function-name
                  :expected-count expected
                  :actual-count actual))

(defun make-vm-control-error (vm-state context)
  "Construct a vm-control-error condition."
  (make-condition 'vm-control-error
                  :vm-state vm-state
                  :context context))

(defun make-vm-reader-error (vm-state position message)
  "Construct a vm-reader-error condition."
  (make-condition 'vm-reader-error
                  :vm-state vm-state
                  :position position
                  :message message))

(defun make-vm-division-by-zero (vm-state dividend)
  "Construct a vm-division-by-zero condition."
  (make-condition 'vm-division-by-zero
                  :vm-state vm-state
                  :dividend dividend))

;;; ----------------------------------------------------------------------------
;;; Integration with Existing VM Operations
;;; ----------------------------------------------------------------------------

(defun vm-checked-binop (op lhs rhs vm-state)
  "Perform a binary operation with type checking.
Signals appropriate conditions for errors."
  (case op
    ((add +) (+ lhs rhs))
    ((sub -) (- lhs rhs))
    ((mul *) (* lhs rhs))
    ((div /)
     (if (zerop rhs)
         (error (make-vm-division-by-zero vm-state lhs))
         (/ lhs rhs)))
    (otherwise
     (error (make-vm-type-error vm-state 'binop op)))))

(defun vm-checked-car (cons-cell vm-state)
  "Get car of cons-cell with type checking."
  (if (typep cons-cell 'vm-cons-cell)
      (vm-heap-car cons-cell)
      (error (make-vm-type-error vm-state 'vm-cons-cell cons-cell))))

(defun vm-checked-cdr (cons-cell vm-state)
  "Get cdr of cons-cell with type checking."
  (if (typep cons-cell 'vm-cons-cell)
      (vm-heap-cdr cons-cell)
      (error (make-vm-type-error vm-state 'vm-cons-cell cons-cell))))
