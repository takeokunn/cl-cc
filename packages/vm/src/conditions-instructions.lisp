(in-package :cl-cc/vm)

;;; Condition definition macros (FR-643) — thin wrappers around CL primitives
(defmacro vm-define-condition (name supers &rest options)
  `(cl:define-condition ,name ,supers ,@options))

(defmacro vm-handler-case (form &rest clauses)
  `(cl:handler-case ,form ,@clauses))

(defmacro vm-handler-bind (bindings &body body)
  `(cl:handler-bind ,bindings ,@body))

(defmacro vm-restart-case (form &rest clauses)
  `(cl:restart-case ,form ,@clauses))

(defmacro vm-ignore-errors (&body body)
  `(cl:ignore-errors ,@body))

;;; VM Condition Instructions and Execution
;;;
;;; Defines vm-signal, vm-error-instruction, vm-cerror, vm-warn, vm-push-handler,
;;; vm-pop-handler, vm-bind-restart, vm-invoke-restart instruction defstructs,
;;; their execute-instruction methods, and condition construction helpers.
;;;
;;; Depends on conditions.lisp (condition classes, vm-handler-stack ops,
;;; vm-restart ops, %vm-jump-to-handler, vm-signal-condition).

(defun vm-abort-restart ()
  (invoke-restart 'abort))

(defun vm-muffle-warning-restart ()
  (invoke-restart 'muffle-warning))

(defun vm-use-value-restart (value)
  (invoke-restart 'use-value value))

(defun vm-store-value-restart (value)
  (invoke-restart 'store-value value))

(defun vm-retry-restart ()
  "Invoke the active RETRY restart."
  (invoke-restart 'retry))

(defun retry (&optional condition)
  "Invoke the active RETRY restart, if one is available."
  (declare (ignore condition))
  (let ((restart (find-restart 'retry)))
    (if restart
        (invoke-restart restart)
        (error "RETRY invoked but no RETRY restart is active"))))

(defun vm-compute-restarts (&optional condition vm-state)
  "Return VM and host restarts active for CONDITION.

When VM-STATE is supplied, VM label/function restarts are returned first and
host Common Lisp restarts are appended for interoperability."
  (append (and vm-state (vm-get-restarts vm-state))
          (compute-restarts condition)))

(defun vm-invoke-restart (restart &rest values)
  "Invoke RESTART, accepting a VM restart record, a host restart, or a name."
  (cond ((typep restart 'vm-restart)
         (let ((fn (vm-restart-restart-fn restart)))
           (if (functionp fn)
               (apply fn values)
               (error "VM restart ~S is label-based and cannot be invoked as a host function"
                      (vm-restart-name restart)))))
        ((ignore-errors (restart-name restart))
         (apply #'invoke-restart restart values))
        (t
         (apply #'invoke-restart restart values))))

(defun vm-find-standard-restart (name &optional condition vm-state)
  "Find restart NAME in VM-STATE's restart table, then host active restarts."
  (or (and vm-state (vm-find-restart vm-state name))
      (find-restart name condition)))

(defun vm-invoke-debugger (condition &optional vm-state labels)
  "Print a VM backtrace when available, then enter the host debugger."
  (when vm-state
    (vm-print-backtrace vm-state :labels labels))
  (invoke-debugger condition))

(defun vm-break (format-control &rest format-arguments)
  "Signal a VM break using FORMAT-CONTROL and FORMAT-ARGUMENTS."
  (apply #'break format-control format-arguments))

(defun %vm-break-on-signals-matches-p (condition)
  "Return true when CL:*BREAK-ON-SIGNALS* requests debugger entry."
  (let ((spec *break-on-signals*))
    (and spec
         (or (eq spec t)
             (ignore-errors (typep condition spec))))))

(defun %vm-maybe-break-on-signal (condition vm-state labels)
  "Enter the debugger for CONDITION when *BREAK-ON-SIGNALS* matches."
  (when (%vm-break-on-signals-matches-p condition)
    (vm-invoke-debugger condition vm-state labels)))

;;; ─── VM Condition Instructions ───────────────────────────────────────────────

(define-vm-instruction vm-signal (vm-instruction)
  "Signal a condition. Handlers will be searched and invoked
if a matching handler is found."
  (condition-reg nil :reader vm-condition-reg)
  (:sexp-tag :signal)
  (:sexp-slots condition-reg))

(define-vm-instruction vm-error-instruction (vm-instruction)
  "Signal an error condition. Unlike vm-signal, this will
propagate if no handler is found."
  (condition-reg nil :reader vm-condition-reg)
  (:sexp-tag :vm-error)
  (:sexp-slots condition-reg))

(define-vm-instruction vm-cerror (vm-instruction)
  "Signal a correctable error with a continue restart."
  (continue-message nil :reader vm-continue-message)
  (condition-reg nil :reader vm-condition-reg)
  (:sexp-tag :cerror)
  (:sexp-slots continue-message condition-reg))

(define-vm-instruction vm-warn (vm-instruction)
  "Signal a warning. Warnings don't interrupt execution
unless a handler explicitly handles them."
  (condition-reg nil :reader vm-condition-reg)
  (:sexp-tag :warn)
  (:sexp-slots condition-reg))


(define-vm-instruction vm-type-error-condition (vm-instruction)
  "Construct a vm-type-error condition in DST."
  (dst nil :reader vm-dst)
  (expected-type nil :reader vm-expected-type)
  (datum-reg nil :reader vm-datum-reg)
  (values-p nil :reader vm-type-error-values-p)
  (:sexp-tag :type-error-condition)
  (:sexp-slots dst expected-type datum-reg values-p))

;;; ─── VM Handler Management Instructions ─────────────────────────────────────

(define-vm-instruction vm-push-handler (vm-instruction)
  "Push a condition handler onto the handler stack."
  (handler-type nil :reader vm-push-handler-type)
  (handler-label nil :reader vm-handler-label)
  (result-reg nil :reader vm-handler-result-reg)
  (:sexp-tag :push-handler)
  (:sexp-slots handler-type handler-label result-reg))

(define-vm-instruction vm-pop-handler (vm-instruction)
  "Pop the top handler from the handler stack."
  (:sexp-tag :pop-handler))

;;; ─── VM Restart Instructions ─────────────────────────────────────────────────

(define-vm-instruction vm-bind-restart (vm-instruction)
  "Bind a restart for the current dynamic extent."
  (name nil :reader vm-restart-name-inst)
  (restart-label nil :reader vm-restart-label)
  (:sexp-tag :bind-restart)
  (:sexp-slots name restart-label))

(define-vm-instruction vm-invoke-restart (vm-instruction)
  "Invoke a restart by name."
  (name nil :reader vm-invoke-restart-name)
  (value-reg nil :reader vm-invoke-value-reg))

;;; vm-invoke-restart has conditional sexp logic, so handle manually.
(defmethod instruction->sexp ((inst vm-invoke-restart))
  (if (vm-invoke-value-reg inst)
      (list :invoke-restart
            (vm-invoke-restart-name inst)
            (vm-invoke-value-reg inst))
      (list :invoke-restart (vm-invoke-restart-name inst))))

(setf (gethash :invoke-restart *instruction-constructors*)
      (lambda (sexp)
        (make-vm-invoke-restart :name (second sexp)
                                :value-reg (third sexp))))

;;; ─── Instruction Execution ───────────────────────────────────────────────────

(defun %vm-jump-to-handler (state labels condition handler)
  "Transfer control to HANDLER, storing CONDITION in the handler's result register."
  (let* ((info       (vm-handler-handler-fn handler))
         (label      (getf info :label))
         (result-reg (getf info :result-reg)))
    (vm-reg-set state result-reg condition)
    (values (vm-label-table-lookup labels label) nil nil)))

(defun vm-signal-condition (condition vm-state &key (error-p nil))
  "Signal a CONDITION in the context of VM-STATE.
If ERROR-P is true, signal an error if no handler is found.
Returns (values handler-found-p handler-info) or signals error."
  (%vm-maybe-break-on-signal condition vm-state nil)
  (let ((handler (vm-find-handler vm-state condition)))
    (if handler
        (values t handler)
        (if error-p
            (error condition)
            (values nil nil)))))

(defmethod execute-instruction ((inst vm-signal) state pc labels)
  (let* ((condition (vm-reg-get state (vm-condition-reg inst)))
         (handler   (vm-find-handler state condition)))
    (%vm-maybe-break-on-signal condition state labels)
    (if handler
        (%vm-jump-to-handler state labels condition handler)
        ;; No handler - continue execution
        (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-error-instruction) state pc labels)
  (let* ((condition (vm-reg-get state (vm-condition-reg inst)))
         (handler   (vm-find-handler state condition)))
    (%vm-maybe-break-on-signal condition state labels)
    (if handler
        (%vm-jump-to-handler state labels condition handler)
        (progn (error condition) (values (1+ pc) nil nil)))))

(defmethod execute-instruction ((inst vm-cerror) state pc labels)
  ;; Create a continue restart and signal the condition
  ;; The restart allows continuing after the error
  (let* ((condition (vm-reg-get state (vm-condition-reg inst)))
         (handler   (vm-find-handler state condition)))
    (%vm-maybe-break-on-signal condition state labels)
    ;; Add a continue restart
    (vm-add-restart state 'continue
                    (list :label nil
                          :continue-message (vm-continue-message inst)))
    ;; Signal the condition
    (if handler
        (%vm-jump-to-handler state labels condition handler)
        ;; If no handler, return to next instruction (continue restart)
        (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-warn) state pc labels)
  (let ((condition (vm-reg-get state (vm-condition-reg inst))))
    (%vm-maybe-break-on-signal condition state labels)
    ;; Warnings don't interrupt execution by default
    ;; But handlers can still process them
    (vm-signal-condition condition state)
    ;; Output warning message if no handler handled it
    (format (vm-output-stream state) "; VM Warning: ~A~%" condition)
    (values (1+ pc) nil nil)))


(defmethod execute-instruction ((inst vm-type-error-condition) state pc labels)
  (declare (ignore labels))
  (let* ((datum (if (vm-type-error-values-p inst)
                    (or (vm-values-list state)
                        (list (vm-reg-get state (vm-datum-reg inst))))
                    (vm-reg-get state (vm-datum-reg inst))))
         (condition (make-vm-type-error state (vm-expected-type inst) datum)))
    (vm-reg-set state (vm-dst inst) condition)
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
        (error 'vm-simple-error
                :vm-state state
                :format-control "No restart named ~S found"
                :format-arguments (list restart-name)))))

;;; ─── Condition Construction Helpers ──────────────────────────────────────────

(defun make-vm-type-error (vm-state expected-type datum
                           &key error-code ((:fix-it vm-fix-it)) source-location source-text suggestions)
  "Construct a vm-type-error condition."
  (make-condition 'vm-type-error
                  :vm-state vm-state
                   :error-code error-code
                   :fix-it vm-fix-it
                   :source-location source-location
                   :source-text source-text
                   :suggestions suggestions
                    :expected-type expected-type
                    :datum datum))

(defun make-vm-simple-error (vm-state format-control format-arguments
                             &key error-code ((:fix-it vm-fix-it)) source-location source-text suggestions)
  "Construct a vm-simple-error condition."
  (make-condition 'vm-simple-error
                  :vm-state vm-state
                   :error-code error-code
                   :fix-it vm-fix-it
                   :source-location source-location
                   :source-text source-text
                   :suggestions suggestions
                   :format-control format-control
                   :format-arguments format-arguments))

(defun make-vm-simple-warning (vm-state format-control format-arguments
                               &key error-code ((:fix-it vm-fix-it)) source-location source-text suggestions)
  "Construct a vm-simple-warning condition."
  (make-condition 'vm-simple-warning
                  :vm-state vm-state
                   :error-code error-code
                   :fix-it vm-fix-it
                   :source-location source-location
                   :source-text source-text
                   :suggestions suggestions
                   :format-control format-control
                   :format-arguments format-arguments))

(defun make-vm-unbound-variable (vm-state variable-name
                                 &key error-code ((:fix-it vm-fix-it)) source-location source-text suggestions)
  "Construct a vm-unbound-variable condition."
  (make-condition 'vm-unbound-variable
                  :vm-state vm-state
                   :error-code error-code
                   :fix-it vm-fix-it
                   :source-location source-location
                   :source-text source-text
                   :suggestions suggestions
                   :name variable-name))

(defun make-vm-undefined-function (vm-state function-name
                                   &key error-code ((:fix-it vm-fix-it)) source-location source-text suggestions)
  "Construct a vm-undefined-function condition."
  (make-condition 'vm-undefined-function
                  :vm-state vm-state
                   :error-code error-code
                   :fix-it vm-fix-it
                   :source-location source-location
                   :source-text source-text
                   :suggestions suggestions
                   :name function-name))

(defun make-vm-division-by-zero (vm-state dividend
                                 &key error-code ((:fix-it vm-fix-it)) source-location source-text suggestions)
  "Construct a vm-division-by-zero condition."
  (make-condition 'vm-division-by-zero
                  :vm-state vm-state
                   :error-code error-code
                   :fix-it vm-fix-it
                   :source-location source-location
                   :source-text source-text
                   :suggestions suggestions
                   :dividend dividend))
