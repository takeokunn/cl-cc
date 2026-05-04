(in-package :cl-cc/vm)

;;; VM Condition Instructions and Execution
;;;
;;; Defines vm-signal, vm-error-instruction, vm-cerror, vm-warn, vm-push-handler,
;;; vm-pop-handler, vm-bind-restart, vm-invoke-restart instruction defstructs,
;;; their execute-instruction methods, and condition construction helpers.
;;;
;;; Depends on conditions.lisp (condition classes, vm-handler-stack ops,
;;; vm-restart ops, %vm-jump-to-handler, vm-signal-condition).

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
  (let ((handler (vm-find-handler vm-state condition)))
    (if handler
        (values t handler)
        (if error-p
            (error condition)
            (values nil nil)))))

(defmethod execute-instruction ((inst vm-signal) state pc labels)
  (let* ((condition (vm-reg-get state (vm-condition-reg inst)))
         (handler   (vm-find-handler state condition)))
    (if handler
        (%vm-jump-to-handler state labels condition handler)
        ;; No handler - continue execution
        (values (1+ pc) nil nil))))

(defmethod execute-instruction ((inst vm-error-instruction) state pc labels)
  (let* ((condition (vm-reg-get state (vm-condition-reg inst)))
         (handler   (vm-find-handler state condition)))
    (if handler
        (%vm-jump-to-handler state labels condition handler)
        (progn (error condition) (values (1+ pc) nil nil)))))

(defmethod execute-instruction ((inst vm-cerror) state pc labels)
  ;; Create a continue restart and signal the condition
  ;; The restart allows continuing after the error
  (let* ((condition (vm-reg-get state (vm-condition-reg inst)))
         (handler   (vm-find-handler state condition)))
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
  (declare (ignore labels))
  (let ((condition (vm-reg-get state (vm-condition-reg inst))))
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
        (error 'vm-error
               :vm-state state
               :format-control "No restart named ~S found"
               :format-arguments (list restart-name)))))

;;; ─── Condition Construction Helpers ──────────────────────────────────────────

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
                  :name variable-name))

(defun make-vm-undefined-function (vm-state function-name)
  "Construct a vm-undefined-function condition."
  (make-condition 'vm-undefined-function
                  :vm-state vm-state
                  :name function-name))

(defun make-vm-division-by-zero (vm-state dividend)
  "Construct a vm-division-by-zero condition."
  (make-condition 'vm-division-by-zero
                  :vm-state vm-state
                  :dividend dividend))
