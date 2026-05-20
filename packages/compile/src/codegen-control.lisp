(in-package :cl-cc/compile)

;;; ── Exception handling: catch / throw / unwind-protect / handler-case ────

(defparameter +condition-signaling-callees+
  '(signal error warn cerror)
  "Function names that explicitly enter the condition system.")

(defparameter +handler-elision-condition-risk-inst-types+
  '(vm-div vm-cl-div vm-float-div vm-mod vm-rem
    vm-signal-error vm-type-error-condition)
  "Instruction types that can create or signal conditions despite otherwise
pure-looking dataflow.  Handler elision must remain conservative here.")

(defun %condition-signaling-callee-p (name)
  "Return true when NAME is a known direct condition-system callee."
  (and (symbolp name)
       (member name +condition-signaling-callees+ :test #'eq)))

(defun %ast-call-callee-symbol (callee)
  "Return CALLEE's statically visible function symbol, or NIL if unknown."
  (cond
    ((symbolp callee) callee)
    ((typep callee 'ast-var) (ast-var-name callee))
    ((typep callee 'ast-function) (ast-function-name callee))
    (t nil)))

(defun %ast-condition-signal-call-p (node)
  "Conservatively detect explicit SIGNAL/ERROR/WARN/CERROR calls in NODE."
  (labels ((walk (form)
             (when (typep form 'ast-node)
               (or (and (typep form 'ast-call)
                        (%condition-signaling-callee-p
                         (%ast-call-callee-symbol (ast-call-func form))))
                   (some #'walk (ast-children form))))))
    (walk node)))

(defun %inst-condition-risk-p (inst)
  "Return true when INST may signal a condition that a handler could observe."
  (or (member (type-of inst) +handler-elision-condition-risk-inst-types+
              :test #'eq)
      (eq (vm-inst-effect-kind inst) :control)))

(defun %handler-case-body-pure-p (instructions)
  "Return true when INSTRUCTIONS are pure and condition-safe for elision."
  (and instructions
       (every (lambda (inst)
                (and (opt-inst-pure-p inst)
                     (not (%inst-condition-risk-p inst))))
              instructions)))

(defun %handler-case-body-non-signaling-p (protected-form instructions)
  "Return true when PROTECTED-FORM has no visible condition signaling path."
  (and instructions
       (not (%ast-condition-signal-call-p protected-form))
       (every (lambda (inst)
                (let ((effect (vm-inst-effect-kind inst)))
                  (and (not (%inst-condition-risk-p inst))
                       (not (eq effect :unknown)))))
              instructions)))

(defun %handler-case-can-elide-handlers-p (protected-form body-instructions)
  "Return true when handler establishment can be omitted for PROTECTED-FORM.

This combines FR-046's explicit condition-callee scan with FR-139's VM
instruction purity check.  The predicate is intentionally conservative: any
unknown, control, or potentially signaling instruction keeps handlers in place."
  (or (%handler-case-body-non-signaling-p protected-form body-instructions)
      (%handler-case-body-pure-p body-instructions)))

(defun %prepend-handler-establishes-before-body (ctx body-rev establish-insts prefix)
  "Install ESTABLISH-INSTS before BODY-REV in CTX's reversed instruction list."
  (setf (ctx-instructions ctx)
        (append body-rev (reverse establish-insts) prefix)))

(defmethod compile-ast ((node ast-catch) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let ((tag-reg (compile-ast (ast-catch-tag node) ctx))
        (result-reg (make-register ctx))
        (handler-label (make-label ctx "catch_handler"))
        (end-label (make-label ctx "catch_end")))
    (emit ctx (make-vm-establish-catch
               :tag-reg tag-reg
               :handler-label handler-label
               :result-reg result-reg))
    (let ((body-result (let ((last nil))
                         (dolist (form (ast-catch-body node))
                           (setf last (compile-ast form ctx)))
                         last)))
      (emit ctx (make-vm-move :dst result-reg :src body-result)))
    (emit ctx (make-vm-remove-handler))
    (emit ctx (make-vm-jump :label end-label))
    (emit ctx (make-vm-label :name handler-label))
    (emit ctx (make-vm-label :name end-label))
    result-reg))

(defmethod compile-ast ((node ast-throw) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let ((tag-reg (compile-ast (ast-throw-tag node) ctx))
        (value-reg (compile-ast (ast-throw-value node) ctx)))
    (emit ctx (make-vm-throw :tag-reg tag-reg :value-reg value-reg))
    value-reg))

(defmethod compile-ast ((node ast-unwind-protect) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let ((result-reg (make-register ctx))
        (error-reg (make-register ctx))
        (error-flag-reg (make-register ctx))
        (handler-label (make-label ctx "unwind_handler"))
        (cleanup-label (make-label ctx "unwind_cleanup"))
        (end-label (make-label ctx "unwind_end")))
    (emit ctx (make-vm-const :dst error-flag-reg :value nil))
    (emit ctx (make-vm-establish-handler
               :handler-label handler-label
               :result-reg error-reg
               :error-type 'error))
    (let ((protected-result (compile-ast (ast-unwind-protected node) ctx)))
      (emit ctx (make-vm-move :dst result-reg :src protected-result)))
    (emit ctx (make-vm-remove-handler))
    (emit ctx (make-vm-jump :label cleanup-label))
    (emit ctx (make-vm-label :name handler-label))
    (emit ctx (make-vm-const :dst error-flag-reg :value t))
    (emit ctx (make-vm-label :name cleanup-label))
    (dolist (form (ast-unwind-cleanup node))
      (compile-ast form ctx))
    (emit ctx (make-vm-jump-zero :reg error-flag-reg :label end-label))
    (emit ctx (make-vm-sync-handler-regs))
    (emit ctx (make-vm-signal-error :error-reg error-reg))
    (emit ctx (make-vm-label :name end-label))
    result-reg))

(defmethod compile-ast ((node ast-handler-case) ctx)
  "Compile handler-case: establish handlers, run protected form, handle errors."
  (setf (ctx-tail-position ctx) nil)
  (let* ((clauses           (ast-handler-case-clauses node))
         (result-reg        (make-register ctx))
         (normal-exit-label (make-label ctx "handler_case_exit"))
         (handler-infos     (loop for clause in clauses
                                   collect (list (make-label ctx "handler")
                                                 (make-register ctx))))
         (protected-form    (ast-handler-case-form node))
         (establish-insts   (loop for clause in (reverse clauses)
                                  for info in (reverse handler-infos)
                                  collect (make-vm-establish-handler
                                           :handler-label (first info)
                                           :result-reg    (second info)
                                           :error-type    (first clause))))
         (prefix            (ctx-instructions ctx))
         (form-result       (compile-ast protected-form ctx))
         (body-rev          (ldiff (ctx-instructions ctx) prefix))
         (body-instructions (nreverse (copy-list body-rev)))
         (elide-handlers-p  (%handler-case-can-elide-handlers-p
                             protected-form body-instructions)))
    (unless elide-handlers-p
      ;; Register handlers before the protected body in VM execution order.
      (when establish-insts
        (%prepend-handler-establishes-before-body ctx body-rev establish-insts prefix)))
    (emit ctx (make-vm-move :dst result-reg :src form-result))
    (unless elide-handlers-p
      (loop repeat (length clauses)
            do (emit ctx (make-vm-remove-handler)))
      (emit ctx (make-vm-jump :label normal-exit-label))
      (loop for clause in clauses
            for info   in handler-infos
            do (let* ((var           (second clause))
                      (body          (cddr clause))
                      (handler-label (first info))
                      (error-reg     (second info))
                      (old-env       (ctx-env ctx)))
                 (emit ctx (make-vm-label :name handler-label))
                 (when var
                   (setf (ctx-env ctx) (cons (cons var error-reg) (ctx-env ctx))))
                 (let ((last-reg (if body
                                     (loop for form in body
                                           for r = (compile-ast form ctx)
                                           finally (return r))
                                     error-reg)))
                   (emit ctx (make-vm-move :dst result-reg :src last-reg)))
                 (setf (ctx-env ctx) old-env)
                 (emit ctx (make-vm-jump :label normal-exit-label)))))
    (unless elide-handlers-p
      (emit ctx (make-vm-label :name normal-exit-label)))
    result-reg))
