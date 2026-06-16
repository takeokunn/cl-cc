(in-package :cl-cc/compile)

;;; ── Exception handling: catch / throw / unwind-protect / handler-case ────

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
