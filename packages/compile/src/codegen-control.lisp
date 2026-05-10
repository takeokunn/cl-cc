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

(defmethod compile-ast ((node ast-handler-case) ctx)
  "Compile handler-case: establish handlers, run protected form, handle errors."
  (setf (ctx-tail-position ctx) nil)
  (let* ((clauses           (ast-handler-case-clauses node))
         (result-reg        (make-register ctx))
         (normal-exit-label (make-label ctx "handler_case_exit"))
         (handler-infos     (loop for clause in clauses
                                  collect (list (make-label ctx "handler")
                                                (make-register ctx)))))
    ;; Register handlers in reverse declaration order
    (loop for clause in (reverse clauses)
          for info   in (reverse handler-infos)
          do (emit ctx (make-vm-establish-handler
                        :handler-label (first info)
                        :result-reg    (second info)
                        :error-type    (first clause))))
    (let ((form-result (compile-ast (ast-handler-case-form node) ctx)))
      (emit ctx (make-vm-move :dst result-reg :src form-result)))
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
               (emit ctx (make-vm-jump :label normal-exit-label))))
    (emit ctx (make-vm-label :name normal-exit-label))
    result-reg))
