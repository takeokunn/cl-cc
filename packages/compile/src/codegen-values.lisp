(in-package :cl-cc/compile)

;;;; Multiple values, APPLY, and MULTIPLE-VALUE-CALL codegen methods.

(defmethod compile-ast ((node ast-values) ctx)
  (%with-no-tail-position ctx
    (%with-compiled-registers (src-regs (ast-values-forms node) ctx)
      (let ((count (length src-regs)))
        (cond
          ((zerop count)
           (let ((dst (make-register ctx)))
             (emit ctx (make-vm-values :dst dst :src-regs nil))
             dst))
          ((= count 1)
           (first src-regs))
          ((%small-values-count-p count)
           (emit ctx (make-vm-values-regs :reg0 (first src-regs)
                                          :reg1 (second src-regs)
                                          :reg2 (third src-regs)
                                          :count count))
           (first src-regs))
          (t
           (let ((dst (make-register ctx)))
             (emit ctx (make-vm-values :dst dst :src-regs src-regs))
             dst)))))))

(defmethod compile-ast ((node ast-multiple-value-bind) ctx)
  (%with-no-tail-position ctx
    (let* ((vars (ast-mvb-vars node))
           (body (ast-mvb-body node))
           (old-env (ctx-env ctx))
           (var-regs (%compile-mvb-value-registers vars (ast-mvb-values-form node) ctx)))
      (unwind-protect
           (progn
             (%install-register-bindings vars var-regs ctx)
             (%compile-body/k body ctx #'identity))
        (setf (ctx-env ctx) old-env)))))

(defun %compile-apply-flat-call (arg-forms func-reg result-reg tail ctx)
  "Compile APPLY as a direct CALL with already-flattened ARG-FORMS."
  (%with-compiled-registers (arg-regs arg-forms ctx)
    (emit ctx (if tail
                  (make-vm-tail-call :dst result-reg :func func-reg :args arg-regs)
                  (make-vm-call :dst result-reg :func func-reg :args arg-regs))))
  result-reg)

(defun %compile-apply-literal-spread (leading-args spread-values func-reg result-reg tail ctx)
  "Compile APPLY as a direct CALL when the final spread arg is a literal list."
  (%compile-apply-flat-call (append leading-args (%quoted-value-forms spread-values))
                            func-reg result-reg tail ctx))

(defun %compile-apply-list-call-spread (leading-args spread-forms func-reg result-reg tail ctx)
  "Compile APPLY as a direct CALL when the final spread arg is a (LIST ...) call."
  (%compile-apply-flat-call (append leading-args spread-forms)
                            func-reg result-reg tail ctx))

(defun %compile-apply-dynamic-spread (args func-reg result-reg tail ctx)
  "Compile APPLY with a runtime spread argument."
  (%with-compiled-registers (arg-regs args ctx)
    (emit ctx (make-vm-apply :dst result-reg :func func-reg :args arg-regs :tail-p tail)))
  result-reg)

(defmethod compile-ast ((node ast-apply) ctx)
  (let* ((tail (ctx-tail-position ctx))
         (func-reg (make-register ctx))
         (result-reg (make-register ctx))
         (args (ast-apply-args node))
         (spread-plan (%apply-spread-plan args ctx)))
    (%with-no-tail-position ctx
      (emit ctx (make-vm-move :dst func-reg
                              :src (%resolve-apply-function-register
                                    (ast-apply-func node) ctx)))
      (%with-apply-spread-dispatch (spread-plan leading-args)
        :literal (%compile-apply-literal-spread leading-args
                                                 (getf spread-plan :values)
                                                 func-reg
                                                 result-reg
                                                 tail
                                                 ctx)
        :list-call (%compile-apply-list-call-spread leading-args
                                                    (getf spread-plan :forms)
                                                    func-reg
                                                    result-reg
                                                    tail
                                                    ctx)
        :dynamic (%compile-apply-dynamic-spread args func-reg result-reg tail ctx))
      result-reg)))

(defun %compile-flat-multiple-value-call (args func-reg result-reg ctx)
  "Compile MULTIPLE-VALUE-CALL by flattening literal VALUES arg forms."
  (%with-compiled-registers (arg-regs (%flat-values-forms args) ctx)
    (emit ctx (make-vm-call :dst result-reg :func func-reg :args arg-regs))))

(defun %compile-dynamic-multiple-value-call (args func-reg result-reg ctx)
  "Compile MULTIPLE-VALUE-CALL by collecting runtime values into lists."
  (let ((list-regs (%compile-values-list-registers args ctx)))
    (emit ctx (make-vm-apply :dst result-reg
                             :func func-reg
                             :args (list (%append-list-registers list-regs ctx))))))

(defmethod compile-ast ((node ast-multiple-value-call) ctx)
  "Evaluate FUNC, collect all values from each arg form, then apply FUNC."
  (%with-no-tail-position ctx
    (let ((func-reg (make-register ctx))
          (result-reg (make-register ctx))
          (args (ast-mv-call-args node)))
      (emit ctx (make-vm-move :dst func-reg
                              :src (compile-ast (ast-mv-call-func node) ctx)))
      (cond
        ((null args)
         (emit ctx (make-vm-call :dst result-reg :func func-reg :args nil)))
        ((%all-values-args-p args)
         (%compile-flat-multiple-value-call args func-reg result-reg ctx))
        (t
         (%compile-dynamic-multiple-value-call args func-reg result-reg ctx)))
      result-reg)))

(defmethod compile-ast ((node ast-multiple-value-prog1) ctx)
  "Save all values of first form, evaluate remaining forms, restore saved values."
  (%with-no-tail-position ctx
    (emit ctx (make-vm-clear-values))
    (let ((first-reg (compile-ast (ast-mv-prog1-first node) ctx))
          (saved-list-reg (make-register ctx)))
      (emit ctx (make-vm-ensure-values :src first-reg))
      (emit ctx (make-vm-values-to-list :dst saved-list-reg))
      (%compile-body/k (ast-mv-prog1-forms node) ctx #'identity)
      (emit ctx (make-vm-spread-values :dst first-reg :src saved-list-reg))
      first-reg)))
