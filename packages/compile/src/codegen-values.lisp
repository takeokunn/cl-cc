(in-package :cl-cc/compile)

;;;; Multiple values, APPLY, and MULTIPLE-VALUE-CALL codegen methods.

(defmethod compile-ast ((node ast-values) ctx)
  (setf (ctx-tail-position ctx) nil)
  (%with-compiled-registers (src-regs (ast-values-forms node) ctx)
    (let ((dst (make-register ctx)))
      (emit ctx (make-vm-values :dst dst :src-regs src-regs))
      dst)))

(defmethod compile-ast ((node ast-multiple-value-bind) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let* ((vars (ast-mvb-vars node))
         (body (ast-mvb-body node))
         (old-env (ctx-env ctx))
         (var-regs (%compile-mvb-value-registers vars (ast-mvb-values-form node) ctx)))
    (unwind-protect
         (progn
           (%install-register-bindings vars var-regs ctx)
           (%compile-body/k body ctx #'identity))
      (setf (ctx-env ctx) old-env))))

(defun %compile-apply-literal-spread (leading-args spread-values func-reg result-reg ctx)
  "Compile APPLY as a direct CALL when the final spread arg is a literal list."
  (%with-compiled-registers (leading-regs leading-args ctx)
    (%with-compiled-registers (spread-regs (%quoted-value-forms spread-values) ctx)
      (emit ctx (make-vm-call :dst result-reg
                              :func func-reg
                              :args (append leading-regs spread-regs))))))

(defun %compile-apply-dynamic-spread (args func-reg result-reg ctx)
  "Compile APPLY with a runtime spread argument."
  (%with-compiled-registers (arg-regs args ctx)
    (emit ctx (make-vm-apply :dst result-reg :func func-reg :args arg-regs))))

(defmethod compile-ast ((node ast-apply) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let* ((func-reg (make-register ctx))
         (result-reg (make-register ctx))
         (args (ast-apply-args node))
         (plan (%apply-argument-plan args)))
    (emit ctx (make-vm-move :dst func-reg
                            :src (%resolve-apply-function-register
                                  (ast-apply-func node) ctx)))
    (multiple-value-bind (literal-spread-p spread-values)
        (%literal-apply-spread-values (getf plan :spread))
      (if literal-spread-p
          (%compile-apply-literal-spread (getf plan :leading)
                                         spread-values
                                         func-reg
                                         result-reg
                                         ctx)
          (%compile-apply-dynamic-spread args func-reg result-reg ctx)))
    result-reg))

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
  (setf (ctx-tail-position ctx) nil)
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
    result-reg))

(defmethod compile-ast ((node ast-multiple-value-prog1) ctx)
  "Save result of first form, evaluate remaining forms, return saved result."
  (setf (ctx-tail-position ctx) nil)
  (let ((first-reg (compile-ast (ast-mv-prog1-first node) ctx))
        (result-reg (make-register ctx)))
    (emit ctx (make-vm-move :dst result-reg :src first-reg))
    (%compile-body/k (ast-mv-prog1-forms node) ctx #'identity)
    result-reg))
