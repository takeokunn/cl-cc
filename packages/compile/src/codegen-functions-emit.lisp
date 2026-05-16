(in-package :cl-cc/compile)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Lambda, Defun, and Defvar compile-ast Methods
;;;
;;; Contains:
;;;   %emit-closure-body — shared jump-over + label + body pattern
;;;   compile-ast (ast-lambda) — closure emission
;;;   compile-ast (ast-defun) — top-level function definition
;;;   compile-ast (ast-defvar) — global variable definition (defvar/defparameter)
;;;
;;; Typed parameter machinery, defmacro, and parameter-list helpers
;;; are in codegen-functions.lisp (loads before).
;;;
;;; Load order: after codegen-functions.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Lambda and defun ─────────────────────────────────────────────────────

(defun %emit-closure-body (ctx func-label end-label params param-regs
                           opt-bindings rest-binding key-bindings
                           non-constant-defaults body
                           &optional supplied-p-entries type-bindings
                           current-function-name current-function-label)
  "Emit the jump-over + label + body + end-label pattern common to lambda and defun."
  (emit ctx (make-vm-jump :label end-label))
  (emit ctx (make-vm-label :name func-label))
  (compile-function-body ctx params param-regs opt-bindings rest-binding
                          key-bindings non-constant-defaults body
                          supplied-p-entries type-bindings
                          current-function-name current-function-label)
  (emit ctx (make-vm-label :name end-label)))

(defmethod compile-ast ((node ast-lambda) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let ((params (ast-lambda-params node))
        (body (ast-lambda-body node))
        (func-label (make-label ctx "lambda"))
        (end-label (make-label ctx "lambda_end"))
        (closure-reg (make-register ctx))
        (inline-policy (%callable-inline-policy
                        (ast-lambda-declarations node)
                        :pending-policy (ctx-pending-inline-policy ctx)))
        (free-vars (find-free-variables node))
        (captured-vars nil)
        (param-regs nil)
        (rest-stack-alloc-p nil))
    (setq captured-vars
          (loop for v in free-vars
                when (%assoc-eq v (ctx-env ctx))
                  collect (cons v (lookup-var ctx v))))
    (setq param-regs (loop for _ in params collect (make-register ctx)))
    (setq rest-stack-alloc-p
          (and (ast-lambda-rest-param node)
               (or (rest-param-stack-alloc-p body (ast-lambda-rest-param node))
                   (dynamic-extent-declared-p (ast-lambda-declarations node)
                                              (ast-lambda-rest-param node)))))
    (multiple-value-bind (opt-closure-data rest-reg key-closure-data
                          opt-bindings rest-binding key-bindings
                          non-constant-defaults supplied-p-entries)
        (allocate-extended-params ctx
                                  (ast-lambda-optional-params node)
                                  (ast-lambda-rest-param node)
                                  (ast-lambda-key-params node))
       (emit ctx (make-vm-closure
                  :dst closure-reg :label func-label :params param-regs
                  :optional-params opt-closure-data :rest-param rest-reg
                  :key-params key-closure-data
                  :rest-stack-alloc-p rest-stack-alloc-p
                  :inline-policy inline-policy
                  :dispatch-tag '(:anonymous . nil)
                  :captured captured-vars))
      (%call-with-declaration-policies
       ctx
       (ast-lambda-declarations node)
       (lambda ()
         (%emit-closure-body ctx func-label end-label params param-regs
                             opt-bindings rest-binding key-bindings
                             non-constant-defaults body supplied-p-entries)))
      closure-reg)))

(defmethod compile-ast ((node ast-defun) ctx)
  "Compile a top-level function definition."
  (setf (ctx-tail-position ctx) nil)
  (let* ((name (ast-defun-name node))
         (params (ast-defun-params node))
         (body (ast-defun-body node))
         (type-bindings (function-param-type-bindings name params))
         (func-label (make-label ctx (format nil "DEFUN_~A" name)))
         (end-label (make-label ctx (format nil "DEFUN_~A_END" name)))
         (closure-reg (make-register ctx))
         (inline-policy (%callable-inline-policy
                         (ast-defun-declarations node)
                         :name name
                         :pending-policy (ctx-pending-inline-policy ctx)))
         (free-vars (let ((temp-ast (make-ast-lambda :params params
                                                     :body body)))
                      (find-free-variables temp-ast)))
         (captured-vars nil)
         (param-regs nil)
         (rest-stack-alloc-p nil))
    (setq captured-vars
          (loop for v in free-vars
                when (%assoc-eq v (ctx-env ctx))
                  collect (cons v (lookup-var ctx v))))
    (setq param-regs (loop for _ in params collect (make-register ctx)))
    (setq rest-stack-alloc-p
          (and (ast-defun-rest-param node)
               (or (rest-param-stack-alloc-p body (ast-defun-rest-param node))
                   (dynamic-extent-declared-p (ast-defun-declarations node)
                                               (ast-defun-rest-param node)))))
    (when (ast-defun-documentation node)
      (compile-ast
       (make-ast-call :func 'cl-cc/vm::%register-documentation
                      :args (list (make-ast-quote :value name)
                                  (make-ast-quote :value 'function)
                                  (make-ast-quote :value (ast-defun-documentation node))))
       ctx))
    (setf (gethash name (ctx-global-functions ctx)) func-label)
    (multiple-value-bind (opt-closure-data rest-reg key-closure-data
                          opt-bindings rest-binding key-bindings
                          non-constant-defaults supplied-p-entries)
        (allocate-extended-params ctx
                                  (ast-defun-optional-params node)
                                  (ast-defun-rest-param node)
                                  (ast-defun-key-params node))
       (emit ctx (make-vm-closure
                  :dst closure-reg :label func-label :params param-regs
                  :optional-params opt-closure-data :rest-param rest-reg
                  :key-params key-closure-data
                  :rest-stack-alloc-p rest-stack-alloc-p
                  :inline-policy inline-policy
                  :dispatch-tag (cons :known-function name)
                  :captured captured-vars))
      (setf (ctx-env ctx) (cons (cons name closure-reg) (ctx-env ctx)))
      (emit ctx (make-vm-register-function :name name :src closure-reg))
      (let ((*compiling-typed-fn* (or name t)))
        (%call-with-declaration-policies
         ctx
         (ast-defun-declarations node)
         (lambda ()
           (%emit-closure-body ctx func-label end-label params param-regs
                               opt-bindings rest-binding key-bindings
                               non-constant-defaults body supplied-p-entries
                               type-bindings name func-label))))
      closure-reg)))

;;; ── defvar / defparameter ────────────────────────────────────────────────

(defmethod compile-ast ((node ast-defvar) ctx)
  "Compile a top-level variable definition.
Global variables are stored in the VM's global variable store so they
persist across function calls.
FR-600: defvar only sets the value if the variable is not already bound;
        defparameter always sets the value."
  (setf (ctx-tail-position ctx) nil)
  (let* ((name (ast-defvar-name node))
         (kind (ast-defvar-kind node))  ; 'defvar or 'defparameter
         (value-form (ast-defvar-value node))
         (result-reg (make-register ctx)))
    (setf (gethash name (ctx-global-variables ctx)) t)
    (cond
      ;; defparameter — always set the global (original behavior)
      ((eq kind 'defparameter)
       (let ((value-reg (if value-form
                            (compile-ast value-form ctx)
                            (progn (emit ctx (make-vm-const :dst result-reg :value nil))
                                   result-reg))))
         (emit ctx (make-vm-set-global :name name :src value-reg))
         value-reg))
      ;; defvar with no init form — just declare, do not set
      ((null value-form)
       (emit ctx (make-vm-const :dst result-reg :value name))
       result-reg)
      ;; defvar with init form — only set if variable not already bound
      (t
       (let* ((sym-reg   (make-register ctx))
              (bound-reg (make-register ctx))
              (label-init (make-label ctx "DEFVAR_INIT"))
              (label-done (make-label ctx "DEFVAR_DONE")))
         ;; Load the symbol and check boundp
         (emit ctx (make-vm-const :dst sym-reg :value name))
         (emit ctx (make-vm-boundp :dst bound-reg :src sym-reg))
         ;; If NOT bound (bound-reg = nil), jump to DO_INIT
         (emit ctx (make-vm-jump-zero :reg bound-reg :label label-init))
         ;; Already bound — skip to done
         (emit ctx (make-vm-jump :label label-done))
         ;; DO_INIT: compile and set the value
         (emit ctx (make-vm-label :name label-init))
         (let ((value-reg (compile-ast value-form ctx)))
           (emit ctx (make-vm-set-global :name name :src value-reg)))
         ;; DONE: result is the variable name (ANSI: defvar returns the name)
         (emit ctx (make-vm-label :name label-done))
         (emit ctx (make-vm-const :dst result-reg :value name))
         result-reg)))))
