(in-package :cl-cc/codegen)
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
  (let* ((params (ast-lambda-params node))
         (body (ast-lambda-body node))
         (func-label (make-label ctx "lambda"))
         (end-label (make-label ctx "lambda_end"))
         (closure-reg (make-register ctx))
         (free-vars (find-free-variables node))
         (captured-vars (mapcar (lambda (v) (cons v (lookup-var ctx v)))
                                 (remove-if-not (lambda (v) (assoc v (ctx-env ctx)))
                                                free-vars)))
         (param-regs (loop for i from 0 below (length params)
                           collect (make-register ctx)))
          (rest-stack-alloc-p (and (ast-lambda-rest-param node)
                                   (or (rest-param-stack-alloc-p body (ast-lambda-rest-param node))
                                       (dynamic-extent-declared-p (ast-lambda-declarations node)
                                                                  (ast-lambda-rest-param node))))))
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
                 :key-params key-closure-data :rest-stack-alloc-p rest-stack-alloc-p
                 :captured captured-vars))
      (%emit-closure-body ctx func-label end-label params param-regs
                          opt-bindings rest-binding key-bindings
                          non-constant-defaults body supplied-p-entries)
      closure-reg)))

(defmethod compile-ast ((node ast-defun) ctx)
  "Compile a top-level function definition.
Generates a closure at the function's label and registers it globally."
  (setf (ctx-tail-position ctx) nil)
  (let* ((name (ast-defun-name node))
         (params (ast-defun-params node))
         (body (ast-defun-body node))
         (type-bindings (function-param-type-bindings name params))
         ;; Pre-make the label before emitting so recursive calls can find it
         (func-label (make-label ctx (format nil "DEFUN_~A" name)))
         (end-label (make-label ctx (format nil "DEFUN_~A_END" name)))
         (closure-reg (make-register ctx))
          (free-vars (let ((temp-ast (make-ast-lambda :params params :body body)))
                       (find-free-variables temp-ast)))
          (captured-vars (mapcar (lambda (v) (cons v (lookup-var ctx v)))
                                 (remove-if-not (lambda (v) (assoc v (ctx-env ctx)))
                                                free-vars)))
          (param-regs (loop for i from 0 below (length params)
                            collect (make-register ctx)))
           (rest-stack-alloc-p (and (ast-defun-rest-param node)
                                    (or (rest-param-stack-alloc-p body (ast-defun-rest-param node))
                                        (dynamic-extent-declared-p (ast-defun-declarations node)
                                                                   (ast-defun-rest-param node))))))
    ;; Pre-register for recursion support
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
                 :key-params key-closure-data :rest-stack-alloc-p rest-stack-alloc-p
                 :captured captured-vars))
      (push (cons name closure-reg) (ctx-env ctx))
      (emit ctx (make-vm-register-function :name name :src closure-reg))
       (let ((*compiling-typed-fn* (or name t)))
          (%emit-closure-body ctx func-label end-label params param-regs
                              opt-bindings rest-binding key-bindings
                              non-constant-defaults body supplied-p-entries
                              type-bindings
                              name func-label))
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
