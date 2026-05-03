(in-package :cl-cc/compile)

(declaim (special cl-cc/target:*x86-64-target*))
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Entry Points + Calls + Exception Handling + Multiple Values
;;;
;;; Contains: compile-toplevel-forms, ast-call (Phase 1 + Phase 2 dispatch),
;;; exception handling (catch/throw/unwind-protect/handler-case),
;;; multiple values (values/mvb/apply/mv-call/mv-prog1),
;;; ast-function, ast-flet, ast-labels, and assembly utilities.
;;;
;;; Load order: after codegen-phase2. compile-ast defgeneric and all
;;; methods for primitives, CLOS, functions, and Phase 2 handlers live in
;;; the preceding files.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Top-level compilation entry point ────────────────────────────────────
;;; (Fold machinery is in codegen-fold.lisp, which loads before this file.)

(defstruct compilation-result
  "Result of compiling expressions or top-level forms."
  (program nil)
  (assembly nil)
  (globals nil)
  (type nil)
  (type-env nil)
  (cps nil)
  (ast nil)
  (vm-instructions nil)
  (optimized-instructions nil))

(defun %type-check-form (ast type-env type-check)
  "Run the type checker on AST in TYPE-ENV, honoring TYPE-CHECK strictness.
Returns the inferred type, or NIL on failure (warning printed unless :strict)."
  (handler-case
      (type-check-ast ast type-env)
    (error (e)
      (if (eq type-check :strict)
          (error e)
      (progn (warn "Type check warning: ~A" e) nil)))))

(defun %extend-type-env-for-defvar (ast type-env best-effort-type)
  "If AST is a defvar with an initializer, extend TYPE-ENV with its inferred type."
  (if (and (typep ast 'ast-defvar) (ast-defvar-value ast))
      (let ((value-type (funcall best-effort-type (ast-defvar-value ast) type-env)))
        (if value-type
            (type-env-extend (ast-defvar-name ast)
                                        (type-to-scheme value-type)
                                        type-env)
            type-env))
      type-env))

(defun %extend-type-env-for-defun (ast type-env best-effort-type)
  "If AST is a defun, extend TYPE-ENV with its inferred function type."
  (if (typep ast 'ast-defun)
      (let ((fn-type (funcall best-effort-type ast type-env)))
        (if fn-type
            (type-env-extend (ast-defun-name ast)
                                        (type-to-scheme fn-type)
                                        type-env)
            type-env))
      type-env))

(defun %maybe-extend-ct-value-env (ast)
  "If AST is a defvar with a statically evaluable initializer, push it onto
*compile-time-value-env* (side-effecting; no return value)."
  (when (and (typep ast 'ast-defvar) (ast-defvar-value ast))
    (multiple-value-bind (value ok)
        (%evaluate-ast (ast-defvar-value ast) *compile-time-eval-depth-limit*)
      (when ok
        (push (cons (ast-defvar-name ast) value) *compile-time-value-env*)))))

(defun %make-compile-opts (&key pass-pipeline print-pass-timings timing-stream
                               print-opt-remarks opt-remarks-stream (opt-remarks-mode :all)
                               print-pass-stats stats-stream trace-json-stream)
  "Build a compilation options plist suitable for APPLYing to compile-*/optimize-* functions."
  (list :pass-pipeline       pass-pipeline
        :print-pass-timings  print-pass-timings
        :timing-stream       timing-stream
        :print-opt-remarks   print-opt-remarks
        :opt-remarks-stream  opt-remarks-stream
        :opt-remarks-mode    opt-remarks-mode
        :print-pass-stats    print-pass-stats
        :stats-stream        stats-stream
        :trace-json-stream   trace-json-stream))

(defun %maybe-compile-toplevel-form-via-cps (ast type-check safety opts)
  "Compile AST through the CPS entry path when the VM-safe subset allows it.
Returns a compilation-result or NIL when AST should stay on the direct path."
  (when (and *enable-cps-vm-primary-path*
             (not *compile-expression-cps-recursion-guard*)
             (%cps-vm-compile-safe-ast-p ast))
    (let ((cps (cps-transform-ast* ast)))
      (let ((*compile-expression-cps-recursion-guard* t))
        (apply #'compile-expression (%cps-identity-entry-form cps)
               :target :vm :type-check type-check :safety safety
               opts)))))

(defun %result-vm-instructions-without-halt (result)
  "Return RESULT's VM instructions without its terminal halt instruction."
  (labels ((scan (instructions)
             (if (consp instructions)
                 (if (consp (cdr instructions))
                     (cons (car instructions) (scan (cdr instructions)))
                     (if (typep (car instructions) 'vm-halt)
                         nil
                         (cons (car instructions) nil)))
                 nil)))
    (scan (compilation-result-vm-instructions result))))

(defun %lower-toplevel-form-to-ast (form)
  "Expand FORM and lower it into an optimized AST node."
  (let* ((expanded (if (typep form 'ast-node)
                       form
                       (cl-cc/expand:compiler-macroexpand-all form)))
         (ast (if (typep expanded 'ast-node)
                  expanded
                  (lower-sexp-to-ast expanded))))
    (optimize-ast ast)))

(defun %record-toplevel-defun-for-ct-env (ast)
  "Register top-level function ASTs for compile-time evaluation helpers."
  (when (typep ast 'ast-defun)
    (push (cons (ast-defun-name ast) ast) *compile-time-function-env*)))

(defun %update-toplevel-type-state (ast type-env type-check best-effort-type)
  "Return updated type metadata after visiting AST as a top-level form.
Values: inferred-type, updated-type-env."
  (let ((last-type nil)
        (next-type-env type-env))
    (when type-check
      (setf last-type (%type-check-form ast type-env type-check)))
    (setf next-type-env (%extend-type-env-for-defvar ast next-type-env best-effort-type))
    (setf next-type-env (%extend-type-env-for-defun ast next-type-env best-effort-type))
    (values last-type next-type-env)))

(defun %compile-toplevel-ast-into-context (ast ctx target type-check safety opts)
  "Compile AST into CTX, preferring the CPS VM path when allowed.
Returns two values: result register and CPS form used for the AST."
  (let* ((last-cps (and (eq target :vm)
                        (%cps-vm-compile-safe-ast-p ast)
                        (cps-transform-ast* ast)))
         (cps-result (and (eq target :vm)
                          (%maybe-compile-toplevel-form-via-cps ast type-check safety opts))))
    (if cps-result
        (progn
          (setf (ctx-instructions ctx)
                (append (reverse (%result-vm-instructions-without-halt cps-result))
                        (ctx-instructions ctx)))
          (values (vm-program-result-register
                   (compilation-result-program cps-result))
                  last-cps))
        (values (compile-ast ast ctx) last-cps))))

(defun %top-level-in-package-form-p (form)
  "Return T when FORM is an in-package declaration skipped by top-level compilation."
  (and (consp form)
       (eq (car form) 'in-package)))

(defun %best-effort-type-check (ast env)
  "Return type of AST in ENV, or NIL when type-checking signals an error."
  (ignore-errors (type-check-ast ast env)))

(defun %process-toplevel-form (form ctx target type-env type-check safety opts compiled-asts)
  "Compile one top-level FORM and return updated compilation state.
Values: last-reg, last-type, last-cps, updated-type-env."
  (let* ((ast (%lower-toplevel-form-to-ast form))
         (last-reg nil) (last-type nil) (last-cps nil))
    (%record-toplevel-defun-for-ct-env ast)
    (push ast compiled-asts)
    (multiple-value-setq (last-type type-env)
      (%update-toplevel-type-state ast type-env type-check #'%best-effort-type-check))
    (%maybe-extend-ct-value-env ast)
    (multiple-value-setq (last-reg last-cps)
      (%compile-toplevel-ast-into-context ast ctx target type-check safety opts))
    (values last-reg last-type last-cps type-env)))

(defun %finalize-toplevel-compilation (ctx target last-reg last-type last-cps compiled-asts opts)
  "Finalize CTX after all top-level forms have been compiled."
  (when last-reg
    (emit ctx (make-vm-halt :reg last-reg)))
  (when *repl-capture-label-counter*
    (setf *repl-capture-label-counter* (ctx-next-label ctx)))
  (let* ((instructions (nreverse (ctx-instructions ctx)))
         (optimized nil)
         (leaf-p    nil)
         (program   nil))
    (multiple-value-setq (optimized leaf-p)
      (apply #'optimize-instructions instructions opts))
    (setf program (make-vm-program :instructions (if (or (eq target :vm) (eq target :wasm))
                                                     instructions
                                                     (or optimized instructions))
                                   :result-register last-reg
                                   :leaf-p          leaf-p))
    (make-compilation-result :program                program
                             :assembly               (emit-assembly program :target target)
                             :globals                (ctx-global-functions ctx)
                             :type                   last-type
                             :type-env               (ctx-type-env ctx)
                             :cps                    last-cps
                             :ast                    (nreverse compiled-asts)
                             :vm-instructions        instructions
                             :optimized-instructions optimized)))


(defun compile-toplevel-forms (forms &key (target :x86_64) type-check (safety 1)
                                    pass-pipeline print-pass-timings timing-stream
                                    print-opt-remarks opt-remarks-stream (opt-remarks-mode :all)
                                    print-pass-stats stats-stream trace-json-stream)
  "Compile a list of top-level forms (e.g., from a source file).
Handles defun, defvar, and expression forms.
Returns a compilation-result struct with program, assembly, and globals."
  (let ((ctx           (make-instance 'compiler-context :safety safety))
        (last-reg      nil)
        (last-type     nil)
        (last-cps      nil)
        (compiled-asts nil)
        (type-env      (type-env-empty))
        (opts          (%make-compile-opts :pass-pipeline pass-pipeline
                                          :print-pass-timings print-pass-timings
                                          :timing-stream timing-stream
                                          :print-opt-remarks print-opt-remarks
                                          :opt-remarks-stream opt-remarks-stream
                                          :opt-remarks-mode opt-remarks-mode
                                          :print-pass-stats print-pass-stats
                                          :stats-stream stats-stream
                                          :trace-json-stream trace-json-stream)))
    (let ((*compile-time-value-env*    nil)
          (*compile-time-function-env* nil))
      (dolist (form forms)
        (unless (%top-level-in-package-form-p form)
          (multiple-value-setq (last-reg last-type last-cps type-env)
            (%process-toplevel-form form ctx target type-env type-check safety opts compiled-asts))))
      (setf (ctx-type-env ctx) type-env)
      (%finalize-toplevel-compilation ctx target last-reg last-type last-cps compiled-asts opts))))

;;; Function call compilation (%resolve-func-sym-reg, %try-compile-*,
;;; %compile-normal-call, compile-ast (ast-call)) is in codegen-calls.lisp (loads next).
