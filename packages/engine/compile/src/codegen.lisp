(in-package :cl-cc/compile)

(declaim (special *x86-64-calling-convention*))
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
            (cl-cc/type:type-env-extend (ast-defvar-name ast)
                                        (cl-cc/type:type-to-scheme value-type)
                                        type-env)
            type-env))
      type-env))

(defun %extend-type-env-for-defun (ast type-env best-effort-type)
  "If AST is a defun, extend TYPE-ENV with its inferred function type."
  (if (typep ast 'ast-defun)
      (let ((fn-type (funcall best-effort-type ast type-env)))
        (if fn-type
            (cl-cc/type:type-env-extend (ast-defun-name ast)
                                        (cl-cc/type:type-to-scheme fn-type)
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

(defun compile-toplevel-forms (forms &key (target :x86_64) type-check (safety 1) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  "Compile a list of top-level forms (e.g., from a source file).
Handles defun, defvar, and expression forms.
Returns a compilation-result struct with program, assembly, and globals."
  (let ((ctx           (make-instance 'compiler-context :safety safety))
        (last-reg      nil)
        (last-type     nil)
        (last-cps      nil)
        (compiled-asts nil)
        (type-env      (cl-cc/type:type-env-empty)))
    (labels ((best-effort-type (ast env)
               (ignore-errors (type-check-ast ast env))))
      (let ((*compile-time-value-env*    nil)
            (*compile-time-function-env* nil))
        (dolist (form forms)
          (unless (and (consp form) (eq (car form) 'in-package))
            (let* ((expanded (if (typep form 'ast-node)
                                 form
                                 (cl-cc/expand:compiler-macroexpand-all form)))
                   (ast (if (typep expanded 'ast-node)
                            expanded
                            (lower-sexp-to-ast expanded))))
              (when (typep ast 'ast-defun)
                (push (cons (ast-defun-name ast) ast) *compile-time-function-env*))
               (setf ast      (optimize-ast ast))
               (push ast compiled-asts)
               (setf last-cps (maybe-cps-transform ast))
               (when type-check
                 (setf last-type (%type-check-form ast type-env type-check)))
               (setf type-env  (%extend-type-env-for-defvar ast type-env #'best-effort-type))
               (setf type-env  (%extend-type-env-for-defun  ast type-env #'best-effort-type))
               (%maybe-extend-ct-value-env ast)
               (setf last-reg  (compile-ast ast ctx)))))))
    (setf (ctx-type-env ctx) type-env)
    (when last-reg
      (emit ctx (make-vm-halt :reg last-reg)))
    (when *repl-capture-label-counter*
      (setf *repl-capture-label-counter* (ctx-next-label ctx)))
    (let* ((instructions (nreverse (ctx-instructions ctx)))
           (optimized    nil)
           (leaf-p       nil)
           (program      nil))
      (multiple-value-setq (optimized leaf-p)
        (optimize-instructions instructions
                               :pass-pipeline        pass-pipeline
                               :print-pass-timings   print-pass-timings
                               :timing-stream        timing-stream
                               :print-pass-stats     print-pass-stats
                               :stats-stream         stats-stream
                               :trace-json-stream    trace-json-stream
                               :print-opt-remarks    print-opt-remarks
                               :opt-remarks-stream   opt-remarks-stream
                               :opt-remarks-mode     opt-remarks-mode))
      (setf program (make-vm-program :instructions    instructions
                                     :result-register last-reg
                                     :leaf-p          leaf-p))
      (make-compilation-result :program             program
                               :assembly            (emit-assembly program :target target)
                               :globals             (ctx-global-functions ctx)
                               :type                last-type
                               :type-env            (ctx-type-env ctx)
                               :cps                 last-cps
                               :ast                 (nreverse compiled-asts)
                               :vm-instructions     instructions
                               :optimized-instructions optimized))))

;;; Function call compilation (%resolve-func-sym-reg, %try-compile-*,
;;; %compile-normal-call, compile-ast (ast-call)) is in codegen-calls.lisp (loads next).
