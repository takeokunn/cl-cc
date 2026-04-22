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

(defun %maybe-compile-toplevel-form-via-cps (ast &key type-check safety pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  "Compile AST through the CPS entry path when the VM-safe subset allows it.
Returns a compilation-result or NIL when AST should stay on the direct path."
  (let ((cps (maybe-cps-transform ast)))
    (when (and *enable-cps-vm-primary-path*
               cps
               (%cps-vm-compile-safe-ast-p ast))
      (compile-expression (%cps-entry-form cps)
                          :target :vm
                          :type-check type-check
                          :safety safety
                          :pass-pipeline pass-pipeline
                          :print-pass-timings print-pass-timings
                          :timing-stream timing-stream
                          :print-opt-remarks print-opt-remarks
                          :opt-remarks-stream opt-remarks-stream
                          :opt-remarks-mode opt-remarks-mode
                          :print-pass-stats print-pass-stats
                          :stats-stream stats-stream
                          :trace-json-stream trace-json-stream))))

(defun %result-vm-instructions-without-halt (result)
  "Return RESULT's VM instructions without its terminal halt instruction."
  (let ((instructions (copy-list (compilation-result-vm-instructions result))))
    (if (and instructions
             (typep (car (last instructions)) 'vm-halt))
        (butlast instructions)
        instructions)))

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

(defun %compile-toplevel-ast-into-context (ast ctx target type-check safety
                                           pass-pipeline print-pass-timings timing-stream
                                           print-opt-remarks opt-remarks-stream
                                           opt-remarks-mode print-pass-stats stats-stream
                                           trace-json-stream)
  "Compile AST into CTX, preferring the CPS VM path when allowed.
Returns two values: result register and CPS form used for the AST."
  (let* ((last-cps (maybe-cps-transform ast))
         (cps-result (and (eq target :vm)
                          (%maybe-compile-toplevel-form-via-cps ast
                                                               :type-check type-check
                                                               :safety safety
                                                               :pass-pipeline pass-pipeline
                                                               :print-pass-timings print-pass-timings
                                                               :timing-stream timing-stream
                                                               :print-opt-remarks print-opt-remarks
                                                               :opt-remarks-stream opt-remarks-stream
                                                               :opt-remarks-mode opt-remarks-mode
                                                               :print-pass-stats print-pass-stats
                                                               :stats-stream stats-stream
                                                               :trace-json-stream trace-json-stream))))
    (if cps-result
        (progn
          (setf (ctx-instructions ctx)
                (append (reverse (%result-vm-instructions-without-halt cps-result))
                        (ctx-instructions ctx)))
          (values (vm-program-result-register
                   (compilation-result-program cps-result))
                  last-cps))
        (values (compile-ast ast ctx)
                last-cps))))


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
            (let ((ast (%lower-toplevel-form-to-ast form)))
              (%record-toplevel-defun-for-ct-env ast)
              (push ast compiled-asts)
              (multiple-value-setq (last-type type-env)
                (%update-toplevel-type-state ast type-env type-check #'best-effort-type))
              (%maybe-extend-ct-value-env ast)
              (multiple-value-setq (last-reg last-cps)
                (%compile-toplevel-ast-into-context ast ctx target type-check safety
                                                   pass-pipeline print-pass-timings timing-stream
                                                   print-opt-remarks opt-remarks-stream
                                                   opt-remarks-mode print-pass-stats stats-stream
                                                   trace-json-stream))))))
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
        (setf program (make-vm-program :instructions    (if (member target '(:vm :wasm))
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
                                 :optimized-instructions optimized)))))

;;; Function call compilation (%resolve-func-sym-reg, %try-compile-*,
;;; %compile-normal-call, compile-ast (ast-call)) is in codegen-calls.lisp (loads next).
