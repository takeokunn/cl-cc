(in-package :cl-cc)
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

(defstruct compilation-result
  "Result of compiling expressions or top-level forms."
  (program nil)
  (assembly nil)
  (globals nil)
  (type nil)
  (cps nil))

(defun compile-toplevel-forms (forms &key (target :x86_64))
  "Compile a list of top-level forms (e.g., from a source file).
Handles defun, defvar, and expression forms.
Returns a compilation-result struct with program, assembly, and globals."
  (let* ((ctx (make-instance 'compiler-context))
         (last-reg nil))
    (dolist (form forms)
      (let* ((expanded (if (typep form 'ast-node)
                           form
                           (compiler-macroexpand-all form)))
             (ast (if (typep expanded 'ast-node)
                      expanded
                      (lower-sexp-to-ast expanded))))
        (setf last-reg (compile-ast ast ctx))))
    (when last-reg
      (emit ctx (make-vm-halt :reg last-reg)))
    (when *repl-capture-label-counter*
      (setf *repl-capture-label-counter* (ctx-next-label ctx)))
    (let* ((instructions (nreverse (ctx-instructions ctx)))
           (optimized (optimize-instructions instructions))
           (program (make-vm-program
                     :instructions optimized
                     :result-register last-reg)))
      (make-compilation-result :program program
                               :assembly (emit-assembly program :target target)
                               :globals (ctx-global-functions ctx)))))

;;; ── Exception handling: catch / throw / unwind-protect / handler-case ────

(defmethod compile-ast ((node ast-catch) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let ((tag-reg (compile-ast (ast-catch-tag node) ctx))
        (result-reg (make-register ctx))
        (handler-label (make-label ctx "catch_handler"))
        (end-label (make-label ctx "catch_end")))
    ;; Establish catch frame with tag
    (emit ctx (make-vm-establish-catch
               :tag-reg tag-reg
               :handler-label handler-label
               :result-reg result-reg))
    ;; Compile body normally
    (let ((body-result (let ((last nil))
                         (dolist (form (ast-catch-body node))
                           (setf last (compile-ast form ctx)))
                         last)))
      (emit ctx (make-vm-move :dst result-reg :src body-result)))
    ;; Remove catch frame on normal exit
    (emit ctx (make-vm-remove-handler))
    (emit ctx (make-vm-jump :label end-label))
    ;; Handler label: thrown value is already in result-reg
    (emit ctx (make-vm-label :name handler-label))
    (emit ctx (make-vm-label :name end-label))
    result-reg))

(defmethod compile-ast ((node ast-throw) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let ((tag-reg (compile-ast (ast-throw-tag node) ctx))
        (value-reg (compile-ast (ast-throw-value node) ctx)))
    ;; Emit throw instruction — will unwind to matching catch
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
  (let* ((clauses (ast-handler-case-clauses node))
         (result-reg (make-register ctx))
         (normal-exit-label (make-label ctx "handler_case_exit"))
         (handler-infos (mapcar (lambda (clause)
                                  (declare (ignore clause))
                                  (list (make-label ctx "handler")
                                        (make-register ctx)))
                                clauses)))
    ;; 1. Establish handlers in reverse so first clause is on top
    (loop for clause in (reverse clauses)
          for info in (reverse handler-infos)
          do (emit ctx (make-vm-establish-handler
                        :handler-label (first info)
                        :result-reg (second info)
                        :error-type (first clause))))
    ;; 2. Protected form
    (let ((form-result (compile-ast (ast-handler-case-form node) ctx)))
      (emit ctx (make-vm-move :dst result-reg :src form-result)))
    ;; 3. Remove all handlers
    (dotimes (i (length clauses))
      (emit ctx (make-vm-remove-handler)))
    (emit ctx (make-vm-jump :label normal-exit-label))
    ;; 4. Handler bodies
    (loop for clause in clauses
          for info in handler-infos
          do (let* ((var (second clause))
                    (body (cddr clause))
                    (handler-label (first info))
                    (error-reg (second info))
                    (old-env (ctx-env ctx)))
               (emit ctx (make-vm-label :name handler-label))
               (unwind-protect
                    (progn
                      (when var
                        (push (cons var error-reg) (ctx-env ctx)))
                      (let ((last-reg nil))
                        (if body
                            (dolist (form body)
                              (setf last-reg (compile-ast form ctx)))
                            (setf last-reg error-reg))
                        (emit ctx (make-vm-move :dst result-reg :src last-reg))))
                 (setf (ctx-env ctx) old-env))
               (emit ctx (make-vm-jump :label normal-exit-label))))
    (emit ctx (make-vm-label :name normal-exit-label))
    result-reg))

;;; ── Multiple values ───────────────────────────────────────────────────────

(defmethod compile-ast ((node ast-values) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let* ((src-regs (mapcar (lambda (form) (compile-ast form ctx))
                           (ast-values-forms node)))
         (dst (make-register ctx)))
    (emit ctx (make-vm-values :dst dst :src-regs src-regs))
    dst))

(defmethod compile-ast ((node ast-multiple-value-bind) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let* ((vars (ast-mvb-vars node))
         (body (ast-mvb-body node))
         (old-env (ctx-env ctx)))
    (compile-ast (ast-mvb-values-form node) ctx)
    (let ((var-regs (loop for v in vars collect (make-register ctx))))
      (emit ctx (make-vm-mv-bind :dst-regs var-regs))
      (unwind-protect
           (progn
             (setf (ctx-env ctx)
                   (append (mapcar #'cons vars var-regs) (ctx-env ctx)))
             (let ((last nil))
               (dolist (form body)
                 (setf last (compile-ast form ctx)))
               last))
        (setf (ctx-env ctx) old-env)))))

(defmethod compile-ast ((node ast-apply) ctx)
  (setf (ctx-tail-position ctx) nil)
  (let* ((func-reg (compile-ast (ast-apply-func node) ctx))
         (arg-regs (mapcar (lambda (arg) (compile-ast arg ctx))
                           (ast-apply-args node)))
         (result-reg (make-register ctx)))
    (emit ctx (make-vm-apply :dst result-reg :func func-reg :args arg-regs))
    result-reg))

(defmethod compile-ast ((node ast-multiple-value-call) ctx)
  "Evaluate FUNC, collect ALL values from each arg form, apply FUNC to them."
  (setf (ctx-tail-position ctx) nil)
  (let* ((func-reg (compile-ast (ast-mv-call-func node) ctx))
         (args (ast-mv-call-args node))
         (result-reg (make-register ctx)))
    (if (null args)
        (let ((empty-reg (make-register ctx)))
          (emit ctx (make-vm-const :dst empty-reg :value nil))
          (emit ctx (make-vm-apply :dst result-reg :func func-reg :args (list empty-reg))))
        (let ((list-regs
               (mapcar (lambda (arg)
                         (emit ctx (make-vm-clear-values))
                         (let ((primary-reg (compile-ast arg ctx)))
                           (emit ctx (make-vm-ensure-values :src primary-reg))
                           (let ((lr (make-register ctx)))
                             (emit ctx (make-vm-values-to-list :dst lr))
                             lr)))
                       args)))
          (let ((combined-reg
                 (reduce (lambda (acc lr)
                           (let ((new-reg (make-register ctx)))
                             (emit ctx (make-vm-append :dst new-reg :src1 acc :src2 lr))
                             new-reg))
                         (cdr list-regs)
                         :initial-value (car list-regs))))
            (emit ctx (make-vm-apply :dst result-reg :func func-reg
                                     :args (list combined-reg))))))
    result-reg))

(defmethod compile-ast ((node ast-multiple-value-prog1) ctx)
  "Save result of first form, evaluate remaining forms, return saved result."
  (setf (ctx-tail-position ctx) nil)
  (let ((first-reg (compile-ast (ast-mv-prog1-first node) ctx))
        (result-reg (make-register ctx)))
    (emit ctx (make-vm-move :dst result-reg :src first-reg))
    (dolist (form (ast-mv-prog1-forms node))
      (compile-ast form ctx))
    result-reg))

;;; ── Function call compilation ────────────────────────────────────────────
;;; Phase 2 (*phase2-builtin-handlers*) is defined in codegen-phase2.lisp.

(defun %resolve-func-sym-reg (sym ctx)
  "Return the register holding the value of function symbol SYM.
   Uses local env binding unless the symbol is a global function, in which
   case it loads the symbol itself into a fresh register."
  (let ((entry (assoc sym (ctx-env ctx)))
        (is-global (gethash sym (ctx-global-functions ctx))))
    (if (and entry (not is-global))
        (cdr entry)
        (let ((reg (make-register ctx)))
          (emit ctx (make-vm-const :dst reg :value sym))
          reg))))

(defmethod compile-ast ((node ast-call) ctx)
  (let* ((tail (ctx-tail-position ctx))
         (func-expr (progn (setf (ctx-tail-position ctx) nil)
                           (ast-call-func node)))
         (func-sym (cond ((symbolp func-expr) func-expr)
                         ((typep func-expr 'ast-var) (ast-var-name func-expr))
                         (t nil)))
         (args (ast-call-args node))
         (result-reg (make-register ctx)))
    ;; ── Phase 1: Table-driven builtin dispatch ──────────────────────────
    ;; ~160 calling conventions via a single hash-table lookup + generic emitter.
    (when func-sym
      (let ((entry (gethash (symbol-name func-sym) *builtin-registry*)))
        (when entry
          (let ((result (emit-registered-builtin entry args result-reg ctx)))
            (when result
              (return-from compile-ast result))))))
    ;; ── Phase 2: AST-introspecting builtins — Prolog-style dispatch ─────
    ;; Each handler returns result-reg on success, nil to fall through.
    (when func-sym
      (let ((handler (gethash (symbol-name func-sym) *phase2-builtin-handlers*)))
        (when handler
          (let ((result (funcall handler args result-reg ctx)))
            (when result
              (return-from compile-ast result))))))
    ;; ── Normal function call ─────────────────────────────────────────────
    (let* ((raw-func-reg
             (cond ((symbolp func-expr) (%resolve-func-sym-reg func-expr ctx))
                   ((typep func-expr 'ast-var)
                    (%resolve-func-sym-reg (ast-var-name func-expr) ctx))
                   (t (compile-ast func-expr ctx))))
           ;; Unbox labels functions: extract closure from cons cell box
           (func-reg (if (and func-sym (assoc func-sym *labels-boxed-fns*))
                         (let ((unboxed (make-register ctx)))
                           (emit ctx (make-vm-car :dst unboxed :src raw-func-reg))
                           unboxed)
                         raw-func-reg))
           (arg-regs (mapcar (lambda (arg) (compile-ast arg ctx)) args)))
      (if (and func-sym (gethash func-sym (ctx-global-generics ctx)))
          (emit ctx (make-vm-generic-call :dst result-reg :gf-reg func-reg :args arg-regs))
          (if tail
              (emit ctx (make-vm-tail-call :dst result-reg :func func-reg :args arg-regs))
              (emit ctx (make-vm-call :dst result-reg :func func-reg :args arg-regs))))
      result-reg)))

;;; ── Function references and local function bindings ──────────────────────

(defun %compile-body-with-tail (body tail ctx)
  "Compile BODY forms with tail-position tracking. Returns the last result register."
  (let ((last-reg nil))
    (dolist (form body)
      (setf (ctx-tail-position ctx)
            (if (eq form (car (last body))) tail nil))
      (setf last-reg (compile-ast form ctx)))
    last-reg))

(defun %compile-closure-body (ctx params param-regs body-forms env)
  "Emit the body of a local function (flet/labels closure).
   Binds PARAMS to PARAM-REGS in CTX-ENV under base ENV, compiles BODY-FORMS
   in tail position, emits vm-ret with the last result register.
   Restores CTX-ENV to ENV on exit."
  (let ((param-bindings (loop for param in params
                               for param-reg in param-regs
                               collect (cons param param-reg))))
    (setf (ctx-env ctx) (append param-bindings env))
    (let ((last-reg nil))
      (dolist (form body-forms)
        (setf (ctx-tail-position ctx)
              (if (eq form (car (last body-forms))) t nil))
        (setf last-reg (compile-ast form ctx)))
      (setf (ctx-tail-position ctx) nil)
      (emit ctx (make-vm-ret :reg last-reg)))))

(defmethod compile-ast ((node ast-function) ctx)
  "Compile #'name — look up function by name, returning a closure reference."
  (setf (ctx-tail-position ctx) nil)
  (let* ((name (ast-function-name node))
         (dst (make-register ctx)))
    (cond
      ((symbolp name)
       (let ((entry (assoc name (ctx-env ctx))))
         (if entry
             (if (assoc name *labels-boxed-fns*)
                 (emit ctx (make-vm-car :dst dst :src (cdr entry)))
                 (emit ctx (make-vm-move :dst dst :src (cdr entry))))
             (emit ctx (make-vm-func-ref :dst dst :label (format nil "~A" name))))))
      ((and (consp name) (eq (car name) 'setf))
       (emit ctx (make-vm-func-ref :dst dst :label (format nil "SETF_~A" (second name)))))
      (t
       (error "Invalid function name: ~S" name)))
    dst))

(defmethod compile-ast ((node ast-flet) ctx)
  "Compile flet: non-recursive local function bindings."
  (let* ((tail (ctx-tail-position ctx))
         (bindings (ast-flet-bindings node))
         (body (ast-flet-body node))
         (old-env (ctx-env ctx)))
    (unwind-protect
         (progn
           (let ((func-bindings nil))
             (dolist (binding bindings)
               (let* ((name (first binding))
                      (params (second binding))
                      (body-forms (cddr binding))
                      (func-label (make-label ctx "flet_fn"))
                      (closure-reg (make-register ctx)))
                 (let* ((body-ast (make-ast-progn :forms body-forms))
                        (free-vars (find-free-variables body-ast))
                        (captured-vars (mapcar (lambda (v) (cons v (lookup-var ctx v)))
                                               (remove-if-not
                                                (lambda (v) (assoc v old-env))
                                                (set-difference free-vars params)))))
                   (let ((param-regs (loop for i from 0 below (length params)
                                           collect (make-register ctx)))
                         (skip-label (make-label ctx "flet_skip")))
                     (emit ctx (make-vm-closure :dst closure-reg :label func-label
                                                :params param-regs :captured captured-vars))
                     (push (cons name closure-reg) func-bindings)
                     (emit ctx (make-vm-jump :label skip-label))
                     (emit ctx (make-vm-label :name func-label))
                     (%compile-closure-body ctx params param-regs body-forms old-env)
                     (emit ctx (make-vm-label :name skip-label))))))
             (setf (ctx-env ctx) (append (nreverse func-bindings) old-env)))
           (%compile-body-with-tail body tail ctx))
      (setf (ctx-env ctx) old-env))))

(defmethod compile-ast ((node ast-labels) ctx)
  "Compile labels: mutually recursive local function bindings via cons-cell boxing."
  (let* ((tail (ctx-tail-position ctx))
         (bindings (ast-labels-bindings node))
         (body (ast-labels-body node))
         (old-env (ctx-env ctx))
         (old-labels-boxes *labels-boxed-fns*))
    (let ((body-result-reg
            (unwind-protect
                 (progn
                   ;; Phase 1: Create mutable boxes for each function
                   (let ((func-infos nil)
                         (nil-reg (make-register ctx)))
                     (emit ctx (make-vm-const :dst nil-reg :value nil))
                     (dolist (binding bindings)
                       (let* ((name (first binding))
                              (params (second binding))
                              (func-label (make-label ctx "labels_fn"))
                              (closure-reg (make-register ctx))
                              (box-reg (make-register ctx)))
                         (emit ctx (make-vm-cons :dst box-reg
                                                 :car-src nil-reg :cdr-src nil-reg))
                         (push (list name func-label closure-reg box-reg params) func-infos)))
                     (let ((forward-env (mapcar (lambda (info) (cons (first info) (fourth info)))
                                                func-infos)))
                       (setf *labels-boxed-fns* (append forward-env old-labels-boxes))
                       (setf (ctx-env ctx) (append forward-env old-env))
                       ;; Phase 2: Create closures and fill boxes
                       (dolist (info (nreverse func-infos))
                         (destructuring-bind (name func-label closure-reg box-reg params) info
                           (let* ((binding-data (find name bindings :key #'first))
                                  (body-forms (cddr binding-data))
                                  (body-ast (make-ast-progn :forms body-forms))
                                  (free-vars (set-difference (find-free-variables body-ast) params))
                                  (captured-vars (mapcar (lambda (v) (cons v (lookup-var ctx v)))
                                                         (remove-if-not (lambda (v) (assoc v (ctx-env ctx)))
                                                                        free-vars)))
                                  (param-regs (loop for i from 0 below (length params)
                                                    collect (make-register ctx)))
                                  (skip-label (make-label ctx "labels_skip")))
                             (emit ctx (make-vm-closure :dst closure-reg :label func-label
                                                        :params param-regs :captured captured-vars))
                             (emit ctx (make-vm-rplaca :cons box-reg :val closure-reg))
                             (emit ctx (make-vm-jump :label skip-label))
                             (emit ctx (make-vm-label :name func-label))
                             (%compile-closure-body ctx params param-regs body-forms
                                                   (append forward-env old-env))
                             (emit ctx (make-vm-label :name skip-label)))))
                       ;; Phase 3: Compile body with box-resolved env
                       (let ((func-env (mapcar (lambda (binding)
                                                 (cons (first binding)
                                                       (lookup-var ctx (first binding))))
                                               bindings)))
                         (setf (ctx-env ctx) (append func-env old-env)))
                       (%compile-body-with-tail body tail ctx))))
              (setf (ctx-env ctx) old-env)
              (setf *labels-boxed-fns* old-labels-boxes))))
      body-result-reg)))

;;; ── Assembly and type utilities ───────────────────────────────────────────

(defun target-instance (target)
  (ecase target
    (:x86_64 (make-instance 'x86-64-target))
    (:aarch64 (make-instance 'aarch64-target))
    (:vm nil)))

(defun emit-assembly (program &key (target :x86_64))
  (when (eq target :vm)
    (return-from emit-assembly ""))
  (when (eq target :wasm)
    (return-from emit-assembly (compile-to-wasm-wat program)))
  (let ((target-object (target-instance target)))
    (when (and (typep target-object 'x86-64-target)
               (vm-program-instructions program))
      (let* ((instructions (vm-program-instructions program))
             (ra (allocate-registers instructions *x86-64-calling-convention*)))
        (setf (target-regalloc target-object) ra)
        (setf program (make-vm-program
                       :instructions (regalloc-instructions ra)
                       :result-register (vm-program-result-register program)))))
    (with-output-to-string (s)
      (format s "; CL-CC bootstrap assembly (~A)~%" target)
      (format s "clcc_entry:~%")
      (dolist (inst (vm-program-instructions program))
        (emit-instruction target-object inst s)))))

(defun type-check-ast (ast &optional (env (cl-cc/type:type-env-empty)))
  "Run type inference on AST. Returns inferred type or signals type error."
  (cl-cc/type:reset-type-vars!)
  (multiple-value-bind (type subst)
      (cl-cc/type:infer ast env)
    (cl-cc/type:type-substitute type subst)))
