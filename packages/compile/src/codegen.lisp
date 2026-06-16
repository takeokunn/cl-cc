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
  (optimized-instructions nil)
  (pgo-counter-plan nil)
  (branch-probability-hints nil :type list)
  (code-placement-hints nil :type list)
  (warnings nil :type list)
  (errors nil :type list)
  (coverage nil))

(defun %copy-compiler-hash-table (table)
  "Return a shallow copy of compiler context hash TABLE."
  (let ((copy (make-hash-table :test (hash-table-test table)
                               :rehash-size (hash-table-rehash-size table)
                               :rehash-threshold (hash-table-rehash-threshold table))))
    (maphash (lambda (key value)
               (setf (gethash key copy) value))
             table)
    copy))

(defstruct toplevel-compilation-snapshot
  "Mutable compiler state saved before compiling one top-level form."
  instructions
  next-register
  next-label
  env
  type-env
  safety
  block-env
  tagbody-env
  global-functions
  global-function-mv-arities
  function-conventions
  global-variables
  global-var-cache
  global-classes
  global-generics
  global-generic-params
  current-function-name
  current-function-label
  current-function-params
  current-function-simple-p
  pending-inline-policy
  top-level-p
  boxed-vars
  noescape-cons-bindings
  noescape-array-bindings
  noescape-instance-bindings
  noescape-closure-bindings
  hash-table-test-bindings
  tail-position
  diagnostics
  compile-time-value-env
  compile-time-function-env
  opts)

;;; Each entry: (field-name copy-fn)
;;;   field-name  — the short field name shared between ctx-<field> and
;;;                 toplevel-compilation-snapshot-<field> accessors
;;;   copy-fn     — unary copy function symbol, or NIL for scalars (no copy)
;;;
;;; %snapshot-toplevel-compilation-state and %restore-toplevel-compilation-state
;;; iterate this table via *toplevel-snapshot-slot-fns* (resolved at load time),
;;; avoiding 60+ lines of repetitive setf forms.
(defparameter *toplevel-snapshot-slots*
  '((instructions              copy-list)
    (next-register             nil)
    (next-label                nil)
    (env                       copy-tree)
    (type-env                  nil)
    (safety                    nil)
    (block-env                 copy-tree)
    (tagbody-env               copy-tree)
    (global-functions          %copy-compiler-hash-table)
    (global-function-mv-arities %copy-compiler-hash-table)
    (function-conventions      %copy-compiler-hash-table)
    (global-variables          %copy-compiler-hash-table)
    (global-var-cache          copy-list)
    (global-classes            %copy-compiler-hash-table)
    (global-generics           %copy-compiler-hash-table)
    (global-generic-params     %copy-compiler-hash-table)
    (current-function-name     nil)
    (current-function-label    nil)
    (current-function-params   copy-list)
    (current-function-simple-p nil)
    (pending-inline-policy     nil)
    (top-level-p               nil)
    (boxed-vars                copy-list)
    (noescape-cons-bindings    copy-tree)
    (noescape-array-bindings   copy-tree)
    (noescape-instance-bindings copy-tree)
    (noescape-closure-bindings  copy-tree)
    (hash-table-test-bindings  copy-tree)
    (tail-position             nil)
    (diagnostics               copy-list))
  "Fields shared between compiler-context and toplevel-compilation-snapshot.
Each entry (field-name copy-fn): copy-fn NIL = scalar, else a unary copy function.")

(defun %build-snapshot-slot-fns ()
  "Resolve accessor/writer functions for *toplevel-snapshot-slots* once at load time.
Returns a list of (ctx-reader snap-writer snap-reader ctx-writer copy-fn) tuples."
  (mapcar (lambda (entry)
            (destructuring-bind (field copy-fn) entry
              (let* ((ctx-sym  (intern (format nil "CTX-~A" field) :cl-cc/compile))
                     (snap-sym (intern (format nil "TOPLEVEL-COMPILATION-SNAPSHOT-~A" field) :cl-cc/compile)))
                (list (fdefinition ctx-sym)
                      (fdefinition (list 'setf snap-sym))
                      (fdefinition snap-sym)
                      (fdefinition (list 'setf ctx-sym))
                      (and copy-fn (fdefinition copy-fn))))))
          *toplevel-snapshot-slots*))

;;; Resolved at load time to avoid per-call fdefinition lookups.
(defvar *toplevel-snapshot-slot-fns* nil)

(defun %ensure-snapshot-slot-fns ()
  (or *toplevel-snapshot-slot-fns*
      (setf *toplevel-snapshot-slot-fns* (%build-snapshot-slot-fns))))

(defun %snapshot-toplevel-compilation-state (ctx opts)
  "Snapshot CTX and dynamic top-level compilation state before one FORM."
  (let ((snap (make-toplevel-compilation-snapshot
               :compile-time-value-env    (copy-list *compile-time-value-env*)
               :compile-time-function-env (copy-list *compile-time-function-env*)
               :opts                      (copy-list opts))))
    (dolist (fns (%ensure-snapshot-slot-fns) snap)
      (destructuring-bind (ctx-reader snap-writer snap-reader ctx-writer copy-fn) fns
        (declare (ignore snap-reader ctx-writer))
        (let* ((raw (funcall ctx-reader ctx))
               (val (if copy-fn (funcall copy-fn raw) raw)))
          (funcall snap-writer val snap))))))

(defun %restore-toplevel-compilation-state (ctx snapshot)
  "Restore CTX and dynamic state from SNAPSHOT. Returns restored compile opts."
  (dolist (fns (%ensure-snapshot-slot-fns))
    (destructuring-bind (ctx-reader snap-writer snap-reader ctx-writer copy-fn) fns
      (declare (ignore ctx-reader snap-writer))
      (let* ((raw (funcall snap-reader snapshot))
             (val (if copy-fn (funcall copy-fn raw) raw)))
        (funcall ctx-writer val ctx))))
  (setf *compile-time-value-env*    (copy-list (toplevel-compilation-snapshot-compile-time-value-env snapshot)))
  (setf *compile-time-function-env* (copy-list (toplevel-compilation-snapshot-compile-time-function-env snapshot)))
  (copy-list (toplevel-compilation-snapshot-opts snapshot)))

(defun %type-check-form (ctx ast type-env type-check)
  "Run the type checker on AST in TYPE-ENV, honoring TYPE-CHECK strictness.
Returns the inferred type, or NIL on failure (structured warning recorded unless :strict)."
  (handler-case
      (type-check-ast ast type-env)
    (error (e)
      (if (eq type-check :strict)
          (error e)
          (progn
            (%emit-compile-warning ctx
                                   (format nil "Type check warning: ~A" e)
                                   :error-code "W0002")
            nil)))))

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

(defun %make-compile-opts (&key pass-pipeline speed (safety 1) (space 0) (debug 0) (inline-threshold-scale 1) block-compile 
                                    opt-bisect-limit
                                    print-pass-timings timing-stream
                                    print-opt-remarks opt-remarks-stream (opt-remarks-mode :all)
                                    print-pass-stats stats-stream trace-json-stream werror werror-categories)
  "Build a compilation options plist suitable for APPLYing to compile-*/optimize-* functions."
  (let ((resolved-speed (or speed (%global-optimize-quality 'speed))))
  (list :pass-pipeline       pass-pipeline
        :speed               resolved-speed
        :safety              safety
        :space               space
        :debug               debug
        :inline-threshold-scale inline-threshold-scale
        :block-compile       block-compile
        :opt-bisect-limit    opt-bisect-limit
        :print-pass-timings  print-pass-timings
        :timing-stream       timing-stream
        :print-opt-remarks   print-opt-remarks
        :opt-remarks-stream  opt-remarks-stream
        :opt-remarks-mode    opt-remarks-mode
        :print-pass-stats    print-pass-stats
        :stats-stream        stats-stream
        :trace-json-stream   trace-json-stream
        :werror              werror
        :werror-categories   werror-categories)))

(defun %ast-declarations-for-optimize-policy (ast)
  "Return declaration list associated with AST when available."
  (cond
    ((typep ast 'ast-defun) (ast-defun-declarations ast))
    ((typep ast 'ast-lambda) (ast-lambda-declarations ast))
    ((typep ast 'ast-let) (ast-let-declarations ast))
    (t nil)))

(defun %ast-forward-reference-names (ast)
  "Return forward-reference declaration names carried by AST."
  (loop for decl in (%ast-declarations-for-optimize-policy ast)
        when (and (consp decl) (eq (car decl) 'forward-reference))
          append (remove-if-not #'symbolp (cdr decl))))

(defun %ast-branch-probability-hints (ast)
  "Return :LIKELY/:UNLIKELY declaration hints carried by AST."
  (loop for decl in (%ast-declarations-for-optimize-policy ast)
        when (and (consp decl)
                  (eq (car decl) 'branch-probability)
                  (member (cadr decl) '(:likely :unlikely) :test #'eq))
          collect (cadr decl)))

(defun %ast-code-placement-hints (ast)
  "Return :HOT/:COLD code placement declarations carried by AST."
  (loop for decl in (%ast-declarations-for-optimize-policy ast)
        when (and (consp decl)
                  (member (car decl) '(hot cold) :test #'eq))
          collect (ecase (car decl)
                    (hot :hot)
                    (cold :cold))))

(defun %diagnostics-after-werror (diagnostics opts)
  "Split DIAGNOSTICS into warnings and promoted errors using OPTS."
  (let ((cl-cc/parse:*werror-p* (getf opts :werror))
        (cl-cc/parse:*werror-categories*
          (mapcar (lambda (category) (string-downcase (string category)))
                  (getf opts :werror-categories)))
        (warnings nil)
        (errors nil))
    (dolist (diag diagnostics)
      (let ((effective (cl-cc/parse::%maybe-promote-warning-diagnostic diag)))
        (if (eq (cl-cc/parse:diagnostic-severity effective) :error)
            (push effective errors)
            (push effective warnings))))
    (values (nreverse warnings) (nreverse errors))))

(defun %maybe-bump-opts-speed-from-ast (opts ast)
  "Merge local `(declare (optimize (speed ...)))` into OPTS conservatively.

Uses max(current-speed, local-speed) when local speed is an integer."
  (let* ((decls (%ast-declarations-for-optimize-policy ast))
         (local-speed (and decls (%local-optimize-quality decls 'speed)))
         (current-speed (getf opts :speed)))
    (when (integerp local-speed)
      (setf (getf opts :speed)
            (if (integerp current-speed)
                (max current-speed local-speed)
                local-speed)))
    opts))

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

(defun %merge-result-warnings-into-context (ctx result)
  "Merge RESULT warnings back into CTX diagnostic storage."
  (setf (ctx-diagnostics ctx)
        (append (reverse (compilation-result-warnings result))
                (ctx-diagnostics ctx))))

(defun %lower-toplevel-form-to-ast (form)
  "Expand FORM and lower it into an optimized AST node."
  (let* ((expanded (if (typep form 'ast-node)
                       form
                       (cl-cc/expand:compiler-macroexpand-all form)))
         (ast (if (typep expanded 'ast-node)
                  expanded
                  (lower-sexp-to-ast expanded))))
    (optimize-ast ast)))

(defun %update-toplevel-type-state (ctx ast type-env type-check best-effort-type)
  "Return updated type metadata after visiting AST as a top-level form.
Values: inferred-type, updated-type-env."
  (let ((last-type nil)
        (next-type-env type-env))
    (when type-check
      (setf last-type (%type-check-form ctx ast type-env type-check)))
    (setf next-type-env (%extend-type-env-for-defvar ast next-type-env best-effort-type))
    (setf next-type-env (%extend-type-env-for-defun ast next-type-env best-effort-type))
    (values last-type next-type-env)))

(defun %compile-toplevel-ast-into-context (ast ctx target type-check opts)
  "Compile AST into CTX, preferring the CPS VM path when allowed.
Returns two values: result register and CPS form used for the AST."
  (let* ((last-cps (and (eq target :vm)
                        (%cps-vm-compile-safe-ast-p ast)
                        (cps-transform-ast* ast)))
         (cps-result (and (eq target :vm)
                          (%maybe-compile-toplevel-form-via-cps
                           ast type-check (ctx-safety ctx) opts))))
    (if cps-result
        (progn
          (setf (ctx-instructions ctx)
                (append (reverse (%result-vm-instructions-without-halt cps-result))
                        (ctx-instructions ctx)))
          (%merge-result-warnings-into-context ctx cps-result)
          (values (vm-program-result-register
                   (compilation-result-program cps-result))
                  last-cps))
        (values (compile-ast ast ctx) last-cps))))

(defmethod compile-ast ((node ast-handler-case) ctx)
  "Compile handler-case with a PC→handler side table instead of push/pop ops."
  (setf (ctx-tail-position ctx) nil)
  (let* ((clauses           (ast-handler-case-clauses node))
         (result-reg        (make-register ctx))
         (normal-exit-label (make-label ctx "handler_case_exit"))
         (handler-infos     (loop repeat (length clauses)
                                   collect (list (make-label ctx "handler")
                                                 (make-register ctx))))
         (protected-form    (ast-handler-case-form node))
         (prefix            (ctx-instructions ctx))
         (form-result       (compile-ast protected-form ctx))
         (body-rev          (ldiff (ctx-instructions ctx) prefix))
         (body-instructions (nreverse (copy-list body-rev))))
    (emit ctx (make-vm-move :dst result-reg :src form-result))
    (when body-instructions
      (loop for clause in clauses
            for info in handler-infos
            do (%record-exception-table-entry
                (first body-instructions)
                (car (last body-instructions))
                (first info)
                (first clause)
                (second info))))
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

(defun %process-toplevel-form (form ctx target type-env type-check safety opts compiled-asts)
  "Compile one top-level FORM and return updated compilation state.
Values: last-reg, last-type, last-cps, updated-type-env, updated-compiled-asts."
  (let ((forward-reference-names (%forward-reference-names-from-form form)))
    (when forward-reference-names
      (%declare-forward-references-in-context forward-reference-names ctx)
      (return-from %process-toplevel-form
        (values nil nil nil type-env compiled-asts))))
  (let* ((ast (%lower-toplevel-form-to-ast form))
          (last-reg nil) (last-type nil) (last-cps nil))
     (setf (ctx-safety ctx) (or (%global-optimize-quality 'safety) safety))
     (%maybe-bump-opts-speed-from-ast opts ast)
    (when (typep ast 'ast-defvar)
      (setf (gethash (ast-defvar-name ast) (ctx-global-variables ctx)) t))
    (let ((forward-names (%ast-forward-reference-names ast)))
      (when forward-names
        (%declare-forward-references-in-context forward-names ctx)))
    (when (typep ast 'ast-defun)
      (push (cons (ast-defun-name ast) ast) *compile-time-function-env*))
    (push ast compiled-asts)
    (multiple-value-setq (last-type type-env)
      (%update-toplevel-type-state ctx ast type-env type-check
                                   (lambda (a e) (ignore-errors (type-check-ast a e)))))
    (setf (ctx-type-env ctx) type-env)
     (%maybe-extend-ct-value-env ast)
     (multiple-value-setq (last-reg last-cps)
       (%compile-toplevel-ast-into-context ast ctx target type-check opts))
     (values last-reg last-type last-cps type-env compiled-asts)))

(defun %maybe-signal-strict-no-alloc-error (errors opts)
  (when (getf opts :strict-no-alloc)
    (let ((entry (find-if (lambda (e) (typep (getf e :condition) 'no-allocation-violation)) errors)))
      (when entry
        (error (getf entry :condition))))))

(defun %determine-program-convention (function-conventions)
  "Scan FUNCTION-CONVENTIONS hash table and return the program calling convention.
Returns :internal when all conventions are :internal; :external otherwise."
  (let ((has-external-p nil)
        (has-internal-p nil))
    (maphash (lambda (_ convention)
               (declare (ignore _))
               (case convention
                 (:external (setf has-external-p t))
                 (:internal (setf has-internal-p t))))
             function-conventions)
    (if (and has-internal-p (not has-external-p)) :internal :external)))

(defun %finalize-toplevel-compilation (ctx target last-reg last-type last-cps compiled-asts opts errors compilation-tier)
  "Finalize CTX after all top-level forms have been compiled."
  (%maybe-signal-strict-no-alloc-error errors opts)
  (when last-reg
    (emit ctx (make-vm-halt :reg last-reg)))
  (when *repl-capture-label-counter*
    (setf *repl-capture-label-counter* (ctx-next-label ctx)))
  (let* ((function-conventions (ctx-function-conventions ctx))
         (program-convention (%determine-program-convention function-conventions))
         (raw-instructions (nreverse (ctx-instructions ctx)))
         (instructions (if (eq target :vm)
                           (%insert-osr-entry-markers raw-instructions)
                           raw-instructions))
         (optimized nil)
         (leaf-p    nil)
         (program   nil)
         (deopt-info nil)
         (osr-entry-points nil))
    (multiple-value-setq (optimized leaf-p)
      (let ((cl-cc/optimize:*skip-optimizer-passes*
               (or cl-cc/optimize:*skip-optimizer-passes*
                   (zerop compilation-tier))))
        (apply #'optimize-instructions instructions opts)))
    (multiple-value-setq (deopt-info osr-entry-points)
      (%build-deopt-info instructions))
    (setf program (make-vm-program :instructions (if (or (eq target :vm) (eq target :wasm))
                                                       instructions
                                                       (or optimized instructions))
                                    :result-register last-reg
                                    :leaf-p          leaf-p
                                     :calling-convention program-convention
                                     :function-conventions function-conventions
                                      :deopt-info deopt-info
                                      :osr-entry-points osr-entry-points
                                      :load-time-value-cells (nreverse (copy-list *load-time-value-cells*))
                                      :compilation-tier compilation-tier))
    (cl-cc/vm::vm-register-program-exception-table
     program
     (%build-exception-table (vm-program-instructions program)))
    (multiple-value-bind (warnings promoted-errors)
        (%diagnostics-after-werror (nreverse (ctx-diagnostics ctx)) opts)
      (let ((compiled-ast-list (nreverse compiled-asts)))
        (make-compilation-result :program                program
                               :assembly               (emit-assembly program :target target)
                               :globals                (ctx-global-functions ctx)
                               :type                   last-type
                               :type-env               (ctx-type-env ctx)
                                :cps                    last-cps
                                :ast                    compiled-ast-list
                                 :vm-instructions        instructions
                                 :optimized-instructions optimized
                                 :branch-probability-hints (mapcan #'%ast-branch-probability-hints (copy-list compiled-ast-list))
                                 :code-placement-hints (mapcan #'%ast-code-placement-hints (copy-list compiled-ast-list))
                                 :warnings               warnings
                                 :errors                 (append (nreverse errors) promoted-errors))))))


(defun compile-toplevel-forms (forms &key (target :x86_64) type-check (safety 1)
                                          speed (inline-threshold-scale 1)
                                          block-compile opt-bisect-limit
                                          pass-pipeline print-pass-timings timing-stream coverage
                                          print-opt-remarks opt-remarks-stream (opt-remarks-mode :all)
                                          print-pass-stats stats-stream trace-json-stream
                                          verify-transforms werror werror-categories (compilation-tier 1)
                                          &allow-other-keys)
  "Compile a list of top-level forms (e.g., from a source file).
Handles defun, defvar, and expression forms.
Returns a compilation-result struct with program, assembly, and globals.
Security-mitigation keywords (:retpoline :spectre-mitigations :stack-protector
:shadow-stack :asan :msan :tsan :ubsan :hwasan) are accepted via &allow-other-keys and ignored."
  (declare (ignore coverage verify-transforms))
  (let ((ctx           (make-instance 'compiler-context :safety safety :target target))
        (last-reg      nil)
        (last-type     nil)
        (last-cps      nil)
        (compiled-asts nil)
        (errors        nil)
        (*string-literal-pool* (make-hash-table :test #'equal))
         (type-env      (type-env-empty))
         (opts          (%make-compile-opts :pass-pipeline pass-pipeline
                                              :speed speed
                                              :opt-bisect-limit opt-bisect-limit
                                              :inline-threshold-scale inline-threshold-scale
                                              :block-compile block-compile
                                            :print-pass-timings print-pass-timings
                                          :timing-stream timing-stream
                                          :print-opt-remarks print-opt-remarks
                                          :opt-remarks-stream opt-remarks-stream
                                           :opt-remarks-mode opt-remarks-mode
                                             :print-pass-stats print-pass-stats
                                             :stats-stream stats-stream
                                             :trace-json-stream trace-json-stream
                                             :werror werror
                                             :werror-categories werror-categories)))
    (let ((*compile-time-value-env*    nil)
           (*compile-time-function-env* nil)
            (*pending-exception-table-entries* nil)
             (*next-exception-table-entry-order* 0)
             (*forward-reference-patch-table* (make-hash-table :test #'equal))
             (*forward-declared-functions* nil)
             (*unresolved-forward-refs* nil)
             (*load-time-value-cells* nil)
             (*next-load-time-value-cell-id* 0)
             (cl-cc/parse:*werror-p* werror)
             (cl-cc/parse:*werror-categories*
               (mapcar (lambda (category) (string-downcase (string category)))
                       werror-categories)))
       (loop for form in forms
             for form-index from 0
             do (unless (and (consp form) (eq (car form) 'in-package))
                   (let ((snapshot (%snapshot-toplevel-compilation-state ctx opts))
                         (string-literal-pool-snapshot
                           (%copy-string-literal-pool *string-literal-pool*)))
                     (handler-case
                         (multiple-value-setq (last-reg last-type last-cps type-env compiled-asts)
                           (%process-toplevel-form form ctx target type-env type-check safety opts compiled-asts))
                       (error (e)
                         (setf opts (%restore-toplevel-compilation-state ctx snapshot))
                          (setf *string-literal-pool* string-literal-pool-snapshot)
                         (push (list :form-index form-index
                                     :form form
                                     :condition e
                                     :message (princ-to-string e))
                               errors))))))
       (setf (ctx-type-env ctx) type-env)
        (resolve-forward-references (ctx-global-functions ctx) :errorp t)
          (%finalize-toplevel-compilation ctx target last-reg last-type last-cps compiled-asts opts errors compilation-tier))))

;;; Function call compilation (%resolve-func-sym-reg, %try-compile-*,
;;; %compile-normal-call, compile-ast (ast-call)) is in codegen-calls.lisp (loads next).
;;; FR-860/FR-861 numeric contagion helpers and FR-542 hot/cold annotations
;;; are in codegen-numeric-hints.lisp (loads after codegen-calls).
