;;;; compile/pipeline.lisp - Top-Level Compilation API
(in-package :cl-cc/pipeline)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Internal helpers
;;; ─────────────────────────────────────────────────────────────────────────

(defun %opts->optimize-kwargs (opts)
  "Extract the pass/stats/remarks subset of OPTS as a keyword plist for
optimize-instructions."
  (list :pass-pipeline      (pipeline-opts-pass-pipeline opts)
        :speed              (pipeline-opts-speed opts)
        :inline-threshold-scale (pipeline-opts-inline-threshold-scale opts)
        :block-compile      (pipeline-opts-block-compile opts)
        :print-pass-timings (pipeline-opts-print-pass-timings opts)
        :timing-stream      (pipeline-opts-timing-stream opts)
        :print-pass-stats   (pipeline-opts-print-pass-stats opts)
        :stats-stream       (pipeline-opts-stats-stream opts)
        :trace-json-stream  (pipeline-opts-trace-json-stream opts)
        :print-opt-remarks  (pipeline-opts-print-opt-remarks opts)
         :opt-remarks-stream (pipeline-opts-opt-remarks-stream opts)
         :opt-remarks-mode   (pipeline-opts-opt-remarks-mode opts)
         :opt-bisect-limit   (pipeline-opts-opt-bisect-limit opts)))

(defun %opts->codegen-kwargs (opts)
  "Extract backend hardening/sanitizer options from OPTS."
  (list :retpoline           (pipeline-opts-retpoline opts)
        :spectre-mitigations (pipeline-opts-spectre-mitigations opts)
        :stack-protector     (pipeline-opts-stack-protector opts)
        :shadow-stack        (pipeline-opts-shadow-stack opts)
        :asan                (pipeline-opts-asan opts)
        :msan                (pipeline-opts-msan opts)
        :tsan                (pipeline-opts-tsan opts)
        :ubsan               (pipeline-opts-ubsan opts)
        :hwasan              (pipeline-opts-hwasan opts)))

(defun %opts->compile-kwargs (opts)
  "Spread all OPTS fields into a keyword plist for compile-expression /
compile-toplevel-forms."
  (append (list :target     (pipeline-opts-target opts)
                :type-check (pipeline-opts-type-check opts)
                :safety     (pipeline-opts-safety opts)
                :coverage   (pipeline-opts-coverage opts)
                :verify-transforms (pipeline-opts-verify-transforms opts)
                :werror (pipeline-opts-werror opts)
                :werror-categories (pipeline-opts-werror-categories opts))
          (%opts->optimize-kwargs opts)
          (%opts->codegen-kwargs opts)))

(defun %tier-0-p (opts)
  "Return T when OPTS requests the fast Tier-0 compilation path."
  (zerop (pipeline-opts-compilation-tier opts)))

(defun %pipeline-optimize-ast (ast opts)
  "Run AST optimization unless OPTS selects Tier-0."
  (if (%tier-0-p opts) ast (optimize-ast ast)))

(defun %call-with-tiered-optimizer-policy (opts thunk)
  "Run THUNK with VM optimizer passes enabled only for Tier-1."
  (let ((cl-cc/optimize:*skip-optimizer-passes*
          (or cl-cc/optimize:*skip-optimizer-passes* (%tier-0-p opts)))
        (cl-cc/optimize:*translation-validation-enabled*
          (or cl-cc/optimize:*translation-validation-enabled*
              (pipeline-opts-verify-transforms opts)))
        (cl-cc/optimize::*optimizer-tier* (pipeline-opts-compilation-tier opts)))
    (funcall thunk)))

(defun %tier1-recompile-closure (closure target-tier)
  "Upgrade CLOSURE's owning instruction stream to Tier-1 optimized bytecode.
Also handles OSR entry compilation: when CLOSURE is a plist (:osr ...),
return normalized OSR entry metadata without recompiling."
  ;; ── OSR path: plist dispatch ──
  (when (and (consp closure) (eq (car closure) :osr))
    (return-from %tier1-recompile-closure
      (let* ((plist closure)
             (id (getf plist :id))
             (label (getf plist :label))
             (pc (getf plist :pc)))
        (list :pc pc :label label :id id :kind :bytecode-osr))))
  ;; ── Tier upgrade path: closure recompilation ──
  (when (and (>= target-tier 1)
             (cl-cc/vm:vm-closure-program-flat closure))
    (let* ((instructions (coerce (cl-cc/vm:vm-closure-program-flat closure) 'list))
           (optimized (multiple-value-bind (optimized-instrs leaf-p)
                          (let ((cl-cc/optimize:*skip-optimizer-passes* nil)
                                (cl-cc/optimize::*optimizer-tier* 1))
                            (optimize-instructions instructions))
                        (declare (ignore leaf-p))
                        (or optimized-instrs instructions)))
           (new-labels (cl-cc/vm:build-label-table optimized)))
      ;; A function's ENTRY label is referenced externally (by the closure /
      ;; sync-call dispatch), not by any internal jump, so the Tier-1 optimizer's
      ;; label-flushing pass can treat it as dead and remove it. Adopting such an
      ;; optimized program leaves vm-closure-entry-label unresolvable in the new
      ;; label table — every later call past the 50-invocation JIT threshold then
      ;; crashed with "Cannot resolve entry label" (e.g. fib(8+)). Only adopt the
      ;; recompiled program when the entry label survived; otherwise keep the
      ;; Tier-0 program (still correct) but mark tier 1 so we don't retry forever.
      (if (cl-cc/vm:vm-label-table-lookup new-labels
                                          (cl-cc/vm:vm-closure-entry-label closure))
          (setf (cl-cc/vm:vm-closure-program-flat closure) (coerce optimized 'vector)
                (cl-cc/vm:vm-closure-label-table closure) new-labels
                (cl-cc/vm:vm-closure-compilation-tier closure) 1)
          (setf (cl-cc/vm:vm-closure-compilation-tier closure) 1))))
  closure)

(setf cl-cc/vm:*vm-recompile-function-hook* #'%tier1-recompile-closure)

(defun %prepare-ast (expr)
  "Macro-expand EXPR (if not already an AST node), then lower to an AST node."
  (let ((expanded (if (typep expr 'ast-node) expr (compiler-macroexpand-all expr))))
    (if (typep expanded 'ast-node) expanded (lower-sexp-to-ast expanded))))

(defun %ast-has-source-location-p (ast)
  "Return T when AST already carries any explicit source location metadata."
  (or (ast-source-file ast)
      (ast-source-line ast)
      (ast-source-column ast)))

(defun %maybe-attach-ast-source-location (ast location)
  "Attach LOCATION=(file line col) to AST only when AST lacks source metadata."
  (when (and (typep ast 'ast-node) location (not (%ast-has-source-location-p ast)))
    (destructuring-bind (file line col) location
      (setf (ast-source-file ast) file
            (ast-source-line ast) line
            (ast-source-column ast) col)))
  ast)

(defun %compilation-result-ast-list (result)
  "Return RESULT's AST payload as a list while preserving NIL as NIL."
  (let ((asts (compilation-result-ast result)))
    (cond
      ((null asts) nil)
      ((listp asts) asts)
      (t (list asts)))))

(defun %attach-source-locations-to-result (result locations)
  "Attach LOCATIONS to RESULT's top-level AST nodes in order, if absent.
Each location is a list (source-file source-line source-column)."
  (when locations
    (let ((asts (%compilation-result-ast-list result)))
      (loop for ast in asts
            for location in locations
            do (%maybe-attach-ast-source-location ast location))))
  result)

(defun %call-with-source-file-macro-eval (source-file thunk)
  "Run THUNK with host macro evaluation for real source-file compile-string paths."
  (if source-file
      (let ((cl-cc/expand:*macro-eval-fn* #'eval))
        (funcall thunk))
      (funcall thunk)))

(defun %type-check-safe (ctx ast type-check)
  "Run type inference on AST; on error record a warning unless TYPE-CHECK is :strict."
  (when type-check
    (handler-case (type-check-ast ast)
      (error (e)
        (if (eq type-check :strict)
            (error e)
            (progn
              (push (make-diagnostic :severity :warning
                                     :message (format nil "Type check warning: ~A" e)
                                     :span (cons 0 0)
                                     :error-code "W0002")
                    (ctx-diagnostics ctx))
              nil))))))


(defun %definition-form-p (form)
  "Return T when FORM is a definition or declaration form (not CPS-batchable)."
  (and (consp form)
       (symbolp (car form))
       (member (string-upcase (symbol-name (car form)))
               *definition-and-declaration-form-heads*
                :test #'string=)
       t))

(defun %pipeline-in-package-form-p (form)
  "Return T when FORM is a top-level in-package declaration."
  (and (consp form)
       (eq (car form) 'in-package)))

(defun %lisp-top-level-source-forms-and-locations (source &optional source-file)
  "Parse SOURCE into normal Lisp sexps plus parallel top-level source locations.
Returns two values: (1) the parsed top-level forms, and (2) a list of
(source-file source-line source-column) triples aligned with those forms."
  (multiple-value-bind (cst-list diagnostics)
      (parse-cl-source source source-file)
    (declare (ignore diagnostics))
    (values
     (mapcar #'cst-to-sexp cst-list)
     (mapcar (lambda (cst-node)
               (multiple-value-bind (line col)
                   (byte-offset-to-line-col source (cst-node-start-byte cst-node))
                 (list (or source-file (cst-node-source-file cst-node))
                       line
                       col)))
             cst-list))))

(defun %non-in-package-form-locations (forms locations)
  "Drop LOCATION entries for top-level in-package forms, mirroring compilation."
  (loop for form in forms
        for location in locations
        unless (%pipeline-in-package-form-p form)
          collect location))

(defun %non-in-package-forms (forms)
  "Return FORMS with top-level in-package declarations removed."
  (remove-if #'%pipeline-in-package-form-p forms))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS rewriting for top-level form batches
;;; ─────────────────────────────────────────────────────────────────────────

(defun %pipeline-cps-safe-ast-p (target ast)
  "Return T when AST can use the CPS top-level rewrite for TARGET.

TARGET selects the backend-specific safety predicate. VM compilation can use
the CPS VM predicate, WASM currently stays on the direct path, and native
targets use the native CPS safety predicate."
  (cond
    ((eq target :vm)   (%cps-vm-compile-safe-ast-p ast))
    ((eq target :wasm) nil)
    (t                 (%cps-native-compile-safe-ast-p ast))))

(defun %maybe-cps-toplevel-form (form opts)
  "Rewrite FORM into a CPS identity-entry form when OPTS make that safe.

Top-level IN-PACKAGE forms are returned unchanged. Other forms are prepared,
optimized, checked for target-specific CPS safety, and either replaced with the
CPS entry expression or returned as the original form."
  (if (and (consp form) (eq (car form) 'in-package))
      form
      (let* ((ast      (%pipeline-optimize-ast (%prepare-ast form) opts))
             (safe-p   (%pipeline-cps-safe-ast-p (pipeline-opts-target opts) ast)))
        (if safe-p
            (%cps-identity-entry-form (cps-transform-ast* ast))
            form))))

(defun %maybe-cps-toplevel-forms (forms opts)
  "Rewrite safe top-level expression FORMS into CPS entry forms when possible.
Definition and control-effect forms stay on the direct path."
  (mapcar (lambda (f) (%maybe-cps-toplevel-form f opts)) forms))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Internal compilation stages — all accept pipeline-opts
;;; ─────────────────────────────────────────────────────────────────────────

(defun %pipeline-expression-cps-safe-p (ast opts)
  "Return T when AST may use the compile-expression CPS fast path.

OPTS supplies the target backend. The recursion guard prevents a CPS rewrite
from recursively re-entering the same fast path while compiling the generated
identity-entry expression."
  (and (not *compile-expression-cps-recursion-guard*)
       (not (%tier-0-p opts))
       (let ((target (pipeline-opts-target opts)))
         (cond
           ((eq target :vm)   (and *enable-cps-vm-primary-path*
                                   (%cps-vm-compile-safe-ast-p ast)))
           ((eq target :wasm) nil)
           (t                 (%cps-native-compile-safe-ast-p ast))))))

(defun %maybe-compile-expression-via-cps (ast opts)
  "Return (values cps cps-result) when the CPS fast path is safe, else (values nil nil)."
  (let ((cps-safe-p (%pipeline-expression-cps-safe-p ast opts)))
    (if cps-safe-p
        (let ((cps (cps-transform-ast* ast)))
          (values cps
                  (let ((*compile-expression-cps-recursion-guard* t))
                    (apply #'compile-expression (%cps-identity-entry-form cps)
                           (%opts->compile-kwargs opts)))))
        (values nil nil))))

(defun %pipeline-runtime-instructions (target full-instrs optimized-instrs)
  "Select the instruction stream embedded in the runtime VM program.

VM and WASM targets keep FULL-INSTRS so interpreter-visible profiling and
control-flow metadata remain aligned. Native targets prefer OPTIMIZED-INSTRS
when available, falling back to FULL-INSTRS."
  (if (or (eq target :vm) (eq target :wasm))
      full-instrs
      (or optimized-instrs full-instrs)))

(defun %pgo-label-position-map (instructions)
  "Return hash table label-name -> PC for INSTRUCTIONS."
  (let ((table (make-hash-table :test #'equal)))
    (loop for inst in instructions
          for pc from 0
          do (when (typep inst 'cl-cc/vm:vm-label)
               (setf (gethash (cl-cc/vm::vm-name inst) table) pc)))
    table))

(defun %pgo-block-start-pc (block instructions label->pc)
  "Resolve BLOCK's start PC in INSTRUCTIONS, preferring explicit labels."
  (or (let ((label (cl-cc/optimize:bb-label block)))
        (and label (gethash (cl-cc/vm::vm-name label) label->pc)))
      (let ((first-inst (car (cl-cc/optimize:bb-instructions block))))
        (and first-inst
             (position first-inst instructions :test #'eq)))
      0))

(defun %pgo-plan-with-runtime-keys (instructions counter-plan)
  "Attach runtime key maps (:bb-runtime-keys/:edge-runtime-keys) to COUNTER-PLAN.

The runtime keys map logical plan IDs onto VM profile keys:
- bb-runtime-keys:   (bb-id . pc)
- edge-runtime-keys: (edge-id . (kind from-pc to-pc))"
  (if (or (null instructions) (null counter-plan))
      counter-plan
      (let* ((cfg (cl-cc/optimize:cfg-build instructions))
             (label->pc (%pgo-label-position-map instructions))
             (id->block (make-hash-table :test #'eql))
             (block->pc (make-hash-table :test #'eq))
             (inst->pc (make-hash-table :test #'eq)))
        (loop for inst in instructions
              for pc from 0
              do (setf (gethash inst inst->pc) pc))
        (dolist (block (coerce (cl-cc/optimize:cfg-blocks cfg) 'list))
          (setf (gethash (cl-cc/optimize:bb-id block) id->block) block)
          (setf (gethash block block->pc)
                (%pgo-block-start-pc block instructions label->pc)))
        (let ((bb-runtime-keys
                (loop for (block-id . bb-id) in (getf counter-plan :bb-counters)
                      for block = (gethash block-id id->block)
                      for pc = (and block (gethash block block->pc))
                      when (and block (integerp pc))
                        collect (cons bb-id pc)))
              (edge-runtime-keys
                (loop for ((from-id . to-id) . edge-id) in (getf counter-plan :edge-counters)
                      for from-block = (gethash from-id id->block)
                      for to-block = (gethash to-id id->block)
                      for from-pc = (and from-block (gethash from-block block->pc))
                      for to-pc = (and to-block (gethash to-block block->pc))
                      for term = (and from-block
                                      (car (last (cl-cc/optimize:bb-instructions from-block))))
                      for term-pc = (and term (gethash term inst->pc))
                      for kind = (cond
                                   ((typep term 'cl-cc/vm:vm-jump) 'cl-cc/vm:vm-jump)
                                   ((typep term 'cl-cc/vm:vm-jump-zero) 'cl-cc/vm:vm-jump-zero)
                                   ((typep term 'cl-cc/vm:vm-call) 'cl-cc/vm:vm-call)
                                   ((typep term 'cl-cc/vm:vm-tail-call) 'cl-cc/vm:vm-tail-call)
                                   ((typep term 'cl-cc/vm:vm-ret) 'cl-cc/vm:vm-ret)
                                   (t nil))
                      when (and from-block to-block
                                kind
                                (integerp term-pc)
                                (integerp to-pc))
                        collect (cons edge-id (list kind term-pc to-pc)))))
          (append counter-plan
                  (list :bb-runtime-keys bb-runtime-keys
                        :edge-runtime-keys edge-runtime-keys))))))

(defun %build-pgo-counter-plan-from-instructions (instructions)
  "Derive a deterministic BB/edge counter plan from flat VM INSTRUCTIONS."
  (when instructions
    (let* ((cfg (cl-cc/optimize:cfg-build instructions))
           (entry (and (cl-cc/optimize:cfg-entry cfg)
                       (cl-cc/optimize:bb-id (cl-cc/optimize:cfg-entry cfg))))
            (successors
              (mapcar (lambda (block)
                        (cons (cl-cc/optimize:bb-id block)
                              (mapcar #'cl-cc/optimize:bb-id
                                      (cl-cc/optimize:bb-successors block))))
                      (coerce (cl-cc/optimize:cfg-blocks cfg) 'list))))
      (%pgo-plan-with-runtime-keys
       instructions
       (cl-cc/optimize:opt-pgo-build-counter-plan entry successors)))))

(defun %pgo-type-feedback-rows (profile-data)
  "Return FR-058 type-feedback rows from PROFILE-DATA."
  (when (consp profile-data)
    (getf profile-data :type-feedback)))

(defun %pgo-dominant-types-by-pc (profile-data)
  "Return an alist of PC -> dominant specializer key for >90% type feedback sites."
  (let ((by-pc (make-hash-table :test #'eql)))
    (dolist (row (%pgo-type-feedback-rows profile-data))
      (destructuring-bind (site-key . count) row
        (when (and (consp site-key)
                   (eq (first site-key) :generic-call)
                   (integerp (second site-key))
                   (plusp count))
          (push (cons (third site-key) count)
                (gethash (second site-key) by-pc)))))
    (let ((dominant nil))
      (maphash
       (lambda (pc rows)
         (let* ((total (reduce #'+ rows :key #'cdr :initial-value 0))
                (best (car (sort (copy-list rows) #'> :key #'cdr))))
           (when (and (plusp total)
                      best
                      (> (/ (float (cdr best)) total)
                         cl-cc/vm::+ic-pgo-dominance-threshold+))
             (push (cons pc (car best)) dominant))))
       by-pc)
      dominant)))

(defun %pgo-apply-type-feedback-to-instructions (instructions profile-data)
  "Annotate generic-call instructions with PGO-dominant type keys from PROFILE-DATA."
  (when (and instructions profile-data)
    (let ((dominant (%pgo-dominant-types-by-pc profile-data)))
      (loop for inst in instructions
            for pc from 0
            for key = (cdr (assoc pc dominant :test #'eql))
            when (and key (typep inst 'cl-cc/vm:vm-generic-call))
              do (setf (cl-cc/vm::vm-pgo-specializer inst) key))))
  instructions)

(defun %pgo-apply-type-feedback-to-result (result opts)
  "Apply FR-058 type-feedback PGO annotations to RESULT in-place."
  (let ((profile-data (pipeline-opts-pgo-profile-data opts)))
    (when profile-data
      (%pgo-apply-type-feedback-to-instructions
       (cl-cc/compile:compilation-result-vm-instructions result)
       profile-data)
      (%pgo-apply-type-feedback-to-instructions
       (cl-cc/compile:compilation-result-optimized-instructions result)
       profile-data)
      (%pgo-apply-type-feedback-to-instructions
       (cl-cc/vm:vm-program-instructions
        (cl-cc/compile:compilation-result-program result))
       profile-data)))
  result)

(defun %make-direct-compilation-result (ctx result-reg inferred-type cps ast opts)
  "Build the normal direct-path compilation result from CTX and RESULT-REG." 
  (let* ((instructions (nreverse (ctx-instructions ctx)))
          (full-instrs  (append instructions (list (make-vm-halt :reg result-reg))))
          (pgo-counter-plan (%build-pgo-counter-plan-from-instructions full-instrs)))
    (%pgo-apply-type-feedback-to-instructions full-instrs
                                              (pipeline-opts-pgo-profile-data opts))
    (multiple-value-bind (optimized-instrs leaf-p)
        (%call-with-tiered-optimizer-policy
         opts
         (lambda ()
           (apply #'optimize-instructions full-instrs (%opts->optimize-kwargs opts))))
      (if *repl-capture-label-counter*
          (setf *repl-capture-label-counter* (ctx-next-label ctx))
          nil)
      (let* ((target (pipeline-opts-target opts))
              (runtime-instructions
               (%pipeline-runtime-instructions target full-instrs optimized-instrs))
               (program (make-vm-program :instructions runtime-instructions
                                         :result-register result-reg
                                         :leaf-p leaf-p
                                         :compilation-tier (pipeline-opts-compilation-tier opts)))
              (result-type (if (pipeline-opts-type-check opts) inferred-type nil))
              (diagnostics (nreverse (ctx-diagnostics ctx)))
              (cl-cc/parse:*werror-p* (pipeline-opts-werror opts))
              (cl-cc/parse:*werror-categories*
                (mapcar (lambda (category) (string-downcase (string category)))
                        (pipeline-opts-werror-categories opts))))
         (multiple-value-bind (warnings errors)
             (let ((warnings nil) (errors nil))
               (dolist (diag diagnostics)
                 (let ((effective (cl-cc/parse::%maybe-promote-warning-diagnostic diag)))
                   (if (eq (cl-cc/parse:diagnostic-severity effective) :error)
                       (push effective errors)
                       (push effective warnings))))
               (values (nreverse warnings) (nreverse errors)))
         (make-compilation-result
          :program                program
          :assembly               (emit-assembly program :target target)
         :type                   result-type
         :type-env               (ctx-type-env ctx)
         :cps                    cps
           :ast                    ast
           :vm-instructions        full-instrs
           :optimized-instructions optimized-instrs
           :branch-probability-hints (cl-cc/compile::%ast-branch-probability-hints ast)
           :code-placement-hints (cl-cc/compile::%ast-code-placement-hints ast)
           :warnings               warnings
           :errors                 errors
           :pgo-counter-plan       pgo-counter-plan))))))

(defun %form-needs-toplevel-path-p (form)
  "True when FORM mentions a condition/handler control construct anywhere, so it
must use the top-level compile path (which establishes the VM handler frame)
rather than the single-expression path.  Matched by symbol-name so it is robust
to the reader's package."
  (labels ((control-sym-p (x)
             (and (symbolp x) (not (null x))
                  (member (symbol-name x)
                          '("HANDLER-CASE" "HANDLER-BIND" "IGNORE-ERRORS"
                            "UNWIND-PROTECT" "RESTART-CASE" "WITH-SIMPLE-RESTART")
                          :test #'string=)))
           (walk (x)
             (and (consp x)
                  (or (control-sym-p (car x))
                      (walk (car x))
                      (walk (cdr x))))))
    (and (walk form) t)))

(defun %compile-string-forms (forms opts)
  "Compile already-parsed FORMS through the single-form or top-level path.

A single CONTROL form (handler-case / ignore-errors / unwind-protect) is routed
through the top-level path, not compile-expression: the single-expression path
does not establish the VM handler frame, so a lone (handler-case (error …) …)
program failed to catch — yet the same program preceded by any other form (which
takes the top-level path) caught correctly."
  (if (and (= (length forms) 1)
           (not (%form-needs-toplevel-path-p (first forms))))
      (apply #'compile-expression (first forms) (%opts->compile-kwargs opts))
      (let ((*enable-cps-vm-primary-path* nil))
        (let ((result (%call-with-tiered-optimizer-policy
                       opts
                       (lambda ()
                         (apply #'compile-toplevel-forms
                                (%maybe-cps-toplevel-forms forms opts)
                                (%opts->compile-kwargs opts))))))
          (setf (cl-cc/vm:vm-program-compilation-tier
                 (cl-cc/compile:compilation-result-program result))
                (pipeline-opts-compilation-tier opts))
          (setf (cl-cc/compile:compilation-result-pgo-counter-plan result)
                (%build-pgo-counter-plan-from-instructions
                 (cl-cc/compile:compilation-result-vm-instructions result)))
          result))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Language-level parsing
;;; ─────────────────────────────────────────────────────────────────────────

(defun %pipeline-strip-shebang-line (source)
  "Return SOURCE with a leading POSIX #! interpreter line removed."
  (let* ((text (or source ""))
         (newline (position #\Newline text))
         (first-line (if newline (subseq text 0 newline) text)))
    (if (and (>= (length first-line) 2)
             (char= (char first-line 0) #\#)
             (char= (char first-line 1) #\!))
        (if newline (subseq text (1+ newline)) "")
        text)))

(defun parse-source-for-language (source language &key source-file)
  "Parse SOURCE according to LANGUAGE, returning top-level forms for compilation.
:LISP always returns normal s-expressions; when SOURCE-FILE is provided, a second
value carries top-level source locations for later AST annotation.
:PHP calls parse-php-source and :JAVASCRIPT calls js-program-forms, both of
which return shared AST nodes directly (the latter prepends a runtime prelude)."
  (cond
    ((eq language :lisp)
     (let ((clean-source (%pipeline-strip-shebang-line source)))
       (if source-file
           (%lisp-top-level-source-forms-and-locations clean-source source-file)
           (values (parse-all-forms clean-source) nil))))
    ((eq language :php)  (parse-php-source source))
    ((eq language :javascript)
     (values (cl-cc/javascript:js-program-forms source) nil))
    (t (error "Unknown language: ~S" language))))

(defparameter *php-runtime-bridge-entries*
  '((cl-cc/php::%php-array           . cl-cc/php:%php-array)
    (cl-cc/php::%php-array-ref       . cl-cc/php:%php-array-ref)
    (cl-cc/php::%php-array-set       . cl-cc/php:%php-array-set)
    (cl-cc/php::%php-array-unset     . cl-cc/php:%php-array-unset)
    (cl-cc/php::%php-count           . cl-cc/php:%php-count)
    (cl-cc/php::%php-strlen          . cl-cc/php:%php-strlen)
    (cl-cc/php::%php-strtolower      . cl-cc/php:%php-strtolower)
    (cl-cc/php::%php-strtoupper      . cl-cc/php:%php-strtoupper)
    (cl-cc/php::%php-stringify       . cl-cc/php:%php-stringify)
    (cl-cc/php::%php-concat          . cl-cc/php:%php-concat)
    (cl-cc/php::%php-modulo          . cl-cc/php:%php-modulo)
    (cl-cc/php::%php-shift-left      . cl-cc/php:%php-shift-left)
    (cl-cc/php::%php-shift-right     . cl-cc/php:%php-shift-right)
    (cl-cc/php::%php-spaceship       . cl-cc/php:%php-spaceship)
    (cl-cc/php::%php-bitwise-and     . cl-cc/php:%php-bitwise-and)
    (cl-cc/php::%php-bitwise-or      . cl-cc/php:%php-bitwise-or)
    (cl-cc/php::%php-bitwise-xor     . cl-cc/php:%php-bitwise-xor)
    (cl-cc/php::%php-bitwise-not     . cl-cc/php:%php-bitwise-not)
    (cl-cc/php::%php-isset           . cl-cc/php:%php-isset)
    (cl-cc/php::%php-array-key-exists . cl-cc/php:%php-array-key-exists)
    (cl-cc/php::%php-yield           . cl-cc/php:%php-yield)
    (cl-cc/php::%php-yield-from      . cl-cc/php:%php-yield-from)
    (cl-cc/php::%php-throw           . cl-cc/php:%php-throw)
    (cl-cc/php::%php-make-exception  . cl-cc/php:%php-make-exception)
    (cl-cc/php::%php-exception-object-p . cl-cc/php:%php-exception-object-p)
    (cl-cc/php::%php-exception-value . cl-cc/php:%php-exception-value)
    (cl-cc/php::%php-exception-matches-p . cl-cc/php:%php-exception-matches-p)
    (cl-cc/php::%php-enum-make-case  . cl-cc/php:%php-enum-make-case)
    (cl-cc/php::%php-enum-cases      . cl-cc/php:%php-enum-cases)
    (cl-cc/php::%php-enum-from       . cl-cc/php:%php-enum-from)
    (cl-cc/php::%php-enum-try-from   . cl-cc/php:%php-enum-try-from)
    (cl-cc/php::%php-enum-case-value . cl-cc/php:%php-enum-case-value))
  "Alist of bridge-symbol -> implementation-symbol pairs for PHP runtime helpers.")

(defun %register-php-runtime-bridges ()
  "Register PHP runtime helpers as VM host bridge functions.

The PHP frontend lowers builtins to calls on cl-cc/php::%php-* helpers, but the
VM host bridge is a whitelist — only registered symbols are callable from
compiled code. The hand-maintained *php-runtime-bridge-entries* alist had
drifted ~60 helpers behind the lowering (array_*, str*, is_*, truthy, math …),
so every PHP program using them hit `Undefined function'. Derive the whitelist
from the package itself: register every fbound, non-macro %PHP-* function whose
home package is :cl-cc/php. The explicit alias entries still run first for any
non-%php- aliases."
  (dolist (entry *php-runtime-bridge-entries*)
    (cl-cc/vm:vm-register-host-bridge (car entry) (fdefinition (cdr entry))))
  (let ((pkg (find-package :cl-cc/php)))
    (when pkg
      (do-symbols (sym pkg)
        (when (and (eq (symbol-package sym) pkg)
                   (fboundp sym)
                   (not (macro-function sym))
                   (not (special-operator-p sym))
                   (let ((name (symbol-name sym)))
                     (and (>= (length name) 5)
                          (string= "%PHP-" name :end2 5))))
          (cl-cc/vm:vm-register-host-bridge sym (fdefinition sym)))))))

(defun %register-js-runtime-bridges ()
  "Register JavaScript runtime helpers as VM host bridge functions.

The JS frontend lowers operators, member access, and builtins to calls on
cl-cc/javascript::%js-* helpers (%js-get-prop, %js-make-array, %js-console-log,
%js-make-console, …). Register every fbound, non-macro %JS-* function whose home
package is :cl-cc/javascript so compiled JS can call them through the VM host
bridge — the same package-derived whitelist used for PHP."
  (let ((pkg (find-package :cl-cc/javascript)))
    (when pkg
      (do-symbols (sym pkg)
        (when (and (eq (symbol-package sym) pkg)
                   (fboundp sym)
                   (not (macro-function sym))
                   (not (special-operator-p sym))
                   (let ((name (symbol-name sym)))
                     (and (>= (length name) 4)
                          (string= "%JS-" name :end2 4))))
          (cl-cc/vm:vm-register-host-bridge sym (fdefinition sym))))
      ;; Route JS callbacks (Array.map/filter/reduce/sort) back through the VM
      ;; when the callback is a compiled-JS closure; host functions still go via
      ;; APPLY. Host array methods call (%js-funcall fn ...) → *js-apply-fn*; this
      ;; is the controlled inverse bridge (host runtime → VM closure) using the
      ;; *vm-state* dynamically bound around VM execution.
      (setf cl-cc/javascript::*js-apply-fn*
            (lambda (fn args)
              (if (cl-cc/vm::%vm-closure-object-p fn)
                  (cl-cc/vm::%vm-call-closure-sync fn cl-cc/vm:*vm-state* args)
                  (apply fn args))))
      ;; Teach prototype method lookup to recognize a compiled-JS method (a
      ;; vm-closure) as callable, so obj.method resolves to a bound method.
      (setf cl-cc/javascript::*js-callable-p*
            (lambda (x) (or (functionp x) (cl-cc/vm::%vm-closure-object-p x))))
      ;; Bind `this' for a method/constructor call in BOTH the host special and
      ;; the VM-global %js-this: a compiled-JS method body reads `this' via
      ;; vm-get-global, so the host dynamic binding alone is invisible to it.
      ;; Save/restore around the call so nested method calls (a.m() calling b.n())
      ;; restore the outer receiver.
      (setf cl-cc/javascript::*js-apply-with-this-fn*
            (lambda (this fn args)
              (let ((state cl-cc/vm:*vm-state*))
                (if state
                    (let* ((gv   (cl-cc/vm:vm-global-vars state))
                           (had  (nth-value 1 (gethash 'cl-cc/javascript::%js-this gv)))
                           (prev (gethash 'cl-cc/javascript::%js-this gv)))
                      (setf (gethash 'cl-cc/javascript::%js-this gv) this)
                      (unwind-protect
                           (let ((cl-cc/javascript::%js-this this))
                             (funcall cl-cc/javascript::*js-apply-fn* fn args))
                        (if had
                            (setf (gethash 'cl-cc/javascript::%js-this gv) prev)
                            (remhash 'cl-cc/javascript::%js-this gv))))
                    (let ((cl-cc/javascript::%js-this this))
                      (funcall cl-cc/javascript::*js-apply-fn* fn args)))))))))

(defun seed-js-runtime-globals (state)
  "Seed JavaScript runtime special variables into STATE's VM globals.

The JS prelude (js-program-forms) binds standard globals to the VALUES of host
specials — e.g. Symbol to *js-symbol-global*, Infinity to *js-inf-float*, the error
classes to *js-error-class* etc. Each such reference compiles to a vm-get-global,
so without seeding EVERY JS program fails at runtime with
'Unbound global variable: *JS-...*'. Mirror the package-derived function-bridge
whitelist: copy every bound *JS-…* special in :cl-cc/javascript into STATE's
globals so the prelude resolves."
  (let ((pkg (find-package :cl-cc/javascript)))
    (when (and pkg state)
      (do-symbols (sym pkg)
        (when (and (eq (symbol-package sym) pkg)
                   (boundp sym)
                   (let ((name (symbol-name sym)))
                     (and (>= (length name) 4)
                          ;; *JS-* specials (Symbol/Infinity/error classes/…) and
                          ;; %JS-* special vars like %js-this (so `this' at top
                          ;; level resolves to undefined rather than erroring; a
                          ;; method call overrides it with the receiver).
                          (or (string= "*JS-" name :end2 4)
                              (string= "%JS-" name :end2 4)))))
          (setf (gethash sym (cl-cc/vm:vm-global-vars state))
                (symbol-value sym)))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Public compilation API
;;; ─────────────────────────────────────────────────────────────────────────

(defun %maybe-attach-mcdc-coverage (result forms opts)
  "Attach MC/DC coverage metadata to RESULT when MC/DC coverage is enabled.
   Returns RESULT unchanged when coverage is disabled."
  (when (cl-cc/compile:mcdc-coverage-enabled-p (pipeline-opts-coverage opts))
    (setf (cl-cc/compile:compilation-result-coverage result)
          (cl-cc/compile:collect-mcdc-coverage forms)))
  result)

;;; Stdlib sexp cache (*stdlib-expanded-cache*, %snapshot-macro-env-table,
;;; %restore-macro-env-table, %build-stdlib-expanded-cache, get-stdlib-forms)
;;; are in pipeline-stdlib.lisp (loaded before this file).
;;; warm-stdlib-cache and %build-stdlib-vm-snapshot are defined below,
;;; after compile-toplevel-forms, which they depend on.

(defun compile-expression (expr &key (target :x86_64) type-check (safety 1)
                                       speed (inline-threshold-scale 1)
                                       block-compile
                                       pass-pipeline print-pass-timings timing-stream coverage
                                        print-opt-remarks opt-remarks-stream
                                        (opt-remarks-mode :all)
                                        opt-bisect-limit
                                        print-pass-stats stats-stream trace-json-stream
                                           retpoline spectre-mitigations stack-protector shadow-stack
                                            asan msan tsan ubsan hwasan pgo-profile-data
                                             verify-transforms werror werror-categories
                                             (compilation-tier *compilation-tier*)
                                             &allow-other-keys)
  "Compile EXPR and return a compilation-result object.

EXPR may be an s-expression or an already-lowered AST node. TARGET chooses the
backend (:VM, :WASM, or a native target keyword). TYPE-CHECK enables type
inference, with :STRICT re-signaling type errors. SAFETY, SPEED,
INLINE-THRESHOLD-SCALE, pass tracing, optimization remarks, and sanitizer
keywords are forwarded to later pipeline stages. The result contains the VM
program, emitted assembly text, AST, optional inferred type, instruction
streams, and PGO counter plan."
  (let* ((opts (%make-pipeline-opts
                  :target target :type-check type-check :safety safety
                   :speed speed
                  :inline-threshold-scale inline-threshold-scale
                  :block-compile block-compile
                  :pass-pipeline pass-pipeline :print-pass-timings print-pass-timings
                    :coverage coverage
                   :verify-transforms verify-transforms
                   :timing-stream timing-stream :print-pass-stats print-pass-stats
                  :stats-stream stats-stream :trace-json-stream trace-json-stream
                  :print-opt-remarks print-opt-remarks :opt-remarks-stream opt-remarks-stream
                   :opt-remarks-mode opt-remarks-mode
                   :opt-bisect-limit opt-bisect-limit
                    :retpoline retpoline
                    :spectre-mitigations spectre-mitigations
                    :stack-protector stack-protector
                    :shadow-stack shadow-stack
                    :asan asan
                    :msan msan
                    :tsan tsan
                    :ubsan ubsan
                    :hwasan hwasan
                    :pgo-profile-data pgo-profile-data
                    :werror werror
                    :werror-categories werror-categories
                    :compilation-tier (normalize-compilation-tier compilation-tier)))
          (ctx           (make-instance 'compiler-context :safety safety))
          (ast           (%pipeline-optimize-ast (%prepare-ast expr) opts))
         (inferred-type (%type-check-safe ctx ast type-check)))
    (%pipeline-maybe-bump-opts-speed-from-ast opts ast)
    (multiple-value-bind (cps cps-result)
        (%maybe-compile-expression-via-cps ast opts)
      (let ((result
              (%pgo-apply-type-feedback-to-result
               (if cps-result
                   (make-compilation-result
                    :program                (compilation-result-program cps-result)
                    :assembly               (compilation-result-assembly cps-result)
                    :type                   (or (and type-check inferred-type)
                                                (compilation-result-type cps-result))
                    :type-env               (compilation-result-type-env cps-result)
                    :cps                    cps
                    :ast                    ast
                    :coverage               (compilation-result-coverage cps-result)
                    :vm-instructions        (compilation-result-vm-instructions cps-result)
                    :optimized-instructions (compilation-result-optimized-instructions cps-result)
                    :warnings               (append (nreverse (ctx-diagnostics ctx))
                                                    (compilation-result-warnings cps-result))
                    :pgo-counter-plan       (%build-pgo-counter-plan-from-instructions
                                             (compilation-result-vm-instructions cps-result)))
                   (let ((result-reg (compile-ast ast ctx)))
                     (%make-direct-compilation-result ctx result-reg inferred-type cps ast opts)))
               opts)))
        (%maybe-attach-mcdc-coverage result (list expr) opts)))))

(defun %pipeline-ast-declarations-for-optimize-policy (ast)
  "Return declaration list associated with AST when available."
  (cond
    ((typep ast 'cl-cc/ast:ast-defun) (cl-cc/ast:ast-defun-declarations ast))
    ((typep ast 'cl-cc/ast:ast-lambda) (cl-cc/ast:ast-lambda-declarations ast))
    ((typep ast 'cl-cc/ast:ast-let) (cl-cc/ast:ast-let-declarations ast))
    (t nil)))

(defun %pipeline-maybe-bump-opts-speed-from-ast (opts ast)
  "Merge local `(declare (optimize (speed ...)))` into OPTS conservatively.

Uses max(current-speed, local-speed) when local speed is an integer."
  (let* ((decls (%pipeline-ast-declarations-for-optimize-policy ast))
         (local-speed (and decls (cl-cc/expand:declaration-optimize-quality decls 'speed)))
         (current-speed (pipeline-opts-speed opts)))
    (when (integerp local-speed)
      (setf (pipeline-opts-speed opts)
            (if (integerp current-speed)
                (max current-speed local-speed)
                local-speed)))
    opts))

(defun compile-string (source &key (target :x86_64) type-check (language :lisp) (safety 1)
                                       source-file speed (inline-threshold-scale 1)
                                       block-compile debug-info sanitize lto eh-model incremental perf-map parallel
                                       pass-pipeline print-pass-timings timing-stream coverage
                                      print-opt-remarks opt-remarks-stream (opt-remarks-mode :all)
                                      opt-bisect-limit
                                      print-pass-stats stats-stream trace-json-stream
                                       retpoline spectre-mitigations stack-protector shadow-stack
                                       asan msan tsan ubsan hwasan pgo-profile-data
                                      verify-transforms werror werror-categories
                                        (compilation-tier *compilation-tier*)
                                        &allow-other-keys)
  "Compile SOURCE text and return a compilation-result object.

LANGUAGE selects the parser (:LISP or :PHP). SOURCE-FILE, when supplied for
Lisp input, enables source-location annotations on top-level AST nodes and host
macro evaluation for real file compilation. TARGET and the remaining keyword
arguments are forwarded to the expression, top-level, and optimization stages."
  (multiple-value-bind (forms source-locations)
      (parse-source-for-language source language :source-file source-file)
    (when (eq language :php)
      (%register-php-runtime-bridges)
      (php-check-supported-forms forms))
    (when (eq language :javascript)
      (%register-js-runtime-bridges))
    (let* ((opts  (%make-pipeline-opts
                    :target target :type-check type-check :safety safety
                     :speed speed
                     :inline-threshold-scale inline-threshold-scale
                      :block-compile block-compile
                      :debug-info debug-info
                      :sanitize sanitize
                      :lto lto
                      :eh-model eh-model
                      :incremental incremental
                      :perf-map perf-map
                      :parallel parallel
                    :pass-pipeline pass-pipeline :print-pass-timings print-pass-timings
                    :coverage coverage
                    :verify-transforms verify-transforms
                    :timing-stream timing-stream :print-pass-stats print-pass-stats
                    :stats-stream stats-stream :trace-json-stream trace-json-stream
                    :print-opt-remarks print-opt-remarks :opt-remarks-stream opt-remarks-stream
                    :opt-remarks-mode opt-remarks-mode
                   :opt-bisect-limit opt-bisect-limit
                    :retpoline retpoline
                    :spectre-mitigations spectre-mitigations
                    :stack-protector stack-protector
                    :shadow-stack shadow-stack
                    :asan asan
                    :msan msan
                    :tsan tsan
                    :ubsan ubsan
                    :hwasan hwasan
                    :pgo-profile-data pgo-profile-data
                    :werror werror
                    :werror-categories werror-categories
                    :compilation-tier (normalize-compilation-tier compilation-tier)))
           (result (%call-with-source-file-macro-eval
                    (and (eq language :lisp) source-file)
                    (lambda ()
                      (if (eq language :lisp)
                          (%compile-string-forms forms opts)
                           (%call-with-tiered-optimizer-policy
                            opts
                            (lambda ()
                              (apply #'compile-toplevel-forms forms (%opts->compile-kwargs opts)))))))))
      (%pgo-apply-type-feedback-to-result result opts)
      (setf (cl-cc/vm:vm-program-compilation-tier
             (cl-cc/compile:compilation-result-program result))
            (pipeline-opts-compilation-tier opts))
      (if (and (eq language :lisp) source-locations)
          (%attach-source-locations-to-result result
                                             (%non-in-package-form-locations forms source-locations))
          result))))

(defun compile-string-with-stdlib (source &key (target :x86_64) type-check (language :lisp) (safety 1)
                                                  source-file speed (inline-threshold-scale 1)
                                                  block-compile debug-info sanitize lto eh-model incremental perf-map parallel
                                                    pass-pipeline print-pass-timings timing-stream coverage
                                                  print-opt-remarks opt-remarks-stream
                                     (opt-remarks-mode :all)
                                     opt-bisect-limit
                                     print-pass-stats stats-stream trace-json-stream
                                       retpoline spectre-mitigations stack-protector shadow-stack
                                       asan msan tsan ubsan hwasan pgo-profile-data
                                       verify-transforms werror werror-categories
                                        (compilation-tier *compilation-tier*)
                                        &allow-other-keys)
 "Compile SOURCE text and return a compilation-result object.
 
LANGUAGE selects the parser (:LISP or :PHP). SOURCE-FILE, when supplied for
Lisp input, enables source-location annotations on top-level AST nodes and host
macro evaluation for real file compilation. TARGET and the remaining keyword
arguments are forwarded to the expression, top-level, and optimization stages."
   (multiple-value-bind (forms source-locations)
       (parse-source-for-language source language :source-file source-file)
     (when (eq language :php)
       (%register-php-runtime-bridges)
       (php-check-supported-forms forms))
     (when (eq language :javascript)
       (%register-js-runtime-bridges))
     (let* ((opts  (%make-pipeline-opts
                     :target target :type-check type-check :safety safety
                      :speed speed
                      :inline-threshold-scale inline-threshold-scale
                       :block-compile block-compile
                       :debug-info debug-info
                       :sanitize sanitize
                       :lto lto
                       :eh-model eh-model
                       :incremental incremental
                       :perf-map perf-map
                       :parallel parallel
                     :pass-pipeline pass-pipeline :print-pass-timings print-pass-timings
                     :coverage coverage
                     :verify-transforms verify-transforms
                     :timing-stream timing-stream :print-pass-stats print-pass-stats
                     :stats-stream stats-stream :trace-json-stream trace-json-stream
                     :print-opt-remarks print-opt-remarks :opt-remarks-stream opt-remarks-stream
                     :opt-remarks-mode opt-remarks-mode
                     :opt-bisect-limit opt-bisect-limit
                     :retpoline retpoline
                    :spectre-mitigations spectre-mitigations
                   :stack-protector stack-protector
                   :shadow-stack shadow-stack
                   :asan asan
                   :msan msan
                   :tsan tsan
                   :ubsan ubsan
                    :hwasan hwasan
                      :pgo-profile-data pgo-profile-data
                      :werror werror
                      :werror-categories werror-categories
                      :compilation-tier (normalize-compilation-tier compilation-tier)))
           (*enable-cps-vm-primary-path* nil)
           (stdlib-forms (get-stdlib-forms))
           (all-forms (append stdlib-forms forms))
            (result (%call-with-source-file-macro-eval
                     source-file
                     (lambda ()
                        (%call-with-tiered-optimizer-policy
                         opts
                         (lambda ()
                           (apply #'compile-toplevel-forms
                                  all-forms
                                  (%opts->compile-kwargs opts))))))))
      (setf (cl-cc/compile:compilation-result-pgo-counter-plan result)
            (%build-pgo-counter-plan-from-instructions
             (cl-cc/compile:compilation-result-vm-instructions result)))
      (setf (cl-cc/vm:vm-program-compilation-tier
             (cl-cc/compile:compilation-result-program result))
            (pipeline-opts-compilation-tier opts))
      (%pgo-apply-type-feedback-to-result result opts)
      (if source-locations
          (%attach-source-locations-to-result
           result
           (append (make-list (length (%non-in-package-forms stdlib-forms)))
                   (%non-in-package-form-locations forms source-locations)))
          result))))
