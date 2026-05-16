;;;; compile/pipeline.lisp - Top-Level Compilation API
(in-package :cl-cc/pipeline)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Internal helpers
;;; ─────────────────────────────────────────────────────────────────────────

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

(defun %type-check-safe (ast type-check)
  "Run type inference on AST; on error warn unless TYPE-CHECK is :strict."
  (when type-check
    (handler-case (type-check-ast ast)
      (error (e)
        (if (eq type-check :strict)
            (error e)
            (progn (warn "Type check warning: ~A" e) nil))))))


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
  (cond
    ((eq target :vm)   (%cps-vm-compile-safe-ast-p ast))
    ((eq target :wasm) nil)
    (t                 (%cps-native-compile-safe-ast-p ast))))

(defun %maybe-cps-toplevel-form (form opts)
  (if (and (consp form) (eq (car form) 'in-package))
      form
      (let* ((ast      (optimize-ast (%prepare-ast form)))
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
  (and (not *compile-expression-cps-recursion-guard*)
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

(defun %make-direct-compilation-result (ctx result-reg inferred-type cps ast opts)
  "Build the normal direct-path compilation result from CTX and RESULT-REG." 
  (let* ((instructions (nreverse (ctx-instructions ctx)))
         (full-instrs  (append instructions (list (make-vm-halt :reg result-reg))))
         (pgo-counter-plan (%build-pgo-counter-plan-from-instructions full-instrs)))
    (multiple-value-bind (optimized-instrs leaf-p)
        (apply #'optimize-instructions full-instrs (%opts->optimize-kwargs opts))
      (if *repl-capture-label-counter*
          (setf *repl-capture-label-counter* (ctx-next-label ctx))
          nil)
      (let* ((target (pipeline-opts-target opts))
             (runtime-instructions
              (%pipeline-runtime-instructions target full-instrs optimized-instrs))
             (program (make-vm-program :instructions runtime-instructions
                                       :result-register result-reg
                                       :leaf-p leaf-p))
             (result-type (if (pipeline-opts-type-check opts) inferred-type nil)))
        (make-compilation-result
         :program                program
         :assembly               (emit-assembly program :target target)
         :type                   result-type
         :type-env               (ctx-type-env ctx)
         :cps                    cps
         :ast                    ast
         :vm-instructions        full-instrs
         :optimized-instructions optimized-instrs
         :pgo-counter-plan       pgo-counter-plan)))))

(defun %compile-string-forms (forms opts)
  "Compile already-parsed FORMS through the single-form or top-level path."
  (if (= (length forms) 1)
      (apply #'compile-expression (first forms) (%opts->compile-kwargs opts))
      (let ((*enable-cps-vm-primary-path* nil))
        (let ((result (apply #'compile-toplevel-forms
                             (%maybe-cps-toplevel-forms forms opts)
                             (%opts->compile-kwargs opts))))
          (setf (cl-cc/compile:compilation-result-pgo-counter-plan result)
                (%build-pgo-counter-plan-from-instructions
                 (cl-cc/compile:compilation-result-vm-instructions result)))
          result))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Language-level parsing
;;; ─────────────────────────────────────────────────────────────────────────

(defun parse-source-for-language (source language &key source-file)
  "Parse SOURCE according to LANGUAGE, returning top-level forms for compilation.
:LISP always returns normal s-expressions; when SOURCE-FILE is provided, a second
value carries top-level source locations for later AST annotation.
:PHP calls parse-php-source which returns AST nodes directly."
  (cond
    ((eq language :lisp)
     (if source-file
         (%lisp-top-level-source-forms-and-locations source source-file)
         (values (parse-all-forms source) nil)))
    ((eq language :php)  (parse-php-source source))
    (t (error "Unknown language: ~S" language))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Public compilation API
;;; ─────────────────────────────────────────────────────────────────────────

;;; Stdlib sexp cache (*stdlib-expanded-cache*, %snapshot-macro-env-table,
;;; %restore-macro-env-table, %build-stdlib-expanded-cache, get-stdlib-forms)
;;; are in pipeline-stdlib.lisp (loaded before this file).
;;; warm-stdlib-cache and %build-stdlib-vm-snapshot are defined below,
;;; after compile-toplevel-forms, which they depend on.

(defun compile-expression (expr &key (target :x86_64) type-check (safety 1)
                                      speed (inline-threshold-scale 1)
                                       pass-pipeline print-pass-timings timing-stream
                                      print-opt-remarks opt-remarks-stream
                                      (opt-remarks-mode :all)
                                      print-pass-stats stats-stream trace-json-stream
                                       retpoline stack-protector shadow-stack
                                       asan msan tsan ubsan hwasan)
  (let* ((opts (%make-pipeline-opts
                 :target target :type-check type-check :safety safety
                  :speed speed
                 :inline-threshold-scale inline-threshold-scale
                  :pass-pipeline pass-pipeline :print-pass-timings print-pass-timings
                  :timing-stream timing-stream :print-pass-stats print-pass-stats
                  :stats-stream stats-stream :trace-json-stream trace-json-stream
                  :print-opt-remarks print-opt-remarks :opt-remarks-stream opt-remarks-stream
                  :opt-remarks-mode opt-remarks-mode
                   :retpoline retpoline
                   :stack-protector stack-protector
                   :shadow-stack shadow-stack
                   :asan asan
                   :msan msan
                   :tsan tsan
                   :ubsan ubsan
                   :hwasan hwasan))
         (ctx           (make-instance 'compiler-context :safety safety))
         (ast           (optimize-ast (%prepare-ast expr)))
         (inferred-type (%type-check-safe ast type-check)))
    (%pipeline-maybe-bump-opts-speed-from-ast opts ast)
    (multiple-value-bind (cps cps-result)
        (%maybe-compile-expression-via-cps ast opts)
      (if cps-result
          (make-compilation-result
           :program                (compilation-result-program cps-result)
           :assembly               (compilation-result-assembly cps-result)
           :type                   (or (and type-check inferred-type)
                                        (compilation-result-type cps-result))
           :type-env               (compilation-result-type-env cps-result)
           :cps                    cps
           :ast                    ast
           :vm-instructions        (compilation-result-vm-instructions cps-result)
           :optimized-instructions (compilation-result-optimized-instructions cps-result)
           :pgo-counter-plan       (%build-pgo-counter-plan-from-instructions
                                    (compilation-result-vm-instructions cps-result)))
          (let ((result-reg (compile-ast ast ctx)))
            (%make-direct-compilation-result ctx result-reg inferred-type cps ast opts))))))

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
                                    pass-pipeline print-pass-timings timing-stream
                                    print-opt-remarks opt-remarks-stream (opt-remarks-mode :all)
                                    print-pass-stats stats-stream trace-json-stream
                                     retpoline stack-protector shadow-stack
                                     asan msan tsan ubsan hwasan)
  (multiple-value-bind (forms source-locations)
      (parse-source-for-language source language :source-file source-file)
    (let* ((opts  (%make-pipeline-opts
                    :target target :type-check type-check :safety safety
                    :speed speed
                    :inline-threshold-scale inline-threshold-scale
                    :pass-pipeline pass-pipeline :print-pass-timings print-pass-timings
                    :timing-stream timing-stream :print-pass-stats print-pass-stats
                    :stats-stream stats-stream :trace-json-stream trace-json-stream
                    :print-opt-remarks print-opt-remarks :opt-remarks-stream opt-remarks-stream
                    :opt-remarks-mode opt-remarks-mode
                    :retpoline retpoline
                    :stack-protector stack-protector
                    :shadow-stack shadow-stack
                    :asan asan
                    :msan msan
                    :tsan tsan
                    :ubsan ubsan
                    :hwasan hwasan))
           (result (%call-with-source-file-macro-eval
                    (and (eq language :lisp) source-file)
                    (lambda ()
                      (if (eq language :lisp)
                          (%compile-string-forms forms opts)
                          (apply #'compile-toplevel-forms forms (%opts->compile-kwargs opts)))))))
      (if (and (eq language :lisp) source-locations)
          (%attach-source-locations-to-result result
                                             (%non-in-package-form-locations forms source-locations))
          result))))

(defun compile-string-with-stdlib (source &key (target :x86_64) type-check (safety 1)
                                                source-file speed (inline-threshold-scale 1)
                                                 pass-pipeline print-pass-timings timing-stream
                                                 print-opt-remarks opt-remarks-stream
                                                 (opt-remarks-mode :all)
                                                print-pass-stats stats-stream trace-json-stream
                                                 retpoline stack-protector shadow-stack
                                                 asan msan tsan ubsan hwasan)
  "Compile SOURCE with standard library prepended."
  (multiple-value-bind (source-forms source-locations)
      (if source-file
          (%lisp-top-level-source-forms-and-locations source source-file)
          (values (parse-all-forms source) nil))
    (let* ((opts (%make-pipeline-opts
                   :target target :type-check type-check :safety safety
                   :speed speed
                   :inline-threshold-scale inline-threshold-scale
                   :pass-pipeline pass-pipeline :print-pass-timings print-pass-timings
                   :timing-stream timing-stream :print-pass-stats print-pass-stats
                   :stats-stream stats-stream :trace-json-stream trace-json-stream
                   :print-opt-remarks print-opt-remarks :opt-remarks-stream opt-remarks-stream
                   :opt-remarks-mode opt-remarks-mode
                   :retpoline retpoline
                   :stack-protector stack-protector
                   :shadow-stack shadow-stack
                   :asan asan
                   :msan msan
                   :tsan tsan
                   :ubsan ubsan
                   :hwasan hwasan))
           (*enable-cps-vm-primary-path* nil)
           (stdlib-forms (get-stdlib-forms))
           (all-forms (append stdlib-forms source-forms))
            (result (%call-with-source-file-macro-eval
                     source-file
                     (lambda ()
                       (apply #'compile-toplevel-forms
                              all-forms
                              (%opts->compile-kwargs opts))))))
      (setf (cl-cc/compile:compilation-result-pgo-counter-plan result)
            (%build-pgo-counter-plan-from-instructions
             (cl-cc/compile:compilation-result-vm-instructions result)))
      (if source-locations
          (%attach-source-locations-to-result
           result
           (append (mapcar (lambda (_form) nil)
                           (%non-in-package-forms stdlib-forms))
                   (%non-in-package-form-locations source-forms source-locations)))
          result))))
