;;;; compile/pipeline.lisp - Top-Level Compilation API
(in-package :cl-cc/pipeline)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Internal helpers
;;; ─────────────────────────────────────────────────────────────────────────

(defun %prepare-ast (expr)
  "Macro-expand EXPR (if not already an AST node), then lower to an AST node."
  (let ((expanded (if (typep expr 'ast-node) expr (compiler-macroexpand-all expr))))
    (if (typep expanded 'ast-node) expanded (lower-sexp-to-ast expanded))))

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
       (not (null (member (string-upcase (symbol-name (car form)))
                          *definition-and-declaration-form-heads*
                          :test #'string=)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS rewriting for top-level form batches
;;; ─────────────────────────────────────────────────────────────────────────

(defun %maybe-cps-toplevel-forms (forms opts)
  "Rewrite safe top-level expression FORMS into CPS entry forms when possible.
Definition and control-effect forms stay on the direct path."
  (mapcar (lambda (form)
            (if (and (consp form) (eq (car form) 'in-package))
                form
                (let* ((ast       (optimize-ast (%prepare-ast form)))
                       (cps-safe-p (case (pipeline-opts-target opts)
                                     (:vm   (%cps-vm-compile-safe-ast-p ast))
                                     (:wasm nil)
                                     (t     (%cps-native-compile-safe-ast-p ast)))))
                  (if cps-safe-p
                      (%cps-identity-entry-form (cps-transform-ast* ast))
                      form))))
          forms))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Internal compilation stages — all accept pipeline-opts
;;; ─────────────────────────────────────────────────────────────────────────

(defun %maybe-compile-expression-via-cps (ast opts)
  "Return (values cps cps-result) when the CPS fast path is safe, else (values nil nil)."
  (let ((cps-safe-p (and (not *compile-expression-cps-recursion-guard*)
                         (case (pipeline-opts-target opts)
                           (:vm (and *enable-cps-vm-primary-path*
                                     (%cps-vm-compile-safe-ast-p ast)))
                           (:wasm nil)
                           (t    (%cps-native-compile-safe-ast-p ast))))))
    (if cps-safe-p
        (let ((cps (cps-transform-ast* ast)))
          (values cps
                  (let ((*compile-expression-cps-recursion-guard* t))
                    (apply #'compile-expression (%cps-identity-entry-form cps)
                           (%opts->compile-kwargs opts)))))
        (values nil nil))))

(defun %make-direct-compilation-result (ctx result-reg inferred-type cps ast opts)
  "Build the normal direct-path compilation result from CTX and RESULT-REG."
  (let* ((instructions (nreverse (ctx-instructions ctx)))
         (full-instrs  (append instructions (list (make-vm-halt :reg result-reg)))))
    (multiple-value-bind (optimized-instrs leaf-p)
        (apply #'optimize-instructions full-instrs (%opts->optimize-kwargs opts))
      (when *repl-capture-label-counter*
        (setf *repl-capture-label-counter* (ctx-next-label ctx)))
      (let* ((target (pipeline-opts-target opts))
             (runtime-instructions
               (if (member target '(:vm :wasm))
                   full-instrs
                   (or optimized-instrs full-instrs)))
             (program (make-vm-program :instructions runtime-instructions
                                       :result-register result-reg
                                       :leaf-p leaf-p)))
        (make-compilation-result
         :program                program
         :assembly               (emit-assembly program :target target)
         :type                   (when (pipeline-opts-type-check opts) inferred-type)
         :type-env               (ctx-type-env ctx)
         :cps                    cps
         :ast                    ast
         :vm-instructions        full-instrs
         :optimized-instructions optimized-instrs)))))

(defun %compile-string-forms (forms opts)
  "Compile already-parsed FORMS through the single-form or top-level path."
  (if (= (length forms) 1)
      (apply #'compile-expression (first forms) (%opts->compile-kwargs opts))
      (let ((*enable-cps-vm-primary-path* nil))
        (apply #'compile-toplevel-forms
               (%maybe-cps-toplevel-forms forms opts)
               (%opts->compile-kwargs opts)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Language-level parsing
;;; ─────────────────────────────────────────────────────────────────────────

(defun parse-source-for-language (source language)
  "Parse SOURCE according to LANGUAGE, returning a list of AST nodes or s-expressions.
:LISP returns s-expressions (compile-toplevel-forms handles lowering).
:PHP calls parse-php-source which returns AST nodes directly."
  (case language
    (:lisp (parse-all-forms source))
    (:php  (parse-php-source source))
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
                                     pass-pipeline print-pass-timings timing-stream
                                     print-opt-remarks opt-remarks-stream
                                     (opt-remarks-mode :all)
                                     print-pass-stats stats-stream trace-json-stream)
  (let* ((opts (%make-pipeline-opts
                :target target :type-check type-check :safety safety
                :pass-pipeline pass-pipeline :print-pass-timings print-pass-timings
                :timing-stream timing-stream :print-pass-stats print-pass-stats
                :stats-stream stats-stream :trace-json-stream trace-json-stream
                :print-opt-remarks print-opt-remarks :opt-remarks-stream opt-remarks-stream
                :opt-remarks-mode opt-remarks-mode))
         (ctx           (make-instance 'compiler-context :safety safety))
         (ast           (optimize-ast (%prepare-ast expr)))
         (inferred-type (%type-check-safe ast type-check)))
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
           :optimized-instructions (compilation-result-optimized-instructions cps-result))
          (let ((result-reg (compile-ast ast ctx)))
            (%make-direct-compilation-result ctx result-reg inferred-type cps ast opts))))))

(defun compile-string (source &key (target :x86_64) type-check (language :lisp) (safety 1)
                                   pass-pipeline print-pass-timings timing-stream
                                   print-opt-remarks opt-remarks-stream (opt-remarks-mode :all)
                                   print-pass-stats stats-stream trace-json-stream)
  (let ((forms (parse-source-for-language source language))
        (opts  (%make-pipeline-opts
                :target target :type-check type-check :safety safety
                :pass-pipeline pass-pipeline :print-pass-timings print-pass-timings
                :timing-stream timing-stream :print-pass-stats print-pass-stats
                :stats-stream stats-stream :trace-json-stream trace-json-stream
                :print-opt-remarks print-opt-remarks :opt-remarks-stream opt-remarks-stream
                :opt-remarks-mode opt-remarks-mode)))
    (if (eq language :lisp)
        (%compile-string-forms forms opts)
        (apply #'compile-toplevel-forms forms (%opts->compile-kwargs opts)))))

(defun compile-string-with-stdlib (source &key (target :x86_64) type-check (safety 1)
                                               pass-pipeline print-pass-timings timing-stream
                                               print-opt-remarks opt-remarks-stream
                                               (opt-remarks-mode :all)
                                               print-pass-stats stats-stream trace-json-stream)
  "Compile SOURCE with standard library prepended."
  (let* ((opts (%make-pipeline-opts
                :target target :type-check type-check :safety safety
                :pass-pipeline pass-pipeline :print-pass-timings print-pass-timings
                :timing-stream timing-stream :print-pass-stats print-pass-stats
                :stats-stream stats-stream :trace-json-stream trace-json-stream
                :print-opt-remarks print-opt-remarks :opt-remarks-stream opt-remarks-stream
                :opt-remarks-mode opt-remarks-mode))
         (*enable-cps-vm-primary-path* nil)
         (all-forms (append (get-stdlib-forms) (parse-all-forms source))))
    (apply #'compile-toplevel-forms
           (%maybe-cps-toplevel-forms all-forms opts)
           (%opts->compile-kwargs opts))))
