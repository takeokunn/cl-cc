;;;; compile/pipeline.lisp - Top-Level Compilation API
(in-package :cl-cc)

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

(defun %maybe-cps-toplevel-forms (forms &key (target :vm))
  "Rewrite safe top-level expression FORMS into CPS entry forms when possible.
Definition and control-effect forms stay on the direct path; this only widens
the existing CPS-primary-path behavior to multi-form top-level compilation."
  (mapcar (lambda (form)
            (if (and (consp form) (eq (car form) 'in-package))
                form
                (let* ((ast (%prepare-ast form))
                       (optimized (optimize-ast ast)))
                  (if (case target
                        (:vm (%cps-vm-compile-safe-ast-p optimized))
                        (:wasm nil)
                        (t (%cps-native-compile-safe-ast-p optimized)))
                      (%cps-identity-entry-form (cps-transform-ast* optimized))
                      form))))
          forms))

(defun %maybe-compile-expression-via-cps (ast target type-check safety pass-pipeline print-pass-timings timing-stream print-pass-stats stats-stream trace-json-stream print-opt-remarks opt-remarks-stream opt-remarks-mode)
  "Return a CPS-backed compilation-result for AST when the target-specific CPS path is safe." 
  (let ((cps-safe-p (and (not *compile-expression-cps-recursion-guard*)
                         (case target
                           (:vm (and *enable-cps-vm-primary-path*
                                     (%cps-vm-compile-safe-ast-p ast)))
                           (:wasm nil)
                           (t (%cps-native-compile-safe-ast-p ast))))))
    (if cps-safe-p
        (let ((cps (cps-transform-ast* ast)))
          (values cps
                  (let ((*compile-expression-cps-recursion-guard* t))
                    (compile-expression (%cps-identity-entry-form cps)
                                        :target target
                                        :type-check type-check
                                        :safety safety
                                        :pass-pipeline pass-pipeline
                                        :print-pass-timings print-pass-timings
                                        :timing-stream timing-stream
                                        :print-pass-stats print-pass-stats
                                        :stats-stream stats-stream
                                        :trace-json-stream trace-json-stream
                                        :print-opt-remarks print-opt-remarks
                                        :opt-remarks-stream opt-remarks-stream
                                        :opt-remarks-mode opt-remarks-mode)))
        (values nil nil)))))

(defun %make-direct-compilation-result (ctx result-reg target type-check inferred-type cps ast pass-pipeline print-pass-timings timing-stream print-pass-stats stats-stream trace-json-stream print-opt-remarks opt-remarks-stream opt-remarks-mode)
  "Build the normal direct-path compilation result from CTX and RESULT-REG."
  (let* ((instructions (nreverse (ctx-instructions ctx)))
         (full-instrs (append instructions (list (make-vm-halt :reg result-reg)))))
    (multiple-value-bind (optimized-instrs leaf-p)
        (optimize-instructions full-instrs
                               :pass-pipeline pass-pipeline
                               :print-pass-timings print-pass-timings
                               :timing-stream timing-stream
                               :print-pass-stats print-pass-stats
                               :stats-stream stats-stream
                               :trace-json-stream trace-json-stream
                               :print-opt-remarks print-opt-remarks
                               :opt-remarks-stream opt-remarks-stream
                               :opt-remarks-mode opt-remarks-mode)
      (when *repl-capture-label-counter*
        (setf *repl-capture-label-counter* (ctx-next-label ctx)))
      (let* ((runtime-instructions (if (member target '(:vm :wasm))
                                       full-instrs
                                       (or optimized-instrs full-instrs)))
             (program (make-vm-program :instructions runtime-instructions
                                       :result-register result-reg
                                       :leaf-p leaf-p)))
        (make-compilation-result :program program
                                 :assembly (emit-assembly program :target target)
                                 :type (when type-check inferred-type)
                                 :type-env (ctx-type-env ctx)
                                 :cps cps
                                 :ast ast
                                 :vm-instructions full-instrs
                                 :optimized-instructions optimized-instrs)))))

(defun %compile-string-forms (forms target type-check safety pass-pipeline print-pass-timings timing-stream print-pass-stats stats-stream trace-json-stream print-opt-remarks opt-remarks-stream opt-remarks-mode)
  "Compile already-parsed FORMS through the single-form or top-level path." 
  (if (= (length forms) 1)
      (compile-expression (first forms)
                          :target target :type-check type-check :safety safety
                          :pass-pipeline pass-pipeline
                          :print-pass-timings print-pass-timings
                          :timing-stream timing-stream
                          :print-pass-stats print-pass-stats
                          :stats-stream stats-stream
                          :trace-json-stream trace-json-stream
                          :print-opt-remarks print-opt-remarks
                          :opt-remarks-stream opt-remarks-stream
                          :opt-remarks-mode opt-remarks-mode)
      (let ((*enable-cps-vm-primary-path* nil))
        (compile-toplevel-forms (%maybe-cps-toplevel-forms forms :target target)
                                :target target :type-check type-check :safety safety
                                :pass-pipeline pass-pipeline
                                :print-pass-timings print-pass-timings
                                :timing-stream timing-stream
                                :print-pass-stats print-pass-stats
                                :stats-stream stats-stream
                                :trace-json-stream trace-json-stream
                                :print-opt-remarks print-opt-remarks
                                :opt-remarks-stream opt-remarks-stream
                                :opt-remarks-mode opt-remarks-mode))))

(defun compile-expression (expr &key (target :x86_64) type-check (safety 1) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  (let* ((ctx            (make-instance 'compiler-context :safety safety))
         (ast            (optimize-ast (%prepare-ast expr)))
         (inferred-type  (%type-check-safe ast type-check)))
    (multiple-value-bind (cps cps-result)
        (%maybe-compile-expression-via-cps ast target type-check safety pass-pipeline print-pass-timings timing-stream print-pass-stats stats-stream trace-json-stream print-opt-remarks opt-remarks-stream opt-remarks-mode)
      (let ((result-reg (unless cps-result (compile-ast ast ctx))))
        (if cps-result
            (make-compilation-result
             :program (compilation-result-program cps-result)
             :assembly (compilation-result-assembly cps-result)
             :type (or (and type-check inferred-type)
                       (compilation-result-type cps-result))
             :type-env (compilation-result-type-env cps-result)
             :cps cps
             :ast ast
             :vm-instructions (compilation-result-vm-instructions cps-result)
             :optimized-instructions (compilation-result-optimized-instructions cps-result))
            (%make-direct-compilation-result ctx result-reg target type-check inferred-type cps ast pass-pipeline print-pass-timings timing-stream print-pass-stats stats-stream trace-json-stream print-opt-remarks opt-remarks-stream opt-remarks-mode))))))

;;; Stdlib sexp cache (*stdlib-expanded-cache*, %snapshot-macro-env-table,
;;; %restore-macro-env-table, %build-stdlib-expanded-cache, get-stdlib-forms)
;;; are in pipeline-stdlib.lisp (loaded before this file).
;;; warm-stdlib-cache and %build-stdlib-vm-snapshot are defined below,
;;; after compile-toplevel-forms, which they depend on.

(defun parse-source-for-language (source language)
  "Parse SOURCE according to LANGUAGE, returning a list of AST nodes or s-expressions.
:LISP returns s-expressions (compile-toplevel-forms handles lowering).
:PHP calls parse-php-source which returns AST nodes directly."
  (case language
    (:lisp (parse-all-forms source))
    (:php (parse-php-source source))
    (t (error "Unknown language: ~S" language))))

(defun compile-string (source &key (target :x86_64) type-check (language :lisp) (safety 1) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  (let ((forms (parse-source-for-language source language)))
    (if (eq language :lisp)
        (%compile-string-forms forms target type-check safety pass-pipeline print-pass-timings timing-stream print-pass-stats stats-stream trace-json-stream print-opt-remarks opt-remarks-stream opt-remarks-mode)
        (compile-toplevel-forms forms
                                :target target :type-check type-check :safety safety
                                :pass-pipeline pass-pipeline
                                :print-pass-timings print-pass-timings
                                :timing-stream timing-stream
                                :print-pass-stats print-pass-stats
                                :stats-stream stats-stream
                                :trace-json-stream trace-json-stream
                                :print-opt-remarks print-opt-remarks
                                :opt-remarks-stream opt-remarks-stream
                                :opt-remarks-mode opt-remarks-mode))))

(defun compile-string-with-stdlib (source &key (target :x86_64) type-check (safety 1) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  "Compile SOURCE with standard library prepended."
  (let ((stdlib-forms (get-stdlib-forms))
        (user-forms (parse-all-forms source)))
    (let ((*enable-cps-vm-primary-path* nil))
      (compile-toplevel-forms (%maybe-cps-toplevel-forms (append stdlib-forms user-forms)
                                                        :target target)
                              :target target
                              :type-check type-check
                              :safety safety
                              :pass-pipeline pass-pipeline
                              :print-pass-timings print-pass-timings
                              :timing-stream timing-stream
                              :print-pass-stats print-pass-stats
                              :stats-stream stats-stream
                              :trace-json-stream trace-json-stream
                              :print-opt-remarks print-opt-remarks
                              :opt-remarks-stream opt-remarks-stream
                              :opt-remarks-mode opt-remarks-mode))))
