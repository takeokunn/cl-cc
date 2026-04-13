;;;; compile/pipeline.lisp - Top-Level Compilation API
(in-package :cl-cc)

(defun compile-expression (expr &key (target :x86_64) type-check (safety 1) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  (let* ((ctx (make-instance 'compiler-context :safety safety))
          (expanded-expr (if (typep expr 'ast-node)
                             expr
                             (compiler-macroexpand-all expr)))
          (ast (if (typep expanded-expr 'ast-node)
                   expanded-expr
                   (lower-sexp-to-ast expanded-expr)))
          (optimized-ast (optimize-ast ast))
          (inferred-type (when type-check
                           (handler-case
                               (type-check-ast optimized-ast)
                             (error (e)
                               (if (eq type-check :strict)
                                   (error e)
                                   (progn
                                     (warn "Type check warning: ~A" e)
                                     nil))))))
          (result-reg (compile-ast optimized-ast ctx))
          (instructions (nreverse (ctx-instructions ctx)))
          (full-instructions (append instructions
                                     (list (make-vm-halt
                                            :reg result-reg))))
          (optimized-instructions nil)
          (leaf-p nil)
          (optimized-program nil))
      (multiple-value-setq (optimized-instructions leaf-p)
        (optimize-instructions full-instructions
                               :pass-pipeline pass-pipeline
                               :print-pass-timings print-pass-timings
                               :timing-stream timing-stream
                               :print-pass-stats print-pass-stats
                               :stats-stream stats-stream
                               :trace-json-stream trace-json-stream
                               :print-opt-remarks print-opt-remarks
                               :opt-remarks-stream opt-remarks-stream
                               :opt-remarks-mode opt-remarks-mode))
      (setf optimized-program (make-vm-program
                              :instructions full-instructions
                              :result-register result-reg
                              :leaf-p leaf-p))
     ;; Capture label counter for REPL continuity
     (when *repl-capture-label-counter*
       (setf *repl-capture-label-counter* (ctx-next-label ctx)))
     (make-compilation-result :program optimized-program
                                :assembly (emit-assembly optimized-program :target target)
                                :type (when type-check inferred-type)
                                :type-env (ctx-type-env ctx)
                                :cps (maybe-cps-transform optimized-ast)
                                :ast optimized-ast
                                :vm-instructions full-instructions
                                :optimized-instructions optimized-instructions)))

;;; Stdlib cache (*stdlib-expanded-cache*, %snapshot-macro-env-table,
;;; %restore-macro-env-table, %build-stdlib-expanded-cache,
;;; get-stdlib-forms, warm-stdlib-cache)
;;; are in pipeline-stdlib.lisp (loaded before this file).

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
    (if (and (eq language :lisp) (= (length forms) 1))
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
        ;; Multiple forms (or non-lisp): use compile-toplevel-forms for sequential macro expansion
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

(defun run-string (source &key stdlib pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  "Compile and run SOURCE. When STDLIB is true, include standard library."
  (let* ((*package* (find-package :cl-cc))
         (*accessor-slot-map* (make-hash-table :test #'eq))
         (*defstruct-slot-registry* (make-hash-table :test #'eq))
         (*labels-boxed-fns* nil)
         (result (if stdlib
                     (compile-string-with-stdlib source :target :vm
                                                  :pass-pipeline pass-pipeline
                                                  :print-pass-timings print-pass-timings
                                                   :timing-stream timing-stream
                                                   :print-pass-stats print-pass-stats
                                                   :stats-stream stats-stream
                                                   :trace-json-stream trace-json-stream
                                                   :print-opt-remarks print-opt-remarks
                                                  :opt-remarks-stream opt-remarks-stream
                                                  :opt-remarks-mode opt-remarks-mode)
                     (compile-string source :target :vm
                                      :pass-pipeline pass-pipeline
                                      :print-pass-timings print-pass-timings
                                       :timing-stream timing-stream
                                       :print-pass-stats print-pass-stats
                                       :stats-stream stats-stream
                                       :trace-json-stream trace-json-stream
                                       :print-opt-remarks print-opt-remarks
                                      :opt-remarks-stream opt-remarks-stream
                                      :opt-remarks-mode opt-remarks-mode)))
         (program (compilation-result-program result)))
    (run-compiled program)))

(defun compile-string-with-stdlib (source &key (target :x86_64) type-check (safety 1) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  "Compile SOURCE with standard library prepended."
  (let ((stdlib-forms (get-stdlib-forms))
        (user-forms (parse-all-forms source)))
    (compile-toplevel-forms (append stdlib-forms user-forms)
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

(defun our-eval (form)
  "Evaluate FORM by compiling it and running it in the VM.
This is the self-hosting eval — used for compile-time macro expansion
instead of the host CL eval."
  (let* ((result (compile-expression form :target :vm))
         (program (compilation-result-program result)))
    (run-compiled program)))

;;; ─── Self-Hosting Bootstrap ──────────────────────────────────────────────
;;;
;;; Now that compile-expression and run-compiled are available, switch macro
;;; expansion from the host CL eval to our-eval.  From this point on, every
;;; defmacro/macrolet body is compiled and executed by cl-cc's own pipeline —
;;; the fundamental requirement for self-hosting.

(setf *macro-eval-fn* #'our-eval)

;;; REPL persistent state and run-string-repl/our-load are in pipeline-repl.lisp.

(defun run-string-typed (source &key (mode :warn) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  "Compile and run SOURCE with type checking enabled.
   MODE is :WARN (default, log warnings) or :STRICT (signal errors)."
  (let* ((result (compile-string source :target :vm :type-check mode
                                  :pass-pipeline pass-pipeline
                                  :print-pass-timings print-pass-timings
                                   :timing-stream timing-stream
                                   :print-pass-stats print-pass-stats
                                   :stats-stream stats-stream
                                   :trace-json-stream trace-json-stream
                                   :print-opt-remarks print-opt-remarks
                                  :opt-remarks-stream opt-remarks-stream
                                  :opt-remarks-mode opt-remarks-mode))
         (program (compilation-result-program result)))
    (values (run-compiled program) (compilation-result-type result))))

