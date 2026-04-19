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

(defun compile-expression (expr &key (target :x86_64) type-check (safety 1) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  (let* ((ctx            (make-instance 'compiler-context :safety safety))
         (ast            (optimize-ast (%prepare-ast expr)))
         (cps            (maybe-cps-transform ast))
         (inferred-type  (%type-check-safe ast type-check))
         (result-reg     (compile-ast ast ctx))
         (instructions   (nreverse (ctx-instructions ctx)))
         (full-instrs    (append instructions (list (make-vm-halt :reg result-reg)))))
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
      ;; Capture label counter for REPL continuity
      (when *repl-capture-label-counter*
        (setf *repl-capture-label-counter* (ctx-next-label ctx)))
       (let ((program (make-vm-program :instructions (or optimized-instrs full-instrs)
                                       :result-register result-reg
                                       :leaf-p leaf-p)))
         (make-compilation-result :program              program
                                  :assembly             (emit-assembly program :target target)
                                  :type                 (when type-check inferred-type)
                                  :type-env             (ctx-type-env ctx)
                                  :cps                  cps
                                  :ast                  ast
                                  :vm-instructions      full-instrs
                                  :optimized-instructions optimized-instrs)))))

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

(defun %copy-snapshot-ht (src)
  (let ((dst (make-hash-table :test #'eq :size (+ (hash-table-count src) 8))))
    (maphash (lambda (k v) (setf (gethash k dst) v)) src)
    dst))

(defvar *run-string-cps-fast-path-hook* nil
  "Optional test hook called with (SOURCE FORM VALUE) when RUN-STRING returns via the CPS fast path.")

(defun %try-run-string-cps-fast-path (source stdlib pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream print-pass-stats stats-stream trace-json-stream)
  "Return (values result t) when SOURCE can be executed through the CPS host fast path.
The fast path is intentionally limited to single-form, non-stdlib Lisp inputs so
definition forms and stdlib-dependent execution continue through the established VM pipeline."
  (if (or stdlib
          pass-pipeline
          print-pass-timings
          timing-stream
          print-opt-remarks
          opt-remarks-stream
          print-pass-stats
          stats-stream
          trace-json-stream)
      (values nil nil)
      (let ((forms (parse-source-for-language source :lisp)))
        (if (and (= (length forms) 1)
                 (not (and (consp (first forms)) (eq (caar forms) 'in-package))))
            (let ((form (first forms)))
              (multiple-value-bind (value ok)
                  (%try-cps-host-eval form)
                (when (and ok *run-string-cps-fast-path-hook*)
                  (funcall *run-string-cps-fast-path-hook* source form value))
                (values value ok)))
            (values nil nil)))))

(defun run-string (source &key stdlib pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  "Compile and run SOURCE. When STDLIB is true, include standard library."
  (let ((*package*          (find-package :cl-cc))
        (*labels-boxed-fns* nil)
        (compile-kwargs (list :target :vm
                              :pass-pipeline       pass-pipeline
                              :print-pass-timings  print-pass-timings
                              :timing-stream       timing-stream
                              :print-pass-stats    print-pass-stats
                              :stats-stream        stats-stream
                              :trace-json-stream   trace-json-stream
                              :print-opt-remarks   print-opt-remarks
                              :opt-remarks-stream  opt-remarks-stream
                              :opt-remarks-mode    opt-remarks-mode)))
    (multiple-value-bind (cps-value cps-ok)
        (%try-run-string-cps-fast-path source stdlib
                                       pass-pipeline print-pass-timings timing-stream
                                       print-opt-remarks opt-remarks-stream
                                       print-pass-stats stats-stream trace-json-stream)
      (if cps-ok
          cps-value
          (if (and stdlib *stdlib-vm-snapshot*)
              (let* ((*accessor-slot-map*       (%copy-snapshot-ht *stdlib-accessor-slot-map*))
                     (*defstruct-slot-registry* (%copy-snapshot-ht *stdlib-defstruct-slot-registry*))
                     (result  (apply #'compile-string source compile-kwargs))
                     (program (compilation-result-program result))
                     (state   (clone-vm-state *stdlib-vm-snapshot*)))
                (run-compiled program :state state))
              (let* ((*accessor-slot-map*       (make-hash-table :test #'eq))
                     (*defstruct-slot-registry* (make-hash-table :test #'eq))
                     (compile-fn (if stdlib #'compile-string-with-stdlib #'compile-string))
                     (result     (apply compile-fn source compile-kwargs))
                     (program    (compilation-result-program result)))
                (run-compiled program)))))))

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
