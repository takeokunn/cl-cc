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
  (let ((*package* (find-package :cl-cc))
        (*labels-boxed-fns* nil))
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
                     (result (compile-string source :target :vm
                                             :pass-pipeline pass-pipeline
                                             :print-pass-timings print-pass-timings
                                             :timing-stream timing-stream
                                             :print-pass-stats print-pass-stats
                                             :stats-stream stats-stream
                                             :trace-json-stream trace-json-stream
                                             :print-opt-remarks print-opt-remarks
                                             :opt-remarks-stream opt-remarks-stream
                                             :opt-remarks-mode opt-remarks-mode))
                     (program (compilation-result-program result))
                     (state   (clone-vm-state *stdlib-vm-snapshot*)))
                (run-compiled program :state state))
              (let* ((*accessor-slot-map*       (make-hash-table :test #'eq))
                     (*defstruct-slot-registry* (make-hash-table :test #'eq))
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

(defun %build-stdlib-vm-snapshot ()
  (let* ((*accessor-slot-map*       (make-hash-table :test #'eq))
         (*defstruct-slot-registry* (make-hash-table :test #'eq))
         (*labels-boxed-fns*        nil)
         (stdlib-program
           (compile-toplevel-forms (get-stdlib-forms) :target :vm))
         (snapshot-state
           (make-instance 'cl-cc/vm:vm-io-state
                          :output-stream (make-broadcast-stream))))
    (cl-cc/vm:run-compiled stdlib-program :state snapshot-state)
    (setf *stdlib-accessor-slot-map*
          (let ((dst (make-hash-table :test #'eq
                                      :size (hash-table-count *accessor-slot-map*))))
            (maphash (lambda (k v) (setf (gethash k dst) v)) *accessor-slot-map*)
            dst))
    (setf *stdlib-defstruct-slot-registry*
          (let ((dst (make-hash-table :test #'eq
                                      :size (hash-table-count *defstruct-slot-registry*))))
            (maphash (lambda (k v) (setf (gethash k dst) v)) *defstruct-slot-registry*)
            dst))
    snapshot-state))

(defun warm-stdlib-cache ()
  (get-stdlib-forms)
  (unless (and *stdlib-vm-snapshot*
               (eq *stdlib-expanded-cache-source*  *standard-library-source*)
               (eq *stdlib-expanded-cache-eval-fn* *macro-eval-fn*))
    (setf *stdlib-vm-snapshot* (%build-stdlib-vm-snapshot)))
  (values))

(defvar *repl-vm-state*)

(defun %cps-host-eval-safe-ast-p (ast)
  "Return T when AST is safe to evaluate through the host CPS path.
Definition-like top-level forms keep using the existing compile→VM route so
registry/class/global side effects still flow through the established pipeline." 
  (not (typecase ast
          (cl-cc/ast::ast-defun t)
          (cl-cc/ast::ast-defvar t)
          (cl-cc/ast::ast-defclass t)
          (cl-cc/ast::ast-defgeneric t)
          (cl-cc/ast::ast-defmethod t)
          (cl-cc/ast::ast-block t)
          (cl-cc/ast::ast-return-from t)
          (cl-cc/ast::ast-catch t)
          (cl-cc/ast::ast-throw t)
          (cl-cc/ast::ast-tagbody t)
          (cl-cc/ast::ast-go t)
          (cl-cc/ast::ast-unwind-protect t)
          (cl-cc/ast::ast-handler-case t)
          (t nil))))

(defun %try-cps-host-eval (form)
  "Best-effort CPS-backed host evaluation for ordinary expressions.
Returns two values: the result and whether the CPS path succeeded." 
  (handler-case
      (let* ((expanded-form (if (typep form 'cl-cc/ast:ast-node)
                                form
                                (compiler-macroexpand-all form)))
             (ast (if (typep expanded-form 'cl-cc/ast:ast-node)
                      expanded-form
                      (lower-sexp-to-ast expanded-form))))
        (if (%cps-host-eval-safe-ast-p ast)
            (let ((cps (maybe-cps-transform ast)))
              (if cps
                  (values (funcall (eval cps) #'identity) t)
                  (values nil nil)))
            (values nil nil)))
    (error (_)
      (declare (ignore _))
      (values nil nil))))

(defun our-eval (form)
  "Evaluate FORM by compiling it and running it in the VM.
This is the self-hosting eval — used for compile-time macro expansion
instead of the host CL eval.
When *repl-vm-state* is available, reuses it so the compiled code has
access to all previously registered functions (essential for macro
expansion during self-host loading)."
  (multiple-value-bind (cps-value cps-ok) (%try-cps-host-eval form)
    (if cps-ok
        cps-value
        (let* ((result (compile-expression form :target :vm))
               (program (compilation-result-program result)))
          (run-compiled program :state cl-cc::*repl-vm-state*)))))

;;; ─── Self-Hosting Bootstrap ──────────────────────────────────────────────
;;;
;;; Now that compile-expression and run-compiled are available, switch macro
;;; expansion from the host CL eval to our-eval.  From this point on, every
;;; defmacro/macrolet body is compiled and executed by cl-cc's own pipeline —
;;; the fundamental requirement for self-hosting.

(defparameter *early-selfhost-macro-bridge-entries*
  '(("PARSE-LAMBDA-LIST"            . :cl-cc/expand)
    ("DESTRUCTURE-LAMBDA-LIST"      . :cl-cc/expand)
    ("GENERATE-LAMBDA-BINDINGS"     . :cl-cc/expand)
    ("LAMBDA-LIST-INFO-ENVIRONMENT" . :cl-cc/expand))
  "Bridge helpers required before selfhost macro evaluation switches to OUR-EVAL.")

(defun %register-host-bridge-entries (entries)
  "Register every (NAME . PACKAGE) pair in ENTRIES when both package and symbol exist."
  (dolist (entry entries)
    (let* ((pkg (find-package (cdr entry)))
           (sym (when pkg (find-symbol (car entry) pkg))))
      (when sym
        (vm-register-host-bridge sym)))))

(eval-when (:load-toplevel :execute)
  (%register-host-bridge-entries *early-selfhost-macro-bridge-entries*))

(setf *macro-eval-fn* #'our-eval)

;;; Wire compile functions into VM hooks for runtime EVAL/compile support
(eval-when (:load-toplevel :execute)
  (when (find-package :cl-cc/vm)
    (let ((pkg (find-package :cl-cc/vm)))
      (setf (symbol-value (find-symbol "*VM-EVAL-HOOK*" pkg)) #'our-eval)
      (setf (symbol-value (find-symbol "*VM-COMPILE-STRING-HOOK*" pkg)) #'compile-string))))

;;; REPL persistent state and run-string-repl/our-load are in pipeline-repl.lisp.

;;; Re-register cross-package symbols in the VM host bridge.
;;; vm-bridge.lisp runs when :cl-cc-vm first loads; if :cl-cc-compile/:cl-cc-parse/
;;; :cl-cc-expand packages don't exist yet (e.g. when :cl-cc-optimize depends on
;;; :cl-cc-vm and loads before the facades), the registration is silently skipped.
;;; This eval-when ensures registration completes once all packages are present.
#-cl-cc-self-hosting
(eval-when (:load-toplevel :execute)
  ;; NOTE: register-macro is intentionally excluded — it stores VM closures in
  ;; macro-env, causing TYPE-ERROR when host CL funcalls them. See vm-bridge.lisp.
  (%register-host-bridge-entries
   '(("RUN-STRING"                   . :cl-cc)
     ("COMPILE-EXPRESSION"           . :cl-cc)
     ("COMPILE-STRING"               . :cl-cc)
     ("OUR-EVAL"                     . :cl-cc)
     ("PARSE-ALL-FORMS"              . :cl-cc)
     ;; Register both umbrella-exported and package-local symbols.
     ;; Selfhosted expand files intern unqualified helper calls in :cl-cc/expand,
     ;; while bridge regression tests assert the public :cl-cc exports stay wired.
     ("PARSE-LAMBDA-LIST"            . :cl-cc)
     ("PARSE-LAMBDA-LIST"            . :cl-cc/expand)
     ("DESTRUCTURE-LAMBDA-LIST"      . :cl-cc)
     ("DESTRUCTURE-LAMBDA-LIST"      . :cl-cc/expand)
     ("GENERATE-LAMBDA-BINDINGS"     . :cl-cc)
     ("GENERATE-LAMBDA-BINDINGS"     . :cl-cc/expand)
     ("LAMBDA-LIST-INFO-ENVIRONMENT" . :cl-cc/expand))))

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
