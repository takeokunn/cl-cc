(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Pipeline — Self-Hosting, CPS Fast Path, and Bootstrap
;;;
;;; Contains: %build-stdlib-vm-snapshot, warm-stdlib-cache, our-eval,
;;; %try-cps-host-eval, %register-host-bridge-entries, VM hook wiring,
;;; and run-string-typed.
;;;
;;; Core compilation API (compile-expression, run-string, etc.)
;;; is in pipeline.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun %build-stdlib-vm-snapshot ()
  (let* ((*accessor-slot-map*       (make-hash-table :test #'eq))
         (*defstruct-slot-registry* (make-hash-table :test #'eq))
         (*labels-boxed-fns*        nil)
         (stdlib-program  (compile-toplevel-forms (get-stdlib-forms) :target :vm))
         (snapshot-state  (make-instance 'cl-cc/vm:vm-io-state
                                         :output-stream (make-broadcast-stream))))
    (cl-cc/vm:run-compiled stdlib-program :state snapshot-state)
    (setf *stdlib-accessor-slot-map*       (%copy-snapshot-ht *accessor-slot-map*)
          *stdlib-defstruct-slot-registry* (%copy-snapshot-ht *defstruct-slot-registry*))
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
  "Return T when AST is safe to evaluate through the host CPS fast path."
  (notany (lambda (type) (typep ast type)) *cps-host-eval-unsafe-ast-types*))

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

;;; REPL persistent state and run-string-repl/our-load are split across
;;; pipeline-repl-state.lisp and pipeline-repl-load.lisp.

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
