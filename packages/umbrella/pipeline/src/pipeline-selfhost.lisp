(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Pipeline — Self-Hosting, CPS Fast Path, and Bootstrap
;;;
;;; Contains: %build-stdlib-vm-snapshot, warm-stdlib-cache, our-eval,
;;; %register-host-bridge-entries, VM hook wiring,
;;; and run-string-typed.
;;;
;;; Core compilation API (compile-expression, run-string, etc.)
;;; is in pipeline.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun %build-stdlib-vm-snapshot ()
  (let* ((*accessor-slot-map*       (make-hash-table :test #'eq))
         (*defstruct-slot-registry* (make-hash-table :test #'eq))
         (*labels-boxed-fns*        nil)
         (stdlib-result   (compile-toplevel-forms (get-stdlib-forms) :target :vm))
         (stdlib-program  (compilation-result-program stdlib-result))
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

(defun our-eval (form)
  "Evaluate FORM by compiling it and running it in the VM.
This is the self-hosting eval — used for compile-time macro expansion
instead of the host CL eval.
When *repl-vm-state* is available, reuses it so the compiled code has
access to all previously registered functions (essential for macro
expansion during self-host loading)."
  (let* ((result (compile-expression form :target :vm))
         (program (compilation-result-program result)))
    (run-compiled program :state cl-cc::*repl-vm-state*)))

;;; ─── Self-Hosting Bootstrap ──────────────────────────────────────────────
;;;
;;; Now that compile-expression and run-compiled are available, switch macro
;;; expansion from the host CL eval to our-eval.  From this point on, every
;;; defmacro/macrolet body is compiled and executed by cl-cc's own pipeline —
;;; the fundamental requirement for self-hosting.

(defun %register-host-bridge-entries (entries)
  "Register every (SYMBOL . FUNCTION) pair in ENTRIES directly into the VM bridge."
  (dolist (entry entries)
    (vm-register-host-bridge (car entry) (cdr entry))))

(eval-when (:load-toplevel :execute)
  (%register-host-bridge-entries
   `((cl-cc/expand::parse-lambda-list . ,#'cl-cc/expand::parse-lambda-list)
     (cl-cc/expand::destructure-lambda-list . ,#'cl-cc/expand::destructure-lambda-list)
     (cl-cc/expand::generate-lambda-bindings . ,#'cl-cc/expand::generate-lambda-bindings)
     (cl-cc/expand::lambda-list-info-environment . ,#'cl-cc/expand::lambda-list-info-environment))))

(setf *macro-eval-fn* #'our-eval)

;;; Wire compile functions into VM hooks for runtime EVAL/compile support
(defun %vm-install-eval-hooks-if-available ()
  (let* ((pkg (cl-cc/runtime:rt-find-package :cl-cc/vm))
         (sym (and pkg (find-symbol "VM-INSTALL-EVAL-HOOKS" pkg))))
    (when (and sym (fboundp sym))
      (funcall (symbol-function sym) #'our-eval #'compile-string))))

(eval-when (:load-toplevel :execute)
  (%vm-install-eval-hooks-if-available))

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
   `((cl-cc::run-string . ,#'cl-cc::run-string)
     (cl-cc::compile-expression . ,#'cl-cc::compile-expression)
     (cl-cc::compile-string . ,#'cl-cc::compile-string)
     (cl-cc::our-eval . ,#'cl-cc::our-eval)
     (cl-cc::parse-all-forms . ,#'cl-cc::parse-all-forms)
     (cl-cc/expand::parse-lambda-list . ,#'cl-cc/expand::parse-lambda-list)
     (cl-cc/expand::destructure-lambda-list . ,#'cl-cc/expand::destructure-lambda-list)
     (cl-cc/expand::generate-lambda-bindings . ,#'cl-cc/expand::generate-lambda-bindings)
     (cl-cc/expand::lambda-list-info-environment . ,#'cl-cc/expand::lambda-list-info-environment))))

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
