(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Compile — REPL Persistent State and Incremental Execution
;;;
;;; Contains: *repl-vm-state*, *repl-accessor-map*, *repl-pool-instructions*,
;;; *repl-pool-labels*, *repl-global-vars-persistent*, *repl-defstruct-registry*,
;;; reset-repl-state, with-fresh-repl-state, %ensure-repl-state,
;;; %run-form-repl-impl, run-string-repl.
;;;
;;; Host-load and top-level macro normalization helpers live in
;;; pipeline-repl-load.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defvar *repl-vm-state* nil
  "Persistent VM state for the interactive REPL.
Reused across form evaluations so that functions and variables defined
in one expression remain accessible in subsequent ones.")

(defvar *repl-accessor-map* nil
  "Persistent accessor map for the REPL.
Accumulates defstruct slot accessor mappings across form evaluations.")

(defvar *repl-pool-instructions* nil
  "Adjustable vector accumulating ALL instructions from the current REPL session.
Enables cross-expression closure calls (body labels remain globally valid).")

(defvar *repl-pool-labels* nil
  "Hash table mapping label names to absolute PCs in *repl-pool-instructions*.")

(defvar *repl-global-vars-persistent* nil
  "Persistent hash table tracking global variable names defined across REPL calls.
When non-nil, this is bound to *repl-global-variables* during compilation
so that variables from (defvar ...) in one REPL call are visible in the next.")

(defvar *repl-defstruct-registry* nil
  "Persistent defstruct slot registry for the REPL.
Accumulates slot info across form evaluations so :include works across calls.")

(defvar *our-load-host-definition-mode* nil
  "When true, OUR-LOAD may evaluate host-only top-level definition forms directly.")

(defvar *our-load-current-path* nil
  "Current source pathname being processed by OUR-LOAD, when available.")

(eval-when (:load-toplevel :execute)
  ;; Keep selfhost macro expansion pinned to OUR-EVAL even when this file is
  ;; loaded or instrumented separately from pipeline.lisp (for example under
  ;; coverage builds). The transient host-side `let` bindings used while
  ;; building top-level macro expanders should never leak into the steady-state
  ;; compiler configuration observed by tests and later loads.
  (setf *macro-eval-fn* #'our-eval))

(defun reset-repl-state ()
  "Reset the REPL persistent state, starting a completely fresh session."
  (setf *repl-vm-state* nil
        *repl-accessor-map* nil
        *repl-pool-instructions* nil
        *repl-pool-labels* nil
        *repl-global-vars-persistent* nil
        *repl-defstruct-registry* nil
        *repl-label-counter* nil))

(defmacro with-fresh-repl-state (&body body)
  "Execute BODY with a dynamically-scoped fresh REPL state.
Unlike reset-repl-state (setf-based, clobbers outer state), this binds
each persistent REPL special to nil for the dynamic extent of BODY, so
the outer REPL state is preserved after BODY returns.

Use in tests and selfhost phases that need isolated REPL state without
disturbing any enclosing REPL session."
  (cons 'let
        (cons '((*repl-vm-state* nil)
                (*repl-accessor-map* nil)
                (*repl-pool-instructions* nil)
                (*repl-pool-labels* nil)
                (*repl-global-vars-persistent* nil)
                (*repl-label-counter* nil)
                (*repl-defstruct-registry* nil))
              body)))

(export 'with-fresh-repl-state :cl-cc)

(defun %ensure-repl-state ()
  "Lazily initialise all persistent REPL state variables on first use."
  (unless *repl-vm-state*
    (setf *repl-vm-state* (make-instance 'vm-io-state :output-stream *standard-output*)))
  (unless *repl-accessor-map*
    (setf *repl-accessor-map* (make-hash-table :test #'eq)))
  (unless *repl-pool-instructions*
    (setf *repl-pool-instructions* (make-array 64 :adjustable t :fill-pointer 0 :element-type t)))
  (unless *repl-pool-labels*
    (setf *repl-pool-labels* (make-hash-table :test #'eql)))
  (unless *repl-global-vars-persistent*
    (setf *repl-global-vars-persistent* (make-hash-table :test #'eq)))
  (unless *repl-defstruct-registry*
    (setf *repl-defstruct-registry* (make-hash-table :test #'eq))))

(defun %run-form-repl-impl (result)
  "Finalize compilation RESULT into the REPL persistent pool and execute
   the newly-added instruction slice.  Factors out the shared post-compile
   machinery used by both run-string-repl and run-form-repl."
  (let* ((program (compilation-result-program result))
         (new-insts (vm-program-instructions program))
         (start-pc (fill-pointer *repl-pool-instructions*)))
    (when (integerp *repl-capture-label-counter*)
      (setf *repl-label-counter* *repl-capture-label-counter*))
    (dolist (inst new-insts)
      (when (typep inst 'vm-set-global)
        (setf (gethash (vm-global-name inst) *repl-global-vars-persistent*) t)))
    (dolist (inst new-insts)
      (vector-push-extend inst *repl-pool-instructions*))
    (let ((new-labels (build-label-table new-insts)))
      (maphash (lambda (_key bucket)
                 (declare (ignore _key))
                 (dolist (entry bucket)
                   (vm-label-table-store *repl-pool-labels*
                                         (car entry)
                                         (+ start-pc (cdr entry)))))
               new-labels))
    (run-program-slice *repl-pool-instructions* *repl-pool-labels*
                       start-pc *repl-vm-state*)))

(defun run-string-repl (source)
  "Compile and run SOURCE using the persistent REPL state.
Unlike run-string, this reuses the VM state (function-registry, class-registry,
heap) across calls so that top-level definitions persist into later expressions.
Cross-expression closure calls work because all instructions share one pool.

Example:
  (run-string-repl "(defun double (x) (* x 2))")
  (run-string-repl "(double 21)")  ; => 42"
  (%ensure-repl-state)
  (let* ((*package* *package*)
         (*accessor-slot-map* *repl-accessor-map*)
         (*defstruct-slot-registry* *repl-defstruct-registry*)
         (*labels-boxed-fns* nil)
         (*repl-global-variables* *repl-global-vars-persistent*)
         (*repl-capture-label-counter* t))
    (let ((forms (parse-source-for-language source :lisp)))
      (when (and (= (length forms) 1)
                 (consp (first forms))
                 (eq (caar forms) 'in-package))
        (let ((pkg (find-package (second (first forms)))))
          (unless pkg
            (error "Unknown package: ~S" (second (first forms))))
          (setf *package* pkg)
          (return-from run-string-repl (second (first forms)))))
      (%run-form-repl-impl (compile-string source :target :vm)))))
