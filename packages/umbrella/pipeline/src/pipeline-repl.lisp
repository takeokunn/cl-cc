(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Compile — REPL Persistent State and Incremental Execution
;;;
;;; Contains: *repl-vm-state*, *repl-accessor-map*, *repl-pool-instructions*,
;;; *repl-pool-labels*, *repl-global-vars-persistent*, *repl-defstruct-registry*,
;;; reset-repl-state, %ensure-repl-state, run-string-repl,
;;; %prescan-in-package, our-load.
;;;
;;; Load order: after pipeline.lisp (compile-expression, our-eval).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── REPL Persistent State ────────────────────────────────────────────────
;;;
;;; The REPL accumulates all compiled instructions into a shared pool so that
;;; closures defined in one expression (with entry labels in that expression's
;;; instruction range) remain callable in subsequent expressions.  Each new
;;; compile-string result is appended to *repl-pool-instructions*, and its
;;; labels are inserted into *repl-pool-labels* with a global offset.  Only
;;; the newly-added slice is executed, but the full pool's label table is used
;;; for all label lookups — so cross-call closure invocations work correctly.

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

(defun reset-repl-state ()
  "Reset the REPL persistent state, starting a completely fresh session."
  (setf *repl-vm-state* nil
        *repl-accessor-map* nil
        *repl-pool-instructions* nil
        *repl-pool-labels* nil
        *repl-global-vars-persistent* nil
        *repl-defstruct-registry* nil
        *repl-label-counter* nil))

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

(defun run-string-repl (source)
  "Compile and run SOURCE using the persistent REPL state.
Unlike run-string, this reuses the VM state (function-registry, class-registry,
heap) across calls so that top-level definitions persist into later expressions.
Cross-expression closure calls work because all instructions share one pool.

Example:
  (run-string-repl \"(defun double (x) (* x 2))\")
  (run-string-repl \"(double 21)\")  ; => 42"
  (%ensure-repl-state)
  (let* ((*package* *package*)
         (*accessor-slot-map* *repl-accessor-map*)
         (*defstruct-slot-registry* *repl-defstruct-registry*)
         (*labels-boxed-fns* nil)
         ;; Bind persistent globals so compiler-context picks them up
         (*repl-global-variables* *repl-global-vars-persistent*)
         ;; Enable label counter capture so we can persist it
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
      (let* ((result (compile-string source :target :vm))
             (program (compilation-result-program result))
             (new-insts (vm-program-instructions program))
             ;; PC where the new code will start in the global pool
             (start-pc (fill-pointer *repl-pool-instructions*)))
        ;; Persist the label counter for the next compilation
        (when (integerp *repl-capture-label-counter*)
          (setf *repl-label-counter* *repl-capture-label-counter*))
        ;; Track any new global variables defined by this compilation
        (dolist (inst new-insts)
          (when (typep inst 'vm-set-global)
            (setf (gethash (vm-global-name inst) *repl-global-vars-persistent*) t)))
        ;; Append new instructions to the shared pool
        (dolist (inst new-insts)
          (vector-push-extend inst *repl-pool-instructions*))
        ;; Merge new labels (with global offset) into the pool label table
        (let ((new-labels (build-label-table new-insts)))
          (maphash (lambda (_key bucket)
                     (declare (ignore _key))
                     (dolist (entry bucket)
                       (vm-label-table-store *repl-pool-labels*
                                             (car entry)
                                             (+ start-pc (cdr entry)))))
                    new-labels))
        ;; Execute only the new slice, using the full pool for label resolution
        (run-program-slice *repl-pool-instructions* *repl-pool-labels*
                           start-pc *repl-vm-state*)))))

;;; ─── Self-Hosting Load ──────────────────────────────────────────────────
;;;
;;; (our-load pathname) reads a file, parses all forms, and compiles+executes
;;; them in the persistent REPL state. This is the key primitive for cl-cc to
;;; load its own source files.

(defun %whitespace-symbol-p (form)
  "T if FORM is a non-NIL, non-keyword symbol whose name has no non-space graphic chars."
  (and (symbolp form) (not (null form)) (not (keywordp form))
       (let ((name (symbol-name form)))
         (and (> (length name) 0)
              (every (lambda (c) (or (not (graphic-char-p c)) (eql c #\Space))) name)))))

(defun %prescan-in-package (source)
  "Pre-scan SOURCE for an (in-package ...) form and return the package name string.
   Returns nil if not found. Used to set *package* before full parsing so that
   #. read-time eval resolves symbols in the correct package."
  (let ((pos (search "(in-package " source :test #'char-equal)))
    (when pos
      (let* ((start (+ pos (length "(in-package ")))
             (trimmed (string-trim '(#\Space #\Tab) (subseq source start))))
        ;; Handle :pkg, :pkg), "pkg", 'pkg forms
        (cond
          ((and (> (length trimmed) 0) (char= (first (coerce trimmed 'list)) #\:))
           (let ((end (position-if (lambda (c) (or (char= c #\)) (char= c #\Space))) trimmed)))
             (when end (subseq trimmed 1 end))))
          ((and (> (length trimmed) 0) (char= (first (coerce trimmed 'list)) #\"))
           (let ((end (position #\" trimmed :start 1)))
             (when end (subseq trimmed 1 end))))
          (t nil))))))

(defun our-load (pathname &key (verbose nil) (print nil) (if-does-not-exist :error)
                                 (external-format :default))
  "Load a Lisp source file by reading, compiling, and executing each form.
Uses the persistent REPL state so definitions accumulate across forms.
VERBOSE prints the file being loaded. PRINT prints each form's result.
IF-DOES-NOT-EXIST controls behavior when file is missing (:error or nil).
EXTERNAL-FORMAT is accepted but ignored (UTF-8 assumed)."
  (declare (ignore external-format))
  (when (and (eq if-does-not-exist nil) (not (probe-file pathname)))
    (return-from our-load nil))
  (let ((path (namestring (truename pathname))))
    (when verbose
      (format *standard-output* "; Loading ~A~%" path))
    (let ((source (with-open-file (in path :direction :input)
                    (let ((buf (make-string (file-length in))))
                      (read-sequence buf in)
                      buf))))
      ;; Pre-scan for (in-package ...) to set *package* before parsing,
      ;; so #. read-time eval resolves symbols in the correct package.
      (let* ((pkg-name (%prescan-in-package source))
             (pkg (when pkg-name (find-package (string-upcase pkg-name))))
             (*package* (or pkg *package*)))
        ;; Parse all forms and compile/run each through the REPL pipeline
        (let ((forms (parse-all-forms source))
              (last-result nil))
          (dolist (form forms last-result)
            (let* ((package-form-p (and (consp form) (eq (car form) 'in-package)))
                   (whitespace-symbol-p (%whitespace-symbol-p form))
                   (unsupported-p (and (consp form)
                                       (member (car form) '(deftype defopcode)))))
              (cond
                (package-form-p
                 (let ((pkg (find-package (second form))))
                   (unless pkg
                     (error "Unknown package: ~S" (second form)))
                   (setf *package* pkg)
                   (setf last-result (second form))))
                ((or whitespace-symbol-p unsupported-p)
                 nil)
                (t
                 (let ((form-str (write-to-string form)))
                   (setf last-result
                         (handler-case (run-string-repl form-str)
                           (error (e)
                             (format *error-output* "; Error loading ~A: ~A~%  Form: ~S~%"
                                     path e form)
                             nil)))
                   (when print
                     (format *standard-output* "~S~%" last-result))))))))))))

;;; Register run-string-repl and our-load in the VM host bridge.
;;; (run-string and compile-* are registered in pipeline.lisp.)
(eval-when (:load-toplevel :execute)
  (dolist (sym '(run-string-repl our-load))
    (vm-register-host-bridge sym)))
