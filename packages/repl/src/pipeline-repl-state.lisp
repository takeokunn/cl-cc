(in-package :cl-cc/repl)
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

(defvar *repl-defstruct-read-only-accessor-map* nil
  "Persistent read-only defstruct accessor map for the REPL.")

(defvar *repl-defstruct-type-registry* nil
  "Persistent defstruct representation registry for the REPL.")

(defvar *repl-setf-compound-place-handlers* nil
  "Persistent SETF compound-place handlers for the REPL.
Starts with the built-in handler table and accumulates typed defstruct accessors
defined across form evaluations.")

(defvar *repl-pool-instructions* nil
  "Adjustable vector accumulating ALL instructions from the current REPL session.
Enables cross-expression closure calls (body labels remain globally valid).")

(defvar *repl-pool-labels* nil
  "Hash table mapping label names to absolute PCs in *repl-pool-instructions*.")

(defvar *repl-global-vars-persistent* nil
  "Persistent hash table tracking global variable names defined across REPL calls.
When non-nil, this is bound to *repl-global-variables* during compilation
so that variables from (defvar ...) in one REPL call are visible in the next.")

(defvar *repl-history* nil
  "Persistent command history for the interactive REPL.
Stored newest-last as a list of source strings so CLI line editing can walk it
without coupling the compiler REPL state to terminal details.")

(defvar *repl-history-cursor* nil
  "Current zero-based offset from newest history entry while editing input.")

(defparameter *repl-history-limit* 200
  "Maximum number of REPL history entries retained in memory.")

(defvar *repl-defstruct-registry* nil
  "Persistent defstruct slot registry for the REPL.
Accumulates slot info across form evaluations so :include works across calls.")

(defvar *our-load-host-definition-mode* nil
  "When true, OUR-LOAD may evaluate host-only top-level definition forms directly.")

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
        *repl-defstruct-read-only-accessor-map* nil
        *repl-defstruct-type-registry* nil
        *repl-setf-compound-place-handlers* nil
        *repl-pool-instructions* nil
        *repl-pool-labels* nil
        *repl-global-vars-persistent* nil
        *repl-history* nil
        *repl-history-cursor* nil
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
                 (*repl-defstruct-read-only-accessor-map* nil)
                 (*repl-defstruct-type-registry* nil)
                 (*repl-setf-compound-place-handlers* nil)
                 (*repl-pool-instructions* nil)
                 (*repl-pool-labels* nil)
                 (*repl-global-vars-persistent* nil)
                 (*repl-history* nil)
                 (*repl-history-cursor* nil)
                 (*repl-label-counter* nil)
                 (*repl-defstruct-registry* nil))
              body)))

;; with-fresh-repl-state is exported from :cl-cc/repl via package.lisp,
;; and re-exported into :cl-cc by the umbrella package.lisp.

(defun %ensure-repl-state ()
  "Lazily initialise all persistent REPL state variables on first use."
  (unless *repl-vm-state*
    (setf *repl-vm-state* (make-vm-state :output-stream *standard-output*)))
  (unless *repl-accessor-map*
    (setf *repl-accessor-map* (make-hash-table :test #'eq)))
  (unless *repl-defstruct-read-only-accessor-map*
    (setf *repl-defstruct-read-only-accessor-map* (make-hash-table :test #'eq)))
  (unless *repl-defstruct-type-registry*
    (setf *repl-defstruct-type-registry* (make-hash-table :test #'eq)))
  (unless *repl-setf-compound-place-handlers*
    (setf *repl-setf-compound-place-handlers*
          (cl-cc/pipeline::%copy-snapshot-ht *setf-compound-place-handlers*)))
  (unless *repl-pool-instructions*
    (setf *repl-pool-instructions* (make-array 64 :adjustable t :fill-pointer 0 :element-type t)))
  (unless *repl-pool-labels*
    (setf *repl-pool-labels* (make-hash-table :test #'eql)))
  (unless *repl-global-vars-persistent*
    (setf *repl-global-vars-persistent* (make-hash-table :test #'eq)))
  (unless *repl-defstruct-registry*
    (setf *repl-defstruct-registry* (make-hash-table :test #'eq))))

(defun %repl-trim-input (source)
  "Return SOURCE trimmed for history and completion bookkeeping."
  (string-trim '(#\Space #\Tab #\Newline #\Return) source))

(defun %repl-record-history (source)
  "Record non-empty SOURCE in REPL history and return it."
  (%ensure-repl-state)
  (let ((entry (%repl-trim-input source)))
    (unless (string= entry "")
      (setf *repl-history*
            (append *repl-history* (list entry)))
      (setf *repl-history-cursor* nil)
      (let ((overflow (- (length *repl-history*) *repl-history-limit*)))
        (when (plusp overflow)
          (setf *repl-history* (nthcdr overflow *repl-history*))))
      entry)))

(defun repl-history ()
  "Return the current REPL history, oldest entry first."
  (%ensure-repl-state)
  (copy-list *repl-history*))

(defun repl-history-entry (index-from-newest)
  "Return a history entry by zero-based INDEX-FROM-NEWEST, or NIL."
  (%ensure-repl-state)
  (let* ((history *repl-history*)
         (index (- (length history) index-from-newest 1)))
    (when (and (integerp index-from-newest)
               (not (minusp index-from-newest))
               (<= 0 index)
               (< index (length history)))
      (nth index history))))

(defun repl-history-previous ()
  "Move one entry backward through history and return the selected entry."
  (%ensure-repl-state)
  (when *repl-history*
    (let ((next-index (if *repl-history-cursor*
                          (min (1+ *repl-history-cursor*)
                               (1- (length *repl-history*)))
                          0)))
      (setf *repl-history-cursor* next-index)
      (repl-history-entry next-index))))

(defun repl-history-next ()
  "Move one entry forward through history and return it, or empty input at end."
  (%ensure-repl-state)
  (cond
    ((null *repl-history-cursor*) "")
    ((zerop *repl-history-cursor*)
     (setf *repl-history-cursor* nil)
     "")
    (t
     (decf *repl-history-cursor*)
     (or (repl-history-entry *repl-history-cursor*) ""))))

(defun %repl-string-prefix-p (prefix string)
  "Return T when PREFIX matches the beginning of STRING, case-insensitively."
  (let ((prefix-length (length prefix)))
    (and (<= prefix-length (length string))
         (string-equal prefix string :end2 prefix-length))))

(defun %repl-symbol-candidate-string (symbol)
  "Return the printable completion candidate for SYMBOL."
  (symbol-name symbol))

(defun %repl-collect-hash-symbol-keys (table candidates)
  "Collect symbol keys from TABLE into CANDIDATES and return CANDIDATES."
  (when table
    (maphash (lambda (key _value)
               (declare (ignore _value))
               (when (symbolp key)
                 (push (%repl-symbol-candidate-string key) candidates)))
             table))
  candidates)

(defun repl-completion-candidates (prefix &key (package *package*) (state *repl-vm-state*))
  "Return sorted REPL completion candidates matching PREFIX.

Candidates include symbols visible in PACKAGE plus names registered in the
persistent VM function, class, and global-variable registries.  The result is a
duplicate-free list of uppercase candidate strings."
  (%ensure-repl-state)
  (let ((candidates nil)
        (normalized-prefix (string-upcase (or prefix ""))))
    (when package
      (do-symbols (symbol package)
        (push (%repl-symbol-candidate-string symbol) candidates)))
    (when state
      (setf candidates (%repl-collect-hash-symbol-keys (vm-function-registry state) candidates))
      (setf candidates (%repl-collect-hash-symbol-keys (vm-class-registry state) candidates))
      (setf candidates (%repl-collect-hash-symbol-keys (vm-global-vars state) candidates)))
    (sort (remove-duplicates
           (remove-if-not (lambda (candidate)
                            (%repl-string-prefix-p normalized-prefix candidate))
                          candidates)
           :test #'string=)
          #'string<)))

(defun %repl-token-start (line end)
  "Return the start offset of the token ending at END in LINE."
  (loop for index downfrom (1- end) to 0
        for char = (char line index)
        when (find char '(#\( #\) #\' #\` #\" #\, #\; #\Space #\Tab #\Newline #\Return))
          return (1+ index)
        finally (return 0)))

(defun %repl-complete-input-line (line)
  "Complete LINE at the first tab character.
Returns three values: completed line, candidates, and whether a replacement was made."
  (let ((tab-pos (position #\Tab line)))
    (if (null tab-pos)
        (values line nil nil)
        (let* ((token-start (%repl-token-start line tab-pos))
               (prefix (subseq line token-start tab-pos))
               (candidates (repl-completion-candidates prefix)))
          (if (= (length candidates) 1)
              (values (concatenate 'string
                                   (subseq line 0 token-start)
                                   (first candidates)
                                   (subseq line (1+ tab-pos)))
                      candidates
                      t)
              (values (concatenate 'string
                                   (subseq line 0 tab-pos)
                                   (subseq line (1+ tab-pos)))
                      candidates
                      nil))))))

(defun %repl-arrow-up-line-p (line)
  "Return T when LINE is an up-arrow escape sequence read by a cooked terminal."
  (or (string= line (format nil "~C[A" #\Escape))
      (string= line (format nil "~COA" #\Escape))))

(defun %repl-arrow-down-line-p (line)
  "Return T when LINE is a down-arrow escape sequence read by a cooked terminal."
  (or (string= line (format nil "~C[B" #\Escape))
      (string= line (format nil "~COB" #\Escape))))

(defun repl-edit-input-line (line)
  "Apply lightweight REPL line-editing commands to LINE.

This works with the existing READ-LINE based REPL: in cooked terminals, arrow
keys and TAB arrive as ordinary characters before Return.  The function handles
history navigation and single-candidate completion now, while keeping the API
small enough to swap in a raw-mode line editor later.

Returns three values: edited line, completion candidates, and whether the line
was replaced or completed."
  (cond
    ((%repl-arrow-up-line-p line)
     (values (or (repl-history-previous) "") nil t))
    ((%repl-arrow-down-line-p line)
     (values (repl-history-next) nil t))
    (t
     (%repl-complete-input-line line))))

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
          (*macro-eval-fn* #'our-eval)
           (*accessor-slot-map* *repl-accessor-map*)
           (*defstruct-read-only-accessor-map* *repl-defstruct-read-only-accessor-map*)
           (*defstruct-slot-registry* *repl-defstruct-registry*)
           (*defstruct-type-registry* *repl-defstruct-type-registry*)
           (*setf-compound-place-handlers* *repl-setf-compound-place-handlers*)
           (*labels-boxed-fns* nil)
          (*repl-global-variables* *repl-global-vars-persistent*)
          (*repl-capture-label-counter* t))
    (let ((forms (parse-source-for-language source :lisp)))
      (when (and (= (length forms) 1)
                 (consp (first forms))
                 (symbolp (caar forms))
                 (string= (symbol-name (caar forms)) "IN-PACKAGE"))
        (let ((pkg (find-package (second (first forms)))))
          (unless pkg
            (error "Unknown package: ~S" (second (first forms))))
          (setf *package* pkg)
          (return-from run-string-repl (second (first forms)))))
      (dolist (form forms)
        (%remember-host-global-definition form))
      (%run-form-repl-impl (compile-string source :target :vm)))))
