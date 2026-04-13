(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Handler-Case Instructions, Label Table, and CLOS-based Interpreter
;;;
;;; Contains: vm-establish-handler / vm-remove-handler / vm-sync-handler-regs /
;;; vm-signal-error (defstructs + execute-instruction), vm-establish-catch /
;;; vm-throw, build-label-table, run-program-slice, run-compiled.
;;;
;;; The Phase-A flat bytecode engine (defopcode, vm2-state, run-vm) is in
;;; vm-opcodes.lisp, which loads after this file.
;;;
;;; Load order: after vm-clos.lisp; vm-opcodes.lisp loads after this.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Handler-Case VM Instructions ─────────────────────────────────────────

(define-vm-instruction vm-establish-handler (vm-instruction)
  "Push a handler entry onto the handler stack for handler-case."
  (handler-label nil :reader vm-handler-label)
  (result-reg nil :reader vm-handler-result-reg)
  (error-type nil :reader vm-error-type)
  (:sexp-tag :establish-handler))

(define-vm-instruction vm-remove-handler (vm-instruction)
  "Pop the top handler from the handler stack."
  (:sexp-tag :remove-handler))

(define-vm-instruction vm-sync-handler-regs (vm-instruction)
  "Update all handlers' saved-regs with current register state."
  (:sexp-tag :sync-handler-regs))

(define-vm-instruction vm-signal-error (vm-instruction)
  "Signal an error: walk the handler stack to find a matching handler."
  (error-reg nil :reader vm-error-reg)
  (:sexp-tag :signal-error))

(defun vm-handler-entry-saved-regs (entry)
  "Return the saved register snapshot stored in handler stack ENTRY."
  (if (and (consp entry) (eq (first entry) :catch))
      (sixth entry)
      (fifth entry)))

(defun (setf vm-handler-entry-saved-regs) (snapshot entry)
  "Replace the saved register snapshot stored in handler stack ENTRY."
  (if (and (consp entry) (eq (first entry) :catch))
      (setf (sixth entry) snapshot)
      (setf (fifth entry) snapshot)))

(defmethod execute-instruction ((inst vm-establish-handler) state pc labels)
  (declare (ignore labels))
  (let ((saved-regs (let ((copy (make-hash-table :test (hash-table-test (vm-state-registers state)))))
                      (maphash (lambda (k v) (setf (gethash k copy) v))
                               (vm-state-registers state))
                      copy)))
    (push (list (vm-handler-label inst)
                (vm-handler-result-reg inst)
                (vm-error-type inst)
                (vm-call-stack state)
                saved-regs
                (vm-method-call-stack state))
          (vm-handler-stack state)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-remove-handler) state pc labels)
  (declare (ignore labels))
  (when (vm-handler-stack state)
    (pop (vm-handler-stack state)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-sync-handler-regs) state pc labels)
  (declare (ignore labels))
  (let ((snapshot (let ((copy (make-hash-table :test (hash-table-test (vm-state-registers state)))))
                    (maphash (lambda (k v) (setf (gethash k copy) v))
                             (vm-state-registers state))
                    copy)))
    (dolist (entry (vm-handler-stack state))
      (setf (vm-handler-entry-saved-regs entry) snapshot)))
  (values (1+ pc) nil nil))

(defun vm-error-type-matches-p (error-value handler-type)
  "Check if ERROR-VALUE matches HANDLER-TYPE for handler-case dispatch.
String errors (from VM error instruction) match error/condition/t but not subtypes.
CL condition objects use typep."
  (cond
    ((member handler-type '(error condition serious-condition t)) t)
    ((typep error-value 'condition)
     (ignore-errors (typep error-value handler-type)))
    (t nil)))

(defun %vm-unwind-to-handler (state labels handler-label result-reg saved-call-stack
                               saved-regs saved-method-call-stack resume-value)
  "Restore VM state from a handler frame and jump to HANDLER-LABEL.
Used by both vm-signal-error and vm-throw."
  (setf (vm-call-stack state)        saved-call-stack)
  (setf (vm-method-call-stack state) (or saved-method-call-stack nil))
  (clrhash (vm-state-registers state))
  (maphash (lambda (k v) (setf (gethash k (vm-state-registers state)) v)) saved-regs)
  (vm-reg-set state result-reg resume-value)
  (values (vm-label-table-lookup labels handler-label) nil nil))

(defmethod execute-instruction ((inst vm-signal-error) state pc labels)
  (let ((error-value (vm-reg-get state (vm-error-reg inst)))
        (matching-handler nil)
        (handlers-to-skip 0))
    (dolist (entry (vm-handler-stack state))
      (if (vm-error-type-matches-p error-value (third entry))
          (progn (setf matching-handler entry) (return))
          (incf handlers-to-skip)))
    (if matching-handler
        (progn
          (dotimes (i (1+ handlers-to-skip)) (pop (vm-handler-stack state)))
          (destructuring-bind (handler-label result-reg _type saved-call-stack saved-regs
                               &optional saved-method-call-stack)
              matching-handler
            (declare (ignore _type))
            (%vm-unwind-to-handler state labels handler-label result-reg
                                   saved-call-stack saved-regs saved-method-call-stack
                                   error-value)))
        (error "Unhandled error in VM: ~S" error-value))))

;;; ── Catch/Throw VM Instructions ────────────────────────────────────────

(define-vm-instruction vm-establish-catch (vm-instruction)
  "Push a catch frame onto the handler stack for CATCH."
  (tag-reg nil :reader vm-catch-tag-reg)
  (handler-label nil :reader vm-catch-handler-label)
  (result-reg nil :reader vm-catch-result-reg)
  (:sexp-tag :establish-catch))

(define-vm-instruction vm-throw (vm-instruction)
  "Throw a value to a matching catch tag."
  (tag-reg nil :reader vm-throw-tag-reg)
  (value-reg nil :reader vm-throw-value-reg)
  (:sexp-tag :throw))

(defmethod execute-instruction ((inst vm-establish-catch) state pc labels)
  (declare (ignore labels))
  (let ((tag-value (vm-reg-get state (vm-catch-tag-reg inst)))
        (saved-regs (let ((copy (make-hash-table :test (hash-table-test (vm-state-registers state)))))
                      (maphash (lambda (k v) (setf (gethash k copy) v))
                               (vm-state-registers state))
                      copy)))
    (push (list :catch
                (vm-catch-handler-label inst)
                (vm-catch-result-reg inst)
                tag-value
                (vm-call-stack state)
                saved-regs
                (vm-method-call-stack state))
          (vm-handler-stack state)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-throw) state pc labels)
  (let ((tag-value   (vm-reg-get state (vm-throw-tag-reg   inst)))
        (throw-value (vm-reg-get state (vm-throw-value-reg inst)))
        (matching nil)
        (to-skip 0))
    (dolist (entry (vm-handler-stack state))
      (if (and (eq (first entry) :catch) (eql (fourth entry) tag-value))
          (progn (setf matching entry) (return))
          (incf to-skip)))
    (if matching
        (progn
          (dotimes (i (1+ to-skip)) (pop (vm-handler-stack state)))
          (%vm-unwind-to-handler state labels
                                 (second  matching)   ; handler-label
                                 (third   matching)   ; result-reg
                                 (fifth   matching)   ; saved-call-stack
                                 (sixth   matching)   ; saved-regs
                                 (seventh matching)   ; saved-method-stack
                                 throw-value))
        (error "No catch tag ~S is active" tag-value))))

;;; ── Label table and CLOS-based execution loop ────────────────────────────

(defun build-label-table (instructions)
  "Build an integer-keyed hash-table mapping label-name buckets → pc.

The outer hash table uses EQL on integer hash keys, while collisions are kept
in per-key buckets keyed by the original label object."
  (let ((labels (make-hash-table :test #'eql)))
    (loop for inst in instructions
          for pc from 0
          do (when (typep inst 'vm-label)
               (vm-label-table-store labels (vm-name inst) pc)))
    labels))

(defun run-program-slice (instructions labels start-pc state)
  "Execute INSTRUCTIONS (a vector) from START-PC using LABELS and STATE.
Returns the halted result value, or NIL if execution falls off the end.
Used by run-string-repl for incremental REPL execution."
  (loop with pc = start-pc
        while (< pc (length instructions))
        do (multiple-value-bind (next-pc halted result)
               (execute-instruction (aref instructions pc) state pc labels)
             (when halted
               (return result))
             (setf pc next-pc))
        finally (return nil)))

(defun run-compiled (program &key (output-stream *standard-output*) state)
  "Run a compiled VM program.
If STATE is provided, execute using that existing vm-io-state (for REPL persistence).
Otherwise a fresh state is created from OUTPUT-STREAM."
  (let* ((instructions (vm-program-instructions program))
         (labels (build-label-table instructions))
         (flat (coerce instructions 'vector))
         (state (or state (make-instance 'vm-io-state :output-stream output-stream))))
    (when (and (vm-profile-enabled-p state)
               (null (vm-profile-call-stack state)))
      (setf (vm-profile-call-stack state) (list "<toplevel>")))
    (let ((*vm-exec-flat* flat)
          (*vm-exec-labels* labels))
      (loop with pc = 0
            while (< pc (length flat))
            do (progn
                 (vm-profile-sample state)
                 (multiple-value-bind (next-pc halted result)
                    (execute-instruction (aref flat pc) state pc labels)
                   (when halted
                     (return result))
                   (setf pc next-pc)))
            finally (return nil)))))
