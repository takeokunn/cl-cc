(in-package :cl-cc/vm)

;;; VM Condition System — Protocol Layer
;;;
;;; Provides condition classes, handler stack management, and restart bindings.
;;; VM instruction definitions, execute-instruction methods, and condition constructors
;;; are in conditions-instructions.lisp (loaded immediately after this file).

;;; ─── Standard/VM Condition Classes ───────────────────────────────────────────

;;; FR-204/252/255 Wasm EH mapping: VM condition classes remain the semantic
;;; source of truth; the Wasm backend lowers handler ranges to tag/try_table/
;;; exnref metadata and uses finalization hooks only when the corresponding
;;; experimental feature flag is explicitly enabled.

(defvar *active-restarts* nil
  "VM-visible list of active restart records.

This mirrors the self-hosted stdlib's restart registry while host-side VM code
continues to interoperate with Common Lisp's native restart protocol.")

(defparameter *vm-error-output-format* :text
  "Error output format.  Supported values are :TEXT and :JSON.")

(defun %source-location-file (location)
  (when location
    (or (ignore-errors (source-location-file location))
        (ignore-errors (source-location-pathname location))
        (ignore-errors (vm-source-location-pathname location))
        (getf location :file)
        (getf location :pathname))))

(defun %source-location-line (location)
  (when location
    (or (ignore-errors (source-location-line location))
        (ignore-errors (vm-source-location-line location))
        (getf location :line))))

(defun %source-location-column (location)
  (when location
    (or (ignore-errors (source-location-column location))
        (ignore-errors (vm-source-location-column location))
        (getf location :column))))

(defun %source-location-string (location)
  (when location
    (let ((file (%source-location-file location))
          (line (%source-location-line location))
          (column (%source-location-column location)))
      (when (or file line column)
        (format nil "~A~@[:~D~]~@[:~D~]"
                (or file "<unknown>") line column)))))

(defun %condition-location (condition)
  (or (ignore-errors (%vm-condition-source-location condition))
      (and (boundp '*current-source-location*) *current-source-location*)))

(defun %condition-source-text (condition)
  (ignore-errors (%vm-condition-source-text condition)))

(defun %read-source-lines (condition location)
  (let ((source (%condition-source-text condition))
        (file (%source-location-file location)))
    (cond
      ((stringp source)
       (loop with start = 0
             for pos = (position #\Newline source :start start)
             collect (subseq source start (or pos (length source)))
             do (setf start (and pos (1+ pos)))
             while start))
      ((and file (ignore-errors (probe-file file)))
       (with-open-file (in file :direction :input)
         (loop for line = (read-line in nil nil)
               while line collect line)))
      (t nil))))

(defun format-source-context (condition stream &key (context 3))
  "Print source context for CONDITION, if source text or file is available."
  (let* ((location (%condition-location condition))
         (line (%source-location-line location))
         (column (or (%source-location-column location) 1))
         (lines (and line (%read-source-lines condition location))))
    (when (and lines line)
      (let ((start (max 1 (- line context)))
            (end (min (length lines) (+ line context))))
        (loop for n from start to end
              for text = (nth (1- n) lines)
              do (format stream "~&~4D | ~A~%" n text)
                 (when (= n line)
                   (format stream "     | ~A^~%"
                           (make-string (max 0 (1- column)) :initial-element #\Space))))))))

(defun vm-levenshtein-distance (s1 s2)
  "Compute Levenshtein edit distance between two string designators."
  (let* ((a (string-downcase (string s1)))
         (b (string-downcase (string s2)))
         (n (length a))
         (m (length b))
         (d (make-array (list (1+ n) (1+ m)) :initial-element 0)))
    (dotimes (i (1+ n)) (setf (aref d i 0) i))
    (dotimes (j (1+ m)) (setf (aref d 0 j) j))
    (dotimes (i n)
      (dotimes (j m)
        (setf (aref d (1+ i) (1+ j))
              (min (1+ (aref d i (1+ j)))
                   (1+ (aref d (1+ i) j))
                   (+ (aref d i j)
                      (if (char= (char a i) (char b j)) 0 1))))))
    (aref d n m)))

(defun vm-did-you-mean (target candidates &key (max-distance 3) (max-results 5))
  "Return closest CANDIDATES to TARGET by edit distance."
  (let ((scored (loop for c in candidates
                      for distance = (vm-levenshtein-distance target c)
                      when (<= distance max-distance)
                        collect (cons c distance))))
    (subseq (mapcar #'car (stable-sort scored #'< :key #'cdr))
            0 (min max-results (length scored)))))

(defun %all-bound-variable-symbols (&optional vm-state)
  (let ((symbols nil))
    (do-all-symbols (sym)
      (when (boundp sym) (pushnew sym symbols :test #'eq)))
    (when vm-state
      (let ((globals (ignore-errors (vm-global-vars vm-state))))
        (when (hash-table-p globals)
          (maphash (lambda (sym value)
                     (declare (ignore value))
                     (pushnew sym symbols :test #'eq))
                   globals))))
    symbols))

(defun %all-function-symbols (&optional vm-state)
  (let ((symbols nil))
    (do-all-symbols (sym)
      (when (fboundp sym) (pushnew sym symbols :test #'eq)))
    (when vm-state
      (let ((functions (ignore-errors (vm-function-registry vm-state))))
        (when (hash-table-p functions)
          (maphash (lambda (sym value)
                     (declare (ignore value))
                     (pushnew sym symbols :test #'eq))
                   functions))))
    symbols))

(defun %condition-suggestions (condition)
  (or (ignore-errors (%vm-condition-suggestions condition))
      (let ((name (and (typep condition 'cell-error)
                       (ignore-errors (cell-error-name condition)))))
        (cond ((and name (typep condition 'undefined-function))
               (vm-did-you-mean name (%all-function-symbols (ignore-errors (vm-condition-state condition)))))
              ((and name (typep condition 'unbound-variable))
               (vm-did-you-mean name (%all-bound-variable-symbols (ignore-errors (vm-condition-state condition)))))
              (t nil)))))

(defun %json-escape-string (string)
  (with-output-to-string (out)
    (loop for ch across (princ-to-string string)
          do (case ch
               (#\\ (write-string "\\\\" out))
               (#\" (write-string "\\\"" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (otherwise (write-char ch out))))))

(defun format-condition-json (condition stream)
  "Write CONDITION as a compact JSON diagnostic object."
  (let ((location (%condition-location condition))
        (suggestions (%condition-suggestions condition)))
    (format stream "{\"type\":\"~A\",\"message\":\"~A\""
            (class-name (class-of condition))
            (%json-escape-string (%vm-condition-report-string condition)))
    (when location
      (format stream ",\"location\":{\"file\":\"~A\",\"line\":~D,\"column\":~D}"
              (%json-escape-string (or (%source-location-file location) ""))
              (or (%source-location-line location) 0)
              (or (%source-location-column location) 0)))
    (when suggestions
      (format stream ",\"suggestions\":[~{\"~A\"~^,~}]"
              (mapcar (lambda (s) (%json-escape-string (symbol-name s))) suggestions)))
    (write-string "}" stream)
    (values)))

(defun format-rich-condition (condition stream)
  "Write CONDITION with source location, context, and did-you-mean hints."
  (if (eq *vm-error-output-format* :json)
      (format-condition-json condition stream)
      (let ((location (%condition-location condition))
            (suggestions (%condition-suggestions condition)))
        (if location
            (format stream "error at ~A: ~A" (%source-location-string location)
                    (%vm-condition-report-string condition))
            (format stream "~A" (%vm-condition-report-string condition)))
        (when suggestions
          (format stream "~%Did you mean: ~{~S~^, ~}?" suggestions))
        (format-source-context condition stream)
        (values))))

(defun format-rich-condition-report (stream control condition &rest arguments)
  "Condition :REPORT helper that avoids recursive condition printing."
  (let ((location (%condition-location condition))
        (suggestions (%condition-suggestions condition)))
    (when location
      (format stream "error at ~A: " (%source-location-string location)))
    (apply #'format stream control arguments)
    (when suggestions
      (format stream "~%Did you mean: ~{~S~^, ~}?" suggestions))
    (format-source-context condition stream)
    (values)))

(defun format-stack-trace (condition stream &key (depth 10))
  "Format a VM stack trace attached to CONDITION's VM state."
  (let ((state (ignore-errors (vm-condition-state condition))))
    (format stream "~&Stack trace:~%")
    (cond ((and state (ignore-errors (vm-call-stack state)))
           (loop for frame in (vm-call-stack state)
                 for index from 0 below depth
                 do (destructuring-bind (return-pc dst-reg _old-env saved-regs &rest _more) frame
                      (declare (ignore _old-env saved-regs _more))
                      (format stream "  ~D: return-pc=~D dst=~A~%" index return-pc dst-reg))))
          (t (format stream "  <empty>~%")))
    (values)))

(define-condition vm-condition (condition)
  ((vm-state :initarg :vm-state :reader vm-condition-state
              :documentation "The VM state when condition was signaled.")
   (error-code :initarg :error-code :initform nil :reader vm-condition-error-code
               :documentation "Machine-readable diagnostic code for this VM condition.")
   (vm-fix-it :initarg :fix-it :initform nil :reader vm-condition-fix-it
              :documentation "Optional structured fix-it suggestion for this VM condition.")
   (source-location :initarg :source-location :initform nil :reader %vm-condition-source-location
                    :documentation "Optional source location for rich error reports.")
   (source-text :initarg :source-text :initform nil :reader %vm-condition-source-text
                :documentation "Optional source text used for context-line display.")
   (suggestions :initarg :suggestions :initform nil :reader %vm-condition-suggestions
                :documentation "Optional did-you-mean suggestions."))
  (:documentation "Base class for all VM conditions."))

(define-condition vm-serious-condition (vm-condition serious-condition)
  ()
  (:documentation "Base class for serious VM conditions."))

(define-condition vm-simple-condition (vm-condition simple-condition)
  ()
  (:documentation "VM condition carrying FORMAT-CONTROL and FORMAT-ARGUMENTS."))

(define-condition vm-error (vm-serious-condition error)
  ()
  (:documentation "Base class for VM errors. These are serious conditions that
typically require intervention to continue execution."))

(define-condition vm-warning (vm-condition warning)
  ()
  (:documentation "Base class for VM warnings. These indicate potential issues
but don't interrupt normal execution."))

(define-condition vm-simple-error (vm-error simple-error)
  ()
  (:report (lambda (condition stream)
             (apply #'format-rich-condition-report stream
                    (simple-condition-format-control condition)
                    condition
                    (simple-condition-format-arguments condition))))
  (:documentation "Simple VM error with FORMAT-CONTROL/FORMAT-ARGUMENTS."))

(define-condition vm-simple-warning (vm-warning simple-warning)
  ()
  (:report (lambda (condition stream)
             (apply #'format-rich-condition-report stream
                    (simple-condition-format-control condition)
                    condition
                    (simple-condition-format-arguments condition))))
  (:documentation "Simple VM warning with FORMAT-CONTROL/FORMAT-ARGUMENTS."))

(define-condition vm-type-error (vm-error type-error)
  ()
  (:documentation "Type mismatch error - raised when a value doesn't match
the expected type.  Inherits from CL's TYPE-ERROR so user code can catch it
via (handler-case ... (type-error (c) ...)).")
  (:report (lambda (condition stream)
             (format stream "VM Type Error: expected ~A, got ~S"
                     (type-error-expected-type condition)
                     (type-error-datum condition)))))

(define-condition vm-unbound-variable (vm-error unbound-variable)
  ()
  (:documentation "Error raised when accessing an undefined variable.
Inherits from CL's UNBOUND-VARIABLE so user code can catch it via
(handler-case ... (unbound-variable (c) ...)).")
  (:report (lambda (condition stream)
             (format-rich-condition-report stream "VM Unbound Variable: ~S" condition
                                           (cell-error-name condition)))))

(define-condition vm-undefined-function (vm-error undefined-function)
  ()
  (:documentation "Error raised when calling an undefined function.
Inherits from CL's UNDEFINED-FUNCTION so user code can catch it via
(handler-case ... (undefined-function (c) ...)).")
  (:report (lambda (condition stream)
             (format-rich-condition-report stream "VM Undefined Function: ~S" condition
                                           (cell-error-name condition)))))

(define-condition vm-arithmetic-error (vm-error arithmetic-error) ()
  (:documentation "Error signaled when an arithmetic operation fails."))

(define-condition vm-division-by-zero (vm-arithmetic-error division-by-zero)
  ((dividend :initarg :dividend :reader vm-dividend))
  (:report (lambda (c s) (format s "VM Division By Zero: attempted to divide ~S by zero" (vm-dividend c)))))

(define-condition vm-floating-point-overflow (vm-arithmetic-error floating-point-overflow) ()
  (:documentation "Error signaled when a floating-point operation overflows."))

(define-condition vm-floating-point-underflow (vm-arithmetic-error floating-point-underflow) ()
  (:documentation "Error signaled when a floating-point operation underflows."))

(define-condition vm-cell-error (vm-error cell-error) ()
  (:documentation "Error signaled when accessing an unbound cell."))

(define-condition vm-unbound-slot (vm-cell-error unbound-slot) ()
  (:documentation "Error signaled when accessing an unbound slot."))

(define-condition vm-control-error (vm-error control-error) ()
  (:documentation "Error signaled for invalid dynamic control transfer."))

(define-condition vm-program-error (vm-control-error program-error) ()
  (:documentation "Error signaled for malformed programs or invalid syntax."))

(define-condition vm-stream-error (vm-error stream-error) ()
  (:documentation "Error signaled for stream-related errors."))

(define-condition vm-end-of-file (vm-stream-error end-of-file) ()
  (:documentation "Error signaled when reading past end of file."))

(define-condition vm-reader-error (vm-stream-error reader-error) ()
  (:documentation "Error signaled when the reader encounters invalid input."))

(define-condition vm-package-error (vm-error package-error) ()
  (:documentation "Error signaled for package-related errors."))

(define-condition vm-storage-condition (vm-condition storage-condition) ()
  (:documentation "Error signaled when storage is exhausted."))

(define-condition vm-style-warning (vm-warning style-warning) ()
  (:documentation "Warning about style issues."))

(defun %vm-condition-printer-name (condition)
  "Return the summary class name used for escaped condition printing."
  (cond ((typep condition 'error) "ERROR")
        ((typep condition 'warning) "WARNING")
        ((typep condition 'serious-condition) "SERIOUS-CONDITION")
        (t "CONDITION")))

(defun %vm-condition-report-string (condition)
  "Return CONDITION's human-readable report without escaped object syntax."
  (let ((*print-escape* nil))
    (princ-to-string condition)))

(defmethod print-object ((condition vm-condition) stream)
  "Print VM conditions as #<ERROR: report> when escaped, otherwise report them.

The unescaped branch delegates to the host condition reporter, preserving
DEFINE-CONDITION :REPORT behavior for VM condition subclasses."
  (if *print-escape*
      (print-unreadable-object (condition stream)
        (format stream "~A: ~A"
                (%vm-condition-printer-name condition)
                (%vm-condition-report-string condition)))
      (call-next-method)))


;;; ─── VM Handler Stack ────────────────────────────────────────────────────────
;;;
;;; Since we cannot modify vm-state, we use a hash table to associate
;;; handler stacks with VM states. This is managed by the VM condition
;;; instructions.

(defvar *vm-handler-stacks* (make-hash-table :test #'eq :weakness :key)
  "Hash table mapping VM states to their handler stacks.
Uses weak keys so handlers are GC'd when the VM state is collected.")

(defvar *vm-restart-bindings* (make-hash-table :test #'eq :weakness :key)
  "Hash table mapping VM states to their restart bindings.
Uses weak keys for the same reason as *vm-handler-stacks*.")

;;; Structure representing a condition handler in the VM.
;;; TYPE - Condition type this handler matches.
;;; HANDLER-FN - Function to call when condition is signaled.
(defstruct (vm-handler (:constructor make-vm-handler (type handler-fn)))
  type
  handler-fn)

;;; Structure representing a restart in the VM.
;;; NAME - Name of the restart.
;;; RESTART-FN - Function to invoke the restart.
(defstruct (vm-restart (:constructor make-vm-restart (name restart-fn)))
  name
  restart-fn
  (description nil)
  (interactive-function nil))

(defun describe-restart (restart)
  "Return a human-readable description for RESTART."
  (cond ((typep restart 'vm-restart)
         (or (vm-restart-description restart)
             (format nil "Invoke restart ~S" (vm-restart-name restart))))
        ((ignore-errors (restart-name restart))
         (with-output-to-string (out)
           (ignore-errors (princ restart out))))
        (t (format nil "Invoke restart ~S" restart))))

(defun restart-interactive (restart)
  "Return RESTART's interactive argument collection function, if any."
  (cond ((typep restart 'vm-restart) (vm-restart-interactive-function restart))
        ((ignore-errors (restart-name restart))
         (ignore-errors (restart-interactive-function restart)))
        (t nil)))

(defun vm-compute-active-restarts (&optional condition vm-state)
  "Return active VM and host restarts for CONDITION."
  (append *active-restarts*
          (and vm-state (vm-get-restarts vm-state))
          (cl:compute-restarts condition)))

(defun vm-invoke-restart-interactively (restart)
  "Collect interactive arguments for RESTART, then invoke it."
  (let ((interactive (restart-interactive restart)))
    (cond ((typep restart 'vm-restart)
           (apply (vm-restart-restart-fn restart)
                  (if interactive (funcall interactive) nil)))
          (interactive
           (apply #'cl:invoke-restart restart (funcall interactive)))
          (t (cl:invoke-restart-interactively restart)))))

(defun vm-show-restart-menu (condition &optional (stream *query-io*))
  "Display a numbered restart menu for CONDITION and return selected restart."
  (let ((restarts (vm-compute-active-restarts condition)))
    (format stream "~&Debugger entered on ~A~%" condition)
    (loop for restart in restarts
          for index from 0
          do (format stream "  ~D: [~A] ~A~%" index
                     (if (typep restart 'vm-restart)
                         (vm-restart-name restart)
                         (restart-name restart))
                     (describe-restart restart)))
    (when restarts
      (format stream "Select restart number: ")
      (finish-output stream)
      (let ((choice (ignore-errors (read stream nil nil))))
        (when (and (integerp choice) (<= 0 choice) (< choice (length restarts)))
          (nth choice restarts))))))

(defmacro vm-with-simple-restart ((name format-string &rest args) &body body)
  "VM-prefixed wrapper for CL:WITH-SIMPLE-RESTART."
  `(cl:with-simple-restart (,name ,format-string ,@args) ,@body))

(defun vm-get-handler-stack (vm-state)
  "Get the handler stack for VM-STATE, creating one if necessary."
  (or (gethash vm-state *vm-handler-stacks*)
      (setf (gethash vm-state *vm-handler-stacks*) nil)))

(defun vm-push-handler-to-stack (vm-state type handler-fn)
  "Push a new handler onto the handler stack for VM-STATE."
  (push (make-vm-handler type handler-fn)
        (gethash vm-state *vm-handler-stacks*)))

(defun vm-pop-handler-from-stack (vm-state)
  "Pop the top handler from the handler stack for VM-STATE.
Returns the popped handler or NIL if stack is empty."
  (let ((stack (vm-get-handler-stack vm-state)))
    (when stack
      (let ((handler (pop stack)))
        (setf (gethash vm-state *vm-handler-stacks*) stack)
        handler))))

(defun vm-find-handler (vm-state condition)
  "Find a handler for CONDITION in VM-STATE's handler stack.
Returns the first matching handler or NIL if none found."
  (let ((stack (vm-get-handler-stack vm-state)))
    (find-if (lambda (handler)
               (typep condition (vm-handler-type handler)))
             stack)))

(defun vm-get-restarts (vm-state)
  "Get the available restarts for VM-STATE."
  (gethash vm-state *vm-restart-bindings*))

(defun vm-add-restart (vm-state name restart-fn)
  "Add a restart binding for VM-STATE."
  (push (make-vm-restart name restart-fn)
        (gethash vm-state *vm-restart-bindings*)))

(defun vm-find-restart (vm-state name)
  "Find a restart by name in VM-STATE's restart bindings."
  (let ((restarts (vm-get-restarts vm-state)))
    (find name restarts :key #'vm-restart-name :test #'eq)))

(defun vm-clear-condition-context (vm-state)
  "Clear handler stack and restarts for VM-STATE (cleanup function)."
  (remhash vm-state *vm-handler-stacks*)
  (remhash vm-state *vm-restart-bindings*))

;;; ─── Zero-Cost Exception Tables ─────────────────────────────────────────────
;;;
;;; FR-138 stores handler metadata outside the hot instruction stream.  The
;;; compiler registers a vector of PC ranges for each VM program; label-table
;;; construction then associates the freshly built label table with that vector
;;; so condition signaling can find a handler from the current PC without
;;; executing establish/remove instructions on protected-form entry/exit.

(defstruct (vm-exception-entry
            (:constructor make-vm-exception-entry
                (start-pc end-pc handler-pc condition-type result-reg order)))
  start-pc
  end-pc
  handler-pc
  condition-type
  result-reg
  order)

(defvar *vm-program-exception-tables* (make-hash-table :test #'eq :weakness :key)
  "Weak map from VM program objects to their zero-cost exception table.")

(defvar *vm-instruction-exception-tables* (make-hash-table :test #'eq :weakness :key)
  "Weak map from VM program instruction lists to their exception table.")

(defvar *vm-label-exception-tables* (make-hash-table :test #'eq :weakness :key)
  "Weak map from per-run label tables to their exception table.")

(defun vm-register-program-exception-table (program exception-table)
  "Attach EXCEPTION-TABLE to PROGRAM and its instruction list.

EXCEPTION-TABLE is a vector of VM-EXCEPTION-ENTRY records with concrete PCs.
An empty table is ignored so programs without handlers keep the old shape."
  (let ((table (and exception-table
                    (if (vectorp exception-table)
                        exception-table
                        (coerce exception-table 'vector)))))
    (when (and program table (plusp (length table)))
      (setf (gethash program *vm-program-exception-tables*) table)
      (setf (gethash (vm-program-instructions program) *vm-instruction-exception-tables*) table))
    program))

(defun %vm-exception-table-for-labels (labels)
  "Return the exception table associated with LABELS, if any."
  (and labels (gethash labels *vm-label-exception-tables*)))

(defun %vm-exception-entry-active-p (entry pc condition)
  "Return true when ENTRY covers PC and accepts CONDITION."
  (and (<= (vm-exception-entry-start-pc entry) pc)
       (< pc (vm-exception-entry-end-pc entry))
       (vm-error-type-matches-p condition (vm-exception-entry-condition-type entry))))

(defun %vm-exception-entry-span (entry)
  (- (vm-exception-entry-end-pc entry)
     (vm-exception-entry-start-pc entry)))

(defun vm-find-exception-table-entry (labels pc condition)
  "Find the innermost exception-table handler for CONDITION at PC.

For identical ranges, preserve source clause order.  For nested protected
forms, prefer the smallest covering range."
  (let ((best nil))
    (let ((table (%vm-exception-table-for-labels labels)))
      (when table
        (loop for entry across table
              when (%vm-exception-entry-active-p entry pc condition)
                do (when (or (null best)
                             (< (%vm-exception-entry-span entry)
                                (%vm-exception-entry-span best))
                             (and (= (%vm-exception-entry-span entry)
                                     (%vm-exception-entry-span best))
                                  (< (vm-exception-entry-order entry)
                                     (vm-exception-entry-order best))))
                     (setf best entry)))))
    best))

(defun %vm-jump-to-exception-entry (state entry condition)
  "Store CONDITION for ENTRY's handler and jump to its handler PC."
  (vm-reg-set state (vm-exception-entry-result-reg entry) condition)
  (values (vm-exception-entry-handler-pc entry) nil nil))

(defun %vm-restore-frame-for-exception-propagation (state frame)
  "Restore one caller frame while propagating an exception-table lookup."
  (destructuring-bind (return-pc _dst-reg old-closure-env saved-regs) frame
    (declare (ignore _dst-reg))
    (vm-restore-registers state saved-regs)
    (when old-closure-env
      (setf (vm-closure-env state) old-closure-env))
    (when (vm-method-call-stack state)
      (pop (vm-method-call-stack state)))
    return-pc))

(defun vm-dispatch-exception-table (state labels pc condition &key error-p)
  "Resolve CONDITION via the PC→handler side table, unwinding callers as needed.

Returns normal EXECUTE-INSTRUCTION values when a handler is found.  If no entry
matches in the current frame, restore one call frame and retry at its return PC;
this models dynamic propagation without a runtime handler stack."
  (if (%vm-exception-table-for-labels labels)
      (loop with probe-pc = pc
            do (let ((entry (vm-find-exception-table-entry labels probe-pc condition)))
                 (when entry
                   (return (%vm-jump-to-exception-entry state entry condition)))
                 (if (vm-call-stack state)
                     (setf probe-pc
                           (%vm-restore-frame-for-exception-propagation
                           state (vm-pop-call-frame state)))
                     (progn
                       (when error-p
                         (vm-print-backtrace state :labels labels)
                         (if (typep condition 'condition)
                             (error condition)
                             (error "Unhandled error in VM: ~S" condition)))
                       (return (values nil nil nil))))))
      (values nil nil nil)))

(defun build-label-table (instructions)
  "Build an integer-keyed label table and associate any exception side table."
  (let ((labels (make-hash-table :test #'eql)))
    (loop for inst in instructions
          for pc from 0
          do (when (typep inst 'vm-label)
               (vm-label-table-store labels (vm-name inst) pc)))
    (let ((exception-table (gethash instructions *vm-instruction-exception-tables*)))
      (when exception-table
        (setf (gethash labels *vm-label-exception-tables*) exception-table)))
    labels))

(defmethod execute-instruction ((inst vm-signal-error) state pc labels)
  "Signal an error using the zero-cost exception table before legacy stacks."
  (let ((error-value (vm-reg-get state (vm-error-reg inst))))
    (multiple-value-bind (next-pc handled-p value)
        (vm-dispatch-exception-table state labels pc error-value :error-p nil)
      (declare (ignore value))
      (if next-pc
          (values next-pc handled-p nil)
          (let ((matching-handler nil)
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
                (progn
                  (vm-print-backtrace state :labels labels)
                  (if (typep error-value 'condition)
                      (error error-value)
                      (error "Unhandled error in VM: ~S" error-value)))))))))
