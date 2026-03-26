(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Handler-Case Instructions, Label Table, and Flat-Vector Interpreter
;;;
;;; Contains: vm-establish-handler / vm-remove-handler / vm-sync-handler-regs /
;;; vm-signal-error (defstructs + execute-instruction), build-label-table,
;;; run-program-slice, run-compiled, the Phase-A defopcode bytecode engine
;;; (vm2-state, defopcode, run-vm), and vm2-state compatibility shims.
;;;
;;; Load order: after vm-clos.lisp.
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

(defmethod execute-instruction ((inst vm-establish-handler) state pc labels)
  (declare (ignore labels))
  (let ((saved-regs (let ((copy (make-hash-table :test (hash-table-test (vm-state-registers state)))))
                      (maphash (lambda (k v) (setf (gethash k copy) v))
                               (vm-state-registers state))
                      copy)))
    (push (list (vm-handler-label inst)
                (vm-handler-result-reg inst)
                (vm-error-type inst)
                (copy-list (vm-call-stack state))
                saved-regs
                (copy-list (vm-method-call-stack state)))
          (vm-handler-stack state)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-remove-handler) state pc labels)
  (declare (ignore labels))
  (when (vm-handler-stack state)
    (pop (vm-handler-stack state)))
  (values (1+ pc) nil nil))

(defmethod execute-instruction ((inst vm-sync-handler-regs) state pc labels)
  (declare (ignore labels))
  (dolist (entry (vm-handler-stack state))
    (let ((saved-regs (fifth entry)))
      (maphash (lambda (k v) (setf (gethash k saved-regs) v))
               (vm-state-registers state))))
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

(defmethod execute-instruction ((inst vm-signal-error) state pc labels)
  (let ((error-value (vm-reg-get state (vm-error-reg inst))))
    (let ((matching-handler nil)
          (handlers-to-skip 0))
      (dolist (entry (vm-handler-stack state))
        (let ((error-type (third entry)))
          (if (vm-error-type-matches-p error-value error-type)
              (progn (setf matching-handler entry) (return))
              (incf handlers-to-skip))))
      (if matching-handler
          (progn
            (dotimes (i (1+ handlers-to-skip))
              (pop (vm-handler-stack state)))
            (destructuring-bind (handler-label result-reg error-type saved-call-stack saved-regs
                                 &optional saved-method-call-stack)
                matching-handler
              (declare (ignore error-type))
              (setf (vm-call-stack state) saved-call-stack)
              (setf (vm-method-call-stack state) (or saved-method-call-stack nil))
              (clrhash (vm-state-registers state))
              (maphash (lambda (k v) (setf (gethash k (vm-state-registers state)) v))
                       saved-regs)
              (vm-reg-set state result-reg error-value)
              (values (gethash handler-label labels) nil nil)))
          (error "Unhandled error in VM: ~S" error-value)))))

;;; ── Label table and CLOS-based execution loop ────────────────────────────

(defun build-label-table (instructions)
  "Build a hash-table mapping label-name → pc for fast jump resolution."
  (let ((labels (make-hash-table :test #'equal)))
    (loop for inst in instructions
          for pc from 0
          do (when (typep inst 'vm-label)
               (setf (gethash (vm-name inst) labels) pc)))
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
         (state (or state (make-instance 'vm-io-state :output-stream output-stream))))
    (loop with pc = 0
          while (< pc (length instructions))
          do (multiple-value-bind (next-pc halted result)
                 (execute-instruction (nth pc instructions) state pc labels)
               (when halted
                 (return result))
               (setf pc next-pc))
          finally (return nil))))

;;; ── Phase A: defopcode dispatch table + flat-vector interpreter ──────────

;;; Opcode dispatch tables (256-slot vectors)
(defparameter *opcode-dispatch-table*
  (make-array 256 :initial-element nil)
  "Vector mapping opcode integer → handler function (lambda (state code pc regs) ...).")

(defparameter *opcode-name-table*
  (make-array 256 :initial-element nil)
  "Vector mapping opcode integer → symbol name.")

(defparameter *opcode-encoder-table*
  (make-hash-table :test 'eq)
  "Hash table mapping opcode symbol name → opcode integer.")

;;; Opcode counter (auto-incremented by defopcode)
;;; eval-when ensures this is bound at compile time (needed by defconstant in defopcode)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *next-opcode* 0
    "Next available opcode integer; incremented by each defopcode form."))

(defmacro defopcode (name &body body)
  "Define a bytecode opcode NAME.
Assigns it the next available opcode number, registers it in all three
dispatch tables, and defines a constant +OP2-NAME+ for the opcode number.
BODY must be a single (lambda (state code pc regs) ...) returning next PC."
  (let ((op-sym (intern (format nil "+OP2-~A+" (symbol-name name))))
        (op-num *next-opcode*))
    (incf *next-opcode*)
    `(progn
       (defconstant ,op-sym ,op-num)
       (setf (aref *opcode-dispatch-table* ,op-sym) ,@body)
       (setf (aref *opcode-name-table*     ,op-sym) ',name)
       (setf (gethash ',name *opcode-encoder-table*) ,op-sym)
       ',name)))

;;; Register file size constant
(defconstant +vm-register-count+ 256
  "Number of integer-indexed registers in the vm2-state register file.")

;;; vm2-state: flat-vector interpreter state (distinct from vm-state CLOS class)
(defstruct (vm2-state
             (:constructor make-vm2-state
               (&key (output-stream *standard-output*)
                &aux (registers (make-array +vm-register-count+ :initial-element nil))
                     (global-vars (let ((ht (make-hash-table)))
                                    (setf (gethash '*features* ht) '(:cl-cc))
                                    (setf (gethash '*active-restarts* ht) nil)
                                    (setf (gethash '*standard-output* ht) *standard-output*)
                                    (setf (gethash '*standard-input* ht) *standard-input*)
                                    (setf (gethash '*error-output* ht) *error-output*)
                                    (setf (gethash '*trace-output* ht) *trace-output*)
                                    (setf (gethash '*debug-io* ht) *debug-io*)
                                    (setf (gethash '*query-io* ht) *query-io*)
                                    ht)))))
  "VM2 state for the flat-vector run-vm interpreter.
REGISTERS: simple-vector of 256 integer-indexed slots (fast svref access).
GLOBAL-VARS: hash table for global variable bindings.
OUTPUT-STREAM: stream for I/O."
  (registers    nil :type simple-vector)
  (global-vars  nil :type hash-table)
  (output-stream *standard-output*))

(defun vm2-reg-get (state reg)
  "Get register REG (integer) from vm2-state STATE."
  (svref (vm2-state-registers state) reg))

(defun vm2-reg-set (state reg value)
  "Set register REG (integer) to VALUE in vm2-state STATE. Returns VALUE."
  (setf (svref (vm2-state-registers state) reg) value)
  value)

;;; ── Opcode definitions ───────────────────────────────────────────────────

(defopcode const
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (imm (svref code (+ pc 2))))
      (setf (svref regs dst) imm))
    (+ pc 4)))

(defopcode move
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2))))
      (setf (svref regs dst) (svref regs src)))
    (+ pc 4)))

(defopcode add2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (+ (svref regs src1) (svref regs src2))))
    (+ pc 4)))

(defopcode sub2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (- (svref regs src1) (svref regs src2))))
    (+ pc 4)))

(defopcode mul2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst  (svref code (+ pc 1)))
          (src1 (svref code (+ pc 2)))
          (src2 (svref code (+ pc 3))))
      (setf (svref regs dst) (* (svref regs src1) (svref regs src2))))
    (+ pc 4)))

(defopcode halt2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((result-reg (svref code (+ pc 1))))
      (throw 'vm-halt (svref regs result-reg)))))

;;; ── Flat-vector interpreter ───────────────────────────────────────────────

(defun run-vm (code state)
  "Run bytecode CODE (a simple-vector) using STATE (a vm2-state struct).
Returns the value in the result register when halt2 executes."
  (declare (type simple-vector code)
           (type vm2-state state))
  (let ((regs (vm2-state-registers state))
        (len  (length code))
        (pc   0))
    (catch 'vm-halt
      (loop while (< pc len)
            do (let ((op (svref code pc)))
                 (let ((handler (aref *opcode-dispatch-table* op)))
                   (setf pc (funcall handler state code pc regs))))
            finally (return nil)))))

;;; ── vm2-state compatibility shims ────────────────────────────────────────
;;;
;;; These make vm2-state accessible via the same API as vm-state, allowing
;;; tests to use make-vm-state, vm-state-registers, vm-reg-get, vm-reg-set,
;;; vm-output-stream, and vm-global-vars uniformly.

(defun make-vm-state (&key (output-stream *standard-output*))
  "Create a vm2-state. Preferred constructor for new code; vm-io-state is
for the legacy CLOS execute-instruction pipeline."
  (make-vm2-state :output-stream output-stream))

(defmethod vm-state-registers ((s vm2-state))
  (vm2-state-registers s))

(defmethod vm-output-stream ((s vm2-state))
  (vm2-state-output-stream s))

(defmethod vm-global-vars ((s vm2-state))
  (vm2-state-global-vars s))

;;; vm-reg-get/vm-reg-set dispatch on vm2-state (svref) vs vm-state (gethash)
(defun vm-reg-get (state reg)
  (if (vm2-state-p state)
      (svref (vm2-state-registers state) reg)
      (gethash reg (slot-value state 'registers) 0)))

(defun vm-reg-set (state reg value)
  (if (vm2-state-p state)
      (setf (svref (vm2-state-registers state) reg) value)
      (setf (gethash reg (slot-value state 'registers)) value))
  value)
