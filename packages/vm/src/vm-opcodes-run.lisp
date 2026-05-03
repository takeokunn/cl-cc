(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Flat-Vector Interpreter and State Access Layer
;;;
;;; Contains: %run-vm-core (fused inner loop), run-vm, run-vm-with-opcode-bigrams,
;;; and the canonical VM state access surface (make-vm-state, vm-reg-get/set, etc.).
;;;
;;; Depends on vm-opcodes-defs.lisp (all defopcode registrations, +op2-* constants,
;;; *opcode-dispatch-table*, *opcode-name-table*, vm2-fuse-immediate-superinstructions).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Flat-vector interpreter ───────────────────────────────────────────────

(defun %vm2-fast-op-const (state code pc regs)
  (declare (ignore state))
  (let ((dst (svref code (+ pc 1)))
        (imm (svref code (+ pc 2))))
    (setf (svref regs dst) imm)
    (+ pc 4)))

(defun %vm2-fast-op-move (state code pc regs)
  (declare (ignore state))
  (let ((dst (svref code (+ pc 1)))
        (src (svref code (+ pc 2))))
    (setf (svref regs dst) (svref regs src))
    (+ pc 4)))

(defun %vm2-fast-op-add-imm2 (state code pc regs)
  (declare (ignore state))
  (let ((dst (svref code (+ pc 1)))
        (src (svref code (+ pc 2)))
        (imm (svref code (+ pc 3))))
    (setf (svref regs dst) (+ (svref regs src) imm))
    (+ pc 4)))

(defun %vm2-fast-op-sub-imm2 (state code pc regs)
  (declare (ignore state))
  (let ((dst (svref code (+ pc 1)))
        (src (svref code (+ pc 2)))
        (imm (svref code (+ pc 3))))
    (setf (svref regs dst) (- (svref regs src) imm))
    (+ pc 4)))

(defun %vm2-fast-op-mul-imm2 (state code pc regs)
  (declare (ignore state))
  (let ((dst (svref code (+ pc 1)))
        (src (svref code (+ pc 2)))
        (imm (svref code (+ pc 3))))
    (setf (svref regs dst) (* (svref regs src) imm))
    (+ pc 4)))

(defun %vm2-fast-op-halt2 (state code pc regs)
  (declare (ignore state pc))
  (throw 'vm-halt (svref regs (svref code (+ pc 1)))))

(defun %vm2-fast-op-const-halt2 (state code pc regs)
  (declare (ignore state regs))
  (throw 'vm-halt (svref code (+ pc 1))))

(defparameter *vm2-fast-opcode-handler-symbols*
  '((+op2-const+      . %vm2-fast-op-const)
    (+op2-move+       . %vm2-fast-op-move)
    (+op2-add-imm2+   . %vm2-fast-op-add-imm2)
    (+op2-sub-imm2+   . %vm2-fast-op-sub-imm2)
    (+op2-mul-imm2+   . %vm2-fast-op-mul-imm2)
    (+op2-halt2+      . %vm2-fast-op-halt2)
    (+op2-const-halt2+ . %vm2-fast-op-const-halt2))
  "Opcode-constant symbol → fast handler function symbol for `%run-vm-core`.")

(defun %make-vm2-fast-opcode-handler-table ()
  "Build the numeric opcode → fast handler lookup table for `%run-vm-core`."
  (let ((table (make-hash-table :test #'eql)))
    (dolist (entry *vm2-fast-opcode-handler-symbols* table)
      (when (and (boundp (car entry))
                 (fboundp (cdr entry)))
        (setf (gethash (symbol-value (car entry)) table)
              (symbol-function (cdr entry)))))))

(defvar *vm2-fast-opcode-handler-table* nil
  "Numeric opcode → fast handler function for the hot-path VM2 interpreter.")

;;; ── Canonical VM state access surface ─────────────────────────────────────

(defun make-vm-state (&key (output-stream *standard-output*))
  "Create a vm-io-state as the canonical public VM execution state." 
  (make-instance 'vm-io-state :output-stream output-stream))

(defmethod vm-state-registers ((state vm2-state))
  (vm2-state-registers state))

(defmethod vm-output-stream ((state vm2-state))
  (vm2-state-output-stream state))

(defmethod vm-global-vars ((state vm2-state))
  (vm2-state-global-vars state))

(defgeneric vm-reg-get (state reg)
  (:documentation "Read register REG from STATE."))

(defgeneric vm-reg-set (state reg value)
  (:documentation "Write VALUE into register REG of STATE and return VALUE."))

(defmethod vm-reg-get ((state vm2-state) reg)
  (svref (vm2-state-registers state) reg))

(defmethod vm-reg-set ((state vm2-state) reg value)
  (setf (svref (vm2-state-registers state) reg) value)
  value)

(defmethod vm-reg-get ((state vm-state) reg)
  (gethash reg (slot-value state 'registers) 0))

(defmethod vm-reg-set ((state vm-state) reg value)
  (setf (gethash reg (slot-value state 'registers)) value)
  value)

(defun %run-vm-core (code state &key bigram-counts)
  "Shared VM2 interpreter core.

When BIGRAM-COUNTS is non-nil, counts executed opcode bigrams keyed by opcode
name symbol pairs."
  (let* ((code (vm2-fuse-immediate-superinstructions code))
         (regs (vm2-state-registers state))
         (len  (length code))
         (pc   0)
         (prev-op nil))
    (unless *vm2-fast-opcode-handler-table*
      (setf *vm2-fast-opcode-handler-table* (%make-vm2-fast-opcode-handler-table)))
    (catch 'vm-halt
      (loop while (< pc len)
            do (let ((op (svref code pc)))
                 (when (and bigram-counts prev-op)
                    (let ((name-a (aref *opcode-name-table* prev-op))
                          (name-b (aref *opcode-name-table* op)))
                      (when (and name-a name-b)
                        (incf (gethash (list name-a name-b) bigram-counts 0)))))
                  (setf prev-op op)
                  (let ((fast-handler (gethash op *vm2-fast-opcode-handler-table*)))
                    (setf pc
                          (if fast-handler
                              (funcall fast-handler state code pc regs)
                              (let ((handler (aref *opcode-dispatch-table* op)))
                                (funcall handler state code pc regs))))))
             finally (return nil)))))

(defun run-vm (code state)
  "Run bytecode CODE (a simple-vector) using STATE (a vm2-state struct).
Returns the value in the result register when halt2 executes."
  (declare (type simple-vector code)
           (type vm2-state state))
  (%run-vm-core code state))

(defun run-vm-with-opcode-bigrams (code state)
  "Run CODE and return two values: result and executed opcode bigram counts."
  (declare (type simple-vector code)
           (type vm2-state state))
  (let ((counts (make-hash-table :test #'equal)))
    (values (%run-vm-core code state :bigram-counts counts)
            counts)))
