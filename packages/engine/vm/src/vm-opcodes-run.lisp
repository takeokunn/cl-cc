(in-package :cl-cc/vm)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Flat-Vector Interpreter and vm2-state Compatibility Shims
;;;
;;; Contains: %run-vm-core (fused inner loop), run-vm, run-vm-with-opcode-bigrams,
;;; and vm2-state compatibility shims (make-vm-state, vm-reg-get/set, etc.).
;;;
;;; Depends on vm-opcodes-defs.lisp (all defopcode registrations, +op2-* constants,
;;; *opcode-dispatch-table*, *opcode-name-table*, vm2-fuse-immediate-superinstructions).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Flat-vector interpreter ───────────────────────────────────────────────

(defun %run-vm-core (code state &key bigram-counts)
  "Shared VM2 interpreter core.

When BIGRAM-COUNTS is non-nil, counts executed opcode bigrams keyed by opcode
name symbol pairs."
  (let* ((code (vm2-fuse-immediate-superinstructions code))
         (regs (vm2-state-registers state))
         (len  (length code))
         (pc   0)
         (prev-op nil))
    (catch 'vm-halt
      (loop while (< pc len)
            do (let ((op (svref code pc)))
                 (when (and bigram-counts prev-op)
                   (let ((name-a (aref *opcode-name-table* prev-op))
                         (name-b (aref *opcode-name-table* op)))
                     (when (and name-a name-b)
                       (incf (gethash (list name-a name-b) bigram-counts 0)))))
                 (setf prev-op op)
                 (setf pc
                       (cond
                         ((= op +op2-const+)
                          (let ((dst (svref code (+ pc 1)))
                                (imm (svref code (+ pc 2))))
                            (setf (svref regs dst) imm)
                            (+ pc 4)))
                         ((= op +op2-move+)
                          (let ((dst (svref code (+ pc 1)))
                                (src (svref code (+ pc 2))))
                            (setf (svref regs dst) (svref regs src))
                            (+ pc 4)))
                         ((= op +op2-add-imm2+)
                          (let ((dst (svref code (+ pc 1)))
                                (src (svref code (+ pc 2)))
                                (imm (svref code (+ pc 3))))
                            (setf (svref regs dst) (+ (svref regs src) imm))
                            (+ pc 4)))
                         ((= op +op2-sub-imm2+)
                          (let ((dst (svref code (+ pc 1)))
                                (src (svref code (+ pc 2)))
                                (imm (svref code (+ pc 3))))
                            (setf (svref regs dst) (- (svref regs src) imm))
                            (+ pc 4)))
                         ((= op +op2-mul-imm2+)
                          (let ((dst (svref code (+ pc 1)))
                                (src (svref code (+ pc 2)))
                                (imm (svref code (+ pc 3))))
                            (setf (svref regs dst) (* (svref regs src) imm))
                            (+ pc 4)))
                         ((= op +op2-halt2+)
                          (throw 'vm-halt (svref regs (svref code (+ pc 1)))))
                         ((= op +op2-const-halt2+)
                          (throw 'vm-halt (svref code (+ pc 1))))
                         (t
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
