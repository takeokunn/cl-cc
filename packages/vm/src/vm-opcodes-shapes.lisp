;;;; vm-opcodes-shapes.lisp — Opcode shape macros and vm2-state struct
;;;;
;;;; Load order: after vm-opcodes.lisp (defopcode macro must exist first).

(in-package :cl-cc/vm)

;;; ── Opcode shape macros ───────────────────────────────────────────────────
;;; These macros generate defopcode forms for the four common opcode shapes,
;;; eliminating ~40 near-identical lambda bodies while keeping all opcode
;;; numbering and table registration intact via the inner defopcode call.

(defmacro defopcode-unary-fn (name fn)
  "Opcode: dst ← fn(src)."
  (list 'defopcode name
        (list 'lambda '(state code pc regs)
              (list 'declare '(ignore state))
              (list 'let (list (list 'dst '(svref code (+ pc 1)))
                               (list 'src '(svref code (+ pc 2))))
                    (list 'setf '(svref regs dst) (list fn '(svref regs src))))
              '(+ pc 4))))

(defmacro defopcode-unary-type-pred (name type)
  "Opcode: dst ← (if (typep src 'type) 1 0).
Used for predicates that lack a simple CL function form (e.g. fixnump)."
  (list 'defopcode name
        (list 'lambda '(state code pc regs)
              (list 'declare '(ignore state))
              (list 'let (list (list 'dst '(svref code (+ pc 1)))
                               (list 'src '(svref code (+ pc 2))))
                    (list 'setf '(svref regs dst)
                          (list 'if (list 'typep '(svref regs src) (list 'quote type)) 1 0)))
              '(+ pc 4))))

(defmacro defopcode-unary-pred (name pred)
  "Opcode: dst ← (if pred(src) 1 0)."
  (list 'defopcode name
        (list 'lambda '(state code pc regs)
              (list 'declare '(ignore state))
              (list 'let (list (list 'dst '(svref code (+ pc 1)))
                               (list 'src '(svref code (+ pc 2))))
                    (list 'setf '(svref regs dst)
                          (list 'if (list pred '(svref regs src)) 1 0)))
              '(+ pc 4))))

(defmacro defopcode-load-literal (name value-form)
  "Opcode: dst ← VALUE-FORM.
VALUE-FORM may reference CODE/PC/REGS for immediate-bearing loads, or be a
literal constant for fixed-value loads."
  (list 'defopcode name
        (list 'lambda '(state code pc regs)
              (list 'declare '(ignore state))
              (list 'let (list (list 'dst '(svref code (+ pc 1))))
                    (list 'setf '(svref regs dst) value-form))
              '(+ pc 4))))

(defmacro defopcode-binary-fn (name fn)
  "Opcode: dst ← fn(src1, src2)."
  (list 'defopcode name
        (list 'lambda '(state code pc regs)
              (list 'declare '(ignore state))
              (list 'let (list (list 'dst '(svref code (+ pc 1)))
                               (list 'src1 '(svref code (+ pc 2)))
                               (list 'src2 '(svref code (+ pc 3))))
                    (list 'setf '(svref regs dst) (list fn '(svref regs src1) '(svref regs src2))))
              '(+ pc 4))))

(defmacro defopcode-binary-pred (name pred)
  "Opcode: dst ← (if pred(src1, src2) 1 0)."
  (list 'defopcode name
        (list 'lambda '(state code pc regs)
              (list 'declare '(ignore state))
              (list 'let (list (list 'dst '(svref code (+ pc 1)))
                               (list 'src1 '(svref code (+ pc 2)))
                               (list 'src2 '(svref code (+ pc 3))))
                    (list 'setf '(svref regs dst)
                          (list 'if (list pred '(svref regs src1) '(svref regs src2)) 1 0)))
              '(+ pc 4))))

(defmacro defopcode-binary-fn-imm (name fn)
  "Opcode: dst ← fn(src, immediate)."
  (list 'defopcode name
        (list 'lambda '(state code pc regs)
              (list 'declare '(ignore state))
              (list 'let (list (list 'dst '(svref code (+ pc 1)))
                               (list 'src '(svref code (+ pc 2)))
                               (list 'imm '(svref code (+ pc 3))))
                    (list 'setf '(svref regs dst) (list fn '(svref regs src) 'imm)))
              '(+ pc 4))))

(defmacro defopcode-binary-pred-imm (name pred)
  "Opcode: dst ← (if pred(src, immediate) 1 0)."
  (list 'defopcode name
        (list 'lambda '(state code pc regs)
              (list 'declare '(ignore state))
              (list 'let (list (list 'dst '(svref code (+ pc 1)))
                               (list 'src '(svref code (+ pc 2)))
                               (list 'imm '(svref code (+ pc 3))))
                    (list 'setf '(svref regs dst) (list 'if (list pred '(svref regs src) 'imm) 1 0)))
              '(+ pc 4))))

;;; ── vm2-state ──────────────────────────────────────────────────────────────

(defconstant +vm-register-count+ 256
  "Number of integer-indexed registers in the vm2-state register file.")

(defstruct (vm2-state
             (:constructor make-vm2-state
                (&key (output-stream *standard-output*)
                 &aux (registers (make-array +vm-register-count+ :initial-element nil))
                      (global-vars (let ((ht (make-hash-table)))
                                     (setf (gethash '*features* ht) '(:common-lisp :cl-cc))
                                     (setf (gethash '*active-restarts* ht) nil)
                                     (setf (gethash '*standard-output* ht) *standard-output*)
                                     (setf (gethash '*standard-input* ht) *standard-input*)
                                     (setf (gethash '*error-output* ht) *error-output*)
                                     (setf (gethash '*trace-output* ht) *trace-output*)
                                      (setf (gethash '*debug-io* ht) *debug-io*)
                                      (setf (gethash '*query-io* ht) *query-io*)
                                      ht))
                      (values-buffer nil)
                      (profile-enabled-p nil)
                      (profile-call-stack nil)
                      (profile-samples (make-hash-table :test #'equal)))))
  "VM2 state for the flat-vector run-vm interpreter.
REGISTERS: simple-vector of 256 integer-indexed slots (fast svref access).
GLOBAL-VARS: hash table for global variable bindings.
OUTPUT-STREAM: stream for I/O."
  (registers    nil :type simple-vector)
  (global-vars  nil :type hash-table)
  (values-buffer nil)
  (profile-enabled-p nil)
  (profile-call-stack nil)
  (profile-samples nil :type hash-table)
  (output-stream *standard-output*))

(defun vm2-reg-get (state reg)
  "Get register REG (integer) from vm2-state STATE."
  (svref (vm2-state-registers state) reg))

(defun vm2-reg-set (state reg value)
  "Set register REG (integer) to VALUE in vm2-state STATE. Returns VALUE."
  (setf (svref (vm2-state-registers state) reg) value)
  value)
