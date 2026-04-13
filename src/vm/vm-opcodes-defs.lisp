(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Opcode Definitions and Flat-Vector Interpreter
;;;
;;; Contains: all defopcode registrations (const, nop, load-const, move,
;;; arithmetic, comparison, jump, call, cons, vector, hash, I/O, etc.),
;;; %run-vm-core (fused inner loop), run-vm, run-vm-with-opcode-bigrams,
;;; and vm2-state compatibility shims (make-vm-state, vm-reg-get/set, etc.).
;;;
;;; Load order: after vm-opcodes.lisp (infrastructure + shape macros).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── Opcode definitions ───────────────────────────────────────────────────

(defopcode const
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (imm (svref code (+ pc 2))))
      (setf (svref regs dst) imm))
    (+ pc 4)))

(defopcode nop
  (lambda (state code pc regs)
    (declare (ignore state code regs))
    (+ pc 4)))

(defopcode load-const
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (literal (svref code (+ pc 2))))
      (setf (svref regs dst) literal))
    (+ pc 4)))

(defopcode load-nil
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1))))
      (setf (svref regs dst) nil))
    (+ pc 4)))

(defopcode load-true
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1))))
      (setf (svref regs dst) t))
    (+ pc 4)))

(defopcode load-fixnum
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (imm (svref code (+ pc 2))))
      (setf (svref regs dst) imm))
    (+ pc 4)))

(defopcode const-halt2
  (lambda (state code pc regs)
    (declare (ignore state regs))
    (throw 'vm-halt (svref code (+ pc 1)))))

(defopcode move
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (src (svref code (+ pc 2))))
      (setf (svref regs dst) (svref regs src)))
    (+ pc 4)))

;;; Unary function opcodes: dst ← fn(src)
(defopcode-unary-fn neg -)
(defopcode-unary-fn inc 1+)
(defopcode-unary-fn dec 1-)

;;; Unary predicate opcodes: dst ← (if pred(src) 1 0)
(defopcode-unary-type-pred fixnump fixnum)   ; no CL fixnump function, use typep
(defopcode-unary-pred consp     consp)
(defopcode-unary-pred symbolp   symbolp)
(defopcode-unary-pred functionp functionp)
(defopcode-unary-pred stringp   stringp)

(defopcode cons
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (car-reg (svref code (+ pc 2)))
          (cdr-reg (svref code (+ pc 3))))
      (setf (svref regs dst) (cons (svref regs car-reg) (svref regs cdr-reg))))
    (+ pc 4)))

(defopcode-unary-fn car car)
(defopcode-unary-fn cdr cdr)

(defopcode make-vector
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (size-reg (svref code (+ pc 2)))
          (init-reg (svref code (+ pc 3))))
      (setf (svref regs dst)
            (make-array (svref regs size-reg) :initial-element (svref regs init-reg))))
    (+ pc 4)))

(defopcode vector-ref
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (vec-reg (svref code (+ pc 2)))
          (idx-reg (svref code (+ pc 3))))
      (setf (svref regs dst)
            (aref (svref regs vec-reg) (svref regs idx-reg))))
    (+ pc 4)))

(defopcode vector-set
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((vec-reg (svref code (+ pc 1)))
          (idx-reg (svref code (+ pc 2)))
          (src-reg (svref code (+ pc 3))))
      (setf (aref (svref regs vec-reg) (svref regs idx-reg))
            (svref regs src-reg)))
    (+ pc 4)))

(defopcode make-hash
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (size-reg (svref code (+ pc 2))))
      (setf (svref regs dst)
            (make-hash-table :test #'equal :size (max 1 (svref regs size-reg)))))
    (+ pc 4)))

(defopcode hash-ref
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((dst (svref code (+ pc 1)))
          (ht-reg (svref code (+ pc 2)))
          (key-reg (svref code (+ pc 3))))
      (setf (svref regs dst)
            (gethash (svref regs key-reg) (svref regs ht-reg))))
    (+ pc 4)))

(defopcode hash-set
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((ht-reg (svref code (+ pc 1)))
          (key-reg (svref code (+ pc 2)))
          (src-reg (svref code (+ pc 3))))
      (setf (gethash (svref regs key-reg) (svref regs ht-reg))
            (svref regs src-reg)))
    (+ pc 4)))

(defopcode get-global
  (lambda (state code pc regs)
    (let ((dst (svref code (+ pc 1)))
          (name (svref code (+ pc 2))))
      (setf (svref regs dst)
            (gethash name (vm2-state-global-vars state)))
      (+ pc 4))))

(defopcode set-global
  (lambda (state code pc regs)
    (let ((name (svref code (+ pc 1)))
          (src (svref code (+ pc 2))))
      (setf (gethash name (vm2-state-global-vars state))
            (svref regs src))
      (+ pc 4))))

(defopcode make-instance
  (lambda (state code pc regs)
    (declare (ignore state))
    (let* ((dst (svref code (+ pc 1)))
           (class-designator (svref code (+ pc 2)))
           (nargs (svref code (+ pc 3)))
           (initargs (loop for i below nargs collect (svref regs i))))
      (setf (svref regs dst) (apply #'make-instance class-designator initargs))
      (+ pc 4))))

;;; Binary equality predicate opcodes: dst ← (if pred(src1, src2) 1 0)
(defopcode-binary-pred eq    eq)
(defopcode-binary-pred eql   eql)
(defopcode-binary-pred equal equal)

;;; Binary arithmetic function opcodes: dst ← fn(src1, src2)
(defopcode-binary-fn add2 +)
(defopcode-binary-fn sub2 -)
(defopcode-binary-fn mul2 *)
(defopcode-binary-fn div  /)
(defopcode-binary-fn mod  mod)

;;; Binary arithmetic immediate opcodes: dst ← fn(src, imm)
(defopcode-binary-fn-imm add-imm2 +)
(defopcode-binary-fn-imm sub-imm2 -)
(defopcode-binary-fn-imm mul-imm2 *)

;;; Numeric comparison register opcodes: dst ← (if pred(src1, src2) 1 0)
(defopcode-binary-pred num-eq2 =)
(defopcode-binary-pred num-lt2 <)
(defopcode-binary-pred num-gt2 >)
(defopcode-binary-pred num-le2 <=)
(defopcode-binary-pred num-ge2 >=)

;;; Numeric comparison immediate opcodes: dst ← (if pred(src, imm) 1 0)
(defopcode-binary-pred-imm num-eq-imm2 =)
(defopcode-binary-pred-imm num-lt-imm2 <)
(defopcode-binary-pred-imm num-gt-imm2 >)
(defopcode-binary-pred-imm num-le-imm2 <=)
(defopcode-binary-pred-imm num-ge-imm2 >=)

(defopcode jump
  (lambda (state code pc regs)
    (declare (ignore state regs))
    (+ pc (svref code (+ pc 1)))))

(defopcode jump-if-nil
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((src (svref code (+ pc 1)))
          (offset (svref code (+ pc 2))))
      (if (null (svref regs src))
          (+ pc offset)
          (+ pc 4)))))

(defopcode jump-if-true
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((src (svref code (+ pc 1)))
          (offset (svref code (+ pc 2))))
      (if (svref regs src)
          (+ pc offset)
          (+ pc 4)))))

(defopcode values
  (lambda (state code pc regs)
    (let ((nvals (svref code (+ pc 1))))
      (setf (vm2-state-values-buffer state)
            (loop for i below nvals collect (svref regs i)))
      (+ pc 4))))

(defopcode recv-values
  (lambda (state code pc regs)
    (let* ((nvals (svref code (+ pc 1)))
           (vals (vm2-state-values-buffer state)))
      (loop for i below nvals
            for value in vals
            do (setf (svref regs i) value)
            finally (when (< (length vals) nvals)
                      (loop for j from (length vals) below nvals
                            do (setf (svref regs j) nil))))
      (+ pc 4))))

(defopcode return
  (lambda (state code pc regs)
    (declare (ignore state pc))
    (throw 'vm-halt (svref regs (svref code (+ pc 1))))))

(defopcode return-nil
  (lambda (state code pc regs)
    (declare (ignore state code pc regs))
    (throw 'vm-halt nil)))

(defopcode halt2
  (lambda (state code pc regs)
    (declare (ignore state))
    (let ((result-reg (svref code (+ pc 1))))
      (throw 'vm-halt (svref regs result-reg)))))

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
