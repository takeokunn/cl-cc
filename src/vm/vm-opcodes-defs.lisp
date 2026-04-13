(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; VM — Opcode Definitions and Flat-Vector Interpreter
;;;
;;; Contains: all defopcode registrations (const, nop, load-const, move,
;;; arithmetic, comparison, jump, call, cons, vector, hash, I/O, etc.).
;;;
;;; %run-vm-core, run-vm, run-vm-with-opcode-bigrams, and vm2-state
;;; compatibility shims are in vm-opcodes-run.lisp.
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

;;; %run-vm-core, run-vm, run-vm-with-opcode-bigrams, and vm2-state compatibility
;;; shims are in vm-opcodes-run.lisp (loaded immediately after this file).
