;;;; packages/codegen/src/post-ra-scheduler.lisp — Post-register-allocation scheduling

(in-package :cl-cc/codegen)

;;; FR-068: Post-RA Instruction Scheduling
;;;
;;; This pass deliberately lives in the codegen package, not the optimizer
;;; package, because it consumes regalloc-result objects.  It mirrors the local
;;; pre-RA scheduler shape but resolves every VM register operand through the
;;; allocation result first and builds the DAG from physical-register hazards.

(defparameter *post-ra-x86-64-aliases*
  '((:eax . :rax) (:ax . :rax) (:al . :rax) (:ah . :rax)
    (:ecx . :rcx) (:cx . :rcx) (:cl . :rcx) (:ch . :rcx)
    (:edx . :rdx) (:dx . :rdx) (:dl . :rdx) (:dh . :rdx)
    (:ebx . :rbx) (:bx . :rbx) (:bl . :rbx) (:bh . :rbx)
    (:esp . :rsp) (:sp . :rsp) (:spl . :rsp)
    (:ebp . :rbp) (:bp . :rbp) (:bpl . :rbp)
    (:esi . :rsi) (:si . :rsi) (:sil . :rsi)
    (:edi . :rdi) (:di . :rdi) (:dil . :rdi)
    (:r8d . :r8) (:r8w . :r8) (:r8b . :r8)
    (:r9d . :r9) (:r9w . :r9) (:r9b . :r9)
    (:r10d . :r10) (:r10w . :r10) (:r10b . :r10)
    (:r11d . :r11) (:r11w . :r11) (:r11b . :r11)
    (:r12d . :r12) (:r12w . :r12) (:r12b . :r12)
    (:r13d . :r13) (:r13w . :r13) (:r13b . :r13)
    (:r14d . :r14) (:r14w . :r14) (:r14b . :r14)
    (:r15d . :r15) (:r15w . :r15) (:r15b . :r15)
    (:ymm0 . :xmm0) (:ymm1 . :xmm1) (:ymm2 . :xmm2) (:ymm3 . :xmm3)
    (:ymm4 . :xmm4) (:ymm5 . :xmm5) (:ymm6 . :xmm6) (:ymm7 . :xmm7)
    (:ymm8 . :xmm8) (:ymm9 . :xmm9) (:ymm10 . :xmm10) (:ymm11 . :xmm11)
    (:ymm12 . :xmm12) (:ymm13 . :xmm13) (:ymm14 . :xmm14) (:ymm15 . :xmm15))
  "Physical register aliases canonicalized before post-RA dependency checks.")

(defparameter *post-ra-implicit-registers*
  '(:rsp :rbp :sp :fp :x29 :x30 :ra :s0)
  "Implicit frame/stack/link registers treated conservatively by post-RA scheduling.")

(defun %post-ra-canonical-phys-reg (reg)
  "Return REG canonicalized for physical alias checks."
  (or (cdr (assoc reg *post-ra-x86-64-aliases* :test #'eq)) reg))

(defun %post-ra-physical-reg-p (reg ra)
  "Return true when REG is already a physical register in RA's assignment range."
  (or (member reg *post-ra-implicit-registers* :test #'eq)
      (loop for phys being the hash-values of (regalloc-assignment ra)
            thereis (eq reg phys))
      (assoc reg *post-ra-x86-64-aliases* :test #'eq)))

(defun %post-ra-resolve-reg (reg ra)
  "Resolve virtual REG to a canonical physical register using RA.

If REG is already physical (for example spill helper scratch registers), keep it.
Return two values: the resolved register and a boolean indicating whether the
dependency is known well enough for reordering."
  (cond
    ((null reg) (values nil t))
    ((regalloc-lookup ra reg)
     (values (%post-ra-canonical-phys-reg (regalloc-lookup ra reg)) t))
    ((%post-ra-physical-reg-p reg ra)
     (values (%post-ra-canonical-phys-reg reg) t))
    (t (values nil nil))))

(defun %post-ra-resolve-regs (regs ra)
  "Resolve REGS to physical registers.  Returns values (resolved known-p)."
  (let ((result nil)
        (known-p t))
    (dolist (reg regs (values (nreverse result) known-p))
      (multiple-value-bind (phys ok-p) (%post-ra-resolve-reg reg ra)
        (unless ok-p (setf known-p nil))
        (when phys (pushnew phys result :test #'eq))))))

(defun %post-ra-inst-read-regs (inst ra)
  "Return physical registers read by INST and whether all reads are known."
  (%post-ra-resolve-regs (remove nil (or (opt-inst-read-regs inst)
                                         (instruction-uses inst)))
                         ra))

(defun %post-ra-inst-write-regs (inst ra)
  "Return physical registers written by INST and whether all writes are known."
  (%post-ra-resolve-regs (remove nil (or (let ((dst (opt-inst-dst inst)))
                                          (and dst (list dst)))
                                        (instruction-defs inst)))
                         ra))

(defun %post-ra-stack-memory-inst-p (inst)
  "Return true for spill helpers that implicitly access the current stack frame."
  (member (type-of inst) '(vm-spill-store vm-spill-load) :test #'eq))

(defun %post-ra-fixed-type-p (inst)
  "Return true for exact VM instruction types that are fixed barriers."
  (member (type-of inst)
          '(vm-label vm-jump vm-jump-zero vm-ret vm-halt
            vm-call vm-tail-call vm-apply vm-generic-call
            vm-slot-write vm-set-global vm-signal vm-signal-error
            cl-cc/vm::vm-error-instruction cl-cc/vm::vm-cerror cl-cc/vm::vm-warn)
          :test #'eq))

(defun %post-ra-scheduler-barrier-p (inst ra)
  "T when INST must retain its exact position in post-RA local scheduling."
  (multiple-value-bind (reads reads-known-p) (%post-ra-inst-read-regs inst ra)
    (declare (ignore reads))
    (multiple-value-bind (writes writes-known-p) (%post-ra-inst-write-regs inst ra)
      (declare (ignore writes))
      (or (not reads-known-p)
          (not writes-known-p)
          (%post-ra-stack-memory-inst-p inst)
          (%post-ra-fixed-type-p inst)
          (not (member (vm-inst-effect-kind inst) '(:pure :read-only) :test #'eq))))))

(defun %post-ra-reg-intersect-p (a b)
  "T when physical register lists A and B intersect by EQ."
  (and a b (intersection a b :test #'eq)))

(defun %post-ra-scheduler-depends-p (earlier later ra)
  "T when EARLIER must precede LATER due to physical RAW, WAR, or WAW hazards."
  (multiple-value-bind (earlier-reads earlier-reads-known-p)
      (%post-ra-inst-read-regs earlier ra)
    (multiple-value-bind (earlier-writes earlier-writes-known-p)
        (%post-ra-inst-write-regs earlier ra)
      (multiple-value-bind (later-reads later-reads-known-p)
          (%post-ra-inst-read-regs later ra)
        (multiple-value-bind (later-writes later-writes-known-p)
            (%post-ra-inst-write-regs later ra)
          (if (and earlier-reads-known-p earlier-writes-known-p
                   later-reads-known-p later-writes-known-p)
              (or (%post-ra-reg-intersect-p earlier-writes later-reads)   ; RAW
                  (%post-ra-reg-intersect-p earlier-reads later-writes)   ; WAR
                  (%post-ra-reg-intersect-p earlier-writes later-writes)) ; WAW
              t))))))

(defun %post-ra-build-scheduler-graph (insts ra)
  "Build physical-register dependency predecessor/successor vectors for INSTS."
  (let* ((n (length insts))
         (vec (coerce insts 'vector))
         (preds (make-array n :initial-element nil))
         (succs (make-array n :initial-element nil)))
    (loop for i from 0 below n do
      (loop for j from (1+ i) below n
            when (%post-ra-scheduler-depends-p (aref vec i) (aref vec j) ra)
              do (pushnew i (aref preds j))
                 (pushnew j (aref succs i))))
    (values preds succs)))

(defun %post-ra-compute-scheduler-priorities (insts succs)
  "Return critical-path priorities for INSTS given successor vector SUCCS."
  (let* ((n (length insts))
         (vec (coerce insts 'vector))
         (memo (make-array n :initial-element nil)))
    (labels ((priority (i)
               (or (aref memo i)
                   (setf (aref memo i)
                         (+ (cl-cc/optimize::%opt-inst-latency (aref vec i))
                            (loop for succ in (aref succs i)
                                  maximize (priority succ) into best
                                  finally (return (or best 0))))))))
      (loop for i from 0 below n do (priority i))
      memo)))

(defun %post-ra-increment-reg-counts (regs counts)
  "Increment COUNTS for each physical register in REGS."
  (dolist (reg regs counts)
    (when reg (incf (gethash reg counts 0)))))

(defun %post-ra-decrement-reg-counts (regs counts)
  "Decrement COUNTS for each physical register in REGS, removing zero entries."
  (dolist (reg regs counts)
    (when reg
      (let ((next (1- (gethash reg counts 0))))
        (if (plusp next)
            (setf (gethash reg counts) next)
            (remhash reg counts))))))

(defun %post-ra-build-read-counts (insts live-out ra)
  "Return physical read-count table for unscheduled INSTS plus LIVE-OUT uses."
  (let ((counts (make-hash-table :test #'eq)))
    (%post-ra-increment-reg-counts live-out counts)
    (dolist (inst insts counts)
      (multiple-value-bind (reads known-p) (%post-ra-inst-read-regs inst ra)
        (when known-p (%post-ra-increment-reg-counts reads counts))))))

(defun %post-ra-pressure-delta (inst counts ra)
  "Estimate physical-register pressure delta if INST is scheduled next."
  (multiple-value-bind (reads reads-known-p) (%post-ra-inst-read-regs inst ra)
    (multiple-value-bind (writes writes-known-p) (%post-ra-inst-write-regs inst ra)
      (if (and reads-known-p writes-known-p)
          (let ((delta 0))
            (dolist (reg reads)
              (when (= (gethash reg counts 0) 1)
                (decf delta)))
            (dolist (reg writes)
              (when (plusp (gethash reg counts 0))
                (incf delta)))
            delta)
          0))))

(defun %post-ra-best-ready-node (ready insts priorities counts ra)
  "Select pressure-aware best node with deterministic tie-breaking."
  (let ((vec (coerce insts 'vector)))
    (reduce (lambda (best candidate)
              (let ((best-delta (%post-ra-pressure-delta (aref vec best) counts ra))
                    (candidate-delta (%post-ra-pressure-delta (aref vec candidate) counts ra))
                    (best-priority (aref priorities best))
                    (candidate-priority (aref priorities candidate)))
                (if (or (< candidate-delta best-delta)
                        (and (= candidate-delta best-delta)
                             (> candidate-priority best-priority))
                        (and (= candidate-delta best-delta)
                             (= candidate-priority best-priority)
                             (< candidate best)))
                    candidate
                    best)))
            ready)))

(defun %post-ra-live-before-instruction (inst live-after ra)
  "Return physical registers live immediately before INST given LIVE-AFTER."
  (multiple-value-bind (reads reads-known-p) (%post-ra-inst-read-regs inst ra)
    (multiple-value-bind (writes writes-known-p) (%post-ra-inst-write-regs inst ra)
      (if (and reads-known-p writes-known-p)
          (let ((live live-after))
            (dolist (reg writes)
              (setf live (remove reg live :test #'eq)))
            (dolist (reg reads live)
              (pushnew reg live :test #'eq)))
          live-after))))

(defun %post-ra-live-before-instructions (instructions live-after ra)
  "Return physical registers live before INSTRUCTIONS given LIVE-AFTER."
  (let ((live live-after))
    (dolist (inst (reverse instructions) live)
      (setf live (%post-ra-live-before-instruction inst live ra)))))

(defun %post-ra-scheduler-segments (instructions ra)
  "Split INSTRUCTIONS into post-RA movable runs and fixed barriers."
  (let ((segments nil)
        (run nil))
    (labels ((flush-run ()
               (when run
                 (push (list :run (nreverse run)) segments)
                 (setf run nil))))
      (dolist (inst instructions)
        (if (%post-ra-scheduler-barrier-p inst ra)
            (progn
              (flush-run)
              (push (list :barrier inst) segments))
            (push inst run)))
      (flush-run)
      (nreverse segments))))

(defun %post-ra-schedule-run (insts live-out ra)
  "Pressure-aware post-RA list scheduling for one side-effect-free run."
  (if (< (length insts) 2)
      insts
      (multiple-value-bind (preds succs) (%post-ra-build-scheduler-graph insts ra)
        (let* ((n (length insts))
               (vec (coerce insts 'vector))
               (priorities (%post-ra-compute-scheduler-priorities insts succs))
               (remaining-preds (make-array n))
               (counts (%post-ra-build-read-counts insts live-out ra))
               (ready nil)
               (emitted nil))
          (loop for i from 0 below n do
            (setf (aref remaining-preds i) (copy-list (aref preds i)))
            (when (null (aref remaining-preds i))
              (push i ready)))
          (loop while ready do
            (let* ((node (%post-ra-best-ready-node ready insts priorities counts ra))
                   (inst (aref vec node)))
              (setf ready (remove node ready :test #'eql))
              (push inst emitted)
              (multiple-value-bind (reads known-p) (%post-ra-inst-read-regs inst ra)
                (when known-p (%post-ra-decrement-reg-counts reads counts)))
              (dolist (succ (aref succs node))
                (setf (aref remaining-preds succ)
                      (remove node (aref remaining-preds succ) :test #'eql))
                (when (null (aref remaining-preds succ))
                  (pushnew succ ready :test #'eql)))))
          (if (= (length emitted) n)
              (nreverse emitted)
              insts)))))

(defun %post-ra-schedule-basic-block (instructions ra)
  "Schedule one basic block after register allocation, preserving barriers."
  (let ((live-after nil)
        (scheduled-segments nil))
    (dolist (segment (reverse (%post-ra-scheduler-segments instructions ra)))
      (ecase (first segment)
        (:barrier
         (let ((inst (second segment)))
           (push (list inst) scheduled-segments)
           (setf live-after (%post-ra-live-before-instruction inst live-after ra))))
        (:run
         (let* ((run (second segment))
                (scheduled (%post-ra-schedule-run run live-after ra)))
           (push scheduled scheduled-segments)
           (setf live-after (%post-ra-live-before-instructions scheduled live-after ra))))))
    (apply #'append scheduled-segments)))

(defun %post-ra-flatten-cfg-block-order (cfg)
  "Flatten CFG in original block order after post-RA local scheduling."
  (let ((result nil))
    (loop for block across (cfg-blocks cfg) do
      (when (bb-label block)
        (push (bb-label block) result))
      (dolist (inst (bb-instructions block))
        (push inst result)))
    (nreverse result)))

(defun schedule-post-ra (instructions regalloc-result)
  "FR-068: pressure-aware list scheduling after register allocation.

INSTRUCTIONS must be the allocated instruction stream from REGALLOC-RESULT.  The
pass resolves VM virtual registers to physical registers with REGALLOC-LOOKUP,
constructs dependency DAGs from physical RAW/WAR/WAW hazards, and uses physical
register pressure as the first ready-node tie-breaker.  Calls, stores, stack
spill helpers, control flow, unknown operands, and side-effecting instructions
are fixed barriers, making the pass conservative and deterministic."
  (let ((cfg (cfg-build instructions)))
    (loop for block across (cfg-blocks cfg) do
      (setf (bb-instructions block)
            (%post-ra-schedule-basic-block (bb-instructions block) regalloc-result)))
    (%post-ra-flatten-cfg-block-order cfg)))
