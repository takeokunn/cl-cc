(in-package :cl-cc/optimize)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Optimizer — Dependency-Aware Local Instruction Scheduling (FR-067/069)
;;;
;;; FR-069: Peephole list-scheduling inside basic blocks using RAW/WAR/WAW
;;; dependency DAGs and estimated VM instruction latencies.
;;; FR-067: Pre-RA pressure-aware scheduling with register live-out tracking.
;;;
;;; Load order: after optimizer-pipeline (optional-pass infrastructure).
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; FR-069: Dependency-Aware Peephole Scheduling
;;;
;;; Schedule movable instruction runs inside each basic block.  A run ends at any
;;; instruction with side effects or control flow, so calls/stores/signals and
;;; labels/terminators keep their original relative position.

(defparameter *opt-vm-instruction-latencies*
  (%alist->eq-hash-table
   '((vm-move . 1) (vm-const . 1)
     (vm-add . 1) (vm-integer-add . 1) (vm-add-checked . 1) (vm-float-add . 1)
     (vm-sub . 1) (vm-integer-sub . 1) (vm-sub-checked . 1) (vm-float-sub . 1)
     (vm-neg . 1) (vm-abs . 1) (vm-inc . 1) (vm-dec . 1)
     (vm-logand . 1) (vm-logior . 1) (vm-logxor . 1) (vm-logeqv . 1)
     (vm-lognot . 1) (vm-ash . 1) (vm-rotate . 1) (vm-bswap . 1)
     (vm-lt . 1) (vm-gt . 1) (vm-le . 1) (vm-ge . 1) (vm-eq . 1) (vm-num-eq . 1)
     (vm-min . 1) (vm-max . 1) (vm-not . 1)
     (vm-cons-p . 1) (vm-null-p . 1) (vm-symbol-p . 1) (vm-number-p . 1)
     (vm-integer-p . 1) (vm-function-p . 1)
     (vm-mul . 4) (vm-integer-mul . 4) (vm-mul-checked . 4) (vm-float-mul . 4)
     (vm-fma . 4)
     (vm-div . 40) (vm-cl-div . 40) (vm-float-div . 40)
     (vm-mod . 30) (vm-rem . 30)
     (vm-truncate . 40) (vm-floor-inst . 40) (vm-ceiling-inst . 40)
     (vm-round-inst . 40) (vm-ffloor . 40) (vm-fceiling . 40)
     (vm-ftruncate . 40) (vm-fround . 40)
     (vm-car . 4) (vm-cdr . 4) (vm-slot-read . 5) (vm-closure-ref-idx . 4)
     (vm-get-global . 5) (vm-func-ref . 4) (vm-values-to-list . 4)))
  "Estimated VM instruction latencies in cycles for local list scheduling.")

(defun %opt-inst-latency (inst)
  "Return the estimated latency of INST in cycles."
  (or (gethash (type-of inst) *opt-vm-instruction-latencies*) 1))

(defun %opt-scheduler-barrier-p (inst)
  "T when INST must keep its position in local scheduling."
  (or (member (type-of inst)
              '(vm-call vm-apply vm-generic-call vm-slot-write vm-set-global
                vm-signal vm-signal-error vm-error-instruction vm-cerror vm-warn)
              :test #'eq)
      (not (member (vm-inst-effect-kind inst) '(:pure :read-only) :test #'eq))))

(defun %opt-inst-write-regs (inst)
  "Return registers written by INST."
  (let ((dst (opt-inst-dst inst)))
    (and dst (list dst))))

(defun %opt-reg-intersect-p (a b)
  "T when register lists A and B intersect by EQ."
  (and a b (intersection a b :test #'eq)))

(defun %opt-scheduler-depends-p (earlier later)
  "T when EARLIER must precede LATER due to RAW, WAR, or WAW hazards."
  (let ((earlier-reads  (opt-inst-read-regs earlier))
        (earlier-writes (%opt-inst-write-regs earlier))
        (later-reads    (opt-inst-read-regs later))
        (later-writes   (%opt-inst-write-regs later)))
    (or (%opt-reg-intersect-p earlier-writes later-reads)   ; RAW
        (%opt-reg-intersect-p earlier-reads later-writes)   ; WAR
        (%opt-reg-intersect-p earlier-writes later-writes)))) ; WAW

(defun %opt-build-scheduler-graph (insts)
  "Build dependency predecessor/successor vectors for INSTS."
  (let* ((n (length insts))
         (preds (make-array n :initial-element nil))
         (succs (make-array n :initial-element nil)))
    (loop for i from 0 below n do
      (loop for j from (1+ i) below n
            when (%opt-scheduler-depends-p (nth i insts) (nth j insts))
              do (pushnew i (aref preds j))
                 (pushnew j (aref succs i))))
    (values preds succs)))

(defun %opt-compute-scheduler-priorities (insts succs)
  "Return critical-path priorities for INSTS given successor vector SUCCS."
  (let* ((n (length insts))
         (memo (make-array n :initial-element nil)))
    (labels ((priority (i)
               (or (aref memo i)
                   (setf (aref memo i)
                         (+ (%opt-inst-latency (nth i insts))
                            (loop for succ in (aref succs i)
                                  maximize (priority succ) into best
                                  finally (return (or best 0))))))))
      (loop for i from 0 below n do (priority i))
      memo)))

(defun %opt-best-ready-node (ready priorities)
  "Select the highest-priority node from READY, preserving original order on ties."
  (reduce (lambda (best candidate)
            (let ((best-priority (aref priorities best))
                  (candidate-priority (aref priorities candidate)))
              (if (or (> candidate-priority best-priority)
                      (and (= candidate-priority best-priority) (< candidate best)))
                  candidate
                  best)))
          ready))

(defun %opt-add-regs (regs live)
  "Return LIVE with REGS added using EQ identity."
  (let ((result live))
    (dolist (reg regs result)
      (when reg
        (pushnew reg result :test #'eq)))))

(defun %opt-remove-regs (regs live)
  "Return LIVE with REGS removed using EQ identity."
  (let ((result live))
    (dolist (reg regs result)
      (setf result (remove reg result :test #'eq)))))

(defun %opt-live-before-instruction (inst live-after)
  "Return registers live immediately before INST given LIVE-AFTER."
  (%opt-add-regs (opt-inst-read-regs inst)
                 (%opt-remove-regs (%opt-inst-write-regs inst) live-after)))

(defun %opt-live-before-instructions (instructions live-after)
  "Return registers live before INSTRUCTIONS given LIVE-AFTER."
  (let ((live live-after))
    (dolist (inst (reverse instructions) live)
      (setf live (%opt-live-before-instruction inst live)))))

(defun %opt-increment-reg-counts (regs counts)
  "Increment COUNTS for each REG in REGS."
  (dolist (reg regs counts)
    (when reg
      (incf (gethash reg counts 0)))))

(defun %opt-decrement-reg-counts (regs counts)
  "Decrement COUNTS for each REG in REGS, removing zero entries."
  (dolist (reg regs counts)
    (when reg
      (let ((next (1- (gethash reg counts 0))))
        (if (plusp next)
            (setf (gethash reg counts) next)
            (remhash reg counts))))))

(defun %opt-build-read-counts (insts live-out)
  "Return a read-count table for unscheduled INSTS plus LIVE-OUT pseudo-uses."
  (let ((counts (make-hash-table :test #'eq)))
    (%opt-increment-reg-counts live-out counts)
    (dolist (inst insts counts)
      (%opt-increment-reg-counts (opt-inst-read-regs inst) counts))))

(defun %opt-pre-ra-pressure-delta (inst counts)
  "Estimate live-register pressure delta if INST is scheduled next.

Negative values reduce pressure: operands whose last unscheduled use is this
instruction die.  Positive values increase pressure: destinations that are still
needed by unscheduled instructions or live-out become live."
  (let ((reads (opt-inst-read-regs inst))
        (writes (%opt-inst-write-regs inst))
        (delta 0))
    (dolist (reg reads)
      (when (= (gethash reg counts 0) 1)
        (decf delta)))
    (dolist (reg writes)
      (when (plusp (gethash reg counts 0))
        (incf delta)))
    delta))

(defun %opt-best-pre-ra-ready-node (ready insts priorities counts)
  "Select pressure-aware best node from READY.

Prefer instructions that reduce register pressure, then longer critical paths,
then original order for deterministic output."
  (reduce (lambda (best candidate)
            (let ((best-delta (%opt-pre-ra-pressure-delta (nth best insts) counts))
                  (candidate-delta (%opt-pre-ra-pressure-delta (nth candidate insts) counts))
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
          ready))

(defun %opt-run-list-scheduler (insts preds succs priorities node-selector-fn
                               &optional (post-select-fn nil))
  "Topological-sort INSTS using the pre-built dependency graph.

PREDS and SUCCS are vectors of predecessor/successor index lists.
PRIORITIES is a vector of critical-path priority values (one per instruction).
NODE-SELECTOR-FN is called as (node-selector-fn ready) and returns the index of
the next instruction to emit from the READY worklist.
POST-SELECT-FN, when non-nil, is called as (post-select-fn node inst) after each
instruction is chosen and before its successors are unblocked; it may be used to
update auxiliary state such as register-pressure counts.

Returns the re-ordered instruction list, or the original INSTS list unchanged if
the topological sort could not complete (cycle detected)."
  (declare (ignore priorities))
  (let* ((n (length insts))
         (remaining-preds (make-array n))
         (ready nil)
         (emitted nil))
    (loop for i from 0 below n do
      (setf (aref remaining-preds i) (copy-list (aref preds i)))
      (when (null (aref remaining-preds i))
        (push i ready)))
    (loop while ready do
      (let* ((node (funcall node-selector-fn ready))
             (inst (nth node insts)))
        (setf ready (remove node ready :test #'eql))
        (push inst emitted)
        (when post-select-fn
          (funcall post-select-fn node inst))
        (dolist (succ (aref succs node))
          (setf (aref remaining-preds succ)
                (remove node (aref remaining-preds succ) :test #'eql))
          (when (null (aref remaining-preds succ))
            (pushnew succ ready :test #'eql)))))
    (if (= (length emitted) n)
        (nreverse emitted)
        insts)))

(defun %opt-schedule-run (insts)
  "List-schedule a side-effect-free instruction run."
  (if (< (length insts) 2)
      insts
      (multiple-value-bind (preds succs) (%opt-build-scheduler-graph insts)
        (let ((priorities (%opt-compute-scheduler-priorities insts succs)))
          (%opt-run-list-scheduler insts preds succs priorities
                                   (lambda (ready)
                                     (%opt-best-ready-node ready priorities)))))))

(defun %opt-schedule-pre-ra-run (insts live-out)
  "Pressure-aware list-schedule a side-effect-free pre-RA instruction run."
  (if (< (length insts) 2)
      insts
      (multiple-value-bind (preds succs) (%opt-build-scheduler-graph insts)
        (let* ((priorities (%opt-compute-scheduler-priorities insts succs))
               (counts (%opt-build-read-counts insts live-out)))
          (%opt-run-list-scheduler insts preds succs priorities
                                   (lambda (ready)
                                     (%opt-best-pre-ra-ready-node ready insts priorities counts))
                                   (lambda (node inst)
                                     (declare (ignore node))
                                     (%opt-decrement-reg-counts
                                       (opt-inst-read-regs inst) counts)))))))

(defun %opt-schedule-basic-block (instructions)
  "Schedule each movable run inside one basic block INSTRUCTIONS."
  (let ((result nil)
        (run nil))
    (labels ((flush-run ()
               (when run
                 (dolist (inst (%opt-schedule-run (nreverse run)))
                   (push inst result))
                 (setf run nil))))
      (dolist (inst instructions)
        (if (%opt-scheduler-barrier-p inst)
            (progn
              (flush-run)
              (push inst result))
            (push inst run)))
      (flush-run)
      (nreverse result))))

(defun %opt-scheduler-segments (instructions)
  "Split INSTRUCTIONS into movable runs and fixed barriers."
  (let ((segments nil)
        (run nil))
    (labels ((flush-run ()
               (when run
                 (push (list :run (nreverse run)) segments)
                 (setf run nil))))
      (dolist (inst instructions)
        (if (%opt-scheduler-barrier-p inst)
            (progn
              (flush-run)
              (push (list :barrier inst) segments))
            (push inst run)))
      (flush-run)
      (nreverse segments))))

(defun %opt-schedule-pre-ra-basic-block (instructions)
  "Pressure-aware scheduling for one basic block, preserving all barriers."
  (let ((live-after nil)
        (scheduled-segments nil))
    (dolist (segment (reverse (%opt-scheduler-segments instructions)))
      (ecase (first segment)
        (:barrier
         (let ((inst (second segment)))
           (push (list inst) scheduled-segments)
           (setf live-after (%opt-live-before-instruction inst live-after))))
        (:run
         (let* ((run (second segment))
                (scheduled (%opt-schedule-pre-ra-run run live-after)))
           (push scheduled scheduled-segments)
           (setf live-after (%opt-live-before-instructions scheduled live-after))))))
    (apply #'append scheduled-segments)))

(defun %opt-flatten-cfg-block-order (cfg)
  "Flatten CFG in original block creation order after local block rewrites."
  (let ((result nil))
    (loop for block across (cfg-blocks cfg) do
      (when (bb-label block)
        (push (bb-label block) result))
      (dolist (inst (bb-instructions block))
        (push inst result)))
    (nreverse result)))

(defun opt-pass-schedule-local (instructions)
  "FR-069: Dependency-aware list scheduling within each basic block.

Builds a local DAG with RAW/WAR/WAW register dependencies, computes critical-path
priorities from estimated VM latencies, and emits highest-priority ready nodes.
Scheduling is limited to side-effect-free runs inside a basic block; calls,
stores, signals, and control-flow instructions are barriers."
  (let ((cfg (cfg-build instructions)))
    (loop for block across (cfg-blocks cfg) do
      (setf (bb-instructions block)
            (%opt-schedule-basic-block (bb-instructions block))))
    (%opt-flatten-cfg-block-order cfg)))

(defun schedule-pre-ra (instructions)
  "FR-067: pressure-aware list scheduling before register allocation.

Builds a dependency DAG for each basic block, computes critical-path priorities,
and schedules ready instructions with a register-pressure tie-breaker.  The pass
never moves instructions across labels, control-flow instructions, calls, stores,
signals, or other side-effecting barriers, so basic-block boundaries and codegen
semantics are preserved.  FR-067 is complete and this function is the backend
entry point used before register allocation."
  (let ((cfg (cfg-build instructions)))
    (loop for block across (cfg-blocks cfg) do
      (setf (bb-instructions block)
            (%opt-schedule-pre-ra-basic-block (bb-instructions block))))
    (%opt-flatten-cfg-block-order cfg)))
