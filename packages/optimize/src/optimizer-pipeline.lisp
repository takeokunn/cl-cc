(in-package :cl-cc/optimize)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Optimizer — Pass Pipeline, Reporting, and Public Driver
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun opt-pass-inline-iterative (instructions)
  "Thresholded inline pass used inside the convergence loop."
  (opt-pass-inline instructions :threshold :adaptive))

(defun %opt-backedge-count (instructions)
  "Return a simple count of backward jumps in INSTRUCTIONS."
  (let ((label-pos (make-hash-table :test #'equal)))
    (loop for inst in instructions
          for i from 0
          when (typep inst 'vm-label)
            do (setf (gethash (vm-name inst) label-pos) i))
    (loop for inst in instructions
          for i from 0
          count (and (typep inst '(or vm-jump vm-jump-zero))
                     (let ((target (gethash (vm-label-name inst) label-pos)))
                       (and target (< target i)))))))

(defun opt-adaptive-loop-unroll-factor (instructions &key call-count hotness)
  "Return adaptive loop-unroll factor and max trip values for INSTRUCTIONS."
  (let* ((n (length instructions))
         (score (or hotness (+ (or call-count 0) (* 10 (%opt-backedge-count instructions))))))
    (cond
      ((>= score 100) (values 4 16))
      ((>= score 30) (values 3 12))
      ((and (> n 800) (zerop score)) (values 1 4))
      (t (values 2 8)))))

(defun opt-pass-loop-unrolling-adaptive (instructions)
  "Run loop unrolling with adaptive loop thresholds scoped to this pass."
  (multiple-value-bind (factor max-trip)
      (opt-adaptive-loop-unroll-factor instructions)
    (let ((*opt-loop-unroll-factor* factor)
          (*opt-loop-unroll-max-trip* max-trip))
      (opt-pass-loop-unrolling instructions))))

(defvar *enable-prolog-peephole* t
  "When non-NIL, run the Prolog peephole and e-graph rewrite stages.

This is an optimizer policy gate, not Prolog engine state.")

(defun %maybe-apply-prolog-rewrite (instructions)
  "Apply the Prolog rewrite stage when enabled, preserving INSTRUCTIONS otherwise.
The stage first applies the instruction-level Prolog peephole rules and then runs
the e-graph rewrite engine whose builtin rule set is also sourced from Prolog facts."
  (if *enable-prolog-peephole*
      (optimize-with-egraph
       (mapcar #'sexp->instruction
               (apply-prolog-peephole (mapcar #'instruction->sexp instructions))))
      instructions))

(defvar *opt-enable-pure-call-optimization* t
  "When NIL, disable pure-call optimization regardless of selected pass pipeline.

This hook is used as an optimization-policy gate so frontends can couple the
pass to `(optimize (speed 3))`-style policy decisions without changing the
optimizer's pass table wiring.")

(defvar *opt-enable-sealed-gf-devirtualization* t
  "When NIL, keep sealed generic calls as dynamic `vm-generic-call` instructions.

This optimization is policy-gated because it trades compilation effort and a
closed-world proof for direct method invocation.  `opt-configure-optimization-policy`
enables it for SPEED >= 2.")

(defun %maybe-run-pure-call-optimization (instructions)
  "Run pure-call optimization only when policy gate permits it."
  (if *opt-enable-pure-call-optimization*
      (opt-pass-pure-call-optimization instructions)
      instructions))

(defun %opt-run-pass-if-fbound (pass-symbol instructions)
  "Run PASS-SYMBOL on INSTRUCTIONS when it is fbound, otherwise no-op."
  (if (fboundp pass-symbol)
      (funcall (symbol-function pass-symbol) instructions)
      instructions))

(defmacro define-optional-pass (name &key pass doc)
  "Define a %maybe-run-NAME wrapper that delegates to opt-pass-PASS (or opt-pass-NAME).
Data: the pass name. Logic: fbound check and dispatch via %opt-run-pass-if-fbound."
  (let* ((n         (symbol-name name))
         (fn-name   (intern (format nil "%MAYBE-RUN-~A" n)))
         (pass-sym  (if pass
                        (intern (format nil "OPT-PASS-~A" (symbol-name pass)))
                        (intern (format nil "OPT-PASS-~A" n)))))
    `(defun ,fn-name (instructions)
       ,@(when doc (list doc))
       (%opt-run-pass-if-fbound ',pass-sym instructions))))

;;; Optional pass wrappers — data-driven with define-optional-pass.
;;; Each entry: (wrapper-name [:pass pass-name] [:doc "docstring"])
;;; Logic (fbound check + dispatch) lives in the macro; only data varies.

(define-optional-pass fr523-affine-loop-analysis :pass affine-loop-analysis)
(define-optional-pass fr524-loop-interchange     :pass loop-interchange)
(define-optional-pass fr525-polyhedral-schedule  :pass polyhedral-schedule)
(define-optional-pass fr526-loop-fusion-fission  :pass loop-fusion-fission)
(define-optional-pass superopt
  :doc "Run FR-750 superoptimization when its pass file is loaded.")
(define-optional-pass speculative-inline
  :doc "Run FR-523 speculative inlining when the optional pass is loaded.")


(define-optional-pass loop-rotate)
(define-optional-pass dead-loop-elimination)
(define-optional-pass loop-unroll)
(define-optional-pass loop-unswitch)
(define-optional-pass dead-argument-elimination)
(define-optional-pass tail-duplication)
(define-optional-pass iv-strength-reduce)
(define-optional-pass div-by-const)
(define-optional-pass loop-peel)
(define-optional-pass idiom-recognition)
(define-optional-pass trmc)
(define-optional-pass value-range-propagation)
(define-optional-pass bounds-check-elimination)
(define-optional-pass overflow-check-elimination)
(define-optional-pass bitwidth-reduction)
(define-optional-pass cps-reduce)
(define-optional-pass defunctionalize)
(define-optional-pass delimited-continuations
  :doc "Run FR-677 delimited-continuation lowering only when explicitly selected.")
(define-optional-pass escape-analysis)
(define-optional-pass path-profiling
  :doc "Run FR-662 Basic Block Versioning / Path Profiling when loaded.")
(define-optional-pass load-store-coalescing :pass load-widening-store-coalescing
  :doc "Run FR-723 Load Widening / Store Coalescing when loaded.")
(define-optional-pass optimization-remarks)
(define-optional-pass abstract-interpretation
  :doc "Run FR-751 abstract interpretation when loaded and enabled.")
(define-optional-pass translation-validation
  :doc "Register FR-752 translation validation; per-pass checks are pipeline-integrated.")
(define-optional-pass loop-fusion)
(define-optional-pass loop-fission)
(define-optional-pass loop-tile)
(define-optional-pass autotune-simd)
(define-optional-pass polyhedral
  :doc "Run explicit FR-513 polyhedral pass only when its implementation is loaded.")
(define-optional-pass mlgo-inline
  :doc "Run explicit FR-580 MLGO inline pass only when its implementation is loaded.")
(define-optional-pass ml-regalloc
  :doc "Run explicit ML register-allocation hint pass only when loaded.")

;;; FR-069: Dependency-Aware Peephole Scheduling
;;;
;;; Schedule movable instruction runs inside each basic block.  A run ends at any
;;; instruction with side effects or control flow, so calls/stores/signals and
;;; labels/terminators keep their original relative position.

(defparameter *opt-vm-instruction-latency-alist*
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
    (vm-get-global . 5) (vm-func-ref . 4) (vm-values-to-list . 4))
  "Raw alist of VM instruction type → estimated latency in cycles.")

(defparameter *opt-vm-instruction-latencies*
  (%alist->eq-hash-table *opt-vm-instruction-latency-alist*)
  "Estimated VM instruction latencies in cycles for local list scheduling.
Derived from *opt-vm-instruction-latency-alist*.")

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

(defun %opt-run-scheduler-toposort (insts &key node-selector counts-or-nil)
  "Toposort-based list scheduler for a side-effect-free instruction run.

NODE-SELECTOR is a function (ready insts priorities [counts]) -> node-index.
COUNTS-OR-NIL is a pre-built read-count table for pre-RA pressure tracking,
or NIL for post-RA scheduling (where pressure is ignored)."
  (if (< (length insts) 2)
      insts
      (multiple-value-bind (preds succs) (%opt-build-scheduler-graph insts)
        (let* ((n              (length insts))
               (priorities     (%opt-compute-scheduler-priorities insts succs))
               (remaining-preds (make-array n))
               (ready          nil)
               (emitted        nil))
          (loop for i from 0 below n do
            (setf (aref remaining-preds i) (copy-list (aref preds i)))
            (when (null (aref remaining-preds i))
              (push i ready)))
          (loop while ready do
            (let* ((node (if counts-or-nil
                             (funcall node-selector ready insts priorities counts-or-nil)
                             (funcall node-selector ready priorities)))
                   (inst (nth node insts)))
              (setf ready (remove node ready :test #'eql))
              (push inst emitted)
              (when counts-or-nil
                (%opt-decrement-reg-counts (opt-inst-read-regs inst) counts-or-nil))
              (dolist (succ (aref succs node))
                (setf (aref remaining-preds succ)
                      (remove node (aref remaining-preds succ) :test #'eql))
                (when (null (aref remaining-preds succ))
                  (pushnew succ ready :test #'eql)))))
          (if (= (length emitted) n)
              (nreverse emitted)
              insts)))))

(defun %opt-schedule-run (insts)
  "List-schedule a side-effect-free instruction run."
  (%opt-run-scheduler-toposort insts :node-selector #'%opt-best-ready-node))

(defun %opt-schedule-pre-ra-run (insts live-out)
  "Pressure-aware list-schedule a side-effect-free pre-RA instruction run."
  (%opt-run-scheduler-toposort insts
                               :node-selector #'%opt-best-pre-ra-ready-node
                               :counts-or-nil (%opt-build-read-counts insts live-out)))

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

;;; FR-099: FMA (Fused Multiply-Add) Pattern Recognition
;;;
;;; Pattern: (vm-float-mul A B → T) followed by (vm-float-add C T → D) where T is used only once
;;; → replace with a single FMA instruction (vm-fma A B C → D).

(defun %opt-register-read-count (reg instructions)
  "Return how many times REG is read by INSTRUCTIONS."
  (loop for inst in instructions
        sum (count reg (opt-inst-read-regs inst) :test #'eq)))

(defun %opt-fma-block-boundary-p (inst)
  "Return T when INST delimits a basic block for local FMA recognition."
  (or (typep inst 'vm-label)
      (eq (vm-inst-effect-kind inst) :control)))

(defun %opt-fma-pure-p (inst)
  "Return T when INST is side-effect free enough for FMA replacement."
  (eq (vm-inst-effect-kind inst) :pure))

(defun %opt-fma-add-accumulator (mul add)
  "Return ADD's non-MUL accumulator operand, or NIL when ADD does not read MUL."
  (let ((tmp (vm-dst mul)))
    (cond
      ((eq tmp (vm-lhs add)) (vm-rhs add))
      ((eq tmp (vm-rhs add)) (vm-lhs add))
      (t nil))))

(defun %opt-inst-writes-reg-p (inst reg)
  "Return T when INST writes REG."
  (eq (opt-inst-dst inst) reg))

(defun %opt-fma-intervening-barrier-p (inst protected-regs)
  "Return T when INST prevents moving an earlier multiply to a later FMA site."
  (or (not (%opt-fma-pure-p inst))
      (some (lambda (reg) (%opt-inst-writes-reg-p inst reg)) protected-regs)))

(defun %opt-fma-replacement (mul add instructions)
  "Return a VM-FMA replacing MUL and ADD when the FR-099 guards hold."
  (let ((acc (%opt-fma-add-accumulator mul add)))
    (when (and acc
               (%opt-fma-pure-p mul)
               (%opt-fma-pure-p add)
               (= 1 (%opt-register-read-count (vm-dst mul) instructions)))
      (make-vm-fma :dst (vm-dst add)
                   :a (vm-lhs mul)
                   :b (vm-rhs mul)
                   :c acc))))

(defun %opt-fuse-fma-in-block (block instructions)
  "Recognize FR-099 FMA patterns inside one basic block."
  (let* ((n (length block))
         (insts (coerce block 'vector))
         (removed (make-array n :initial-element nil))
         (replacements (make-hash-table :test #'eql)))
    (loop for mul-index from 0 below n
          for mul = (aref insts mul-index)
          when (and (not (aref removed mul-index))
                    (typep mul 'vm-float-mul)
                    (%opt-fma-pure-p mul)
                    (= 1 (%opt-register-read-count (vm-dst mul) instructions)))
            do (let ((protected-regs (remove-duplicates
                                      (list (vm-dst mul) (vm-lhs mul) (vm-rhs mul))
                                      :test #'eq)))
                 (loop for add-index from (1+ mul-index) below n
                       for candidate = (aref insts add-index)
                       do (cond
                            ((and (not (aref removed add-index))
                                  (typep candidate 'vm-float-add))
                             (let ((replacement (%opt-fma-replacement mul candidate instructions)))
                               (when replacement
                                 (setf (aref removed mul-index) t)
                                 (setf (gethash add-index replacements) replacement)
                                 (return))))
                            ((%opt-fma-intervening-barrier-p candidate protected-regs)
                             (return))))))
    (loop for i from 0 below n
          unless (aref removed i)
            collect (or (gethash i replacements) (aref insts i)))))

(defun opt-pass-fma-recognition (instructions)
  "FR-099: Recognize scalar floating FMA in flat VM instruction streams.

Within each basic block, detects (vm-float-mul A B → T) feeding exactly one
(vm-float-add T C → D) or (vm-float-add C T → D), then replaces the pair with
(vm-fma D A B C).  The pass refuses to cross labels/control-flow boundaries,
requires both arithmetic instructions to be pure, and preserves operand values by
not fusing across intervening writes to the multiply destination or operands."
  (let ((result nil)
        (block nil))
    (labels ((flush-block ()
               (when block
                 (setf result (nconc result (%opt-fuse-fma-in-block (nreverse block) instructions)))
                 (setf block nil))))
      (dolist (inst instructions)
        (if (%opt-fma-block-boundary-p inst)
            (progn
              (flush-block)
              (setf result (nconc result (list inst))))
            (push inst block)))
      (flush-block)
      result)))

;;; Single source of truth: ordered keyword → function pairs.
;;; *opt-convergence-passes* and *opt-pass-registry* are both derived from this.
(defparameter *opt-pass-table*
  `((:prolog-rewrite            . ,#'%maybe-apply-prolog-rewrite)
    (:superopt                  . ,#'%maybe-run-superopt)
    (:sequence-fusion           . ,#'opt-pass-sequence-fusion)
    (:egraph                    . ,#'optimize-with-egraph)
    (:call-site-splitting       . ,#'opt-pass-call-site-splitting)
     (:demand-analysis           . ,#'opt-pass-demand-analysis)
     (:devirtualize              . ,#'opt-pass-devirtualize)
     (:speculative-inline        . ,#'%maybe-run-speculative-inline)
     (:if-conversion             . ,#'opt-pass-if-conversion)
    (:closure-capture-dedup     . ,#'opt-pass-closure-capture-dedup)
    (:closure-thunk-sharing     . ,#'opt-pass-closure-thunk-sharing)
      (:inline                    . ,#'opt-pass-inline-iterative)
      (:fold                      . ,#'opt-pass-fold)
       (:overflow-check-elim       . ,#'%maybe-run-overflow-check-elimination)
       (:bounds-check-elim         . ,#'%maybe-run-bounds-check-elimination)
       (:value-range-propagation   . ,#'%maybe-run-value-range-propagation)
       (:sccp                      . ,#'opt-pass-sccp)
     (:cons-slot-forward         . ,#'opt-pass-cons-slot-forward)
      (:strength-reduce           . ,#'opt-pass-strength-reduce)
      (:iv-strength-reduce        . ,#'%maybe-run-iv-strength-reduce)
      (:div-by-const              . ,#'%maybe-run-div-by-const)
      (:bitwidth-reduction        . ,#'%maybe-run-bitwidth-reduction)
      (:idiom-recognition         . ,#'%maybe-run-idiom-recognition)
      (:fma-recognition           . ,#'opt-pass-fma-recognition)
      (:bswap-recognition         . ,#'opt-pass-bswap-recognition)
      (:rotate-recognition        . ,#'opt-pass-rotate-recognition)
      (:fill-recognition          . ,#'opt-pass-fill-recognition)
       (:copy-recognition          . ,#'opt-pass-copy-recognition)
       (:trmc                      . ,#'%maybe-run-trmc)
       (:auto-vectorization        . ,#'opt-pass-auto-vectorization)
       (:slp-vectorize             . ,#'opt-pass-slp-vectorize)
       (:function-outlining        . ,#'opt-pass-function-outlining)
      (:safepoint-polling         . ,#'opt-pass-safepoint-polling)
      (:software-pipelining       . ,#'opt-pass-software-pipelining)
      (:affine-loop-analysis      . ,#'%maybe-run-fr523-affine-loop-analysis)
     (:loop-interchange          . ,#'%maybe-run-fr524-loop-interchange)
     (:polyhedral-schedule       . ,#'%maybe-run-fr525-polyhedral-schedule)
      (:loop-fusion-fission       . ,#'%maybe-run-fr526-loop-fusion-fission)
        (:loop-fusion               . ,#'%maybe-run-loop-fusion)
        (:loop-fission              . ,#'%maybe-run-loop-fission)
        (:loop-tile                 . ,#'%maybe-run-loop-tile)
        (:autotune-simd             . ,#'%maybe-run-autotune-simd)
         (:polyhedral                . ,#'%maybe-run-polyhedral)
        (:mlgo-inline               . ,#'%maybe-run-mlgo-inline)
        (:ml-regalloc               . ,#'%maybe-run-ml-regalloc)
        (:loop-unswitch             . ,#'%maybe-run-loop-unswitch)
      (:reassociate               . ,#'opt-pass-reassociate)
     (:copy-prop                 . ,#'opt-pass-copy-prop)
     (:pure-call-optimization    . ,#'%maybe-run-pure-call-optimization)
     (:gvn                       . ,#'opt-pass-gvn)
     (:batch-concatenate         . ,#'opt-pass-batch-concatenate)
    (:cse                       . ,#'opt-pass-cse)
    (:jump                      . ,(symbol-function 'opt-pass-jump))
    (:loop-unrolling            . ,#'opt-pass-loop-unrolling-adaptive)
    (:loop-unroll               . ,#'%maybe-run-loop-unroll)
    (:loop-rotation             . ,#'opt-pass-loop-rotation)
    (:loop-rotate               . ,#'%maybe-run-loop-rotate)
      (:loop-peeling              . ,#'opt-pass-loop-peel)
      (:loop-peel                 . ,#'%maybe-run-loop-peel)
      (:prefetch-insertion        . ,#'opt-pass-prefetch-insertion)
      (:allocation-sinking
       . ,#'(lambda (instructions)
              (let ((cfg (cfg-build instructions))
                    (alias-facts (opt-compute-heap-aliases instructions)))
                (opt-sink-allocations instructions cfg alias-facts))))
     (:code-sinking              . ,#'opt-pass-code-sinking)
    (:unreachable               . ,#'opt-pass-unreachable)
    (:dead-basic-blocks         . ,#'opt-pass-dead-basic-blocks)
    (:store-to-load-forward     . ,#'opt-pass-store-to-load-forward)
    (:dead-store-elim           . ,#'opt-pass-dead-store-elim)
    (:load-store-coalescing     . ,#'%maybe-run-load-store-coalescing)
    (:nil-check-elim            . ,#'opt-pass-dominated-type-check-elim)
    (:dominated-type-check-elim . ,#'opt-pass-dominated-type-check-elim)
    (:branch-correlation        . ,#'opt-pass-branch-correlation)
    (:tail-duplication          . ,#'%maybe-run-tail-duplication)
    (:block-merge               . ,#'opt-pass-block-merge)
    (:tail-merge                . ,#'opt-pass-tail-merge)
    (:pre                       . ,#'opt-pass-pre)
    (:constant-hoist            . ,#'opt-pass-licm)
    (:global-dce                . ,#'opt-pass-global-dce)
     (:dead-labels               . ,#'opt-pass-dead-labels)
      (:hot-cold-layout           . ,#'opt-pass-hot-cold-layout)
      (:dead-loop-elimination     . ,#'%maybe-run-dead-loop-elimination)
      (:dead-argument-elimination . ,#'%maybe-run-dead-argument-elimination)
      (:cps-reduce                . ,#'%maybe-run-cps-reduce)
      (:defunctionalize           . ,#'%maybe-run-defunctionalize)
      (:delimited-continuations   . ,#'%maybe-run-delimited-continuations)
      (:escape-analysis           . ,#'%maybe-run-escape-analysis)
        (:branch-weights            . ,#'opt-analyze-branch-weights)
        (:path-profiling            . ,#'%maybe-run-path-profiling)
        (:dce                       . ,#'opt-pass-dce)
       (:optimization-remarks      . ,#'%maybe-run-optimization-remarks)
       (:abstract-interpretation   . ,#'%maybe-run-abstract-interpretation)
       (:translation-validation    . ,#'%maybe-run-translation-validation)
       (:schedule-local            . ,#'opt-pass-schedule-local))
  "Ordered (keyword . function) pairs — single source for pipeline and registry.")

(defparameter *opt-pass-registry*
  (loop with ht = (make-hash-table :test #'eq)
        for (k . v) in *opt-pass-table*
        do (setf (gethash k ht) v)
        finally (return ht))
  "Keyword → pass function mapping derived from *opt-pass-table*.")

(defparameter *opt-default-convergence-pass-keys*
  '(:prolog-rewrite
     :call-site-splitting
       :devirtualize
       :if-conversion
       :closure-capture-dedup
      :closure-thunk-sharing
       :inline
         :overflow-check-elim
         :sccp
       :cons-slot-forward
         :value-range-propagation
         :bounds-check-elim
       :sequence-fusion
     :demand-analysis
        :fma-recognition
        :iv-strength-reduce
        :div-by-const
        :bitwidth-reduction
        :idiom-recognition
        :bswap-recognition
     :rotate-recognition
      :fill-recognition
       :copy-recognition
       :trmc
       :auto-vectorization
      :slp-vectorize
       :function-outlining
       :safepoint-polling
       :software-pipelining
       :affine-loop-analysis
     :loop-interchange
     :polyhedral-schedule
      :loop-fusion-fission
       :loop-fusion
       :loop-fission
       :loop-tile
       :autotune-simd
       :loop-unswitch
      :reassociate
     :copy-prop
     :pure-call-optimization
     :gvn
     :batch-concatenate
    :cse
    :jump
     :loop-unrolling
     :loop-unroll
     :loop-rotation
     :loop-rotate
     :loop-peeling
     :loop-peel
     :prefetch-insertion
      :allocation-sinking
     :code-sinking
    :unreachable
    :dead-basic-blocks
    :store-to-load-forward
     :dead-store-elim
     :load-store-coalescing
     :nil-check-elim
    :dominated-type-check-elim
    :branch-correlation
    :tail-duplication
    :block-merge
    :tail-merge
    :pre
     :constant-hoist
     :global-dce
     :dead-labels
     :hot-cold-layout
     :dead-loop-elimination
     :dead-argument-elimination
     :cps-reduce
     :defunctionalize
     :escape-analysis
        :branch-weights
        :path-profiling
        :dce
       :fma-recognition
       :optimization-remarks
       :abstract-interpretation
       :translation-validation
       :schedule-local)
  "Default convergence pipeline keys.
`:egraph` remains available as an explicit pass, but the default rewrite stage is
`:prolog-rewrite`, which already composes both the Prolog peephole backend and
the e-graph engine.")

(defparameter *opt-convergence-passes*
  (mapcar (lambda (k) (gethash k *opt-pass-registry*))
          *opt-default-convergence-pass-keys*)
  "Ordered default pass functions derived from `*opt-default-convergence-pass-keys*`.")
