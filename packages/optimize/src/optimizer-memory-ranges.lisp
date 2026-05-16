;;;; optimizer-memory-ranges.lisp — CFG value ranges, induction variables, alias reasoning
(in-package :cl-cc/optimize)

(defun %opt-compute-value-ranges-linear (instructions)
  "Compute conservative intervals for a straight-line instruction list."
  (let ((intervals (make-hash-table :test #'eq)))
    (dolist (inst instructions intervals)
      (%opt-transfer-interval-inst inst intervals))))

(defun %opt-cfg-value-ranges-transfer (block state-in)
  "Transfer interval facts through BLOCK using CFG-safe updates."
  (let ((state-out (%opt-copy-interval-state state-in)))
    (dolist (inst (bb-instructions block) state-out)
      (%opt-transfer-interval-inst inst state-out :kill-self-updates t))))

(defun opt-compute-cfg-value-ranges (cfg-or-instructions)
  "Compute conservative CFG-aware integer value ranges.

Returns an OPT-DATAFLOW-RESULT with per-block IN/OUT maps. Join points keep
only registers known on every incoming path and union their intervals.
Self-updating destinations are killed conservatively to guarantee termination."
  (let* ((cfg (if (cfg-p cfg-or-instructions)
                  cfg-or-instructions
                  (cfg-build cfg-or-instructions)))
         (empty-state (make-hash-table :test #'eq)))
    (opt-run-dataflow cfg
                      :direction :forward
                      :meet #'%opt-merge-interval-states
                      :transfer #'%opt-cfg-value-ranges-transfer
                      :state-equal #'%opt-interval-state-equal-p
                      :initial-state empty-state
                      :boundary-state empty-state
                      :copy-state #'%opt-copy-interval-state)))

(defun %opt-control-flow-range-analysis-p (instructions)
  "Return T when INSTRUCTIONS require CFG-aware range analysis."
  (loop for inst in instructions
        thereis (typep inst '(or vm-label vm-jump vm-jump-zero))))

(defun %opt-cfg-result-exit-state (result)
  "Extract a copy of RESULT's exit OUT state as a plain interval table."
  (let* ((cfg (opt-dataflow-result-cfg result))
         (exit (and cfg (cfg-exit cfg))))
    (if exit
        (%opt-copy-interval-state
         (gethash exit (opt-dataflow-result-out result)))
        (make-hash-table :test #'eq))))

(defun opt-compute-value-ranges (instructions)
  "Compute conservative integer value ranges.

Straight-line callers keep the existing reg -> interval hash-table API. When
INSTRUCTIONS contain control flow, this wrapper runs CFG-aware analysis and
returns the merged exit-state interval table for convenience callers."
  (if (%opt-control-flow-range-analysis-p instructions)
      (%opt-cfg-result-exit-state (opt-compute-cfg-value-ranges instructions))
      (%opt-compute-value-ranges-linear instructions)))

(defparameter +opt-range-negative-infinity+ most-negative-fixnum
  "Finite sentinel used as the conservative lower bound for path facts.")

(defparameter +opt-range-positive-infinity+ most-positive-fixnum
  "Finite sentinel used as the conservative upper bound for path facts.")

(defun %opt-top-interval ()
  (opt-make-interval +opt-range-negative-infinity+
                     +opt-range-positive-infinity+))

(defun %opt-interval-intersect (a b)
  "Return the intersection of A and B, or NIL when empty."
  (let ((lo (max (opt-interval-lo a) (opt-interval-lo b)))
        (hi (min (opt-interval-hi a) (opt-interval-hi b))))
    (when (<= lo hi)
      (opt-make-interval lo hi))))

(defun %opt-state-interval-or-top (state reg)
  (or (gethash reg state) (%opt-top-interval)))

(defun %opt-narrow-state-reg (state reg constraint)
  "Intersect REG in STATE with CONSTRAINT. Return NIL if the edge is infeasible."
  (let ((narrowed (%opt-interval-intersect
                   (%opt-state-interval-or-top state reg)
                   constraint)))
    (when narrowed
      (setf (gethash reg state) narrowed)
      state)))

(defun %opt-last-instruction-of-type (instructions type)
  (find-if (lambda (inst) (typep inst type)) (reverse instructions)))

(defun %opt-branch-predicate-inst (block jump-inst)
  "Return the comparison instruction feeding JUMP-INST, if it is block-local."
  (let ((cond-reg (vm-reg jump-inst)))
    (find-if (lambda (inst)
               (and (typep inst '(or vm-lt vm-le vm-gt vm-ge vm-eq vm-num-eq))
                    (eq (vm-dst inst) cond-reg)))
             (reverse (bb-instructions block)))))

(defun %opt-successor-is-jump-target-p (succ jump-inst)
  (let ((label (bb-label succ)))
    (and label (equal (vm-name label) (vm-label-name jump-inst)))))

(defun %opt-apply-lt-constraint (state lhs rhs true-p strict-p)
  "Apply LHS < RHS or LHS <= RHS when TRUE-P, otherwise its negation."
  (let* ((lhs-iv (%opt-state-interval-or-top state lhs))
         (rhs-iv (%opt-state-interval-or-top state rhs))
         (lhs-lo (opt-interval-lo lhs-iv))
         (lhs-hi (opt-interval-hi lhs-iv))
         (rhs-lo (opt-interval-lo rhs-iv))
         (rhs-hi (opt-interval-hi rhs-iv)))
    (if true-p
        (let ((lhs-bound (if strict-p (1- rhs-hi) rhs-hi))
              (rhs-bound (if strict-p (1+ lhs-lo) lhs-lo)))
          (and (%opt-narrow-state-reg state lhs
                                      (opt-make-interval +opt-range-negative-infinity+
                                                         lhs-bound))
               (%opt-narrow-state-reg state rhs
                                      (opt-make-interval rhs-bound
                                                         +opt-range-positive-infinity+))))
        (let ((lhs-bound (if strict-p rhs-lo (1+ rhs-lo)))
              (rhs-bound (if strict-p lhs-hi (1- lhs-hi))))
          (and (%opt-narrow-state-reg state lhs
                                      (opt-make-interval lhs-bound
                                                         +opt-range-positive-infinity+))
               (%opt-narrow-state-reg state rhs
                                      (opt-make-interval +opt-range-negative-infinity+
                                                         rhs-bound)))))))

(defun %opt-apply-eq-constraint (state lhs rhs true-p)
  "Apply equality narrowing for the true edge; false edge has no interval fact."
  (if (not true-p)
      state
      (let* ((lhs-iv (%opt-state-interval-or-top state lhs))
             (rhs-iv (%opt-state-interval-or-top state rhs))
             (intersection (%opt-interval-intersect lhs-iv rhs-iv)))
        (when intersection
          (setf (gethash lhs state) intersection
                (gethash rhs state) intersection)
          state))))

(defun %opt-apply-branch-predicate-fact (state cmp-inst true-p)
  "Return STATE narrowed by CMP-INST for TRUE-P, or NIL if infeasible.

For vm-jump-zero, the explicit jump target is the zero/false branch
(the VM jumps when the condition register is 0), while the fallthrough
is the non-zero/true branch."
  (cond
    ((typep cmp-inst 'vm-lt)
     (%opt-apply-lt-constraint state (vm-lhs cmp-inst) (vm-rhs cmp-inst) true-p t))
    ((typep cmp-inst 'vm-le)
     (%opt-apply-lt-constraint state (vm-lhs cmp-inst) (vm-rhs cmp-inst) true-p nil))
    ((typep cmp-inst 'vm-gt)
     (%opt-apply-lt-constraint state (vm-rhs cmp-inst) (vm-lhs cmp-inst) true-p t))
    ((typep cmp-inst 'vm-ge)
     (%opt-apply-lt-constraint state (vm-rhs cmp-inst) (vm-lhs cmp-inst) true-p nil))
    ((typep cmp-inst '(or vm-eq vm-num-eq))
     (%opt-apply-eq-constraint state (vm-lhs cmp-inst) (vm-rhs cmp-inst) true-p))
    (t state)))

(defun %opt-path-edge-state (block succ state-out)
  "Return the outgoing interval state for edge BLOCK -> SUCC."
  (let* ((state (%opt-copy-interval-state state-out))
         (jump (%opt-last-instruction-of-type (bb-instructions block) 'vm-jump-zero))
         (cmp (and jump (%opt-branch-predicate-inst block jump))))
    (if cmp
        (%opt-apply-branch-predicate-fact
         state cmp (not (%opt-successor-is-jump-target-p succ jump)))
        state)))

(defun %opt-predecessor-path-states (block out-table)
  (loop for pred in (bb-predecessors block)
        for out-state = (gethash pred out-table)
        for edge-state = (and out-state (%opt-path-edge-state pred block out-state))
        when edge-state
          collect edge-state))

(defun %opt-path-sensitive-entry-table (in-table)
  "Flatten block entry states to a (BLOCK . REG) -> interval hash-table."
  (let ((ranges (make-hash-table :test #'equal)))
    (maphash (lambda (block state)
               (maphash (lambda (reg interval)
                          (setf (gethash (cons block reg) ranges)
                                (opt-make-interval (opt-interval-lo interval)
                                                   (opt-interval-hi interval))))
                        state))
             in-table)
    ranges))

(defun opt-compute-path-sensitive-ranges (instructions)
  "Compute value ranges with branch predicate narrowing.
Returns a hash-table mapping (block . reg) to (lo . hi) interval."
  (let* ((cfg (if (cfg-p instructions) instructions (cfg-build instructions)))
         (rpo (progn (cfg-compute-dominators cfg) (cfg-compute-rpo cfg)))
         (in-table (make-hash-table :test #'eq))
         (out-table (make-hash-table :test #'eq))
         (empty-state (make-hash-table :test #'eq)))
    (when (cfg-entry cfg)
      (setf (gethash (cfg-entry cfg) in-table) (%opt-copy-interval-state empty-state)))
    (let ((changed t))
      (loop while changed
            do (setf changed nil)
               (dolist (block rpo)
                 (let* ((incoming (if (eq block (cfg-entry cfg))
                                      (or (gethash block in-table) empty-state)
                                      (%opt-merge-interval-states
                                       (%opt-predecessor-path-states block out-table))))
                        (old-in (gethash block in-table))
                        (out (%opt-cfg-value-ranges-transfer block incoming))
                        (old-out (gethash block out-table)))
                   (unless (and old-in (%opt-interval-state-equal-p old-in incoming))
                     (setf (gethash block in-table) (%opt-copy-interval-state incoming)
                           changed t))
                   (unless (and old-out (%opt-interval-state-equal-p old-out out))
                     (setf (gethash block out-table) out
                           changed t))))))
    (%opt-path-sensitive-entry-table in-table)))

(defun opt-compute-constant-intervals (instructions)
  "Compute a conservative interval map from straight-line constant arithmetic.

Handles vm-const and interval propagation through vm-add/vm-sub/vm-mul when
both operands already have known intervals."
  (opt-compute-value-ranges instructions))

(defun opt-array-bounds-check-eliminable-p (index-reg length-reg intervals &optional block)
  "Return T when INTERVALS prove INDEX-REG is within LENGTH-REG bounds.

INTERVALS may be the classic reg -> interval table, or the path-sensitive
(block . reg) -> interval table returned by OPT-COMPUTE-PATH-SENSITIVE-RANGES
when BLOCK is supplied."
  (flet ((lookup (reg)
           (or (gethash reg intervals)
               (and block (gethash (cons block reg) intervals)))))
    (opt-interval-valid-index-p (lookup index-reg)
                                (lookup length-reg))))

(defstruct (opt-induction-var (:conc-name opt-iv-))
  "Minimal scalar-evolution summary for a single affine induction variable."
  reg
  init
  step
  update-inst)

(defun %opt-constant-reg-value (reg constants)
  "Return (values VALUE T) when REG has a known integer constant in CONSTANTS."
  (multiple-value-bind (value found-p) (gethash reg constants)
    (if (and found-p (integerp value))
        (values value t)
        (values nil nil))))

(defun %opt-simple-induction-step (inst constants)
  "Return (values REG STEP T) when INST updates REG by a constant step."
  (let ((dst (opt-inst-dst inst)))
    (cond
      ((and dst (typep inst 'vm-add))
       (cond
         ((eq dst (vm-lhs inst))
          (multiple-value-bind (c ok) (%opt-constant-reg-value (vm-rhs inst) constants)
            (when (and ok (not (zerop c))) (values dst c t))))
         ((eq dst (vm-rhs inst))
          (multiple-value-bind (c ok) (%opt-constant-reg-value (vm-lhs inst) constants)
            (when (and ok (not (zerop c))) (values dst c t))))))
      ((and dst (typep inst 'vm-sub) (eq dst (vm-lhs inst)))
       (multiple-value-bind (c ok) (%opt-constant-reg-value (vm-rhs inst) constants)
         (when (and ok (not (zerop c))) (values dst (- c) t))))
      ((and dst (typep inst 'vm-inc) (eq dst (vm-src inst)))
       (values dst 1 t))
      ((and dst (typep inst 'vm-dec) (eq dst (vm-src inst)))
       (values dst -1 t)))))

(defun %opt-copy-constant-fact (inst constants)
  "Propagate a constant fact through a vm-move, or kill the destination fact."
  (multiple-value-bind (value found-p) (gethash (vm-src inst) constants)
    (if found-p
        (setf (gethash (vm-dst inst) constants) value)
        (remhash (vm-dst inst) constants))))

(defun %opt-compute-simple-inductions-with-constants (instructions constants)
  "Return simple induction summaries for INSTRUCTIONS seeded by CONSTANTS."
  (let ((constants (%opt-copy-constant-table constants))
        (inductions (make-hash-table :test #'eq)))
    (dolist (inst instructions inductions)
      (cond
        ((typep inst 'vm-const)
         (remhash (vm-dst inst) inductions)
         (if (integerp (vm-value inst))
             (setf (gethash (vm-dst inst) constants) (vm-value inst))
             (remhash (vm-dst inst) constants)))
        ((typep inst 'vm-move)
         (remhash (vm-dst inst) inductions)
         (%opt-copy-constant-fact inst constants))
        (t
         (multiple-value-bind (reg step ok)
             (%opt-simple-induction-step inst constants)
           (if ok
               (multiple-value-bind (init found-p) (gethash reg constants)
                 (if found-p
                     (setf (gethash reg inductions)
                           (make-opt-induction-var :reg reg
                                                   :init init
                                                   :step step
                                                   :update-inst inst))
                     (remhash reg inductions))
                 (remhash reg constants))
               (let ((dst (opt-inst-dst inst)))
                 (when dst
                   (remhash dst constants)
                   (remhash dst inductions))))))))))

(defun opt-compute-simple-inductions (instructions)
  "Return reg -> opt-induction-var summaries for simple affine updates.

  Recognized patterns are intentionally conservative: a register must first hold a
  known integer constant, then be updated by `(add dst dst const)`,
`(add dst const dst)`, `(sub dst dst const)`, `vm-inc`, or `vm-dec`. This
supplies the small SCEV facts needed by loop unrolling, peeling, and
bounds-check reasoning without changing program instructions."
  (%opt-compute-simple-inductions-with-constants
   instructions
   (make-hash-table :test #'eq)))

(defun %opt-copy-constant-table (constants)
  "Return an EQ copy of a reg -> integer constant table."
  (let ((copy (make-hash-table :test #'eq)))
    (when constants
      (maphash (lambda (reg value)
                 (setf (gethash reg copy) value))
               constants))
    copy))

(defun %opt-constant-transfer-inst (inst constants)
  "Conservatively update CONSTANTS for simple constant propagation."
  (cond
    ((typep inst 'vm-const)
     (if (integerp (vm-value inst))
         (setf (gethash (vm-dst inst) constants) (vm-value inst))
         (remhash (vm-dst inst) constants)))
    ((typep inst 'vm-move)
     (%opt-copy-constant-fact inst constants))
    (t
     (let ((dst (opt-inst-dst inst)))
       (when dst (remhash dst constants)))))
  constants)

(defun %opt-loop-member-table (blocks)
  (let ((members (make-hash-table :test #'eq)))
    (dolist (block blocks members)
      (setf (gethash block members) t))))

(defun %opt-blocks-in-rpo-order (blocks)
  (sort (copy-list blocks) #'< :key #'bb-rpo-index))

(defun %opt-blocks-instructions (blocks)
  (loop for block in (%opt-blocks-in-rpo-order blocks)
        append (bb-instructions block)))

(defun %opt-loop-seed-constants (loop-blocks)
  "Collect constants from non-loop predecessors entering LOOP-BLOCKS."
  (let ((members (%opt-loop-member-table loop-blocks))
        (constants (make-hash-table :test #'eq)))
    (dolist (block loop-blocks constants)
      (dolist (pred (bb-predecessors block))
        (unless (gethash pred members)
          (dolist (inst (bb-instructions pred))
            (%opt-constant-transfer-inst inst constants)))))))

(defun opt-compute-loop-inductions (cfg-or-instructions)
  "Return header-block -> (reg -> opt-induction-var) for CFG natural loops.

The analysis uses CFG backedges (`tail -> header` where HEADER dominates TAIL)
and `cfg-collect-natural-loop` to keep induction facts scoped to each loop.
Constants from non-loop predecessor blocks seed the per-loop SCEV scan."
  (let* ((cfg (if (cfg-p cfg-or-instructions)
                  cfg-or-instructions
                  (cfg-build cfg-or-instructions)))
         (result (make-hash-table :test #'eq)))
    (cfg-compute-dominators cfg)
    (cfg-compute-loop-depths cfg)
    (cfg-compute-rpo cfg)
    (loop for tail across (cfg-blocks cfg)
          do (dolist (header (bb-successors tail))
               (when (cfg-dominates-p header tail)
                 (let* ((loop-blocks (cfg-collect-natural-loop header tail))
                        (seed (%opt-loop-seed-constants loop-blocks))
                        (ivs (%opt-compute-simple-inductions-with-constants
                              (%opt-blocks-instructions loop-blocks)
                              seed)))
                   (when (> (hash-table-count ivs) 0)
                     (setf (gethash header result) ivs))))))
    result))

(defun opt-induction-trip-count (init limit step &key inclusive-p predicate)
  "Return a conservative integer trip count for an affine induction variable.

  The loop condition is interpreted as `< limit` for positive STEP and `> limit`
  for negative STEP. With INCLUSIVE-P, the corresponding boundary is `<=` or `>=`.
PREDICATE may be a comparison instruction class/symbol such as `vm-le`, `vm-ge`,
or `vm-eq`; it overrides INCLUSIVE-P when supplied. Returns NIL when STEP is
zero except equality predicates, which are single-test guards."
  (let ((pred (and predicate
                   (if (symbolp predicate) predicate (type-of predicate)))))
    (cond
      ((member pred '(vm-le vm-ge) :test #'eq)
       (return-from opt-induction-trip-count
         (opt-induction-trip-count init limit step :inclusive-p t)))
      ((member pred '(vm-eq vm-num-eq) :test #'eq)
       (return-from opt-induction-trip-count
         (cond
           ((= init limit) 1)
           (t 0))))))
  (cond
    ((zerop step) nil)
    ((plusp step)
     (cond
       ((if inclusive-p (> init limit) (>= init limit)) 0)
       (inclusive-p (1+ (floor (- limit init) step)))
       (t (ceiling (- limit init) step))))
    (t
     (let ((magnitude (- step)))
       (cond
         ((if inclusive-p (< init limit) (<= init limit)) 0)
         (inclusive-p (1+ (floor (- init limit) magnitude)))
         (t (ceiling (- init limit) magnitude)))))))

(defun opt-compute-heap-kinds (instructions)
  "Compute a conservative root -> heap-kind table for TBAA-style checks."
  (let ((points-to (opt-compute-heap-aliases instructions))
        (kinds (make-hash-table :test #'eq)))
    (dolist (inst instructions kinds)
      (let ((dst (opt-inst-dst inst)))
        (when (and dst (opt-heap-root-inst-p inst))
          (let ((root (gethash dst points-to)))
            (when root
              (setf (gethash root kinds) (opt-heap-root-kind inst)))))))
    kinds))

(defun opt-may-alias-by-type-p (reg-a reg-b points-to heap-kinds)
  "Return T if REG-A and REG-B may alias under type-based heap classification.

If both roots and kinds are known and the kinds differ, return NIL. Otherwise
stay conservative and return T."
  (let ((root-a (gethash reg-a points-to))
        (root-b (gethash reg-b points-to)))
    (cond
      ((or (null root-a) (null root-b)) t)
      ((eq root-a root-b) t)
      (t (let ((kind-a (gethash root-a heap-kinds))
               (kind-b (gethash root-b heap-kinds)))
           (if (and kind-a kind-b)
               (eq kind-a kind-b)
               t))))))

(defun opt-must-alias-p (reg-a reg-b alias-roots)
  "Return T when REG-A and REG-B definitely alias under ALIAS-ROOTS."
  (multiple-value-bind (root-a found-a) (gethash reg-a alias-roots)
    (multiple-value-bind (root-b found-b) (gethash reg-b alias-roots)
      (and found-a found-b (eq root-a root-b)))))

(defun opt-may-alias-p (reg-a reg-b alias-roots)
  "Return T when REG-A and REG-B may alias under ALIAS-ROOTS.

Unknown roots remain conservative and therefore return T."
  (multiple-value-bind (root-a found-a) (gethash reg-a alias-roots)
    (multiple-value-bind (root-b found-b) (gethash reg-b alias-roots)
      (or (not found-a)
          (not found-b)
          (eq root-a root-b)))))

(defun opt-slot-alias-key (obj-reg slot-name alias-roots)
  "Return a canonical slot key for OBJ-REG/SLOT-NAME using ALIAS-ROOTS."
  (multiple-value-bind (root found-p) (gethash obj-reg alias-roots)
    (list :slot (if found-p root obj-reg) slot-name)))

(defun opt-rewrite-inst-regs (inst copies)
  "Return INST with all source registers replaced by their canonical copies.
   Uses sexp roundtrip: instruction->sexp rewrites all register-keyword leaves
   except the destination slot (position 1 for instructions with a dst), then
   reconstructs via sexp->instruction.  Falls back to INST unchanged on error."
  (let ((c (lambda (x) (if (opt-register-keyword-p x) (or (gethash x copies) x) x))))
    (handler-case
        (let* ((sexp     (instruction->sexp inst))
               (has-dst  (not (null (opt-inst-dst inst))))
               ;; Rewrite all leaves; for instructions with a dst, the dst sits at
               ;; position 1 (immediately after the opcode tag) — leave it intact.
               (new-sexp (if has-dst
                             (list* (first sexp) (second sexp) (opt-map-tree c (cddr sexp)))
                             (cons  (first sexp) (opt-map-tree c (cdr sexp))))))
          (if (equal sexp new-sexp) inst (sexp->instruction new-sexp)))
      (error () inst))))

;; opt-pass-dead-store-elim and opt-pass-store-to-load-forward are in
;; optimizer-memory-passes.lisp (loaded next).
