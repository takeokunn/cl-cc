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

(defun opt-compute-constant-intervals (instructions)
  "Compute a conservative interval map from straight-line constant arithmetic.

Handles vm-const and interval propagation through vm-add/vm-sub/vm-mul when
both operands already have known intervals."
  (opt-compute-value-ranges instructions))

(defun opt-array-bounds-check-eliminable-p (index-reg length-reg intervals)
  "Return T when INTERVALS prove INDEX-REG is within LENGTH-REG bounds."
  (opt-interval-valid-index-p (gethash index-reg intervals)
                              (gethash length-reg intervals)))

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
         (when (and ok (not (zerop c))) (values dst (- c) t)))))))

(defun %opt-copy-constant-fact (inst constants)
  "Propagate a constant fact through a vm-move, or kill the destination fact."
  (multiple-value-bind (value found-p) (gethash (vm-src inst) constants)
    (if found-p
        (setf (gethash (vm-dst inst) constants) value)
        (remhash (vm-dst inst) constants))))

(defun opt-compute-simple-inductions (instructions)
  "Return reg -> opt-induction-var summaries for simple affine updates.

Recognized patterns are intentionally conservative: a register must first hold a
known integer constant, then be updated by `(add dst dst const)`,
`(add dst const dst)`, or `(sub dst dst const)`. This supplies the small SCEV
facts needed by loop unrolling, peeling, and bounds-check reasoning without
changing program instructions."
  (let ((constants (make-hash-table :test #'eq))
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

(defun opt-induction-trip-count (init limit step &key inclusive-p)
  "Return a conservative integer trip count for an affine induction variable.

The loop condition is interpreted as `< limit` for positive STEP and `> limit`
for negative STEP. With INCLUSIVE-P, the corresponding boundary is `<=` or `>=`.
Returns NIL when STEP is zero."
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
