(in-package :cl-cc/optimize)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Optimizer — CSE, GVN, Dead Label Elimination, Leaf Function Detection
;;;
;;; Contains: opt-pass-cse (common subexpression elimination via generation-
;;; numbered value numbering), %gvn-process-block / opt-pass-gvn (global
;;; value numbering over the dominator tree), opt-pass-dead-labels (removes
;;; unreferenced labels), *opt-leaf-call-types* + opt-pass-leaf-detect.
;;;
;;; Load order: after optimizer-strength.lisp, before optimizer-licm.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── CSE state struct and helpers ───────────────────────────────────────────

(defstruct (cse-state (:conc-name cse-))
  "Mutable state threaded through the CSE pass."
  (gen     (make-hash-table :test #'eq)    :type hash-table) ; reg → generation
  (val-env (make-hash-table :test #'eq)    :type hash-table) ; reg → canonical key
  (memo    (make-hash-table :test #'equal) :type hash-table)) ; key → canonical reg

(defun %cse-get-val (state reg)
  "Canonical value key for REG: stored value if known, else (reg . gen)."
  (or (gethash reg (cse-val-env state))
      (cons reg (or (gethash reg (cse-gen state)) 0))))

(defun %cse-bump-gen (state dst)
  "Overwrite DST: evict stale val-env/memo entries and bump the generation."
  (let ((old-val (gethash dst (cse-val-env state))))
    (when old-val
      (when (eq (gethash old-val (cse-memo state)) dst)
        (remhash old-val (cse-memo state)))
      (remhash dst (cse-val-env state))))
  (setf (gethash dst (cse-gen state))
        (1+ (or (gethash dst (cse-gen state)) 0))))

(defun %cse-record (state dst key)
  "Associate DST as the canonical register for KEY."
  (setf (gethash dst (cse-val-env state)) key)
  (unless (gethash key (cse-memo state))
    (setf (gethash key (cse-memo state)) dst)))

(defun %cse-try-find (state key)
  "Return the canonical register for KEY if memoized, else NIL."
  (gethash key (cse-memo state)))

(defun %cse-flush (state)
  "Clear all value-numbering state (called at label boundaries)."
  (clrhash (cse-gen state))
  (clrhash (cse-val-env state))
  (clrhash (cse-memo state)))

;;; ─── Pass: Common Subexpression Elimination ──────────────────────────────

(defun %cse-emit-or-cse (inst dst key state result)
  "Emit INST or replace with vm-move if KEY is already in STATE.
   Returns updated RESULT list (head = most recently emitted, not yet reversed)."
  (let ((existing (%cse-try-find state key)))
    (if existing
        (progn (%cse-bump-gen state dst)
               (setf (gethash dst (cse-val-env state)) key)
               (cons (make-vm-move :dst dst :src existing) result))
        (progn (%cse-bump-gen state dst)
               (%cse-record state dst key)
               (cons inst result)))))

(defun opt-pass-cse (instructions)
  "Common subexpression elimination via generation-numbered value numbering.
   Replaces redundant computations with vm-move from the first computing register.
   At vm-label boundaries, all knowledge is conservatively flushed."
  (let ((state         (make-cse-state))
        (target-labels (%opt-branch-target-labels instructions))
        (result        nil))
    (dolist (inst instructions)
      (typecase inst
        (vm-label
         (when (gethash (vm-name inst) target-labels)
           (%cse-flush state))
         (push inst result))
        (vm-const
         ;; Never replace vm-const with vm-move: doing so creates a
         ;; fold<->CSE oscillation that DCE can turn into a dangling
         ;; register reference (first canonical reg gets removed by
         ;; DCE while later moves still point to it, leaving the
         ;; register uninitialized = 0 instead of NIL).
         ;; The fold pass already propagates constant values; CSE is
         ;; only needed for computed expressions.
         (let ((dst (vm-dst inst))
               (key (list :const (vm-value inst))))
           (%cse-bump-gen state dst)
           (%cse-record state dst key)
           (push inst result)))
        (vm-move
         (let* ((dst     (vm-move-dst inst))
                (src-val (%cse-get-val state (vm-move-src inst))))
           (%cse-bump-gen state dst)
           (setf (gethash dst (cse-val-env state)) src-val)
           (push inst result)))
        (t
         (cond
           ((opt-binary-lhs-rhs-p inst)
            (let* ((dst (vm-dst inst))
                   (lv  (%cse-get-val state (vm-lhs inst)))
                   (rv  (%cse-get-val state (vm-rhs inst)))
                   (op  (type-of inst))
                   (key (if (%opt-commutative-inst-p inst)
                            (list op (if (%opt-value< lv rv) lv rv)
                                     (if (%opt-value< lv rv) rv lv))
                            (list op lv rv))))
              (setf result (%cse-emit-or-cse inst dst key state result))))
           ((opt-unary-src-p inst)
            (let* ((dst (vm-dst inst))
                   (sv  (%cse-get-val state (vm-src inst)))
                   (key (list (type-of inst) sv)))
              (setf result (%cse-emit-or-cse inst dst key state result))))
           (t
            (let ((dst (opt-inst-dst inst)))
              (when dst (%cse-bump-gen state dst)))
            (push inst result))))))
    (nreverse result)))

;;; ─── GVN helpers ─────────────────────────────────────────────────────────

(defun %gvn-copy-env (env &key (test #'eq))
  "Return a shallow copy of hash-table ENV using TEST."
  (let ((copy (make-hash-table :test test)))
    (maphash (lambda (k v) (setf (gethash k copy) v)) env)
    copy))

(defun %gvn-kill (reg gen val-env memo)
  "Evict REG from the GVN value environment and bump its generation."
  (remhash reg val-env)
  (maphash (lambda (k v) (when (eq v reg) (remhash k memo))) memo)
  (setf (gethash reg gen) (1+ (or (gethash reg gen) 0))))

(defun %gvn-get-val (reg gen val-env)
  "Return the canonical value for REG: from VAL-ENV or (reg . generation)."
  (or (gethash reg val-env)
      (cons reg (or (gethash reg gen) 0))))

(defun %gvn-record (dst key gen val-env memo)
  "Bind DST to KEY in the GVN value environment and update generation."
  (setf (gethash dst val-env) key)
  (unless (gethash key memo)
    (setf (gethash key memo) dst))
  (setf (gethash dst gen) (1+ (or (gethash dst gen) 0))))

(defun %gvn-key (inst gen val-env)
  "Compute the canonical GVN key for INST using current GEN/VAL-ENV."
  (let ((reads (opt-inst-read-regs inst)))
    (typecase inst
      (vm-const (list :const (vm-value inst)))
      (vm-move  (%gvn-get-val (vm-src inst) gen val-env))
      (t
       (when (and (opt-inst-pure-p inst) reads)
         (let ((vals (mapcar (lambda (reg) (%gvn-get-val reg gen val-env)) reads)))
           (if (%opt-commutative-inst-p inst)
                (destructuring-bind (a b) vals
                  (if (%opt-value< a b)
                      (list (type-of inst) a b)
                      (list (type-of inst) b a)))
                (cons (type-of inst) vals))))))))

;;; ─── Conservative Global CSE via Available Expressions ────────────────────

(defun %gcse-expression-key (inst)
  "Return the exact available-expression key tracked for INST, or NIL.

This intentionally stays conservative:
  - only effect-proven CSE-eligible instructions participate;
  - vm-move is handled as value propagation, not as an expression rewrite;
  - operand order must match exactly, mirroring the available-expression helper."
  (let ((reads (opt-inst-read-regs inst)))
    (and (opt-inst-cse-eligible-p inst)
         (opt-inst-dst inst)
         reads
         (not (typep inst 'vm-move))
         (cons (type-of inst) (copy-list reads)))))

(defun %gcse-key-mentions-reg-p (key reg)
  "Return T when KEY's operand list mentions REG."
  (member reg (cdr key) :test #'eq))

(defun %gcse-record-expression-if-safe (env key dst)
  "Record KEY → DST only when overwriting DST does not invalidate KEY syntax."
  (unless (%gcse-key-mentions-reg-p key dst)
    (setf (gethash key env) dst))
  env)

(defun %gcse-kill-dst (env dst)
  "Remove ENV entries invalidated by overwriting DST."
  (let ((stale nil))
    (maphash (lambda (key holder)
               (when (or (eq holder dst)
                         (member dst (cdr key) :test #'eq))
                 (push key stale)))
             env)
    (dolist (key stale)
      (remhash key env)))
  env)

(defun %gcse-forward-copy (env src dst)
  "Forward all ENV-held values currently rooted in SRC into DST."
  (let ((copied nil))
    (maphash (lambda (key holder)
               (when (eq holder src)
                 (push key copied)))
             env)
    (dolist (key copied)
      (setf (gethash key env) dst)))
  env)

(defun %gcse-seed-env (block available reaching)
  "Seed BLOCK's key→register environment from AVAILABLE and REACHING results.

An expression is only seeded when every reaching definition that realizes the
available expression agrees on a single destination register.  This avoids
unsafe join-point reuse when different predecessor registers hold the same
value."
  (let ((env (make-hash-table :test #'equal))
        (available-in (gethash block (opt-dataflow-result-in available)))
        (reaching-in (gethash block (opt-dataflow-result-in reaching))))
    (dolist (entry available-in env)
      (let* ((key (%available-expression-entry-key entry))
             (regs (remove-duplicates
                    (loop for definition in reaching-in
                          for inst = (fourth definition)
                          for def-key = (%gcse-expression-key inst)
                          when (and def-key (equal def-key key))
                            collect (%reaching-definition-reg definition))
                    :test #'eq)))
        (when (= (length regs) 1)
          (setf (gethash key env) (first regs)))))))

(defun %gcse-process-block (block available reaching)
  "Rewrite redundant pure expressions in BLOCK using global entry facts."
  (let ((env (%gcse-seed-env block available reaching))
        (changed nil)
        (new-insts nil))
    (dolist (inst (bb-instructions block))
      (let* ((dst (opt-inst-dst inst))
             (key (%gcse-expression-key inst))
             (existing (and key (gethash key env))))
        (when dst
          (%gcse-kill-dst env dst))
        (cond
          ((and key existing (not (eq existing dst)))
            (push (make-vm-move :dst dst :src existing) new-insts)
            (%gcse-record-expression-if-safe env key dst)
            (setf changed t))
          (t
            (push inst new-insts)
            (cond
              ((and dst key)
               (%gcse-record-expression-if-safe env key dst))
              ((and dst (typep inst 'vm-move))
               (%gcse-forward-copy env (vm-src inst) dst)))))))
    (setf (bb-instructions block) (nreverse new-insts))
    changed))

(defun %gcse-apply-to-cfg (cfg)
  "Apply conservative available-expression-based GCSE to CFG.

This is intentionally narrower than full PRE / global value numbering: it only
reuses pure expressions already available at block entry, and only when the
reaching definitions prove that a single register name carries the value on all
incoming paths."
  (let ((available (opt-compute-available-expressions cfg))
        (reaching (opt-compute-reaching-definitions cfg))
        (changed nil))
    (loop for block across (cfg-blocks cfg)
          when block
            do (when (%gcse-process-block block available reaching)
                 (setf changed t)))
    changed))

(defun opt-pass-global-cse (instructions)
  "Conservative global common-subexpression elimination for simple CFG regions.

The pass uses available-expressions and reaching-definitions analyses to reuse
pure values at block entry.  It deliberately avoids PRE-style compensation,
memory reads, calls, allocations, and any case where different incoming paths
would require different source registers."
  (let ((cfg (cfg-build instructions)))
    (%gcse-apply-to-cfg cfg)
    (cfg-flatten cfg)))

;;; ─── Pass: Global Value Numbering ────────────────────────────────────────

(defun %gvn-process-block (block gen val-env memo)
  "Process BLOCK for global value numbering using GEN/VAL-ENV/MEMO state."
  (let ((local-gen     (%gvn-copy-env gen))
        (local-val-env (%gvn-copy-env val-env))
        (local-memo    (%gvn-copy-env memo :test #'equal))
        (new-insts nil))
    (dolist (inst (bb-instructions block))
      (let ((dst (opt-inst-dst inst)))
        (when dst
          (%gvn-kill dst local-gen local-val-env local-memo)))
      (typecase inst
        (vm-const
         (let* ((dst (vm-dst inst))
                (key (%gvn-key inst local-gen local-val-env)))
           (%gvn-record dst key local-gen local-val-env local-memo)
           (push inst new-insts)))
        (vm-move
         (let* ((dst (vm-dst inst))
                (key (%gvn-key inst local-gen local-val-env))
                (existing (gethash key local-memo)))
           (if existing
               (progn
                 (%gvn-record dst key local-gen local-val-env local-memo)
                 (push (make-vm-move :dst dst :src existing) new-insts))
               (progn
                 (%gvn-record dst key local-gen local-val-env local-memo)
                 (push inst new-insts)))))
        (t
         (cond
           ((and (opt-inst-pure-p inst) (opt-inst-dst inst))
            (let* ((dst (opt-inst-dst inst))
                   (key (%gvn-key inst local-gen local-val-env))
                   (existing (and key (gethash key local-memo))))
              (if existing
                  (progn
                    (%gvn-record dst key local-gen local-val-env local-memo)
                    (push (make-vm-move :dst dst :src existing) new-insts))
                  (progn
                    (when key
                      (%gvn-record dst key local-gen local-val-env local-memo))
                    (push inst new-insts)))))
           (t
            (push inst new-insts))))))
    (setf (bb-instructions block) (nreverse new-insts))
    (dolist (child (bb-dom-children block))
      (%gvn-process-block child local-gen local-val-env local-memo))))

(defun opt-pass-gvn (instructions)
  "Conservative global value numbering over the dominator tree.

   The pass first applies a narrow FR-518-style global CSE step using
   available-expressions / reaching-definitions at block entries, then performs
   dominator-tree value numbering. Unlike opt-pass-cse, knowledge is propagated
   into dominated basic blocks instead of being flushed at every label."
  (let ((cfg (cfg-build (opt-pass-global-cse instructions))))
    (when (cfg-entry cfg)
      (cfg-compute-dominators cfg)
      (%gvn-process-block (cfg-entry cfg)
                          (make-hash-table :test #'eq)
                          (make-hash-table :test #'eq)
                          (make-hash-table :test #'eq))
      (cfg-flatten-hot-cold cfg))))

;;; ─── Unused Label Elimination ────────────────────────────────────────────

(defun opt-pass-dead-labels (instructions)
  "Remove labels that are not referenced by any jump or closure instruction."
  (let ((used (make-hash-table :test #'equal)))
    ;; Collect all label references: jumps AND closure body labels
    (dolist (inst instructions)
      (let ((accessor (gethash (type-of inst) *opt-label-ref-table*)))
        (when accessor
          (setf (gethash (funcall accessor inst) used) t))))
    ;; Filter out unreferenced labels
      (remove-if (lambda (inst)
                   (and (vm-label-p inst)
                        (not (gethash (vm-name inst) used))))
                instructions)))

;;; ─── Leaf Function Detection ────────────────────────────────────────────

(defparameter *opt-leaf-call-types*
  '(vm-call vm-generic-call vm-tail-call vm-apply vm-call-next-method)
  "Instruction types that disqualify a function from being a leaf function.")

(defun opt-pass-leaf-detect (instructions)
  "Return INSTRUCTIONS unchanged and a leaf-function flag.

   A program is a leaf function when it contains no call-like instructions."
  (let ((leaf-p (not (some (lambda (inst)
                             (some (lambda (call-type)
                                     (typep inst call-type))
                                   *opt-leaf-call-types*))
                           instructions))))
    (values instructions leaf-p)))
