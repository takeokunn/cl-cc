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

   Unlike opt-pass-cse, knowledge is propagated into dominated basic blocks
   instead of being flushed at every label. This lets a computation in a
   dominator block replace an equivalent computation in a dominated block,
   while still avoiding cross-join speculation."
  (let ((cfg (cfg-build instructions)))
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
