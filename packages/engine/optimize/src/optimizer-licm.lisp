(in-package :cl-cc/optimize)
;;; ─── Pass: Loop Invariant Code Motion (LICM) ──────────────────────────────

;;; ─── LICM helper: collect definition sites ────────────────────────────────

(defun %licm-collect-def-sites (cfg)
  "Return a register → list-of-blocks table for all registers defined in CFG."
  (let ((def-sites (make-hash-table :test #'eq)))
    (loop for b across (cfg-blocks cfg)
          do (dolist (inst (bb-instructions b))
               (let ((dst (opt-inst-dst inst)))
                 (when dst
                   (pushnew b (gethash dst def-sites) :test #'eq)))))
    def-sites))

;;; ─── LICM helper: find loop headers ──────────────────────────────────────

(defun %licm-find-loop-headers (cfg)
  "Return (values loop-headers header-to-tails-ht) for all natural loop headers in CFG."
  (let ((header-to-tails (make-hash-table :test #'eq))
        (headers nil))
    (loop for b across (cfg-blocks cfg)
          do (dolist (succ (bb-successors b))
               (when (cfg-dominates-p succ b)
                 (pushnew succ headers :test #'eq)
                 (pushnew b (gethash succ header-to-tails) :test #'eq))))
    (values headers header-to-tails)))

;;; ─── LICM helper: collect loop member blocks ──────────────────────────────

(defun %licm-collect-members (header tails)
  "Return hash-table block → t for the natural loop (HEADER, TAILS)."
  (let ((members (make-hash-table :test #'eq)))
    (dolist (tail tails)
      (dolist (member (cfg-collect-natural-loop header tail))
        (setf (gethash member members) t)))
    members))

;;; ─── LICM helper: collect registers defined inside the loop ──────────────

(defun %licm-loop-def-regs (members)
  "Return set of registers defined inside MEMBERS (hash-table block → t)."
  (let ((regs (make-hash-table :test #'eq)))
    (maphash (lambda (member _)
               (dolist (inst (bb-instructions member))
                 (let ((dst (opt-inst-dst inst)))
                   (when dst (setf (gethash dst regs) t)))))
             members)
    regs))

;;; ─── LICM helper: collect invariant instructions ─────────────────────────

(defun %licm-collect-invariants (members def-sites)
  "Return list of loop-invariant instructions across all MEMBERS blocks."
  (let ((loop-def-regs (%licm-loop-def-regs members))
        (invariants nil))
    (maphash (lambda (member _)
               (dolist (inst (bb-instructions member))
                 (when (opt-inst-loop-invariant-p inst loop-def-regs members def-sites)
                   (pushnew inst invariants :test #'eq))))
             members)
    invariants))

;;; ─── LICM: invariance predicate ───────────────────────────────────────────

(defun opt-inst-loop-invariant-p (inst loop-def-regs loop-members def-sites)
  "Return T if INST is loop-invariant:
     1. Pure (no observable side effects)
     2. All read registers are defined outside the loop"
  ;; Must be pure to be safely hoisted
  (unless (opt-inst-pure-p inst)
    (return-from opt-inst-loop-invariant-p nil))

  ;; Check all read registers
  (let ((reads (opt-inst-read-regs inst)))
    (dolist (reg reads t)
      ;; If reg is defined inside the loop, not invariant
      (when (gethash reg loop-def-regs)
        (return-from opt-inst-loop-invariant-p nil))
      ;; If all known definitions of REG are inside the loop, it is not invariant.
      (let ((sites (gethash reg def-sites)))
        (when (and sites
                   (every (lambda (block)
                            (gethash block loop-members))
                          sites))
          (return-from opt-inst-loop-invariant-p nil))))))

;;; ─── LICM: preheader insertion helpers ───────────────────────────────────

(defun %licm-redirect-successor (block old new)
  "Redirect BLOCK's successor edge from OLD to NEW."
  (setf (bb-successors block)
        (mapcar (lambda (succ) (if (eq succ old) new succ))
                (bb-successors block)))
  (setf (bb-predecessors old)
        (remove block (bb-predecessors old) :test #'eq))
  (pushnew block (bb-predecessors new) :test #'eq))

(defun %licm-hoist-one-header (cfg header invariants members-ht counter)
  "Insert a preheader block before HEADER, hoisting INVARIANTS into it.
   Returns the new counter value."
  (if (and invariants (bb-label header))
      (let* ((header-label    (vm-name (bb-label header)))
             (preheader-name  (format nil "LICM_PREHEADER_~A" counter))
             (preheader-label (make-vm-label :name preheader-name))
             (preheader       (cfg-new-block cfg :label preheader-label))
             (outside-preds   (remove-if (lambda (pred) (gethash pred members-ht))
                                         (copy-list (bb-predecessors header)))))
        (setf (bb-instructions preheader)
              (append (copy-list invariants)
                      (list (make-vm-jump :label header-label)))
              (bb-successors preheader) (list header))
        (pushnew preheader (bb-predecessors header) :test #'eq)
        (dolist (pred outside-preds)
          (%licm-redirect-successor pred header preheader)
          (%opt-rewrite-block-terminator pred header-label preheader-name))
        (let ((all-members (loop for k being the hash-keys of members-ht collect k)))
          (dolist (member all-members)
            (setf (bb-instructions member)
                  (remove-if (lambda (inst) (member inst invariants :test #'eq))
                             (bb-instructions member)))))
        (1+ counter))
      counter))

;;; ─── LICM: preheader emission ─────────────────────────────────────────────

(defun opt-licm-emit-with-preheaders (cfg header-to-members header-to-invariants)
  "Insert preheaders for all loops with invariants, then flatten CFG back to instructions."
  (let ((counter 0))
    (maphash (lambda (header invariants)
               (setf counter
                     (%licm-hoist-one-header cfg header invariants
                                             (gethash header header-to-members)
                                             counter)))
             header-to-invariants)
    (cfg-flatten cfg)))

;;; ─── Pass entry point ─────────────────────────────────────────────────────

(defun opt-pass-licm (instructions)
  "Loop Invariant Code Motion: hoist loop-invariant instructions to preheaders.

   Algorithm:
     1. Build CFG and compute loop depths via cfg-compute-loop-depths
     2. Identify loop headers (blocks with back-edge predecessors)
       3. For each loop, collect invariant instructions:
         - Pure (opt-inst-pure-p)
         - All operand registers defined outside the loop
     4. Create preheader block before each loop header
     5. Move invariant instructions to preheader

   This pass activates the dormant bb-loop-depth field computed by cfg.lisp.
   It is FR-003 from docs/optimize-passes.md."
  (when (null instructions)
    (return-from opt-pass-licm instructions))
  (let ((cfg (cfg-build instructions)))
    (cfg-compute-dominators cfg)
    (cfg-compute-loop-depths cfg)
    (let ((loop-blocks (remove-if (lambda (b) (= (bb-loop-depth b) 0))
                                  (coerce (cfg-blocks cfg) 'list))))
      (when (null loop-blocks)
        (return-from opt-pass-licm instructions))
      (let* ((def-sites (%licm-collect-def-sites cfg))
             (header-to-members (make-hash-table :test #'eq))
             (header-to-invariants (make-hash-table :test #'eq)))
        (multiple-value-bind (loop-headers header-to-tails)
            (%licm-find-loop-headers cfg)
          (dolist (header loop-headers)
            (let ((members (%licm-collect-members header (gethash header header-to-tails))))
              (setf (gethash header header-to-members) members
                    (gethash header header-to-invariants)
                    (%licm-collect-invariants members def-sites))))
          (when (every (lambda (header)
                         (null (gethash header header-to-invariants)))
                       loop-headers)
            (return-from opt-pass-licm instructions))
          (opt-licm-emit-with-preheaders cfg header-to-members header-to-invariants))))))

;;; ─── PRE support helpers ──────────────────────────────────────────────────

(defun %opt-pre-expression-key (inst)
  "Return a structural key for a pure instruction INST, or NIL."
  (when (opt-inst-pure-p inst)
    (typecase inst
      (vm-const (list :const (vm-value inst)))
      (t
       (let* ((reads (opt-inst-read-regs inst))
              (normalized (if (member (type-of inst) *opt-commutative-inst-types* :test #'eq)
                              (sort (copy-list reads)
                                    #'string<
                                    :key #'prin1-to-string)
                              reads)))
         (and normalized (cons (type-of inst) normalized)))))))

(defun %opt-pre-env-evict-dst (env dst)
  "Remove all entries in ENV whose value is DST (dst was overwritten)."
  (let ((stale nil))
    (maphash (lambda (k v) (when (eq v dst) (push k stale))) env)
    (dolist (k stale) (remhash k env))))

(defun %opt-pre-block-out-env (block)
  "Return key→dst availability after BLOCK for PRE join-point analysis."
  (let ((env (make-hash-table :test #'equal)))
    (dolist (inst (bb-instructions block) env)
      (let ((dst (opt-inst-dst inst)))
        (when dst (%opt-pre-env-evict-dst env dst)))
      (let ((key (%opt-pre-expression-key inst)))
        (when key
          (setf (gethash key env) (opt-inst-dst inst)))))))

(defun %opt-pre-splice-before-terminator (insts additions)
  "Insert ADDITIONS before INSTS' terminator, preserving order."
  (let ((term (car (last insts))))
    (if (and term (typep term '(or vm-jump vm-jump-zero vm-ret vm-halt)))
        (append (butlast insts) additions (list term))
        (append insts additions))))

(defun %opt-pre-reconstruct-inst (inst)
  "Round-trip INST through sexp form, returning INST unchanged on error."
  (handler-case (sexp->instruction (instruction->sexp inst))
    (error () inst)))

(defun %opt-pre-available-in-any-p (key pred-envs)
  "Return T if KEY is available in at least one predecessor environment."
  (some (lambda (pair) (gethash key (cdr pair))) pred-envs))

(defun %opt-pre-emit-compensating (pair key dst inst pred-inserts)
  "Emit a compensating instruction into PAIR's predecessor block, updating its availability env."
  (let* ((pred (car pair))
         (env  (cdr pair))
         (src  (gethash key env)))
    (when (or (null src) (not (eq src dst)))
      (push (if src
                (make-vm-move :dst dst :src src)
                (%opt-pre-reconstruct-inst inst))
            (gethash pred pred-inserts)))
    (setf (gethash key env) dst)))

(defun %opt-pre-join-elim (cfg)
  "Do a small join-point PRE pass over CFG blocks."
  (let ((pred-inserts (make-hash-table :test #'eq))
        (changed nil))
    (loop for block across (cfg-blocks cfg)
          do (when (> (length (bb-predecessors block)) 1)
               (let ((pred-envs (mapcar (lambda (pred)
                                          (cons pred (%opt-pre-block-out-env pred)))
                                        (bb-predecessors block)))
                     (new-insts nil))
                 (dolist (inst (bb-instructions block))
                   (let* ((dst (opt-inst-dst inst))
                          (key (%opt-pre-expression-key inst)))
                     (when dst
                       (dolist (pair pred-envs)
                         (%opt-pre-env-evict-dst (cdr pair) dst)))
                     (if (and dst key (%opt-pre-available-in-any-p key pred-envs))
                         (progn
                           (setf changed t)
                           (dolist (pair pred-envs)
                             (%opt-pre-emit-compensating pair key dst inst pred-inserts)))
                         (push inst new-insts))))
                 (setf (bb-instructions block) (nreverse new-insts)))))
    (maphash (lambda (pred insts)
               (setf (bb-instructions pred)
                     (%opt-pre-splice-before-terminator (bb-instructions pred)
                                                        (nreverse insts))))
             pred-inserts)
    changed))

(defun opt-pass-pre (instructions)
  "Partial Redundancy Elimination (PRE).

   This combines LICM with a small join-point PRE step and CSE so partially
   redundant pure computations are exposed and removed."
  (let* ((licm1 (opt-pass-licm instructions))
         (cse1 (opt-pass-cse licm1))
         (cfg (cfg-build cse1)))
    (%opt-pre-join-elim cfg)
    (opt-pass-cse (opt-pass-licm (cfg-flatten cfg)))))

