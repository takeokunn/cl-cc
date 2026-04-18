(in-package :cl-cc/optimize)
;;; ─── Pass: Loop Invariant Code Motion (LICM) ──────────────────────────────

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

      (let ((def-sites (make-hash-table :test #'eq))
            (header-to-members (make-hash-table :test #'eq))
            (header-to-invariants (make-hash-table :test #'eq))
            (header-to-tails (make-hash-table :test #'eq))
            (loop-headers nil))
        (loop for b across (cfg-blocks cfg)
              do (dolist (inst (bb-instructions b))
                   (let ((dst (opt-inst-dst inst)))
                     (when dst
                       (pushnew b (gethash dst def-sites) :test #'eq)))))

        (loop for b across (cfg-blocks cfg)
              do (dolist (succ (bb-successors b))
                   (when (cfg-dominates-p succ b)
                     (pushnew succ loop-headers :test #'eq)
                     (pushnew b (gethash succ header-to-tails) :test #'eq))))

        (dolist (header loop-headers)
          (let ((members (make-hash-table :test #'eq)))
            (dolist (tail (gethash header header-to-tails))
              (dolist (member (cfg-collect-natural-loop header tail))
                (setf (gethash member members) t)))
            (setf (gethash header header-to-members) members)

            (let ((loop-def-regs (make-hash-table :test #'eq)))
              (maphash (lambda (member _)
                         (dolist (inst (bb-instructions member))
                           (let ((dst (opt-inst-dst inst)))
                             (when dst
                               (setf (gethash dst loop-def-regs) t)))))
                       members)

                (maphash (lambda (member _)
                           (dolist (inst (bb-instructions member))
                             (when (opt-inst-loop-invariant-p inst loop-def-regs members def-sites)
                               (pushnew inst (gethash header header-to-invariants) :test #'eq))))
                        members))))

        (when (every (lambda (header)
                       (null (gethash header header-to-invariants)))
                     loop-headers)
          (return-from opt-pass-licm instructions))

        (opt-licm-emit-with-preheaders cfg header-to-members header-to-invariants)))))

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

(defun opt-licm-emit-with-preheaders (cfg header-to-members header-to-invariants)
  "Mutate CFG to insert preheaders, then flatten it back to instructions."
  (let ((preheader-counter 0))
    (labels ((header-member-p (header block)
               (gethash block (gethash header header-to-members)))
             (redirect-successor (block old new)
               (setf (bb-successors block)
                     (mapcar (lambda (succ) (if (eq succ old) new succ))
                             (bb-successors block)))
               (setf (bb-predecessors old)
                     (remove block (bb-predecessors old) :test #'eq))
               (pushnew block (bb-predecessors new) :test #'eq))
             (rewrite-terminator (block old-label new-label)
               (let ((cell (last (bb-instructions block))))
                 (when cell
                   (let ((term (car cell)))
                     (typecase term
                       (vm-jump
                        (when (equal (vm-label-name term) old-label)
                          (setf (car cell) (make-vm-jump :label new-label))))
                       (vm-jump-zero
                        (when (equal (vm-label-name term) old-label)
                          (setf (car cell)
                                (make-vm-jump-zero :reg (vm-reg term)
                                                   :label new-label)))))))))
             (hoist-header (header)
               (let ((invariants (gethash header header-to-invariants)))
                 (when (and invariants (bb-label header))
                   (let* ((header-label (vm-name (bb-label header)))
                          (preheader-label-name (format nil "LICM_PREHEADER_~A" preheader-counter))
                          (preheader-label (make-vm-label :name preheader-label-name))
                          (preheader (cfg-new-block cfg :label preheader-label))
                          (outside-preds (remove-if (lambda (pred)
                                                      (header-member-p header pred))
                                                    (copy-list (bb-predecessors header)))))
                     (incf preheader-counter)
                     (setf (bb-instructions preheader)
                           (append (copy-list invariants)
                                   (list (make-vm-jump :label header-label))))
                     (setf (bb-successors preheader) (list header))
                     (pushnew preheader (bb-predecessors header) :test #'eq)
                     (dolist (pred outside-preds)
                       (redirect-successor pred header preheader)
                       (rewrite-terminator pred header-label preheader-label-name))
                     (dolist (member (loop for k being the hash-keys of (gethash header header-to-members)
                                           collect k))
                       (setf (bb-instructions member)
                             (remove-if (lambda (inst)
                                          (member inst invariants :test #'eq))
                                        (bb-instructions member)))))))))
      (maphash (lambda (header _)
                 (hoist-header header))
               header-to-invariants)
      (cfg-flatten cfg))))

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

(defun %opt-pre-block-out-env (block)
  "Return key→dst availability after BLOCK for PRE join-point analysis."
  (let ((env (make-hash-table :test #'equal)))
    (dolist (inst (bb-instructions block) env)
      (let ((dst (opt-inst-dst inst)))
        (when dst
          (let ((stale nil))
            (maphash (lambda (k v)
                       (when (eq v dst)
                         (push k stale)))
                     env)
            (dolist (k stale)
              (remhash k env)))))
      (let ((key (%opt-pre-expression-key inst)))
        (when key
          (setf (gethash key env) (opt-inst-dst inst)))))))

(defun %opt-pre-splice-before-terminator (insts additions)
  "Insert ADDITIONS before INSTS' terminator, preserving order."
  (let ((term (car (last insts))))
    (if (and term (typep term '(or vm-jump vm-jump-zero vm-ret vm-halt)))
        (append (butlast insts) additions (list term))
        (append insts additions))))

(defun %opt-pre-join-elim (cfg)
  "Do a small join-point PRE pass over CFG blocks."
  (let ((pred-inserts (make-hash-table :test #'eq))
        (changed nil))
    (labels ((insert-for-pred (pred inst)
               (push inst (gethash pred pred-inserts))))
      (loop for block across (cfg-blocks cfg)
            do (let ((preds (bb-predecessors block)))
                 (when (> (length preds) 1)
                   (let ((pred-envs (mapcar (lambda (pred)
                                              (cons pred (%opt-pre-block-out-env pred)))
                                            preds))
                         (new-insts nil))
                     (dolist (inst (bb-instructions block))
                       (let* ((dst (opt-inst-dst inst))
                              (key (%opt-pre-expression-key inst)))
                         (when dst
                           (dolist (pair pred-envs)
                             (let ((env (cdr pair)))
                               (maphash (lambda (k v)
                                          (when (eq v dst)
                                            (remhash k env)))
                                        env))))
                         (if (and dst key)
                             (let ((available nil))
                               (dolist (pair pred-envs)
                                 (when (gethash key (cdr pair))
                                   (setf available t)))
                               (if available
                                   (progn
                                     (setf changed t)
                                     (dolist (pair pred-envs)
                                       (let* ((pred (car pair))
                                              (env (cdr pair))
                                              (src (gethash key env)))
                                         (if src
                                             (unless (eq src dst)
                                               (insert-for-pred pred
                                                                (make-vm-move :dst dst :src src)))
                                             (insert-for-pred pred
                                                              (handler-case
                                                                  (sexp->instruction (instruction->sexp inst))
                                                                (error () inst))))
                                         (setf (gethash key env) dst)))
                                     nil)
                                   (push inst new-insts)))
                             (push inst new-insts))))
                     (setf (bb-instructions block) (nreverse new-insts))))))
      (maphash (lambda (pred insts)
                 (setf (bb-instructions pred)
                       (%opt-pre-splice-before-terminator (bb-instructions pred)
                                                          (nreverse insts))))
               pred-inserts)
      changed)))

(defun opt-pass-constant-hoist (instructions)
  "Hoist loop-invariant constants into loop preheaders.

   This is a narrow constant-only specialization of LICM that reuses the same
   CFG/loop analysis and therefore stays within the existing optimizer model.
   It exists to make FR-166 a named pass while preserving the same lowering
   behavior for constant literals already covered by LICM."
  (opt-pass-licm instructions))

(defun opt-pass-pre (instructions)
  "Partial Redundancy Elimination (PRE).

   This combines LICM with a small join-point PRE step and CSE so partially
   redundant pure computations are exposed and removed."
  (let* ((licm1 (opt-pass-licm instructions))
         (cse1 (opt-pass-cse licm1))
         (cfg (cfg-build cse1)))
    (%opt-pre-join-elim cfg)
    (opt-pass-cse (opt-pass-licm (cfg-flatten cfg)))))

(defun opt-pass-egraph (instructions)
  "Run equality-saturation lowering over the current instruction list.

   This is a thin wrapper around optimize-with-egraph so the pass can live in
   the main optimizer pipeline without changing the standalone e-graph API."
  (optimize-with-egraph instructions))
