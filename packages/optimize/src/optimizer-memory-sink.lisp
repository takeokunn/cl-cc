(in-package :cl-cc/optimize)

;;; ─── Bounds check elimination annotation ──────────────────────────────────

(defun %opt-bce-array-length-regs (instructions)
  "Return a conservative array-reg -> length-reg table for BCE.

Tracks lengths produced by vm-make-array and explicit vm-array-length
instructions. Unknown overwrites kill stale array-length facts."
  (let ((array-lengths (make-hash-table :test #'eq)))
    (dolist (inst instructions array-lengths)
      (typecase inst
        (vm-make-array
         (let ((dst (vm-dst inst)))
           (when dst
             (setf (gethash dst array-lengths) (vm-size-reg inst)))))
        (vm-array-length
         (let ((known (gethash (vm-src inst) array-lengths)))
           ;; Preserve a known allocation size when available; otherwise record
           ;; the explicit length register as the best available fact.
           (setf (gethash (vm-src inst) array-lengths)
                 (or known (vm-dst inst)))))
        (vm-move
         (let ((dst (vm-dst inst)))
           (when dst
             (multiple-value-bind (length-reg found-p)
                 (gethash (vm-src inst) array-lengths)
               (if found-p
                   (setf (gethash dst array-lengths) length-reg)
                   (remhash dst array-lengths))))))
        (t
         (let ((dst (opt-inst-dst inst)))
           (when (and dst (not (typep inst 'vm-array-length)))
             (remhash dst array-lengths))))))))

(defun %opt-bce-inst-array-reg (inst)
  (typecase inst
    ((or vm-aref vm-aset) (vm-array-reg inst))
    (t nil)))

(defun %opt-bce-inst-index-reg (inst)
  (typecase inst
    ((or vm-aref vm-aset) (vm-index-reg inst))
    (t nil)))

(defun %opt-bce-instruction-blocks (cfg)
  "Return an EQ table mapping instructions to their containing basic block."
  (let ((inst->block (make-hash-table :test #'eq)))
    (loop for block across (cfg-blocks cfg)
          do (dolist (inst (bb-instructions block))
               (setf (gethash inst inst->block) block)))
    inst->block))

(defun opt-pass-bounds-check-elimination (instructions)
  "Annotate array accesses whose bounds checks are provably redundant.

This FR-039 pass is intentionally non-rewriting until unchecked VM array
instructions exist. It builds CFG facts, computes dominators and loop depths,
uses path-sensitive integer ranges, and marks proven `vm-aref` / `vm-aset`
instructions with BCE metadata. The returned instruction list is valid and keeps
the original instruction objects/order."
  (let* ((cfg (cfg-build instructions))
         (length-regs (%opt-bce-array-length-regs instructions))
         (inst->block (%opt-bce-instruction-blocks cfg)))
    ;; Clear stale BCE metadata from previous passes.
    (clrhash *opt-bounds-check-eliminable-metadata*)
    (cfg-compute-dominators cfg)
    (cfg-compute-loop-depths cfg)
    (let ((ranges (opt-compute-path-sensitive-ranges cfg)))
      (dolist (inst instructions instructions)
        (when (typep inst '(or vm-aref vm-aset))
          (let* ((array-reg (%opt-bce-inst-array-reg inst))
                 (index-reg (%opt-bce-inst-index-reg inst))
                 (length-reg (and array-reg (gethash array-reg length-regs)))
                 (block (gethash inst inst->block)))
            (when (and index-reg length-reg
                       (opt-array-bounds-check-eliminable-p
                        index-reg length-reg ranges block))
              (opt-mark-bounds-check-eliminable inst
                                                 :array-reg array-reg
                                                 :index-reg index-reg
                                                 :length-reg length-reg
                                                 :block block))))))))

;;; ─── Allocation sinking ───────────────────────────────────────────────────

(defun %opt-sink-allocation-inst-p (inst)
  "Return T for allocations currently safe to sink into a dominated branch."
  (typep inst '(or vm-cons vm-make-array)))

(defun %opt-sink-inst-blocks (cfg)
  (let ((table (make-hash-table :test #'eq)))
    (loop for block across (cfg-blocks cfg)
          when block
          do (dolist (inst (bb-instructions block))
               (setf (gethash inst table) block)))
    table))

(defun %opt-sink-def-blocks (cfg)
  (let ((defs (make-hash-table :test #'eq)))
    (loop for block across (cfg-blocks cfg)
          when block
          do (dolist (inst (bb-instructions block))
               (let ((dst (opt-inst-dst inst)))
                 (when dst
                   (setf (gethash dst defs) block)))))
    defs))

(defun %opt-sink-use-blocks (cfg reg alloc-inst)
  (let ((blocks nil))
    (loop for block across (cfg-blocks cfg)
          when block
          do (dolist (inst (bb-instructions block))
               (unless (eq inst alloc-inst)
                 (when (member reg (opt-inst-read-regs inst) :test #'eq)
                   (pushnew block blocks :test #'eq)))))
    blocks))

(defun %opt-sink-branch-successor-for-uses (def-block use-blocks)
  "Return the unique successor branch containing all USE-BLOCKS, or NIL."
  (let ((candidates nil))
    (when def-block
      (dolist (succ (bb-successors def-block))
        (when (and use-blocks
                   (every (lambda (use-block) (cfg-dominates-p succ use-block))
                          use-blocks))
          (push succ candidates))))
    (when (= (length candidates) 1)
      (first candidates))))

(defun %opt-sink-operands-available-p (inst target-block def-blocks)
  "Return T when INST operands are available at TARGET-BLOCK entry."
  (every (lambda (reg)
           (let ((reg-def (gethash reg def-blocks)))
             (or (null reg-def)
                 (cfg-dominates-p reg-def target-block))))
         (opt-inst-read-regs inst)))

(defun %opt-remove-inst-from-block (block inst)
  (setf (bb-instructions block)
        (remove inst (bb-instructions block) :test #'eq :count 1)))

(defun %opt-insert-inst-at-branch-entry (block inst)
  "Insert INST after any leading safepoints and before first ordinary use."
  (let ((prefix nil)
        (rest (bb-instructions block)))
    (loop while (and rest (opt-safepoint-inst-p (first rest)))
          do (push (pop rest) prefix))
    (setf (bb-instructions block)
          (append (nreverse prefix) (list inst) rest))))

(defun %opt-sink-candidate-p (inst def-block use-blocks target-block cfg def-blocks)
  (and (%opt-sink-allocation-inst-p inst)
       target-block
       (not (eq def-block target-block))
       (every (lambda (use-block) (cfg-dominates-p target-block use-block)) use-blocks)
       (%opt-sink-operands-available-p inst target-block def-blocks)))

(defun opt-sink-allocations (instructions cfg alias-facts)
  "Sink branch-local allocations into the conditional branch that uses them.

Currently handles `vm-cons' and `vm-make-array'.  An allocation is moved from a
branching block into the unique successor branch that dominates all uses of its
destination register.  The pass is intentionally conservative: operands must be
available at the target entry and all uses must remain dominated by the sunk
location.  ALIAS-FACTS is accepted for pipeline integration and future escape
checks; this foundation does not weaken existing alias safety."
  (declare (ignore alias-facts))
  (let* ((graph (or cfg (cfg-build instructions)))
         (inst-blocks (%opt-sink-inst-blocks graph))
         (def-blocks (%opt-sink-def-blocks graph))
         (moved nil))
    (cfg-compute-dominators graph)
    (loop for inst in instructions
          when (%opt-sink-allocation-inst-p inst)
          do (let* ((dst (opt-inst-dst inst))
                    (def-block (gethash inst inst-blocks))
                    (use-blocks (and dst (%opt-sink-use-blocks graph dst inst)))
                    (target (%opt-sink-branch-successor-for-uses def-block use-blocks)))
               (when (and dst def-block
                          (> (length (bb-successors def-block)) 1)
                          (%opt-sink-candidate-p inst def-block use-blocks target graph def-blocks))
                 (%opt-remove-inst-from-block def-block inst)
                 (%opt-insert-inst-at-branch-entry target inst)
                 (push inst moved))))
    (if moved (cfg-flatten graph) instructions)))
