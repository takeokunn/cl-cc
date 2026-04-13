(in-package :cl-cc)
;;; ─── Pass 2: Dead Code Elimination ──────────────────────────────────────

(defun opt-pass-dce (instructions)
  "Dead code elimination via global usedness analysis.
   Pass 1: collect every register that appears as a source operand anywhere
   in the program into a 'used' set (ignoring control flow).
   Pass 2: remove pure instructions (vm-const, vm-move) whose destination
   register never appears in the used set.
   This is safe across branches/labels: a register defined in both branches
   is preserved as long as it is read anywhere -- the linear-order issue that
   plagued the previous backward-liveness DCE is entirely avoided."
  (let ((used (make-hash-table :test #'eq)))
    ;; Pass 1: mark every register that is read by any instruction
    (dolist (inst instructions)
      (dolist (reg (opt-inst-read-regs inst))
        (setf (gethash reg used) t)))
    ;; Pass 2: drop DCE-eligible instructions (pure + alloc) whose dst is never read
    (remove-if (lambda (inst)
                 (and (opt-inst-dce-eligible-p inst)
                      (let ((dst (ignore-errors (vm-dst inst))))
                        (and dst (not (gethash dst used))))))
               instructions)))

;;; ─── Pass 3: Jump Threading + Dead Jump Elimination ─────────────────────

(defun opt-build-label-index (instructions)
  "Return (values vector label→index-ht) for threading analysis."
  (let ((vec (coerce instructions 'vector))
        (idx (make-hash-table :test #'equal)))
    (loop for i from 0 below (length vec)
          when (vm-label-p (aref vec i))
          do (setf (gethash (vm-name (aref vec i)) idx) i))
    (values vec idx)))

(defun opt-thread-label (label idx vec &optional (seen (make-hash-table :test #'equal)))
  "Follow jump chains starting at LABEL. Returns the ultimate jump target label.

   Cycle detection prevents infinite loops in pathological (cyclic) code while
   still allowing long but valid jump chains to fully thread."
  (when (gethash label seen)
    (return-from opt-thread-label label))
  (setf (gethash label seen) t)
  (let ((pos (gethash label idx)))
    (unless pos (return-from opt-thread-label label))
    ;; Scan forward past any labels to find the first real instruction
    (loop for i from pos below (length vec)
          for inst = (aref vec i)
          do (typecase inst
                (vm-label nil) ; skip label markers
                (vm-jump ; found a chained jump → follow it
                 (let ((next (vm-label-name inst)))
                   (return-from opt-thread-label
                     (opt-thread-label next idx vec seen))))
                (t (return-from opt-thread-label label)))))
  label)

(defun opt-falls-through-to-p (vec i target)
  "T if scanning forward from position I+1 we reach TARGET before any non-label."
  (loop for j from (1+ i) below (length vec)
        for inst = (aref vec j)
        if (not (vm-label-p inst)) return nil
        if (equal (vm-name inst) target) return t
        finally (return nil)))

(defun opt-pass-jump (instructions)
  "Thread jump chains and remove jumps to the immediately following label."
  (multiple-value-bind (vec idx) (opt-build-label-index instructions)
    (let ((result nil)
          (n (length vec)))
      (loop for i from 0 below n
            for inst = (aref vec i)
            do (typecase inst
                 (vm-jump
                  (let ((threaded (opt-thread-label (vm-label-name inst) idx vec)))
                    (unless (opt-falls-through-to-p vec i threaded)
                      (push (if (equal threaded (vm-label-name inst))
                                inst
                                (make-vm-jump :label threaded))
                            result))))
                 (vm-jump-zero
                  (let ((threaded (opt-thread-label (vm-label-name inst) idx vec)))
                    (push (if (equal threaded (vm-label-name inst))
                              inst
                              (make-vm-jump-zero :reg (vm-reg inst) :label threaded))
                          result)))
                 (t (push inst result))))
      (nreverse result))))

;;; ─── Pass 4: Unreachable Code Elimination ────────────────────────────────

(defun opt-pass-unreachable (instructions)
  "Remove instructions that follow unconditional control transfers (jump/ret)
   and precede the next label — they can never be executed."
  (let ((result nil)
        (dead nil))
    (dolist (inst instructions)
      (typecase inst
        (vm-label
         (setf dead nil)
         (push inst result))
        (t
         (unless dead
           (push inst result))
         ;; Mark subsequent instructions as unreachable after unconditional transfer
          (when (or (vm-jump-p inst) (vm-ret-p inst))
            (setf dead t)))))
    (nreverse result)))

(defun opt-pass-dead-basic-blocks (instructions)
  "Eliminate unreachable basic blocks by round-tripping through the CFG.
   Reachability is computed from the entry block; unreachable blocks and their
   instructions are dropped when the CFG is flattened back to a linear list."
  (cfg-flatten (cfg-build instructions)))

(defun opt-pass-dominated-type-check-elim (instructions)
  "Eliminate redundant pure type predicates dominated by an earlier identical
    predicate on the same source register. Nil checks via vm-not are treated
    the same way. The first check is kept; later checks are replaced with
    vm-move from the dominating result register."
  (let ((cfg (cfg-build instructions)))
    (when (cfg-entry cfg)
      (cfg-compute-dominators cfg)
      (labels ((copy-facts (facts)
                 (copy-list facts))
               (forget-def (facts reg)
                 (remove-if (lambda (fact)
                              (or (eq (getf fact :src) reg)
                                  (eq (getf fact :dst) reg)))
                            facts))
               (lookup-fact (facts pred src)
                 (find-if (lambda (fact)
                            (and (eq (getf fact :pred) pred)
                                 (eq (getf fact :src) src)))
                          facts))
               (process-block (block facts)
                 (let ((local-facts (copy-facts facts))
                       (new-insts nil))
                    (dolist (inst (bb-instructions block))
                      (let ((dst (opt-inst-dst inst)))
                        (when dst
                          (setf local-facts (forget-def local-facts dst))))
                      (cond
                        ((and (or (opt-foldable-type-pred-p inst)
                                  (typep inst 'vm-not))
                              (vm-src inst)
                              (vm-dst inst))
                         (let* ((pred (type-of inst))
                                (src  (vm-src inst))
                                (dst  (vm-dst inst))
                                (fact (lookup-fact local-facts pred src)))
                          (if (and fact (not (eq dst (getf fact :dst))))
                              (push (make-vm-move :dst dst :src (getf fact :dst)) new-insts)
                              (progn
                                (push inst new-insts)
                                (push (list :pred pred :src src :dst dst) local-facts)))))
                       (t
                        (push inst new-insts))))
                   (setf (bb-instructions block) (nreverse new-insts))
                   (dolist (child (bb-dom-children block))
                     (process-block child local-facts)))))
         (process-block (cfg-entry cfg) nil)))
     (cfg-flatten cfg)))

(defun opt-pass-nil-check-elim (instructions)
  "Eliminate redundant nil checks dominated by an earlier identical check.

   This is a thin specialization of dominated type-check elimination so FR-040
   exists as a named optimizer pass in the pipeline."
  (opt-pass-dominated-type-check-elim instructions))

(defun %opt-branch-predicate-fact-for-block (block)
  "Return a branch fact plist for BLOCK, or NIL.

The fact is inferred only when BLOCK has a single predecessor ending in a
vm-jump-zero whose condition register is defined by a foldable type predicate
or vm-not in that predecessor block. The returned plist contains:
  :pred  instruction type
  :src   predicate source register
  :value replacement constant (1 on fallthrough, 0 on taken branch)."
  (when (= 1 (length (bb-predecessors block)))
    (let* ((pred (first (bb-predecessors block)))
           (term (car (last (bb-instructions pred))))
           (target-label (and (typep term 'vm-jump-zero)
                              (vm-label-name term)))
           (block-label (and (bb-label block) (vm-name (bb-label block)))))
      (when target-label
        (let ((cond-reg (vm-reg term)))
          (loop for inst in (reverse (bb-instructions pred))
                do (let ((dst (opt-inst-dst inst)))
                     (when (eq dst cond-reg)
                       (return
                         (when (or (opt-foldable-type-pred-p inst)
                                   (typep inst 'vm-not))
                           (list :pred (type-of inst)
                                 :src (vm-src inst)
                                 :value (if (and block-label
                                                 (equal block-label target-label))
                                            0
                                            1))))))))))))

(defun opt-pass-branch-correlation (instructions)
  "Propagate known predicate outcomes from a dominating conditional edge.

This is a conservative FR-168 style pass: when a block has exactly one
predecessor ending in vm-jump-zero over a foldable predicate, repeated tests of
the same predicate on the same source register inside the successor block are
replaced with vm-const 1/0."
  (let ((cfg (cfg-build instructions)))
    (loop for block across (cfg-blocks cfg)
          do (let ((fact (%opt-branch-predicate-fact-for-block block)))
               (when fact
                 (let ((live-fact fact)
                       (new-insts nil))
                   (dolist (inst (bb-instructions block))
                     (let ((dst (opt-inst-dst inst)))
                       (when (and live-fact dst
                                  (eq dst (getf live-fact :src)))
                         (setf live-fact nil)))
                     (cond
                       ((and live-fact
                             (or (opt-foldable-type-pred-p inst)
                                 (typep inst 'vm-not))
                             (eq (type-of inst) (getf live-fact :pred))
                             (eq (vm-src inst) (getf live-fact :src))
                             (opt-inst-dst inst))
                        (push (make-vm-const :dst (opt-inst-dst inst)
                                             :value (getf live-fact :value))
                              new-insts))
                       (t
                        (push inst new-insts))))
                   (setf (bb-instructions block) (nreverse new-insts))))))
    (cfg-flatten cfg)))

(defun opt-pass-block-merge (instructions)
  "Merge linear CFG chains where a block has exactly one successor and that
   successor has exactly one predecessor. This removes redundant labels/jumps
   on straight-line code paths without changing branching structure."
  (let ((cfg (cfg-build instructions)))
    (labels ((mergeable-successor-p (block)
               (let ((succs (bb-successors block)))
                 (and (= (length succs) 1)
                      (let ((succ (first succs)))
                        (and (= (length (bb-predecessors succ)) 1)
                             (eq (first (bb-predecessors succ)) block))))))
             (strip-merge-jump (insts target-label)
               (if (and insts
                        (vm-jump-p (car (last insts)))
                        (equal (vm-label-name (car (last insts))) target-label))
                   (butlast insts)
                   insts))
            (emit-block (block visited suppress-label)
                (when (or (null block) (gethash block visited))
                  (return-from emit-block nil))
                (setf (gethash block visited) t)
                (let ((result nil))
                 (unless suppress-label
                   (when (bb-label block)
                     (push (bb-label block) result)))
                 (let ((insts (bb-instructions block)))
                   (if (mergeable-successor-p block)
                       (let* ((succ (first (bb-successors block)))
                              (succ-label (and (bb-label succ)
                                               (vm-name (bb-label succ)))))
                         (setf insts (if succ-label
                                         (strip-merge-jump insts succ-label)
                                         insts))
                         (setf result (nconc result insts))
                         (setf result (nconc result (emit-block succ visited t))))
                       (progn
                         (setf result (nconc result insts))
                         (dolist (succ (bb-successors block))
                           (setf result (nconc result (emit-block succ visited nil)))))))
                result)))
        (if (cfg-entry cfg)
            (emit-block (cfg-entry cfg) (make-hash-table :test #'eq) nil)
            instructions))))

(defun opt-pass-tail-merge (instructions)
  "Merge CFG blocks with identical bodies and identical successor labels.

   This is a conservative tail-merging pass: it only merges whole basic blocks
   whose instruction sequences and outgoing edges are exactly the same. That
   keeps the transformation safe while still removing duplicated block tails."
  (let ((cfg (cfg-build instructions)))
    (when (cfg-entry cfg)
      (labels ((succ-labels (block)
                 (mapcar (lambda (succ)
                           (and (bb-label succ) (vm-name (bb-label succ))))
                         (bb-successors block)))
               (block-signature (block)
                 (list (mapcar #'instruction->sexp (bb-instructions block))
                       (succ-labels block)))
               (replace-successor (block old new)
                 (setf (bb-successors block)
                       (mapcar (lambda (succ) (if (eq succ old) new succ))
                               (bb-successors block))))
                (rewrite-terminator (block old-label new-label)
                  (let ((cell (last (bb-instructions block))))
                    (when cell
                      (let ((last (car cell)))
                        (cond
                          ((and (typep last 'vm-jump)
                                (equal (vm-label-name last) old-label))
                           (setf (car cell) (make-vm-jump :label new-label)))
                          ((and (typep last 'vm-jump-zero)
                                (equal (vm-label-name last) old-label))
                           (setf (car cell)
                                 (make-vm-jump-zero :reg (vm-reg last)
                                                    :label new-label))))))))
               (merge-duplicate-blocks ()
                 (let ((canonical-by-sig (make-hash-table :test #'equal)))
                    (dolist (block (coerce (cfg-blocks cfg) 'list))
                     (let ((label (and (bb-label block) (vm-name (bb-label block)))))
                       (when label
                         (let* ((sig (block-signature block))
                                (canon (gethash sig canonical-by-sig)))
                           (if (null canon)
                               (setf (gethash sig canonical-by-sig) block)
                               (unless (eq canon block)
                                 (let* ((canon-label (and (bb-label canon)
                                                          (vm-name (bb-label canon))))
                                        (block-label label))
                                   (when (and canon-label block-label)
                                     (dolist (pred (copy-list (bb-predecessors block)))
                                       (replace-successor pred block canon)
                                       (rewrite-terminator pred block-label canon-label)
                                       (pushnew pred (bb-predecessors canon) :test #'eq))
                                     (setf (bb-predecessors block) nil))))))))))))
        (merge-duplicate-blocks)
        (cfg-flatten cfg)))))
