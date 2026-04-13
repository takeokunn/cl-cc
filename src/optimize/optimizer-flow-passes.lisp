;;;; src/optimize/optimizer-flow-passes.lisp — Advanced CFG optimization passes
;;;
;;; Extracted from optimizer-flow.lisp.
;;; Contains: nil-check-elim, branch-correlation, block-merge, tail-merge.
;;;
;;; Depends on optimizer-flow.lisp (opt-pass-dominated-type-check-elim,
;;;   opt-foldable-type-pred-p, opt-inst-dst, cfg-build, cfg-flatten, bb-*).
;;; Load order: immediately after optimizer-flow.lisp.

(in-package :cl-cc)

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
