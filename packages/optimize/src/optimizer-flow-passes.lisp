;;;; packages/optimize/src/optimizer-flow-passes.lisp — Advanced CFG optimization passes
;;;
;;; Extracted from optimizer-flow.lisp.
;;; Contains: branch-correlation, block-merge, tail-merge.
;;;
;;; Depends on optimizer-flow.lisp (opt-pass-dominated-type-check-elim,
;;;   opt-foldable-type-pred-p, opt-inst-dst, cfg-build, cfg-flatten, bb-*).
;;; Load order: immediately after optimizer-flow.lisp.

(in-package :cl-cc/optimize)

(defun %opt-branch-correlation-forwarder-block-p (block)
  "Return T when BLOCK is a trivial forwarder (single unconditional jump)."
  (let ((insts (bb-instructions block)))
    (and (= (length insts) 1)
         (typep (first insts) 'vm-jump))))

(defun %opt-branch-predicate-fact-from-edge (pred edge-label &optional (seen (make-hash-table :test #'eq)))
  "Return branch fact seen on PRED -> EDGE-LABEL, recursively through forwarders.

EDGE-LABEL is the successor label on the edge being analyzed."
  (when (gethash pred seen)
    (return-from %opt-branch-predicate-fact-from-edge nil))
  (setf (gethash pred seen) t)
  (let* ((term (car (last (bb-instructions pred))))
         (target-label (and (typep term 'vm-jump-zero)
                            (vm-label-name term))))
    (cond
      (target-label
       (let ((cond-reg (vm-reg term)))
         (loop for inst in (reverse (bb-instructions pred))
               do (let ((dst (opt-inst-dst inst)))
                    (when (eq dst cond-reg)
                      (return
                        (when (or (opt-foldable-type-pred-p inst)
                                  (typep inst 'vm-not))
                          (list :pred (type-of inst)
                                :src (vm-src inst)
                                :value (if (and edge-label
                                                (equal edge-label target-label))
                                           0
                                           1)))))))))
      ((and (typep term 'vm-jump)
            (%opt-branch-correlation-forwarder-block-p pred)
            (= (length (bb-predecessors pred)) 1)
            (equal (vm-label-name term) edge-label)
            (bb-label pred))
       (%opt-branch-predicate-fact-from-edge
        (first (bb-predecessors pred))
        (vm-name (bb-label pred))
        seen))
      (t nil))))

(defun %opt-branch-predicate-fact-from-predecessor (pred block)
  "Return the branch fact carried on edge PRED -> BLOCK, or NIL."
  (let ((block-label (and (bb-label block) (vm-name (bb-label block)))))
    (%opt-branch-predicate-fact-from-edge pred block-label)))

(defun %opt-same-branch-fact-p (a b)
  "Return T when branch facts A and B prove the same replacement value."
  (and a b
       (eq (getf a :pred) (getf b :pred))
       (eq (getf a :src) (getf b :src))
       (eql (getf a :value) (getf b :value))))

(defun %opt-branch-predicate-fact-for-block (block)
  "Return a branch fact plist for BLOCK, or NIL.

Each predecessor edge must carry the same predicate fact. This preserves the
old single-predecessor behavior and extends FR-168 to simple joins where all
incoming edges agree on the predicate outcome. The returned plist contains:
  :pred  instruction type
  :src   predicate source register
  :value replacement constant (1 on fallthrough, 0 on taken branch)."
  (let ((preds (bb-predecessors block)))
    (when preds
      (let ((facts (mapcar (lambda (pred)
                             (%opt-branch-predicate-fact-from-predecessor pred block))
                           preds)))
        (when (and (every #'identity facts)
                   (every (lambda (fact)
                            (%opt-same-branch-fact-p fact (first facts)))
                          (rest facts)))
          (first facts))))))

(defun opt-pass-branch-correlation (instructions)
  "Propagate known predicate outcomes from a dominating conditional edge.

This is a conservative FR-168 style pass: when every predecessor edge into a
block carries the same vm-jump-zero fact over a foldable predicate, repeated
tests of the same predicate on the same source register inside the successor
block are replaced with vm-const 1/0."
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

;;; ─── Block merge helpers ──────────────────────────────────────────────────

(defun %block-mergeable-successor-p (block)
  "Return T when BLOCK has exactly one successor that has BLOCK as its sole predecessor."
  (let ((succs (bb-successors block)))
    (and (= (length succs) 1)
         (let ((succ (first succs)))
           (and (= (length (bb-predecessors succ)) 1)
                (eq (first (bb-predecessors succ)) block))))))

(defun %block-strip-merge-jump (insts target-label)
  "Remove the trailing vm-jump to TARGET-LABEL from INSTS when merging blocks."
  (if (and insts
           (vm-jump-p (car (last insts)))
           (equal (vm-label-name (car (last insts))) target-label))
      (butlast insts)
      insts))

(defun %block-merge-emit (block visited suppress-label)
  "Recursively emit BLOCK and its mergeable successors into a flat instruction list."
  (when (or (null block) (gethash block visited))
    (return-from %block-merge-emit nil))
  (setf (gethash block visited) t)
  (let ((result nil))
    (unless suppress-label
      (when (bb-label block)
        (push (bb-label block) result)))
    (let ((insts (bb-instructions block)))
      (if (%block-mergeable-successor-p block)
          (let* ((succ       (first (bb-successors block)))
                 (succ-label (and (bb-label succ) (vm-name (bb-label succ)))))
            (setf insts (if succ-label
                            (%block-strip-merge-jump insts succ-label)
                            insts))
            (setf result (nconc result insts))
            (setf result (nconc result (%block-merge-emit succ visited t))))
          (progn
            (setf result (nconc result insts))
            (dolist (succ (bb-successors block))
              (setf result (nconc result (%block-merge-emit succ visited nil)))))))
    result))

(defun opt-pass-block-merge (instructions)
  "Merge linear CFG chains where a block has exactly one successor and that
   successor has exactly one predecessor. This removes redundant labels/jumps
   on straight-line code paths without changing branching structure."
  (let ((cfg (cfg-build instructions)))
    (if (cfg-entry cfg)
        (%block-merge-emit (cfg-entry cfg) (make-hash-table :test #'eq) nil)
        instructions)))

;;; ─── Tail merge helpers ───────────────────────────────────────────────────

(defun %tail-merge-succ-labels (block)
  "Return list of successor label names for BLOCK."
  (mapcar (lambda (succ) (and (bb-label succ) (vm-name (bb-label succ))))
          (bb-successors block)))

(defun %tail-merge-block-signature (block)
  "Return a structural equality key for BLOCK: (instruction-sexps successor-labels)."
  (list (mapcar #'instruction->sexp (bb-instructions block))
        (%tail-merge-succ-labels block)))

(defun %tail-merge-replace-successor (block old new)
  "Replace the OLD successor of BLOCK with NEW."
  (setf (bb-successors block)
        (mapcar (lambda (succ) (if (eq succ old) new succ))
                (bb-successors block))))

(defun %tail-merge-merge-duplicates (cfg)
  "Merge duplicate labeled blocks in CFG in-place, rewiring predecessors."
  (let ((canonical-by-sig (make-hash-table :test #'equal)))
    (dolist (block (coerce (cfg-blocks cfg) 'list))
      (let ((label (and (bb-label block) (vm-name (bb-label block)))))
        (when label
          (let* ((sig   (%tail-merge-block-signature block))
                 (canon (gethash sig canonical-by-sig)))
            (if (null canon)
                (setf (gethash sig canonical-by-sig) block)
                (unless (eq canon block)
                  (let ((canon-label (and (bb-label canon) (vm-name (bb-label canon)))))
                    (when (and canon-label label)
                      (dolist (pred (copy-list (bb-predecessors block)))
                        (%tail-merge-replace-successor pred block canon)
                        (%opt-rewrite-block-terminator pred label canon-label)
                        (pushnew pred (bb-predecessors canon) :test #'eq))
                      (setf (bb-predecessors block) nil)))))))))))

(defun opt-pass-tail-merge (instructions)
  "Merge CFG blocks with identical bodies and identical successor labels.

   This is a conservative tail-merging pass: it only merges whole basic blocks
   whose instruction sequences and outgoing edges are exactly the same. That
   keeps the transformation safe while still removing duplicated block tails."
  (let ((cfg (cfg-build instructions)))
    (when (cfg-entry cfg)
      (%tail-merge-merge-duplicates cfg)
      (cfg-flatten cfg))))

;;; ─── Tail duplication (conservative FR-167 subset) ───────────────────────

(defparameter *tail-dup-max-insts* 3
  "Maximum number of tail-block instructions duplicated into a predecessor.")

(defun %tail-dup-terminator (block)
  "Return BLOCK's last instruction, or NIL."
  (car (last (bb-instructions block))))

(defun %tail-dup-candidate-p (succ)
  "Return T when SUCC is a small tail block safe for conservative duplication."
  (let ((insts (bb-instructions succ)))
    (and insts
         (<= (length insts) *tail-dup-max-insts*)
         (notany #'vm-label-p insts)
         (or (vm-ret-p (%tail-dup-terminator succ))
             (vm-jump-p (%tail-dup-terminator succ))
             (vm-jump-zero-p (%tail-dup-terminator succ))))))

(defun %tail-dup-rewrite-pred (pred succ)
  "Duplicate SUCC instructions into PRED by replacing trailing jump-to-SUCC."
  (let* ((pred-insts (bb-instructions pred))
         (term (car (last pred-insts)))
         (succ-label (and (bb-label succ) (vm-name (bb-label succ)))))
    (when (and succ-label
               pred-insts
               (typep term 'vm-jump)
               (equal (vm-label-name term) succ-label))
      (setf (bb-instructions pred)
            (nconc (butlast pred-insts)
                   (copy-list (bb-instructions succ))))
      t)))

(defun %tail-dup-apply (cfg)
  "Apply conservative tail duplication on CFG. Returns T if changed."
  (let ((changed nil))
    (loop for succ across (cfg-blocks cfg)
          do (when (and (> (length (bb-predecessors succ)) 1)
                        (%tail-dup-candidate-p succ))
               (dolist (pred (bb-predecessors succ))
                 (when (%tail-dup-rewrite-pred pred succ)
                   (setf changed t)))))
    changed))

(defun opt-pass-tail-duplication (instructions)
  "Conservative tail duplication pass (FR-167 subset).

Duplicates small tail blocks into predecessors when the predecessor ends with an
unconditional jump to that block, reducing branch overhead and exposing follow-up
threading/merge opportunities."
  (let ((cfg (cfg-build instructions)))
    (%tail-dup-apply cfg)
    (cfg-flatten (cfg-build (cfg-flatten cfg)))))
