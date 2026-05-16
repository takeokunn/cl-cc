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

(defparameter *opt-tail-dup-max-instructions* 12
  "Maximum number of tail-block instructions duplicated into a predecessor.")

(defparameter *tail-dup-max-insts* *opt-tail-dup-max-instructions*
  "Compatibility alias for the old tail-duplication instruction budget.")

(defun %tail-dup-terminator (block)
  "Return BLOCK's last instruction, or NIL."
  (car (last (bb-instructions block))))

(defun %tail-dup-candidate-p (succ)
  "Return T when SUCC is a tail block safe for duplication."
  (let ((insts (bb-instructions succ)))
    (and insts
         (<= (length insts) *opt-tail-dup-max-instructions*)
         (notany #'vm-label-p insts)
         (or (vm-ret-p (%tail-dup-terminator succ))
             (vm-jump-p (%tail-dup-terminator succ))
             (typep (%tail-dup-terminator succ) 'vm-jump-zero)))))

(defun %tail-dup-copy-insts (succ)
  "Return structural copies of SUCC's instructions."
  (mapcar #'%tail-dup-copy-inst (bb-instructions succ)))

(defun %tail-dup-copy-inst (inst)
  "Return a structural copy of INST for tail duplication."
  (handler-case
      (sexp->instruction (instruction->sexp inst))
    (error () inst)))

(defun %tail-dup-frequent-p (succ)
  "Return T when SUCC is worth considering under the loop-depth heuristic."
  (not (%cfg-block-cold-p succ)))

(defun %tail-dup-beneficial-p (pred succ)
  "Return T when duplicating SUCC into PRED removes at least one jump."
  (let* ((term (car (last (bb-instructions pred))))
         (succ-label (and (bb-label succ) (vm-name (bb-label succ)))))
    (and succ-label
         (or (and (typep term 'vm-jump)
                  (equal (vm-label-name term) succ-label))
             (and (typep term 'vm-jump-zero)
                  (equal (vm-label-name term) succ-label))))))

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
                    (%tail-dup-copy-insts succ)))
      t)))

(defun %tail-dup-split-conditional-target-edge (cfg pred succ)
  "Create an edge pad for a conditional PRED branch to SUCC."
  (let* ((term (car (last (bb-instructions pred))))
         (succ-label (%cfg-ensure-label succ cfg "TAIL_DUP_TARGET_")))
    (when (and (typep term 'vm-jump-zero)
               (equal (vm-label-name term) (vm-name succ-label)))
      (let ((pad (%cfg-split-edge cfg pred succ succ-label)))
        (%cfg-replace-terminator pred term
                                 (make-vm-jump-zero :reg (vm-reg term)
                                                    :label (vm-name (bb-label pad))))
        pad))))

(defun %tail-dup-rewrite-conditional-pred (cfg pred succ)
  "Duplicate SUCC through an edge pad for a conditional predecessor."
  (let ((pad (%tail-dup-split-conditional-target-edge cfg pred succ)))
    (when pad
      (setf (bb-instructions pad) (%tail-dup-copy-insts succ)
            (bb-successors pad) (copy-list (bb-successors succ)))
      (dolist (old-succ (bb-successors succ))
        (pushnew pad (bb-predecessors old-succ) :test #'eq))
      t)))

(defun %tail-dup-apply (cfg)
  "Apply CFG-based tail duplication on CFG. Returns T if changed."
  (let ((changed nil))
    (cfg-compute-dominators cfg)
    (cfg-compute-loop-depths cfg)
    (loop for succ across (cfg-blocks cfg)
          do (when (and (> (length (bb-predecessors succ)) 1)
                        (%tail-dup-frequent-p succ)
                        (%tail-dup-candidate-p succ))
               (dolist (pred (copy-list (bb-predecessors succ)))
                 (when (%tail-dup-beneficial-p pred succ)
                   (when (or (%tail-dup-rewrite-pred pred succ)
                             (%tail-dup-rewrite-conditional-pred cfg pred succ))
                     (setf changed t))))))
    changed))

(defun %tail-dup-linear-candidates (cfg)
  "Return an alist of label-name to tail instruction copies for CFG candidates."
  (let ((candidates nil))
    (cfg-compute-dominators cfg)
    (cfg-compute-loop-depths cfg)
    (loop for succ across (cfg-blocks cfg)
          for label = (and (bb-label succ) (vm-name (bb-label succ)))
          when (and label
                    (> (length (bb-predecessors succ)) 1)
                    (%tail-dup-frequent-p succ)
                    (%tail-dup-candidate-p succ))
            do (push (cons label (bb-instructions succ)) candidates))
    candidates))

(defun %tail-dup-fresh-label-name (counter)
  "Return a fresh tail-duplication label name."
  (format nil "TAIL_DUP_~D" counter))

(defun %tail-dup-linear (instructions candidates)
  "Apply tail duplication to INSTRUCTIONS while preserving linear layout."
  (let ((insertions (make-hash-table :test #'equal))
        (counter 0)
        (out nil)
        (changed nil))
    (dolist (inst instructions)
      (cond
        ((and (typep inst 'vm-jump)
              (assoc (vm-label-name inst) candidates :test #'equal))
         (dolist (copy (mapcar #'%tail-dup-copy-inst
                               (cdr (assoc (vm-label-name inst) candidates :test #'equal))))
           (push copy out))
         (setf changed t))
        ((and (typep inst 'vm-jump-zero)
              (assoc (vm-label-name inst) candidates :test #'equal))
         (let* ((target (vm-label-name inst))
                (tail (cdr (assoc target candidates :test #'equal)))
                (fresh (%tail-dup-fresh-label-name (incf counter))))
           (push (make-vm-jump-zero :reg (vm-reg inst) :label fresh) out)
           (push (cons (make-vm-label :name fresh)
                       (mapcar #'%tail-dup-copy-inst tail))
                 (gethash target insertions))
           (setf changed t)))
        ((and (vm-label-p inst) (gethash (vm-name inst) insertions))
         (dolist (entry (nreverse (gethash (vm-name inst) insertions)))
           (push (car entry) out)
           (dolist (copy (cdr entry))
             (push copy out)))
         (push inst out))
        (t
         (push inst out))))
    (values (nreverse out) changed)))

(defun opt-pass-tail-duplication (instructions)
  "Duplicate profitable shared tail blocks into CFG predecessors.

The pass considers multi-predecessor tail blocks up to
*OPT-TAIL-DUP-MAX-INSTRUCTIONS*, handles unconditional predecessors directly,
  and handles conditional taken edges by inserting an edge pad before duplicating.
Duplication is only applied when it removes at least one jump."
  (let* ((cfg (cfg-build instructions))
         (candidates (%tail-dup-linear-candidates cfg)))
    (multiple-value-bind (out changed)
        (%tail-dup-linear instructions candidates)
      (if changed out (cfg-flatten cfg)))))
