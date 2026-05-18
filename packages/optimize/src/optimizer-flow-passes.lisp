;;;; packages/optimize/src/optimizer-flow-passes.lisp — Advanced CFG optimization passes
;;;
;;; Extracted from optimizer-flow-core.lisp.
;;; Contains: branch-correlation, block-merge, tail-merge.
;;;
;;; Depends on optimizer-flow-core.lisp (opt-pass-dominated-type-check-elim,
;;;   opt-foldable-type-pred-p, opt-inst-dst, cfg-build, cfg-flatten, bb-*).
;;; Load order: immediately after optimizer-flow-core.lisp.

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

(in-package :cl-cc/optimize)
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

(defun %opt-rewrite-block-terminator (block old-label new-label)
  "Rewrite the jump terminator of BLOCK from OLD-LABEL to NEW-LABEL when it matches.
Handles both vm-jump (unconditional) and vm-jump-zero (conditional)."
  (let ((cell (last (bb-instructions block))))
    (when cell
      (let ((term (car cell)))
        (when (equal (vm-label-name term) old-label)
          (setf (car cell)
                (typecase term
                  (vm-jump
                   (make-vm-jump :label new-label))
                  (vm-jump-zero
                   (make-vm-jump-zero :reg (vm-reg term) :label new-label))
                  (t (return-from %opt-rewrite-block-terminator)))))))))

(defun opt-falls-through-to-p (vec i target)
  "T if scanning forward from position I+1 we reach TARGET before any non-label."
  (loop for j from (1+ i) below (length vec)
        for inst = (aref vec j)
        if (not (vm-label-p inst)) return nil
        if (equal (vm-name inst) target) return t
        finally (return nil)))

(defun %opt-thread-jump (inst vec i idx)
  "Thread unconditional INST; return the (possibly relabeled) jump, or NIL if it falls through."
  (let ((threaded (opt-thread-label (vm-label-name inst) idx vec)))
    (unless (opt-falls-through-to-p vec i threaded)
      (if (equal threaded (vm-label-name inst))
          inst
          (make-vm-jump :label threaded)))))

(defun %opt-thread-jump-zero (inst vec i idx)
  "Thread conditional INST; always return an instruction (never elide conditionals)."
  (declare (ignore i))
  (let ((threaded (opt-thread-label (vm-label-name inst) idx vec)))
    (if (equal threaded (vm-label-name inst))
        inst
        (make-vm-jump-zero :reg (vm-reg inst) :label threaded))))

(defparameter *opt-jump-thread-table*
  (list (cons 'vm-jump      #'%opt-thread-jump)
        (cons 'vm-jump-zero #'%opt-thread-jump-zero))
  "Maps jump instruction types to their threading handlers.")

(defun opt-pass-jump (instructions)
  "Thread jump chains, remove fallthrough jumps, and propagate edge facts.

The value-propagation part is deliberately conservative: it only carries facts
derived from vm-jump-zero conditions whose comparison operands are both known
constants in the predecessor block, only into single-predecessor successors,
and never across CFG back-edges."
  (opt-pass-jump-threading-with-propagation instructions))

(defun %opt-pass-jump-chain-only (instructions)
  "Thread jump chains and remove jumps to the immediately following label."
  (multiple-value-bind (vec idx) (opt-build-label-index instructions)
    (let ((result nil)
          (n (length vec)))
      (loop for i from 0 below n
            for inst = (aref vec i)
             do (let ((handler (loop for entry in *opt-jump-thread-table*
                                      for type = (car entry)
                                      for fn = (cdr entry)
                                      when (typep inst type) return fn)))
                  (if handler
                      (let ((new (funcall handler inst vec i idx)))
                        (when new (push new result)))
                      (push inst result))))
      (nreverse result))))

(defun %opt-jump-comparison-p (inst)
  "Return T when INST is a comparison supported by jump fact propagation."
  (typep inst '(or vm-lt vm-le vm-gt vm-ge vm-eq vm-num-eq)))

(defun %opt-jump-fact-killed-p (fact dst)
  "Return T when a write to DST invalidates FACT's comparison operands."
  (and fact dst
       (or (eq dst (getf fact :lhs))
           (eq dst (getf fact :rhs)))))

(defun %opt-jump-same-comparison-p (inst fact)
  "Return T when INST repeats the comparison described by FACT."
  (and fact
       (%opt-jump-comparison-p inst)
       (eq (type-of inst) (getf fact :pred))
       (eq (vm-lhs inst) (getf fact :lhs))
       (eq (vm-rhs inst) (getf fact :rhs))
       (opt-inst-dst inst)))

(defun %opt-jump-branch-comparison-fact (block)
  "Return a comparison fact for BLOCK's vm-jump-zero terminator, or NIL.

The comparison must define the branch condition register, and both comparison
operands must be known constants at that point in the same block. The returned
fact does not include the edge value; callers add :VALUE for taken/fallthrough
successors."
  (let* ((insts (bb-instructions block))
         (term (car (last insts))))
    (unless (typep term 'vm-jump-zero)
      (return-from %opt-jump-branch-comparison-fact nil))
    (let ((env (make-hash-table :test #'eq))
          (cond-reg (vm-reg term))
          (fact nil))
      (dolist (inst insts fact)
        (when (eq inst term)
          (return fact))
        (let ((dst (opt-inst-dst inst)))
          (when dst
            (cond
              ((typep inst 'vm-const)
               (setf (gethash dst env) (vm-value inst)))
              (t
               (remhash dst env))))
          (when (and dst (eq dst cond-reg))
            (setf fact nil)
            (when (%opt-jump-comparison-p inst)
              (multiple-value-bind (lval lfound) (gethash (vm-lhs inst) env)
                (declare (ignore lval))
                (multiple-value-bind (rval rfound) (gethash (vm-rhs inst) env)
                  (declare (ignore rval))
                  (when (and lfound rfound)
                    (setf fact (list :pred (type-of inst)
                                     :lhs (vm-lhs inst)
                                     :rhs (vm-rhs inst)))))))))))))

(defun %opt-jump-back-edge-p (pred succ)
  "Return T when PRED -> SUCC is a CFG back-edge."
  (and (bb-idom pred)
       (cfg-dominates-p succ pred)))

(defun %opt-jump-edge-fact (pred succ cfg live-fact)
  "Return the fact propagated on edge PRED -> SUCC, or NIL."
  (unless (and (= (length (bb-predecessors succ)) 1)
               (not (%opt-jump-back-edge-p pred succ)))
    (return-from %opt-jump-edge-fact nil))
  (let* ((term (car (last (bb-instructions pred))))
         (branch-fact (%opt-jump-branch-comparison-fact pred)))
    (if (and branch-fact (typep term 'vm-jump-zero))
        (let ((target (cfg-get-block-by-label cfg (vm-label-name term))))
          (append branch-fact (list :value (if (eq succ target) 0 1))))
        live-fact)))

(defun %opt-jump-rewrite-block-with-fact (block fact)
  "Rewrite redundant comparisons in BLOCK using incoming FACT.
Returns the still-live fact after scanning the block."
  (let ((live-fact fact)
        (new-insts nil))
    (dolist (inst (bb-instructions block))
      (let ((dst (opt-inst-dst inst)))
        (when (%opt-jump-fact-killed-p live-fact dst)
          (setf live-fact nil))
        (cond
          ((%opt-jump-same-comparison-p inst live-fact)
           (push (make-vm-const :dst dst :value (getf live-fact :value))
                 new-insts))
          (t
           (push inst new-insts)))))
    (setf (bb-instructions block) (nreverse new-insts))
    live-fact))

(defun %opt-pass-jump-propagate-edge-values (instructions)
  "Propagate known comparison values from conditional CFG edges."
  (let ((cfg (cfg-build instructions)))
    (cfg-compute-dominators cfg)
    (let ((rpo (cfg-compute-rpo cfg))
          (facts (make-hash-table :test #'eq)))
      (dolist (block rpo)
        (let ((live-fact (%opt-jump-rewrite-block-with-fact
                          block (gethash block facts))))
          (dolist (succ (bb-successors block))
            (let ((edge-fact (%opt-jump-edge-fact block succ cfg live-fact)))
              (when edge-fact
                (setf (gethash succ facts) edge-fact))))))
      (cfg-flatten cfg))))

(defun opt-pass-jump-threading-with-propagation (instructions)
  "Run jump-chain threading, then conservative comparison-value propagation."
  (%opt-pass-jump-propagate-edge-values
   (%opt-pass-jump-chain-only instructions)))
