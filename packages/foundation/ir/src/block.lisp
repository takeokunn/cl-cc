;;;; compile/ir/block.lisp — CFG Construction, RPO, Dominator Tree
;;;;
;;;; Provides:
;;;;   ir-add-edge   — add a directed CFG edge
;;;;   ir-emit       — append an instruction to a block
;;;;   ir-set-terminator — set block's exit instruction
;;;;   ir-rpo        — reverse post-order traversal
;;;;   ir-dominators — Cooper et al. 2001 dominator tree
;;;;   ir-collect-uses — use-def chains
;;;;   ir-verify-ssa — check the single-definition SSA property

(in-package :cl-cc/ir)

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; CFG Edge Management
;;;; ─────────────────────────────────────────────────────────────────────────

(defun ir-add-edge (from to)
  "Add a directed CFG edge FROM->TO.
   Updates both SUCCESSORS of FROM and PREDECESSORS of TO.
   Duplicate edges are ignored."
  (pushnew to   (irb-successors   from) :test #'eq)
  (pushnew from (irb-predecessors to)   :test #'eq))

(defun ir-emit (block inst)
  "Append INST to BLOCK's instruction list (before the terminator).
   Sets INST's owning block back-pointer. Returns INST."
  (setf (iri-block inst) block)
  (setf (irb-insts block) (nconc (irb-insts block) (list inst)))
  inst)

(defun ir-set-terminator (block inst)
  "Set BLOCK's terminator to INST and update its block back-pointer.
   The terminator is always the last instruction of a block."
  (setf (iri-block inst) block)
  (setf (irb-terminator block) inst)
  inst)

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Reverse Post-Order Traversal
;;;; ─────────────────────────────────────────────────────────────────────────

(defun %ir-rpo-dfs (blk visited result-cell)
  "Post-order DFS: push BLK into (car RESULT-CELL) after visiting successors."
  (unless (gethash blk visited)
    (setf (gethash blk visited) t)
    (dolist (succ (irb-successors blk))
      (%ir-rpo-dfs succ visited result-cell))
    (push blk (car result-cell))))

(defun ir-rpo (fn)
  "Return all blocks of FN reachable from entry in reverse post-order.
   RPO guarantees each block appears before all successors (except back-edges
   in loops).  Uses the DFS + prepend trick: post-order DFS with (push blk result)
   gives RPO because push prepends, reversing the post-order in place."
  (let ((visited     (make-hash-table :test #'eq))
        (result-cell (list nil)))
    (when (irf-entry fn)
      (%ir-rpo-dfs (irf-entry fn) visited result-cell))
    (car result-cell)))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Dominator Tree — Cooper et al. "A Simple, Fast Dominance Algorithm" (2001)
;;;; ─────────────────────────────────────────────────────────────────────────
;;;
;;; Iteratively computes immediate dominators using reverse post-order.
;;; Converges in O(n * d) where d is the depth of the dominator tree (typically
;;; small for real programs).

(defun ir-dominators (fn)
  "Compute the immediate dominator of every reachable block in FN.
   Returns a hash table mapping ir-block -> ir-block (immediate dominator).
   The entry block maps to itself.  Unreachable blocks are absent."
  (let* ((rpo   (ir-rpo fn))
         (entry (irf-entry fn))
         (idom  (make-hash-table :test #'eq))   ; block -> idom block
         (idx   (make-hash-table :test #'eq)))  ; block -> RPO index (for finger alg.)
    ;; Assign RPO indices
    (loop for blk in rpo for i from 0
          do (setf (gethash blk idx) i))
    ;; Entry dominates itself
    (setf (gethash entry idom) entry)
    ;; Iterate to fixed point
    (loop
      (let ((changed nil))
        (dolist (blk (cdr rpo))                  ; skip entry
          (let ((new-idom nil))
            (dolist (pred (irb-predecessors blk))
              (when (gethash pred idom)           ; pred already processed
                (setf new-idom
                      (if new-idom
                          (ir--dom-intersect pred new-idom idom idx)
                          pred))))
            (when (and new-idom
                       (not (eq new-idom (gethash blk idom))))
              (setf (gethash blk idom) new-idom)
              (setf changed t))))
        (unless changed (return))))
    idom))

(defun ir--dom-intersect (b1 b2 idom idx)
  "Find the nearest common dominator of B1 and B2 using the finger algorithm.
   Both B1 and B2 must already have entries in IDOM."
  (let ((f1 b1) (f2 b2))
    (loop until (eq f1 f2)
      do (loop while (> (gethash f1 idx 0) (gethash f2 idx 0))
               do (setf f1 (gethash f1 idom)))
         (loop while (> (gethash f2 idx 0) (gethash f1 idx 0))
               do (setf f2 (gethash f2 idom))))
    f1))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Use-Def Chain Utilities
;;;; ─────────────────────────────────────────────────────────────────────────

(defun ir-collect-uses (fn)
  "Return a hash table mapping ir-value -> list of ir-inst that use that value.
   Walks all reachable blocks of FN via RPO, calling IR-OPERANDS on each inst."
  (let ((uses (make-hash-table :test #'eq)))
    (dolist (blk (ir-rpo fn))
      (dolist (inst (irb-insts blk))
        (dolist (operand (ir-operands inst))
          (push inst (gethash operand uses)))))
    uses))


;;;; ─────────────────────────────────────────────────────────────────────────
;;;; SSA Verifier
;;;; ─────────────────────────────────────────────────────────────────────────

(defun ir-verify-ssa (fn)
  "Verify the single-assignment property: every ir-value has at most one
   defining instruction in FN.  Returns T if valid; signals an error otherwise."
  (let ((defined (make-hash-table :test #'eq)))
    (dolist (blk (ir-rpo fn))
      ;; Check block parameters (they are also SSA values)
      (dolist (param (irb-params blk))
        (when (gethash param defined)
          (error "SSA violation: block-param ir-value %~D defined more than once in ~A"
                 (irv-id param) (irf-name fn)))
        (setf (gethash param defined) blk))
      ;; Check instruction results
      (dolist (inst (irb-insts blk))
        (let ((result (iri-result inst)))
          (when result
            (when (gethash result defined)
              (error "SSA violation: ir-value %~D defined more than once in ~A"
                     (irv-id result) (irf-name fn)))
            (setf (gethash result defined) inst)))))
    t))
