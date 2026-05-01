(in-package :cl-cc/optimize)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; CFG — Post-Dominator Tree, Critical Edge Splitting, and Dominance Frontiers
;;;
;;; Contains: cfg-compute-post-dominators, cfg-post-dominates-p,
;;; cfg-split-critical-edges, cfg-compute-dominance-frontiers, cfg-idf.
;;;
;;; CFG construction (cfg-build), RPO (cfg-compute-rpo), forward dominators
;;; (cfg-compute-dominators), and loop-depth analysis are in cfg.lisp (loads before).
;;;
;;; Load order: after cfg.lisp, before cfg-layout.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ─── Post-dominator helpers ───────────────────────────────────────────────

(defun %cfg-post-dom-dfs (b visited post-order)
  "Depth-first walk of the reverse CFG from B, building POST-ORDER."
  (unless (gethash b visited)
    (setf (gethash b visited) t)
    (dolist (p (bb-predecessors b))
      (%cfg-post-dom-dfs p visited post-order))
    (push b (car post-order))))

(defun %cfg-post-dom-intersect (b1 b2 post-rpo)
  "Cooper et al. intersect for immediate post-dominator computation."
  (let ((f1 b1) (f2 b2))
    (loop until (eq f1 f2)
          do (loop while (> (gethash f1 post-rpo) (gethash f2 post-rpo))
                   do (setf f1 (bb-post-idom f1)))
             (loop while (> (gethash f2 post-rpo) (gethash f1 post-rpo))
                   do (setf f2 (bb-post-idom f2))))
    f1))

;;; ─── Critical edge helpers ────────────────────────────────────────────────

(defun %cfg-replace-successor (block old new)
  "Rewrite BLOCK's successor list replacing OLD with NEW."
  (setf (bb-successors block)
        (mapcar (lambda (succ) (if (eq succ old) new succ))
                (bb-successors block))))

(defun %cfg-replace-predecessor (block old new)
  "Rewrite BLOCK's predecessor list replacing OLD with NEW."
  (setf (bb-predecessors block)
        (mapcar (lambda (pred) (if (eq pred old) new pred))
                (bb-predecessors block))))

(defun %cfg-replace-terminator (block old new)
  "Replace terminator OLD in BLOCK's instruction list with NEW."
  (setf (bb-instructions block)
        (mapcar (lambda (inst) (if (eq inst old) new inst))
                (bb-instructions block))))

(defun %cfg-ensure-label (block cfg prefix)
  "Return BLOCK's label, creating and registering a fresh one if absent."
  (or (bb-label block)
      (let ((label (make-vm-label :name (format nil "~A~D" prefix (cfg-next-id cfg)))))
        (setf (bb-label block) label
              (gethash (vm-name label) (cfg-label->block cfg)) block)
        label)))

(defun %cfg-split-edge (cfg pred succ target-label)
  "Insert a landing-pad block on the CFG edge PRED→SUCC, returning the new block."
  (let* ((pad-label (make-vm-label
                     :name (format nil "SPLIT_~D_~D_~D"
                                   (bb-id pred) (bb-id succ) (cfg-next-id cfg))))
         (pad (cfg-new-block cfg :label pad-label)))
    (setf (bb-instructions pad) (list (make-vm-jump :label (vm-name target-label)))
          (bb-successors pad)   (list succ)
          (bb-predecessors pad) (list pred))
    (%cfg-replace-successor   pred succ pad)
    (%cfg-replace-predecessor succ pred pad)
    pad))

(defun cfg-compute-post-dominators (cfg)
  "Compute immediate post-dominators for all blocks in CFG.
   Traverses the reverse CFG starting from CFG's exit block. Sets bb-post-idom
   for each reachable block and populates bb-post-children lists.
   Returns the exit block (root of the post-dominator tree)."
  (let ((exit (cfg-exit cfg)))
    (unless exit (return-from cfg-compute-post-dominators nil))
    (loop for b across (cfg-blocks cfg)
          do (setf (bb-post-idom b) nil (bb-post-children b) nil))
    (let* ((post-order-cell (list nil))
           (visited   (make-hash-table :test #'eq))
           (post-rpo  (make-hash-table :test #'eq)))
      (%cfg-post-dom-dfs exit visited post-order-cell)
      (let ((post-order (car post-order-cell)))
        (loop for b in post-order for i from 0
              do (setf (gethash b post-rpo) i))
        (setf (bb-post-idom exit) exit)
        (loop with changed = t
              while changed
              do (setf changed nil)
                 (dolist (b post-order)
                   (unless (eq b exit)
                     (let ((new-idom nil))
                       (dolist (s (bb-successors b))
                         (when (bb-post-idom s)
                           (setf new-idom (if new-idom
                                              (%cfg-post-dom-intersect s new-idom post-rpo)
                                              s))))
                       (when (and new-idom (not (eq new-idom (bb-post-idom b))))
                         (setf (bb-post-idom b) new-idom changed t))))))
        (loop for b across (cfg-blocks cfg)
              when (and (bb-post-idom b) (not (eq b exit)))
              do (push b (bb-post-children (bb-post-idom b))))
        exit)))

(defun cfg-post-dominates-p (a b)
  "T if block A post-dominates block B (A is an ancestor of B in the post-dominator tree)."
  (or (eq a b)
      (and (bb-post-idom b)
           (not (eq b (bb-post-idom b)))
           (cfg-post-dominates-p a (bb-post-idom b)))))

;;; ─── Critical Edge Splitting ─────────────────────────────────────────────

(defun cfg-split-critical-edges (cfg)
  "Split critical edges by inserting empty landing-pad blocks.

   A critical edge is an edge from a block with multiple successors to a block
   with multiple predecessors.  This pass inserts a fresh block on each such
   edge and rewires the CFG so later SSA / code-motion passes can place code on
   the split edge without duplicating it along other incoming paths."
  (dolist (pred (coerce (cfg-blocks cfg) 'list) cfg)
    (when (> (length (bb-successors pred)) 1)
      (let ((term (find-if (lambda (i) (typep i '(or vm-jump vm-jump-zero)))
                           (reverse (bb-instructions pred)))))
        (dolist (succ (copy-list (bb-successors pred)))
          (when (> (length (bb-predecessors succ)) 1)
            (let ((target-label (%cfg-ensure-label succ cfg "SPLIT_TARGET_")))
              (cond
                ((and (typep term 'vm-jump-zero)
                      (equal (vm-label-name term) (vm-name (bb-label succ))))
                 (let ((pad (%cfg-split-edge cfg pred succ target-label)))
                   (%cfg-replace-terminator pred term
                                            (make-vm-jump-zero :reg (vm-reg term)
                                                               :label (vm-name (bb-label pad))))))
                (t
                 (%cfg-split-edge cfg pred succ target-label)))))))))))

;;; ─── Dominance Frontiers ─────────────────────────────────────────────────

(defun cfg-compute-dominance-frontiers (cfg)
  "Compute dominance frontiers for all blocks in CFG.
   Sets bb-dom-frontier for each block.
   DF(b) = { y | ∃ x ∈ pred(y) such that b dom x and b !strictdom y }

   Algorithm (Cytron et al.):
     For each block y with ≥2 predecessors:
       For each predecessor x of y:
         Walk up dominator tree from x to idom(y) (exclusive),
         adding y to DF(runner) at each step."
  (loop for y across (cfg-blocks cfg)
        when (>= (length (bb-predecessors y)) 2)
        do (let ((iy (bb-idom y)))
             (dolist (x (bb-predecessors y))
               (let ((runner x))
                 (loop until (eq runner iy)
                       do (pushnew y (bb-dom-frontier runner) :test #'eq)
                          (setf runner (bb-idom runner))
                          (unless runner (return)))))))
  cfg)

;;; ─── Iterated Dominance Frontier ─────────────────────────────────────────

(defun cfg-idf (def-blocks)
  "Compute the iterated dominance frontier (IDF) of DEF-BLOCKS.
   Returns the set of join points where phi-nodes must be placed.
   DEF-BLOCKS themselves are NOT included unless they appear in a frontier.

   Algorithm (Cytron et al.):
     visited tracks processed nodes to prevent infinite loops.
     result contains only blocks that are in some dominance frontier."
  (let ((result  (make-hash-table :test #'eq))  ; actual IDF members
        (visited (make-hash-table :test #'eq))  ; processed worklist nodes
        (worklist (copy-list def-blocks)))
    ;; Mark def-blocks as visited to avoid redundant re-processing
    (dolist (b def-blocks) (setf (gethash b visited) t))
    (loop while worklist
          do (let ((b (pop worklist)))
               (dolist (f (bb-dom-frontier b))
                 (unless (gethash f result)
                   (setf (gethash f result) t)
                   (unless (gethash f visited)
                     (setf (gethash f visited) t)
                     (push f worklist))))))
    (loop for b being the hash-keys of result collect b)))

