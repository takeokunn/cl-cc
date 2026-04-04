(in-package :cl-cc)

;;; ─── Control Flow Graph (CFG) ────────────────────────────────────────────
;;;
;;; Constructs a CFG from a flat list of VM instructions by splitting at
;;; leaders (first instruction, jump targets, fall-through after branches).
;;;
;;; Also computes:
;;;   - Reverse post-order (RPO) for iterative algorithms
;;;   - Immediate dominators via Cooper et al. 2001 simple iterative algorithm
;;;   - Dominance frontiers (used for phi-node placement in SSA construction)
;;;
;;; References:
;;;   Cooper, Harvey, Kennedy (2001). "A Simple, Fast Dominance Algorithm"
;;;   Cytron, Ferrante et al. (1991). "Efficiently Computing Static Single
;;;     Assignment Form and the Control Dependence Graph"

;;; ─── Data Structures ─────────────────────────────────────────────────────

(defstruct (basic-block (:conc-name bb-))
  "A maximal straight-line sequence of VM instructions with a single entry
   and single exit.  The block's control flow edges are stored as predecessor
   and successor lists of basic-block structs."
  (id       0   :type fixnum)          ; unique block ID (0 = entry)
  (label    nil)                        ; vm-label instruction opening this block, or NIL
  (instructions nil :type list)        ; list of vm-instruction (excluding the opening label)
  (predecessors nil :type list)        ; list of basic-block
  (successors   nil :type list)        ; list of basic-block
  (idom         nil)                   ; immediate dominator (basic-block or NIL for entry)
  (dom-children nil :type list)        ; blocks dominated by this block
  (dom-frontier nil :type list)        ; dominance frontier blocks
  (post-idom    nil)                   ; immediate post-dominator
  (post-children nil :type list)       ; blocks post-dominated by this block
  (loop-depth   0   :type fixnum)      ; nesting depth (0 = not in any loop)
  (rpo-index    0   :type fixnum))     ; index in reverse post-order

(defstruct (cfg (:conc-name cfg-))
  "Control Flow Graph for a single function / compilation unit.
   Blocks are identified by integer IDs.  Entry is always block 0."
  (blocks     (make-array 0 :adjustable t :fill-pointer 0) :type (vector *))
  (entry      nil)                     ; entry basic-block
  (exit       nil)                     ; exit basic-block (last ret/halt block)
  (label->block (make-hash-table :test #'equal) :type hash-table)
  (next-id    0   :type fixnum))

;;; ─── CFG Construction ────────────────────────────────────────────────────

(defun cfg-new-block (cfg &key label)
  "Allocate a new basic-block in CFG and return it."
  (let ((b (make-basic-block :id (cfg-next-id cfg) :label label)))
    (incf (cfg-next-id cfg))
    (vector-push-extend b (cfg-blocks cfg))
    (when label
      (setf (gethash (vm-name label) (cfg-label->block cfg)) b))
    b))

(defun cfg-add-edge (from to)
  "Add a directed edge FROM → TO in the CFG."
  (pushnew to (bb-successors from) :test #'eq)
  (pushnew from (bb-predecessors to) :test #'eq))

(defun cfg-build (instructions)
  "Build a CFG from a flat list of VM INSTRUCTIONS.
   Returns a cfg struct with all basic blocks, edges, entry, and exit set.

   Algorithm:
     1. Identify leaders: instruction[0], every jump target, every
        fall-through instruction after a branch/jump.
     2. Group instructions into blocks starting at each leader.
     3. Connect blocks via fall-through and explicit jump edges."
  (when (null instructions)
    (let ((g (make-cfg)))
      (let ((entry (cfg-new-block g)))
        (setf (cfg-entry g) entry
              (cfg-exit  g) entry))
      (return-from cfg-build g)))

  (let* ((vec    (coerce instructions 'simple-vector))
         (n      (length vec))
         (leader (make-array n :element-type 'bit :initial-element 0))
         (g      (make-cfg)))

    ;; Pass 1: mark leaders
    (setf (aref leader 0) 1)
    (loop for i from 0 below n
          for inst = (aref vec i)
          do (typecase inst
               (vm-jump
                ;; unconditional jump: mark instruction after as potential leader
                ;; (for unreachable-block handling); edges wired in Pass 3
                (when (< (1+ i) n) (setf (aref leader (1+ i)) 1)))
               (vm-jump-zero
                ;; conditional jump: both fall-through and target are leaders
                (when (< (1+ i) n) (setf (aref leader (1+ i)) 1)))
               ((or vm-ret vm-halt)
                (when (< (1+ i) n) (setf (aref leader (1+ i)) 1)))
               (vm-label
                ;; A label starts a new block
                (setf (aref leader i) 1))))

    ;; Also mark targets of jumps as leaders
    (loop for i from 0 below n
          for inst = (aref vec i)
          do (typecase inst
               ((or vm-jump vm-jump-zero)
                (let* ((target-name (vm-label-name inst))
                       (tgt-pos (cfg-find-label-position vec n target-name)))
                  (when tgt-pos
                    (setf (aref leader tgt-pos) 1))))))

    ;; Pass 2: create basic blocks
    ;; blocks-by-start: start-index → basic-block
    (let ((blocks-by-start (make-hash-table)))
      (let ((cur-start 0)
            (cur-label nil))
        (loop for i from 0 below n
              do (when (= (aref leader i) 1)
                   ;; Start new block at i
                   (setf cur-start i)
                   ;; If this position IS a vm-label, use it as the block label
                   (setf cur-label
                         (if (vm-label-p (aref vec i))
                             (aref vec i)
                             nil))
                   (unless (gethash cur-start blocks-by-start)
                     (let ((b (cfg-new-block g :label cur-label)))
                       (setf (gethash cur-start blocks-by-start) b)))))

        ;; Pass 3: populate block instructions and add edges
        ;; For each block, collect instructions until the next leader
        (let ((starts (sort (loop for k being the hash-keys of blocks-by-start collect k)
                            #'<)))
          (loop for (s . rest) on starts
                do (let* ((e        (or (car rest) n))
                          (b        (gethash s blocks-by-start))
                          (insts    (loop for j from s below e
                                          for inst = (aref vec j)
                                          ;; Skip the opening label (stored in bb-label)
                                          unless (and (= j s) (vm-label-p inst))
                                          collect inst)))
                     (setf (bb-instructions b) insts)

                     ;; Add edges based on the terminator instruction
                     (let ((term (find-if (lambda (i) (typep i '(or vm-jump vm-jump-zero
                                                                    vm-ret vm-halt)))
                                          (reverse insts))))
                       (typecase term
                         ;; Unconditional jump: edge to target only
                         (vm-jump
                          (let ((tgt-b (cfg-get-block-by-label g (vm-label-name term))))
                            (when tgt-b (cfg-add-edge b tgt-b))))
                         ;; Conditional jump: fall-through AND target
                         (vm-jump-zero
                          (let ((tgt-b (cfg-get-block-by-label g (vm-label-name term)))
                                (fall-b (and (car rest) (gethash (car rest) blocks-by-start))))
                            (when tgt-b  (cfg-add-edge b tgt-b))
                            (when fall-b (cfg-add-edge b fall-b))))
                         ;; Return / halt: no outgoing edges
                         ((or vm-ret vm-halt)
                          nil)
                         ;; No explicit terminator (or nil term): fall through
                         (t
                          (let ((fall-b (and (car rest) (gethash (car rest) blocks-by-start))))
                            (when fall-b (cfg-add-edge b fall-b))))))))))

      ;; Set entry and exit blocks
      (let* ((entry-b  (gethash 0 blocks-by-start))
             ;; Exit is the first block with no successors (or the last block)
             (all-blocks (loop for b across (cfg-blocks g) collect b))
             (exit-b   (or (find-if (lambda (b) (null (bb-successors b))) all-blocks)
                           (car (last all-blocks)))))
        (setf (cfg-entry g) entry-b
              (cfg-exit  g) exit-b))

      g)))

(defun cfg-find-label-position (vec n label-name)
  "Find the index of a vm-label with name LABEL-NAME in instruction vector VEC."
  (loop for i from 0 below n
        when (and (vm-label-p (aref vec i))
                  (equal (vm-name (aref vec i)) label-name))
        return i))

(defun cfg-get-block-by-label (cfg label-name)
  "Return the basic-block for the given LABEL-NAME, or NIL."
  (gethash label-name (cfg-label->block cfg)))

;;; ─── Reverse Post-Order ──────────────────────────────────────────────────

(defun cfg-compute-rpo (cfg)
  "Compute reverse post-order (RPO) for CFG blocks starting from entry.
   Sets bb-rpo-index for each reachable block.
   Returns a list of blocks in RPO order."
  (let ((visited (make-hash-table :test #'eq))
        (post-order nil))
    (labels ((dfs (b)
               (unless (gethash b visited)
                 (setf (gethash b visited) t)
                 (dolist (s (bb-successors b))
                   (dfs s))
                 (push b post-order))))
      (when (cfg-entry cfg)
        (dfs (cfg-entry cfg))))
    ;; `push` prepends, so the last-pushed node (entry) is at the front.
    ;; post-order already holds blocks in RPO order — no nreverse needed.
    (loop for b in post-order for i from 0
          do (setf (bb-rpo-index b) i))
    post-order))

;;; ─── Dominator Tree (Cooper et al. 2001) ─────────────────────────────────

(defun cfg-compute-dominators (cfg)
  "Compute immediate dominators for all blocks in CFG using Cooper et al.'s
   simple iterative algorithm (2001).  Sets bb-idom for each block.
   Returns the entry block (root of the dominator tree)."
  (let* ((rpo    (cfg-compute-rpo cfg))
         (entry  (cfg-entry cfg)))
    (unless entry (return-from cfg-compute-dominators nil))

    ;; Initialize: entry dominates itself; all others are undefined (nil)
    (setf (bb-idom entry) entry)

    ;; Iterate until stable
    (let ((changed t))
      (loop while changed
            do (setf changed nil)
               (dolist (b rpo)
                 (unless (eq b entry)
                   (let ((new-idom nil))
                     ;; new-idom = first processed predecessor
                     (dolist (p (bb-predecessors b))
                       (when (bb-idom p)
                         (if (null new-idom)
                             (setf new-idom p)
                             (setf new-idom (cfg-intersect p new-idom)))))
                     (when (and new-idom (not (eq new-idom (bb-idom b))))
                       (setf (bb-idom b) new-idom
                             changed t)))))))

    ;; Build dom-children lists
    (loop for b across (cfg-blocks cfg)
          when (and (bb-idom b) (not (eq b entry)))
          do (push b (bb-dom-children (bb-idom b))))

    entry))

(defun cfg-intersect (b1 b2)
  "Find the common dominator of B1 and B2 using the RPO-indexed finger walk.
   Called during iterative dominator computation."
  (let ((f1 b1) (f2 b2))
    (loop until (eq f1 f2)
          do (loop while (> (bb-rpo-index f1) (bb-rpo-index f2))
                   do (setf f1 (bb-idom f1)))
             (loop while (> (bb-rpo-index f2) (bb-rpo-index f1))
                   do (setf f2 (bb-idom f2))))
    f1))

(defun cfg-dominates-p (a b)
  "T if block A dominates block B (A is an ancestor of B in the dominator tree)."
  (or (eq a b)
      (and (bb-idom b)
           (not (eq b (bb-idom b)))
           (cfg-dominates-p a (bb-idom b)))))

(defun cfg-collect-natural-loop (header tail)
  "Return the natural loop blocks for a backedge TAIL → HEADER."
  (let ((members (make-hash-table :test #'eq))
        (worklist (list tail)))
    (setf (gethash header members) t)
    (loop while worklist
          do (let ((b (pop worklist)))
               (unless (gethash b members)
                 (setf (gethash b members) t)
                 (dolist (p (bb-predecessors b))
                   (unless (gethash p members)
                     (push p worklist))))))
    (loop for b being the hash-keys of members collect b)))

(defun cfg-compute-loop-depths (cfg)
  "Annotate each block with a simple natural-loop nesting depth.
   A backedge is any edge whose target dominates its source."
  (loop for b across (cfg-blocks cfg)
        do (setf (bb-loop-depth b) 0))
  (loop for b across (cfg-blocks cfg)
        do (dolist (s (bb-successors b))
             (when (cfg-dominates-p s b)
               (dolist (member (cfg-collect-natural-loop s b))
                 (incf (bb-loop-depth member))))))
  cfg)

;;; ─── Post-Dominator Tree ────────────────────────────────────────────────

(defun cfg-compute-post-dominators (cfg)
  "Compute immediate post-dominators for all blocks in CFG.
   Traverses the reverse CFG starting from CFG's exit block. Sets bb-post-idom
   for each reachable block and populates bb-post-children lists.
   Returns the exit block (root of the post-dominator tree)."
  (let ((exit (cfg-exit cfg)))
    (unless exit (return-from cfg-compute-post-dominators nil))

    (loop for b across (cfg-blocks cfg)
          do (setf (bb-post-idom b) nil
                   (bb-post-children b) nil))

    (let ((visited (make-hash-table :test #'eq))
          (post-order nil)
          (post-rpo (make-hash-table :test #'eq)))
      (labels ((dfs (b)
                 (unless (gethash b visited)
                   (setf (gethash b visited) t)
                   (dolist (p (bb-predecessors b))
                     (dfs p))
                   (push b post-order))))
        (dfs exit))

      (loop for b in post-order
            for i from 0
            do (setf (gethash b post-rpo) i))

      (labels ((intersect (b1 b2)
                 (let ((f1 b1) (f2 b2))
                   (loop until (eq f1 f2)
                         do (loop while (> (gethash f1 post-rpo) (gethash f2 post-rpo))
                                  do (setf f1 (bb-post-idom f1)))
                            (loop while (> (gethash f2 post-rpo) (gethash f1 post-rpo))
                                  do (setf f2 (bb-post-idom f2))))
                   f1)))
        (setf (bb-post-idom exit) exit)
        (loop with changed = t
              while changed
              do (setf changed nil)
                 (dolist (b post-order)
                   (unless (eq b exit)
                     (let ((new-idom nil))
                       (dolist (s (bb-successors b))
                         (when (bb-post-idom s)
                           (if (null new-idom)
                               (setf new-idom s)
                               (setf new-idom (intersect s new-idom)))))
                       (when (and new-idom (not (eq new-idom (bb-post-idom b))))
                         (setf (bb-post-idom b) new-idom
                               changed t))))))

        (loop for b across (cfg-blocks cfg)
              when (and (bb-post-idom b) (not (eq b exit)))
              do (push b (bb-post-children (bb-post-idom b))))

        exit))))

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
  (labels ((replace-successor (block old new)
             (setf (bb-successors block)
                   (mapcar (lambda (succ)
                             (if (eq succ old) new succ))
                           (bb-successors block))))
           (replace-predecessor (block old new)
             (setf (bb-predecessors block)
                   (mapcar (lambda (pred)
                             (if (eq pred old) new pred))
                           (bb-predecessors block))))
           (ensure-label (block cfg prefix)
             (or (bb-label block)
                 (let ((label (make-vm-label
                               :name (format nil "~A~D" prefix (cfg-next-id cfg)))))
                   (setf (bb-label block) label
                         (gethash (vm-name label) (cfg-label->block cfg)) block)
                   label)))
           (split-edge (pred succ target-label)
             (let* ((pad-label (make-vm-label
                                :name (format nil "SPLIT_~D_~D_~D"
                                              (bb-id pred) (bb-id succ) (cfg-next-id cfg))))
                    (pad (cfg-new-block cfg :label pad-label)))
               (setf (bb-instructions pad)
                     (list (make-vm-jump :label (vm-name target-label))))
               (setf (bb-successors pad) (list succ)
                     (bb-predecessors pad) (list pred))
               (replace-successor pred succ pad)
               (replace-predecessor succ pred pad)
               pad)))
    (dolist (pred (coerce (cfg-blocks cfg) 'list) cfg)
      (when (> (length (bb-successors pred)) 1)
        (let ((term (find-if (lambda (i)
                               (typep i '(or vm-jump vm-jump-zero)))
                             (reverse (bb-instructions pred)))))
          (dolist (succ (copy-list (bb-successors pred)))
            (when (> (length (bb-predecessors succ)) 1)
              (let ((target-label (ensure-label succ cfg "SPLIT_TARGET_")))
                (cond
                  ((and (typep term 'vm-jump-zero)
                        (equal (vm-label-name term) (vm-name (bb-label succ))))
                   (let ((pad (split-edge pred succ target-label)))
                     (setf (vm-label-name term) (vm-name (bb-label pad)))))
                  (t
                    (split-edge pred succ target-label)))))))))))

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

;;; ─── CFG → Flat Instruction List ─────────────────────────────────────────

(defun cfg-flatten (cfg)
  "Emit a flat instruction list from the CFG (for round-trip testing).
   Blocks are emitted in RPO order.  Each block's opening label (if any)
   is prepended to the block's instruction list."
  (let ((rpo (cfg-compute-rpo cfg))
        (result nil))
    (dolist (b rpo)
      (when (bb-label b)
        (push (bb-label b) result))
      (dolist (inst (bb-instructions b))
        (push inst result)))
    (nreverse result)))

(defun cfg-flatten-hot-cold (cfg)
  "Emit a flat instruction list using loop depth as a hot/cold heuristic.

   Reachable blocks are ordered by explicit cold-path markers first, then by
   descending bb-loop-depth, with RPO used as the stable tie-breaker. This
   keeps loop bodies and error/condition blocks in a better layout without
   changing control-flow semantics."
  (labels ((block-cold-p (block)
             (some (lambda (inst)
                     (member (type-of inst)
                             '(vm-signal-error vm-establish-handler vm-remove-handler
                               vm-establish-catch vm-throw)
                             :test #'eq))
                   (bb-instructions block)))
           (hotter-p (a b)
             (cond
               ((and (block-cold-p a) (not (block-cold-p b))) nil)
               ((and (block-cold-p b) (not (block-cold-p a))) t)
               ((> (bb-loop-depth a) (bb-loop-depth b)) t)
               ((< (bb-loop-depth a) (bb-loop-depth b)) nil)
                (t (< (bb-rpo-index a) (bb-rpo-index b))))))
    (let* ((rpo (cfg-compute-rpo cfg))
          (ordered (stable-sort (copy-list rpo) #'hotter-p))
          (result nil))
    (dolist (b ordered)
      (when (bb-label b)
        (push (bb-label b) result))
      (dolist (inst (bb-instructions b))
        (push inst result)))
    (nreverse result))))

;;; ─── Accessors / Utilities ───────────────────────────────────────────────

(defun cfg-block-count (cfg)
  "Return the number of basic blocks in CFG."
  (length (cfg-blocks cfg)))
