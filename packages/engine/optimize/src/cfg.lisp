(in-package :cl-cc/optimize)

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

(defun %cfg-mark-leaders (vec n)
  "Single pass: return a bit array marking every leader in VEC.
   Leaders: index 0, any vm-label, fall-through after branch/ret/halt,
   and every explicit jump target."
  (let ((leader (make-array n :element-type 'bit :initial-element 0)))
    (setf (aref leader 0) 1)
    (loop for i from 0 below n
          for inst = (aref vec i)
          do (typecase inst
               (vm-label
                (setf (aref leader i) 1))
               ((or vm-jump vm-jump-zero)
                (when (< (1+ i) n) (setf (aref leader (1+ i)) 1))
                (let ((tgt (cfg-find-label-position vec n (vm-label-name inst))))
                  (when tgt (setf (aref leader tgt) 1))))
               ((or vm-ret vm-halt)
                (when (< (1+ i) n) (setf (aref leader (1+ i)) 1)))))
    leader))

(defun %cfg-fallthrough-edge (b next-start blocks-by-start)
  "Add a fall-through edge from B to the block starting at NEXT-START, if any."
  (let ((fall (and next-start (gethash next-start blocks-by-start))))
    (when fall (cfg-add-edge b fall))))

(defun %cfg-jump-target-edge (b inst g)
  "Add an unconditional jump edge from B to the explicit target of INST."
  (let ((tgt (cfg-get-block-by-label g (vm-label-name inst))))
    (when tgt (cfg-add-edge b tgt))))

(defun %cfg-connect-block (b insts g blocks-by-start next-start)
  "Wire outgoing edges for block B whose instructions are INSTS."
  (let ((term (find-if (lambda (i) (typep i '(or vm-jump vm-jump-zero vm-ret vm-halt)))
                       (reverse insts))))
    (typecase term
      (vm-jump
       (%cfg-jump-target-edge b term g))
      (vm-jump-zero
       (%cfg-jump-target-edge b term g)
       (%cfg-fallthrough-edge b next-start blocks-by-start))
      ((or vm-ret vm-halt) nil)
      (t
       (%cfg-fallthrough-edge b next-start blocks-by-start)))))

(defun %cfg-build-create-blocks (g vec n blocks-by-start leader)
  "Pass 2: allocate a basic block for each leader position in VEC."
  (loop for i from 0 below n
        when (= (aref leader i) 1)
        do (let ((cur-label (and (vm-label-p (aref vec i)) (aref vec i))))
             (unless (gethash i blocks-by-start)
               (setf (gethash i blocks-by-start) (cfg-new-block g :label cur-label))))))

(defun %cfg-build-collect-instructions (vec s e)
  "Collect instructions in VEC for the block spanning [S, E), skipping any opening label."
  (loop for j from s below e
        for inst = (aref vec j)
        unless (and (= j s) (vm-label-p inst))
        collect inst))

(defun %cfg-build-connect-blocks (g vec n blocks-by-start)
  "Pass 3: populate each block's instruction list and wire CFG edges."
  (let ((starts (sort (loop for k being the hash-keys of blocks-by-start collect k) #'<)))
    (loop for (s . rest) on starts
          do (let* ((e     (or (car rest) n))
                    (b     (gethash s blocks-by-start))
                    (insts (%cfg-build-collect-instructions vec s e)))
               (setf (bb-instructions b) insts)
               (%cfg-connect-block b insts g blocks-by-start (car rest))))))

(defun cfg-build (instructions)
  "Build a CFG from a flat list of VM INSTRUCTIONS.
Returns a cfg struct with all basic blocks, edges, entry, and exit set.

Algorithm:
  1. Mark leaders: index 0, every jump target, fall-throughs after branches.
  2. Allocate a basic block per leader.
  3. Populate each block's instruction list and wire fall-through / jump edges."
  (when (null instructions)
    (let* ((g (make-cfg)) (entry (cfg-new-block g)))
      (setf (cfg-entry g) entry (cfg-exit g) entry)
      (return-from cfg-build g)))
  (let* ((vec             (coerce instructions 'simple-vector))
         (n               (length vec))
         (leader          (%cfg-mark-leaders vec n))
         (g               (make-cfg))
         (blocks-by-start (make-hash-table)))
    (%cfg-build-create-blocks  g vec n blocks-by-start leader)
    (%cfg-build-connect-blocks g vec n blocks-by-start)
    (let* ((all-blocks (loop for b across (cfg-blocks g) collect b))
           (entry-b    (gethash 0 blocks-by-start))
           (exit-b     (or (find-if (lambda (b) (null (bb-successors b))) all-blocks)
                           (car (last all-blocks)))))
      (setf (cfg-entry g) entry-b (cfg-exit g) exit-b))
    g))

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

(defun %cfg-rpo-dfs (b visited post-order-cell)
  "Post-order DFS from B; results are consed onto (car POST-ORDER-CELL)."
  (unless (gethash b visited)
    (setf (gethash b visited) t)
    (dolist (s (bb-successors b))
      (%cfg-rpo-dfs s visited post-order-cell))
    (push b (car post-order-cell))))

(defun cfg-compute-rpo (cfg)
  "Compute reverse post-order (RPO) for CFG blocks starting from entry.
   Sets bb-rpo-index for each reachable block.
   Returns a list of blocks in RPO order."
  (let ((visited        (make-hash-table :test #'eq))
        (post-order-cell (list nil)))
    (when (cfg-entry cfg)
      (%cfg-rpo-dfs (cfg-entry cfg) visited post-order-cell))
    ;; `push` prepends, so the last-pushed node (entry) is at the front.
    ;; post-order already holds blocks in RPO order — no nreverse needed.
    (let ((post-order (car post-order-cell)))
      (loop for b in post-order for i from 0
            do (setf (bb-rpo-index b) i))
      post-order)))

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

;;; ─── Dominator-based analysis helpers ───────────────────────────────────────
;;; cfg-post-dominates-p, %cfg-replace-*, %cfg-ensure-label, %cfg-split-edge,
;;; and cfg-split-critical-edges live in cfg-analysis.lisp (loads after this file).

(defun %cfg-tree-ancestor-p (a b idom-fn)
  "Return T if A is an ancestor of B in the tree defined by IDOM-FN."
  (or (eq a b)
      (let ((idom (funcall idom-fn b)))
        (and idom (not (eq b idom))
             (%cfg-tree-ancestor-p a idom idom-fn)))))

(defun cfg-dominates-p (a b)
  "T if block A dominates block B (A is an ancestor of B in the dominator tree)."
  (%cfg-tree-ancestor-p a b #'bb-idom))

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

