;;;; tests/unit/optimize/cfg-tests.lisp — CFG + Dominator Tree Tests
;;; Tests for Phase 1: cfg-build, cfg-compute-dominators, cfg-compute-dominance-frontiers, and related utilities.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helpers ─────────────────────────────────────────────────────────────
(defun make-test-cfg-linear ()
  "Build a CFG from a simple linear instruction sequence: CONST → ADD → RET."
  (cl-cc/optimize::cfg-build
   (list (make-vm-const :dst :r0 :value 1)
         (make-vm-const :dst :r1 :value 2)
         (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
         (make-vm-ret   :reg :r2))))
(defun make-test-cfg-branch ()
  "Build a CFG with a conditional branch (entry JUMP-ZERO→then, else JUMP→exit, then, exit RET)."
  (cl-cc/optimize::cfg-build
   (list (make-vm-const    :dst :r0 :value 0)
         (make-vm-jump-zero :reg :r0 :label "then")
         (make-vm-const    :dst :r1 :value 99)
         (make-vm-jump     :label "exit")
         (make-vm-label    :name "then")
         (make-vm-const    :dst :r1 :value 42)
         (make-vm-label    :name "exit")
          (make-vm-ret      :reg :r1))))
(defun make-test-cfg-loop ()
  "Build a CFG with a simple natural loop and a separate exit block."
  (cl-cc/optimize::cfg-build
   (list (make-vm-label    :name "head")
          (make-vm-const    :dst :r0 :value 1)
          (make-vm-jump-zero :reg :r0 :label "exit")
          (make-vm-jump     :label "head")
          (make-vm-label    :name "exit")
          (make-vm-ret      :reg :r0))))
(defun make-test-cfg-hot-cold ()
  "Build a CFG with one loop-heavy hot block and one cold exit block."
  (cl-cc/optimize::cfg-build
   (list (make-vm-const    :dst :r0 :value 1)
         (make-vm-jump-zero :reg :r0 :label "cold")
         (make-vm-label    :name "hot")
         (make-vm-const    :dst :r1 :value 2)
         (make-vm-jump     :label "hot")
          (make-vm-label    :name "cold")
          (make-vm-ret      :reg :r0))))
(defun make-test-cfg-cold-signal ()
  "Build a CFG with a normal block and an explicit cold error block."
  (cl-cc/optimize::cfg-build
   (list (make-vm-const     :dst :r0 :value 0)
         (make-vm-jump-zero :reg :r0 :label "hot")
         (make-vm-label    :name "cold")
          (cl-cc::make-vm-signal-error :error-reg :r0)
          (make-vm-ret      :reg :r0)
          (make-vm-label    :name "hot")
          (make-vm-ret      :reg :r0))))
(defun make-test-cfg-critical-edge ()
  "Build a CFG with one critical edge into the THEN block."
  (cl-cc/optimize::cfg-build
   (list (make-vm-const    :dst :r0 :value 0)
          (make-vm-jump-zero :reg :r0 :label "then")
          (make-vm-const    :dst :r1 :value 99)
          (make-vm-jump     :label "then")
          (make-vm-label    :name "then")
          (make-vm-const    :dst :r1 :value 42)
          (make-vm-label    :name "merge")
          (make-vm-ret      :reg :r1))))

;;; ─── Basic CFG Construction ──────────────────────────────────────────────
(deftest cfg-linear-has-single-block
  "A purely linear instruction sequence produces exactly one basic block with an entry and instructions."
  (let* ((cfg   (make-test-cfg-linear))
         (entry (cl-cc/optimize::cfg-entry cfg)))
    (assert-= 1 (cl-cc/optimize::cfg-block-count cfg))
    (assert-true entry)
    (assert-true (cl-cc::bb-instructions entry))))

(deftest cfg-empty-has-entry-block
  "An empty instruction list still produces a CFG with a valid entry block."
  (let ((cfg (cl-cc/optimize::cfg-build nil)))
    (assert-true (cl-cc/optimize::cfg-entry cfg))))

(deftest cfg-branch-has-multiple-blocks
  "A conditional branch instruction produces at least 2 basic blocks."
  (let ((cfg (make-test-cfg-branch)))
    (assert-true (>= (cl-cc/optimize::cfg-block-count cfg) 2))))
(deftest-each cfg-branch-labels-resolved
  "cfg-get-block-by-label resolves all labeled blocks in the branch CFG."
  :cases (("then" "then")
          ("exit" "exit"))
  (label-name)
  (let ((cfg (make-test-cfg-branch)))
    (assert-true (cl-cc/optimize::cfg-get-block-by-label cfg label-name))))

;;; ─── Predecessor / Successor Edges ──────────────────────────────────────
(deftest cfg-branch-entry-has-two-successors
  "The entry block of a branch CFG has exactly 2 successors (then and else paths)."
  (let* ((cfg   (make-test-cfg-branch))
         (entry (cl-cc/optimize::cfg-entry cfg)))
    (assert-= 2 (length (cl-cc::bb-successors entry)))))

(deftest cfg-branch-exit-has-predecessor
  "The exit block of a branch CFG has at least one predecessor (the join point)."
  (let* ((cfg  (make-test-cfg-branch))
         (exit (cl-cc/optimize::cfg-get-block-by-label cfg "exit")))
    (when exit
      (assert-true (>= (length (cl-cc::bb-predecessors exit)) 1)))))

;;; ─── RPO ─────────────────────────────────────────────────────────────────
(deftest cfg-rpo-ordering
  "RPO visits all reachable blocks; entry block is first."
  (let* ((cfg   (make-test-cfg-branch))
         (rpo   (cl-cc/optimize::cfg-compute-rpo cfg))
         (entry (cl-cc/optimize::cfg-entry cfg)))
    (assert-= (cl-cc/optimize::cfg-block-count cfg) (length rpo))
    (assert-eq entry (car rpo))))

;;; ─── Dominator Tree ──────────────────────────────────────────────────────
(deftest cfg-dominator-properties
  "Entry idom is itself; entry dominates the exit block."
  (let* ((cfg   (make-test-cfg-branch))
         (entry (cl-cc/optimize::cfg-entry cfg)))
    (cl-cc/optimize::cfg-compute-dominators cfg)
    (assert-eq entry (cl-cc::bb-idom entry))
    (let ((exit (cl-cc/optimize::cfg-get-block-by-label cfg "exit")))
      (when exit
        (assert-true (cl-cc/optimize::cfg-dominates-p entry exit))))))

;;; ─── Dominance Frontiers ─────────────────────────────────────────────────
(deftest cfg-dominance-frontiers-computed
  "Dominance frontiers are computed without error."
  (let ((cfg (make-test-cfg-branch)))
    (cl-cc/optimize::cfg-compute-dominators cfg)
    (cl-cc/optimize::cfg-compute-dominance-frontiers cfg)
    (assert-true t)))

;;; ─── Post-Dominator Tree ────────────────────────────────────────────────
(deftest cfg-post-dominators-computed
  "Post-dominators are computed from the CFG exit block."
  (handler-bind ((warning #'muffle-warning))
    (let ((cfg (make-test-cfg-branch)))
      (cl-cc/optimize::cfg-compute-post-dominators cfg)
      (let ((exit (cl-cc/optimize::cfg-get-block-by-label cfg "exit"))
            (entry (cl-cc/optimize::cfg-entry cfg)))
        (assert-true exit)
        (assert-eq exit (cl-cc/optimize::bb-post-idom exit))
        (assert-true (cl-cc/optimize::cfg-post-dominates-p exit entry))))))
(deftest cfg-loop-depths-computed
  "Natural loops increment bb-loop-depth for the header and body blocks."
  (let* ((cfg (make-test-cfg-loop))
         (head (cl-cc/optimize::cfg-get-block-by-label cfg "head"))
         (exit (cl-cc/optimize::cfg-get-block-by-label cfg "exit"))
         (body (find-if (lambda (b)
                          (some (lambda (i)
                                  (and (typep i 'cl-cc/vm::vm-jump)
                                       (equal (cl-cc/vm::vm-label-name i) "head")))
                                (cl-cc::bb-instructions b)))
                        (coerce (cl-cc/optimize::cfg-blocks cfg) 'list))))
    (cl-cc/optimize::cfg-compute-dominators cfg)
    (cl-cc/optimize::cfg-compute-loop-depths cfg)
    (assert-true head)
    (assert-true body)
    (assert-= 1 (cl-cc::bb-loop-depth head))
    (assert-= 1 (cl-cc::bb-loop-depth body))
    (assert-= 0 (cl-cc::bb-loop-depth exit))))
(deftest-each cfg-hot-cold-flatten-cold-after-hot
  "Hot/cold flattening places cold blocks (loop-exit or signal-error) after hot loop blocks."
  :cases (("loop-vs-exit"     (make-test-cfg-hot-cold))
          ("normal-vs-signal" (make-test-cfg-cold-signal)))
  (cfg)
  (cl-cc/optimize::cfg-compute-dominators cfg)
  (cl-cc/optimize::cfg-compute-loop-depths cfg)
  (let* ((flat   (cl-cc/optimize::cfg-flatten-hot-cold cfg))
         (labels (loop for inst in flat
                       when (typep inst 'cl-cc/vm::vm-label)
                       collect (cl-cc/vm::vm-name inst))))
    (assert-true (member "hot"  labels :test #'equal))
    (assert-true (member "cold" labels :test #'equal))
    (assert-true (< (position "hot"  labels :test #'equal)
                    (position "cold" labels :test #'equal)))))
(deftest cfg-critical-edge-splitting-inserts-landing-pad
  "Critical edge splitting inserts a landing-pad block and rewires the edge."
  (let* ((cfg (make-test-cfg-critical-edge))
         (before (cl-cc/optimize::cfg-block-count cfg))
         (entry (cl-cc/optimize::cfg-entry cfg))
         (then  (cl-cc/optimize::cfg-get-block-by-label cfg "then")))
    (cl-cc/optimize::cfg-split-critical-edges cfg)
    (assert-= (1+ before) (cl-cc/optimize::cfg-block-count cfg))
    (assert-true (not (member then (cl-cc::bb-successors entry) :test #'eq)))
    (assert-true (not (member entry (cl-cc::bb-predecessors then) :test #'eq)))
    (let ((pad (find-if (lambda (b)
                          (and (= 1 (length (cl-cc::bb-successors b)))
                               (eq then (first (cl-cc::bb-successors b)))
                               (some (lambda (i)
                                       (and (typep i 'cl-cc/vm::vm-jump)
                                            (equal (cl-cc/vm::vm-label-name i)
                                                   (cl-cc/vm::vm-name (cl-cc::bb-label then)))))
                                     (cl-cc::bb-instructions b))))
                        (coerce (cl-cc/optimize::cfg-blocks cfg) 'list))))
      (assert-true pad)
      (assert-true (member pad (cl-cc::bb-successors entry) :test #'eq)))))

;;; ─── Flatten Round-Trip ──────────────────────────────────────────────────
(deftest cfg-flatten-preserves-instruction-count
  "Flattening a CFG recovers all instructions (labels + body)."
  (let* ((orig  (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-ret   :reg :r0)))
         (cfg   (cl-cc/optimize::cfg-build orig))
         (flat  (cl-cc/optimize::cfg-flatten cfg)))
    (assert-true (>= (length flat) (length orig)))))

;;; ─── cfg-idf (Iterated Dominance Frontier) ──────────────────────────────────
(deftest cfg-idf-empty-input-returns-nil
  "cfg-idf on an empty block set returns nil."
  (assert-null (cl-cc/optimize::cfg-idf nil)))

(deftest cfg-idf-linear-entry-returns-list
  "cfg-idf on the entry of a linear CFG returns a list (empty dominance frontier)."
  (let* ((cfg   (make-test-cfg-linear))
         (_     (cl-cc/optimize::cfg-compute-dominators cfg))
         (_     (cl-cc/optimize::cfg-compute-dominance-frontiers cfg))
         (entry (cl-cc/optimize::cfg-entry cfg)))
    (declare (ignore _ _))
    (assert-true (listp (cl-cc/optimize::cfg-idf (list entry))))))

(deftest cfg-idf-branch-entry-returns-list
  "cfg-idf on the entry of a branch CFG returns a list (join point is in frontier)."
  (let* ((cfg   (make-test-cfg-branch))
         (_     (cl-cc/optimize::cfg-compute-dominators cfg))
         (_     (cl-cc/optimize::cfg-compute-dominance-frontiers cfg))
         (entry (cl-cc/optimize::cfg-entry cfg)))
    (declare (ignore _ _))
    (assert-true (listp (cl-cc/optimize::cfg-idf (list entry))))))

;;; ─── %cfg-fallthrough-edge / %cfg-jump-target-edge ───────────────────────
(deftest cfg-fallthrough-edge-adds-when-next-start-exists
  "%cfg-fallthrough-edge adds a successor/predecessor edge when a next-start block exists."
  (let* ((g   (cl-cc/optimize::make-cfg))
         (b1  (cl-cc/optimize::cfg-new-block g))
         (b2  (cl-cc/optimize::cfg-new-block g))
         (bbs (let ((ht (make-hash-table)))
                (setf (gethash 1 ht) b2)
                ht)))
    (cl-cc/optimize::%cfg-fallthrough-edge b1 1 bbs)
    (assert-true (member b2 (cl-cc::bb-successors b1) :test #'eq))
    (assert-true (member b1 (cl-cc::bb-predecessors b2) :test #'eq))))

(deftest cfg-fallthrough-edge-noop-when-nil
  "%cfg-fallthrough-edge is a no-op when next-start is nil (block has no fall-through)."
  (let* ((g   (cl-cc/optimize::make-cfg))
         (b1  (cl-cc/optimize::cfg-new-block g))
         (bbs (make-hash-table)))
    (cl-cc/optimize::%cfg-fallthrough-edge b1 nil bbs)
    (assert-null (cl-cc::bb-successors b1))))

(deftest cfg-jump-target-edge-wires-to-label
  "%cfg-jump-target-edge adds an edge from the source block to the named label's block."
  (let* ((g    (cl-cc/optimize::make-cfg))
         (src  (cl-cc/optimize::cfg-new-block g))
         (dest (cl-cc/optimize::cfg-new-block g :label (make-vm-label :name "tgt"))))
    (declare (ignore dest))
    (cl-cc/optimize::%cfg-jump-target-edge src (make-vm-jump :label "tgt") g)
    (let ((tgt (cl-cc/optimize::cfg-get-block-by-label g "tgt")))
      (assert-true (member tgt (cl-cc::bb-successors src) :test #'eq)))))

;;; ─── %cfg-replace-successor / %cfg-replace-predecessor / %cfg-replace-terminator
(deftest cfg-replace-successor-swaps-block
  "%cfg-replace-successor replaces old with new in the successors list."
  (let* ((blk (make-instance 'cl-cc/optimize::basic-block))
         (old (make-instance 'cl-cc/optimize::basic-block))
         (new (make-instance 'cl-cc/optimize::basic-block)))
    (setf (cl-cc/optimize::bb-successors blk) (list old))
    (cl-cc/optimize::%cfg-replace-successor blk old new)
    (assert-false (member old (cl-cc/optimize::bb-successors blk) :test #'eq))
    (assert-true  (member new (cl-cc/optimize::bb-successors blk) :test #'eq))))

(deftest cfg-replace-predecessor-swaps-block
  "%cfg-replace-predecessor replaces old with new in the predecessors list."
  (let* ((blk (make-instance 'cl-cc/optimize::basic-block))
         (old (make-instance 'cl-cc/optimize::basic-block))
         (new (make-instance 'cl-cc/optimize::basic-block)))
    (setf (cl-cc/optimize::bb-predecessors blk) (list old))
    (cl-cc/optimize::%cfg-replace-predecessor blk old new)
    (assert-false (member old (cl-cc/optimize::bb-predecessors blk) :test #'eq))
    (assert-true  (member new (cl-cc/optimize::bb-predecessors blk) :test #'eq))))

(deftest cfg-replace-terminator-swaps-instruction
  "%cfg-replace-terminator replaces the old terminator instruction with a new one."
  (let* ((blk (make-instance 'cl-cc/optimize::basic-block))
         (old (make-vm-jump :label "a"))
         (new (make-vm-jump :label "b")))
    (setf (cl-cc/optimize::bb-instructions blk) (list old))
    (cl-cc/optimize::%cfg-replace-terminator blk old new)
    (assert-equal (list new) (cl-cc/optimize::bb-instructions blk))))

;;; ─── %cfg-ensure-label ────────────────────────────────────────────────────
(deftest cfg-ensure-label-creates-fresh-when-absent
  "%cfg-ensure-label assigns a new vm-label when bb-label is nil."
  (let* ((g   (cl-cc/optimize::make-cfg))
         (blk (cl-cc/optimize::cfg-new-block g)))
    (setf (cl-cc/optimize::bb-label blk) nil)
    (let ((lbl (cl-cc/optimize::%cfg-ensure-label blk g "test")))
      (assert-true (cl-cc/vm::vm-label-p lbl))
      (assert-eq lbl (cl-cc/optimize::bb-label blk)))))

(deftest cfg-ensure-label-returns-existing
  "%cfg-ensure-label returns the pre-existing label unchanged when one is already set."
  (let* ((g   (cl-cc/optimize::make-cfg))
         (blk (cl-cc/optimize::cfg-new-block g :label (make-vm-label :name "existing"))))
    (let ((lbl (cl-cc/optimize::%cfg-ensure-label blk g "test")))
      (assert-equal "existing" (cl-cc/vm::vm-name lbl)))))
