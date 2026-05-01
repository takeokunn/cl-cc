;;;; tests/unit/compile/ir/ir-types-tests.lisp — Compile IR Foundation Tests
;;;;
;;;; Tests for src/compile/ir/{types,block,ssa}.lisp
;;;; Covers: ir-value allocation, ir-block construction, CFG edge management,
;;;;         ir-rpo, ir-dominators (linear + diamond), SSA write/read/seal,
;;;;         ir-verify-ssa.
;;;; Printer tests → ir-printer-tests.lisp.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; ir-value allocation (types.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────

(deftest ir-value-creation
  "ir-new-value: IDs, type annotation, predicate, and def slot."
  (let* ((fn (cl-cc/ir:ir-make-function 'test))
         (v0 (cl-cc/ir:ir-new-value fn))
         (v1 (cl-cc/ir:ir-new-value fn))
         (v2 (cl-cc/ir:ir-new-value fn :type :integer)))
    ;; monotonically increasing IDs
    (assert-= 0 (cl-cc/ir:irv-id v0))
    (assert-= 1 (cl-cc/ir:irv-id v1))
    (assert-= 2 (cl-cc/ir:irv-id v2))
    ;; type annotation
    (assert-eq :integer (cl-cc/ir:irv-type v2))
    (assert-null (cl-cc/ir:irv-type v0))
    ;; predicate
    (assert-true  (cl-cc/ir:ir-value-p v0))
    (assert-false (cl-cc/ir:ir-value-p 42))
    (assert-false (cl-cc/ir:ir-value-p nil))
    (assert-false (cl-cc/ir:ir-value-p "string"))
    ;; def slot initially nil
    (assert-null (cl-cc/ir:irv-def v0))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; ir-block allocation and ir-make-function (types.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────

(deftest ir-block-creation
  "ir-make-function and ir-new-block: entry block, auto-labels, empty slots, ordering."
  (let* ((fn  (cl-cc/ir:ir-make-function 'my-fn :return-type :integer))
         (b1  (cl-cc/ir:ir-new-block fn))
         (b2  (cl-cc/ir:ir-new-block fn :then))
         (fn2 (cl-cc/ir:ir-make-function 'test))
         (ba  (cl-cc/ir:ir-new-block fn2 :b1))
         (bb  (cl-cc/ir:ir-new-block fn2 :b2)))
    ;; ir-make-function creates entry block
    (assert-true  (cl-cc/ir:ir-function-p fn))
    (assert-true  (cl-cc/ir:ir-block-p    (cl-cc/ir:irf-entry fn)))
    (assert-eq    :entry (cl-cc/ir:irb-label (cl-cc/ir:irf-entry fn)))
    (assert-equal :integer (cl-cc/ir:irf-return-type fn))
    (assert-eq    'my-fn (cl-cc/ir:irf-name fn))
    ;; auto-generated labels and IDs (entry=0, b1=1, b2=2)
    (assert-= 1 (cl-cc/ir:irb-id b1))
    (assert-= 2 (cl-cc/ir:irb-id b2))
    (assert-eq :block1 (cl-cc/ir:irb-label b1))
    (assert-eq :then   (cl-cc/ir:irb-label b2))
    ;; new block starts empty
    (assert-null (cl-cc/ir:irb-insts        b1))
    (assert-null (cl-cc/ir:irb-params       b1))
    (assert-null (cl-cc/ir:irb-predecessors b1))
    (assert-null (cl-cc/ir:irb-successors   b1))
    (assert-null (cl-cc/ir:irb-terminator   b1))
    ;; blocks appended in creation order
    (let ((blocks (cl-cc/ir:irf-blocks fn2)))
      (assert-= 3 (length blocks))
      (assert-eq (cl-cc/ir:irf-entry fn2) (first blocks))
      (assert-eq ba (second blocks))
      (assert-eq bb (third  blocks)))))


(deftest ir-emit-appends-in-order-with-back-pointer
  "ir-emit appends instructions in order and sets the block back-pointer on each."
  (let* ((fn    (cl-cc/ir:ir-make-function 'test))
         (entry (cl-cc/ir:irf-entry fn))
         (i1    (cl-cc/ir:make-ir-inst))
         (i2    (cl-cc/ir:make-ir-inst)))
    (cl-cc/ir:ir-emit entry i1)
    (cl-cc/ir:ir-emit entry i2)
    (assert-= 2 (length (cl-cc/ir:irb-insts entry)))
    (assert-eq i1 (first  (cl-cc/ir:irb-insts entry)))
    (assert-eq i2 (second (cl-cc/ir:irb-insts entry)))
    (assert-eq entry (cl-cc/ir:iri-block i1))
    (assert-eq entry (cl-cc/ir:iri-block i2))))

(deftest ir-set-terminator-installs-terminator-and-back-pointer
  "ir-set-terminator stores the terminator and sets its block back-pointer."
  (let* ((fn    (cl-cc/ir:ir-make-function 'test))
         (entry (cl-cc/ir:irf-entry fn))
         (term  (cl-cc/ir:make-ir-inst)))
    (cl-cc/ir:ir-set-terminator entry term)
    (assert-eq term  (cl-cc/ir:irb-terminator entry))
    (assert-eq entry (cl-cc/ir:iri-block term))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; RPO traversal (block.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────


(deftest ir-rpo-linear-chain-in-order
  "ir-rpo: linear chain A→B→C returns blocks in forward order."
  (let* ((fn  (cl-cc/ir:ir-make-function 'test))
         (a   (cl-cc/ir:irf-entry fn))
         (b   (cl-cc/ir:ir-new-block fn :mid))
         (c   (cl-cc/ir:ir-new-block fn :exit)))
    (cl-cc/ir:ir-add-edge a b)
    (cl-cc/ir:ir-add-edge b c)
    (let ((rpo (cl-cc/ir:ir-rpo fn)))
      (assert-= 3 (length rpo))
      (assert-eq a (first  rpo))
      (assert-eq b (second rpo))
      (assert-eq c (third  rpo)))))

(deftest ir-rpo-excludes-unreachable-blocks
  "ir-rpo: unreachable blocks (no edge from entry) are excluded from the traversal."
  (let* ((fn          (cl-cc/ir:ir-make-function 'test))
         (entry        (cl-cc/ir:irf-entry fn))
         (reachable    (cl-cc/ir:ir-new-block fn :reachable))
         (_unreachable (cl-cc/ir:ir-new-block fn :unreachable)))
    (declare (ignore _unreachable))
    (cl-cc/ir:ir-add-edge entry reachable)
    (let ((rpo (cl-cc/ir:ir-rpo fn)))
      (assert-= 2 (length rpo))
      (assert-true (member entry     rpo :test #'eq))
      (assert-true (member reachable rpo :test #'eq)))))

(deftest ir-rpo-diamond-includes-all-four-blocks
  "ir-rpo: diamond graph A→{B,C}→D includes all 4 blocks with A first."
  (let* ((fn (cl-cc/ir:ir-make-function 'test))
         (a  (cl-cc/ir:irf-entry fn))
         (b  (cl-cc/ir:ir-new-block fn :b))
         (c  (cl-cc/ir:ir-new-block fn :c))
         (d  (cl-cc/ir:ir-new-block fn :d)))
    (cl-cc/ir:ir-add-edge a b)
    (cl-cc/ir:ir-add-edge a c)
    (cl-cc/ir:ir-add-edge b d)
    (cl-cc/ir:ir-add-edge c d)
    (let ((rpo (cl-cc/ir:ir-rpo fn)))
      (assert-= 4 (length rpo))
      (assert-eq a (first rpo))
      (assert-true (member b rpo :test #'eq))
      (assert-true (member c rpo :test #'eq))
      (assert-true (member d rpo :test #'eq)))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Dominator tree (block.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────

(deftest ir-dominators-entry-self-dominates
  "ir-dominators: the entry block is its own immediate dominator."
  (let* ((fn    (cl-cc/ir:ir-make-function 'test))
         (entry (cl-cc/ir:irf-entry fn))
         (idom  (cl-cc/ir:ir-dominators fn)))
    (assert-eq entry (gethash entry idom))))

(deftest ir-dominators-linear-chain
  "ir-dominators: in A→B→C, idom(A)=A, idom(B)=A, idom(C)=B."
  (let* ((fn (cl-cc/ir:ir-make-function 'test))
         (a  (cl-cc/ir:irf-entry fn))
         (b  (cl-cc/ir:ir-new-block fn :b))
         (c  (cl-cc/ir:ir-new-block fn :c)))
    (cl-cc/ir:ir-add-edge a b)
    (cl-cc/ir:ir-add-edge b c)
    (let ((idom (cl-cc/ir:ir-dominators fn)))
      (assert-eq a (gethash a idom))
      (assert-eq a (gethash b idom))
      (assert-eq b (gethash c idom)))))

(deftest ir-dominators-branch-from-single-node
  "ir-dominators: in A→B→{C,D}, idom(C)=B and idom(D)=B."
  (let* ((fn (cl-cc/ir:ir-make-function 'test))
         (a  (cl-cc/ir:irf-entry fn))
         (b  (cl-cc/ir:ir-new-block fn :b))
         (c  (cl-cc/ir:ir-new-block fn :c))
         (d  (cl-cc/ir:ir-new-block fn :d)))
    (cl-cc/ir:ir-add-edge a b)
    (cl-cc/ir:ir-add-edge b c)
    (cl-cc/ir:ir-add-edge b d)
    (let ((idom (cl-cc/ir:ir-dominators fn)))
      (assert-eq a (gethash a idom))
      (assert-eq a (gethash b idom))
      (assert-eq b (gethash c idom))
      (assert-eq b (gethash d idom)))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; SSA variable tracking (ssa.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────

(deftest ir-ssa-write-read-var
  "ir-write-var / ir-read-var: same-block lookup, linear propagation, overwrite."
  (let* ((fn    (cl-cc/ir:ir-make-function 'test))
         (entry (cl-cc/ir:irf-entry fn))
         (val   (cl-cc/ir:ir-new-value fn :type :integer))
         (next  (cl-cc/ir:ir-new-block fn :next))
         (v1    (cl-cc/ir:ir-new-value fn))
         (v2    (cl-cc/ir:ir-new-value fn)))
    ;; same-block read
    (cl-cc/ir:ir-write-var fn 'x entry val)
    (assert-eq val (cl-cc/ir:ir-read-var fn 'x entry))
    ;; propagation through sealed predecessor
    (cl-cc/ir:ir-add-edge entry next)
    (cl-cc/ir:ir-seal-block fn next)
    (assert-eq val (cl-cc/ir:ir-read-var fn 'x next))
    ;; overwrite in same block keeps last value
    (cl-cc/ir:ir-write-var fn 'y entry v1)
    (cl-cc/ir:ir-write-var fn 'y entry v2)
    (assert-eq v2 (cl-cc/ir:ir-read-var fn 'y entry))))

(deftest ir-read-var-undefined-returns-nil
  "ir-read-var returns nil for a variable with no definition and no predecessors."
  (let* ((fn    (cl-cc/ir:ir-make-function 'test))
         (entry (cl-cc/ir:irf-entry fn)))
    (cl-cc/ir:ir-seal-block fn entry)
    (assert-null (cl-cc/ir:ir-read-var fn 'undefined-var entry))))

(deftest ir-seal-block-marks-sealed
  "ir-seal-block sets the sealed-p flag on the block."
  (let* ((fn  (cl-cc/ir:ir-make-function 'test))
         (blk (cl-cc/ir:ir-new-block fn :b)))
    (assert-false (cl-cc/ir:irb-sealed-p blk))
    (cl-cc/ir:ir-seal-block fn blk)
    (assert-true  (cl-cc/ir:irb-sealed-p blk))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; SSA verifier (block.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each ir-verify-ssa-valid-cases
  "ir-verify-ssa passes on both empty functions and functions with unique result values."
  :cases (("empty-function"  nil)
          ("unique-results"  t))
  (has-insts)
  (let* ((fn    (cl-cc/ir:ir-make-function 'test))
         (entry (cl-cc/ir:irf-entry fn)))
    (when has-insts
      (let ((v0 (cl-cc/ir:ir-new-value fn))
            (v1 (cl-cc/ir:ir-new-value fn)))
        (cl-cc/ir:ir-emit entry (cl-cc/ir:make-ir-inst :result v0))
        (cl-cc/ir:ir-emit entry (cl-cc/ir:make-ir-inst :result v1))))
    (assert-true (cl-cc/ir:ir-verify-ssa fn))))
