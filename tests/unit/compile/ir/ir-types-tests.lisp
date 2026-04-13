;;;; tests/unit/compile/ir/ir-types-tests.lisp — Compile IR Foundation Tests
;;;;
;;;; Tests for src/compile/ir/{types,block,ssa,printer}.lisp
;;;; Covers: ir-value allocation, ir-block construction, CFG edge management,
;;;;         ir-rpo, ir-dominators (linear + diamond), SSA write/read/seal,
;;;;         ir-verify-ssa, and the printer.

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; ir-value allocation (types.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────

(deftest ir-value-creation
  "ir-new-value: IDs, type annotation, predicate, and def slot."
  (let* ((fn (cl-cc:ir-make-function 'test))
         (v0 (cl-cc:ir-new-value fn))
         (v1 (cl-cc:ir-new-value fn))
         (v2 (cl-cc:ir-new-value fn :type :integer)))
    ;; monotonically increasing IDs
    (assert-= 0 (cl-cc:irv-id v0))
    (assert-= 1 (cl-cc:irv-id v1))
    (assert-= 2 (cl-cc:irv-id v2))
    ;; type annotation
    (assert-eq :integer (cl-cc:irv-type v2))
    (assert-null (cl-cc:irv-type v0))
    ;; predicate
    (assert-true  (cl-cc:ir-value-p v0))
    (assert-false (cl-cc:ir-value-p 42))
    (assert-false (cl-cc:ir-value-p nil))
    (assert-false (cl-cc:ir-value-p "string"))
    ;; def slot initially nil
    (assert-null (cl-cc:irv-def v0))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; ir-block allocation and ir-make-function (types.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────

(deftest ir-block-creation
  "ir-make-function and ir-new-block: entry block, auto-labels, empty slots, ordering."
  (let* ((fn  (cl-cc:ir-make-function 'my-fn :return-type :integer))
         (b1  (cl-cc:ir-new-block fn))
         (b2  (cl-cc:ir-new-block fn :then))
         (fn2 (cl-cc:ir-make-function 'test))
         (ba  (cl-cc:ir-new-block fn2 :b1))
         (bb  (cl-cc:ir-new-block fn2 :b2)))
    ;; ir-make-function creates entry block
    (assert-true  (cl-cc:ir-function-p fn))
    (assert-true  (cl-cc:ir-block-p    (cl-cc:irf-entry fn)))
    (assert-eq    :entry (cl-cc:irb-label (cl-cc:irf-entry fn)))
    (assert-equal :integer (cl-cc:irf-return-type fn))
    (assert-eq    'my-fn (cl-cc:irf-name fn))
    ;; auto-generated labels and IDs (entry=0, b1=1, b2=2)
    (assert-= 1 (cl-cc:irb-id b1))
    (assert-= 2 (cl-cc:irb-id b2))
    (assert-eq :block1 (cl-cc:irb-label b1))
    (assert-eq :then   (cl-cc:irb-label b2))
    ;; new block starts empty
    (assert-null (cl-cc:irb-insts        b1))
    (assert-null (cl-cc:irb-params       b1))
    (assert-null (cl-cc:irb-predecessors b1))
    (assert-null (cl-cc:irb-successors   b1))
    (assert-null (cl-cc:irb-terminator   b1))
    ;; blocks appended in creation order
    (let ((blocks (cl-cc:irf-blocks fn2)))
      (assert-= 3 (length blocks))
      (assert-eq (cl-cc:irf-entry fn2) (first blocks))
      (assert-eq ba (second blocks))
      (assert-eq bb (third  blocks)))))


(deftest ir-emit-appends-to-block
  "ir-emit appends instructions in order and sets block back-pointer."
  (let* ((fn    (cl-cc:ir-make-function 'test))
         (entry (cl-cc:irf-entry fn))
         (i1    (cl-cc:make-ir-inst))
         (i2    (cl-cc:make-ir-inst)))
    (cl-cc:ir-emit entry i1)
    (cl-cc:ir-emit entry i2)
    (assert-= 2 (length (cl-cc:irb-insts entry)))
    (assert-eq i1 (first  (cl-cc:irb-insts entry)))
    (assert-eq i2 (second (cl-cc:irb-insts entry)))
    (assert-eq entry (cl-cc:iri-block i1))
    (assert-eq entry (cl-cc:iri-block i2))))

(deftest ir-set-terminator-sets-back-pointer
  "ir-set-terminator sets the block terminator and its block back-pointer."
  (let* ((fn    (cl-cc:ir-make-function 'test))
         (entry (cl-cc:irf-entry fn))
         (term  (cl-cc:make-ir-inst)))
    (cl-cc:ir-set-terminator entry term)
    (assert-eq term  (cl-cc:irb-terminator entry))
    (assert-eq entry (cl-cc:iri-block term))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; RPO traversal (block.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────


(deftest ir-rpo-linear-chain-entry-first
  "ir-rpo returns entry before mid before exit in a linear chain A→B→C."
  (let* ((fn  (cl-cc:ir-make-function 'test))
         (a   (cl-cc:irf-entry fn))
         (b   (cl-cc:ir-new-block fn :mid))
         (c   (cl-cc:ir-new-block fn :exit)))
    (cl-cc:ir-add-edge a b)
    (cl-cc:ir-add-edge b c)
    (let ((rpo (cl-cc:ir-rpo fn)))
      (assert-= 3 (length rpo))
      (assert-eq a (first  rpo))
      (assert-eq b (second rpo))
      (assert-eq c (third  rpo)))))

(deftest ir-rpo-excludes-unreachable-blocks
  "ir-rpo excludes blocks that have no path from entry."
  (let* ((fn          (cl-cc:ir-make-function 'test))
         (entry        (cl-cc:irf-entry fn))
         (reachable    (cl-cc:ir-new-block fn :reachable))
         (_unreachable (cl-cc:ir-new-block fn :unreachable)))
    (declare (ignore _unreachable))
    (cl-cc:ir-add-edge entry reachable)
    ;; unreachable has no incoming edge from entry
    (let ((rpo (cl-cc:ir-rpo fn)))
      (assert-= 2 (length rpo))
      (assert-true (member entry     rpo :test #'eq))
      (assert-true (member reachable rpo :test #'eq)))))

(deftest ir-rpo-diamond-all-blocks-reachable
  "ir-rpo includes all 4 blocks of a diamond graph A→{B,C}→D."
  (let* ((fn (cl-cc:ir-make-function 'test))
         (a  (cl-cc:irf-entry fn))
         (b  (cl-cc:ir-new-block fn :b))
         (c  (cl-cc:ir-new-block fn :c))
         (d  (cl-cc:ir-new-block fn :d)))
    (cl-cc:ir-add-edge a b)
    (cl-cc:ir-add-edge a c)
    (cl-cc:ir-add-edge b d)
    (cl-cc:ir-add-edge c d)
    (let ((rpo (cl-cc:ir-rpo fn)))
      (assert-= 4 (length rpo))
      ;; Entry must be first
      (assert-eq a (first rpo))
      ;; All blocks present
      (assert-true (member b rpo :test #'eq))
      (assert-true (member c rpo :test #'eq))
      (assert-true (member d rpo :test #'eq)))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Dominator tree (block.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────

(deftest ir-dominators-entry-dominates-itself
  "The entry block is its own immediate dominator."
  (let* ((fn    (cl-cc:ir-make-function 'test))
         (entry (cl-cc:irf-entry fn))
         (idom  (cl-cc:ir-dominators fn)))
    (assert-eq entry (gethash entry idom))))

(deftest ir-dominators-linear-chain
  "In A→B→C: idom(A)=A, idom(B)=A, idom(C)=B."
  (let* ((fn (cl-cc:ir-make-function 'test))
         (a  (cl-cc:irf-entry fn))
         (b  (cl-cc:ir-new-block fn :b))
         (c  (cl-cc:ir-new-block fn :c)))
    (cl-cc:ir-add-edge a b)
    (cl-cc:ir-add-edge b c)
    (let ((idom (cl-cc:ir-dominators fn)))
      (assert-eq a (gethash a idom))
      (assert-eq a (gethash b idom))
      (assert-eq b (gethash c idom)))))


(deftest ir-dominators-Y-shape
  "In A→B→{C,D}: idom(B)=A, idom(C)=B, idom(D)=B."
  (let* ((fn (cl-cc:ir-make-function 'test))
         (a  (cl-cc:irf-entry fn))
         (b  (cl-cc:ir-new-block fn :b))
         (c  (cl-cc:ir-new-block fn :c))
         (d  (cl-cc:ir-new-block fn :d)))
    (cl-cc:ir-add-edge a b)
    (cl-cc:ir-add-edge b c)
    (cl-cc:ir-add-edge b d)
    (let ((idom (cl-cc:ir-dominators fn)))
      (assert-eq a (gethash a idom))
      (assert-eq a (gethash b idom))
      (assert-eq b (gethash c idom))
      (assert-eq b (gethash d idom)))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; SSA variable tracking (ssa.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────

(deftest ir-ssa-write-read-var
  "ir-write-var / ir-read-var: same-block lookup, linear propagation, overwrite."
  (let* ((fn    (cl-cc:ir-make-function 'test))
         (entry (cl-cc:irf-entry fn))
         (val   (cl-cc:ir-new-value fn :type :integer))
         (next  (cl-cc:ir-new-block fn :next))
         (v1    (cl-cc:ir-new-value fn))
         (v2    (cl-cc:ir-new-value fn)))
    ;; same-block read
    (cl-cc:ir-write-var fn 'x entry val)
    (assert-eq val (cl-cc:ir-read-var fn 'x entry))
    ;; propagation through sealed predecessor
    (cl-cc:ir-add-edge entry next)
    (cl-cc:ir-seal-block fn next)
    (assert-eq val (cl-cc:ir-read-var fn 'x next))
    ;; overwrite in same block keeps last value
    (cl-cc:ir-write-var fn 'y entry v1)
    (cl-cc:ir-write-var fn 'y entry v2)
    (assert-eq v2 (cl-cc:ir-read-var fn 'y entry))))

(deftest ir-read-var-undefined-returns-nil
  "ir-read-var returns nil for a variable with no definition and no predecessors."
  (let* ((fn    (cl-cc:ir-make-function 'test))
         (entry (cl-cc:irf-entry fn)))
    (cl-cc:ir-seal-block fn entry)
    (assert-null (cl-cc:ir-read-var fn 'undefined-var entry))))

(deftest ir-seal-block-marks-sealed
  "ir-seal-block sets the sealed-p flag on the block."
  (let* ((fn  (cl-cc:ir-make-function 'test))
         (blk (cl-cc:ir-new-block fn :b)))
    (assert-false (cl-cc:irb-sealed-p blk))
    (cl-cc:ir-seal-block fn blk)
    (assert-true  (cl-cc:irb-sealed-p blk))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; SSA verifier (block.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────

(deftest ir-verify-ssa-empty-function
  "ir-verify-ssa passes on an empty function with no instructions."
  (let ((fn (cl-cc:ir-make-function 'test)))
    (assert-true (cl-cc:ir-verify-ssa fn))))

(deftest ir-verify-ssa-unique-results
  "ir-verify-ssa passes when all instruction results are distinct ir-values."
  (let* ((fn    (cl-cc:ir-make-function 'test))
         (entry (cl-cc:irf-entry fn))
         (v0    (cl-cc:ir-new-value fn))
         (v1    (cl-cc:ir-new-value fn))
         (i0    (cl-cc:make-ir-inst :result v0))
         (i1    (cl-cc:make-ir-inst :result v1)))
    (cl-cc:ir-emit entry i0)
    (cl-cc:ir-emit entry i1)
    (assert-true (cl-cc:ir-verify-ssa fn))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; IR Printer (printer.lisp)
;;;; ─────────────────────────────────────────────────────────────────────────

(deftest ir-format-value-produces-percent-id
  "ir-format-value returns %<id> for ir-values."
  (let* ((fn (cl-cc:ir-make-function 'test))
         (v  (cl-cc:ir-new-value fn)))
    (assert-equal "%0" (cl-cc:ir-format-value v))))

(deftest-each ir-format-value-non-ir-value
  "ir-format-value falls back to Lisp-print for non-ir-values."
  :cases (("integer" 42   "42")
          ("nil"     nil  "NIL")
          ("keyword" :foo ":FOO"))
  (value expected)
  (assert-equal expected (cl-cc:ir-format-value value)))

(deftest-each ir-function-to-string-content
  "ir-function-to-string: non-empty output; contains function name; contains entry label."
  :cases (("non-empty"   'my-fn   nil)
          ("fn-name"     'compute "compute")
          ("entry-label" 'test    "entry"))
  (fn-sym expected-substr)
  (let* ((fn (cl-cc:ir-make-function fn-sym :return-type :integer))
         (s  (cl-cc:ir-function-to-string fn)))
    (assert-true (> (length s) 0))
    (when expected-substr
      (assert-true (search expected-substr s)))))

(deftest ir-print-block-shows-predecessor-annotation
  "ir-print-block includes a '; preds:' comment."
  (let* ((fn  (cl-cc:ir-make-function 'test))
         (blk (cl-cc:irf-entry fn))
         (s   (with-output-to-string (stream)
                (cl-cc:ir-print-block blk stream))))
    (assert-true (search "preds" s))))
