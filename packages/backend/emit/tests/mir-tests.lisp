;;;; tests/unit/emit/mir-tests.lisp — Unit tests for MIR (Phase 1)
;;;
;;; Covers: mir-value, mir-const, mir-inst, mir-block, mir-function,
;;;         mir-module, builder API, SSA variable tracking, CFG utilities,
;;;         target-desc and predefined targets.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;;; ─── mir-value ────────────────────────────────────────────────────────

(deftest mir-value-creation
  "mir-new-value: monotonic IDs; default type :any; predicate distinguishes values from non-values."
  (let* ((fn (mir-make-function :test-fn))
         (v0 (mir-new-value fn))
         (v1 (mir-new-value fn))
         (v2 (mir-new-value fn :name :x :type :integer))
         (c  (make-mir-const :value 42)))
    (assert-= 0 (mirv-id v0))
    (assert-= 1 (mirv-id v1))
    (assert-= 2 (mirv-id v2))
    (assert-eq :x       (mirv-name v2))
    (assert-eq :integer (mirv-type v2))
    (assert-= 3 (mirf-value-counter fn))
    (assert-eq :any (mirv-type v0))
    (assert-true  (mir-value-p v0))
    (assert-false (mir-value-p c))
    (assert-false (mir-value-p 42))))

;;;; ─── mir-const ────────────────────────────────────────────────────────

(deftest-each mir-const-types
  "make-mir-const stores value and type for integer, nil, and string payloads."
  :cases (("integer" 42      :integer
           (lambda (c)
             (assert-equal 42 (mirc-value c))))
          ("nil"     nil     :pointer
           (lambda (c)
             (assert-null (mirc-value c))))
          ("string"  "hello" :any
           (lambda (c)
             (assert-equal "hello" (mirc-value c)))))
  (val type verify)
  (let ((c (make-mir-const :value val :type type)))
    (assert-true (mir-const-p c))
    (assert-eq type (mirc-type c))
    (funcall verify c)))

;;;; ─── mir-block ────────────────────────────────────────────────────────

(deftest mir-block-creation
  "mir-new-block: unique IDs and labels; fresh block has no insts/preds/succs/phis."
  (let* ((fn (mir-make-function :test-fn))
         (b1 (mir-new-block fn))
         (b2 (mir-new-block fn :label :then)))
    (assert-false (null (mirf-entry fn)))
    (assert-eq :entry (mirb-label (mirf-entry fn)))
    (assert-eq :then (mirb-label b2))
    (assert-false (= (mirb-id b1) (mirb-id b2)))
    (assert-null (mirb-insts b1))
    (assert-null (mirb-preds b1))
    (assert-null (mirb-succs b1))
    (assert-null (mirb-phis b1))
    (assert-false (mirb-sealed-p b1))))

(deftest mir-block-pred-succ-linking
  "mir-add-succ establishes bidirectional predecessor/successor edges."
  (let* ((fn   (mir-make-function :f))
         (b1   (mirf-entry fn))
         (b2   (mir-new-block fn :label :exit)))
    (mir-add-succ b1 b2)
    (assert-true  (member b2 (mirb-succs b1) :test #'eq))
    (assert-true  (member b1 (mirb-preds b2) :test #'eq))
    ;; idempotent
    (mir-add-succ b1 b2)
    (assert-= 1 (length (mirb-succs b1)))))

;;;; ─── mir-function ──────────────────────────────────────────────────────

(deftest mir-make-function-structure
  "mir-make-function: initialises name/entry/counters; params stored in mirf-params."
  (let ((fn (mir-make-function :fib)))
    (assert-eq :fib (mirf-name fn))
    (assert-false (null (mirf-entry fn)))
    (assert-eq :entry (mirb-label (mirf-entry fn)))
    (assert-= 0 (mirf-value-counter fn))
    (assert-= 1 (mirf-block-counter fn)))
  (let* ((fn (mir-make-function :add))
         (p0 (mir-new-value fn :name :a :type :integer))
         (p1 (mir-new-value fn :name :b :type :integer)))
    (setf (mirf-params fn) (list p0 p1))
    (assert-= 2 (length (mirf-params fn)))
    (assert-eq :a (mirv-name (first (mirf-params fn))))))

;;;; ─── mir-emit ──────────────────────────────────────────────────────────

(deftest mir-emit-wires-op-dst-srcs-and-block
  "mir-emit: op, dst, srcs, block all correctly wired on the returned instruction."
  (let* ((fn   (mir-make-function :f))
         (blk  (mirf-entry fn))
         (dst  (mir-new-value fn :name :result :type :integer))
         (a    (make-mir-const :value 1 :type :integer))
         (b    (make-mir-const :value 2 :type :integer))
         (inst (mir-emit blk :add :dst dst :srcs (list a b))))
    (assert-true (mir-inst-p inst))
    (assert-eq   :add (miri-op inst))
    (assert-eq   dst  (miri-dst inst))
    (assert-= 2 (length (miri-srcs inst)))
    (assert-= 1 (length (mirb-insts blk)))
    (assert-eq blk (miri-block inst))))

(deftest mir-emit-sets-def-inst-pointer
  "mir-emit sets the mirv-def-inst pointer on the destination value."
  (let* ((fn   (mir-make-function :f))
         (blk  (mirf-entry fn))
         (dst  (mir-new-value fn))
         (inst (mir-emit blk :const :dst dst :srcs (list (make-mir-const :value 0)))))
    (assert-eq inst (mirv-def-inst dst))))

(deftest mir-emit-ret-has-nil-dst
  "mir-emit :ret produces an instruction with nil dst."
  (let* ((fn  (mir-make-function :f))
         (blk (mirf-entry fn))
         (v   (mir-new-value fn)))
    (assert-null (miri-dst (mir-emit blk :ret :srcs (list v))))))

(deftest mir-emit-phi-goes-to-phis-list-not-insts
  "mir-emit :phi adds to mirb-phis (not mirb-insts)."
  (let* ((fn  (mir-make-function :f))
         (blk (mir-new-block fn :label :loop))
         (dst (mir-new-value fn :name :x))
         (phi (mir-emit blk :phi :dst dst)))
    (assert-true (member phi (mirb-phis blk) :test #'eq))
    (assert-null (mirb-insts blk))))

(deftest mir-emit-preserves-instruction-ordering
  "mir-emit preserves ordering: 3 emitted instructions appear first-to-last in mirb-insts."
  (let* ((fn  (mir-make-function :f))
         (blk (mirf-entry fn))
         (d0  (mir-new-value fn))
         (d1  (mir-new-value fn))
         (d2  (mir-new-value fn))
         (i0  (mir-emit blk :const :dst d0 :srcs (list (make-mir-const :value 1))))
         (i1  (mir-emit blk :const :dst d1 :srcs (list (make-mir-const :value 2))))
         (i2  (mir-emit blk :add   :dst d2 :srcs (list d0 d1))))
    (assert-= 3 (length (mirb-insts blk)))
    (assert-eq i0 (first  (mirb-insts blk)))
    (assert-eq i1 (second (mirb-insts blk)))
    (assert-eq i2 (third  (mirb-insts blk)))))

;;;; ─── SSA variable tracking ────────────────────────────────────────────

(deftest mir-ssa-variable-read-write
  "mir-write/read-var: same block returns value; single predecessor propagates without phi."
  (let* ((fn   (mir-make-function :f))
         (blk  (mirf-entry fn))
         (v    (mir-new-value fn :name :x)))
    (mir-write-var fn :x blk v)
    (assert-eq v (mir-read-var fn :x blk)))
  (let* ((fn    (mir-make-function :f))
         (entry (mirf-entry fn))
         (b1    (mir-new-block fn :label :b1))
         (v     (mir-new-value fn :name :y)))
    (mir-seal-block fn entry)
    (mir-seal-block fn b1)
    (mir-add-succ entry b1)
    (mir-write-var fn :y entry v)
    (assert-eq v (mir-read-var fn :y b1))
    (assert-null (mirb-phis b1))))

(deftest mir-seal-block-resolves-incomplete-phi
  "Sealing a block with multiple predecessors resolves incomplete phi nodes."
  (let* ((fn    (mir-make-function :f))
         (entry (mirf-entry fn))
         (b1    (mir-new-block fn :label :b1))
         (b2    (mir-new-block fn :label :b2))
         (merge (mir-new-block fn :label :merge))
         (v1    (mir-new-value fn :name :val1))
         (v2    (mir-new-value fn :name :val2)))
    ;; CFG: entry → b1, entry → b2, b1 → merge, b2 → merge
    (mir-add-succ entry b1)
    (mir-add-succ entry b2)
    (mir-add-succ b1 merge)
    (mir-add-succ b2 merge)
    ;; Seal all blocks except merge (its preds must be known first)
    (mir-seal-block fn entry)
    (mir-seal-block fn b1)
    (mir-seal-block fn b2)
    ;; Write different definitions in b1 and b2
    (mir-write-var fn :z b1 v1)
    (mir-write-var fn :z b2 v2)
    ;; Now read :z in merge (not sealed yet) — should create incomplete phi
    (let ((phi-val (mir-read-var fn :z merge)))
      (assert-false (null phi-val))
      ;; There should be one phi in merge's phis or incomplete-phis
      (assert-false (= 0 (hash-table-count (mirb-incomplete-phis merge))))
      ;; Seal merge — should resolve the phi
      (mir-seal-block fn merge)
      (assert-= 0 (hash-table-count (mirb-incomplete-phis merge)))
      ;; After sealing, the phi should have 2 src operands
      (when (mirb-phis merge)
        (let ((phi-inst (first (mirb-phis merge))))
          (assert-= 2 (length (miri-srcs phi-inst))))))))

;;;; ─── CFG utilities ────────────────────────────────────────────────────

(deftest mir-rpo-single-block
  "mir-rpo on a function with only the entry block returns that block."
  (let* ((fn    (mir-make-function :f))
         (entry (mirf-entry fn))
         (rpo   (mir-rpo fn)))
    (assert-= 1 (length rpo))
    (assert-eq entry (first rpo))))

(deftest mir-linear-chain-rpo-and-dominators
  "Linear chain b0→b1→b2: RPO includes all 3 in order; idom(b1)=b0, idom(b2)=b1."
  (let* ((fn  (mir-make-function :f))
         (b0  (mirf-entry fn))
         (b1  (mir-new-block fn :label :b1))
         (b2  (mir-new-block fn :label :b2)))
    (mir-add-succ b0 b1)
    (mir-add-succ b1 b2)
    (let ((rpo (mir-rpo fn)))
      (assert-= 3 (length rpo))
      (let ((pos (lambda (b) (position b rpo :test #'eq))))
        (assert-true (< (funcall pos b0) (funcall pos b1)))
        (assert-true (< (funcall pos b1) (funcall pos b2)))))
    (let ((idom (mir-dominators fn)))
      (assert-eq b0 (gethash (mirb-id b0) idom))
      (assert-eq b0 (gethash (mirb-id b1) idom))
      (assert-eq b1 (gethash (mirb-id b2) idom)))))

(deftest mir-diamond-cfg-rpo-and-dominators
  "Diamond CFG (entry→then/else→merge): RPO visits all 4 blocks with entry first; merge dominated by entry."
  (let* ((fn    (mir-make-function :f))
         (entry (mirf-entry fn))
         (then  (mir-new-block fn :label :then))
         (else  (mir-new-block fn :label :else))
         (merge (mir-new-block fn :label :merge)))
    (mir-add-succ entry then)
    (mir-add-succ entry else)
    (mir-add-succ then merge)
    (mir-add-succ else merge)
    (let ((rpo (mir-rpo fn)))
      (assert-= 4 (length rpo))
      (assert-eq entry (first rpo)))
    (let ((idom (mir-dominators fn)))
      (assert-eq entry (gethash (mirb-id merge) idom))
      (assert-eq entry (gethash (mirb-id then) idom))
      (assert-eq entry (gethash (mirb-id else) idom)))))

;;;; ─── mir-module ────────────────────────────────────────────────────────

(deftest mir-module-has-empty-functions-and-globals
  "make-mir-module creates a module with nil functions/globals and a non-nil string-table."
  (let ((m (make-mir-module)))
    (assert-null (mirm-functions m))
    (assert-null (mirm-globals m))
    (assert-false (null (mirm-string-table m)))))

(deftest mir-generic-ops-contains-all-core-ops
  "All core MIR ops (:add, :sub, ..., :nop) are members of *mir-generic-ops*."
  (dolist (op '(:add :sub :mul :div :mod :neg
                :band :bor :bxor :bnot
                :lt :le :gt :ge :eq :ne
                :load :store :alloca
                :call :tail-call :ret :jump :branch
                :phi :values :mv-bind :safepoint :nop))
    (assert-true (member op *mir-generic-ops*))))


;;; ─── %mir-rpo-dfs (extracted helper) ────────────────────────────────────────

(deftest mir-rpo-dfs-single-block
  "%mir-rpo-dfs on a single block appends it to result-cell and marks visited."
  (let* ((fn    (mir-make-function :f))
         (entry (mirf-entry fn))
         (visited (make-hash-table))
         (cell    (list nil)))
    (cl-cc/mir::%mir-rpo-dfs entry visited cell)
    (assert-equal (list entry) (car cell))
    (assert-true  (gethash (mirb-id entry) visited))))

(deftest mir-rpo-dfs-no-revisit
  "%mir-rpo-dfs: calling twice on same block only appends it once."
  (let* ((fn    (mir-make-function :f))
         (entry (mirf-entry fn))
         (visited (make-hash-table))
         (cell    (list nil)))
    (cl-cc/mir::%mir-rpo-dfs entry visited cell)
    (cl-cc/mir::%mir-rpo-dfs entry visited cell)
    (assert-= 1 (length (car cell)))))

(deftest mir-rpo-dfs-chain-post-order
  "%mir-rpo-dfs currently accumulates helper traversal order [ENTRY B C]."
  (let* ((fn (mir-make-function :f))
         (a  (mirf-entry fn))
         (b  (mir-new-block fn :label :b))
         (c  (mir-new-block fn :label :c)))
    (mir-add-succ a b)
    (mir-add-succ b c)
    (let ((visited (make-hash-table))
          (cell    (list nil)))
      (cl-cc/mir::%mir-rpo-dfs a visited cell)
      (assert-equal (list a b c) (car cell)))))
