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

(deftest-each mir-op-effect-kind-classifies-core-ops
  "mir-op-effect-kind classifies target-neutral MIR operations for IR effect analysis."
  :cases (("pure arithmetic" :add :pure)
          ("division may raise" :div :control)
          ("modulo may raise" :mod :control)
          ("memory read" :load :read-only)
          ("memory write" :store :write-global)
          ("stack allocation" :alloca :alloc)
          ("control transfer" :branch :control)
          ("unknown call" :call :unknown)
          ("unknown op" :not-a-mir-op :unknown))
  (op expected)
  (assert-eq expected (cl-cc/mir:mir-op-effect-kind op)))

(deftest mir-inst-effect-kind-allows-meta-override
  "mir-inst-effect-kind respects precise metadata supplied by lowering passes."
  (let* ((fn (mir-make-function :effects))
         (blk (mirf-entry fn))
         (dst (mir-new-value fn))
         (call (mir-emit blk :call :dst dst :meta '(:effect-kind :read-only))))
    (assert-eq :unknown (cl-cc/mir:mir-op-effect-kind :call))
    (assert-eq :read-only (cl-cc/mir:mir-inst-effect-kind call))
    (assert-false (cl-cc/mir:mir-inst-pure-p call))
    (assert-false (cl-cc/mir:mir-inst-dce-eligible-p call))))

(deftest mir-inst-effect-kind-rejects-malformed-meta
  "Malformed instruction metadata falls back to the opcode effect without error."
  (let* ((fn (mir-make-function :effects))
         (blk (mirf-entry fn))
         (dst (mir-new-value fn))
         (call (mir-emit blk :call :dst dst :meta (cons :effect-kind :read-only)))
         (div (mir-emit blk :div :dst (mir-new-value fn)
                        :srcs (list (mir-new-value fn :type :integer)
                                    (mir-new-value fn :type :integer)))))
    (assert-eq :unknown (cl-cc/mir:mir-inst-effect-kind call))
    (assert-eq :control (cl-cc/mir:mir-inst-effect-kind div))
    (assert-false (cl-cc/mir:mir-inst-dce-eligible-p div))))

(deftest mir-propagate-types-updates-instructions-and-values
  "mir-propagate-types infers simple SSA value types over a straight-line MIR block."
  (let* ((fn (mir-make-function :typed))
         (blk (mirf-entry fn))
         (a (mir-new-value fn))
         (b (mir-new-value fn))
         (sum (mir-new-value fn))
         (cmp (mir-new-value fn)))
    (mir-emit blk :const :dst a :srcs (list (make-mir-const :value 1 :type :integer)))
    (mir-emit blk :const :dst b :srcs (list (make-mir-const :value 2 :type :integer)))
    (let ((add-inst (mir-emit blk :add :dst sum :srcs (list a b)))
          (cmp-inst (mir-emit blk :lt :dst cmp :srcs (list sum b))))
      (cl-cc/mir:mir-propagate-types fn)
      (assert-eq :integer (miri-type add-inst))
      (assert-eq :integer (mirv-type sum))
      (assert-eq :boolean (miri-type cmp-inst))
      (assert-eq :boolean (mirv-type cmp)))))

(deftest mir-propagate-types-joins-phi-input-types-conservatively
  "Phi result types remain concrete only when all incoming value types agree."
  (let* ((fn (mir-make-function :phi-types))
         (entry (mirf-entry fn))
         (then (mir-new-block fn :label :then))
         (else (mir-new-block fn :label :else))
         (merge (mir-new-block fn :label :merge))
         (int-a (mir-new-value fn :type :integer))
         (int-b (mir-new-value fn :type :integer))
         (bool-c (mir-new-value fn :type :boolean))
         (phi-same-dst (mir-new-value fn))
         (phi-mixed-dst (mir-new-value fn))
         (phi-same (mir-emit merge :phi :dst phi-same-dst
                             :srcs (list (cons then int-a) (cons else int-b))))
         (phi-mixed (mir-emit merge :phi :dst phi-mixed-dst
                              :srcs (list (cons then int-a) (cons else bool-c)))))
    (mir-add-succ entry then)
    (mir-add-succ entry else)
    (mir-add-succ then merge)
    (mir-add-succ else merge)
    (cl-cc/mir:mir-propagate-types fn)
    (assert-eq :integer (miri-type phi-same))
    (assert-eq :integer (mirv-type phi-same-dst))
    (assert-eq :any (miri-type phi-mixed))
    (assert-eq :any (mirv-type phi-mixed-dst))))

(deftest mir-propagate-types-reaches-fixed-point-for-late-producers
  "mir-propagate-types revisits phi nodes when later blocks refine source value types."
  (let* ((fn (mir-make-function :fixed-point-types))
         (entry (mirf-entry fn))
         (merge (mir-new-block fn :label :merge))
         (late (mir-new-block fn :label :late))
         (a (mir-new-value fn))
         (b (mir-new-value fn))
         (late-sum (mir-new-value fn))
         (phi-dst (mir-new-value fn))
         (phi (mir-emit merge :phi :dst phi-dst
                        :srcs (list (cons entry a) (cons late late-sum)))))
    (mir-add-succ entry merge)
    (mir-add-succ merge late)
    (mir-emit entry :const :dst a :srcs (list (make-mir-const :value 1 :type :integer)))
    (mir-emit late :const :dst b :srcs (list (make-mir-const :value 2 :type :integer)))
    (mir-emit late :add :dst late-sum :srcs (list a b))
    (cl-cc/mir:mir-propagate-types fn)
    (assert-eq :integer (miri-type phi))
    (assert-eq :integer (mirv-type phi-dst))))


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
