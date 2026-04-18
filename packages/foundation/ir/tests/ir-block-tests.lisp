;;;; tests/unit/compile/ir/ir-block-tests.lisp — CFG Block & SSA Tests
;;;
;;; Tests for ir-add-edge, ir-emit, ir-set-terminator, ir-rpo,
;;; ir-dominators, ir-collect-uses, ir-verify-ssa, and Braun SSA.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun make-test-fn (&optional (name 'test-fn))
  "Create a fresh IR function with entry block."
  (cl-cc/ir:ir-make-function name))

(defun make-test-inst (fn &key result)
  "Create a simple IR instruction, optionally with a result value."
  (let ((inst (cl-cc/ir:make-ir-inst :result result)))
    inst))

;;; ─── ir-add-edge ────────────────────────────────────────────────────────────

(deftest ir-add-edge-behavior
  "ir-add-edge: sets successor+predecessor; is idempotent (no duplicates); supports multiple successors."
  ;; basic: adds forward and back edges
  (let* ((fn (make-test-fn))
         (b1 (cl-cc/ir:irf-entry fn))
         (b2 (cl-cc/ir:ir-new-block fn :then)))
    (cl-cc/ir:ir-add-edge b1 b2)
    (assert-true (member b2 (cl-cc/ir:irb-successors b1)))
    (assert-true (member b1 (cl-cc/ir:irb-predecessors b2))))
  ;; idempotent: double add → still one edge each direction
  (let* ((fn (make-test-fn))
         (b1 (cl-cc/ir:irf-entry fn))
         (b2 (cl-cc/ir:ir-new-block fn :then)))
    (cl-cc/ir:ir-add-edge b1 b2)
    (cl-cc/ir:ir-add-edge b1 b2)
    (assert-= 1 (length (cl-cc/ir:irb-successors b1)))
    (assert-= 1 (length (cl-cc/ir:irb-predecessors b2))))
  ;; branch: block can have two successors
  (let* ((fn (make-test-fn))
         (entry  (cl-cc/ir:irf-entry fn))
         (then-b (cl-cc/ir:ir-new-block fn :then))
         (else-b (cl-cc/ir:ir-new-block fn :else)))
    (cl-cc/ir:ir-add-edge entry then-b)
    (cl-cc/ir:ir-add-edge entry else-b)
    (assert-= 2 (length (cl-cc/ir:irb-successors entry)))))

;;; ─── ir-emit / ir-set-terminator ────────────────────────────────────────────

(deftest ir-emit-behavior
  "ir-emit appends instructions in order and sets each instruction's owning block."
  (let* ((fn    (make-test-fn))
         (blk   (cl-cc/ir:irf-entry fn))
         (inst1 (make-test-inst fn))
         (inst2 (make-test-inst fn)))
    (cl-cc/ir:ir-emit blk inst1)
    (cl-cc/ir:ir-emit blk inst2)
    (assert-= 2 (length (cl-cc/ir:irb-insts blk)))
    (assert-eq inst1 (first  (cl-cc/ir:irb-insts blk)))
    (assert-eq inst2 (second (cl-cc/ir:irb-insts blk)))
    (assert-eq blk (cl-cc/ir:iri-block inst1))
    (assert-eq blk (cl-cc/ir:iri-block inst2))))

(deftest ir-set-terminator-stores
  "ir-set-terminator sets the block's terminator."
  (let* ((fn (make-test-fn))
         (blk (cl-cc/ir:irf-entry fn))
         (term (make-test-inst fn)))
    (cl-cc/ir:ir-set-terminator blk term)
    (assert-eq term (cl-cc/ir:irb-terminator blk))
    (assert-eq blk (cl-cc/ir:iri-block term))))

;;; ─── ir-rpo ─────────────────────────────────────────────────────────────────

(deftest-each ir-rpo-cases
  "ir-rpo: single block, linear chain, diamond, and unreachable block exclusion."
  :cases (("single-block"
           (lambda ()
             (let* ((fn (make-test-fn)))
               (list fn (cl-cc/ir:irf-entry fn) 1 nil nil nil)))
          )
          ("linear-chain"
           (lambda ()
             (let* ((fn (make-test-fn))
                    (entry (cl-cc/ir:irf-entry fn))
                    (a (cl-cc/ir:ir-new-block fn :A))
                    (b (cl-cc/ir:ir-new-block fn :B)))
               (cl-cc/ir:ir-add-edge entry a)
               (cl-cc/ir:ir-add-edge a b)
               (list fn entry 3 a b nil)))
          )
          ("diamond"
           (lambda ()
             (let* ((fn (make-test-fn))
                    (entry (cl-cc/ir:irf-entry fn))
                    (left  (cl-cc/ir:ir-new-block fn :left))
                    (right (cl-cc/ir:ir-new-block fn :right))
                    (join  (cl-cc/ir:ir-new-block fn :join)))
               (cl-cc/ir:ir-add-edge entry left)
               (cl-cc/ir:ir-add-edge entry right)
               (cl-cc/ir:ir-add-edge left join)
               (cl-cc/ir:ir-add-edge right join)
               (list fn entry 4 nil nil nil)))
          )
          ("unreachable-excluded"
           (lambda ()
             (let* ((fn (make-test-fn))
                    (orphan (cl-cc/ir:ir-new-block fn :orphan)))
               (declare (ignore orphan))
               (list fn (cl-cc/ir:irf-entry fn) 1 nil nil nil)))))
  (make-cfg)
  (destructuring-bind (fn entry expected-count second-blk third-blk _) (funcall make-cfg)
    (declare (ignore _))
    (let ((rpo (cl-cc/ir:ir-rpo fn)))
      (assert-= expected-count (length rpo))
      (assert-eq entry (first rpo))
      (when second-blk (assert-eq second-blk (second rpo)))
      (when third-blk  (assert-eq third-blk  (third  rpo))))))

;;; ─── ir-dominators ──────────────────────────────────────────────────────────

(deftest-each ir-dominators-cases
  "ir-dominators: self-domination, linear chain idom, and diamond join idom."
  :cases (("single-block"
           (lambda ()
             (let* ((fn (make-test-fn))
                    (entry (cl-cc/ir:irf-entry fn)))
               ;; Returns (fn . list-of-(block . expected-idom) pairs)
               (cons fn (list (cons entry entry))))))
          ("linear-chain"
           (lambda ()
             (let* ((fn (make-test-fn))
                    (entry (cl-cc/ir:irf-entry fn))
                    (a (cl-cc/ir:ir-new-block fn :A))
                    (b (cl-cc/ir:ir-new-block fn :B)))
               (cl-cc/ir:ir-add-edge entry a)
               (cl-cc/ir:ir-add-edge a b)
               (cons fn (list (cons a entry) (cons b a))))))
          ("diamond"
           (lambda ()
             (let* ((fn (make-test-fn))
                    (entry (cl-cc/ir:irf-entry fn))
                    (left  (cl-cc/ir:ir-new-block fn :left))
                    (right (cl-cc/ir:ir-new-block fn :right))
                    (join  (cl-cc/ir:ir-new-block fn :join)))
               (cl-cc/ir:ir-add-edge entry left)
               (cl-cc/ir:ir-add-edge entry right)
               (cl-cc/ir:ir-add-edge left join)
               (cl-cc/ir:ir-add-edge right join)
               (cons fn (list (cons left entry) (cons right entry) (cons join entry)))))))
  (make-cfg)
  (let* ((result (funcall make-cfg))
         (fn     (car result))
         (checks (cdr result))
         (idom   (cl-cc/ir:ir-dominators fn)))
    (dolist (pair checks)
      (assert-eq (cdr pair) (gethash (car pair) idom)))))

;;; ─── ir-collect-uses ────────────────────────────────────────────────────────

(deftest ir-collect-uses-empty
  "ir-collect-uses on empty function returns empty table."
  (let* ((fn (make-test-fn))
         (uses (cl-cc/ir:ir-collect-uses fn)))
    (assert-= 0 (hash-table-count uses))))

;;; ─── ir-verify-ssa ──────────────────────────────────────────────────────────

(deftest ir-verify-ssa-behavior
  "ir-verify-ssa: returns T for valid SSA; signals error on duplicate defs."
  ;; valid: two distinct values each defined once
  (let* ((fn (make-test-fn))
         (blk (cl-cc/ir:irf-entry fn))
         (v1 (cl-cc/ir:ir-new-value fn))
         (v2 (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-emit blk (cl-cc/ir:make-ir-inst :result v1))
    (cl-cc/ir:ir-emit blk (cl-cc/ir:make-ir-inst :result v2))
    (assert-true (cl-cc/ir:ir-verify-ssa fn)))
  ;; invalid: same value defined twice — SSA violation
  (let* ((fn (make-test-fn))
         (blk (cl-cc/ir:irf-entry fn))
         (v1 (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-emit blk (cl-cc/ir:make-ir-inst :result v1))
    (cl-cc/ir:ir-emit blk (cl-cc/ir:make-ir-inst :result v1))
    (assert-true
     (handler-case (progn (cl-cc/ir:ir-verify-ssa fn) nil)
       (error () t)))))

;;; ─── Braun SSA: ir-write-var / ir-read-var ──────────────────────────────────

(deftest ir-ssa-write-read-same-block
  "ir-write/read-var in the same block: basic read-back and overwrite behavior."
  ;; basic: read finds what was written
  (let* ((fn (make-test-fn))
         (blk (cl-cc/ir:irf-entry fn))
         (val (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-write-var fn 'x blk val)
    (assert-eq val (cl-cc/ir:ir-read-var fn 'x blk)))
  ;; overwrite: later write shadows earlier in same block
  (let* ((fn (make-test-fn))
         (blk (cl-cc/ir:irf-entry fn))
         (v1 (cl-cc/ir:ir-new-value fn))
         (v2 (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-write-var fn 'x blk v1)
    (cl-cc/ir:ir-write-var fn 'x blk v2)
    (assert-eq v2 (cl-cc/ir:ir-read-var fn 'x blk))))

(deftest ir-ssa-read-propagates-from-predecessor
  "ir-read-var finds value from single sealed predecessor."
  (let* ((fn (make-test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (next (cl-cc/ir:ir-new-block fn :next))
         (val (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-add-edge entry next)
    (cl-cc/ir:ir-seal-block fn entry)
    (cl-cc/ir:ir-seal-block fn next)
    (cl-cc/ir:ir-write-var fn 'x entry val)
    (assert-eq val (cl-cc/ir:ir-read-var fn 'x next))))

(deftest ir-ssa-join-creates-block-arg
  "ir-read-var at join with two predecessors creates a block argument."
  (let* ((fn (make-test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (left (cl-cc/ir:ir-new-block fn :left))
         (right (cl-cc/ir:ir-new-block fn :right))
         (join (cl-cc/ir:ir-new-block fn :join))
         (v1 (cl-cc/ir:ir-new-value fn))
         (v2 (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-add-edge entry left)
    (cl-cc/ir:ir-add-edge entry right)
    (cl-cc/ir:ir-add-edge left join)
    (cl-cc/ir:ir-add-edge right join)
    (cl-cc/ir:ir-seal-block fn entry)
    (cl-cc/ir:ir-seal-block fn left)
    (cl-cc/ir:ir-seal-block fn right)
    (cl-cc/ir:ir-seal-block fn join)
    (cl-cc/ir:ir-write-var fn 'x left v1)
    (cl-cc/ir:ir-write-var fn 'x right v2)
    ;; Reading x at join should create a block argument (phi)
    (let ((result (cl-cc/ir:ir-read-var fn 'x join)))
      (assert-true (cl-cc/ir:ir-value-p result))
      ;; The join block should have at least one param (the phi)
      (assert-true (> (length (cl-cc/ir:irb-params join)) 0)))))

(deftest ir-ssa-seal-block-marks-sealed
  "ir-seal-block sets sealed-p to T."
  (let* ((fn (make-test-fn))
         (blk (cl-cc/ir:irf-entry fn)))
    (cl-cc/ir:ir-seal-block fn blk)
    (assert-true (cl-cc/ir:irb-sealed-p blk))))

;;; ─── ir-new-value / ir-new-block ────────────────────────────────────────────

(deftest ir-new-value-increments-id
  "ir-new-value allocates values with incrementing IDs."
  (let* ((fn (make-test-fn))
         (v0 (cl-cc/ir:ir-new-value fn))
         (v1 (cl-cc/ir:ir-new-value fn)))
    (assert-= 0 (cl-cc/ir:irv-id v0))
    (assert-= 1 (cl-cc/ir:irv-id v1))))

(deftest ir-block-and-function-construction
  "ir-new-block increments IDs and adds to function; ir-make-function creates entry block."
  (let* ((fn (make-test-fn))
         (b1 (cl-cc/ir:ir-new-block fn :test)))
    ;; Entry block is ID 0; new block is ID 1
    (assert-= 0 (cl-cc/ir:irb-id (cl-cc/ir:irf-entry fn)))
    (assert-= 1 (cl-cc/ir:irb-id b1))
    ;; New block is registered in function's block list
    (assert-= 2 (length (cl-cc/ir:irf-blocks fn)))
    ;; Entry block has the correct label and type
    (assert-true (cl-cc/ir:ir-block-p (cl-cc/ir:irf-entry fn)))
    (assert-eq :entry (cl-cc/ir:irb-label (cl-cc/ir:irf-entry fn)))))

;;; ─── Braun SSA: advanced paths ────────────────────────────────────────────────

(deftest ir-ssa-read-no-predecessors-returns-nil
  "ir-read-var on a block with no predecessors and no local def returns NIL."
  (let* ((fn (make-test-fn))
         (orphan (cl-cc/ir:ir-new-block fn :orphan)))
    (cl-cc/ir:ir-seal-block fn orphan)
    (assert-false (cl-cc/ir:ir-read-var fn 'x orphan))))
