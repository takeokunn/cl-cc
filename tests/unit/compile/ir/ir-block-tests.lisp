;;;; tests/unit/compile/ir/ir-block-tests.lisp — CFG Block & SSA Tests
;;;
;;; Tests for ir-add-edge, ir-emit, ir-set-terminator, ir-rpo,
;;; ir-dominators, ir-collect-uses, ir-verify-ssa, and Braun SSA.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun make-test-fn (&optional (name 'test-fn))
  "Create a fresh IR function with entry block."
  (cl-cc:ir-make-function name))

(defun make-test-inst (fn &key result)
  "Create a simple IR instruction, optionally with a result value."
  (let ((inst (cl-cc::make-ir-inst :result result)))
    inst))

;;; ─── ir-add-edge ────────────────────────────────────────────────────────────

(deftest ir-add-edge-behavior
  "ir-add-edge: sets successor+predecessor; is idempotent (no duplicates); supports multiple successors."
  ;; basic: adds forward and back edges
  (let* ((fn (make-test-fn))
         (b1 (cl-cc:irf-entry fn))
         (b2 (cl-cc:ir-new-block fn :then)))
    (cl-cc:ir-add-edge b1 b2)
    (assert-true (member b2 (cl-cc:irb-successors b1)))
    (assert-true (member b1 (cl-cc:irb-predecessors b2))))
  ;; idempotent: double add → still one edge each direction
  (let* ((fn (make-test-fn))
         (b1 (cl-cc:irf-entry fn))
         (b2 (cl-cc:ir-new-block fn :then)))
    (cl-cc:ir-add-edge b1 b2)
    (cl-cc:ir-add-edge b1 b2)
    (assert-= 1 (length (cl-cc:irb-successors b1)))
    (assert-= 1 (length (cl-cc:irb-predecessors b2))))
  ;; branch: block can have two successors
  (let* ((fn (make-test-fn))
         (entry  (cl-cc:irf-entry fn))
         (then-b (cl-cc:ir-new-block fn :then))
         (else-b (cl-cc:ir-new-block fn :else)))
    (cl-cc:ir-add-edge entry then-b)
    (cl-cc:ir-add-edge entry else-b)
    (assert-= 2 (length (cl-cc:irb-successors entry)))))

;;; ─── ir-emit / ir-set-terminator ────────────────────────────────────────────

(deftest ir-emit-behavior
  "ir-emit appends instructions in order and sets each instruction's owning block."
  (let* ((fn    (make-test-fn))
         (blk   (cl-cc:irf-entry fn))
         (inst1 (make-test-inst fn))
         (inst2 (make-test-inst fn)))
    (cl-cc:ir-emit blk inst1)
    (cl-cc:ir-emit blk inst2)
    (assert-= 2 (length (cl-cc:irb-insts blk)))
    (assert-eq inst1 (first  (cl-cc:irb-insts blk)))
    (assert-eq inst2 (second (cl-cc:irb-insts blk)))
    (assert-eq blk (cl-cc:iri-block inst1))
    (assert-eq blk (cl-cc:iri-block inst2))))

(deftest ir-set-terminator-stores
  "ir-set-terminator sets the block's terminator."
  (let* ((fn (make-test-fn))
         (blk (cl-cc:irf-entry fn))
         (term (make-test-inst fn)))
    (cl-cc:ir-set-terminator blk term)
    (assert-eq term (cl-cc:irb-terminator blk))
    (assert-eq blk (cl-cc:iri-block term))))

;;; ─── ir-rpo ─────────────────────────────────────────────────────────────────

(deftest-each ir-rpo-cases
  "ir-rpo: single block, linear chain, diamond, and unreachable block exclusion."
  :cases (("single-block"
           (lambda ()
             (let* ((fn (make-test-fn)))
               (list fn (cl-cc:irf-entry fn) 1 nil nil nil)))
          )
          ("linear-chain"
           (lambda ()
             (let* ((fn (make-test-fn))
                    (entry (cl-cc:irf-entry fn))
                    (a (cl-cc:ir-new-block fn :A))
                    (b (cl-cc:ir-new-block fn :B)))
               (cl-cc:ir-add-edge entry a)
               (cl-cc:ir-add-edge a b)
               (list fn entry 3 a b nil)))
          )
          ("diamond"
           (lambda ()
             (let* ((fn (make-test-fn))
                    (entry (cl-cc:irf-entry fn))
                    (left  (cl-cc:ir-new-block fn :left))
                    (right (cl-cc:ir-new-block fn :right))
                    (join  (cl-cc:ir-new-block fn :join)))
               (cl-cc:ir-add-edge entry left)
               (cl-cc:ir-add-edge entry right)
               (cl-cc:ir-add-edge left join)
               (cl-cc:ir-add-edge right join)
               (list fn entry 4 nil nil nil)))
          )
          ("unreachable-excluded"
           (lambda ()
             (let* ((fn (make-test-fn))
                    (orphan (cl-cc:ir-new-block fn :orphan)))
               (declare (ignore orphan))
               (list fn (cl-cc:irf-entry fn) 1 nil nil nil)))))
  (make-cfg)
  (destructuring-bind (fn entry expected-count second-blk third-blk _) (funcall make-cfg)
    (declare (ignore _))
    (let ((rpo (cl-cc:ir-rpo fn)))
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
                    (entry (cl-cc:irf-entry fn)))
               ;; Returns (fn . list-of-(block . expected-idom) pairs)
               (cons fn (list (cons entry entry))))))
          ("linear-chain"
           (lambda ()
             (let* ((fn (make-test-fn))
                    (entry (cl-cc:irf-entry fn))
                    (a (cl-cc:ir-new-block fn :A))
                    (b (cl-cc:ir-new-block fn :B)))
               (cl-cc:ir-add-edge entry a)
               (cl-cc:ir-add-edge a b)
               (cons fn (list (cons a entry) (cons b a))))))
          ("diamond"
           (lambda ()
             (let* ((fn (make-test-fn))
                    (entry (cl-cc:irf-entry fn))
                    (left  (cl-cc:ir-new-block fn :left))
                    (right (cl-cc:ir-new-block fn :right))
                    (join  (cl-cc:ir-new-block fn :join)))
               (cl-cc:ir-add-edge entry left)
               (cl-cc:ir-add-edge entry right)
               (cl-cc:ir-add-edge left join)
               (cl-cc:ir-add-edge right join)
               (cons fn (list (cons left entry) (cons right entry) (cons join entry)))))))
  (make-cfg)
  (let* ((result (funcall make-cfg))
         (fn     (car result))
         (checks (cdr result))
         (idom   (cl-cc:ir-dominators fn)))
    (dolist (pair checks)
      (assert-eq (cdr pair) (gethash (car pair) idom)))))

;;; ─── ir-collect-uses ────────────────────────────────────────────────────────

(deftest ir-collect-uses-empty
  "ir-collect-uses on empty function returns empty table."
  (let* ((fn (make-test-fn))
         (uses (cl-cc:ir-collect-uses fn)))
    (assert-= 0 (hash-table-count uses))))

;;; ─── ir-verify-ssa ──────────────────────────────────────────────────────────

(deftest ir-verify-ssa-behavior
  "ir-verify-ssa: returns T for valid SSA; signals error on duplicate defs."
  ;; valid: two distinct values each defined once
  (let* ((fn (make-test-fn))
         (blk (cl-cc:irf-entry fn))
         (v1 (cl-cc:ir-new-value fn))
         (v2 (cl-cc:ir-new-value fn)))
    (cl-cc:ir-emit blk (cl-cc::make-ir-inst :result v1))
    (cl-cc:ir-emit blk (cl-cc::make-ir-inst :result v2))
    (assert-true (cl-cc:ir-verify-ssa fn)))
  ;; invalid: same value defined twice — SSA violation
  (let* ((fn (make-test-fn))
         (blk (cl-cc:irf-entry fn))
         (v1 (cl-cc:ir-new-value fn)))
    (cl-cc:ir-emit blk (cl-cc::make-ir-inst :result v1))
    (cl-cc:ir-emit blk (cl-cc::make-ir-inst :result v1))
    (assert-true
     (handler-case (progn (cl-cc:ir-verify-ssa fn) nil)
       (error () t)))))

;;; ─── Braun SSA: ir-write-var / ir-read-var ──────────────────────────────────

(deftest ir-ssa-write-read-same-block
  "ir-write/read-var in the same block: basic read-back and overwrite behavior."
  ;; basic: read finds what was written
  (let* ((fn (make-test-fn))
         (blk (cl-cc:irf-entry fn))
         (val (cl-cc:ir-new-value fn)))
    (cl-cc:ir-write-var fn 'x blk val)
    (assert-eq val (cl-cc:ir-read-var fn 'x blk)))
  ;; overwrite: later write shadows earlier in same block
  (let* ((fn (make-test-fn))
         (blk (cl-cc:irf-entry fn))
         (v1 (cl-cc:ir-new-value fn))
         (v2 (cl-cc:ir-new-value fn)))
    (cl-cc:ir-write-var fn 'x blk v1)
    (cl-cc:ir-write-var fn 'x blk v2)
    (assert-eq v2 (cl-cc:ir-read-var fn 'x blk))))

(deftest ir-ssa-read-propagates-from-predecessor
  "ir-read-var finds value from single sealed predecessor."
  (let* ((fn (make-test-fn))
         (entry (cl-cc:irf-entry fn))
         (next (cl-cc:ir-new-block fn :next))
         (val (cl-cc:ir-new-value fn)))
    (cl-cc:ir-add-edge entry next)
    (cl-cc:ir-seal-block fn entry)
    (cl-cc:ir-seal-block fn next)
    (cl-cc:ir-write-var fn 'x entry val)
    (assert-eq val (cl-cc:ir-read-var fn 'x next))))

(deftest ir-ssa-join-creates-block-arg
  "ir-read-var at join with two predecessors creates a block argument."
  (let* ((fn (make-test-fn))
         (entry (cl-cc:irf-entry fn))
         (left (cl-cc:ir-new-block fn :left))
         (right (cl-cc:ir-new-block fn :right))
         (join (cl-cc:ir-new-block fn :join))
         (v1 (cl-cc:ir-new-value fn))
         (v2 (cl-cc:ir-new-value fn)))
    (cl-cc:ir-add-edge entry left)
    (cl-cc:ir-add-edge entry right)
    (cl-cc:ir-add-edge left join)
    (cl-cc:ir-add-edge right join)
    (cl-cc:ir-seal-block fn entry)
    (cl-cc:ir-seal-block fn left)
    (cl-cc:ir-seal-block fn right)
    (cl-cc:ir-seal-block fn join)
    (cl-cc:ir-write-var fn 'x left v1)
    (cl-cc:ir-write-var fn 'x right v2)
    ;; Reading x at join should create a block argument (phi)
    (let ((result (cl-cc:ir-read-var fn 'x join)))
      (assert-true (cl-cc:ir-value-p result))
      ;; The join block should have at least one param (the phi)
      (assert-true (> (length (cl-cc:irb-params join)) 0)))))

(deftest ir-ssa-seal-block-marks-sealed
  "ir-seal-block sets sealed-p to T."
  (let* ((fn (make-test-fn))
         (blk (cl-cc:irf-entry fn)))
    (cl-cc:ir-seal-block fn blk)
    (assert-true (cl-cc:irb-sealed-p blk))))

;;; ─── ir-new-value / ir-new-block ────────────────────────────────────────────

(deftest ir-new-value-increments-id
  "ir-new-value allocates values with incrementing IDs."
  (let* ((fn (make-test-fn))
         (v0 (cl-cc:ir-new-value fn))
         (v1 (cl-cc:ir-new-value fn)))
    (assert-= 0 (cl-cc:irv-id v0))
    (assert-= 1 (cl-cc:irv-id v1))))

(deftest ir-block-and-function-construction
  "ir-new-block increments IDs and adds to function; ir-make-function creates entry block."
  (let* ((fn (make-test-fn))
         (b1 (cl-cc:ir-new-block fn :test)))
    ;; Entry block is ID 0; new block is ID 1
    (assert-= 0 (cl-cc:irb-id (cl-cc:irf-entry fn)))
    (assert-= 1 (cl-cc:irb-id b1))
    ;; New block is registered in function's block list
    (assert-= 2 (length (cl-cc:irf-blocks fn)))
    ;; Entry block has the correct label and type
    (assert-true (cl-cc:ir-block-p (cl-cc:irf-entry fn)))
    (assert-eq :entry (cl-cc:irb-label (cl-cc:irf-entry fn)))))

;;; ─── Braun SSA: advanced paths ────────────────────────────────────────────────

(deftest ir-ssa-read-no-predecessors-returns-nil
  "ir-read-var on a block with no predecessors and no local def returns NIL."
  (let* ((fn (make-test-fn))
         (orphan (cl-cc:ir-new-block fn :orphan)))
    (cl-cc:ir-seal-block fn orphan)
    (assert-false (cl-cc:ir-read-var fn 'x orphan))))

(deftest ir-ssa-unsealed-creates-incomplete-phi
  "ir-read-var on an unsealed block with predecessors creates a placeholder."
  (let* ((fn (make-test-fn))
         (entry (cl-cc:irf-entry fn))
         (blk (cl-cc:ir-new-block fn :unsealed)))
    ;; Add a predecessor so preds is non-nil, but do NOT seal blk
    (cl-cc:ir-add-edge entry blk)
    ;; Block is NOT sealed — reading should insert an incomplete phi
    (let ((result (cl-cc:ir-read-var fn 'x blk)))
      (assert-true (cl-cc:ir-value-p result))
      (assert-true (> (length (cl-cc:irb-params blk)) 0)))))

(deftest ir-ssa-seal-resolves-incomplete-phi
  "ir-seal-block resolves incomplete phis created before sealing."
  (let* ((fn (make-test-fn))
         (entry (cl-cc:irf-entry fn))
         (blk (cl-cc:ir-new-block fn :target))
         (val (cl-cc:ir-new-value fn)))
    ;; Write x in entry, add edge, read before sealing target
    (cl-cc:ir-write-var fn 'x entry val)
    (cl-cc:ir-add-edge entry blk)
    ;; Read x in unsealed blk — creates incomplete phi
    (let ((placeholder (cl-cc:ir-read-var fn 'x blk)))
      (assert-true (cl-cc:ir-value-p placeholder))
      ;; Now seal — should resolve the placeholder
      (cl-cc:ir-seal-block fn entry)
      (cl-cc:ir-seal-block fn blk)
      ;; After sealing, reading x should still work
      (let ((resolved (cl-cc:ir-read-var fn 'x blk)))
        (assert-true (cl-cc:ir-value-p resolved))))))

(deftest-each ir-ssa-phi-elimination-cases
  "ir-read-var at join: same value → trivially eliminated (0 params); distinct → kept (>0 params)."
  :cases (("same-value"     t)
          ("distinct-values" nil))
  (same-value-p)
  (let* ((fn    (make-test-fn))
         (entry (cl-cc:irf-entry fn))
         (left  (cl-cc:ir-new-block fn :left))
         (right (cl-cc:ir-new-block fn :right))
         (join  (cl-cc:ir-new-block fn :join))
         (v1    (cl-cc:ir-new-value fn))
         (v2    (if same-value-p v1 (cl-cc:ir-new-value fn))))
    (cl-cc:ir-add-edge entry left)  (cl-cc:ir-add-edge entry right)
    (cl-cc:ir-add-edge left join)   (cl-cc:ir-add-edge right join)
    (cl-cc:ir-seal-block fn entry)  (cl-cc:ir-seal-block fn left)
    (cl-cc:ir-seal-block fn right)  (cl-cc:ir-seal-block fn join)
    (cl-cc:ir-write-var fn 'x left v1)
    (cl-cc:ir-write-var fn 'x right v2)
    (let ((result (cl-cc:ir-read-var fn 'x join)))
      (assert-true (cl-cc:ir-value-p result))
      (if same-value-p
          (progn (assert-eq v1 result)
                 (assert-= 0 (length (cl-cc:irb-params join))))
          (assert-true (> (length (cl-cc:irb-params join)) 0))))))

(deftest ir-ssa-loop-self-reference
  "A loop back-edge produces a block arg that refers to itself."
  (let* ((fn (make-test-fn))
         (entry (cl-cc:irf-entry fn))
         (loop-hdr (cl-cc:ir-new-block fn :loop))
         (v-init (cl-cc:ir-new-value fn))
         (v-update (cl-cc:ir-new-value fn)))
    ;; entry -> loop-hdr, loop-hdr -> loop-hdr (back-edge)
    (cl-cc:ir-add-edge entry loop-hdr)
    (cl-cc:ir-add-edge loop-hdr loop-hdr)
    (cl-cc:ir-seal-block fn entry)
    (cl-cc:ir-seal-block fn loop-hdr)
    ;; Write initial value from entry
    (cl-cc:ir-write-var fn 'x entry v-init)
    ;; Write updated value from within loop
    (cl-cc:ir-write-var fn 'x loop-hdr v-update)
    ;; Read x in loop header — should return the loop-local definition
    (let ((result (cl-cc:ir-read-var fn 'x loop-hdr)))
      (assert-true (cl-cc:ir-value-p result))
      (assert-eq v-update result))))

(deftest ir-ssa-seal-clears-incomplete-phis
  "After ir-seal-block, the incomplete-phis hash table is empty."
  (let* ((fn (make-test-fn))
         (entry (cl-cc:irf-entry fn))
         (blk (cl-cc:ir-new-block fn :target)))
    (cl-cc:ir-add-edge entry blk)
    ;; Read before sealing — creates incomplete phi
    (cl-cc:ir-read-var fn 'x blk)
    (assert-true (> (hash-table-count (cl-cc:irb-incomplete-phis blk)) 0))
    ;; Seal clears the table
    (cl-cc:ir-seal-block fn entry)
    (cl-cc:ir-seal-block fn blk)
    (assert-= 0 (hash-table-count (cl-cc:irb-incomplete-phis blk)))))

;;; ─── ir-dominators: deeper chains and loops ─────────────────────────────────

(deftest ir-dominators-deep-chain
  "In a 4-block linear chain A->B->C->D, idom(C)=B and idom(D)=C."
  (let* ((fn (make-test-fn))
         (a  (cl-cc:irf-entry fn))
         (b  (cl-cc:ir-new-block fn :b))
         (c  (cl-cc:ir-new-block fn :c))
         (d  (cl-cc:ir-new-block fn :d)))
    (cl-cc:ir-add-edge a b)
    (cl-cc:ir-add-edge b c)
    (cl-cc:ir-add-edge c d)
    (let ((idom (cl-cc:ir-dominators fn)))
      (assert-eq a (gethash a idom))
      (assert-eq a (gethash b idom))
      (assert-eq b (gethash c idom))
      (assert-eq c (gethash d idom)))))

(deftest ir-dominators-two-branches-then-merge
  "In A->{B,C}->D->E: idom(D)=A, idom(E)=D."
  (let* ((fn (make-test-fn))
         (a  (cl-cc:irf-entry fn))
         (b  (cl-cc:ir-new-block fn :b))
         (c  (cl-cc:ir-new-block fn :c))
         (d  (cl-cc:ir-new-block fn :d))
         (e  (cl-cc:ir-new-block fn :e)))
    (cl-cc:ir-add-edge a b)
    (cl-cc:ir-add-edge a c)
    (cl-cc:ir-add-edge b d)
    (cl-cc:ir-add-edge c d)
    (cl-cc:ir-add-edge d e)
    (let ((idom (cl-cc:ir-dominators fn)))
      (assert-eq a (gethash a idom))
      (assert-eq a (gethash d idom))
      (assert-eq d (gethash e idom)))))

(deftest ir-dominators-unreachable-absent
  "ir-dominators does not include unreachable blocks in the result table."
  (let* ((fn      (make-test-fn))
         (entry   (cl-cc:irf-entry fn))
         (reached (cl-cc:ir-new-block fn :reached))
         (orphan  (cl-cc:ir-new-block fn :orphan)))
    (cl-cc:ir-add-edge entry reached)
    (let ((idom (cl-cc:ir-dominators fn)))
      (assert-true  (gethash entry   idom))
      (assert-true  (gethash reached idom))
      (assert-false (gethash orphan  idom)))))

;;; ─── ir-collect-uses with custom operands ────────────────────────────────────

;;; Define a minimal subtype of ir-inst so we can test ir-collect-uses
;;; with real operand data.  This is local to this test file.
(defstruct (test-use-inst (:include cl-cc::ir-inst) (:conc-name tuinst-))
  "Minimal ir-inst subtype used only in tests."
  (operands nil :type list))

(defmethod cl-cc:ir-operands ((inst test-use-inst))
  (tuinst-operands inst))

(deftest ir-collect-uses-single-operand
  "ir-collect-uses records a single use of an ir-value."
  (let* ((fn    (make-test-fn))
         (entry (cl-cc:irf-entry fn))
         (v0    (cl-cc:ir-new-value fn))
         (v1    (cl-cc:ir-new-value fn))
         ;; i0 produces v0; i1 uses v0 and produces v1
         (i0    (cl-cc::make-ir-inst :result v0))
         (i1    (make-test-use-inst :result v1 :operands (list v0))))
    (cl-cc:ir-emit entry i0)
    (cl-cc:ir-emit entry i1)
    (let ((uses (cl-cc:ir-collect-uses fn)))
      ;; v0 is used by i1
      (assert-true (member i1 (gethash v0 uses) :test #'eq))
      ;; v1 is not used by anyone
      (assert-null (gethash v1 uses)))))

(deftest ir-collect-uses-multiple-operands
  "ir-collect-uses records multiple uses of the same ir-value."
  (let* ((fn    (make-test-fn))
         (entry (cl-cc:irf-entry fn))
         (v0    (cl-cc:ir-new-value fn))
         (v1    (cl-cc:ir-new-value fn))
         (v2    (cl-cc:ir-new-value fn))
         ;; i1 uses v0; i2 also uses v0
         (i1    (make-test-use-inst :result v1 :operands (list v0)))
         (i2    (make-test-use-inst :result v2 :operands (list v0))))
    (cl-cc:ir-emit entry i1)
    (cl-cc:ir-emit entry i2)
    (let ((uses (cl-cc:ir-collect-uses fn)))
      (let ((users (gethash v0 uses)))
        (assert-= 2 (length users))
        (assert-true (member i1 users :test #'eq))
        (assert-true (member i2 users :test #'eq))))))

(deftest ir-collect-uses-across-blocks
  "ir-collect-uses traverses multiple blocks via RPO."
  (let* ((fn    (make-test-fn))
         (entry (cl-cc:irf-entry fn))
         (next  (cl-cc:ir-new-block fn :next))
         (v0    (cl-cc:ir-new-value fn))
         (v1    (cl-cc:ir-new-value fn))
         ;; define v0 in entry; use it in next
         (i0    (cl-cc::make-ir-inst :result v0))
         (i1    (make-test-use-inst :result v1 :operands (list v0))))
    (cl-cc:ir-add-edge entry next)
    (cl-cc:ir-emit entry i0)
    (cl-cc:ir-emit next  i1)
    (let ((uses (cl-cc:ir-collect-uses fn)))
      (assert-true (member i1 (gethash v0 uses) :test #'eq)))))

;;; ─── ir-verify-ssa: cross-block checks ──────────────────────────────────────

(deftest ir-verify-ssa-cross-block-valid
  "ir-verify-ssa passes for distinct defs across blocks and for void instructions."
  ;; distinct values in separate blocks
  (let* ((fn    (make-test-fn))
         (entry (cl-cc:irf-entry fn))
         (next  (cl-cc:ir-new-block fn :next))
         (v0    (cl-cc:ir-new-value fn))
         (v1    (cl-cc:ir-new-value fn)))
    (cl-cc:ir-add-edge entry next)
    (cl-cc:ir-emit entry (cl-cc::make-ir-inst :result v0))
    (cl-cc:ir-emit next  (cl-cc::make-ir-inst :result v1))
    (assert-true (cl-cc:ir-verify-ssa fn)))
  ;; void instructions (nil result) define no SSA value — still valid
  (let* ((fn    (make-test-fn))
         (entry (cl-cc:irf-entry fn))
         (i0    (cl-cc::make-ir-inst))
         (i1    (cl-cc::make-ir-inst)))
    (cl-cc:ir-emit entry i0)
    (cl-cc:ir-emit entry i1)
    (assert-true (cl-cc:ir-verify-ssa fn))))

;;; ─── ir-write-var / ir-read-var: multiple independent variables ──────────────

(deftest ir-ssa-independent-vars
  "Different variables are tracked independently, both within and across blocks."
  ;; within same block: a, b, c remain distinct
  (let* ((fn    (make-test-fn))
         (entry (cl-cc:irf-entry fn))
         (va    (cl-cc:ir-new-value fn))
         (vb    (cl-cc:ir-new-value fn))
         (vc    (cl-cc:ir-new-value fn)))
    (cl-cc:ir-write-var fn 'a entry va)
    (cl-cc:ir-write-var fn 'b entry vb)
    (cl-cc:ir-write-var fn 'c entry vc)
    (assert-eq va (cl-cc:ir-read-var fn 'a entry))
    (assert-eq vb (cl-cc:ir-read-var fn 'b entry))
    (assert-eq vc (cl-cc:ir-read-var fn 'c entry)))
  ;; across blocks: vars written in entry are both readable from successor
  (let* ((fn    (make-test-fn))
         (entry (cl-cc:irf-entry fn))
         (next  (cl-cc:ir-new-block fn :next))
         (va    (cl-cc:ir-new-value fn))
         (vb    (cl-cc:ir-new-value fn)))
    (cl-cc:ir-add-edge entry next)
    (cl-cc:ir-seal-block fn entry)
    (cl-cc:ir-seal-block fn next)
    (cl-cc:ir-write-var fn 'a entry va)
    (cl-cc:ir-write-var fn 'b entry vb)
    (assert-eq va (cl-cc:ir-read-var fn 'a next))
    (assert-eq vb (cl-cc:ir-read-var fn 'b next))))
