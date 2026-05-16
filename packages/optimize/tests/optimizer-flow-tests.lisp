;;;; tests/unit/optimize/optimizer-flow-tests.lisp
;;;; Unit tests for optimizer-flow.lisp — DCE, jump threading, loop transforms
;;;;
;;;; Covers: opt-pass-dce, opt-build-label-index, opt-thread-label,
;;;;   opt-pass-jump, %opt-rewrite-block-terminator, opt-pass-unreachable,
;;;;   loop-rotation, loop-peeling, loop-unrolling, code-sinking,
;;;;   opt-pass-dominated-type-check-elim, opt-passes-preserve-straight-line.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Test helpers ────────────────────────────────────────────────────────

(defun %make-test-basic-block (&key (successors nil) (predecessors nil)
                                    (loop-depth 0) (rpo-index 0)
                                    (instructions nil))
  "Build a minimal basic-block for testing."
  (cl-cc/optimize::make-basic-block
   :successors successors
   :predecessors predecessors
   :loop-depth loop-depth
   :rpo-index rpo-index
   :instructions instructions))

;;; ─── opt-pass-dce / opt-build-label-index / opt-thread-label ────────────

(deftest dce-eliminates-unread-const
  "opt-pass-dce removes a vm-const whose destination register is never subsequently read."
  (let* ((insts (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-const :dst :r1 :value 1)
                      (make-vm-ret   :reg :r1)))
         (result (cl-cc/optimize::opt-pass-dce insts)))
    (assert-false (some (lambda (i)
                          (and (typep i 'cl-cc/vm::vm-const)
                               (eq (cl-cc/vm::vm-dst i) :r0)))
                        result))
    (assert-true (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq (cl-cc/vm::vm-dst i) :r1)))
                       result))))

(deftest dce-keeps-read-const
  "opt-pass-dce preserves a vm-const whose destination is read by a later instruction."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-dce insts)))
    (assert-true (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq (cl-cc/vm::vm-dst i) :r0)))
                       result))
    (assert-true (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq (cl-cc/vm::vm-dst i) :r1)))
                       result))))

(deftest dce-eliminates-unread-move
  "opt-pass-dce removes a vm-move whose destination register is never subsequently read."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-move  :dst :r5 :src :r0)
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::opt-pass-dce insts)))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-move)) result))))

(deftest dce-nil-input-returns-nil
  "opt-pass-dce on an empty instruction list returns nil."
  (assert-null (cl-cc/optimize::opt-pass-dce nil)))

(deftest build-label-index-maps-names-to-positions
  "opt-build-label-index maps each label name to its 0-based position in the instruction vector."
  (let* ((lab (make-vm-label :name "loop"))
         (c   (make-vm-const :dst :r0 :value 1)))
    (multiple-value-bind (vec idx)
        (cl-cc/optimize::opt-build-label-index (list c lab))
      (assert-= 2 (length vec))
      (assert-= 1 (gethash "loop" idx)))))

(deftest build-label-index-empty-input
  "opt-build-label-index on an empty list returns an empty vector and an empty index."
  (multiple-value-bind (vec idx)
      (cl-cc/optimize::opt-build-label-index nil)
    (assert-= 0 (length vec))
    (assert-= 0 (hash-table-count idx))))

(deftest-each thread-label-returns-input-unchanged
  "opt-thread-label returns the queried label name unchanged — no jump chain, or unknown label."
  :cases (("no-chain"  (list (make-vm-label :name "end") (make-vm-ret :reg :r0))  "end")
          ("unknown"   nil                                                          "nowhere"))
  (insts query)
  (multiple-value-bind (vec idx)
      (cl-cc/optimize::opt-build-label-index insts)
    (assert-equal query (cl-cc/optimize::opt-thread-label query idx vec))))

;;; ─── opt-pass-jump / %opt-rewrite-block-terminator / opt-pass-unreachable ─

(deftest-each jump-pass-fallthrough-and-non-fallthrough
  "opt-pass-jump removes fallthrough jumps but preserves jumps over intervening instructions."
  :cases (("removes-fallthrough"
           (list (make-vm-jump  :label "next")
                 (make-vm-label :name  "next")
                 (make-vm-ret   :reg   :r0))
           nil)
          ("keeps-non-fallthrough"
           (list (make-vm-jump  :label "far")
                 (make-vm-const :dst :r0 :value 1)
                 (make-vm-label :name "far")
                 (make-vm-ret   :reg :r0))
           t))
  (insts expect-jump)
  (let ((result (cl-cc/optimize::opt-pass-jump insts)))
    (if expect-jump
        (assert-true  (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) result))
        (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) result)))))

(deftest jump-pass-threads-through-jump-only-block
  "opt-pass-jump rewrites a jump to a jump-only block to point directly to the final target."
  (let* ((insts (list (make-vm-const :dst :r0 :value 0)
                      (make-vm-jump  :label "middle")
                      (make-vm-label :name  "middle")
                      (make-vm-jump  :label "end")
                      (make-vm-label :name  "end")
                      (make-vm-ret   :reg   :r0)))
         (result (cl-cc/optimize::opt-pass-jump insts))
         (jumps  (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-jump)) result)))
    (when jumps
      (assert-equal "end" (cl-cc/vm::vm-label-name (first jumps))))))

(deftest opt-rewrite-terminator-vm-jump
  "%opt-rewrite-block-terminator rewrites a vm-jump to the new label when the old label matches."
  (let ((b (%make-test-basic-block)))
    (setf (cl-cc/optimize:bb-instructions b)
          (list (make-vm-const :dst :r0 :value 1)
                (make-vm-jump :label "old")))
    (cl-cc/optimize::%opt-rewrite-block-terminator b "old" "new")
    (let ((term (car (last (cl-cc/optimize:bb-instructions b)))))
      (assert-true (typep term 'cl-cc/vm::vm-jump))
      (assert-equal "new" (cl-cc/vm::vm-label-name term)))))

(deftest opt-rewrite-terminator-vm-jump-zero
  "%opt-rewrite-block-terminator rewrites a vm-jump-zero label while preserving the condition register."
  (let ((b (%make-test-basic-block)))
    (setf (cl-cc/optimize:bb-instructions b)
          (list (make-vm-jump-zero :reg :r0 :label "old")))
    (cl-cc/optimize::%opt-rewrite-block-terminator b "old" "new")
    (let ((term (car (cl-cc/optimize:bb-instructions b))))
      (assert-true (typep term 'cl-cc/vm::vm-jump-zero))
      (assert-equal "new" (cl-cc/vm::vm-label-name term))
      (assert-eq :r0 (cl-cc/vm::vm-reg term)))))

(deftest opt-rewrite-terminator-no-match-unchanged
  "%opt-rewrite-block-terminator leaves the block unchanged when the label does not match."
  (let ((b    (%make-test-basic-block))
        (orig (make-vm-jump :label "other")))
    (setf (cl-cc/optimize:bb-instructions b) (list orig))
    (cl-cc/optimize::%opt-rewrite-block-terminator b "old" "new")
    (assert-eq orig (car (cl-cc/optimize:bb-instructions b)))))

(deftest opt-jump-thread-table-covers-both-jump-types
  "*opt-jump-thread-table* contains exactly 2 entries: vm-jump and vm-jump-zero."
  (assert-= 2 (length cl-cc/optimize::*opt-jump-thread-table*))
  (assert-true (assoc 'vm-jump      cl-cc/optimize::*opt-jump-thread-table*))
  (assert-true (assoc 'vm-jump-zero cl-cc/optimize::*opt-jump-thread-table*)))

(deftest opt-thread-jump-returns-nil-for-fallthrough
  "%opt-thread-jump returns NIL when the target label immediately follows the jump (fallthrough)."
  (let* ((insts (list (make-vm-jump  :label "next")
                      (make-vm-label :name  "next")
                      (make-vm-ret   :reg   :r0))))
    (multiple-value-bind (vec idx) (cl-cc/optimize::opt-build-label-index insts)
      (assert-null (cl-cc/optimize::%opt-thread-jump
                    (make-vm-jump :label "next") vec 0 idx)))))

(deftest opt-thread-jump-returns-relabeled-when-threading
  "%opt-thread-jump returns a new vm-jump pointing to the final target when the target threads."
  (let* ((insts (list (make-vm-jump  :label "middle")
                      (make-vm-label :name  "middle")
                      (make-vm-jump  :label "end")
                      (make-vm-label :name  "end")
                      (make-vm-ret   :reg   :r0))))
    (multiple-value-bind (vec idx) (cl-cc/optimize::opt-build-label-index insts)
      (let ((result (cl-cc/optimize::%opt-thread-jump
                     (make-vm-jump :label "middle") vec 0 idx)))
        (assert-true result)
        (assert-equal "end" (cl-cc/vm::vm-label-name result))))))

(deftest opt-thread-jump-zero-always-returns-instruction
  "%opt-thread-jump-zero always returns an instruction — conditional jumps are never eliminated."
  (let* ((inst  (make-vm-jump-zero :reg :r0 :label "next"))
         (insts (list inst (make-vm-label :name "next") (make-vm-ret :reg :r0))))
    (multiple-value-bind (vec idx) (cl-cc/optimize::opt-build-label-index insts)
      (let ((result (cl-cc/optimize::%opt-thread-jump-zero inst vec 0 idx)))
        (assert-true result)
        (assert-true (typep result 'cl-cc/vm::vm-jump-zero))))))

(deftest jump-pass-propagates-constant-comparison-to-fallthrough
  "opt-pass-jump replaces a repeated comparison in the fallthrough successor with constant 1."
  (let* ((insts (list (make-vm-const :dst :i :value 1)
                      (make-vm-const :dst :lim :value 3)
                      (make-vm-lt :dst :c :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c :label "false")
                      (make-vm-label :name "true")
                      (make-vm-lt :dst :c2 :lhs :i :rhs :lim)
                      (make-vm-ret :reg :c2)
                      (make-vm-label :name "false")
                      (make-vm-ret :reg :c)))
         (out (cl-cc/optimize::opt-pass-jump insts)))
    (assert-true
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-const)
                  (eq (cl-cc/vm::vm-dst i) :c2)
                  (eql (cl-cc/vm::vm-value i) 1)))
           out))))

(deftest jump-pass-replaces-redundant-comparison-in-taken-successor
  "opt-pass-jump replaces a repeated comparison in the taken successor with constant 0."
  (let* ((insts (list (make-vm-const :dst :i :value 5)
                      (make-vm-const :dst :lim :value 3)
                      (make-vm-lt :dst :c :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c :label "false")
                      (make-vm-label :name "true")
                      (make-vm-ret :reg :c)
                      (make-vm-label :name "false")
                      (make-vm-lt :dst :c2 :lhs :i :rhs :lim)
                      (make-vm-ret :reg :c2)))
         (out (cl-cc/optimize::opt-pass-jump insts)))
    (assert-true
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-const)
                  (eq (cl-cc/vm::vm-dst i) :c2)
                  (eql (cl-cc/vm::vm-value i) 0)))
           out))))

(deftest jump-pass-kills-comparison-fact-on-source-redefinition
  "opt-pass-jump does not rewrite after a comparison source register is redefined."
  (let* ((insts (list (make-vm-const :dst :i :value 1)
                      (make-vm-const :dst :lim :value 3)
                      (make-vm-lt :dst :c :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c :label "false")
                      (make-vm-label :name "true")
                      (make-vm-ret :reg :c)
                      (make-vm-label :name "false")
                      (make-vm-const :dst :i :value 9)
                      (make-vm-lt :dst :c2 :lhs :i :rhs :lim)
                      (make-vm-ret :reg :c2)))
         (out (cl-cc/optimize::opt-pass-jump insts)))
    (assert-true
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-lt)
                  (eq (cl-cc/vm::vm-dst i) :c2)))
           out))
    (assert-false
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-const)
                  (eq (cl-cc/vm::vm-dst i) :c2)))
           out))))

(deftest jump-pass-does-not-propagate-across-loop-back-edge
  "opt-pass-jump does not carry comparison facts from a loop body back to the header."
  (let* ((insts (list (make-vm-const :dst :i :value 1)
                      (make-vm-const :dst :lim :value 3)
                      (make-vm-label :name "header")
                      (make-vm-lt :dst :c2 :lhs :i :rhs :lim)
                      (make-vm-lt :dst :c :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c :label "exit")
                      (make-vm-label :name "body")
                      (make-vm-lt :dst :body-c :lhs :i :rhs :lim)
                      (make-vm-jump :label "header")
                      (make-vm-label :name "exit")
                      (make-vm-ret :reg :c)))
         (out (cl-cc/optimize::opt-pass-jump insts)))
    (assert-true
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-lt)
                  (eq (cl-cc/vm::vm-dst i) :c2)))
           out))))

(deftest jump-pass-combines-chain-threading-with-value-propagation
  "opt-pass-jump still threads jump chains while propagating facts to the fallthrough successor."
  (let* ((insts (list (make-vm-const :dst :i :value 1)
                      (make-vm-const :dst :lim :value 3)
                      (make-vm-lt :dst :c :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c :label "middle")
                      (make-vm-label :name "true")
                      (make-vm-lt :dst :c2 :lhs :i :rhs :lim)
                      (make-vm-ret :reg :c2)
                      (make-vm-label :name "middle")
                      (make-vm-jump :label "final")
                      (make-vm-label :name "final")
                      (make-vm-ret :reg :c)))
         (out (cl-cc/optimize::opt-pass-jump insts)))
    (assert-true
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-jump-zero)
                  (equal (cl-cc/vm::vm-label-name i) "final")))
           out))
    (assert-true
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-const)
                  (eq (cl-cc/vm::vm-dst i) :c2)
                  (eql (cl-cc/vm::vm-value i) 1)))
           out))))

(deftest loop-rotation-rotates-simple-while-shape
  "opt-pass-loop-rotation rewrites a simple while-shape into guard+body form."
  (let* ((insts (list (make-vm-label :name "Lh")
                      (cl-cc:make-vm-integer-p :dst :r1 :src :r0)
                      (make-vm-jump-zero :reg :r1 :label "Lexit")
                      (make-vm-add :dst :r0 :lhs :r0 :rhs :r2)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :r0)))
         (out (cl-cc/optimize::opt-pass-loop-rotation insts))
         (first-inst (first out))
         (jumps-to-lh (count-if (lambda (i)
                                  (and (typep i 'cl-cc/vm::vm-jump)
                                       (equal (cl-cc/vm::vm-label-name i) "Lh")))
                                out))
         (guard-jumps (count-if (lambda (i)
                                  (and (typep i 'cl-cc/vm::vm-jump-zero)
                                       (equal (cl-cc/vm::vm-label-name i) "Lexit")))
                                out)))
    (assert-true (typep first-inst 'cl-cc/vm::vm-jump))
    (assert-= 0 jumps-to-lh)
    (assert-= 1 guard-jumps)))

(deftest loop-rotation-noop-on-nonmatching-shape
  "opt-pass-loop-rotation leaves non-matching code unchanged in length."
  (let* ((insts (list (make-vm-label :name "A")
                      (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump :label "B")
                      (make-vm-label :name "B")
                      (make-vm-ret :reg :r0)))
         (out (cl-cc/optimize::opt-pass-loop-rotation insts)))
    (assert-= (length insts) (length out))))

(deftest loop-peeling-duplicates-first-iteration-for-simple-while
  "opt-pass-loop-peeling duplicates first iteration for simple while shape."
  (let* ((insts (list (make-vm-label :name "Lh")
                      (cl-cc:make-vm-integer-p :dst :r1 :src :r0)
                      (make-vm-jump-zero :reg :r1 :label "Lexit")
                      (make-vm-add :dst :r0 :lhs :r0 :rhs :r2)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :r0)))
         (out (cl-cc/optimize::opt-pass-loop-peeling insts))
         (jz-count (count-if (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out))
         (add-count (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out)))
    (assert-= 2 jz-count)
    (assert-= 2 add-count)))

(deftest loop-peeling-noop-on-nonmatching-shape
  "opt-pass-loop-peeling is a no-op on nonmatching control-flow shape."
  (let* ((insts (list (make-vm-label :name "A")
                      (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump :label "B")
                      (make-vm-label :name "B")
                      (make-vm-ret :reg :r0)))
         (out (cl-cc/optimize::opt-pass-loop-peeling insts)))
    (assert-= (length insts) (length out))))

(deftest loop-unrolling-fully-unrolls-small-counted-loop
  "opt-pass-loop-unrolling fully unrolls a tiny counted loop with known trip count."
  (let* ((insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :lim :value 3)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-label :name "Lh")
                      (make-vm-lt :dst :c :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c :label "Lexit")
                      (make-vm-add :dst :sum :lhs :sum :rhs :i)
                      (make-vm-add :dst :i :lhs :i :rhs :one)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :sum)))
         (out (cl-cc/optimize::opt-pass-loop-unrolling insts))
         (jump-to-lh (count-if (lambda (x)
                                (and (typep x 'cl-cc/vm::vm-jump)
                                     (equal (cl-cc/vm::vm-label-name x) "Lh")))
                              out))
         (lt-count (count-if (lambda (x) (typep x 'cl-cc/vm::vm-lt)) out))
         (step-count (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-add)
                                      (eq (cl-cc/vm::vm-dst x) :i)))
                               out)))
    (assert-= 0 jump-to-lh)
    (assert-= 0 lt-count)
    (assert-= 3 step-count)))

(deftest-each loop-unrolling-supports-generalized-comparisons
  "opt-pass-loop-unrolling fully unrolls small counted loops with non-vm-lt comparisons."
  :cases (("le" (make-vm-const :dst :i :value 0)
            (make-vm-const :dst :lim :value 2)
            (make-vm-const :dst :step :value 1)
            (make-vm-le :dst :c :lhs :i :rhs :lim)
            3)
           ("ge" (make-vm-const :dst :i :value 3)
            (make-vm-const :dst :lim :value 1)
            (make-vm-const :dst :step :value -1)
            (make-vm-ge :dst :c :lhs :i :rhs :lim)
            3)
           ("eq" (make-vm-const :dst :i :value 0)
            (make-vm-const :dst :lim :value 0)
            (make-vm-const :dst :step :value 1)
            (make-vm-eq :dst :c :lhs :i :rhs :lim)
            1))
  (init-inst lim-inst step-inst cmp-inst expected-steps)
  (let* ((insts (list init-inst lim-inst step-inst
                      (make-vm-label :name "Lh")
                      cmp-inst
                      (make-vm-jump-zero :reg :c :label "Lexit")
                      (make-vm-add :dst :sum :lhs :sum :rhs :i)
                      (make-vm-add :dst :i :lhs :i :rhs :step)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :sum)))
         (out (cl-cc/optimize::opt-pass-loop-unrolling insts))
         (step-count (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-add)
                                      (eq (cl-cc/vm::vm-dst x) :i)))
                               out))
         (jump-to-lh (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-jump)
                                      (equal (cl-cc/vm::vm-label-name x) "Lh")))
                               out)))
    (assert-= expected-steps step-count)
    (assert-= 0 jump-to-lh)))

(deftest loop-unrolling-partially-unrolls-when-trip-count-too-large
  "opt-pass-loop-unrolling partially unrolls small loops when trip count exceeds budget."
  (let* ((insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :lim :value 10)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-label :name "Lh")
                      (make-vm-lt :dst :c :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c :label "Lexit")
                      (make-vm-add :dst :sum :lhs :sum :rhs :i)
                      (make-vm-add :dst :i :lhs :i :rhs :one)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :sum)))
          (out (cl-cc/optimize::opt-pass-loop-unrolling insts))
          (jump-to-lh (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-jump)
                                      (equal (cl-cc/vm::vm-label-name x) "Lh")))
                              out))
          (lt-count (count-if (lambda (x) (typep x 'cl-cc/vm::vm-lt)) out))
          (step-count (count-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-add)
                                       (eq (cl-cc/vm::vm-dst x) :i)))
                                out)))
    (assert-= 1 jump-to-lh)
    (assert-= 3 lt-count)
    (assert-= 3 step-count)))

(deftest loop-unrolling-partially-unrolls-unknown-trip-with-remainder
  "opt-pass-loop-unrolling emits guarded partial copies plus the original loop for unknown trips."
  (let* ((insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-label :name "Lh")
                      (make-vm-lt :dst :c :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c :label "Lexit")
                      (make-vm-add :dst :sum :lhs :sum :rhs :i)
                      (make-vm-add :dst :i :lhs :i :rhs :one)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :sum)))
         (out (cl-cc/optimize::opt-pass-loop-unrolling insts))
         (lt-count (count-if (lambda (x) (typep x 'cl-cc/vm::vm-lt)) out))
         (jump-to-lh (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-jump)
                                      (equal (cl-cc/vm::vm-label-name x) "Lh")))
                               out)))
    (assert-= 3 lt-count)
    (assert-= 1 jump-to-lh)))

(deftest loop-rotation-detects-cfg-natural-loop
  "opt-pass-loop-rotation rotates a CFG-detected natural loop while preserving the exit branch."
  (let* ((insts (list (make-vm-label :name "Entry")
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lh")
                      (cl-cc:make-vm-integer-p :dst :r1 :src :r0)
                      (make-vm-jump-zero :reg :r1 :label "Lexit")
                      (make-vm-add :dst :r0 :lhs :r0 :rhs :r2)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :r0)))
         (out (cl-cc/optimize::opt-pass-loop-rotation insts)))
    (assert-true (some (lambda (x)
                         (and (typep x 'cl-cc/vm::vm-jump-zero)
                              (equal (cl-cc/vm::vm-label-name x) "Lexit")))
                       out))
    (assert-false (some (lambda (x)
                          (and (typep x 'cl-cc/vm::vm-jump)
                               (equal (cl-cc/vm::vm-label-name x) "Lh")))
                        out))))

(deftest loop-peeling-detects-cfg-natural-loop
  "opt-pass-loop-peeling peels a CFG-detected natural loop."
  (let* ((insts (list (make-vm-label :name "Entry")
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lh")
                      (cl-cc:make-vm-integer-p :dst :r1 :src :r0)
                      (make-vm-jump-zero :reg :r1 :label "Lexit")
                      (make-vm-add :dst :r0 :lhs :r0 :rhs :r2)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :r0)))
         (out (cl-cc/optimize::opt-pass-loop-peeling insts))
         (jz-count (count-if (lambda (i) (typep i 'cl-cc/vm::vm-jump-zero)) out))
         (add-count (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) out)))
    (assert-= 2 jz-count)
    (assert-= 2 add-count)))

(deftest-each loop-unrolling-supports-additional-comparisons
  "opt-pass-loop-unrolling fully unrolls small counted loops using le/ge/eq predicates."
  :cases (("le" (make-vm-const :dst :i :value 0)
                 (make-vm-const :dst :lim :value 2)
                 (make-vm-const :dst :step :value 1)
                 (make-vm-le :dst :c :lhs :i :rhs :lim)
                 'cl-cc/vm::vm-le 3)
          ("ge" (make-vm-const :dst :i :value 3)
                 (make-vm-const :dst :lim :value 1)
                 (make-vm-const :dst :step :value -1)
                 (make-vm-ge :dst :c :lhs :i :rhs :lim)
                 'cl-cc/vm::vm-ge 3)
          ("eq" (make-vm-const :dst :i :value 4)
                 (make-vm-const :dst :lim :value 4)
                 (make-vm-const :dst :step :value 1)
                 (make-vm-eq :dst :c :lhs :i :rhs :lim)
                 'cl-cc/vm::vm-eq 1))
  (init-inst limit-inst step-inst cmp-inst cmp-type expected-steps)
  (let* ((insts (list init-inst
                      limit-inst
                      step-inst
                      (make-vm-label :name "Lh")
                      cmp-inst
                      (make-vm-jump-zero :reg :c :label "Lexit")
                      (make-vm-add :dst :sum :lhs :sum :rhs :i)
                      (make-vm-add :dst :i :lhs :i :rhs :step)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :sum)))
         (out (cl-cc/optimize::opt-pass-loop-unrolling insts))
         (cmp-count (count-if (lambda (x) (typep x cmp-type)) out))
         (step-count (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-add)
                                      (eq (cl-cc/vm::vm-dst x) :i)))
                               out)))
    (assert-= 0 cmp-count)
    (assert-= expected-steps step-count)))

(deftest loop-unrolling-partial-keeps-remainder-loop
  "opt-pass-loop-unrolling partially unrolls larger loops and keeps a back-edge remainder loop."
  (let* ((insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :lim :value 10)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-label :name "Lh")
                      (make-vm-lt :dst :c :lhs :i :rhs :lim)
                      (make-vm-jump-zero :reg :c :label "Lexit")
                      (make-vm-add :dst :sum :lhs :sum :rhs :i)
                      (make-vm-add :dst :i :lhs :i :rhs :one)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :sum)))
         (out (cl-cc/optimize::opt-pass-loop-unrolling insts))
         (lt-count (count-if (lambda (x) (typep x 'cl-cc/vm::vm-lt)) out))
         (step-count (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-add)
                                      (eq (cl-cc/vm::vm-dst x) :i)))
                               out))
         (jump-to-lh (count-if (lambda (x)
                                 (and (typep x 'cl-cc/vm::vm-jump)
                                      (equal (cl-cc/vm::vm-label-name x) "Lh")))
                               out)))
    (assert-= 3 lt-count)
    (assert-= 3 step-count)
    (assert-= 1 jump-to-lh)))

(deftest-each cfg-natural-loop-transforms-detected
  "CFG-based loop rotation and peeling detect single-latch natural loops."
  :cases (("rotation" #'cl-cc/optimize::opt-pass-loop-rotation 0 1)
          ("peeling"  #'cl-cc/optimize::opt-pass-loop-peeling  1 2))
  (pass expected-jumps-to-lh expected-adds)
  (let* ((insts (list (make-vm-const :dst :one :value 1)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lh")
                      (cl-cc:make-vm-integer-p :dst :c :src :i)
                      (make-vm-jump-zero :reg :c :label "Lexit")
                      (make-vm-add :dst :i :lhs :i :rhs :one)
                      (make-vm-jump :label "Lh")
                      (make-vm-label :name "Lexit")
                      (make-vm-ret :reg :i)))
         (out (funcall pass insts))
         (jumps-to-lh (count-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-jump)
                                       (equal (cl-cc/vm::vm-label-name x) "Lh")))
                                out))
         (add-count (count-if (lambda (x) (typep x 'cl-cc/vm::vm-add)) out)))
    (assert-= expected-jumps-to-lh jumps-to-lh)
    (assert-= expected-adds add-count)))

(deftest code-sinking-moves-const-into-target-block
  "opt-pass-code-sinking moves a uniquely-used const into its jump target block entry."
  (let* ((insts (list (make-vm-const :dst :r1 :value 42)
                      (make-vm-jump :label "Luse")
                      (make-vm-label :name "Ldead")
                      (make-vm-ret :reg :r0)
                      (make-vm-label :name "Luse")
                      (make-vm-add :dst :r2 :lhs :r1 :rhs :r0)
                      (make-vm-ret :reg :r2)))
         (out (cl-cc/optimize::opt-pass-code-sinking insts))
         (r1-const-pos (position-if (lambda (x)
                                      (and (typep x 'cl-cc/vm::vm-const)
                                           (eq (cl-cc/vm::vm-dst x) :r1)))
                                    out))
         (luse-pos (position-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-label)
                                       (equal (cl-cc/vm::vm-name x) "Luse")))
                                out)))
    (assert-true r1-const-pos)
    (assert-true luse-pos)
    (assert-true (> r1-const-pos luse-pos))))

(deftest code-sinking-noop-when-value-is-read-multiple-times
  "opt-pass-code-sinking does not move const when the value has multiple reads."
  (let* ((insts (list (make-vm-const :dst :r1 :value 7)
                      (make-vm-jump :label "Luse")
                      (make-vm-label :name "Luse")
                      (make-vm-add :dst :r2 :lhs :r1 :rhs :r0)
                      (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)
                      (make-vm-ret :reg :r3)))
         (out (cl-cc/optimize::opt-pass-code-sinking insts))
         (r1-const-pos (position-if (lambda (x)
                                      (and (typep x 'cl-cc/vm::vm-const)
                                           (eq (cl-cc/vm::vm-dst x) :r1)))
                                    out))
         (jump-pos (position-if (lambda (x) (typep x 'cl-cc/vm::vm-jump)) out)))
    (assert-true r1-const-pos)
    (assert-true jump-pos)
    (assert-true (< r1-const-pos jump-pos))))

(deftest code-sinking-moves-cons-into-target-block
  "opt-pass-code-sinking moves a uniquely-used vm-cons into its jump target block entry."
  (let* ((insts (list (make-vm-cons :dst :pair :car-src :r0 :cdr-src :r1)
                      (make-vm-jump :label "Luse")
                      (make-vm-label :name "Ldead")
                      (make-vm-ret :reg :r0)
                      (make-vm-label :name "Luse")
                      (make-vm-car :dst :r2 :src :pair)
                      (make-vm-ret :reg :r2)))
         (out (cl-cc/optimize::opt-pass-code-sinking insts))
         (pair-cons-pos (position-if (lambda (x)
                                       (and (typep x 'cl-cc/vm::vm-cons)
                                            (eq (cl-cc/vm::vm-dst x) :pair)))
                                     out))
         (luse-pos (position-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-label)
                                       (equal (cl-cc/vm::vm-name x) "Luse")))
                                out)))
    (assert-true pair-cons-pos)
    (assert-true luse-pos)
    (assert-true (> pair-cons-pos luse-pos))))

(deftest-each code-sinking-moves-arithmetic-and-move-into-target-block
  "opt-pass-code-sinking moves constant-operand arithmetic and constant-source moves to the use block."
  :cases (("add"  (list (make-vm-const :dst :a :value 2)
                         (make-vm-const :dst :b :value 3)
                         (make-vm-add   :dst :v :lhs :a :rhs :b)
                         (make-vm-jump  :label "Luse")
                         (make-vm-label :name "Luse")
                         (make-vm-ret   :reg :v))
            'cl-cc/vm::vm-add)
          ("move" (list (make-vm-const :dst :a :value 2)
                         (make-vm-move  :dst :v :src :a)
                         (make-vm-jump  :label "Luse")
                         (make-vm-label :name "Luse")
                         (make-vm-ret   :reg :v))
            'cl-cc/vm::vm-move))
  (insts moved-type)
  (let* ((out (cl-cc/optimize::opt-pass-code-sinking insts))
         (moved-pos (position-if (lambda (x)
                                   (and (typep x moved-type)
                                        (eq (cl-cc/optimize::opt-inst-dst x) :v)))
                                 out))
         (luse-pos (position-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-label)
                                       (equal (cl-cc/vm::vm-name x) "Luse")))
                                out)))
    (assert-true moved-pos)
    (assert-true luse-pos)
    (assert-true (> moved-pos luse-pos))))

(deftest code-sinking-does-not-sink-impure-random
  "opt-pass-code-sinking does not move side-effecting vm-random instructions."
  (let* ((insts (list (make-vm-const :dst :limit :value 10)
                      (cl-cc/vm::make-vm-random :dst :v :src :limit)
                      (make-vm-jump :label "Luse")
                      (make-vm-label :name "Luse")
                      (make-vm-ret :reg :v)))
         (out (cl-cc/optimize::opt-pass-code-sinking insts))
         (random-pos (position-if (lambda (x) (typep x 'cl-cc/vm::vm-random)) out))
         (jump-pos (position-if (lambda (x) (typep x 'cl-cc/vm::vm-jump)) out)))
    (assert-true random-pos)
    (assert-true jump-pos)
    (assert-true (< random-pos jump-pos))))

(deftest code-sinking-duplicates-cheap-const-into-conditional-targets
  "opt-pass-code-sinking duplicates a cheap const into both conditional successors when both use it."
  (let* ((insts (list (make-vm-const :dst :v :value 1)
                      (make-vm-jump-zero :reg :cond :label "Lzero")
                      (make-vm-label :name "Lnonzero")
                      (make-vm-add :dst :r1 :lhs :v :rhs :x)
                      (make-vm-ret :reg :r1)
                      (make-vm-label :name "Lzero")
                      (make-vm-add :dst :r2 :lhs :v :rhs :y)
                      (make-vm-ret :reg :r2)))
         (out (cl-cc/optimize::opt-pass-code-sinking insts))
         (const-count (count-if (lambda (x)
                                  (and (typep x 'cl-cc/vm::vm-const)
                                       (eq (cl-cc/vm::vm-dst x) :v)))
                                out)))
    (assert-true (<= 1 const-count 2))))

(deftest code-sinking-noop-for-cons-read-multiple-times
  "opt-pass-code-sinking does not move vm-cons when its result is read multiple times."
  (let* ((insts (list (make-vm-cons :dst :pair :car-src :r0 :cdr-src :r1)
                      (make-vm-jump :label "Luse")
                      (make-vm-label :name "Luse")
                      (make-vm-car :dst :r2 :src :pair)
                      (make-vm-cdr :dst :r3 :src :pair)
                      (make-vm-ret :reg :r3)))
         (out (cl-cc/optimize::opt-pass-code-sinking insts))
         (pair-cons-pos (position-if (lambda (x)
                                       (and (typep x 'cl-cc/vm::vm-cons)
                                            (eq (cl-cc/vm::vm-dst x) :pair)))
                                     out))
         (jump-pos (position-if (lambda (x) (typep x 'cl-cc/vm::vm-jump)) out)))
    (assert-true pair-cons-pos)
    (assert-true jump-pos)
    (assert-true (< pair-cons-pos jump-pos))))

(deftest-each unreachable-removes-dead-code-cases
  "opt-pass-unreachable drops instructions between vm-ret/vm-jump and next label."
  :cases (("after-ret"  (list (make-vm-const :dst :r0 :value 1)
                               (make-vm-ret   :reg :r0)
                               (make-vm-const :dst :r1 :value 2)
                               (make-vm-label :name "ok")
                               (make-vm-ret   :reg :r0))
           (lambda (i) (and (typep i 'cl-cc/vm::vm-const) (eq (cl-cc/vm::vm-dst i) :r1))))
          ("after-jump" (list (make-vm-jump  :label "end")
                               (make-vm-const :dst :r0 :value 99)
                               (make-vm-label :name "end")
                               (make-vm-ret   :reg :r0))
           (lambda (i) (and (typep i 'cl-cc/vm::vm-const)
                            (eq (cl-cc/vm::vm-dst i) :r0)
                            (= 99 (cl-cc/vm::vm-value i))))))
  (insts dead-pred)
  (let ((result (cl-cc/optimize::opt-pass-unreachable insts)))
    (assert-false (some dead-pred result))))

(deftest unreachable-preserves-label-after-ret
  "opt-pass-unreachable keeps a vm-label that immediately follows a vm-ret."
  (let* ((insts (list (make-vm-ret   :reg :r0)
                      (make-vm-label :name "resume")
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::opt-pass-unreachable insts)))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-label)) result))))

(deftest unreachable-straight-line-code-unchanged
  "opt-pass-unreachable leaves pure straight-line code (no dead instructions) untouched."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-unreachable insts)))
    (assert-= (length insts) (length result))))

(deftest type-check-elim-forget-def-removes-matching-facts
  "%type-check-elim-forget-def removes facts that mention the killed register as either src or dst."
  (let* ((f1    (list :pred 'p :src :r0 :dst :r1))
         (f2    (list :pred 'q :src :r2 :dst :r3))
         (facts (list f1 f2)))
    (let ((after (cl-cc/optimize::%type-check-elim-forget-def facts :r0)))
      (assert-false (member f1 after))
      (assert-true  (member f2 after)))
    (let ((after (cl-cc/optimize::%type-check-elim-forget-def facts :r1)))
      (assert-false (member f1 after))
      (assert-true  (member f2 after)))))
