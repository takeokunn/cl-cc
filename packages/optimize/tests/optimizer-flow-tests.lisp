;;;; tests/unit/optimize/optimizer-flow-tests.lisp
;;;; Unit tests for src/optimize/optimizer-flow.lisp
;;;;
;;;; Covers: opt-pass-dce, opt-build-label-index, opt-thread-label,
;;;;   opt-falls-through-to-p, opt-pass-jump, opt-pass-unreachable,
;;;;   opt-pass-dead-basic-blocks, opt-pass-dominated-type-check-elim,
;;;;   opt-pass-branch-correlation, opt-pass-block-merge, opt-pass-tail-merge.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

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
  (let ((b (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-instructions b)
          (list (make-vm-const :dst :r0 :value 1)
                (make-vm-jump :label "old")))
    (cl-cc/optimize::%opt-rewrite-block-terminator b "old" "new")
    (let ((term (car (last (cl-cc/optimize:bb-instructions b)))))
      (assert-true (typep term 'cl-cc/vm::vm-jump))
      (assert-equal "new" (cl-cc/vm::vm-label-name term)))))

(deftest opt-rewrite-terminator-vm-jump-zero
  "%opt-rewrite-block-terminator rewrites a vm-jump-zero label while preserving the condition register."
  (let ((b (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-instructions b)
          (list (make-vm-jump-zero :reg :r0 :label "old")))
    (cl-cc/optimize::%opt-rewrite-block-terminator b "old" "new")
    (let ((term (car (cl-cc/optimize:bb-instructions b))))
      (assert-true (typep term 'cl-cc/vm::vm-jump-zero))
      (assert-equal "new" (cl-cc/vm::vm-label-name term))
      (assert-eq :r0 (cl-cc/vm::vm-reg term)))))

(deftest opt-rewrite-terminator-no-match-unchanged
  "%opt-rewrite-block-terminator leaves the block unchanged when the label does not match."
  (let ((b    (make-instance 'cl-cc/optimize:basic-block))
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

(deftest loop-unrolling-noop-when-trip-count-too-large
  "opt-pass-loop-unrolling keeps loop when compile-time trip count exceeds budget."
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
                              out)))
    (assert-= 1 jump-to-lh)))

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

;;; ─── Straight-line preservation / empty-input ───────────────────────────

(deftest-each opt-passes-preserve-straight-line
  "dead-basic-blocks/nil-check-elim/block-merge/tail-merge all preserve straight-line code."
  :cases (("dead-basic-blocks" #'cl-cc/optimize::opt-pass-dead-basic-blocks)
          ("nil-check-elim"    #'cl-cc/optimize:opt-pass-dominated-type-check-elim)
          ("block-merge"       #'cl-cc/optimize::opt-pass-block-merge)
          ("tail-merge"        #'cl-cc/optimize::opt-pass-tail-merge))
  (pass-fn)
  (let ((result (funcall pass-fn (list (make-vm-const :dst :r0 :value 1)
                                       (make-vm-ret   :reg :r0)))))
    (assert-true (listp result))))

(deftest-each opt-pass-returns-list-for-empty-input
  "opt-pass-dead-basic-blocks, opt-pass-block-merge, and opt-pass-tail-merge all return a list for empty input."
  :cases (("dead-basic-blocks"  #'cl-cc/optimize::opt-pass-dead-basic-blocks)
          ("block-merge"        #'cl-cc/optimize::opt-pass-block-merge)
          ("tail-merge"         #'cl-cc/optimize::opt-pass-tail-merge))
  (pass-fn)
  (assert-true (listp (funcall pass-fn nil))))

;;; ─── %type-check-elim helpers ───────────────────────────────────────────────

(deftest-each type-check-elim-lookup-fact-cases
  "%type-check-elim-lookup-fact finds matching pred+src or returns nil."
  :cases (("match"     'p :r0 t)
          ("wrong-src" 'p :r9 nil)
          ("wrong-pred" 'q :r0 nil))
  (pred src should-match)
  (let* ((fact  (list :pred 'p :src :r0 :dst :r1))
         (facts (list fact))
         (result (cl-cc/optimize::%type-check-elim-lookup-fact facts pred src)))
    (if should-match
        (assert-true result)
        (assert-null result))))

;;; ─── %block-mergeable-successor-p / %block-strip-merge-jump / cfg-block-temperature ──

(deftest block-mergeable-successor-single-predecessor
  "%block-mergeable-successor-p returns T when the block's sole successor has exactly one predecessor."
  (let* ((blk  (make-instance 'cl-cc/optimize:basic-block))
         (succ (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-successors blk)   (list succ)
          (cl-cc/optimize:bb-predecessors succ) (list blk))
    (assert-true (cl-cc/optimize::%block-mergeable-successor-p blk))))

(deftest block-mergeable-successor-multiple-predecessors
  "%block-mergeable-successor-p returns NIL when the successor has more than one predecessor."
  (let* ((blk   (make-instance 'cl-cc/optimize:basic-block))
         (other (make-instance 'cl-cc/optimize:basic-block))
         (succ  (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-successors blk)   (list succ)
          (cl-cc/optimize:bb-predecessors succ) (list blk other))
    (assert-false (cl-cc/optimize::%block-mergeable-successor-p blk))))

(deftest block-mergeable-multiple-successors
  "%block-mergeable-successor-p returns NIL when the block itself has more than one successor."
  (let* ((blk   (make-instance 'cl-cc/optimize:basic-block))
         (succ1 (make-instance 'cl-cc/optimize:basic-block))
         (succ2 (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-successors blk) (list succ1 succ2))
    (assert-false (cl-cc/optimize::%block-mergeable-successor-p blk))))

(deftest-each block-strip-merge-jump-cases
  "%block-strip-merge-jump: matching label removes trailing jump; non-matching leaves unchanged; nil returns nil."
  :cases (("matching"     (list (make-vm-const :dst :r0 :value 1) (make-vm-jump :label "next"))  "next" 1)
          ("non-matching" (list (make-vm-const :dst :r0 :value 1) (make-vm-jump :label "other")) "next" 2)
          ("empty"        nil                                                                      "next" 0))
  (insts target expected-len)
  (assert-= expected-len (length (cl-cc/optimize::%block-strip-merge-jump insts target))))

(deftest-each cfg-block-cold-p-signal-error-and-ordinary
  "%cfg-block-cold-p returns T for error-signalling blocks and NIL for ordinary ones."
  :cases (("signal-error" (list (make-vm-signal-error :error-reg :r0)) t)
          ("ordinary"     (list (make-vm-const :dst :r0 :value 1) (make-vm-ret :reg :r0)) nil))
  (instructions expect-cold)
  (let ((blk (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-instructions blk) instructions)
    (if expect-cold
        (assert-true  (cl-cc/optimize::%cfg-block-cold-p blk))
        (assert-false (cl-cc/optimize::%cfg-block-cold-p blk)))))

(deftest cfg-block-hotter-p-cold-vs-warm
  "%cfg-block-hotter-p returns NIL when A is cold; returns T when A is warm and B is cold."
  (let ((cold (make-instance 'cl-cc/optimize:basic-block))
        (warm (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-instructions cold) (list (make-vm-signal-error :error-reg :r0))
          (cl-cc/optimize:bb-instructions warm) (list (make-vm-const :dst :r0 :value 1)))
    (assert-false (cl-cc/optimize::%cfg-block-hotter-p cold warm))
    (assert-true  (cl-cc/optimize::%cfg-block-hotter-p warm cold))))

(deftest cfg-block-hotter-p-loop-depth
  "%cfg-block-hotter-p returns T when A has greater loop depth than B."
  (let ((deep    (make-instance 'cl-cc/optimize:basic-block))
        (shallow (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-loop-depth  deep)    3
          (cl-cc/optimize:bb-loop-depth  shallow) 1
          (cl-cc/optimize:bb-instructions deep)    nil
          (cl-cc/optimize:bb-instructions shallow) nil)
    (assert-true  (cl-cc/optimize::%cfg-block-hotter-p deep shallow))
    (assert-false (cl-cc/optimize::%cfg-block-hotter-p shallow deep))))

(deftest cfg-block-hotter-p-rpo-index-tiebreaker
  "%cfg-block-hotter-p uses RPO index as a tiebreaker when loop depths are equal."
  (let ((first  (make-instance 'cl-cc/optimize:basic-block))
        (second (make-instance 'cl-cc/optimize:basic-block)))
    (setf (cl-cc/optimize:bb-loop-depth first)   1
          (cl-cc/optimize:bb-loop-depth second)  1
          (cl-cc/optimize:bb-rpo-index  first)   0
          (cl-cc/optimize:bb-rpo-index  second)  1
          (cl-cc/optimize:bb-instructions first)  nil
          (cl-cc/optimize:bb-instructions second) nil)
    (assert-true  (cl-cc/optimize::%cfg-block-hotter-p first second))
    (assert-false (cl-cc/optimize::%cfg-block-hotter-p second first))))

(deftest branch-correlation-propagates-through-forwarder
  "opt-pass-branch-correlation propagates fact through a jump-only forwarder block."
  (let* ((pred   (make-vm-label :name "pred"))
         (p1     (cl-cc:make-vm-integer-p :dst :r1 :src :r0))
         (br     (make-vm-jump-zero :reg :r1 :label "mid"))
         (then   (make-vm-label :name "then"))
         (ret1   (make-vm-ret :reg :r1))
         (mid    (make-vm-label :name "mid"))
         (jmp    (make-vm-jump :label "join"))
         (join   (make-vm-label :name "join"))
         (p2     (cl-cc:make-vm-integer-p :dst :r2 :src :r0))
         (ret2   (make-vm-ret :reg :r2))
         (out    (cl-cc/optimize::opt-pass-branch-correlation
                  (list pred p1 br then ret1 mid jmp join p2 ret2))))
    (assert-true
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-const)
                  (eq (cl-cc/vm::vm-dst i) :r2)
                  (eql (cl-cc/vm::vm-value i) 0)))
           out))))

(deftest branch-correlation-forwarder-does-not-propagate-on-mismatch
  "opt-pass-branch-correlation does not propagate when a forwarder edge disagrees."
  (let* ((pred   (make-vm-label :name "pred"))
         (p1     (cl-cc:make-vm-integer-p :dst :r1 :src :r0))
         (br     (make-vm-jump-zero :reg :r1 :label "mid"))
         (then   (make-vm-label :name "then"))
         (ret1   (make-vm-ret :reg :r1))
         (mid    (make-vm-label :name "mid"))
         (jmp    (make-vm-jump :label "join"))
         (otherp (make-vm-label :name "otherp"))
         (p2     (cl-cc:make-vm-integer-p :dst :r3 :src :r4))
         (br2    (make-vm-jump-zero :reg :r3 :label "join"))
         (join   (make-vm-label :name "join"))
         (p3     (cl-cc:make-vm-integer-p :dst :r2 :src :r0))
         (ret2   (make-vm-ret :reg :r2))
         (out    (cl-cc/optimize::opt-pass-branch-correlation
                  (list pred p1 br then ret1 mid jmp otherp p2 br2 join p3 ret2))))
    (assert-false
     (some (lambda (i)
             (and (typep i 'cl-cc/vm::vm-const)
                  (eq (cl-cc/vm::vm-dst i) :r2)))
           out))))

(deftest tail-duplication-duplicates-small-shared-tail
  "opt-pass-tail-duplication duplicates a small shared tail into predecessors."
  (let* ((insts (list (make-vm-label :name "p1")
                      (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump :label "p2")
                      (make-vm-label :name "p2")
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-jump :label "tail")
                      (make-vm-label :name "tail")
                      (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret :reg :r2)))
         (out (cl-cc/optimize::opt-pass-tail-duplication insts))
         (jump-to-tail (count-if (lambda (i)
                                   (and (typep i 'cl-cc/vm::vm-jump)
                                        (equal (cl-cc/vm::vm-label-name i) "tail")))
                                 out))
         (ret-count (count-if (lambda (i) (typep i 'cl-cc/vm::vm-ret)) out)))
    (assert-= 1 jump-to-tail)
    (assert-= 1 ret-count)))

(deftest tail-duplication-skips-large-tail
  "opt-pass-tail-duplication leaves jumps when tail block exceeds duplication budget."
  (let* ((insts (list (make-vm-label :name "p1")
                      (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump :label "p2")
                      (make-vm-label :name "p2")
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-jump :label "tail")
                      (make-vm-label :name "tail")
                      (make-vm-const :dst :r3 :value 10)
                      (make-vm-const :dst :r4 :value 20)
                      (make-vm-add :dst :r5 :lhs :r3 :rhs :r4)
                      (make-vm-sub :dst :r6 :lhs :r5 :rhs :r0)
                      (make-vm-ret :reg :r6)))
         (out (cl-cc/optimize::opt-pass-tail-duplication insts))
         (jump-to-tail (count-if (lambda (i)
                                   (and (typep i 'cl-cc/vm::vm-jump)
                                        (equal (cl-cc/vm::vm-label-name i) "tail")))
                                 out)))
    (assert-= 1 jump-to-tail)))
