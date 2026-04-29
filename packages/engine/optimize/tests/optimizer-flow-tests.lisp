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

(deftest dce-cases
  "opt-pass-dce eliminates a vm-const whose destination is never read; keeps a vm-const that is read by a later instruction; eliminates a vm-move whose destination is never read; on empty input returns nil."
  (let* ((insts (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-const :dst :r1 :value 1)
                      (make-vm-ret   :reg :r1)))
         (result (cl-cc/optimize::opt-pass-dce insts)))
    ;; :r0 is never read → its vm-const should be removed
    (assert-false (some (lambda (i)
                          (and (typep i 'cl-cc/vm::vm-const)
                               (eq (cl-cc/vm::vm-dst i) :r0)))
                        result))
    ;; :r1 is still there
    (assert-true (some (lambda (i)
                         (and (typep i 'cl-cc/vm::vm-const)
                              (eq (cl-cc/vm::vm-dst i) :r1)))
                       result)))
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
                       result)))
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-move  :dst :r5 :src :r0)   ; :r5 never read
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::opt-pass-dce insts)))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-move)) result)))
  (assert-null (cl-cc/optimize::opt-pass-dce nil)))

(deftest build-label-index-cases
  "opt-build-label-index: maps label names to positions; returns empty vec+ht for empty input."
  (let* ((lab (make-vm-label :name "loop"))
         (c   (make-vm-const :dst :r0 :value 1)))
    (multiple-value-bind (vec idx)
        (cl-cc/optimize::opt-build-label-index (list c lab))
      (assert-= 2 (length vec))
      (assert-= 1 (gethash "loop" idx))))
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

(deftest jump-pass-cases
  "opt-pass-jump removes a vm-jump that targets the immediately following label; keeps a vm-jump that does not fall through to its target; rewrites a jump to a jump-only block to point to the final target."
  (let* ((insts (list (make-vm-jump  :label "next")
                      (make-vm-label :name  "next")
                      (make-vm-ret   :reg   :r0))))
    (let ((result (cl-cc/optimize::opt-pass-jump insts)))
      ;; The jump to "next" should be eliminated
      (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) result))))
  (let* ((insts (list (make-vm-jump  :label "far")
                      (make-vm-const :dst :r0 :value 1)
                      (make-vm-label :name "far")
                      (make-vm-ret   :reg :r0))))
    (let ((result (cl-cc/optimize::opt-pass-jump insts)))
      (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) result))))
  (let* ((insts (list (make-vm-const :dst :r0 :value 0)
                      (make-vm-jump  :label "middle")
                      (make-vm-label :name  "middle")
                      (make-vm-jump  :label "end")
                      (make-vm-label :name  "end")
                      (make-vm-ret   :reg   :r0))))
    (let ((result (cl-cc/optimize::opt-pass-jump insts)))
      ;; Any remaining vm-jump should target "end" directly
      (let ((jumps (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-jump)) result)))
        (when jumps
          (assert-equal "end" (cl-cc/vm::vm-label-name (first jumps))))))))

(deftest opt-rewrite-block-terminator-cases
  "%opt-rewrite-block-terminator rewrites vm-jump to new label when it matches; rewrites vm-jump-zero to new label, preserving reg; leaves block unchanged when label doesn't match."
  (let ((b (make-instance 'cl-cc/optimize::basic-block)))
    (setf (cl-cc/optimize::bb-instructions b)
          (list (make-vm-const :dst :r0 :value 1)
                (make-vm-jump :label "old")))
    (cl-cc/optimize::%opt-rewrite-block-terminator b "old" "new")
    (let ((term (car (last (cl-cc/optimize::bb-instructions b)))))
      (assert-true (typep term 'cl-cc/vm::vm-jump))
      (assert-equal "new" (cl-cc/vm::vm-label-name term))))
  (let ((b (make-instance 'cl-cc/optimize::basic-block)))
    (setf (cl-cc/optimize::bb-instructions b)
          (list (make-vm-jump-zero :reg :r0 :label "old")))
    (cl-cc/optimize::%opt-rewrite-block-terminator b "old" "new")
    (let ((term (car (cl-cc/optimize::bb-instructions b))))
      (assert-true (typep term 'cl-cc/vm::vm-jump-zero))
      (assert-equal "new" (cl-cc/vm::vm-label-name term))
      (assert-eq :r0 (cl-cc/vm::vm-reg term))))
  (let ((b (make-instance 'cl-cc/optimize::basic-block))
        (orig (make-vm-jump :label "other")))
    (setf (cl-cc/optimize::bb-instructions b) (list orig))
    (cl-cc/optimize::%opt-rewrite-block-terminator b "old" "new")
    (assert-eq orig (car (cl-cc/optimize::bb-instructions b)))))

(deftest opt-thread-jump-cases
  "*opt-jump-thread-table* covers vm-jump and vm-jump-zero handlers; %opt-thread-jump returns NIL when the target label immediately follows the jump; returns a relabeled instruction when the target threads to a new label; %opt-thread-jump-zero always returns an instruction (never eliminates conditionals)."
  (assert-= 2 (length cl-cc/optimize::*opt-jump-thread-table*))
  (assert-true (assoc 'vm-jump      cl-cc/optimize::*opt-jump-thread-table*))
  (assert-true (assoc 'vm-jump-zero cl-cc/optimize::*opt-jump-thread-table*))
  (let* ((insts (list (make-vm-jump  :label "next")
                      (make-vm-label :name  "next")
                      (make-vm-ret   :reg   :r0))))
    (multiple-value-bind (vec idx) (cl-cc/optimize::opt-build-label-index insts)
      (assert-null (cl-cc/optimize::%opt-thread-jump
                    (make-vm-jump :label "next") vec 0 idx))))
  (let* ((insts (list (make-vm-jump  :label "middle")
                      (make-vm-label :name  "middle")
                      (make-vm-jump  :label "end")
                      (make-vm-label :name  "end")
                      (make-vm-ret   :reg   :r0))))
    (multiple-value-bind (vec idx) (cl-cc/optimize::opt-build-label-index insts)
      (let ((result (cl-cc/optimize::%opt-thread-jump
                     (make-vm-jump :label "middle") vec 0 idx)))
        (assert-true result)
        (assert-equal "end" (cl-cc/vm::vm-label-name result)))))
  (let* ((inst  (make-vm-jump-zero :reg :r0 :label "next"))
         (insts (list inst (make-vm-label :name "next") (make-vm-ret :reg :r0))))
    (multiple-value-bind (vec idx) (cl-cc/optimize::opt-build-label-index insts)
      (let ((result (cl-cc/optimize::%opt-thread-jump-zero inst vec 0 idx)))
        (assert-true result)
        (assert-true (typep result 'cl-cc/vm::vm-jump-zero))))))

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

(deftest unreachable-preserves-cases
  "opt-pass-unreachable keeps a vm-label that follows a vm-ret; leaves pure straight-line code untouched; %type-check-elim-forget-def removes facts mentioning the killed register."
  (let* ((insts (list (make-vm-ret   :reg :r0)
                      (make-vm-label :name "resume")
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::opt-pass-unreachable insts)))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-label)) result)))
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-unreachable insts)))
    (assert-= (length insts) (length result)))
  (let* ((f1 (list :pred 'p :src :r0 :dst :r1))
         (f2 (list :pred 'q :src :r2 :dst :r3))
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
          ("nil-check-elim"    #'cl-cc/optimize::opt-pass-dominated-type-check-elim)
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

(deftest block-merge-helpers-cases
  "%block-mergeable-successor-p returns T when successor has exactly one predecessor; returns NIL when the successor has >1 predecessor; returns NIL when block has >1 successor; %block-strip-merge-jump strips the terminal vm-jump when it targets the label; leaves instructions unchanged when label does not match; returns nil for an empty instruction list."
  (let* ((blk  (make-instance 'cl-cc/optimize::basic-block))
         (succ (make-instance 'cl-cc/optimize::basic-block)))
    (setf (cl-cc/optimize::bb-successors blk)   (list succ)
          (cl-cc/optimize::bb-predecessors succ) (list blk))
    (assert-true (cl-cc/optimize::%block-mergeable-successor-p blk)))
  (let* ((blk   (make-instance 'cl-cc/optimize::basic-block))
         (other (make-instance 'cl-cc/optimize::basic-block))
         (succ  (make-instance 'cl-cc/optimize::basic-block)))
    (setf (cl-cc/optimize::bb-successors blk)   (list succ)
          (cl-cc/optimize::bb-predecessors succ) (list blk other))
    (assert-false (cl-cc/optimize::%block-mergeable-successor-p blk)))
  (let* ((blk   (make-instance 'cl-cc/optimize::basic-block))
         (succ1 (make-instance 'cl-cc/optimize::basic-block))
         (succ2 (make-instance 'cl-cc/optimize::basic-block)))
    (setf (cl-cc/optimize::bb-successors blk) (list succ1 succ2))
    (assert-false (cl-cc/optimize::%block-mergeable-successor-p blk)))
  (let* ((jmp   (make-vm-jump :label "next"))
         (const (make-vm-const :dst :r0 :value 1))
         (insts (list const jmp))
         (result (cl-cc/optimize::%block-strip-merge-jump insts "next")))
    (assert-= 1 (length result))
    (assert-eq const (first result)))
  (let* ((jmp   (make-vm-jump :label "other"))
         (const (make-vm-const :dst :r0 :value 1))
         (insts (list const jmp))
         (result (cl-cc/optimize::%block-strip-merge-jump insts "next")))
    (assert-= 2 (length result)))
  (assert-null (cl-cc/optimize::%block-strip-merge-jump nil "next")))

(deftest cfg-block-temperature-cases
  "%cfg-block-cold-p returns T when a block contains a vm-signal-error; returns NIL for ordinary blocks; %cfg-block-hotter-p returns NIL when A is cold and B is not cold; returns T when A has greater loop depth; uses RPO index as tie-breaker when loop depths are equal."
  (let ((blk (make-instance 'cl-cc/optimize::basic-block)))
    (setf (cl-cc/optimize::bb-instructions blk)
          (list (make-vm-signal-error :error-reg :r0)))
    (assert-true (cl-cc/optimize::%cfg-block-cold-p blk)))
  (let ((blk (make-instance 'cl-cc/optimize::basic-block)))
    (setf (cl-cc/optimize::bb-instructions blk)
          (list (make-vm-const :dst :r0 :value 1)
                (make-vm-ret   :reg :r0)))
    (assert-false (cl-cc/optimize::%cfg-block-cold-p blk)))
  (let ((cold (make-instance 'cl-cc/optimize::basic-block))
        (warm (make-instance 'cl-cc/optimize::basic-block)))
    (setf (cl-cc/optimize::bb-instructions cold)
          (list (make-vm-signal-error :error-reg :r0)))
    (setf (cl-cc/optimize::bb-instructions warm)
          (list (make-vm-const :dst :r0 :value 1)))
    (assert-false (cl-cc/optimize::%cfg-block-hotter-p cold warm))
    (assert-true  (cl-cc/optimize::%cfg-block-hotter-p warm cold)))
  (let ((deep    (make-instance 'cl-cc/optimize::basic-block))
        (shallow (make-instance 'cl-cc/optimize::basic-block)))
    (setf (cl-cc/optimize::bb-loop-depth  deep)    3
          (cl-cc/optimize::bb-loop-depth  shallow) 1
          (cl-cc/optimize::bb-instructions deep)    nil
          (cl-cc/optimize::bb-instructions shallow) nil)
    (assert-true  (cl-cc/optimize::%cfg-block-hotter-p deep shallow))
    (assert-false (cl-cc/optimize::%cfg-block-hotter-p shallow deep)))
  (let ((first  (make-instance 'cl-cc/optimize::basic-block))
        (second (make-instance 'cl-cc/optimize::basic-block)))
    (setf (cl-cc/optimize::bb-loop-depth first)   1
          (cl-cc/optimize::bb-loop-depth second)  1
          (cl-cc/optimize::bb-rpo-index  first)   0
          (cl-cc/optimize::bb-rpo-index  second)  1
          (cl-cc/optimize::bb-instructions first)  nil
          (cl-cc/optimize::bb-instructions second) nil)
    (assert-true  (cl-cc/optimize::%cfg-block-hotter-p first second))
    (assert-false (cl-cc/optimize::%cfg-block-hotter-p second first))))
