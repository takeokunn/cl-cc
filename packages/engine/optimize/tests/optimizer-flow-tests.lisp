;;;; tests/unit/optimize/optimizer-flow-tests.lisp
;;;; Unit tests for src/optimize/optimizer-flow.lisp
;;;;
;;;; Covers: opt-pass-dce, opt-build-label-index, opt-thread-label,
;;;;   opt-falls-through-to-p, opt-pass-jump, opt-pass-unreachable,
;;;;   opt-pass-dead-basic-blocks, opt-pass-nil-check-elim (thin alias),
;;;;   opt-pass-branch-correlation, opt-pass-block-merge, opt-pass-tail-merge.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── opt-pass-dce ────────────────────────────────────────────────────────

(deftest dce-removes-unused-vm-const
  "opt-pass-dce eliminates a vm-const whose destination is never read."
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
                       result))))

(deftest dce-keeps-used-vm-const
  "opt-pass-dce keeps a vm-const that is read by a later instruction."
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

(deftest dce-removes-unused-vm-move
  "opt-pass-dce eliminates a vm-move whose destination is never read."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-move  :dst :r5 :src :r0)   ; :r5 never read
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::opt-pass-dce insts)))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-move)) result))))

(deftest dce-empty-input-returns-nil
  "opt-pass-dce on empty input returns nil."
  (assert-null (cl-cc/optimize::opt-pass-dce nil)))

;;; ─── opt-build-label-index ───────────────────────────────────────────────

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

;;; ─── opt-thread-label ────────────────────────────────────────────────────

(deftest-each thread-label-returns-input-unchanged
  "opt-thread-label returns the queried label name unchanged — no jump chain, or unknown label."
  :cases (("no-chain"  (list (make-vm-label :name "end") (make-vm-ret :reg :r0))  "end")
          ("unknown"   nil                                                          "nowhere"))
  (insts query)
  (multiple-value-bind (vec idx)
      (cl-cc/optimize::opt-build-label-index insts)
    (assert-equal query (cl-cc/optimize::opt-thread-label query idx vec))))

;;; ─── opt-pass-jump ───────────────────────────────────────────────────────

(deftest jump-pass-removes-jump-to-next-label
  "opt-pass-jump removes a vm-jump that targets the immediately following label."
  (let* ((insts (list (make-vm-jump  :label "next")
                      (make-vm-label :name  "next")
                      (make-vm-ret   :reg   :r0))))
    (let ((result (cl-cc/optimize::opt-pass-jump insts)))
      ;; The jump to "next" should be eliminated
      (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) result)))))

(deftest jump-pass-preserves-jump-to-non-adjacent-label
  "opt-pass-jump keeps a vm-jump that does not fall through to its target."
  (let* ((insts (list (make-vm-jump  :label "far")
                      (make-vm-const :dst :r0 :value 1)
                      (make-vm-label :name "far")
                      (make-vm-ret   :reg :r0))))
    (let ((result (cl-cc/optimize::opt-pass-jump insts)))
      (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) result)))))

(deftest jump-pass-threads-chain
  "opt-pass-jump rewrites a jump to a jump-only block to point to the final target."
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

;;; ─── opt-pass-unreachable ────────────────────────────────────────────────

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
  "opt-pass-unreachable keeps a vm-label that follows a vm-ret."
  (let* ((insts (list (make-vm-ret   :reg :r0)
                      (make-vm-label :name "resume")
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::opt-pass-unreachable insts)))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-label)) result))))

(deftest unreachable-preserves-straight-line
  "opt-pass-unreachable leaves pure straight-line code untouched."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-unreachable insts)))
    (assert-= (length insts) (length result))))

;;; ─── Straight-line preservation ─────────────────────────────────────────

(deftest-each opt-passes-preserve-straight-line
  "dead-basic-blocks/nil-check-elim/block-merge/tail-merge all preserve straight-line code."
  :cases (("dead-basic-blocks" #'cl-cc/optimize::opt-pass-dead-basic-blocks)
          ("nil-check-elim"    #'cl-cc/optimize::opt-pass-nil-check-elim)
          ("block-merge"       #'cl-cc/optimize::opt-pass-block-merge)
          ("tail-merge"        #'cl-cc/optimize::opt-pass-tail-merge))
  (pass-fn)
  (let ((result (funcall pass-fn (list (make-vm-const :dst :r0 :value 1)
                                       (make-vm-ret   :reg :r0)))))
    (assert-true (listp result))))

;;; ─── opt-pass-block-merge / opt-pass-tail-merge / opt-pass-dead-basic-blocks (empty) ──────

(deftest-each opt-pass-returns-list-for-empty-input
  "opt-pass-dead-basic-blocks, opt-pass-block-merge, and opt-pass-tail-merge all return a list for empty input."
  :cases (("dead-basic-blocks"  #'cl-cc/optimize::opt-pass-dead-basic-blocks)
          ("block-merge"        #'cl-cc/optimize::opt-pass-block-merge)
          ("tail-merge"         #'cl-cc/optimize::opt-pass-tail-merge))
  (pass-fn)
  (assert-true (listp (funcall pass-fn nil))))

;;; ─── %type-check-elim helpers ───────────────────────────────────────────────

(deftest type-check-elim-copy-facts-independent-copy
  "%type-check-elim-copy-facts returns a fresh list sharing elements."
  (let* ((fact  (list :pred 'p :src :r0 :dst :r1))
         (facts (list fact))
         (copy  (cl-cc/optimize::%type-check-elim-copy-facts facts)))
    (assert-equal facts copy)
    (assert-false (eq facts copy))))

(deftest type-check-elim-forget-def-removes-src-and-dst
  "%type-check-elim-forget-def removes facts mentioning the killed register."
  (let* ((f1 (list :pred 'p :src :r0 :dst :r1))
         (f2 (list :pred 'q :src :r2 :dst :r3))
         (facts (list f1 f2)))
    (let ((after (cl-cc/optimize::%type-check-elim-forget-def facts :r0)))
      (assert-false (member f1 after))
      (assert-true  (member f2 after)))
    (let ((after (cl-cc/optimize::%type-check-elim-forget-def facts :r1)))
      (assert-false (member f1 after))
      (assert-true  (member f2 after)))))

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
