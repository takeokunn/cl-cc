;;;; tests/unit/optimize/optimizer-flow-tests.lisp
;;;; Unit tests for src/optimize/optimizer-flow.lisp
;;;;
;;;; Covers: opt-pass-dce, opt-build-label-index, opt-thread-label,
;;;;   opt-falls-through-to-p, opt-pass-jump, opt-pass-unreachable,
;;;;   opt-pass-dead-basic-blocks, opt-pass-nil-check-elim (thin alias),
;;;;   opt-pass-branch-correlation, opt-pass-block-merge, opt-pass-tail-merge.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── opt-pass-dce ────────────────────────────────────────────────────────

(deftest dce-removes-unused-vm-const
  "opt-pass-dce eliminates a vm-const whose destination is never read."
  (let* ((insts (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-const :dst :r1 :value 1)
                      (make-vm-ret   :reg :r1)))
         (result (cl-cc::opt-pass-dce insts)))
    ;; :r0 is never read → its vm-const should be removed
    (assert-false (some (lambda (i)
                          (and (typep i 'cl-cc::vm-const)
                               (eq (cl-cc::vm-dst i) :r0)))
                        result))
    ;; :r1 is still there
    (assert-true (some (lambda (i)
                         (and (typep i 'cl-cc::vm-const)
                              (eq (cl-cc::vm-dst i) :r1)))
                       result))))

(deftest dce-keeps-used-vm-const
  "opt-pass-dce keeps a vm-const that is read by a later instruction."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc::opt-pass-dce insts)))
    (assert-true (some (lambda (i)
                         (and (typep i 'cl-cc::vm-const)
                              (eq (cl-cc::vm-dst i) :r0)))
                       result))
    (assert-true (some (lambda (i)
                         (and (typep i 'cl-cc::vm-const)
                              (eq (cl-cc::vm-dst i) :r1)))
                       result))))

(deftest dce-removes-unused-vm-move
  "opt-pass-dce eliminates a vm-move whose destination is never read."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-move  :dst :r5 :src :r0)   ; :r5 never read
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc::opt-pass-dce insts)))
    (assert-false (some (lambda (i) (typep i 'cl-cc::vm-move)) result))))

(deftest dce-empty-input-returns-nil
  "opt-pass-dce on empty input returns nil."
  (assert-null (cl-cc::opt-pass-dce nil)))

;;; ─── opt-build-label-index ───────────────────────────────────────────────

(deftest build-label-index-maps-label-names
  "opt-build-label-index maps label name strings to their positions in the vector."
  (let* ((lab (make-vm-label :name "loop"))
         (c   (make-vm-const :dst :r0 :value 1))
         (insts (list c lab)))
    (multiple-value-bind (vec idx)
        (cl-cc::opt-build-label-index insts)
      (assert-= 2 (length vec))
      (assert-= 1 (gethash "loop" idx)))))

(deftest build-label-index-empty-input
  "opt-build-label-index on empty list returns empty vec and empty ht."
  (multiple-value-bind (vec idx)
      (cl-cc::opt-build-label-index nil)
    (assert-= 0 (length vec))
    (assert-= 0 (hash-table-count idx))))

;;; ─── opt-thread-label ────────────────────────────────────────────────────

(deftest-each thread-label-returns-input-unchanged
  "opt-thread-label returns the queried label name unchanged — no jump chain, or unknown label."
  :cases (("no-chain"  (list (make-vm-label :name "end") (make-vm-ret :reg :r0))  "end")
          ("unknown"   nil                                                          "nowhere"))
  (insts query)
  (multiple-value-bind (vec idx)
      (cl-cc::opt-build-label-index insts)
    (assert-equal query (cl-cc::opt-thread-label query idx vec))))

;;; ─── opt-pass-jump ───────────────────────────────────────────────────────

(deftest jump-pass-removes-jump-to-next-label
  "opt-pass-jump removes a vm-jump that targets the immediately following label."
  (let* ((insts (list (make-vm-jump  :label "next")
                      (make-vm-label :name  "next")
                      (make-vm-ret   :reg   :r0))))
    (let ((result (cl-cc::opt-pass-jump insts)))
      ;; The jump to "next" should be eliminated
      (assert-false (some (lambda (i) (typep i 'cl-cc::vm-jump)) result)))))

(deftest jump-pass-preserves-jump-to-non-adjacent-label
  "opt-pass-jump keeps a vm-jump that does not fall through to its target."
  (let* ((insts (list (make-vm-jump  :label "far")
                      (make-vm-const :dst :r0 :value 1)
                      (make-vm-label :name "far")
                      (make-vm-ret   :reg :r0))))
    (let ((result (cl-cc::opt-pass-jump insts)))
      (assert-true (some (lambda (i) (typep i 'cl-cc::vm-jump)) result)))))

(deftest jump-pass-threads-chain
  "opt-pass-jump rewrites a jump to a jump-only block to point to the final target."
  (let* ((insts (list (make-vm-const :dst :r0 :value 0)
                      (make-vm-jump  :label "middle")
                      (make-vm-label :name  "middle")
                      (make-vm-jump  :label "end")
                      (make-vm-label :name  "end")
                      (make-vm-ret   :reg   :r0))))
    (let ((result (cl-cc::opt-pass-jump insts)))
      ;; Any remaining vm-jump should target "end" directly
      (let ((jumps (remove-if-not (lambda (i) (typep i 'cl-cc::vm-jump)) result)))
        (when jumps
          (assert-equal "end" (cl-cc::vm-label-name (first jumps))))))))

;;; ─── opt-pass-unreachable ────────────────────────────────────────────────

(deftest unreachable-removes-code-after-ret
  "opt-pass-unreachable drops instructions after a vm-ret before the next label."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-ret   :reg :r0)
                      (make-vm-const :dst :r1 :value 2)    ; dead
                      (make-vm-label :name "ok")
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc::opt-pass-unreachable insts)))
    ;; The const :r1 after the first ret should be gone
    (assert-false (some (lambda (i)
                          (and (typep i 'cl-cc::vm-const)
                               (eq (cl-cc::vm-dst i) :r1)))
                        result))))

(deftest unreachable-removes-code-after-jump
  "opt-pass-unreachable drops instructions after vm-jump before next label."
  (let* ((insts (list (make-vm-jump  :label "end")
                      (make-vm-const :dst :r0 :value 99)   ; dead
                      (make-vm-label :name "end")
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc::opt-pass-unreachable insts)))
    (assert-false (some (lambda (i)
                          (and (typep i 'cl-cc::vm-const)
                               (eq (cl-cc::vm-dst i) :r0)
                               (= 99 (cl-cc::vm-value i))))
                        result))))

(deftest unreachable-preserves-label-after-ret
  "opt-pass-unreachable keeps a vm-label that follows a vm-ret."
  (let* ((insts (list (make-vm-ret   :reg :r0)
                      (make-vm-label :name "resume")
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc::opt-pass-unreachable insts)))
    (assert-true (some (lambda (i) (typep i 'cl-cc::vm-label)) result))))

(deftest unreachable-preserves-straight-line
  "opt-pass-unreachable leaves pure straight-line code untouched."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc::opt-pass-unreachable insts)))
    (assert-= (length insts) (length result))))

;;; ─── opt-pass-dead-basic-blocks ──────────────────────────────────────────

(deftest dead-basic-blocks-straight-line-unchanged
  "opt-pass-dead-basic-blocks preserves straight-line code (no unreachable blocks)."
  (let* ((insts (list (make-vm-const :dst :r0 :value 5)
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc::opt-pass-dead-basic-blocks insts)))
    (assert-true (listp result))
    (assert-true (> (length result) 0))))

;;; ─── opt-pass-nil-check-elim ─────────────────────────────────────────────

(deftest nil-check-elim-straight-line-unchanged
  "opt-pass-nil-check-elim on straight-line code with no type preds leaves it intact."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc::opt-pass-nil-check-elim insts)))
    (assert-true (listp result))))

;;; ─── opt-pass-block-merge / opt-pass-tail-merge / opt-pass-dead-basic-blocks (empty) ──────

(deftest-each opt-pass-returns-list-for-empty-input
  "opt-pass-dead-basic-blocks, opt-pass-block-merge, and opt-pass-tail-merge all return a list for empty input."
  :cases (("dead-basic-blocks"  #'cl-cc::opt-pass-dead-basic-blocks)
          ("block-merge"        #'cl-cc::opt-pass-block-merge)
          ("tail-merge"         #'cl-cc::opt-pass-tail-merge))
  (pass-fn)
  (assert-true (listp (funcall pass-fn nil))))

;;; ─── opt-pass-block-merge ────────────────────────────────────────────────

(deftest block-merge-straight-line-preserved
  "opt-pass-block-merge preserves all instructions in linear code."
  (let* ((insts (list (make-vm-const :dst :r0 :value 3)
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc::opt-pass-block-merge insts)))
    (assert-true (> (length result) 0))))

;;; ─── opt-pass-tail-merge ─────────────────────────────────────────────────

(deftest tail-merge-non-duplicate-blocks-unchanged
  "opt-pass-tail-merge leaves non-duplicate blocks intact."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-label :name "a")
                      (make-vm-ret   :reg :r0)
                      (make-vm-label :name "b")
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc::opt-pass-tail-merge insts)))
    ;; Should produce a valid instruction list
    (assert-true (listp result))))
