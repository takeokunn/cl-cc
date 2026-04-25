;;;; packages/foundation/ir/tests/ir-ssa-advanced-tests.lisp — Advanced SSA Tests
;;;
;;; Advanced Braun SSA tests: unsealed blocks, phi resolution, loop self-reference.
;;; Dominator tree, ir-collect-uses, ir-verify-ssa, and independent variable tracking
;;; are covered in ir-ssa-dominator-tests.lisp.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; Define a minimal subtype of ir-inst so we can test ir-collect-uses
;;; with real operand data.  This is local to this test file.
(defstruct (test-use-inst (:include cl-cc/ir:ir-inst) (:conc-name tuinst-))
  "Minimal ir-inst subtype used only in tests."
  (operands nil :type list))

(defmethod cl-cc/ir:ir-operands ((inst test-use-inst))
  (tuinst-operands inst))

(deftest ir-ssa-unsealed-creates-incomplete-phi
  "ir-read-var on an unsealed block with predecessors creates a placeholder."
  (let* ((fn (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (blk (cl-cc/ir:ir-new-block fn :unsealed)))
    ;; Add a predecessor so preds is non-nil, but do NOT seal blk
    (cl-cc/ir:ir-add-edge entry blk)
    ;; Block is NOT sealed — reading should insert an incomplete phi
    (let ((result (cl-cc/ir:ir-read-var fn 'x blk)))
      (assert-true (cl-cc/ir:ir-value-p result))
      (assert-true (> (length (cl-cc/ir:irb-params blk)) 0)))))

(deftest ir-ssa-seal-resolves-incomplete-phi
  "ir-seal-block resolves incomplete phis created before sealing."
  (let* ((fn (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (blk (cl-cc/ir:ir-new-block fn :target))
         (val (cl-cc/ir:ir-new-value fn)))
    ;; Write x in entry, add edge, read before sealing target
    (cl-cc/ir:ir-write-var fn 'x entry val)
    (cl-cc/ir:ir-add-edge entry blk)
    ;; Read x in unsealed blk — creates incomplete phi
    (let ((placeholder (cl-cc/ir:ir-read-var fn 'x blk)))
      (assert-true (cl-cc/ir:ir-value-p placeholder))
      ;; Now seal — should resolve the placeholder
      (cl-cc/ir:ir-seal-block fn entry)
      (cl-cc/ir:ir-seal-block fn blk)
      ;; After sealing, reading x should still work
      (let ((resolved (cl-cc/ir:ir-read-var fn 'x blk)))
        (assert-true (cl-cc/ir:ir-value-p resolved))))))

(deftest-each ir-ssa-phi-elimination-cases
  "ir-read-var at join: same value → trivially eliminated (0 params); distinct → kept (>0 params)."
  :cases (("same-value"     t)
          ("distinct-values" nil))
  (same-value-p)
  (let* ((fn    (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (left  (cl-cc/ir:ir-new-block fn :left))
         (right (cl-cc/ir:ir-new-block fn :right))
         (join  (cl-cc/ir:ir-new-block fn :join))
         (v1    (cl-cc/ir:ir-new-value fn))
         (v2    (if same-value-p v1 (cl-cc/ir:ir-new-value fn))))
    (cl-cc/ir:ir-add-edge entry left)  (cl-cc/ir:ir-add-edge entry right)
    (cl-cc/ir:ir-add-edge left join)   (cl-cc/ir:ir-add-edge right join)
    (cl-cc/ir:ir-seal-block fn entry)  (cl-cc/ir:ir-seal-block fn left)
    (cl-cc/ir:ir-seal-block fn right)  (cl-cc/ir:ir-seal-block fn join)
    (cl-cc/ir:ir-write-var fn 'x left v1)
    (cl-cc/ir:ir-write-var fn 'x right v2)
    (let ((result (cl-cc/ir:ir-read-var fn 'x join)))
      (assert-true (cl-cc/ir:ir-value-p result))
      (if same-value-p
          (progn (assert-eq v1 result)
                 (assert-= 0 (length (cl-cc/ir:irb-params join))))
          (assert-true (> (length (cl-cc/ir:irb-params join)) 0))))))

(deftest ir-ssa-loop-self-reference
  "A loop back-edge produces a block arg that refers to itself."
  (let* ((fn (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (loop-hdr (cl-cc/ir:ir-new-block fn :loop))
         (v-init (cl-cc/ir:ir-new-value fn))
         (v-update (cl-cc/ir:ir-new-value fn)))
    ;; entry -> loop-hdr, loop-hdr -> loop-hdr (back-edge)
    (cl-cc/ir:ir-add-edge entry loop-hdr)
    (cl-cc/ir:ir-add-edge loop-hdr loop-hdr)
    (cl-cc/ir:ir-seal-block fn entry)
    (cl-cc/ir:ir-seal-block fn loop-hdr)
    ;; Write initial value from entry
    (cl-cc/ir:ir-write-var fn 'x entry v-init)
    ;; Write updated value from within loop
    (cl-cc/ir:ir-write-var fn 'x loop-hdr v-update)
    ;; Read x in loop header — should return the loop-local definition
    (let ((result (cl-cc/ir:ir-read-var fn 'x loop-hdr)))
      (assert-true (cl-cc/ir:ir-value-p result))
      (assert-eq v-update result))))

(deftest ir-ssa-seal-clears-incomplete-phis
  "After ir-seal-block, the incomplete-phis hash table is empty."
  (let* ((fn (cl-cc/ir:ir-make-function 'test-fn))
         (entry (cl-cc/ir:irf-entry fn))
         (blk (cl-cc/ir:ir-new-block fn :target)))
    (cl-cc/ir:ir-add-edge entry blk)
    ;; Read before sealing — creates incomplete phi
    (cl-cc/ir:ir-read-var fn 'x blk)
    (assert-true (> (hash-table-count (cl-cc/ir:irb-incomplete-phis blk)) 0))
    ;; Seal clears the table
    (cl-cc/ir:ir-seal-block fn entry)
    (cl-cc/ir:ir-seal-block fn blk)
    (assert-= 0 (hash-table-count (cl-cc/ir:irb-incomplete-phis blk)))))
