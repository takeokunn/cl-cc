;;;; tests/unit/compile/ir/ir-block-ssa-tests.lisp — IR SSA Tests
;;;
;;; Tests for ir-collect-uses, ir-verify-ssa, Braun SSA
;;; (ir-write-var/ir-read-var), ir-seal-block, ir-new-value, ir-new-block.
;;; Depends on make-test-fn defined in ir-block-tests.lisp (same package, loads first).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── ir-collect-uses ────────────────────────────────────────────────────────

(deftest ir-collect-uses-empty
  "ir-collect-uses on empty function returns empty table."
  (let* ((fn (make-test-fn))
         (uses (cl-cc/ir:ir-collect-uses fn)))
    (assert-= 0 (hash-table-count uses))))

;;; ─── ir-verify-ssa ──────────────────────────────────────────────────────────

(deftest ir-verify-ssa-behavior
  "ir-verify-ssa: returns T for valid SSA; signals error on duplicate defs."
  (let* ((fn (make-test-fn))
         (blk (cl-cc/ir:irf-entry fn))
         (v1 (cl-cc/ir:ir-new-value fn))
         (v2 (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-emit blk (cl-cc/ir:make-ir-inst :result v1))
    (cl-cc/ir:ir-emit blk (cl-cc/ir:make-ir-inst :result v2))
    (assert-true (cl-cc/ir:ir-verify-ssa fn)))
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
  (let* ((fn (make-test-fn))
         (blk (cl-cc/ir:irf-entry fn))
         (val (cl-cc/ir:ir-new-value fn)))
    (cl-cc/ir:ir-write-var fn 'x blk val)
    (assert-eq val (cl-cc/ir:ir-read-var fn 'x blk)))
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
    (let ((result (cl-cc/ir:ir-read-var fn 'x join)))
      (assert-true (cl-cc/ir:ir-value-p result))
      (assert-true (> (length (cl-cc/ir:irb-params join)) 0)))))

(deftest-each ir-ssa-sealed-block-behavior
  "ir-seal-block marks block sealed; ir-read-var on sealed orphan with no def returns nil."
  :cases (("marks-sealed"       :sealed)
          ("no-def-returns-nil" :read-nil))
  (scenario)
  (let* ((fn  (make-test-fn))
         (blk (cl-cc/ir:ir-new-block fn :orphan)))
    (cl-cc/ir:ir-seal-block fn blk)
    (ecase scenario
      (:sealed   (assert-true  (cl-cc/ir:irb-sealed-p blk)))
      (:read-nil (assert-false (cl-cc/ir:ir-read-var fn 'x blk))))))

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
    (assert-= 0 (cl-cc/ir:irb-id (cl-cc/ir:irf-entry fn)))
    (assert-= 1 (cl-cc/ir:irb-id b1))
    (assert-= 2 (length (cl-cc/ir:irf-blocks fn)))
    (assert-true (cl-cc/ir:ir-block-p (cl-cc/ir:irf-entry fn)))
    (assert-eq :entry (cl-cc/ir:irb-label (cl-cc/ir:irf-entry fn)))))
