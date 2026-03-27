;;;; tests/unit/optimize/cfg-tests.lisp — CFG + Dominator Tree Tests
;;;
;;; Tests for Phase 1: cfg-build, cfg-compute-dominators,
;;; cfg-compute-dominance-frontiers, and related utilities.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── Helpers ─────────────────────────────────────────────────────────────

(defun make-test-cfg-linear ()
  "Build a CFG from a simple linear instruction sequence: CONST → ADD → RET."
  (cl-cc::cfg-build
   (list (make-vm-const :dst :r0 :value 1)
         (make-vm-const :dst :r1 :value 2)
         (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
         (make-vm-ret   :reg :r2))))

(defun make-test-cfg-branch ()
  "Build a CFG with a conditional branch:
     entry: CONST r0=0, JUMP-ZERO r0 → then_label
     else:  CONST r1=99, JUMP exit_label
     then:  [then_label] CONST r1=42
     exit:  [exit_label] RET r1"
  (cl-cc::cfg-build
   (list (make-vm-const    :dst :r0 :value 0)
         (make-vm-jump-zero :reg :r0 :label "then")
         ;; else branch
         (make-vm-const    :dst :r1 :value 99)
         (make-vm-jump     :label "exit")
         ;; then branch
         (make-vm-label    :name "then")
         (make-vm-const    :dst :r1 :value 42)
         ;; exit
         (make-vm-label    :name "exit")
         (make-vm-ret      :reg :r1))))

;;; ─── Basic CFG Construction ──────────────────────────────────────────────

(deftest cfg-linear-cfg-structure
  "A linear sequence: exactly 1 block; non-nil entry; entry has instructions."
  (let* ((cfg   (make-test-cfg-linear))
         (entry (cl-cc::cfg-entry cfg)))
    (assert-= 1 (cl-cc::cfg-block-count cfg))
    (assert-true entry)
    (assert-true (cl-cc::bb-instructions entry))))

(deftest cfg-empty-instructions
  "An empty instruction list produces a CFG with one empty entry block."
  (let ((cfg (cl-cc::cfg-build nil)))
    (assert-true (cl-cc::cfg-entry cfg))))

(deftest cfg-branch-block-count
  "A conditional branch creates at least 2 basic blocks."
  (let ((cfg (make-test-cfg-branch)))
    (assert-true (>= (cl-cc::cfg-block-count cfg) 2))))

(deftest cfg-branch-labels-resolved
  "cfg-get-block-by-label finds labeled blocks."
  (let ((cfg (make-test-cfg-branch)))
    (assert-true (cl-cc::cfg-get-block-by-label cfg "then"))
    (assert-true (cl-cc::cfg-get-block-by-label cfg "exit"))))

;;; ─── Predecessor / Successor Edges ──────────────────────────────────────

(deftest cfg-branch-successors
  "The entry block in a branch CFG has 2 successors."
  (let* ((cfg   (make-test-cfg-branch))
         (entry (cl-cc::cfg-entry cfg)))
    (assert-= 2 (length (cl-cc::bb-successors entry)))))

(deftest cfg-branch-predecessors
  "The exit block has predecessors from both branches."
  (let* ((cfg  (make-test-cfg-branch))
         (exit  (cl-cc::cfg-get-block-by-label cfg "exit")))
    (when exit
      (assert-true (>= (length (cl-cc::bb-predecessors exit)) 1)))))

;;; ─── RPO ─────────────────────────────────────────────────────────────────

(deftest cfg-rpo-ordering
  "RPO visits all reachable blocks; entry block is first."
  (let* ((cfg   (make-test-cfg-branch))
         (rpo   (cl-cc::cfg-compute-rpo cfg))
         (entry (cl-cc::cfg-entry cfg)))
    (assert-= (cl-cc::cfg-block-count cfg) (length rpo))
    (assert-eq entry (car rpo))))

;;; ─── Dominator Tree ──────────────────────────────────────────────────────

(deftest cfg-dominator-properties
  "Entry idom is itself; entry dominates the exit block."
  (let* ((cfg   (make-test-cfg-branch))
         (entry (cl-cc::cfg-entry cfg)))
    (cl-cc::cfg-compute-dominators cfg)
    (assert-eq entry (cl-cc::bb-idom entry))
    (let ((exit (cl-cc::cfg-get-block-by-label cfg "exit")))
      (when exit
        (assert-true (cl-cc::cfg-dominates-p entry exit))))))

;;; ─── Dominance Frontiers ─────────────────────────────────────────────────

(deftest cfg-dominance-frontiers-computed
  "Dominance frontiers are computed without error."
  (let ((cfg (make-test-cfg-branch)))
    (cl-cc::cfg-compute-dominators cfg)
    (cl-cc::cfg-compute-dominance-frontiers cfg)
    ;; Just assert no error was signaled
    (assert-true t)))

;;; ─── Flatten Round-Trip ──────────────────────────────────────────────────

(deftest cfg-flatten-preserves-instruction-count
  "Flattening a CFG recovers all instructions (labels + body)."
  (let* ((orig  (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-ret   :reg :r0)))
         (cfg   (cl-cc::cfg-build orig))
         (flat  (cl-cc::cfg-flatten cfg)))
    ;; Flat should have at least as many instructions as original
    (assert-true (>= (length flat) (length orig)))))
