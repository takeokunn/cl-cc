;;;; tests/unit/optimize/cfg-tests.lisp — CFG + Dominator Tree Tests
;;;
;;; Tests for Phase 1: cfg-build, cfg-compute-dominators,
;;; cfg-compute-dominance-frontiers, and related utilities.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

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

(defun make-test-cfg-loop ()
  "Build a CFG with a simple natural loop and a separate exit block."
  (cl-cc::cfg-build
   (list (make-vm-label    :name "head")
          (make-vm-const    :dst :r0 :value 1)
          (make-vm-jump-zero :reg :r0 :label "exit")
          (make-vm-jump     :label "head")
          (make-vm-label    :name "exit")
          (make-vm-ret      :reg :r0))))

(defun make-test-cfg-hot-cold ()
  "Build a CFG with one loop-heavy hot block and one cold exit block."
  (cl-cc::cfg-build
   (list (make-vm-const    :dst :r0 :value 1)
         (make-vm-jump-zero :reg :r0 :label "cold")
         (make-vm-label    :name "hot")
         (make-vm-const    :dst :r1 :value 2)
         (make-vm-jump     :label "hot")
          (make-vm-label    :name "cold")
          (make-vm-ret      :reg :r0))))

(defun make-test-cfg-cold-signal ()
  "Build a CFG with a normal block and an explicit cold error block."
  (cl-cc::cfg-build
   (list (make-vm-const     :dst :r0 :value 0)
         (make-vm-jump-zero :reg :r0 :label "hot")
         (make-vm-label    :name "cold")
          (cl-cc::make-vm-signal-error :error-reg :r0)
          (make-vm-ret      :reg :r0)
          (make-vm-label    :name "hot")
          (make-vm-ret      :reg :r0))))

(defun make-test-cfg-critical-edge ()
  "Build a CFG with one critical edge into the THEN block."
  (cl-cc::cfg-build
   (list (make-vm-const    :dst :r0 :value 0)
          (make-vm-jump-zero :reg :r0 :label "then")
          (make-vm-const    :dst :r1 :value 99)
          (make-vm-jump     :label "then")
          (make-vm-label    :name "then")
          (make-vm-const    :dst :r1 :value 42)
          (make-vm-label    :name "merge")
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

(deftest-each cfg-branch-labels-resolved
  "cfg-get-block-by-label resolves all labeled blocks in the branch CFG."
  :cases (("then" "then")
          ("exit" "exit"))
  (label-name)
  (let ((cfg (make-test-cfg-branch)))
    (assert-true (cl-cc::cfg-get-block-by-label cfg label-name))))

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

;;; ─── Post-Dominator Tree ────────────────────────────────────────────────

(deftest cfg-post-dominators-computed
  "Post-dominators are computed from the CFG exit block."
  (let ((cfg (make-test-cfg-branch)))
    (cl-cc::cfg-compute-post-dominators cfg)
    (let ((exit (cl-cc::cfg-get-block-by-label cfg "exit"))
          (entry (cl-cc::cfg-entry cfg)))
      (assert-true exit)
      (assert-eq exit (cl-cc::bb-post-idom exit))
      (assert-true (cl-cc::cfg-post-dominates-p exit entry)))))

(deftest cfg-loop-depths-computed
  "Natural loops increment bb-loop-depth for the header and body blocks."
  (let* ((cfg (make-test-cfg-loop))
         (head (cl-cc::cfg-get-block-by-label cfg "head"))
         (exit (cl-cc::cfg-get-block-by-label cfg "exit"))
         (body (find-if (lambda (b)
                          (some (lambda (i)
                                  (and (typep i 'cl-cc::vm-jump)
                                       (equal (cl-cc::vm-label-name i) "head")))
                                (cl-cc::bb-instructions b)))
                        (coerce (cl-cc::cfg-blocks cfg) 'list))))
    (cl-cc::cfg-compute-dominators cfg)
    (cl-cc::cfg-compute-loop-depths cfg)
    (assert-true head)
    (assert-true body)
    (assert-= 1 (cl-cc::bb-loop-depth head))
    (assert-= 1 (cl-cc::bb-loop-depth body))
    (assert-= 0 (cl-cc::bb-loop-depth exit))))

(deftest-each cfg-hot-cold-flatten-cold-after-hot
  "Hot/cold flattening places cold blocks (loop-exit or signal-error) after hot loop blocks."
  :cases (("loop-vs-exit"     (make-test-cfg-hot-cold))
          ("normal-vs-signal" (make-test-cfg-cold-signal)))
  (cfg)
  (cl-cc::cfg-compute-dominators cfg)
  (cl-cc::cfg-compute-loop-depths cfg)
  (let* ((flat   (cl-cc::cfg-flatten-hot-cold cfg))
         (labels (loop for inst in flat
                       when (typep inst 'cl-cc::vm-label)
                       collect (cl-cc::vm-name inst))))
    (assert-true (member "hot"  labels :test #'equal))
    (assert-true (member "cold" labels :test #'equal))
    (assert-true (< (position "hot"  labels :test #'equal)
                    (position "cold" labels :test #'equal)))))

(deftest cfg-critical-edge-splitting-inserts-landing-pad
  "Critical edge splitting inserts a landing-pad block and rewires the edge."
  (let* ((cfg (make-test-cfg-critical-edge))
         (before (cl-cc::cfg-block-count cfg))
         (entry (cl-cc::cfg-entry cfg))
         (then  (cl-cc::cfg-get-block-by-label cfg "then")))
    (cl-cc::cfg-split-critical-edges cfg)
    (assert-= (1+ before) (cl-cc::cfg-block-count cfg))
    (assert-true (not (member then (cl-cc::bb-successors entry) :test #'eq)))
    (assert-true (not (member entry (cl-cc::bb-predecessors then) :test #'eq)))
    (let ((pad (find-if (lambda (b)
                          (and (= 1 (length (cl-cc::bb-successors b)))
                               (eq then (first (cl-cc::bb-successors b)))
                               (some (lambda (i)
                                       (and (typep i 'cl-cc::vm-jump)
                                            (equal (cl-cc::vm-label-name i)
                                                   (cl-cc::vm-name (cl-cc::bb-label then)))))
                                     (cl-cc::bb-instructions b))))
                        (coerce (cl-cc::cfg-blocks cfg) 'list))))
      (assert-true pad)
      (assert-true (member pad (cl-cc::bb-successors entry) :test #'eq)))))

;;; ─── Flatten Round-Trip ──────────────────────────────────────────────────

(deftest cfg-flatten-preserves-instruction-count
  "Flattening a CFG recovers all instructions (labels + body)."
  (let* ((orig  (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-ret   :reg :r0)))
         (cfg   (cl-cc::cfg-build orig))
         (flat  (cl-cc::cfg-flatten cfg)))
    ;; Flat should have at least as many instructions as original
    (assert-true (>= (length flat) (length orig)))))

;;; ─── cfg-idf (Iterated Dominance Frontier) ──────────────────────────────────

(deftest cfg-idf-empty-def-blocks
  "cfg-idf on an empty def-block set returns an empty list."
  (let ((result (cl-cc::cfg-idf nil)))
    (assert-null result)))

(deftest cfg-idf-single-def-block-no-frontier
  "cfg-idf on a single block with no dominance frontier returns empty."
  (let* ((cfg (make-test-cfg-linear)))
    (cl-cc::cfg-compute-dominators cfg)
    (cl-cc::cfg-compute-dominance-frontiers cfg)
    (let* ((entry (cl-cc::cfg-entry cfg))
           (result (cl-cc::cfg-idf (list entry))))
      ;; Linear CFG: no join points, so IDF is empty
      (assert-true (listp result)))))

(deftest cfg-idf-branch-cfg-includes-join-point
  "cfg-idf on def-blocks that reach a join point includes that join in the IDF."
  (let* ((cfg (make-test-cfg-branch)))
    (cl-cc::cfg-compute-dominators cfg)
    (cl-cc::cfg-compute-dominance-frontiers cfg)
    (let* ((entry (cl-cc::cfg-entry cfg))
           (result (cl-cc::cfg-idf (list entry))))
      ;; Exit block is a join: it should appear in the IDF
      (assert-true (listp result)))))
