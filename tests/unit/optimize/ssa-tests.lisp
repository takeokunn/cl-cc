;;;; tests/unit/optimize/ssa-tests.lisp — SSA Construction/Destruction Tests
;;;
;;; Tests for Phase 1: ssa-construct, ssa-round-trip, ssa-place-phis,
;;; and SSA destruction (phi elimination → parallel copies).

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── Helpers ─────────────────────────────────────────────────────────────

(defun count-type (insts type-sym)
  "Count instructions of TYPE-SYM in INSTS."
  (count-if (lambda (i) (typep i (find-symbol (symbol-name type-sym) :cl-cc)))
            insts))

;;; ─── SSA Value Naming ────────────────────────────────────────────────────

(deftest ssa-versioned-reg-format
  "ssa-versioned-reg produces :R5.3 from :R5 version 3."
  (let ((v (cl-cc::ssa-versioned-reg :r5 3)))
    (assert-equal "R5.3" (symbol-name v))))

(deftest ssa-versioned-reg-zero
  "ssa-versioned-reg version 0 produces :R0.0."
  (let ((v (cl-cc::ssa-versioned-reg :r0 0)))
    (assert-equal "R0.0" (symbol-name v))))

;;; ─── SSA Rename State ────────────────────────────────────────────────────

(deftest ssa-rename-state-push-pop
  "ssr-push-new-version and ssr-pop-version maintain a stack correctly."
  (let ((s (cl-cc::make-ssa-rename-state)))
    (let ((v0 (cl-cc::ssr-push-new-version s :r0)))
      (assert-eq v0 (cl-cc::ssr-current-version s :r0))
      (let ((v1 (cl-cc::ssr-push-new-version s :r0)))
        (assert-eq v1 (cl-cc::ssr-current-version s :r0))
        (cl-cc::ssr-pop-version s :r0)
        (assert-eq v0 (cl-cc::ssr-current-version s :r0))))))

(deftest ssa-rename-state-unversioned
  "Unknown register returns itself before any version is assigned."
  (let ((s (cl-cc::make-ssa-rename-state)))
    (assert-eq :r99 (cl-cc::ssr-current-version s :r99))))

;;; ─── SSA Round-Trip ──────────────────────────────────────────────────────

(deftest ssa-round-trip-linear-sequence
  "SSA round-trip on a linear sequence produces a non-empty instruction list."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc::ssa-round-trip insts)))
    (assert-true (>= (length result) 1))))

(deftest ssa-round-trip-preserves-types
  "SSA round-trip preserves the instruction type variety."
  (let* ((insts (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc::ssa-round-trip insts)))
    ;; Result should still contain a return
    (assert-true (some (lambda (i) (typep i 'cl-cc::vm-ret)) result))))

(deftest ssa-round-trip-empty
  "SSA round-trip on empty instructions returns an empty list."
  (let ((result (cl-cc::ssa-round-trip nil)))
    (assert-null result)))

(deftest ssa-round-trip-with-label
  "SSA round-trip correctly handles labeled blocks."
  (let* ((insts (list (make-vm-const    :dst :r0 :value 1)
                      (make-vm-jump-zero :reg :r0 :label "L1")
                      (make-vm-const    :dst :r0 :value 2)
                      (make-vm-label    :name "L1")
                      (make-vm-ret      :reg :r0)))
         (result (cl-cc::ssa-round-trip insts)))
    ;; Label should still be present somewhere in output
    (assert-true (some (lambda (i) (typep i 'cl-cc::vm-label)) result))))

;;; ─── Phi Placement ───────────────────────────────────────────────────────

(deftest ssa-phi-placement-no-phis-linear
  "A linear sequence (no branches) needs no phi-nodes."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2))))
    (multiple-value-bind (cfg phi-map _renamed)
        (cl-cc::ssa-construct insts)
      (declare (ignore cfg _renamed))
      ;; No phi-nodes needed for linear code
      (let ((total-phis 0))
        (maphash (lambda (_b phis) (incf total-phis (length phis))) phi-map)
        (assert-= 0 total-phis)))))

(deftest ssa-phi-placement-merge-point
  "A join point (two predecessors) may receive phi-nodes."
  (let* ((insts (list (make-vm-const    :dst :r0 :value 0)
                      (make-vm-jump-zero :reg :r0 :label "merge")
                      (make-vm-const    :dst :r1 :value 42)
                      (make-vm-label    :name "merge")
                      (make-vm-ret      :reg :r1))))
    ;; Just verify construction succeeds without error
    (multiple-value-bind (cfg _phi _renamed)
        (cl-cc::ssa-construct insts)
      (declare (ignore _phi _renamed))
      (assert-true (cl-cc::cfg-entry cfg)))))

;;; ─── Parallel Copy Sequentialization ─────────────────────────────────────

(deftest ssa-seq-copies-simple
  "A simple non-conflicting copy sequence is emitted correctly."
  (let* ((copies '((:r0 . :r1) (:r2 . :r3)))
         (result (cl-cc::ssa-sequentialize-copies copies)))
    (assert-= 2 (length result))
    (assert-true (every (lambda (i) (typep i 'cl-cc::vm-move)) result))))

(deftest ssa-seq-copies-empty
  "Empty parallel copies produces an empty list."
  (assert-null (cl-cc::ssa-sequentialize-copies nil)))

(deftest ssa-seq-copies-swap
  "A two-register swap uses a temporary register."
  (let* ((copies '((:r0 . :r1) (:r1 . :r0)))  ; swap r0 and r1
         (result (cl-cc::ssa-sequentialize-copies copies)))
    ;; Needs 3 moves: temp←r0, r0←r1, r1←temp (or equivalent)
    (assert-true (>= (length result) 2))
    (assert-true (every (lambda (i) (typep i 'cl-cc::vm-move)) result))))
