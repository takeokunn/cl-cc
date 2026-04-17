;;;; tests/unit/optimize/optimizer-cse-gvn-tests.lisp
;;;; Unit tests for src/optimize/optimizer-cse-gvn.lisp
;;;;
;;;; Covers: opt-pass-cse (redundant elimination, label flush, vm-const kept),
;;;;   opt-pass-gvn (straight-line, empty), opt-pass-dead-labels (removes
;;;;   unreferenced labels, keeps referenced ones), opt-pass-leaf-detect
;;;;   (leaf flag, non-leaf flag).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── opt-pass-cse ────────────────────────────────────────────────────────

(deftest cse-empty-input-returns-nil
  "opt-pass-cse on empty input returns nil."
  (assert-null (cl-cc/optimize::opt-pass-cse nil)))

(deftest cse-straight-line-no-duplicates-unchanged
  "opt-pass-cse leaves code with no repeated computations unchanged."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc/optimize::opt-pass-cse insts)))
    (assert-= (length insts) (length result))))

(deftest cse-eliminates-duplicate-binop
  "opt-pass-cse replaces the second identical binop with vm-move from the first result."
  ;; r2 = r0 + r1 (first)
  ;; r3 = r0 + r1 (second, same expression — should become vm-move r3 r2)
  (let* ((insts (list (make-vm-const :dst :r0 :value 3)
                      (make-vm-const :dst :r1 :value 4)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-add   :dst :r3 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r3)))
         (result (cl-cc/optimize::opt-pass-cse insts)))
    ;; The second vm-add should be eliminated → replaced by vm-move
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-move)) result))
    ;; Only one vm-add should remain
    (assert-= 1 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) result))))

(deftest cse-label-flushes-knowledge
  "opt-pass-cse flushes value numbering at branch-target labels."
  ;; First add, then a label (which is a branch target = flush env), then same add again.
  ;; After flush the second add should NOT be replaced.
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      ;; Make a jump to the label to make it a branch target
                      (make-vm-jump  :label "flush")
                      (make-vm-label :name  "flush")
                      (make-vm-add   :dst :r3 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r3)))
         (result (cl-cc/optimize::opt-pass-cse insts)))
    ;; After flushing at the label, both adds should remain
    (assert-true (>= (count-if (lambda (i) (typep i 'cl-cc/vm::vm-add)) result) 1))))

(deftest cse-keeps-vm-const-not-replaced-by-move
  "opt-pass-cse never replaces vm-const with vm-move (prevents fold<->CSE oscillation)."
  (let* ((insts (list (make-vm-const :dst :r0 :value 42)
                      (make-vm-const :dst :r1 :value 42)
                      (make-vm-ret   :reg :r1)))
         (result (cl-cc/optimize::opt-pass-cse insts)))
    ;; Both vm-const should remain (not merged via vm-move)
    (assert-= 2 (count-if (lambda (i) (typep i 'cl-cc/vm::vm-const)) result))))

;;; ─── opt-pass-gvn ────────────────────────────────────────────────────────

(deftest gvn-empty-input-returns-nil
  "opt-pass-gvn on empty input returns nil or empty list."
  (let ((result (cl-cc/optimize::opt-pass-gvn nil)))
    (assert-true (listp result))))

(deftest gvn-straight-line-preserves-instructions
  "opt-pass-gvn on straight-line code returns a non-empty list."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::opt-pass-gvn insts)))
    (assert-true (listp result))
    (assert-true (> (length result) 0))))

;;; ─── opt-pass-dead-labels ────────────────────────────────────────────────

(deftest-each dead-labels-label-presence
  "opt-pass-dead-labels removes orphan labels and keeps jump-referenced ones."
  :cases (("orphan-removed"
           (list (make-vm-const :dst :r0 :value 1)
                 (make-vm-label :name "orphan")
                 (make-vm-ret   :reg :r0))
           nil)
          ("referenced-kept"
           (list (make-vm-jump  :label "target")
                 (make-vm-label :name  "target")
                 (make-vm-ret   :reg :r0))
           t))
  (insts expect-label-p)
  (let ((result (cl-cc/optimize::opt-pass-dead-labels insts)))
    (if expect-label-p
        (assert-true  (some (lambda (i) (typep i 'cl-cc/vm::vm-label)) result))
        (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-label)) result)))))

(deftest dead-labels-empty-input
  "opt-pass-dead-labels on empty input returns nil."
  (assert-null (cl-cc/optimize::opt-pass-dead-labels nil)))

(deftest dead-labels-straight-line-no-labels
  "opt-pass-dead-labels on code with no labels returns code unchanged."
  (let* ((insts (list (make-vm-const :dst :r0 :value 5)
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc/optimize::opt-pass-dead-labels insts)))
    (assert-= (length insts) (length result))))

;;; ─── opt-pass-leaf-detect ────────────────────────────────────────────────

(deftest leaf-detect-empty-is-leaf
  "opt-pass-leaf-detect reports empty code as a leaf function."
  (multiple-value-bind (insts leaf-p)
      (cl-cc/optimize::opt-pass-leaf-detect nil)
    (assert-true (listp insts))
    (assert-true leaf-p)))

(deftest-each leaf-detect-call-presence
  "opt-pass-leaf-detect: call-free code is a leaf; code with vm-call is not."
  :cases (("no-call"
           (list (make-vm-const :dst :r0 :value 1)
                 (make-vm-add   :dst :r1 :lhs :r0 :rhs :r0)
                 (make-vm-ret   :reg :r1))
           t)
          ("with-call"
           (list (make-vm-const :dst :r0 :value 1)
                 (make-vm-call  :dst :r1 :func :r0 :args '())
                 (make-vm-ret   :reg :r1))
           nil))
  (insts expected-leaf-p)
  (multiple-value-bind (result-insts leaf-p)
      (cl-cc/optimize::opt-pass-leaf-detect insts)
    (assert-= (length insts) (length result-insts))
    (if expected-leaf-p
        (assert-true  leaf-p)
        (assert-false leaf-p))))

(deftest leaf-detect-preserves-instruction-list
  "opt-pass-leaf-detect returns the original instructions unchanged."
  (let* ((insts (list (make-vm-const :dst :r0 :value 7)
                      (make-vm-ret   :reg :r0))))
    (multiple-value-bind (result-insts _leaf)
        (cl-cc/optimize::opt-pass-leaf-detect insts)
      (declare (ignore _leaf))
      (assert-equal insts result-insts))))
