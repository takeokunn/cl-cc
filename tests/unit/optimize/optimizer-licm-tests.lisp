;;;; tests/unit/optimize/optimizer-licm-tests.lisp
;;;; Unit tests for src/optimize/optimizer-licm.lisp
;;;;
;;;; Covers: opt-inst-loop-invariant-p, %opt-pre-expression-key,
;;;;   %opt-pre-splice-before-terminator, opt-pass-licm (trivial paths),
;;;;   opt-pass-constant-hoist (wrapper), opt-pass-egraph (wrapper).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── opt-inst-loop-invariant-p ───────────────────────────────────────────

(deftest-each licm-invariant-p-cases
  "opt-inst-loop-invariant-p: pure const is invariant; loop-defined read reg makes it variant; impure instruction is never invariant."
  :cases (("pure-const"   (make-vm-const :dst :r0 :value 42)         nil   t)
          ("dst-in-loop"  (make-vm-add   :dst :r1 :lhs :r0 :rhs :r0) :r0   nil)
          ("impure"       (make-vm-halt  :reg :r0)                    nil   nil))
  (inst def-reg expected)
  (let ((loop-def-regs (make-hash-table :test #'eq))
        (loop-members  (make-hash-table :test #'eq))
        (def-sites     (make-hash-table :test #'eq)))
    (when def-reg
      (setf (gethash def-reg loop-def-regs) t))
    (if expected
        (assert-true  (cl-cc::opt-inst-loop-invariant-p inst loop-def-regs loop-members def-sites))
        (assert-false (cl-cc::opt-inst-loop-invariant-p inst loop-def-regs loop-members def-sites)))))

;;; ─── %opt-pre-expression-key ─────────────────────────────────────────────

(deftest pre-expression-key-returns-const-key-for-vm-const
  "%opt-pre-expression-key returns (:const value) for a vm-const instruction."
  (let* ((inst (make-vm-const :dst :r0 :value 7))
         (key  (cl-cc::%opt-pre-expression-key inst)))
    (assert-equal '(:const 7) key)))

(deftest pre-expression-key-returns-type-regs-for-binop
  "%opt-pre-expression-key returns (type . regs) for a pure binary instruction."
  (let* ((inst (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
         (key  (cl-cc::%opt-pre-expression-key inst)))
    (assert-true (consp key))
    (assert-eq 'cl-cc::vm-add (car key))))

(deftest pre-expression-key-returns-nil-for-impure
  "%opt-pre-expression-key returns nil for an impure (side-effecting) instruction."
  (let* ((inst (make-vm-halt :reg :r0))
         (key  (cl-cc::%opt-pre-expression-key inst)))
    (assert-null key)))

(deftest pre-expression-key-commutative-sorts-operands
  "%opt-pre-expression-key produces the same key regardless of lhs/rhs order
   for commutative instructions (vm-add is commutative)."
  (let* ((inst-ab (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
         (inst-ba (make-vm-add :dst :r2 :lhs :r1 :rhs :r0))
         (key-ab  (cl-cc::%opt-pre-expression-key inst-ab))
         (key-ba  (cl-cc::%opt-pre-expression-key inst-ba)))
    (assert-equal key-ab key-ba)))

;;; ─── %opt-pre-splice-before-terminator ───────────────────────────────────

(deftest-each pre-splice-inserts-before-terminator
  "%opt-pre-splice-before-terminator places an extra instruction just before vm-jump and vm-ret terminators."
  :cases (("before-jump"  (list (make-vm-const :dst :r0 :value 1) (make-vm-jump :label "end"))  'cl-cc::vm-jump)
          ("before-ret"   (list (make-vm-move  :dst :r0 :src :r1) (make-vm-ret  :reg  :r0))     'cl-cc::vm-ret))
  (insts term-type)
  (let* ((extra  (make-vm-const :dst :r9 :value 0))
         (result (cl-cc::%opt-pre-splice-before-terminator insts (list extra))))
    (assert-= 3 (length result))
    (assert-true (typep (second result) 'cl-cc::vm-const))
    (assert-true (typep (third  result) term-type))))

(deftest pre-splice-appends-when-no-terminator
  "%opt-pre-splice-before-terminator appends at end when no terminator is present."
  (let* ((const (make-vm-const :dst :r0 :value 5))
         (extra (make-vm-const :dst :r1 :value 6))
         (result (cl-cc::%opt-pre-splice-before-terminator
                  (list const)
                  (list extra))))
    (assert-= 2 (length result))
    (assert-true (typep (second result) 'cl-cc::vm-const))))

;;; ─── opt-pass-licm (trivial paths) ───────────────────────────────────────

(deftest licm-pass-returns-nil-for-empty-input
  "opt-pass-licm returns nil immediately for an empty instruction list."
  (assert-null (cl-cc::opt-pass-licm nil)))

(deftest licm-pass-straight-line-no-change
  "opt-pass-licm returns straight-line code unchanged (no loops detected)."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))
         (result (cl-cc::opt-pass-licm insts)))
    ;; No loops → pass returns same instructions
    (assert-= (length insts) (length result))))

;;; ─── opt-pass-constant-hoist (wrapper) ───────────────────────────────────

(deftest constant-hoist-is-alias-for-licm
  "opt-pass-constant-hoist is a thin wrapper: empty input → nil."
  (assert-null (cl-cc::opt-pass-constant-hoist nil)))

;;; ─── opt-pass-egraph (wrapper) ───────────────────────────────────────────

(deftest egraph-pass-returns-list-for-empty-input
  "opt-pass-egraph on empty instruction list returns a list (possibly empty)."
  (let ((result (cl-cc::opt-pass-egraph nil)))
    (assert-true (listp result))))
