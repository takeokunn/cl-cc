;;;; tests/unit/optimize/effects-tests.lisp — Effect-Kind System Tests
;;;
;;; Tests for vm-inst-effect-kind, opt-inst-pure-p, opt-inst-dce-eligible-p,
;;; and opt-inst-cse-eligible-p (Phase 0 of optimizer modernization).

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── vm-inst-effect-kind ─────────────────────────────────────────────────

(deftest-each effect-kind-pure
  "Arithmetic and comparison instructions are classified as :pure."
  :cases (("vm-const"   (make-vm-const :dst :r0 :value 42))
          ("vm-move"    (make-vm-move  :dst :r0 :src :r1))
          ("vm-add"     (make-vm-add   :dst :r0 :lhs :r1 :rhs :r2))
          ("vm-sub"     (make-vm-sub   :dst :r0 :lhs :r1 :rhs :r2))
          ("vm-mul"     (make-vm-mul   :dst :r0 :lhs :r1 :rhs :r2)))
  (inst)
  (assert-eq :pure (cl-cc::vm-inst-effect-kind inst)))

(deftest-each effect-kind-control
  "Control flow instructions are classified as :control."
  :cases (("vm-ret"  (make-vm-ret  :reg :r0))
          ("vm-jump" (make-vm-jump :label "L0")))
  (inst)
  (assert-eq :control (cl-cc::vm-inst-effect-kind inst)))

(deftest effect-kind-call-unknown
  "vm-call is classified as :unknown (conservative)."
  (assert-eq :unknown (cl-cc::vm-inst-effect-kind
                        (make-vm-call :dst :r0 :func :r1 :args nil))))

;;; ─── opt-inst-pure-p ─────────────────────────────────────────────────────

(deftest-each opt-pure-arithmetic
  "All arithmetic instructions are pure."
  :cases (("add" (make-vm-add :dst :r0 :lhs :r1 :rhs :r2))
          ("sub" (make-vm-sub :dst :r0 :lhs :r1 :rhs :r2))
          ("mul" (make-vm-mul :dst :r0 :lhs :r1 :rhs :r2))
          ("neg" (make-vm-neg :dst :r0 :src :r1))
          ("inc" (make-vm-inc :dst :r0 :src :r1))
          ("dec" (make-vm-dec :dst :r0 :src :r1)))
  (inst)
  (assert-true (cl-cc::opt-inst-pure-p inst)))

(deftest-each opt-pure-comparison
  "Comparison instructions are pure."
  :cases (("lt" (make-vm-lt :dst :r0 :lhs :r1 :rhs :r2))
          ("gt" (make-vm-gt :dst :r0 :lhs :r1 :rhs :r2))
          ("le" (make-vm-le :dst :r0 :lhs :r1 :rhs :r2))
          ("ge" (make-vm-ge :dst :r0 :lhs :r1 :rhs :r2)))
  (inst)
  (assert-true (cl-cc::opt-inst-pure-p inst)))

(deftest-each opt-pure-type-predicates
  "Type predicate instructions are pure."
  :cases (("null-p"   (make-vm-null-p   :dst :r0 :src :r1))
          ("cons-p"   (make-vm-cons-p   :dst :r0 :src :r1))
          ("number-p" (make-vm-number-p :dst :r0 :src :r1)))
  (inst)
  (assert-true (cl-cc::opt-inst-pure-p inst)))

(deftest-each opt-not-pure-io
  "I/O instructions are not pure."
  :cases (("print" (make-vm-print :reg :r0)))
  (inst)
  (assert-false (cl-cc::opt-inst-pure-p inst)))

(deftest opt-not-pure-call
  "vm-call is not pure (unknown effects)."
  (assert-false (cl-cc::opt-inst-pure-p
                  (make-vm-call :dst :r0 :func :r1 :args nil))))

;;; ─── opt-inst-dce-eligible-p ─────────────────────────────────────────────

(deftest dce-eligible-pure-instructions
  "Pure instructions are DCE-eligible."
  (assert-true (cl-cc::opt-inst-dce-eligible-p
                 (make-vm-add :dst :r0 :lhs :r1 :rhs :r2))))

(deftest dce-eligible-alloc
  "Allocation instructions (vm-cons) are DCE-eligible."
  (assert-true (cl-cc::opt-inst-dce-eligible-p
                 (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))))

(deftest dce-not-eligible-print
  "I/O instructions are NOT DCE-eligible."
  (assert-false (cl-cc::opt-inst-dce-eligible-p
                  (make-vm-print :reg :r0))))

(deftest dce-not-eligible-set-global
  "vm-set-global is NOT DCE-eligible."
  (assert-false (cl-cc::opt-inst-dce-eligible-p
                  (make-vm-set-global :src :r0 :var-name 'x))))

;;; ─── opt-inst-cse-eligible-p ─────────────────────────────────────────────

(deftest cse-eligible-pure
  "Pure instructions are CSE-eligible."
  (assert-true (cl-cc::opt-inst-cse-eligible-p
                 (make-vm-add :dst :r0 :lhs :r1 :rhs :r2))))

(deftest cse-not-eligible-alloc
  "Allocation instructions (vm-cons) are NOT CSE-eligible (creates distinct objects)."
  (assert-false (cl-cc::opt-inst-cse-eligible-p
                  (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))))

;;; ─── DCE Extended Coverage ───────────────────────────────────────────────

(deftest dce-eliminates-dead-add
  "Dead vm-add with unused result is eliminated by the extended DCE pass."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1) ; dead: r2 unused
                      (make-vm-ret   :reg :r0)))
         (result (cl-cc::opt-pass-dce insts)))
    (assert-false (some (lambda (i) (typep i 'cl-cc::vm-add)) result))))

(deftest dce-preserves-used-add
  "vm-add whose result IS used is preserved."
  (let* ((insts (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-const :dst :r1 :value 2)
                      (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                      (make-vm-ret   :reg :r2)))  ; r2 is used here
         (result (cl-cc::opt-pass-dce insts)))
    (assert-true (some (lambda (i) (typep i 'cl-cc::vm-add)) result))))
