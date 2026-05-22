;;;; Unit tests for FR-601 loop unrolling.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(defun %fr601-counted-loop (&key (limit-constant-p t) (limit-value 3))
  "Build a small counted loop in the optimizer's canonical loop shape."
  (append (list (make-vm-const :dst :i :value 0))
          (when limit-constant-p
            (list (make-vm-const :dst :limit :value limit-value)))
          (list (make-vm-const :dst :one :value 1)
                (make-vm-label :name "loop")
                (make-vm-lt :dst :cond :lhs :i :rhs :limit)
                (make-vm-jump-zero :reg :cond :label "exit")
                (make-vm-print :reg :i)
                (make-vm-add :dst :i :lhs :i :rhs :one)
                (make-vm-jump :label "loop")
                (make-vm-label :name "exit")
                (make-vm-ret :reg :i))))

(deftest loop-unroll-fr601-full-unrolls-small-constant-trip-count
  "FR-601: constant trip count <= 8 is fully unrolled and the back-edge is gone."
  (let* ((out (cl-cc/optimize::opt-pass-loop-unroll
               (%fr601-counted-loop :limit-constant-p t :limit-value 3))))
    (assert-= 3 (count-if (lambda (inst) (typep inst 'cl-cc/vm::vm-print)) out))
    (assert-false (%test-label-position out "loop"))
    (assert-false (some (lambda (inst)
                          (and (typep inst 'cl-cc/vm::vm-jump)
                               (equal (cl-cc/vm::vm-label-name inst) "loop")))
                        out))
    (assert-true (%test-label-position out "exit"))))

(deftest loop-unroll-fr601-partially-unrolls-unknown-bound-with-remainder-loop
  "FR-601: unknown-bound loops get a factor-4 guarded prefix plus the original loop."
  (let* ((out (cl-cc/optimize::opt-pass-loop-unroll
               (%fr601-counted-loop :limit-constant-p nil))))
    (assert-= 5 (count-if (lambda (inst) (typep inst 'cl-cc/vm::vm-print)) out))
    (assert-= 5 (count-if (lambda (inst) (typep inst 'cl-cc/vm::vm-jump-zero)) out))
    (assert-true (%test-label-position out "loop"))
    (assert-true (some (lambda (inst)
                         (and (typep inst 'cl-cc/vm::vm-jump)
                              (equal (cl-cc/vm::vm-label-name inst) "loop")))
                       out))))
