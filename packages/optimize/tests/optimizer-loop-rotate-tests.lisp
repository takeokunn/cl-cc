;;;; Unit tests for FR-683 loop rotation.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest loop-rotate-fr683-while-becomes-bottom-guard
  "FR-683: opt-pass-loop-rotate rewrites a while-header loop into bottom-guard form."
  (let* ((insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :limit :value 8)
                      (make-vm-label :name "loop")
                      (make-vm-lt :dst :cond :lhs :i :rhs :limit)
                      (make-vm-jump-zero :reg :cond :label "exit")
                      (make-vm-add :dst :i :lhs :i :rhs :one)
                      (make-vm-jump :label "loop")
                      (make-vm-label :name "exit")
                      (make-vm-ret :reg :i)))
         (out (cl-cc/optimize::opt-pass-loop-rotate insts))
         (body-pos (%test-label-position out "loop_body"))
         (guard-pos (%test-label-position out "loop_guard"))
         (exit-pos (%test-label-position out "exit")))
    (assert-true body-pos)
    (assert-true guard-pos)
    (assert-true exit-pos)
    (assert-true (< body-pos guard-pos exit-pos))
    (assert-true (typep (third out) 'cl-cc/vm::vm-jump))
    (assert-equal "loop_guard" (cl-cc/vm::vm-label-name (third out)))
    (assert-true
     (some (lambda (inst)
             (and (typep inst 'cl-cc/vm::vm-jump)
                  (equal (cl-cc/vm::vm-label-name inst) "loop_body")))
           out))))

(deftest loop-rotate-fr683-skips-multiple-backedges
  "FR-683: opt-pass-loop-rotate skips loops with multiple latch/back-edge blocks."
  (let* ((insts (list (make-vm-label :name "loop")
                      (make-vm-lt :dst :cond :lhs :i :rhs :limit)
                      (make-vm-jump-zero :reg :cond :label "exit")
                      (make-vm-label :name "a")
                      (make-vm-jump :label "loop")
                      (make-vm-label :name "b")
                      (make-vm-jump :label "loop")
                      (make-vm-label :name "exit")
                      (make-vm-ret :reg :i)))
         (out (cl-cc/optimize::opt-pass-loop-rotate insts)))
    (assert-equal (mapcar #'cl-cc/vm::instruction->sexp insts)
                  (mapcar #'cl-cc/vm::instruction->sexp out))))
