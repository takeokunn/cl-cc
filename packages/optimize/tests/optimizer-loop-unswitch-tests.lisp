;;;; Unit tests for FR-602 loop unswitching.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(defun %fr602-unswitch-loop (&key (condition-side-effect-p nil))
  "Build a counted loop containing an invariant if/else in its body."
  (append (list (make-vm-const :dst :i :value 0)
                (make-vm-const :dst :limit :value 8)
                (make-vm-const :dst :one :value 1))
          (unless condition-side-effect-p
            (list (make-vm-const :dst :flag :value 1)))
          (list (make-vm-label :name "loop")
                (make-vm-lt :dst :cond :lhs :i :rhs :limit)
                (make-vm-jump-zero :reg :cond :label "exit"))
          (when condition-side-effect-p
            (list (make-vm-call :dst :flag :func :predicate :args (list :i))))
          (list (make-vm-jump-zero :reg :flag :label "else")
                (make-vm-print :reg :i)
                (make-vm-jump :label "join")
                (make-vm-label :name "else")
                (make-vm-print :reg :limit)
                (make-vm-label :name "join")
                (make-vm-add :dst :i :lhs :i :rhs :one)
                (make-vm-jump :label "loop")
                (make-vm-label :name "exit")
                (make-vm-ret :reg :i))))

(deftest loop-unswitch-fr602-hoists-invariant-condition
  "FR-602: invariant body branch is hoisted and two specialized loops are emitted."
  (let* ((out (cl-cc/optimize::opt-pass-loop-unswitch (%fr602-unswitch-loop))))
    (assert-true (%test-label-position out "loop_unsw_true"))
    (assert-true (%test-label-position out "loop_unsw_false"))
    (assert-true (%test-label-position out "exit"))
    (assert-false (%test-label-position out "loop"))
    (assert-true (some (lambda (inst)
                         (and (typep inst 'cl-cc/vm::vm-jump-zero)
                              (eq (cl-cc/vm::vm-reg inst) :flag)
                              (equal (cl-cc/vm::vm-label-name inst) "loop_unsw_false")))
                       out))
    (assert-false (some (lambda (inst)
                          (and (typep inst 'cl-cc/vm::vm-jump-zero)
                               (equal (cl-cc/vm::vm-label-name inst) "else")))
                        out))))

(deftest loop-unswitch-fr602-skips-side-effecting-condition
  "FR-602: side-effecting loop-local condition computations are not unswitched."
  (let* ((insts (%fr602-unswitch-loop :condition-side-effect-p t))
         (out (cl-cc/optimize::opt-pass-loop-unswitch insts)))
    (assert-equal (mapcar #'cl-cc/vm::instruction->sexp insts)
                  (mapcar #'cl-cc/vm::instruction->sexp out))))
