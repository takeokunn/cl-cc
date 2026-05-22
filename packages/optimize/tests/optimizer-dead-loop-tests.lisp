;;;; Unit tests for FR-686 dead loop elimination.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest dead-loop-fr686-removes-pure-unused-loop
  "FR-686: pure loop with unused loop-defined values is removed."
  (let* ((insts (list (make-vm-const :dst :limit :value 10)
                      (make-vm-label :name "loop")
                      (make-vm-lt :dst :cond :lhs :i :rhs :limit)
                      (make-vm-jump-zero :reg :cond :label "exit")
                      (make-vm-add :dst :tmp :lhs :tmp :rhs :one)
                      (make-vm-jump :label "loop")
                      (make-vm-label :name "exit")
                      (make-vm-ret :reg :result)))
         (out (cl-cc/optimize::opt-pass-dead-loop-elimination insts)))
    (assert-false (some (lambda (inst)
                          (and (typep inst 'cl-cc/vm::vm-label)
                               (equal (cl-cc/vm::vm-name inst) "loop")))
                        out))
    (assert-false (some (lambda (inst)
                          (and (typep inst 'cl-cc/vm::vm-add)
                               (eq (cl-cc/vm::vm-dst inst) :tmp)))
                        out))
    (assert-true (some (lambda (inst)
                         (and (typep inst 'cl-cc/vm::vm-label)
                              (equal (cl-cc/vm::vm-name inst) "exit")))
                       out))))

(deftest dead-loop-fr686-preserves-volatile-loop
  "FR-686: loops containing a volatile marker are preserved."
  (let* ((insts (list (make-vm-label :name "loop")
                      (make-vm-lt :dst :cond :lhs :i :rhs :limit)
                      (make-vm-jump-zero :reg :cond :label "exit")
                      (make-vm-const :dst :marker :value :volatile-write)
                      (make-vm-add :dst :tmp :lhs :tmp :rhs :one)
                      (make-vm-jump :label "loop")
                      (make-vm-label :name "exit")
                      (make-vm-ret :reg :result)))
         (out (cl-cc/optimize::opt-pass-dead-loop-elimination insts)))
    (assert-equal (mapcar #'cl-cc/vm::instruction->sexp insts)
                  (mapcar #'cl-cc/vm::instruction->sexp out))))
