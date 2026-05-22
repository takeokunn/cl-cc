;;;; optimizer-tail-dup-tests.lisp --- FR-608 tests

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest tail-duplication-duplicates-small-shared-successor
  "FR-608 duplicates a small multi-predecessor successor into jump predecessors."
  (let* ((insts (list (make-vm-const :dst :cond :value 0)
                      (make-vm-jump-zero :reg :cond :label "p2")
                      (make-vm-jump :label "join")
                      (make-vm-label :name "p2")
                      (make-vm-jump :label "join")
                      (make-vm-label :name "join")
                      (make-vm-const :dst :r0 :value 42)
                      (make-vm-ret :reg :r0)))
         (out (cl-cc/optimize:opt-pass-tail-duplication insts)))
    (assert-true (> (count-if (lambda (inst)
                                (and (typep inst 'cl-cc/vm::vm-const)
                                     (eq (cl-cc/vm::vm-dst inst) :r0)))
                              out)
                    1))
    (assert-false (some (lambda (inst)
                          (and (typep inst 'cl-cc/vm::vm-jump)
                               (equal (cl-cc/vm::vm-label-name inst) "join")))
                        out))))

(deftest tail-duplication-does-not-copy-side-effecting-block
  "FR-608 refuses side-effecting shared successors."
  (let* ((insts (list (make-vm-const :dst :cond :value 0)
                      (make-vm-jump-zero :reg :cond :label "p2")
                      (make-vm-jump :label "join")
                      (make-vm-label :name "p2")
                      (make-vm-jump :label "join")
                      (make-vm-label :name "join")
                      (make-vm-print :reg :value)
                      (make-vm-ret :reg :value)))
         (out (cl-cc/optimize:opt-pass-tail-duplication insts)))
    (assert-equal (mapcar #'cl-cc/vm:instruction->sexp insts)
                  (mapcar #'cl-cc/vm:instruction->sexp out))))
