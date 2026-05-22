;;;; Unit tests for FR-681 strength-reduction passes.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest fr-681-div-by-const-skips-power-of-two
  "FR-681: opt-pass-div-by-const does not handle powers of two; shift pass owns that case."
  (let* ((c (make-vm-const :dst :d :value 8))
         (div (make-vm-div :dst :q :lhs :x :rhs :d))
         (out (cl-cc/optimize::opt-pass-div-by-const (list c div))))
    (assert-true (member div out))
    (assert-false (some (lambda (inst) (typep inst 'cl-cc/vm::vm-ash)) out))))

(deftest fr-681-div-by-const-lowers-bounded-non-power-divisor
  "FR-681: bounded integer division by non-power-of-two constant lowers to multiply+shift."
  (let* ((c4 (make-vm-const :dst :a :value 4))
         (c8 (make-vm-const :dst :b :value 8))
         (sum (make-vm-add :dst :x :lhs :a :rhs :b))
         (c3 (make-vm-const :dst :d :value 3))
         (div (make-vm-div :dst :q :lhs :x :rhs :d))
         (out (cl-cc/optimize::opt-pass-div-by-const (list c4 c8 sum c3 div))))
    (assert-false (some (lambda (inst) (typep inst 'cl-cc/vm::vm-div)) out))
    (assert-true (some (lambda (inst) (typep inst 'cl-cc/vm::vm-mul)) out))
    (assert-true (some (lambda (inst) (typep inst 'cl-cc/vm::vm-ash)) out))))

(deftest fr-681-iv-strength-reduce-replaces-loop-mul-with-derived-iv
  "FR-681: loop-body i*stride is replaced by a derived additive induction value."
  (let* ((mul (make-vm-mul :dst :prod :lhs :i :rhs :scale))
         (insts (list (make-vm-const :dst :i :value 0)
                      (make-vm-const :dst :limit :value 8)
                      (make-vm-const :dst :one :value 1)
                      (make-vm-const :dst :scale :value 4)
                      (make-vm-label :name "loop")
                      (make-vm-lt :dst :cond :lhs :i :rhs :limit)
                      (make-vm-jump-zero :reg :cond :label "exit")
                      mul
                      (make-vm-add :dst :sum :lhs :sum :rhs :prod)
                      (make-vm-add :dst :i :lhs :i :rhs :one)
                      (make-vm-jump :label "loop")
                      (make-vm-label :name "exit")
                      (make-vm-ret :reg :sum)))
         (out (cl-cc/optimize::opt-pass-iv-strength-reduce insts)))
    (assert-false (member mul out))
    (assert-true (some (lambda (inst)
                         (and (typep inst 'cl-cc/vm::vm-move)
                              (eq (cl-cc/vm::vm-dst inst) :prod)))
                       out))
    (assert-true (some (lambda (inst)
                         (and (typep inst 'cl-cc/vm::vm-add)
                              (not (eq (cl-cc/vm::vm-dst inst) :i))))
                       out))))
