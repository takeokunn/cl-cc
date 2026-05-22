(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest fr-685-div-by-const-bounded-dividend-emits-multiply-shift
  "FR-685: non-power-of-two constant division with a proved range lowers to mul+shift."
  (let* ((c4 (make-vm-const :dst :r0 :value 4))
         (c8 (make-vm-const :dst :r1 :value 8))
         (sum (make-vm-add :dst :x :lhs :r0 :rhs :r1))
         (d3 (make-vm-const :dst :d :value 3))
         (div (make-vm-div :dst :q :lhs :x :rhs :d))
         (result (cl-cc/optimize:opt-pass-div-by-const (list c4 c8 sum d3 div))))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result))))

(deftest fr-685-mod-by-const-uses-quotient-times-divisor-subtraction
  "FR-685: mod by a non-power-of-two constant lowers through dividend - divisor*quotient."
  (let* ((c4 (make-vm-const :dst :r0 :value 4))
         (c8 (make-vm-const :dst :r1 :value 8))
         (sum (make-vm-add :dst :x :lhs :r0 :rhs :r1))
         (d3 (make-vm-const :dst :d :value 3))
         (mod (make-vm-mod :dst :m :lhs :x :rhs :d))
         (result (cl-cc/optimize:opt-pass-div-by-const (list c4 c8 sum d3 mod))))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-mod)) result))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-mul)) result))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-sub)) result))))

(deftest fr-685-div-by-zero-not-transformed
  "FR-685: division by compile-time zero remains vm-div."
  (let* ((d0 (make-vm-const :dst :d :value 0))
         (div (make-vm-div :dst :q :lhs :x :rhs :d))
         (result (cl-cc/optimize:opt-pass-div-by-const (list d0 div))))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result))))

(deftest fr-685-power-of-two-left-for-strength-reduce
  "FR-685: power-of-two division is skipped so existing vm-ash lowering handles it."
  (let* ((d8 (make-vm-const :dst :d :value 8))
         (div (make-vm-div :dst :q :lhs :x :rhs :d))
         (result (cl-cc/optimize:opt-pass-div-by-const (list d8 div))))
    (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-div)) result))
    (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-ash)) result))))
