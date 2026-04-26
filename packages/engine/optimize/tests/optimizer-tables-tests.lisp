;;;; tests/unit/optimize/optimizer-tables-tests.lisp
;;;; Coverage tests for src/optimize/optimizer-tables.lisp
;;;;
;;;; Covers the data tables:
;;;;   *opt-binary-fold-table*, *opt-binary-cmp-fold-table*,
;;;;   *opt-unary-fold-table*, *opt-type-pred-fold-table*,
;;;;   *opt-foldable-binary-types*, *opt-foldable-unary-types*,
;;;;   *opt-binary-zero-guard-types*, *opt-binary-no-fold-types*,
;;;;   *opt-commutative-inst-types*, *opt-binary-lhs-rhs-types*,
;;;;   *opt-unary-src-types*, *opt-read-regs-table*
;;;; and the dispatchers:
;;;;   opt-inst-dst, opt-inst-read-regs.
;;;;
;;;; Note: opt-falsep, opt-register-keyword-p, opt-binary-lhs-rhs-p,
;;;; opt-unary-src-p, opt-foldable-unary-arith-p, opt-foldable-type-pred-p
;;;; are already covered in optimizer-tests.lisp.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── *opt-binary-fold-table* ─────────────────────────────────────────────

(deftest-each binary-fold-table-arithmetic-ops-registered
  "*opt-binary-fold-table* has all core arithmetic instruction types registered."
  :cases (("vm-add"        'vm-add)
          ("vm-integer-add" 'vm-integer-add)
          ("vm-float-add"  'vm-float-add)
          ("vm-sub"        'vm-sub)
          ("vm-mul"        'vm-mul)
          ("vm-mod"        'vm-mod)
          ("vm-min"        'vm-min)
          ("vm-max"        'vm-max)
          ("vm-logand"     'vm-logand)
          ("vm-logior"     'vm-logior)
          ("vm-logxor"     'vm-logxor)
          ("vm-ash"        'vm-ash)
          ("vm-gcd"        'vm-gcd)
          ("vm-lcm"        'vm-lcm))
  (type-sym)
  (assert-true (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*)))

(deftest-each binary-fold-table-function-evaluation-cases
  "Functions in *opt-binary-fold-table* correctly evaluate arithmetic operations."
  :cases (("add" 'vm-add 3 4  7)
          ("sub" 'vm-sub 9 4  5)
          ("mul" 'vm-mul 6 7 42))
  (type-sym a b expected)
  (let ((fn (gethash type-sym cl-cc/optimize::*opt-binary-fold-table*)))
    (assert-= expected (funcall fn a b))))

;;; ─── *opt-binary-cmp-fold-table* ─────────────────────────────────────────

(deftest-each binary-cmp-fold-table-ops-registered
  "*opt-binary-cmp-fold-table* has all comparison instruction types registered."
  :cases (("vm-lt"     'vm-lt)
          ("vm-gt"     'vm-gt)
          ("vm-le"     'vm-le)
          ("vm-ge"     'vm-ge)
          ("vm-num-eq" 'vm-num-eq))
  (type-sym)
  (assert-true (gethash type-sym cl-cc/optimize::*opt-binary-cmp-fold-table*)))

(deftest binary-cmp-fold-table-lt-function-evaluates-correctly
  "The vm-lt entry in *opt-binary-cmp-fold-table* correctly compares."
  (let ((fn (gethash 'vm-lt cl-cc/optimize::*opt-binary-cmp-fold-table*)))
    (assert-true (funcall fn 3 5))
    (assert-false (funcall fn 5 3))))

;;; ─── *opt-unary-fold-table* ──────────────────────────────────────────────

(deftest-each unary-fold-table-ops-registered
  "*opt-unary-fold-table* has all unary instruction types registered."
  :cases (("vm-neg"         'vm-neg)
          ("vm-abs"         'vm-abs)
          ("vm-inc"         'vm-inc)
          ("vm-dec"         'vm-dec)
          ("vm-lognot"      'vm-lognot)
          ("vm-rational"    'vm-rational)
          ("vm-numerator"   'vm-numerator)
          ("vm-denominator" 'vm-denominator)
          ("vm-not"         'vm-not))
  (type-sym)
  (assert-true (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*)))

(deftest-each unary-fold-table-function-evaluation-cases
  "Functions in *opt-unary-fold-table* correctly evaluate unary operations."
  :cases (("neg" 'vm-neg  7  -7)
          ("abs" 'vm-abs -5   5))
  (type-sym input expected)
  (let ((fn (gethash type-sym cl-cc/optimize::*opt-unary-fold-table*)))
    (assert-= expected (funcall fn input))))

(deftest unary-fold-table-not-nil-returns-t
  "The vm-not entry in *opt-unary-fold-table* converts nil→t and t→nil."
  (let ((fn (gethash 'vm-not cl-cc/optimize::*opt-unary-fold-table*)))
    (assert-true (funcall fn nil))
    (assert-null (funcall fn t))))

;;; ─── *opt-type-pred-fold-table* ──────────────────────────────────────────

(deftest-each type-pred-fold-table-ops-registered
  "*opt-type-pred-fold-table* has all type-predicate instruction types registered."
  :cases (("vm-null-p"     'vm-null-p)
          ("vm-cons-p"     'vm-cons-p)
          ("vm-symbol-p"   'vm-symbol-p)
          ("vm-number-p"   'vm-number-p)
          ("vm-integer-p"  'vm-integer-p)
          ("vm-function-p" 'vm-function-p))
  (type-sym)
  (assert-true (gethash type-sym cl-cc/optimize::*opt-type-pred-fold-table*)))

(deftest type-pred-fold-table-null-p-evaluates-correctly
  "The vm-null-p entry correctly identifies nil."
  (let ((fn (gethash 'vm-null-p cl-cc/optimize::*opt-type-pred-fold-table*)))
    (assert-true (funcall fn nil))
    (assert-false (funcall fn 42))))

(deftest type-pred-fold-table-function-p-always-false-for-constants
  "The vm-function-p entry always returns nil for constants (functions not known at compile time)."
  (let ((fn (gethash 'vm-function-p cl-cc/optimize::*opt-type-pred-fold-table*)))
    (assert-null (funcall fn 42))
    (assert-null (funcall fn nil))))

;;; ─── Derived type lists ───────────────────────────────────────────────────

(deftest foldable-binary-types-contains-arithmetic-types
  "*opt-foldable-binary-types* includes both arithmetic and comparison types."
  (assert-true (member 'vm-add cl-cc/optimize::*opt-foldable-binary-types*))
  (assert-true (member 'vm-lt  cl-cc/optimize::*opt-foldable-binary-types*))
  (assert-true (member 'vm-mul cl-cc/optimize::*opt-foldable-binary-types*)))

(deftest foldable-unary-types-contains-expected-types
  "*opt-foldable-unary-types* includes both arithmetic and predicate types."
  (assert-true (member 'vm-neg    cl-cc/optimize::*opt-foldable-unary-types*))
  (assert-true (member 'vm-null-p cl-cc/optimize::*opt-foldable-unary-types*))
  (assert-true (member 'vm-abs    cl-cc/optimize::*opt-foldable-unary-types*)))

(deftest binary-zero-guard-types-contains-division-ops
  "*opt-binary-zero-guard-types* contains division-family instruction types."
  (assert-true (member 'vm-div cl-cc/optimize::*opt-binary-zero-guard-types*))
  (assert-true (member 'vm-mod cl-cc/optimize::*opt-binary-zero-guard-types*))
  (assert-true (member 'vm-rem cl-cc/optimize::*opt-binary-zero-guard-types*)))

(deftest binary-no-fold-types-contains-floor-ceiling
  "*opt-binary-no-fold-types* contains floor/ceiling/truncate/round instruction types."
  (assert-true (member 'vm-floor-inst   cl-cc/optimize::*opt-binary-no-fold-types*))
  (assert-true (member 'vm-ceiling-inst cl-cc/optimize::*opt-binary-no-fold-types*))
  (assert-true (member 'vm-truncate     cl-cc/optimize::*opt-binary-no-fold-types*))
  (assert-true (member 'vm-round-inst   cl-cc/optimize::*opt-binary-no-fold-types*)))

(deftest commutative-inst-types-contains-add-and-mul
  "*opt-commutative-inst-types* includes vm-add, vm-mul, vm-logand, vm-num-eq."
  (assert-true (member 'vm-add    cl-cc/optimize::*opt-commutative-inst-types*))
  (assert-true (member 'vm-mul    cl-cc/optimize::*opt-commutative-inst-types*))
  (assert-true (member 'vm-logand cl-cc/optimize::*opt-commutative-inst-types*))
  (assert-true (member 'vm-num-eq cl-cc/optimize::*opt-commutative-inst-types*)))

(deftest commutative-inst-types-excludes-sub-and-div
  "*opt-commutative-inst-types* does NOT include vm-sub (subtraction is not commutative)."
  (assert-false (member 'vm-sub cl-cc/optimize::*opt-commutative-inst-types*)))

;;; ─── opt-inst-dst ─────────────────────────────────────────────────────────

(deftest opt-inst-dst-cases
  "opt-inst-dst returns :dst register when present; nil for instructions without one."
  (assert-eq :r3 (cl-cc/optimize::opt-inst-dst (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)))
  (assert-null (cl-cc/optimize::opt-inst-dst (make-vm-jump :label "end"))))

;;; ─── opt-inst-read-regs ───────────────────────────────────────────────────

(deftest-each opt-inst-read-regs-single-src-cases
  "opt-inst-read-regs returns nil for vm-const; (:r0) for vm-move and vm-neg."
  :cases (("vm-const" (make-vm-const :dst :r0 :value 42) nil)
          ("vm-move"  (make-vm-move  :dst :r1 :src :r0)  '(:r0))
          ("vm-neg"   (make-vm-neg   :dst :r1 :src :r0)  '(:r0)))
  (inst expected)
  (assert-equal expected (cl-cc/optimize::opt-inst-read-regs inst)))

(deftest-each opt-inst-read-regs-lhs-rhs-cases
  "opt-inst-read-regs returns (lhs rhs) for both vm-binop and non-binop binary instructions."
  :cases (("vm-add" (make-vm-add :dst :r2 :lhs :r0 :rhs :r1))
          ("vm-lt"  (make-vm-lt  :dst :r2 :lhs :r0 :rhs :r1)))
  (inst)
  (let ((regs (cl-cc/optimize::opt-inst-read-regs inst)))
    (assert-true (member :r0 regs))
    (assert-true (member :r1 regs))))

;;; ─── %opt-branch-target-labels ───────────────────────────────────────────

(deftest-each branch-target-labels-cases
  "%opt-branch-target-labels collects labels from vm-jump and vm-jump-zero; ignores others."
  :cases (("jump-only"       (list (make-vm-jump      :label "a"))          '("a"))
          ("jump-zero-only"  (list (make-vm-jump-zero :reg :r0 :label "b")) '("b"))
          ("both-kinds"      (list (make-vm-jump      :label "a")
                                   (make-vm-jump-zero :reg :r0 :label "b")) '("a" "b"))
          ("no-jumps"        (list (make-vm-const :dst :r0 :value 1)
                                   (make-vm-ret   :reg :r0))                '()))
  (insts expected-labels)
  (let ((targets (cl-cc/optimize::%opt-branch-target-labels insts)))
    (assert-= (length expected-labels) (hash-table-count targets))
    (dolist (lbl expected-labels)
      (assert-true (gethash lbl targets)))))

;;; ─── %fold-vm-jump-zero ──────────────────────────────────────────────────

(deftest fold-vm-jump-zero-known-false-becomes-unconditional-jump
  "%fold-vm-jump-zero: known-false condition → unconditional vm-jump."
  (let ((env (make-hash-table :test #'eq))
        (emitted nil))
    (setf (gethash :r0 env) nil) ; nil is the canonical false value
    (cl-cc/optimize::%fold-vm-jump-zero
     (make-vm-jump-zero :reg :r0 :label "target")
     env
     (lambda (i) (push i emitted)))
    (assert-= 1 (length emitted))
    (assert-true (typep (first emitted) 'cl-cc/vm::vm-jump))
    (assert-equal "target" (cl-cc/vm::vm-label-name (first emitted)))))

(deftest fold-vm-jump-zero-known-true-eliminates-branch
  "%fold-vm-jump-zero: known-truthy condition → branch is never taken → emits nothing."
  (let ((env (make-hash-table :test #'eq))
        (emitted nil))
    (setf (gethash :r0 env) 1) ; truthy value
    (cl-cc/optimize::%fold-vm-jump-zero
     (make-vm-jump-zero :reg :r0 :label "target")
     env
     (lambda (i) (push i emitted)))
    (assert-= 0 (length emitted))))

(deftest fold-vm-jump-zero-unknown-condition-emits-unchanged
  "%fold-vm-jump-zero: unknown condition → emits the original instruction unchanged."
  (let ((env (make-hash-table :test #'eq))
        (emitted nil))
    (cl-cc/optimize::%fold-vm-jump-zero
     (make-vm-jump-zero :reg :r0 :label "target")
     env
     (lambda (i) (push i emitted)))
    (assert-= 1 (length emitted))
    (assert-true (typep (first emitted) 'cl-cc/vm::vm-jump-zero))))

;;; ─── %opt-known-constant-p ───────────────────────────────────────────────

(deftest-each opt-known-constant-p-cases
  "%opt-known-constant-p: numbers are known, :unknown sentinel and symbols are not."
  :cases (("integer"  42       t)
          ("zero"     0        t)
          ("float"    1.5      t)
          ("unknown"  :unknown nil)
          ("symbol"   :r0      nil)
          ("nil"      nil      nil))
  (val expected)
  (if expected
      (assert-true  (cl-cc/optimize::%opt-known-constant-p val))
      (assert-false (cl-cc/optimize::%opt-known-constant-p val))))

;;; ─── %opt-apply-algebraic-action ─────────────────────────────────────────

(deftest opt-apply-algebraic-action-move-lhs
  "%opt-apply-algebraic-action :move-lhs produces vm-move from lhs register."
  (let ((result (cl-cc/optimize::%opt-apply-algebraic-action :move-lhs :r2 :r0 :r1)))
    (assert-true (typep result 'cl-cc/vm::vm-move))
    (assert-eq :r2 (vm-dst result))
    (assert-eq :r0 (vm-src result))))

(deftest opt-apply-algebraic-action-move-rhs
  "%opt-apply-algebraic-action :move-rhs produces vm-move from rhs register."
  (let ((result (cl-cc/optimize::%opt-apply-algebraic-action :move-rhs :r2 :r0 :r1)))
    (assert-true (typep result 'cl-cc/vm::vm-move))
    (assert-eq :r2 (vm-dst result))
    (assert-eq :r1 (vm-src result))))

(deftest opt-apply-algebraic-action-const
  "%opt-apply-algebraic-action (:const V) produces vm-const with value V."
  (let ((result (cl-cc/optimize::%opt-apply-algebraic-action '(:const 0) :r2 :r0 :r1)))
    (assert-true (typep result 'cl-cc/vm::vm-const))
    (assert-eq :r2 (vm-dst result))
    (assert-= 0 (vm-value result))))

(deftest opt-apply-algebraic-action-neg-lhs
  "%opt-apply-algebraic-action (:neg :lhs) produces vm-neg on lhs register."
  (let ((result (cl-cc/optimize::%opt-apply-algebraic-action '(:neg :lhs) :r2 :r0 :r1)))
    (assert-true (typep result 'cl-cc/vm::vm-neg))
    (assert-eq :r2 (vm-dst result))
    (assert-eq :r0 (vm-src result))))

(deftest opt-apply-algebraic-action-unknown-returns-nil
  "%opt-apply-algebraic-action returns NIL for unrecognized actions."
  (assert-null (cl-cc/optimize::%opt-apply-algebraic-action :unknown-action :r2 :r0 :r1)))
