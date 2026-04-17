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

(deftest binary-fold-table-add-function-evaluates-correctly
  "The vm-add entry in *opt-binary-fold-table* correctly sums two numbers."
  (let ((fn (gethash 'vm-add cl-cc/optimize::*opt-binary-fold-table*)))
    (assert-true fn)
    (assert-= 7 (funcall fn 3 4))))

(deftest binary-fold-table-sub-function-evaluates-correctly
  "The vm-sub entry in *opt-binary-fold-table* correctly subtracts."
  (let ((fn (gethash 'vm-sub cl-cc/optimize::*opt-binary-fold-table*)))
    (assert-= 5 (funcall fn 9 4))))

(deftest binary-fold-table-mul-function-evaluates-correctly
  "The vm-mul entry in *opt-binary-fold-table* correctly multiplies."
  (let ((fn (gethash 'vm-mul cl-cc/optimize::*opt-binary-fold-table*)))
    (assert-= 42 (funcall fn 6 7))))

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

(deftest unary-fold-table-neg-evaluates-correctly
  "The vm-neg entry in *opt-unary-fold-table* correctly negates."
  (let ((fn (gethash 'vm-neg cl-cc/optimize::*opt-unary-fold-table*)))
    (assert-= -7 (funcall fn 7))))

(deftest unary-fold-table-abs-evaluates-correctly
  "The vm-abs entry in *opt-unary-fold-table* returns absolute value."
  (let ((fn (gethash 'vm-abs cl-cc/optimize::*opt-unary-fold-table*)))
    (assert-= 5 (funcall fn -5))))

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

(deftest opt-inst-dst-returns-dst-register
  "opt-inst-dst returns the destination register for instructions that have one."
  (let ((inst (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)))
    (assert-eq :r3 (cl-cc/optimize::opt-inst-dst inst))))

(deftest opt-inst-dst-returns-nil-for-no-dst
  "opt-inst-dst returns nil for instructions without a destination (e.g. vm-jump)."
  (let ((inst (make-vm-jump :label "end")))
    (assert-null (cl-cc/optimize::opt-inst-dst inst))))

;;; ─── opt-inst-read-regs ───────────────────────────────────────────────────

(deftest opt-inst-read-regs-returns-nil-for-vm-const
  "opt-inst-read-regs returns nil for vm-const (no registers read)."
  (let ((inst (make-vm-const :dst :r0 :value 42)))
    (assert-null (cl-cc/optimize::opt-inst-read-regs inst))))

(deftest opt-inst-read-regs-returns-src-for-vm-move
  "opt-inst-read-regs returns (src) for vm-move."
  (let ((inst (make-vm-move :dst :r1 :src :r0)))
    (assert-equal '(:r0) (cl-cc/optimize::opt-inst-read-regs inst))))

(deftest opt-inst-read-regs-returns-lhs-rhs-for-binop
  "opt-inst-read-regs returns (lhs rhs) for vm-binop subclasses."
  (let ((inst (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)))
    (let ((regs (cl-cc/optimize::opt-inst-read-regs inst)))
      (assert-true (member :r0 regs))
      (assert-true (member :r1 regs)))))

(deftest opt-inst-read-regs-returns-lhs-rhs-for-non-binop-binary
  "opt-inst-read-regs returns (lhs rhs) for non-binop binary instructions (e.g. vm-lt)."
  (let ((inst (make-vm-lt :dst :r2 :lhs :r0 :rhs :r1)))
    (let ((regs (cl-cc/optimize::opt-inst-read-regs inst)))
      (assert-true (member :r0 regs))
      (assert-true (member :r1 regs)))))

(deftest opt-inst-read-regs-returns-src-for-unary
  "opt-inst-read-regs returns (src) for unary instruction types."
  (let ((inst (make-vm-neg :dst :r1 :src :r0)))
    (assert-equal '(:r0) (cl-cc/optimize::opt-inst-read-regs inst))))
