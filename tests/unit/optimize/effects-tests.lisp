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
  "I/O and call instructions are not pure."
  :cases (("print" (make-vm-print :reg :r0))
          ("call"  (make-vm-call  :dst :r0 :func :r1 :args nil)))
  (inst)
  (assert-false (cl-cc::opt-inst-pure-p inst)))

;;; ─── opt-inst-dce-eligible-p ─────────────────────────────────────────────

(deftest-each dce-eligible-simple
  "Pure and alloc instructions are DCE-eligible."
  :cases (("pure-add"   (make-vm-add  :dst :r0 :lhs :r1 :rhs :r2))
          ("alloc-cons" (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2)))
  (inst)
  (assert-true (cl-cc::opt-inst-dce-eligible-p inst)))

(deftest-each dce-not-eligible-simple
  "I/O and global-write instructions are NOT DCE-eligible."
  :cases (("io-print"       (make-vm-print      :reg :r0))
          ("set-global"     (make-vm-set-global :src :r0 :name 'x)))
  (inst)
  (assert-false (cl-cc::opt-inst-dce-eligible-p inst)))

;;; ─── opt-inst-cse-eligible-p ─────────────────────────────────────────────

(deftest cse-eligibility
  "Pure instructions are CSE-eligible; allocation instructions are not."
  (assert-true  (cl-cc::opt-inst-cse-eligible-p
                  (make-vm-add  :dst :r0 :lhs :r1 :rhs :r2)))
  (assert-false (cl-cc::opt-inst-cse-eligible-p
                  (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))))

;;; ─── Effect Kind: IO / Alloc / Read-Only / Write-Global ─────────────────

(deftest-each effect-kind-io
  "I/O instructions are classified as :io."
  :cases (("vm-print"  (make-vm-print      :reg :r0))
          ("vm-format" (make-vm-format-inst :dst :r0 :fmt "" :arg-regs nil)))
  (inst)
  (assert-eq :io (cl-cc::vm-inst-effect-kind inst)))

(deftest-each effect-kind-alloc
  "Allocation instructions are classified as :alloc."
  :cases (("vm-cons"        (make-vm-cons        :dst :r0 :car-src :r1 :cdr-src :r2))
          ("vm-make-string" (make-vm-make-string  :dst :r0 :src :r1 :char nil)))
  (inst)
  (assert-eq :alloc (cl-cc::vm-inst-effect-kind inst)))

(deftest-each effect-kind-read-only
  "Read-only heap/global instructions are classified as :read-only."
  :cases (("vm-car"        (make-vm-car        :dst :r0 :src :r1))
          ("vm-cdr"        (make-vm-cdr        :dst :r0 :src :r1))
          ("vm-get-global" (make-vm-get-global  :dst :r0 :name 'x)))
  (inst)
  (assert-eq :read-only (cl-cc::vm-inst-effect-kind inst)))

(deftest-each effect-kind-write-global
  "Global/heap mutation instructions are classified as :write-global."
  :cases (("vm-set-global" (make-vm-set-global :src :r0 :name 'x))
          ("vm-rplaca"     (make-vm-rplaca     :cons :r0 :val :r1))
          ("vm-rplacd"     (make-vm-rplacd     :cons :r0 :val :r1)))
  (inst)
  (assert-eq :write-global (cl-cc::vm-inst-effect-kind inst)))

(deftest-each effect-kind-bitwise-pure
  "Bitwise / boolean unary instructions are classified as :pure."
  :cases (("vm-not"    (make-vm-not    :dst :r0 :src :r1))
          ("vm-lognot" (make-vm-lognot :dst :r0 :src :r1)))
  (inst)
  (assert-eq :pure (cl-cc::vm-inst-effect-kind inst)))

;;; ─── CSE / DCE Properties of New Kinds ──────────────────────────────────

(deftest-each dce-eligible-kinds
  "Pure and alloc instructions are DCE-eligible."
  :cases (("pure-add"   (make-vm-add  :dst :r0 :lhs :r1 :rhs :r2))
          ("alloc-cons" (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2)))
  (inst)
  (assert-true (cl-cc::opt-inst-dce-eligible-p inst)))

(deftest-each dce-not-eligible-kinds
  "IO, write-global, and read-only instructions are NOT DCE-eligible."
  :cases (("io-print"        (make-vm-print      :reg :r0))
          ("write-set-global" (make-vm-set-global :src :r0 :name 'x))
          ("read-get-global"  (make-vm-get-global :dst :r0 :name 'x)))
  (inst)
  (assert-false (cl-cc::opt-inst-dce-eligible-p inst)))

(deftest-each cse-not-eligible-non-pure
  "Non-pure instructions are not CSE-eligible."
  :cases (("alloc-cons"      (make-vm-cons       :dst :r0 :car-src :r1 :cdr-src :r2))
          ("io-print"        (make-vm-print       :reg :r0))
          ("read-get-global" (make-vm-get-global  :dst :r0 :name 'x)))
  (inst)
  (assert-false (cl-cc::opt-inst-cse-eligible-p inst)))

;;; ─── DCE Extended Coverage ───────────────────────────────────────────────

(deftest dce-add-elimination
  "DCE eliminates dead vm-add (unused result) but preserves used vm-add."
  (let* ((dead-insts (list (make-vm-const :dst :r0 :value 1)
                           (make-vm-const :dst :r1 :value 2)
                           (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1) ; dead: r2 unused
                           (make-vm-ret   :reg :r0)))
         (used-insts (list (make-vm-const :dst :r0 :value 1)
                           (make-vm-const :dst :r1 :value 2)
                           (make-vm-add   :dst :r2 :lhs :r0 :rhs :r1)
                           (make-vm-ret   :reg :r2)))) ; r2 is used here
    (assert-false (some (lambda (i) (typep i 'cl-cc::vm-add))
                        (cl-cc::opt-pass-dce dead-insts)))
    (assert-true  (some (lambda (i) (typep i 'cl-cc::vm-add))
                        (cl-cc::opt-pass-dce used-insts)))))

;;; ─── effect-row->effect-kind bridge ─────────────────────────────────────

(defun make-effect-row (&rest effect-names)
  "Helper: build a type-effect-row with the given effect name symbols."
  (cl-cc/type:make-type-effect-row
   :effects (mapcar (lambda (n) (cl-cc/type:make-type-effect :name n)) effect-names)
   :row-var nil))

(deftest-each effect-row->kind
  "effect-row->effect-kind maps type system rows to optimizer effect kinds."
  :cases (("pure"         (make-effect-row)         :pure)
          ("io"           (make-effect-row 'io)      :io)
          ("state"        (make-effect-row 'state)   :write-global)
          ("error"        (make-effect-row 'error)   :control)
          ("unknown-tag"  (make-effect-row 'network) :unknown))
  (row expected)
  (assert-eq expected (cl-cc::effect-row->effect-kind row)))

(deftest effect-kind-vm-label-is-control
  "vm-label instruction is classified as :control by the typecase fast-path."
  (assert-eq :control
             (cl-cc::vm-inst-effect-kind (make-vm-label :name "L0"))))
