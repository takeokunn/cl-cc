;;;; tests/unit/emit/x86-64-regs-tests.lisp
;;;; Coverage for src/emit/x86-64-regs.lisp:
;;;;   x86-64-red-zone-spill-p, vm-reg-to-x86, vm-reg-to-xmm,
;;;;   x86-64-compute-float-vregs, vm-const-to-integer,
;;;;   x86-64-double-float-bits, *vm-reg-map*, *phys-reg-to-x86-code*.

(in-package :cl-cc/test)

(defsuite x86-64-regs-suite
  :description "Register map and translation tests for x86-64-regs.lisp"
  :parent cl-cc-unit-suite)

(in-suite x86-64-regs-suite)

;;; ─── x86-64-red-zone-spill-p ─────────────────────────────────────────────

(deftest x86-64-regs-red-zone-spill-leaf-small
  "x86-64-red-zone-spill-p: leaf function with ≤16 spill slots → T."
  (assert-true (cl-cc::x86-64-red-zone-spill-p t 1))
  (assert-true (cl-cc::x86-64-red-zone-spill-p t 16)))

(deftest x86-64-regs-red-zone-spill-non-leaf
  "x86-64-red-zone-spill-p: non-leaf function → NIL regardless of count."
  (assert-false (cl-cc::x86-64-red-zone-spill-p nil 1))
  (assert-false (cl-cc::x86-64-red-zone-spill-p nil 16)))

(deftest x86-64-regs-red-zone-spill-too-many
  "x86-64-red-zone-spill-p: leaf with >16 spill slots → NIL."
  (assert-false (cl-cc::x86-64-red-zone-spill-p t 17))
  (assert-false (cl-cc::x86-64-red-zone-spill-p t 100)))

(deftest x86-64-regs-red-zone-spill-zero
  "x86-64-red-zone-spill-p: zero spill count → NIL (no spills needed)."
  (assert-false (cl-cc::x86-64-red-zone-spill-p t 0)))

;;; ─── vm-reg-to-x86 (no regalloc) ─────────────────────────────────────────

(deftest-each x86-64-regs-vm-reg-to-x86-naive-map
  "vm-reg-to-x86 uses naive *vm-reg-map* when *current-regalloc* is nil.
Codes 4 and 5 are rsp/rbp (reserved as stack/base pointers) and are skipped,
so R4→rsi=6 and R5→rdi=7 in the naive mapping."
  :cases (("r0" :R0 0)   ; rax = 0
          ("r1" :R1 1)   ; rcx = 1
          ("r2" :R2 2)   ; rdx = 2
          ("r3" :R3 3)   ; rbx = 3
          ("r4" :R4 6)   ; rsi = 6 (skips rsp=4)
          ("r5" :R5 7)   ; rdi = 7 (skips rbp=5)
          ("r6" :R6 8)   ; r8  = 8
          ("r7" :R7 9))  ; r9  = 9
  (vreg expected-code)
  (let ((cl-cc::*current-regalloc* nil)
        (cl-cc::*phys-reg-to-x86-code* nil))
    (assert-= expected-code (cl-cc::vm-reg-to-x86 vreg))))

(deftest x86-64-regs-vm-reg-to-x86-out-of-range-errors
  "vm-reg-to-x86 signals error for an unmapped VM register."
  (let ((cl-cc::*current-regalloc* nil)
        (cl-cc::*phys-reg-to-x86-code* nil))
    (assert-signals error (cl-cc::vm-reg-to-x86 :R99))))

;;; ─── vm-reg-to-xmm (no regalloc) ────────────────────────────────────────

(deftest-each x86-64-regs-vm-reg-to-xmm-naive-map
  "vm-reg-to-xmm uses naive *vm-fp-reg-map* when *current-regalloc* is nil."
  :cases (("xmm0" :R0 0)
          ("xmm1" :R1 1)
          ("xmm2" :R2 2)
          ("xmm7" :R7 7))
  (vreg expected-code)
  (let ((cl-cc::*current-regalloc* nil)
        (cl-cc::*phys-fp-reg-to-x86-code*
         '((:xmm0 . 0) (:xmm1 . 1) (:xmm2 . 2) (:xmm3 . 3)
           (:xmm4 . 4) (:xmm5 . 5) (:xmm6 . 6) (:xmm7 . 7))))
    (assert-= expected-code (cl-cc::vm-reg-to-xmm vreg))))

;;; ─── vm-const-to-integer ─────────────────────────────────────────────────

(deftest-each x86-64-regs-vm-const-to-integer-cases
  "vm-const-to-integer coerces various CL values to integers."
  :cases (("nil"     nil   0)
          ("t"       t     1)
          ("zero"    0     0)
          ("pos"     42    42)
          ("neg"    -7    -7)
          ("other"  3.14  0))
  (val expected)
  (assert-= expected (cl-cc::vm-const-to-integer val)))

;;; ─── x86-64-double-float-bits ────────────────────────────────────────────

(deftest x86-64-regs-double-float-bits-positive
  "x86-64-double-float-bits returns a non-negative 64-bit value for 1.0d0."
  (let ((bits (cl-cc::x86-64-double-float-bits 1.0)))
    (assert-true (integerp bits))
    (assert-true (>= bits 0))
    (assert-true (< bits (expt 2 64)))))

(deftest x86-64-regs-double-float-bits-zero
  "x86-64-double-float-bits returns 0 for 0.0."
  (assert-= 0 (cl-cc::x86-64-double-float-bits 0.0)))

(deftest x86-64-regs-double-float-bits-one
  "x86-64-double-float-bits for 1.0 matches the IEEE 754 bit pattern #x3FF0...0."
  (assert-= #x3FF0000000000000 (cl-cc::x86-64-double-float-bits 1.0)))

;;; ─── x86-64-compute-float-vregs ──────────────────────────────────────────

(deftest x86-64-regs-compute-float-vregs-empty
  "x86-64-compute-float-vregs returns empty hash table for no instructions."
  (let ((result (cl-cc::x86-64-compute-float-vregs nil)))
    (assert-true (hash-table-p result))
    (assert-= 0 (hash-table-count result))))

(deftest x86-64-regs-compute-float-vregs-const-float
  "x86-64-compute-float-vregs marks register holding a float constant."
  (let* ((insts (list (cl-cc::make-vm-const :dst :R0 :value 3.14)))
         (result (cl-cc::x86-64-compute-float-vregs insts)))
    (assert-true (gethash :R0 result))))

(deftest x86-64-regs-compute-float-vregs-const-integer-not-float
  "x86-64-compute-float-vregs does NOT mark register holding an integer constant."
  (let* ((insts (list (cl-cc::make-vm-const :dst :R0 :value 42)))
         (result (cl-cc::x86-64-compute-float-vregs insts)))
    (assert-false (gethash :R0 result))))

(deftest x86-64-regs-compute-float-vregs-propagates-via-move
  "x86-64-compute-float-vregs propagates float status through vm-move."
  (let* ((insts (list (cl-cc::make-vm-const :dst :R0 :value 1.0)
                      (cl-cc::make-vm-move :dst :R1 :src :R0)))
         (result (cl-cc::x86-64-compute-float-vregs insts)))
    (assert-true (gethash :R0 result))
    (assert-true (gethash :R1 result))))

;;; ─── *vm-reg-map* and *phys-reg-to-x86-code* data checks ─────────────────

(deftest x86-64-regs-vm-reg-map-has-8-entries
  "*vm-reg-map* covers exactly 8 virtual registers :R0..:R7."
  (assert-= 8 (length cl-cc::*vm-reg-map*)))

(deftest x86-64-regs-phys-reg-to-x86-code-has-14-entries
  "*phys-reg-to-x86-code* covers 14 physical registers (rax..r15 minus rsp/rbp)."
  (assert-= 14 (length cl-cc::*phys-reg-to-x86-code*)))
