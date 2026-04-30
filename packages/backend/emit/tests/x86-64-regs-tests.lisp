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

(deftest x86-64-regs-red-zone-spill-leaf-within-limit-returns-true
  "x86-64-red-zone-spill-p: leaf=T with count≤16 returns T."
  (assert-true (cl-cc/emit::x86-64-red-zone-spill-p t 1))
  (assert-true (cl-cc/emit::x86-64-red-zone-spill-p t 16)))

(deftest x86-64-regs-red-zone-spill-non-leaf-always-false
  "x86-64-red-zone-spill-p: leaf=NIL always returns NIL regardless of count."
  (assert-false (cl-cc/emit::x86-64-red-zone-spill-p nil 1))
  (assert-false (cl-cc/emit::x86-64-red-zone-spill-p nil 16)))

(deftest x86-64-regs-red-zone-spill-leaf-exceeds-limit-returns-false
  "x86-64-red-zone-spill-p: leaf=T with count>16 returns NIL."
  (assert-false (cl-cc/emit::x86-64-red-zone-spill-p t 17))
  (assert-false (cl-cc/emit::x86-64-red-zone-spill-p t 100)))

(deftest x86-64-regs-red-zone-spill-zero-count-returns-false
  "x86-64-red-zone-spill-p: leaf=T with count=0 returns NIL."
  (assert-false (cl-cc/emit::x86-64-red-zone-spill-p t 0)))

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
  (let ((cl-cc/emit::*current-regalloc* nil)
        (cl-cc/emit::*phys-reg-to-x86-code* nil))
    (assert-= expected-code (cl-cc/emit::vm-reg-to-x86 vreg))))

(deftest x86-64-regs-vm-reg-to-x86-out-of-range-errors
  "vm-reg-to-x86 signals error for an unmapped VM register."
  (let ((cl-cc/emit::*current-regalloc* nil)
        (cl-cc/emit::*phys-reg-to-x86-code* nil))
    (assert-signals error (cl-cc/emit::vm-reg-to-x86 :R99))))

;;; ─── vm-reg-to-xmm (no regalloc) ────────────────────────────────────────

(deftest-each x86-64-regs-vm-reg-to-xmm-naive-map
  "vm-reg-to-xmm uses naive *vm-fp-reg-map* when *current-regalloc* is nil."
  :cases (("xmm0" :R0 0)
          ("xmm1" :R1 1)
          ("xmm2" :R2 2)
          ("xmm7" :R7 7))
  (vreg expected-code)
  (let ((cl-cc/emit::*current-regalloc* nil)
        (cl-cc/emit::*phys-fp-reg-to-x86-code*
         '((:xmm0 . 0) (:xmm1 . 1) (:xmm2 . 2) (:xmm3 . 3)
           (:xmm4 . 4) (:xmm5 . 5) (:xmm6 . 6) (:xmm7 . 7))))
    (assert-= expected-code (cl-cc/emit::vm-reg-to-xmm vreg))))

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
  (assert-= expected (cl-cc/emit::vm-const-to-integer val)))

;;; ─── x86-64-double-float-bits ────────────────────────────────────────────

(deftest x86-64-regs-double-float-bits-valid-64-bit-range
  "x86-64-double-float-bits: result for 1.0 is a non-negative 64-bit integer."
  (let ((bits (cl-cc/emit::x86-64-double-float-bits 1.0)))
    (assert-true (integerp bits))
    (assert-true (>= bits 0))
    (assert-true (< bits (expt 2 64)))))

(deftest x86-64-regs-double-float-bits-ieee754-values
  "x86-64-double-float-bits: 0.0→0; 1.0→IEEE 754 representation #x3FF0000000000000."
  (assert-= 0 (cl-cc/emit::x86-64-double-float-bits 0.0))
  (assert-= #x3FF0000000000000 (cl-cc/emit::x86-64-double-float-bits 1.0)))

;;; ─── x86-64-compute-float-vregs ──────────────────────────────────────────

(deftest x86-64-regs-compute-float-vregs-empty-returns-empty-table
  "x86-64-compute-float-vregs on nil returns an empty hash table."
  (let ((result (cl-cc/emit::x86-64-compute-float-vregs nil)))
    (assert-true (hash-table-p result))
    (assert-= 0 (hash-table-count result))))

(deftest x86-64-regs-compute-float-vregs-float-const-marks-register
  "x86-64-compute-float-vregs marks a register as float when loaded from a float constant."
  (let* ((insts (list (cl-cc::make-vm-const :dst :R0 :value 3.14)))
         (result (cl-cc/emit::x86-64-compute-float-vregs insts)))
    (assert-true (gethash :R0 result))))

(deftest x86-64-regs-compute-float-vregs-int-const-does-not-mark
  "x86-64-compute-float-vregs does not mark a register loaded from an integer constant."
  (let* ((insts (list (cl-cc::make-vm-const :dst :R0 :value 42)))
         (result (cl-cc/emit::x86-64-compute-float-vregs insts)))
    (assert-false (gethash :R0 result))))

(deftest x86-64-regs-compute-float-vregs-propagates-via-move
  "x86-64-compute-float-vregs propagates float type through vm-move."
  (let* ((insts (list (cl-cc::make-vm-const :dst :R0 :value 1.0)
                      (cl-cc::make-vm-move :dst :R1 :src :R0)))
         (result (cl-cc/emit::x86-64-compute-float-vregs insts)))
    (assert-true (gethash :R0 result))
    (assert-true (gethash :R1 result))))

;;; ─── *vm-reg-map* and *phys-reg-to-x86-code* data checks ─────────────────

(deftest x86-64-regs-vm-reg-map-covers-eight-vregs
  "*vm-reg-map* covers exactly 8 virtual registers."
  (assert-= 8 (length cl-cc/emit::*vm-reg-map*)))

(deftest x86-64-regs-phys-reg-to-x86-code-covers-fourteen-registers
  "*phys-reg-to-x86-code* covers exactly 14 physical registers."
  (assert-= 14 (length cl-cc/emit::*phys-reg-to-x86-code*)))
