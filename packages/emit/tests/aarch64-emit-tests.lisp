;;;; tests/unit/emit/aarch64-emit-tests.lisp — AArch64 Assembly Emit Tests
;;;;
;;;; Tests for src/emit/aarch64.lisp:
;;;; aarch64-target, target-register, emit-instruction methods

(in-package :cl-cc/test)

(defsuite aarch64-emit-suite :description "AArch64 assembly emit tests"
  :parent cl-cc-unit-suite)


(in-suite aarch64-emit-suite)
;;; ─── Helper ─────────────────────────────────────────────────────────────────

(defun %aarch64-emit (target inst)
  "Emit INST to a string using TARGET and return the result."
  (let ((s (make-string-output-stream)))
    (cl-cc/codegen::emit-instruction target inst s)
    (get-output-stream-string s)))

(defun %make-aarch64-target ()
  (make-instance 'cl-cc/codegen::aarch64-target))

;;; ─── target-register ──────────────────────────────────────────────────────────

(deftest-each aarch64-target-register-pool
  "AArch64 target-register maps :R0..:R7 to x0..x7."
  :cases (("r0" :r0 "x0")
          ("r1" :r1 "x1")
          ("r2" :r2 "x2")
          ("r3" :r3 "x3")
          ("r4" :r4 "x4")
          ("r5" :r5 "x5")
          ("r6" :r6 "x6")
          ("r7" :r7 "x7"))
  (vreg expected)
  (let ((tgt (%make-aarch64-target)))
    (assert-equal expected (cl-cc/codegen::target-register tgt vreg))))

(deftest aarch64-target-register-overflow
  "AArch64 target-register signals error for :R8+ (pool exhausted)."
  (let ((tgt (%make-aarch64-target)))
    (assert-signals error (cl-cc/codegen::target-register tgt :r8))))

;;; ─── emit-instruction methods ──────────────────────────────────────────────────

(deftest aarch64-emit-const
  "vm-const emits mov xN, #value (immediate prefix)."
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-const :dst :r0 :value 42))))
    (assert-true (search "mov" asm))
    (assert-true (search "x0" asm))
    (assert-true (search "#42" asm))))

(deftest aarch64-emit-move
  "vm-move emits mov xD, xS."
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-move :dst :r0 :src :r1))))
    (assert-true (search "mov" asm))
    (assert-true (search "x0" asm))
    (assert-true (search "x1" asm))))

(deftest aarch64-emit-add
  "vm-add emits 3-operand add xD, xL, xR."
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-add :dst :r0 :lhs :r1 :rhs :r2))))
    (assert-true (search "add" asm))
    (assert-true (search "x0" asm))
    (assert-true (search "x1" asm))
    (assert-true (search "x2" asm))))

(deftest-each aarch64-emit-arithmetic-mnemonics
  "vm-sub and vm-mul each emit their expected mnemonic in the output."
  :cases (("sub" (make-vm-sub :dst :r0 :lhs :r1 :rhs :r2) "sub")
          ("mul" (make-vm-mul :dst :r0 :lhs :r1 :rhs :r2) "mul"))
  (inst expected-mnemonic)
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt inst)))
    (assert-true (search expected-mnemonic asm))))

(deftest aarch64-emit-label
  "vm-label emits label: format."
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-label :name "loop"))))
    (assert-true (search ".align 4" asm))
    (assert-true (search "loop:" asm))))

(deftest aarch64-emit-jump
  "vm-jump emits b label (unconditional branch)."
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-jump :label "done"))))
    (assert-true (search "b " asm))
    (assert-true (search "done" asm))))

(deftest aarch64-emit-jump-zero
  "vm-jump-zero emits cmp+b.eq sequence."
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-jump-zero :reg :r0 :label "else"))))
    (assert-true (search "cmp" asm))
    (assert-true (search "#0" asm))
    (assert-true (search "b.eq" asm))
    (assert-true (search "else" asm))))

(deftest aarch64-emit-halt
  "vm-halt emits mov x0+ret."
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-halt :reg :r0))))
    (assert-true (search "mov x0" asm))
    (assert-true (search "ret" asm))))

(deftest aarch64-emit-print-calls-bl-rt-print
  "vm-print emits bl rt-print with x0 as the argument register."
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-print :reg :r0))))
    (assert-true (search "bl rt-print" asm))
    (assert-true (search "x0" asm))))

(deftest aarch64-emit-unsupported-instruction-signals-error
  "Unsupported instructions (vm-ret on AArch64) signal an error."
  (let ((tgt (%make-aarch64-target)))
    (assert-signals error (%aarch64-emit tgt (make-vm-ret :reg :r0)))))

(deftest aarch64-emit-spill-operations
  "vm-spill-store emits str [x29-N], reg; vm-spill-load emits ldr reg, [x29-N]."
  (let ((tgt (%make-aarch64-target)))
    (let ((asm (%aarch64-emit tgt (make-vm-spill-store :src-reg :x19 :slot 2))))
      (assert-true (search "str" asm))
      (assert-true (search "x29" asm))
      (assert-true (search "16" asm))
      (assert-true (search "x19" asm)))
    (let ((asm (%aarch64-emit tgt (make-vm-spill-load :dst-reg :x20 :slot 3))))
      (assert-true (search "ldr" asm))
      (assert-true (search "x20" asm))
      (assert-true (search "x29" asm))
      (assert-true (search "24" asm)))))
