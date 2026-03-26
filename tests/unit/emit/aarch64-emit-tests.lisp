;;;; tests/unit/emit/aarch64-emit-tests.lisp — AArch64 Assembly Emit Tests
;;;;
;;;; Tests for src/emit/aarch64.lisp:
;;;; aarch64-target, target-register, emit-instruction methods

(in-package :cl-cc/test)

(defsuite aarch64-emit-suite :description "AArch64 assembly emit tests")

;;; ─── Helper ─────────────────────────────────────────────────────────────────

(defun %aarch64-emit (target inst)
  "Emit INST to a string using TARGET and return the result."
  (let ((s (make-string-output-stream)))
    (cl-cc::emit-instruction target inst s)
    (get-output-stream-string s)))

(defun %make-aarch64-target ()
  (make-instance 'cl-cc::aarch64-target))

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
    (assert-equal expected (cl-cc::target-register tgt vreg))))

(deftest aarch64-target-register-overflow
  "AArch64 target-register signals error for :R8+ (pool exhausted)."
  (let ((tgt (%make-aarch64-target)))
    (assert-signals error (cl-cc::target-register tgt :r8))))

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

(deftest aarch64-emit-sub
  "vm-sub emits 3-operand sub."
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-sub :dst :r0 :lhs :r1 :rhs :r2))))
    (assert-true (search "sub" asm))))

(deftest aarch64-emit-mul
  "vm-mul emits 3-operand mul."
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-mul :dst :r0 :lhs :r1 :rhs :r2))))
    (assert-true (search "mul" asm))))

(deftest aarch64-emit-label
  "vm-label emits label: format."
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-label :name "loop"))))
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

(deftest aarch64-emit-print-signals-error
  "vm-print signals error (not implemented)."
  (let ((tgt (%make-aarch64-target)))
    (assert-signals error (%aarch64-emit tgt (make-vm-print :reg :r0)))))

(deftest aarch64-emit-unsupported-silent
  "Unsupported instructions emit nothing (base method)."
  (let* ((tgt (%make-aarch64-target))
         (asm (%aarch64-emit tgt (make-vm-ret :reg :r0))))
    (assert-equal "" asm)))
