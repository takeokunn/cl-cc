;;;; tests/unit/emit/target-tests.lisp — Target Descriptor + Calling Convention Tests
;;;;
;;;; Tests for calling-convention struct, target-desc struct, target registry,
;;;; and target utility functions.

(in-package :cl-cc/test)

(defsuite target-suite :description "Target descriptor and calling convention tests")

;;; ─── Calling Convention Struct ──────────────────────────────────────────────

(deftest cc-x86-64-arg-registers
  "x86-64 SysV ABI passes first 6 args in registers."
  (assert-equal 6 (length (cl-cc::cc-arg-registers cl-cc::*x86-64-calling-convention*)))
  (assert-eq :rdi (first (cl-cc::cc-arg-registers cl-cc::*x86-64-calling-convention*))))

(deftest cc-x86-64-return-register
  "x86-64 returns in rax."
  (assert-eq :rax (cl-cc::cc-return-register cl-cc::*x86-64-calling-convention*)))

(deftest cc-aarch64-arg-registers
  "AArch64 AAPCS passes first 8 args in registers."
  (assert-equal 8 (length (cl-cc::cc-arg-registers cl-cc::*aarch64-calling-convention*)))
  (assert-eq :x0 (first (cl-cc::cc-arg-registers cl-cc::*aarch64-calling-convention*))))

(deftest cc-aarch64-return-register
  "AArch64 returns in x0."
  (assert-eq :x0 (cl-cc::cc-return-register cl-cc::*aarch64-calling-convention*)))

(deftest cc-callee-saved-disjoint-from-caller
  "Callee-saved and caller-saved are disjoint on x86-64."
  (let ((callee (cl-cc::cc-callee-saved cl-cc::*x86-64-calling-convention*))
        (caller (cl-cc::cc-caller-saved cl-cc::*x86-64-calling-convention*)))
    (assert-null (intersection callee caller))))

;;; ─── Target Descriptor Struct ───────────────────────────────────────────────

(deftest target-x86-64-basics
  "x86-64 target has correct basic properties."
  (let ((t1 cl-cc::*x86-64-target*))
    (assert-eq :x86-64 (cl-cc::target-name t1))
    (assert-equal 8 (cl-cc::target-word-size t1))
    (assert-eq :little (cl-cc::target-endianness t1))
    (assert-equal 16 (cl-cc::target-gpr-count t1))))

(deftest target-aarch64-basics
  "AArch64 target has correct basic properties."
  (let ((t1 cl-cc::*aarch64-target*))
    (assert-eq :aarch64 (cl-cc::target-name t1))
    (assert-equal 31 (cl-cc::target-gpr-count t1))
    (assert-eq :x0 (cl-cc::target-ret-reg t1))))

(deftest target-riscv64-basics
  "RISC-V 64 target has correct basic properties."
  (let ((t1 cl-cc::*riscv64-target*))
    (assert-eq :riscv64 (cl-cc::target-name t1))
    (assert-equal 32 (cl-cc::target-gpr-count t1))
    (assert-eq :a0 (cl-cc::target-ret-reg t1))))

(deftest target-wasm32-stack-machine
  "Wasm32 is a stack machine (0 GPRs, no arg/ret regs)."
  (let ((t1 cl-cc::*wasm32-target*))
    (assert-eq :wasm32 (cl-cc::target-name t1))
    (assert-equal 4 (cl-cc::target-word-size t1))
    (assert-equal 0 (cl-cc::target-gpr-count t1))
    (assert-null (cl-cc::target-arg-regs t1))
    (assert-null (cl-cc::target-ret-reg t1))))

;;; ─── Target Registry ────────────────────────────────────────────────────────

(deftest target-registry-find-x86-64
  "find-target looks up :x86-64."
  (let ((found (cl-cc::find-target :x86-64)))
    (assert-true (cl-cc::target-desc-p found))
    (assert-eq :x86-64 (cl-cc::target-name found))))

(deftest target-registry-find-all-predefined
  "All 4 predefined targets are in the registry."
  (assert-true (cl-cc::target-desc-p (cl-cc::find-target :x86-64)))
  (assert-true (cl-cc::target-desc-p (cl-cc::find-target :aarch64)))
  (assert-true (cl-cc::target-desc-p (cl-cc::find-target :riscv64)))
  (assert-true (cl-cc::target-desc-p (cl-cc::find-target :wasm32))))

(deftest target-registry-find-nonexistent
  "find-target returns nil for unknown target."
  (assert-null (cl-cc::find-target :pdp-11)))

;;; ─── Target Utility Functions ───────────────────────────────────────────────

(deftest target-64-bit-p-true
  "target-64-bit-p returns true for 64-bit targets."
  (assert-true (cl-cc::target-64-bit-p cl-cc::*x86-64-target*))
  (assert-true (cl-cc::target-64-bit-p cl-cc::*aarch64-target*))
  (assert-true (cl-cc::target-64-bit-p cl-cc::*riscv64-target*)))

(deftest target-64-bit-p-false-wasm
  "target-64-bit-p returns false for wasm32."
  (assert-false (cl-cc::target-64-bit-p cl-cc::*wasm32-target*)))

(deftest target-has-feature-p-present
  "target-has-feature-p finds known features."
  (assert-true (cl-cc::target-has-feature-p cl-cc::*x86-64-target* :sysv-abi))
  (assert-true (cl-cc::target-has-feature-p cl-cc::*aarch64-target* :aapcs64))
  (assert-true (cl-cc::target-has-feature-p cl-cc::*wasm32-target* :structured-control-flow)))

(deftest target-has-feature-p-absent
  "target-has-feature-p returns nil for absent features."
  (assert-false (cl-cc::target-has-feature-p cl-cc::*x86-64-target* :structured-control-flow))
  (assert-false (cl-cc::target-has-feature-p cl-cc::*wasm32-target* :sysv-abi)))

(deftest target-allocatable-regs-excludes-scratch
  "target-allocatable-regs excludes scratch registers."
  (let ((alloc (cl-cc::target-allocatable-regs cl-cc::*x86-64-target*)))
    ;; rsp and r11 are scratch, should not appear
    (assert-false (member :rsp alloc))
    (assert-false (member :r11 alloc))
    ;; rax should be allocatable
    (assert-true (member :rax alloc))))

(deftest target-allocatable-regs-wasm-empty
  "Wasm has no allocatable registers."
  (assert-null (cl-cc::target-allocatable-regs cl-cc::*wasm32-target*)))

(deftest target-caller-saved-subset-of-allocatable
  "Caller-saved regs are a subset of allocatable regs."
  (let ((caller (cl-cc::target-caller-saved cl-cc::*x86-64-target*))
        (alloc  (cl-cc::target-allocatable-regs cl-cc::*x86-64-target*)))
    (assert-true (every (lambda (r) (member r alloc)) caller))))

(deftest target-reg-index-known
  "target-reg-index returns correct index for known registers."
  (assert-equal 0 (cl-cc::target-reg-index cl-cc::*x86-64-target* :rax))
  (assert-equal 7 (cl-cc::target-reg-index cl-cc::*x86-64-target* :rdi)))

(deftest target-reg-index-unknown
  "target-reg-index returns nil for unknown register."
  (assert-null (cl-cc::target-reg-index cl-cc::*x86-64-target* :r99)))

(deftest target-op-legal-default
  "Unregistered ops are legal by default (permissive)."
  (assert-true (cl-cc::target-op-legal-p cl-cc::*x86-64-target* :add)))
