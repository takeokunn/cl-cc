;;;; tests/unit/emit/target-tests.lisp — Target Descriptor Tests
;;;;
;;;; Tests for target-desc struct, target registry, and utility functions.

(in-package :cl-cc/test)

(defsuite target-suite :description "Target descriptor tests"
  :parent cl-cc-unit-suite)


(in-suite target-suite)

;;; ─── Target Descriptor Struct ───────────────────────────────────────────────

(deftest target-x86-64-basics
  "x86-64 target has correct basic properties."
  (let ((t1 cl-cc/target:*x86-64-target*))
    (assert-eq :x86-64 (cl-cc/target:target-name t1))
    (assert-equal 8 (cl-cc/target:target-word-size t1))
    (assert-eq :little (cl-cc/target:target-endianness t1))
    (assert-equal 16 (cl-cc/target:target-gpr-count t1))))

(deftest target-aarch64-basics
  "AArch64 target has correct basic properties."
  (let ((t1 cl-cc/target:*aarch64-target*))
    (assert-eq :aarch64 (cl-cc/target:target-name t1))
    (assert-equal 31 (cl-cc/target:target-gpr-count t1))
    (assert-eq :x0 (cl-cc/target:target-ret-reg t1))))

(deftest target-riscv64-basics
  "RISC-V 64 target has correct basic properties."
  (let ((t1 cl-cc/target:*riscv64-target*))
    (assert-eq :riscv64 (cl-cc/target:target-name t1))
    (assert-equal 32 (cl-cc/target:target-gpr-count t1))
    (assert-eq :a0 (cl-cc/target:target-ret-reg t1))))

(deftest target-wasm32-stack-machine
  "Wasm32 is a stack machine (0 GPRs, no arg/ret regs)."
  (let ((t1 cl-cc/target:*wasm32-target*))
    (assert-eq :wasm32 (cl-cc/target:target-name t1))
    (assert-equal 4 (cl-cc/target:target-word-size t1))
    (assert-equal 0 (cl-cc/target:target-gpr-count t1))
    (assert-null (cl-cc/target:target-arg-regs t1))
    (assert-null (cl-cc/target:target-ret-reg t1))))

;;; ─── Target Registry ────────────────────────────────────────────────────────

(deftest target-registry-find
  "find-target: all 4 predefined targets found; unknown target returns nil."
  (let ((x86 (cl-cc/target:find-target :x86-64)))
    (assert-true (cl-cc/target:target-desc-p x86))
    (assert-eq :x86-64 (cl-cc/target:target-name x86)))
  (assert-true (cl-cc/target:target-desc-p (cl-cc/target:find-target :aarch64)))
  (assert-true (cl-cc/target:target-desc-p (cl-cc/target:find-target :riscv64)))
  (assert-true (cl-cc/target:target-desc-p (cl-cc/target:find-target :wasm32)))
  (assert-null (cl-cc/target:find-target :pdp-11)))

;;; ─── Target Utility Functions ───────────────────────────────────────────────

(deftest-each target-64-bit-p-classification
  "target-64-bit-p: true for register ISAs, false for the wasm32 stack machine."
  :cases (("x86-64"
           cl-cc/target:*x86-64-target*
           (lambda (target)
             (assert-true (cl-cc/target:target-64-bit-p target))))
          ("aarch64"
           cl-cc/target:*aarch64-target*
           (lambda (target)
             (assert-true (cl-cc/target:target-64-bit-p target))))
          ("riscv64"
           cl-cc/target:*riscv64-target*
           (lambda (target)
             (assert-true (cl-cc/target:target-64-bit-p target))))
          ("wasm32"
           cl-cc/target:*wasm32-target*
           (lambda (target)
             (assert-false (cl-cc/target:target-64-bit-p target)))))
  (target verify)
  (funcall verify target))

(deftest-each target-has-feature-p-cases
  "target-has-feature-p finds architecture-specific features and rejects absent ones."
  :cases (("x86-sysv"
           cl-cc/target:*x86-64-target* :sysv-abi
           (lambda (target feature)
             (assert-true (cl-cc/target:target-has-feature-p target feature))))
          ("arm-aapcs"
           cl-cc/target:*aarch64-target* :aapcs64
           (lambda (target feature)
             (assert-true (cl-cc/target:target-has-feature-p target feature))))
          ("wasm-structured"
           cl-cc/target:*wasm32-target* :structured-control-flow
           (lambda (target feature)
             (assert-true (cl-cc/target:target-has-feature-p target feature))))
          ("x86-no-wasm-feat"
           cl-cc/target:*x86-64-target* :structured-control-flow
           (lambda (target feature)
             (assert-false (cl-cc/target:target-has-feature-p target feature))))
          ("wasm-no-sysv"
           cl-cc/target:*wasm32-target* :sysv-abi
           (lambda (target feature)
             (assert-false (cl-cc/target:target-has-feature-p target feature)))))
  (target feature verify)
  (funcall verify target feature))

(deftest target-allocatable-regs-behavior
  "target-allocatable-regs: x86-64 excludes scratch (:rsp/:r11), includes :rax; wasm32 empty."
  (let ((alloc (cl-cc/target:target-allocatable-regs cl-cc/target:*x86-64-target*)))
    (assert-false (member :rsp alloc))
    (assert-false (member :r11 alloc))
    (assert-true  (member :rax alloc)))
  (assert-null (cl-cc/target:target-allocatable-regs cl-cc/target:*wasm32-target*)))

(deftest target-caller-saved-subset-of-allocatable
  "Caller-saved regs are a subset of allocatable regs."
  (let ((caller (cl-cc/target:target-caller-saved cl-cc/target:*x86-64-target*))
        (alloc  (cl-cc/target:target-allocatable-regs cl-cc/target:*x86-64-target*)))
    (assert-true (every (lambda (r) (member r alloc)) caller))))

(deftest-each target-reg-index-lookup
  "target-reg-index maps known x86-64 registers to indices; returns nil for unknown."
  :cases (("rax"     0   :rax)
          ("rdi"     7   :rdi)
          ("unknown" nil :r99))
  (expected reg)
  (assert-equal expected (cl-cc/target:target-reg-index cl-cc/target:*x86-64-target* reg)))

(deftest target-op-legal-default
  "Unregistered ops are legal by default (permissive)."
  (assert-true (cl-cc/target:target-op-legal-p cl-cc/target:*x86-64-target* :add)))
