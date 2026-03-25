;;;; tests/unit/emit/calling-convention-tests.lisp — Calling Convention Tests
;;;;
;;;; Tests for src/emit/calling-convention.lisp:
;;;; calling-convention struct, x86-64 and AArch64 ABI definitions.

(in-package :cl-cc/test)

(defsuite calling-convention-suite :description "Calling convention definition tests")

;;; ─── x86-64 System V ABI ──────────────────────────────────────────────────

(deftest cc-x86-64-exists
  "x86-64 calling convention is defined."
  (assert-true (cl-cc::calling-convention-p cl-cc::*x86-64-calling-convention*)))

(deftest cc-x86-64-arg-registers
  "x86-64 SysV uses rdi, rsi, rdx, rcx, r8, r9 for args."
  (let ((args (cl-cc::cc-arg-registers cl-cc::*x86-64-calling-convention*)))
    (assert-equal 6 (length args))
    (assert-eq :rdi (first args))
    (assert-eq :r9 (car (last args)))))

(deftest cc-x86-64-return-register
  "x86-64 returns in rax."
  (assert-eq :rax (cl-cc::cc-return-register cl-cc::*x86-64-calling-convention*)))

(deftest cc-x86-64-scratch-register
  "x86-64 scratch register is r11."
  (assert-eq :r11 (cl-cc::cc-scratch-register cl-cc::*x86-64-calling-convention*)))

(deftest cc-x86-64-callee-saved
  "x86-64 callee-saved includes rbx and r12-r15."
  (let ((saved (cl-cc::cc-callee-saved cl-cc::*x86-64-calling-convention*)))
    (assert-true (member :rbx saved))
    (assert-true (member :r12 saved))
    (assert-true (member :r15 saved))
    ;; rax is NOT callee-saved
    (assert-false (member :rax saved))))

(deftest cc-x86-64-caller-saved
  "x86-64 caller-saved includes rax, rcx, rdx."
  (let ((saved (cl-cc::cc-caller-saved cl-cc::*x86-64-calling-convention*)))
    (assert-true (member :rax saved))
    (assert-true (member :rcx saved))
    (assert-true (member :rdx saved))
    ;; rbx is NOT caller-saved
    (assert-false (member :rbx saved))))

(deftest cc-x86-64-gpr-pool
  "x86-64 GPR pool has 13 registers (excludes rsp, rbp, r11)."
  (let ((pool (cl-cc::cc-gpr-pool cl-cc::*x86-64-calling-convention*)))
    (assert-equal 13 (length pool))
    ;; rsp and rbp should not be in the pool
    (assert-false (member :rsp pool))
    (assert-false (member :rbp pool))))

;;; ─── AArch64 AAPCS ────────────────────────────────────────────────────────

(deftest cc-aarch64-exists
  "AArch64 calling convention is defined."
  (assert-true (cl-cc::calling-convention-p cl-cc::*aarch64-calling-convention*)))

(deftest cc-aarch64-arg-registers
  "AArch64 uses x0-x7 for args."
  (let ((args (cl-cc::cc-arg-registers cl-cc::*aarch64-calling-convention*)))
    (assert-equal 8 (length args))
    (assert-eq :x0 (first args))
    (assert-eq :x7 (car (last args)))))

(deftest cc-aarch64-return-register
  "AArch64 returns in x0."
  (assert-eq :x0 (cl-cc::cc-return-register cl-cc::*aarch64-calling-convention*)))

(deftest cc-aarch64-scratch-register
  "AArch64 scratch register is x16."
  (assert-eq :x16 (cl-cc::cc-scratch-register cl-cc::*aarch64-calling-convention*)))

(deftest cc-aarch64-callee-saved
  "AArch64 callee-saved includes x19-x28."
  (let ((saved (cl-cc::cc-callee-saved cl-cc::*aarch64-calling-convention*)))
    (assert-true (member :x19 saved))
    (assert-true (member :x28 saved))
    (assert-equal 10 (length saved))))

(deftest cc-aarch64-no-x18
  "AArch64 GPR pool excludes x18 (platform register)."
  (let ((pool (cl-cc::cc-gpr-pool cl-cc::*aarch64-calling-convention*)))
    (assert-false (member :x18 pool))))
