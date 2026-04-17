;;;; tests/unit/emit/calling-convention-tests.lisp — Calling Convention Tests
;;;;
;;;; Tests for src/emit/calling-convention.lisp:
;;;; calling-convention struct, x86-64 and AArch64 ABI definitions.

(in-package :cl-cc/test)

(defsuite calling-convention-suite :description "Calling convention definition tests"
  :parent cl-cc-unit-suite)


(in-suite calling-convention-suite)
;;; ─── x86-64 System V ABI ──────────────────────────────────────────────────

(deftest cc-x86-64-basic-properties
  "x86-64 calling convention: exists, arg registers, return rax, scratch r11."
  (assert-true (cl-cc/emit::calling-convention-p cl-cc/emit::*x86-64-calling-convention*))
  (let ((args (cl-cc::cc-arg-registers cl-cc/emit::*x86-64-calling-convention*)))
    (assert-equal 6 (length args))
    (assert-eq :rdi (first args))
    (assert-eq :r9 (car (last args))))
  (assert-eq :rax (cl-cc::cc-return-register cl-cc/emit::*x86-64-calling-convention*))
  (assert-eq :r11 (cl-cc::cc-scratch-register cl-cc/emit::*x86-64-calling-convention*)))

(deftest cc-x86-64-save-categories
  "x86-64 callee-saved includes rbx/r12-r15 (not rax); caller-saved includes rax/rcx/rdx (not rbx)."
  (let ((callee (cl-cc::cc-callee-saved cl-cc/emit::*x86-64-calling-convention*))
        (caller (cl-cc::cc-caller-saved cl-cc/emit::*x86-64-calling-convention*)))
    (assert-true  (member :rbx callee))
    (assert-true  (member :r12 callee))
    (assert-true  (member :r15 callee))
    (assert-false (member :rax callee))
    (assert-true  (member :rax caller))
    (assert-true  (member :rcx caller))
    (assert-true  (member :rdx caller))
    (assert-false (member :rbx caller))))

(deftest cc-x86-64-gpr-pool
  "x86-64 GPR pool has 13 registers (excludes rsp, rbp, r11)."
  (let ((pool (cl-cc::cc-gpr-pool cl-cc/emit::*x86-64-calling-convention*)))
    (assert-equal 13 (length pool))
    ;; rsp and rbp should not be in the pool
    (assert-false (member :rsp pool))
    (assert-false (member :rbp pool))))

;;; ─── AArch64 AAPCS ────────────────────────────────────────────────────────

(deftest cc-aarch64-basic-properties
  "AArch64 calling convention: exists, arg registers x0-x7, return x0, scratch x16."
  (assert-true (cl-cc/emit::calling-convention-p cl-cc/emit::*aarch64-calling-convention*))
  (let ((args (cl-cc::cc-arg-registers cl-cc/emit::*aarch64-calling-convention*)))
    (assert-equal 8 (length args))
    (assert-eq :x0 (first args))
    (assert-eq :x7 (car (last args))))
  (assert-eq :x0 (cl-cc::cc-return-register cl-cc/emit::*aarch64-calling-convention*))
  (assert-eq :x16 (cl-cc::cc-scratch-register cl-cc/emit::*aarch64-calling-convention*)))

(deftest cc-aarch64-save-categories
  "AArch64 callee-saved includes x19-x28 (10 regs); GPR pool excludes x18 (platform register)."
  (let ((saved (cl-cc::cc-callee-saved cl-cc/emit::*aarch64-calling-convention*))
        (pool  (cl-cc::cc-gpr-pool    cl-cc/emit::*aarch64-calling-convention*)))
    (assert-true  (member :x19 saved))
    (assert-true  (member :x28 saved))
    (assert-equal 10 (length saved))
    (assert-false (member :x18 pool))))
