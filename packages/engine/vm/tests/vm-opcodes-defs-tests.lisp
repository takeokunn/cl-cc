;;;; tests/unit/vm/vm-opcodes-defs-tests.lisp
;;;; Unit tests for src/vm/vm-opcodes-defs.lisp public API.
;;;;
;;;; Covers:
;;;;   make-vm-state        — public constructor (compatibility shim)
;;;;   vm-reg-get/vm-reg-set — polymorphic register accessors on vm2-state
;;;;   vm-state-registers   — generic method on vm2-state
;;;;   vm-output-stream     — generic method on vm2-state
;;;;   vm-global-vars       — generic method on vm2-state
;;;;   run-vm-with-opcode-bigrams — bigram profiling variant of run-vm

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helper ─────────────────────────────────────────────────────────────────

(defun make-bytecode2 (&rest words)
  "Build a simple-vector from WORDS for vm2 bytecode tests."
  (coerce words 'simple-vector))

;;; ─── make-vm-state ──────────────────────────────────────────────────────────

(deftest vm-opcodes-defs-make-vm-state-cases
  "make-vm-state: returns vm2-state; defaults to *standard-output*; accepts custom :output-stream."
  (let ((s (cl-cc/vm::make-vm-state)))
    (assert-true (cl-cc::vm2-state-p s)))
  (let ((s (cl-cc/vm::make-vm-state)))
    (assert-eq *standard-output* (cl-cc::vm2-state-output-stream s)))
  (let* ((str (make-string-output-stream))
         (s   (cl-cc/vm::make-vm-state :output-stream str)))
    (assert-eq str (cl-cc::vm2-state-output-stream s))))

;;; ─── vm-state-registers / vm-output-stream / vm-global-vars (generics) ─────

(deftest vm-opcodes-defs-generic-accessors-cases
  "Generic accessors on vm2-state: registers→256-slot vector; output-stream→correct; global-vars→hash-table."
  (let* ((s    (cl-cc/vm::make-vm-state))
         (regs (cl-cc/vm::vm-state-registers s)))
    (assert-true (simple-vector-p regs))
    (assert-= 256 (length regs)))
  (let* ((str (make-string-output-stream))
         (s   (cl-cc/vm::make-vm-state :output-stream str)))
    (assert-eq str (cl-cc/vm::vm-output-stream s)))
  (let ((s (cl-cc/vm::make-vm-state)))
    (assert-true (hash-table-p (cl-cc/vm::vm-global-vars s)))))

;;; ─── vm-reg-get / vm-reg-set on vm2-state ────────────────────────────────────

(deftest vm-opcodes-defs-reg-get-fresh-register-nil
  "vm-reg-get returns nil for any fresh register in a new vm2-state."
  (let ((s (cl-cc/vm::make-vm-state)))
    (assert-null (cl-cc/vm::vm-reg-get s 0))
    (assert-null (cl-cc/vm::vm-reg-get s 127))
    (assert-null (cl-cc/vm::vm-reg-get s 255))))

(deftest vm-opcodes-defs-reg-set-cases
  "vm-reg-set: stores and returns value; overwrites previous; leaves adjacent slots unchanged."
  (let ((s (cl-cc/vm::make-vm-state)))
    (let ((ret (cl-cc/vm::vm-reg-set s 0 42)))
      (assert-= 42 ret)
      (assert-= 42 (cl-cc/vm::vm-reg-get s 0))))
  (let ((s (cl-cc/vm::make-vm-state)))
    (cl-cc/vm::vm-reg-set s 3 :first)
    (cl-cc/vm::vm-reg-set s 3 :second)
    (assert-eq :second (cl-cc/vm::vm-reg-get s 3)))
  (let ((s (cl-cc/vm::make-vm-state)))
    (cl-cc/vm::vm-reg-set s 10 'alpha)
    (cl-cc/vm::vm-reg-set s 11 'beta)
    (assert-eq 'alpha (cl-cc/vm::vm-reg-get s 10))
    (assert-eq 'beta  (cl-cc/vm::vm-reg-get s 11))))

(deftest vm-opcodes-defs-reg-roundtrip-all-slots
  "vm-reg-set/get round-trip is consistent for all 256 register slots."
  (let ((s (cl-cc/vm::make-vm-state)))
    (dotimes (i 256)
      (cl-cc/vm::vm-reg-set s i i))
    (dotimes (i 256)
      (assert-= i (cl-cc/vm::vm-reg-get s i)))))

;;; ─── run-vm-with-opcode-bigrams ─────────────────────────────────────────────

(deftest vm-opcodes-defs-run-vm-bigrams-cases
  "run-vm-with-opcode-bigrams: returns halted value; second value is hash-table; always hash-table."
  (let* ((code (make-bytecode2 cl-cc::+op2-const+ 0 99 nil
                               cl-cc::+op2-halt2+ 0 nil nil))
         (s    (cl-cc/vm::make-vm-state)))
    (assert-= 99 (cl-cc/vm::run-vm-with-opcode-bigrams code s)))
  (let* ((code (make-bytecode2 cl-cc::+op2-const+ 0 1 nil
                               cl-cc::+op2-halt2+ 0 nil nil))
         (s    (cl-cc/vm::make-vm-state)))
    (multiple-value-bind (result counts)
        (cl-cc/vm::run-vm-with-opcode-bigrams code s)
      (declare (ignore result))
      (assert-true (hash-table-p counts))))
  (let* ((code (make-bytecode2 cl-cc::+op2-const+ 0 42 nil
                               cl-cc::+op2-halt2+ 0 nil nil))
         (s    (cl-cc/vm::make-vm-state)))
    (multiple-value-bind (_result counts)
        (cl-cc/vm::run-vm-with-opcode-bigrams code s)
      (assert-true (hash-table-p counts)))))
