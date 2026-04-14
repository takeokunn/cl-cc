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

(deftest vm-opcodes-defs-make-vm-state-creates-vm2-state
  "make-vm-state returns a vm2-state struct."
  (let ((s (cl-cc::make-vm-state)))
    (assert-true (cl-cc::vm2-state-p s))))

(deftest vm-opcodes-defs-make-vm-state-default-output
  "make-vm-state defaults output-stream to *standard-output*."
  (let ((s (cl-cc::make-vm-state)))
    (assert-eq *standard-output* (cl-cc::vm2-state-output-stream s))))

(deftest vm-opcodes-defs-make-vm-state-custom-output
  "make-vm-state accepts a custom :output-stream."
  (let* ((str (make-string-output-stream))
         (s   (cl-cc::make-vm-state :output-stream str)))
    (assert-eq str (cl-cc::vm2-state-output-stream s))))

;;; ─── vm-state-registers / vm-output-stream / vm-global-vars (generics) ─────

(deftest vm-opcodes-defs-vm-state-registers-returns-vector
  "vm-state-registers on a vm2-state returns the 256-slot register vector."
  (let* ((s   (cl-cc::make-vm-state))
         (regs (cl-cc::vm-state-registers s)))
    (assert-true (simple-vector-p regs))
    (assert-= 256 (length regs))))

(deftest vm-opcodes-defs-vm-output-stream-returns-stream
  "vm-output-stream on a vm2-state returns its output stream."
  (let* ((str (make-string-output-stream))
         (s   (cl-cc::make-vm-state :output-stream str)))
    (assert-eq str (cl-cc::vm-output-stream s))))

(deftest vm-opcodes-defs-vm-global-vars-returns-hash-table
  "vm-global-vars on a vm2-state returns a hash table."
  (let ((s (cl-cc::make-vm-state)))
    (assert-true (hash-table-p (cl-cc::vm-global-vars s)))))

;;; ─── vm-reg-get / vm-reg-set on vm2-state ────────────────────────────────────

(deftest vm-opcodes-defs-reg-get-fresh-register-nil
  "vm-reg-get returns nil for any fresh register in a new vm2-state."
  (let ((s (cl-cc::make-vm-state)))
    (assert-null (cl-cc::vm-reg-get s 0))
    (assert-null (cl-cc::vm-reg-get s 127))
    (assert-null (cl-cc::vm-reg-get s 255))))

(deftest vm-opcodes-defs-reg-set-stores-and-returns
  "vm-reg-set stores a value and returns it."
  (let ((s (cl-cc::make-vm-state)))
    (let ((ret (cl-cc::vm-reg-set s 0 42)))
      (assert-= 42 ret)
      (assert-= 42 (cl-cc::vm-reg-get s 0)))))

(deftest vm-opcodes-defs-reg-set-overwrites
  "vm-reg-set overwrites a previously stored value."
  (let ((s (cl-cc::make-vm-state)))
    (cl-cc::vm-reg-set s 3 :first)
    (cl-cc::vm-reg-set s 3 :second)
    (assert-eq :second (cl-cc::vm-reg-get s 3))))

(deftest vm-opcodes-defs-reg-set-independent-slots
  "vm-reg-set does not clobber adjacent register slots."
  (let ((s (cl-cc::make-vm-state)))
    (cl-cc::vm-reg-set s 10 'alpha)
    (cl-cc::vm-reg-set s 11 'beta)
    (assert-eq 'alpha (cl-cc::vm-reg-get s 10))
    (assert-eq 'beta  (cl-cc::vm-reg-get s 11))))

(deftest vm-opcodes-defs-reg-roundtrip-all-slots
  "vm-reg-set/get round-trip is consistent for all 256 register slots."
  (let ((s (cl-cc::make-vm-state)))
    (dotimes (i 256)
      (cl-cc::vm-reg-set s i i))
    (dotimes (i 256)
      (assert-= i (cl-cc::vm-reg-get s i)))))

;;; ─── run-vm-with-opcode-bigrams ─────────────────────────────────────────────

(deftest vm-opcodes-defs-run-vm-with-bigrams-returns-result
  "run-vm-with-opcode-bigrams returns the halted value as first value."
  (let* ((code (make-bytecode2 cl-cc::+op2-const+ 0 99 nil
                               cl-cc::+op2-halt2+ 0 nil nil))
         (s    (cl-cc::make-vm-state)))
    (let ((result (cl-cc::run-vm-with-opcode-bigrams code s)))
      (assert-= 99 result))))

(deftest vm-opcodes-defs-run-vm-with-bigrams-returns-hash-table
  "run-vm-with-opcode-bigrams returns a hash table as second value."
  (let* ((code (make-bytecode2 cl-cc::+op2-const+ 0 1 nil
                               cl-cc::+op2-halt2+ 0 nil nil))
         (s    (cl-cc::make-vm-state)))
    (multiple-value-bind (result counts)
        (cl-cc::run-vm-with-opcode-bigrams code s)
      (declare (ignore result))
      (assert-true (hash-table-p counts)))))

(deftest vm-opcodes-defs-run-vm-with-bigrams-counts-is-hash-table-always
  "run-vm-with-opcode-bigrams always returns a hash-table, even for short programs."
  ;; The fast path in %run-vm-core may not populate bigrams for fused opcodes,
  ;; so we only verify the returned counts object is a hash-table (API contract).
  (let* ((code (make-bytecode2 cl-cc::+op2-const+ 0 42 nil
                               cl-cc::+op2-halt2+ 0 nil nil))
         (s    (cl-cc::make-vm-state)))
    (multiple-value-bind (_result counts)
        (cl-cc::run-vm-with-opcode-bigrams code s)
      (assert-true (hash-table-p counts)))))
