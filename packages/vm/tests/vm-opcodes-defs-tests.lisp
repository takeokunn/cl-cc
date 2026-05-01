;;;; tests/unit/vm/vm-opcodes-defs-tests.lisp
;;;; Unit tests for src/vm/vm-opcodes-defs.lisp public API.
;;;;
;;;; Covers:
;;;;   make-vm-state        — public constructor for canonical execution state
;;;;   vm-reg-get/vm-reg-set — polymorphic register accessors across state implementations
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

(deftest vm-opcodes-defs-make-vm-state-returns-vm-io-state
  "make-vm-state returns an instance of vm-io-state."
  (let ((s (cl-cc/vm::make-vm-state)))
    (assert-true (typep s 'cl-cc/vm::vm-io-state))))

(deftest vm-opcodes-defs-make-vm-state-defaults-to-standard-output
  "make-vm-state defaults the output stream to *standard-output*."
  (let ((s (cl-cc/vm::make-vm-state)))
    (assert-eq *standard-output* (cl-cc/vm::vm-standard-output s))))

(deftest vm-opcodes-defs-make-vm-state-accepts-custom-output-stream
  "make-vm-state :output-stream stores the supplied stream."
  (let* ((str (make-string-output-stream))
         (s   (cl-cc/vm::make-vm-state :output-stream str)))
    (assert-eq str (cl-cc/vm::vm-standard-output s))))

;;; ─── vm-state-registers / vm-output-stream / vm-global-vars (generics) ─────

(deftest vm-opcodes-defs-vm-state-registers-returns-hash-table
  "vm-state-registers returns a hash table for the public VM state."
  (let* ((s    (cl-cc/vm::make-vm-state))
         (regs (cl-cc/vm::vm-state-registers s)))
    (assert-true (hash-table-p regs))))

(deftest vm-opcodes-defs-vm-standard-output-reflects-custom-stream
  "vm-standard-output returns the stream passed to make-vm-state :output-stream."
  (let* ((str (make-string-output-stream))
         (s   (cl-cc/vm::make-vm-state :output-stream str)))
    (assert-eq str (cl-cc/vm::vm-standard-output s))))

(deftest vm-opcodes-defs-vm-global-vars-returns-hash-table
  "vm-global-vars returns a hash table for the public VM state."
  (let ((s (cl-cc/vm::make-vm-state)))
    (assert-true (hash-table-p (cl-cc/vm::vm-global-vars s)))))

;;; ─── vm-reg-get / vm-reg-set on public VM state ─────────────────────────────

(deftest vm-opcodes-defs-reg-get-fresh-register-nil
  "vm-reg-get returns 0 for any fresh register in a new public vm-io-state (hash-table based)."
  (let ((s (cl-cc/vm::make-vm-state)))
    (assert-= 0 (cl-cc/vm::vm-reg-get s 0))
    (assert-= 0 (cl-cc/vm::vm-reg-get s 127))
    (assert-= 0 (cl-cc/vm::vm-reg-get s 255))))

(deftest vm-opcodes-defs-reg-set-returns-stored-value
  "vm-reg-set returns the value it stored and makes it visible to vm-reg-get."
  (let ((s (cl-cc/vm::make-vm-state)))
    (let ((ret (cl-cc/vm::vm-reg-set s 0 42)))
      (assert-= 42 ret)
      (assert-= 42 (cl-cc/vm::vm-reg-get s 0)))))

(deftest vm-opcodes-defs-reg-set-overwrite-takes-last
  "vm-reg-set overwrites the previous value; only the last write is visible."
  (let ((s (cl-cc/vm::make-vm-state)))
    (cl-cc/vm::vm-reg-set s 3 :first)
    (cl-cc/vm::vm-reg-set s 3 :second)
    (assert-eq :second (cl-cc/vm::vm-reg-get s 3))))

(deftest vm-opcodes-defs-reg-set-adjacent-registers-independent
  "vm-reg-set to adjacent registers are independent; each holds its own value."
  (let ((s (cl-cc/vm::make-vm-state)))
    (cl-cc/vm::vm-reg-set s 10 'alpha)
    (cl-cc/vm::vm-reg-set s 11 'beta)
    (assert-eq 'alpha (cl-cc/vm::vm-reg-get s 10))
    (assert-eq 'beta  (cl-cc/vm::vm-reg-get s 11))))

(deftest vm-opcodes-defs-reg-roundtrip-all-slots
  "vm-reg-set/get round-trip is consistent for many representative register slots."
  (let ((s (cl-cc/vm::make-vm-state)))
    (dotimes (i 64)
      (cl-cc/vm::vm-reg-set s i i))
    (dotimes (i 64)
      (assert-= i (cl-cc/vm::vm-reg-get s i)))))

;;; ─── vm2 low-level constructor remains available explicitly ────────────────

(deftest vm-opcodes-defs-make-vm2-state-returns-vm2-state
  "make-vm2-state returns an object satisfying vm2-state-p."
  (let ((s (cl-cc::make-vm2-state)))
    (assert-true (cl-cc::vm2-state-p s))))

(deftest vm-opcodes-defs-make-vm2-state-accepts-custom-output-stream
  "make-vm2-state :output-stream stores the supplied stream in vm2-state-output-stream."
  (let* ((str (make-string-output-stream))
         (s   (cl-cc::make-vm2-state :output-stream str)))
    (assert-eq str (cl-cc::vm2-state-output-stream s))))

;;; ─── run-vm-with-opcode-bigrams ─────────────────────────────────────────────

(deftest vm-opcodes-defs-run-vm-bigrams-returns-halted-value
  "run-vm-with-opcode-bigrams primary return value is the halted register value."
  (let* ((code (make-bytecode2 cl-cc::+op2-const+ 0 99 nil
                                cl-cc::+op2-halt2+ 0 nil nil))
         (s    (cl-cc::make-vm2-state)))
    (assert-= 99 (cl-cc/vm::run-vm-with-opcode-bigrams code s))))

(deftest vm-opcodes-defs-run-vm-bigrams-second-value-is-hash-table
  "run-vm-with-opcode-bigrams second return value is a bigram count hash table."
  (let* ((code (make-bytecode2 cl-cc::+op2-const+ 0 1 nil
                                cl-cc::+op2-halt2+ 0 nil nil))
         (s    (cl-cc::make-vm2-state)))
    (multiple-value-bind (result counts)
        (cl-cc/vm::run-vm-with-opcode-bigrams code s)
      (declare (ignore result))
      (assert-true (hash-table-p counts)))))
