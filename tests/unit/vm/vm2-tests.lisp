;;;; tests/unit/vm/vm2-tests.lisp — Phase A: defopcode + run-vm tests
;;;
;;; Tests for the new defopcode macro infrastructure, vm2-state defstruct,
;;; integer-indexed register file, and run-vm flat-vector interpreter.

(in-package :cl-cc/test)

(defsuite vm2-suite
  :description "Phase A: defopcode dispatch table and run-vm interpreter tests"
  :parent cl-cc-suite)

(in-suite vm2-suite)

;;; ------------------------------------------------------------
;;; defopcode registration
;;; ------------------------------------------------------------

(deftest vm2-opcode-const-registered
  "defopcode const assigns a constant and populates the dispatch table."
  (assert-true (numberp cl-cc::+op2-const+))
  (assert-true (not (null (aref cl-cc::*opcode-dispatch-table* cl-cc::+op2-const+)))))

(deftest vm2-opcode-add2-registered
  "defopcode add2 has a dispatch handler and name entry."
  (assert-true (not (null (aref cl-cc::*opcode-dispatch-table* cl-cc::+op2-add2+))))
  (assert-equal 'cl-cc::add2 (aref cl-cc::*opcode-name-table* cl-cc::+op2-add2+)))

(deftest vm2-opcode-encoder-table
  "defopcode registers the name->opcode mapping in *opcode-encoder-table*."
  (assert-= cl-cc::+op2-const+
            (gethash 'cl-cc::const cl-cc::*opcode-encoder-table*))
  (assert-= cl-cc::+op2-move+
            (gethash 'cl-cc::move cl-cc::*opcode-encoder-table*))
  (assert-= cl-cc::+op2-add2+
            (gethash 'cl-cc::add2 cl-cc::*opcode-encoder-table*)))

(deftest vm2-opcode-distinct-values
  "Each defopcode gets a unique opcode number."
  (let ((ops (list cl-cc::+op2-const+
                   cl-cc::+op2-move+
                   cl-cc::+op2-add2+
                   cl-cc::+op2-sub2+
                   cl-cc::+op2-mul2+
                   cl-cc::+op2-halt2+)))
    (assert-= (length ops) (length (remove-duplicates ops)))))

;;; ------------------------------------------------------------
;;; vm2-state defstruct
;;; ------------------------------------------------------------

(deftest vm2-state-is-struct
  "make-vm-state creates a vm2-state struct."
  (let ((s (cl-cc::make-vm2-state)))
    (assert-true (cl-cc::vm2-state-p s))))

(deftest vm2-state-register-file-is-simple-vector
  "vm-state-registers is a simple-vector of 256 elements."
  (let ((s (cl-cc::make-vm2-state)))
    (assert-true (simple-vector-p (cl-cc::vm2-state-registers s)))
    (assert-= 256 (length (cl-cc::vm2-state-registers s)))))

(deftest vm2-state-registers-init-nil
  "All 256 registers are initialised to NIL."
  (let ((s (cl-cc::make-vm2-state)))
    (dotimes (i 256)
      (assert-true (null (cl-cc::vm2-reg-get s i))))))

(deftest vm2-state-output-stream
  "make-vm-state :output-stream sets the output stream slot."
  (let* ((str (make-string-output-stream))
         (s   (cl-cc::make-vm2-state :output-stream str)))
    (assert-equal str (cl-cc::vm2-state-output-stream s))))

(deftest vm2-state-globals-populated
  "make-vm-state pre-populates *features* and other global vars."
  (let ((s (cl-cc::make-vm2-state)))
    (assert-true (not (null (gethash '*features* (cl-cc::vm2-state-global-vars s)))))))

;;; ------------------------------------------------------------
;;; vm-reg-get / vm-reg-set (integer-indexed)
;;; ------------------------------------------------------------

(deftest vm2-reg-set-and-get
  "vm-reg-set stores a value; vm-reg-get retrieves it."
  (let ((s (cl-cc::make-vm2-state)))
    (cl-cc::vm2-reg-set s 0 42)
    (assert-= 42 (cl-cc::vm2-reg-get s 0))))

(deftest vm2-reg-all-256-independent
  "All 256 registers are independent storage slots."
  (let ((s (cl-cc::make-vm2-state)))
    (dotimes (i 256)
      (cl-cc::vm2-reg-set s i i))
    (dotimes (i 256)
      (assert-= i (cl-cc::vm2-reg-get s i)))))

(deftest vm2-reg-set-returns-value
  "vm-reg-set returns the written value."
  (let ((s (cl-cc::make-vm2-state)))
    (assert-= 99 (cl-cc::vm2-reg-set s 5 99))))

(deftest vm2-reg-overwrite
  "vm-reg-set overwrites a previously written value."
  (let ((s (cl-cc::make-vm2-state)))
    (cl-cc::vm2-reg-set s 3 100)
    (cl-cc::vm2-reg-set s 3 200)
    (assert-= 200 (cl-cc::vm2-reg-get s 3))))

;;; ------------------------------------------------------------
;;; make-register allocates unique keyword names
;;; ------------------------------------------------------------

(deftest make-register-returns-keyword
  "make-register returns a keyword symbol :R0, :R1, ... (old-style register names)."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (let ((r0 (cl-cc::make-register ctx))
          (r1 (cl-cc::make-register ctx))
          (r2 (cl-cc::make-register ctx)))
      (assert-true (keywordp r0))
      (assert-true (keywordp r1))
      (assert-true (keywordp r2)))))

(deftest make-register-sequential
  "make-register allocates :R0, :R1, :R2, ... in order."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (assert-eq :r0 (cl-cc::make-register ctx))
    (assert-eq :r1 (cl-cc::make-register ctx))
    (assert-eq :r2 (cl-cc::make-register ctx))))

;;; ------------------------------------------------------------
;;; run-vm — basic programs
;;; ------------------------------------------------------------

(defun make-bytecode (&rest words)
  "Build a simple-vector bytecode from alternating opcode/dst/src1/src2 quads."
  (coerce words 'simple-vector))

(deftest run-vm-const-load
  "OP2-CONST loads an immediate value into a register."
  (let ((s (cl-cc::make-vm2-state)))
    ;; Program: r0 ← 42; halt(r0)
    (let ((code (make-bytecode cl-cc::+op2-const+ 0 42 nil
                               cl-cc::+op2-halt2+ 0 nil nil)))
      (let ((result (cl-cc::run-vm code s)))
        (assert-= 42 result)))))

(deftest run-vm-move
  "OP2-MOVE copies a register value."
  (let ((s (cl-cc::make-vm2-state)))
    ;; r1 ← 7; r0 ← r1; halt(r0)
    (let ((code (make-bytecode cl-cc::+op2-const+ 1 7 nil
                               cl-cc::+op2-move+  0 1 nil
                               cl-cc::+op2-halt2+ 0 nil nil)))
      (assert-= 7 (cl-cc::run-vm code s)))))

(deftest run-vm-add
  "OP2-ADD computes sum of two registers."
  (let ((s (cl-cc::make-vm2-state)))
    ;; r1 ← 3; r2 ← 4; r0 ← r1 + r2; halt
    (let ((code (make-bytecode cl-cc::+op2-const+ 1 3   nil
                               cl-cc::+op2-const+ 2 4   nil
                               cl-cc::+op2-add2+  0 1   2
                               cl-cc::+op2-halt2+ 0 nil nil)))
      (assert-= 7 (cl-cc::run-vm code s)))))

(deftest run-vm-sub
  "OP2-SUB computes difference of two registers."
  (let ((s (cl-cc::make-vm2-state)))
    (let ((code (make-bytecode cl-cc::+op2-const+ 1 10  nil
                               cl-cc::+op2-const+ 2 3   nil
                               cl-cc::+op2-sub2+  0 1   2
                               cl-cc::+op2-halt2+ 0 nil nil)))
      (assert-= 7 (cl-cc::run-vm code s)))))

(deftest run-vm-mul
  "OP2-MUL computes product of two registers."
  (let ((s (cl-cc::make-vm2-state)))
    (let ((code (make-bytecode cl-cc::+op2-const+ 1 6   nil
                               cl-cc::+op2-const+ 2 7   nil
                               cl-cc::+op2-mul2+  0 1   2
                               cl-cc::+op2-halt2+ 0 nil nil)))
      (assert-= 42 (cl-cc::run-vm code s)))))

(deftest run-vm-chained-arithmetic
  "run-vm correctly chains multiple arithmetic operations."
  (let ((s (cl-cc::make-vm2-state)))
    ;; (3 + 4) * 5 = 35: r1=3, r2=4, r3=r1+r2=7, r4=5, r0=r3*r4=35
    (let ((code (make-bytecode cl-cc::+op2-const+ 1 3   nil
                               cl-cc::+op2-const+ 2 4   nil
                               cl-cc::+op2-add2+  3 1   2
                               cl-cc::+op2-const+ 4 5   nil
                               cl-cc::+op2-mul2+  0 3   4
                               cl-cc::+op2-halt2+ 0 nil nil)))
      (assert-= 35 (cl-cc::run-vm code s)))))

(deftest run-vm-const-any-cl-object
  "OP2-CONST can load any CL object as an immediate value."
  (let ((s (cl-cc::make-vm2-state)))
    (let ((code (make-bytecode cl-cc::+op2-const+ 0 :hello nil
                               cl-cc::+op2-halt2+ 0 nil    nil)))
      (assert-equal :hello (cl-cc::run-vm code s)))))

(deftest run-vm-register-independence
  "run-vm leaves non-result registers with their last computed values."
  (let ((s (cl-cc::make-vm2-state)))
    ;; r1=10, r2=20; halt on r0 (which we set to 0 via sub2 of same reg)
    (let ((code (make-bytecode cl-cc::+op2-const+ 1 10  nil
                               cl-cc::+op2-const+ 2 20  nil
                               cl-cc::+op2-const+ 0 99  nil
                               cl-cc::+op2-halt2+ 0 nil nil)))
      (cl-cc::run-vm code s)
      (assert-= 10 (cl-cc::vm2-reg-get s 1))
      (assert-= 20 (cl-cc::vm2-reg-get s 2)))))
