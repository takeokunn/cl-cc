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

(deftest vm2-opcode-registration
  "defopcode: each opcode has a numeric id, dispatch handler, name entry, and encoder mapping."
  (assert-true (numberp cl-cc::+op2-const+))
  (assert-true (not (null (aref cl-cc::*opcode-dispatch-table* cl-cc::+op2-const+))))
  (assert-true (not (null (aref cl-cc::*opcode-dispatch-table* cl-cc::+op2-add2+))))
  (assert-equal 'cl-cc::add2 (aref cl-cc::*opcode-name-table* cl-cc::+op2-add2+))
  (assert-= cl-cc::+op2-const+ (gethash 'cl-cc::const cl-cc::*opcode-encoder-table*))
  (assert-= cl-cc::+op2-move+  (gethash 'cl-cc::move  cl-cc::*opcode-encoder-table*))
  (assert-= cl-cc::+op2-add2+  (gethash 'cl-cc::add2  cl-cc::*opcode-encoder-table*)))

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

(deftest vm2-state-structure
  "make-vm2-state: struct predicate, 256-register vector (all nil), :output-stream kwarg, *features* populated."
  (let ((s (cl-cc::make-vm2-state)))
    (assert-true (cl-cc::vm2-state-p s))
    (assert-true (simple-vector-p (cl-cc::vm2-state-registers s)))
    (assert-= 256 (length (cl-cc::vm2-state-registers s)))
    (dotimes (i 256)
      (assert-true (null (cl-cc::vm2-reg-get s i))))
    (assert-true (not (null (gethash '*features* (cl-cc::vm2-state-global-vars s))))))
  (let* ((str (make-string-output-stream))
         (s   (cl-cc::make-vm2-state :output-stream str)))
    (assert-equal str (cl-cc::vm2-state-output-stream s))))

;;; ------------------------------------------------------------
;;; vm-reg-get / vm-reg-set (integer-indexed)
;;; ------------------------------------------------------------

(deftest vm2-reg-operations
  "vm2-reg-set/get: stores, retrieves, returns value, overwrites; all 256 slots independent."
  (let ((s (cl-cc::make-vm2-state)))
    (cl-cc::vm2-reg-set s 0 42)
    (assert-= 42 (cl-cc::vm2-reg-get s 0))
    ;; set returns the written value
    (assert-= 99 (cl-cc::vm2-reg-set s 5 99))
    ;; overwrite
    (cl-cc::vm2-reg-set s 3 100)
    (cl-cc::vm2-reg-set s 3 200)
    (assert-= 200 (cl-cc::vm2-reg-get s 3))
    ;; all 256 slots are independent
    (dotimes (i 256)
      (cl-cc::vm2-reg-set s i i))
    (dotimes (i 256)
      (assert-= i (cl-cc::vm2-reg-get s i)))))

;;; ------------------------------------------------------------
;;; run-vm — basic programs
;;; ------------------------------------------------------------

(defun make-bytecode (&rest words)
  "Build a simple-vector bytecode from alternating opcode/dst/src1/src2 quads."
  (coerce words 'simple-vector))

(deftest-each run-vm-basic-ops
  "run-vm executes a basic VM2 program and returns the correct halted value."
  :cases (("const-load"
           (make-bytecode cl-cc::+op2-const+ 0 42 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           42)
          ("move"
           (make-bytecode cl-cc::+op2-const+ 1 7   nil
                          cl-cc::+op2-move+  0 1   nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           7)
          ("add"
           (make-bytecode cl-cc::+op2-const+ 1 3   nil
                          cl-cc::+op2-const+ 2 4   nil
                          cl-cc::+op2-add2+  0 1   2
                          cl-cc::+op2-halt2+ 0 nil nil)
           7)
          ("sub"
           (make-bytecode cl-cc::+op2-const+ 1 10  nil
                          cl-cc::+op2-const+ 2 3   nil
                          cl-cc::+op2-sub2+  0 1   2
                          cl-cc::+op2-halt2+ 0 nil nil)
           7)
          ("mul"
           (make-bytecode cl-cc::+op2-const+ 1 6   nil
                          cl-cc::+op2-const+ 2 7   nil
                          cl-cc::+op2-mul2+  0 1   2
                          cl-cc::+op2-halt2+ 0 nil nil)
           42))
  (bytecode expected)
  (let ((s (cl-cc::make-vm2-state)))
    (assert-= expected (cl-cc::run-vm bytecode s))))

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


(deftest vm2-state-globals-init
  "vm2-state pre-populates *active-restarts* (nil) and *standard-output* (non-nil)."
  (let ((s (cl-cc::make-vm2-state)))
    (multiple-value-bind (val found-p)
        (gethash 'cl-cc::*active-restarts* (cl-cc::vm2-state-global-vars s))
      (assert-true found-p)
      (assert-null val))
    (assert-true (not (null (gethash '*standard-output* (cl-cc::vm2-state-global-vars s)))))))

;;; ------------------------------------------------------------
;;; vm2-state output-stream slot
;;; ------------------------------------------------------------

(deftest vm2-state-output-stream-writable
  "vm2-state :output-stream slot can be set to a custom stream."
  (let* ((str (make-string-output-stream))
         (s   (cl-cc::make-vm2-state :output-stream str)))
    (assert-eq str (cl-cc::vm2-state-output-stream s))))

;;; ------------------------------------------------------------
;;; run-vm — edge cases (negatives, zero, large immediate, nil, move chain)
;;; ------------------------------------------------------------

(deftest-each run-vm-edge-cases
  "run-vm handles edge-case operand values correctly."
  :cases (("sub-negative"
           (make-bytecode cl-cc::+op2-const+ 1 3   nil
                          cl-cc::+op2-const+ 2 10  nil
                          cl-cc::+op2-sub2+  0 1   2
                          cl-cc::+op2-halt2+ 0 nil nil)
           -7)
          ("mul-by-zero"
           (make-bytecode cl-cc::+op2-const+ 1 12  nil
                          cl-cc::+op2-const+ 2 0   nil
                          cl-cc::+op2-mul2+  0 1   2
                          cl-cc::+op2-halt2+ 0 nil nil)
           0)
          ("large-immediate"
           (make-bytecode cl-cc::+op2-const+ 0 1000000 nil
                          cl-cc::+op2-halt2+ 0 nil     nil)
           1000000)
          ("nil-immediate"
           (make-bytecode cl-cc::+op2-const+ 0 nil nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           nil)
          ("move-chain"
           (make-bytecode cl-cc::+op2-const+ 1 55  nil
                          cl-cc::+op2-move+  2 1   nil
                          cl-cc::+op2-move+  0 2   nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           55))
  (code expected)
  (let ((s (cl-cc::make-vm2-state)))
    (assert-equal expected (cl-cc::run-vm code s))))

;;; ------------------------------------------------------------
;;; vm2-state — global-vars API through vm-global-vars shim
;;; ------------------------------------------------------------

(deftest vm2-global-vars-shim
  "vm-global-vars shim returns a hash table; vm2-state pre-populates *features*."
  (let ((s (cl-cc::make-vm2-state)))
    (assert-true (hash-table-p (cl-cc::vm-global-vars s)))
    (assert-true (not (null (gethash '*features* (cl-cc::vm-global-vars s)))))))
