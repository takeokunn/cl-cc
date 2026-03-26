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

;;; ------------------------------------------------------------
;;; vm-state (CLOS) — execute-instruction tests
;;; ------------------------------------------------------------
;;;
;;; The CLOS vm-state uses keyword-named registers (:R0, :R1, ...)
;;; and the execute-instruction generic function pipeline.
;;; We use make-instance 'cl-cc::vm-state directly.
;;;

(defun make-clos-vm-state ()
  "Create a legacy CLOS vm-state for execute-instruction tests."
  (make-instance 'cl-cc::vm-state))

(defun make-labels-table (&rest pairs)
  "Build a label->pc hash table from alternating label/pc pairs."
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (label pc) on pairs by #'cddr
          do (setf (gethash label ht) pc))
    ht))

(defun exec1 (inst state &optional (labels (make-hash-table :test #'equal)))
  "Execute a single instruction against STATE at pc=0. Returns (values next-pc halt-p result)."
  (cl-cc::execute-instruction inst state 0 labels))

;;; ------------------------------------------------------------
;;; vm-const (CLOS)
;;; ------------------------------------------------------------

(deftest vm-const-stores-integer
  "vm-const stores an integer into a register."
  (let ((s (make-clos-vm-state)))
    (exec1 (cl-cc::make-vm-const :dst :r0 :value 42) s)
    (assert-= 42 (cl-cc::vm-reg-get s :r0))))

(deftest vm-const-stores-string
  "vm-const stores a string into a register."
  (let ((s (make-clos-vm-state)))
    (exec1 (cl-cc::make-vm-const :dst :r1 :value "hello") s)
    (assert-string= "hello" (cl-cc::vm-reg-get s :r1))))

(deftest vm-const-stores-nil
  "vm-const stores nil into a register."
  (let ((s (make-clos-vm-state)))
    (exec1 (cl-cc::make-vm-const :dst :r2 :value nil) s)
    (assert-null (cl-cc::vm-reg-get s :r2))))

(deftest vm-const-stores-symbol
  "vm-const stores a symbol into a register."
  (let ((s (make-clos-vm-state)))
    (exec1 (cl-cc::make-vm-const :dst :r3 :value 'foo) s)
    (assert-eq 'foo (cl-cc::vm-reg-get s :r3))))

(deftest vm-const-stores-list
  "vm-const stores a list into a register."
  (let ((s (make-clos-vm-state)))
    (exec1 (cl-cc::make-vm-const :dst :r4 :value '(1 2 3)) s)
    (assert-equal '(1 2 3) (cl-cc::vm-reg-get s :r4))))

(deftest vm-const-advances-pc
  "vm-const returns next-pc = 1."
  (let ((s (make-clos-vm-state)))
    (multiple-value-bind (next-pc halt-p)
        (exec1 (cl-cc::make-vm-const :dst :r0 :value 0) s)
      (assert-= 1 next-pc)
      (assert-null halt-p))))

;;; ------------------------------------------------------------
;;; vm-move (CLOS)
;;; ------------------------------------------------------------

(deftest vm-move-copies-register
  "vm-move copies the value of src to dst."
  (let ((s (make-clos-vm-state)))
    (cl-cc::vm-reg-set s :r1 99)
    (exec1 (cl-cc::make-vm-move :dst :r0 :src :r1) s)
    (assert-= 99 (cl-cc::vm-reg-get s :r0))))

(deftest vm-move-does-not-clobber-src
  "vm-move leaves the source register unchanged."
  (let ((s (make-clos-vm-state)))
    (cl-cc::vm-reg-set s :r1 77)
    (exec1 (cl-cc::make-vm-move :dst :r0 :src :r1) s)
    (assert-= 77 (cl-cc::vm-reg-get s :r1))))

;;; ------------------------------------------------------------
;;; vm-add / vm-sub / vm-mul (CLOS)
;;; ------------------------------------------------------------

(deftest-each vm-binop-basic
  "Basic binary arithmetic instructions produce correct results."
  :cases (("add-3+4"   (cl-cc::make-vm-add :dst :r0 :lhs :r1 :rhs :r2) 3 4 7)
          ("sub-10-3"  (cl-cc::make-vm-sub :dst :r0 :lhs :r1 :rhs :r2) 10 3 7)
          ("mul-6*7"   (cl-cc::make-vm-mul :dst :r0 :lhs :r1 :rhs :r2) 6 7 42)
          ("add-neg"   (cl-cc::make-vm-add :dst :r0 :lhs :r1 :rhs :r2) -5 3 -2)
          ("sub-neg"   (cl-cc::make-vm-sub :dst :r0 :lhs :r1 :rhs :r2) 0 10 -10))
  (inst lhs-val rhs-val expected)
  (let ((s (make-clos-vm-state)))
    (cl-cc::vm-reg-set s :r1 lhs-val)
    (cl-cc::vm-reg-set s :r2 rhs-val)
    (exec1 inst s)
    (assert-= expected (cl-cc::vm-reg-get s :r0))))

;;; ------------------------------------------------------------
;;; vm-halt (CLOS)
;;; ------------------------------------------------------------

(deftest vm-halt-signals-halt
  "vm-halt sets halt-p to T and returns the register value."
  (let ((s (make-clos-vm-state)))
    (cl-cc::vm-reg-set s :r0 123)
    (multiple-value-bind (next-pc halt-p result)
        (exec1 (cl-cc::make-vm-halt :reg :r0) s)
      (assert-null next-pc)
      (assert-true halt-p)
      (assert-= 123 result))))

(deftest vm-halt-with-nil
  "vm-halt returns nil when the halting register contains nil."
  (let ((s (make-clos-vm-state)))
    (cl-cc::vm-reg-set s :r0 nil)
    (multiple-value-bind (next-pc halt-p result)
        (exec1 (cl-cc::make-vm-halt :reg :r0) s)
      (assert-null next-pc)
      (assert-true halt-p)
      (assert-null result))))

;;; ------------------------------------------------------------
;;; vm-jump / vm-jump-zero (CLOS)
;;; ------------------------------------------------------------

(deftest vm-jump-goes-to-label
  "vm-jump transfers control to the specified label's pc."
  (let ((s   (make-clos-vm-state))
        (lbl (make-labels-table "entry" 5)))
    (multiple-value-bind (next-pc halt-p)
        (cl-cc::execute-instruction
         (cl-cc::make-vm-jump :label "entry") s 0 lbl)
      (assert-= 5 next-pc)
      (assert-null halt-p))))

(deftest vm-jump-zero-taken-on-nil
  "vm-jump-zero jumps when register is nil (falsy)."
  (let ((s   (make-clos-vm-state))
        (lbl (make-labels-table "skip" 10)))
    (cl-cc::vm-reg-set s :r0 nil)
    (multiple-value-bind (next-pc halt-p)
        (cl-cc::execute-instruction
         (cl-cc::make-vm-jump-zero :reg :r0 :label "skip") s 0 lbl)
      (assert-= 10 next-pc)
      (assert-null halt-p))))

(deftest vm-jump-zero-not-taken-on-truthy
  "vm-jump-zero falls through when register is truthy."
  (let ((s   (make-clos-vm-state))
        (lbl (make-labels-table "skip" 10)))
    (cl-cc::vm-reg-set s :r0 42)
    (multiple-value-bind (next-pc halt-p)
        (cl-cc::execute-instruction
         (cl-cc::make-vm-jump-zero :reg :r0 :label "skip") s 0 lbl)
      (assert-= 1 next-pc)
      (assert-null halt-p))))

(deftest vm-jump-zero-taken-on-zero
  "vm-jump-zero also jumps when register value is 0."
  (let ((s   (make-clos-vm-state))
        (lbl (make-labels-table "zero-target" 7)))
    (cl-cc::vm-reg-set s :r0 0)
    (multiple-value-bind (next-pc)
        (cl-cc::execute-instruction
         (cl-cc::make-vm-jump-zero :reg :r0 :label "zero-target") s 0 lbl)
      (assert-= 7 next-pc))))

;;; ------------------------------------------------------------
;;; vm-set-global / vm-get-global (CLOS)
;;; ------------------------------------------------------------

(deftest vm-set-global-stores-value
  "vm-set-global stores a value in the global-vars table."
  (let ((s (make-clos-vm-state)))
    (cl-cc::vm-reg-set s :r0 42)
    (exec1 (cl-cc::make-vm-set-global :name 'myvar :src :r0) s)
    (assert-= 42 (gethash 'myvar (cl-cc::vm-global-vars s)))))

(deftest vm-get-global-retrieves-value
  "vm-get-global loads a previously stored global into a register."
  (let ((s (make-clos-vm-state)))
    (setf (gethash 'myvar2 (cl-cc::vm-global-vars s)) 99)
    (exec1 (cl-cc::make-vm-get-global :dst :r0 :name 'myvar2) s)
    (assert-= 99 (cl-cc::vm-reg-get s :r0))))

(deftest vm-globals-roundtrip
  "set-global then get-global round-trips a string value."
  (let ((s (make-clos-vm-state)))
    (cl-cc::vm-reg-set s :r1 "world")
    (exec1 (cl-cc::make-vm-set-global :name 'strvar :src :r1) s)
    (exec1 (cl-cc::make-vm-get-global :dst :r2 :name 'strvar) s)
    (assert-string= "world" (cl-cc::vm-reg-get s :r2))))

(deftest vm2-state-globals-active-restarts
  "vm2-state pre-populates *active-restarts* to nil."
  (let ((s (cl-cc::make-vm2-state)))
    (multiple-value-bind (val found-p)
        (gethash 'cl-cc::*active-restarts* (cl-cc::vm2-state-global-vars s))
      (assert-true found-p)
      (assert-null val))))

(deftest vm2-state-globals-standard-output
  "vm2-state pre-populates *standard-output* in global-vars."
  (let ((s (cl-cc::make-vm2-state)))
    (assert-true (not (null (gethash '*standard-output* (cl-cc::vm2-state-global-vars s)))))))

;;; ------------------------------------------------------------
;;; vm-values / vm-mv-bind / vm-values-to-list / vm-spread-values
;;; ------------------------------------------------------------

(deftest vm-values-sets-primary-and-list
  "vm-values stores primary value in dst and all values in values-list."
  (let ((s (make-clos-vm-state)))
    (cl-cc::vm-reg-set s :r1 10)
    (cl-cc::vm-reg-set s :r2 20)
    (cl-cc::vm-reg-set s :r3 30)
    (exec1 (cl-cc::make-vm-values :dst :r0 :src-regs (list :r1 :r2 :r3)) s)
    (assert-= 10 (cl-cc::vm-reg-get s :r0))
    (assert-equal '(10 20 30) (cl-cc::vm-values-list s))))

(deftest vm-mv-bind-distributes-values
  "vm-mv-bind distributes values-list into multiple destination registers."
  (let ((s (make-clos-vm-state)))
    (setf (cl-cc::vm-values-list s) '(1 2 3))
    (exec1 (cl-cc::make-vm-mv-bind :dst-regs (list :r0 :r1 :r2)) s)
    (assert-= 1 (cl-cc::vm-reg-get s :r0))
    (assert-= 2 (cl-cc::vm-reg-get s :r1))
    (assert-= 3 (cl-cc::vm-reg-get s :r2))))

(deftest vm-mv-bind-pads-nil-for-missing-values
  "vm-mv-bind pads nil when values-list is shorter than dst-regs."
  (let ((s (make-clos-vm-state)))
    (setf (cl-cc::vm-values-list s) '(42))
    (exec1 (cl-cc::make-vm-mv-bind :dst-regs (list :r0 :r1)) s)
    (assert-= 42 (cl-cc::vm-reg-get s :r0))
    (assert-null (cl-cc::vm-reg-get s :r1))))

(deftest vm-values-to-list-copies-values-list
  "vm-values-to-list copies vm-values-list into a register."
  (let ((s (make-clos-vm-state)))
    (setf (cl-cc::vm-values-list s) '(7 8 9))
    (exec1 (cl-cc::make-vm-values-to-list :dst :r0) s)
    (assert-equal '(7 8 9) (cl-cc::vm-reg-get s :r0))))

(deftest vm-spread-values-from-list
  "vm-spread-values sets values-list from a register holding a list."
  (let ((s (make-clos-vm-state)))
    (cl-cc::vm-reg-set s :r1 '(100 200 300))
    (exec1 (cl-cc::make-vm-spread-values :dst :r0 :src :r1) s)
    (assert-= 100 (cl-cc::vm-reg-get s :r0))
    (assert-equal '(100 200 300) (cl-cc::vm-values-list s))))

(deftest vm-clear-values-resets-list
  "vm-clear-values sets vm-values-list to nil."
  (let ((s (make-clos-vm-state)))
    (setf (cl-cc::vm-values-list s) '(1 2 3))
    (exec1 (cl-cc::make-vm-clear-values) s)
    (assert-null (cl-cc::vm-values-list s))))

(deftest vm-ensure-values-sets-list-when-nil
  "vm-ensure-values initialises values-list from src when list is nil."
  (let ((s (make-clos-vm-state)))
    (cl-cc::vm-reg-set s :r0 55)
    (setf (cl-cc::vm-values-list s) nil)
    (exec1 (cl-cc::make-vm-ensure-values :src :r0) s)
    (assert-equal '(55) (cl-cc::vm-values-list s))))

(deftest vm-ensure-values-noop-when-already-set
  "vm-ensure-values does not overwrite an existing values-list."
  (let ((s (make-clos-vm-state)))
    (cl-cc::vm-reg-set s :r0 99)
    (setf (cl-cc::vm-values-list s) '(1 2))
    (exec1 (cl-cc::make-vm-ensure-values :src :r0) s)
    (assert-equal '(1 2) (cl-cc::vm-values-list s))))

;;; ------------------------------------------------------------
;;; vm-heap-alloc / vm-heap-get / vm-heap-set
;;; ------------------------------------------------------------

(deftest vm-heap-alloc-returns-integer
  "vm-heap-alloc returns a positive integer address."
  (let ((s (make-clos-vm-state)))
    (let ((addr (cl-cc::vm-heap-alloc s :some-object)))
      (assert-true (integerp addr))
      (assert-true (> addr 0)))))

(deftest vm-heap-alloc-and-get-roundtrip
  "vm-heap-alloc followed by vm-heap-get retrieves the original object."
  (let ((s (make-clos-vm-state)))
    (let ((addr (cl-cc::vm-heap-alloc s "test-payload")))
      (assert-string= "test-payload" (cl-cc::vm-heap-get s addr)))))

(deftest vm-heap-set-overwrites-object
  "vm-heap-set replaces an object at an existing address."
  (let ((s (make-clos-vm-state)))
    (let ((addr (cl-cc::vm-heap-alloc s "original")))
      (cl-cc::vm-heap-set s addr "replaced")
      (assert-string= "replaced" (cl-cc::vm-heap-get s addr)))))

(deftest vm-heap-alloc-multiple-addresses-unique
  "Multiple vm-heap-alloc calls return distinct addresses."
  (let ((s (make-clos-vm-state)))
    (let ((a1 (cl-cc::vm-heap-alloc s 1))
          (a2 (cl-cc::vm-heap-alloc s 2))
          (a3 (cl-cc::vm-heap-alloc s 3)))
      (assert-true (/= a1 a2))
      (assert-true (/= a2 a3))
      (assert-true (/= a1 a3)))))

(deftest vm-heap-alloc-stores-list-object
  "vm-heap-alloc can store arbitrary CL objects including lists."
  (let ((s (make-clos-vm-state)))
    (let ((addr (cl-cc::vm-heap-alloc s '(a b c))))
      (assert-equal '(a b c) (cl-cc::vm-heap-get s addr)))))

;;; ------------------------------------------------------------
;;; vm-closure-object creation and inspection
;;; ------------------------------------------------------------

(deftest vm-closure-object-creation
  "make-instance vm-closure-object stores entry-label and params."
  (let ((c (make-instance 'cl-cc::vm-closure-object
                           :entry-label "my-fn"
                           :params (list :r1 :r2)
                           :captured-values nil)))
    (assert-string= "my-fn" (cl-cc::vm-closure-entry-label c))
    (assert-equal (list :r1 :r2) (cl-cc::vm-closure-params c))
    (assert-null (cl-cc::vm-closure-captured-values c))))

(deftest vm-closure-object-with-captures
  "vm-closure-object stores captured variable values."
  (let ((c (make-instance 'cl-cc::vm-closure-object
                           :entry-label "adder"
                           :params (list :r1)
                           :captured-values (list (cons :r0 10)))))
    (assert-equal (list (cons :r0 10)) (cl-cc::vm-closure-captured-values c))))

(deftest vm-closure-object-type-predicate
  "vm-closure-object passes typep check."
  (let ((c (make-instance 'cl-cc::vm-closure-object
                           :entry-label "fn"
                           :params nil
                           :captured-values nil)))
    (assert-true (typep c 'cl-cc::vm-closure-object))))

;;; ------------------------------------------------------------
;;; vm-register-function / vm-function-registry
;;; ------------------------------------------------------------

(deftest vm-register-function-stores-closure
  "vm-register-function stores a closure in the function registry."
  (let ((s (make-clos-vm-state)))
    (let ((closure (make-instance 'cl-cc::vm-closure-object
                                   :entry-label "myfn"
                                   :params nil
                                   :captured-values nil)))
      (cl-cc::vm-reg-set s :r0 closure)
      (exec1 (cl-cc::make-vm-register-function :name 'myfn :src :r0) s)
      (assert-true (not (null (gethash 'myfn (cl-cc::vm-function-registry s))))))))

;;; ------------------------------------------------------------
;;; vm-print (CLOS) — output stream side-effect
;;; ------------------------------------------------------------

(deftest vm-print-writes-to-output-stream
  "vm-print writes the register value followed by newline to output-stream."
  (let* ((str (make-string-output-stream))
         (s   (make-instance 'cl-cc::vm-state :output-stream str)))
    (cl-cc::vm-reg-set s :r0 42)
    (exec1 (cl-cc::make-vm-print :reg :r0) s)
    (assert-string= (format nil "42~%") (get-output-stream-string str))))

;;; ------------------------------------------------------------
;;; vm2-state output-stream slot
;;; ------------------------------------------------------------

(deftest vm2-state-output-stream-writable
  "vm2-state :output-stream slot can be set to a custom stream."
  (let* ((str (make-string-output-stream))
         (s   (cl-cc::make-vm2-state :output-stream str)))
    (assert-eq str (cl-cc::vm2-state-output-stream s))))

;;; ------------------------------------------------------------
;;; run-vm — sub instruction (regression for negatives)
;;; ------------------------------------------------------------

(deftest run-vm-sub-produces-negative
  "run-vm sub2 correctly produces a negative result."
  (let ((s (cl-cc::make-vm2-state)))
    (let ((code (make-bytecode cl-cc::+op2-const+ 1 3   nil
                               cl-cc::+op2-const+ 2 10  nil
                               cl-cc::+op2-sub2+  0 1   2
                               cl-cc::+op2-halt2+ 0 nil nil)))
      (assert-= -7 (cl-cc::run-vm code s)))))

(deftest run-vm-mul-by-zero
  "run-vm mul2 with zero produces zero."
  (let ((s (cl-cc::make-vm2-state)))
    (let ((code (make-bytecode cl-cc::+op2-const+ 1 12  nil
                               cl-cc::+op2-const+ 2 0   nil
                               cl-cc::+op2-mul2+  0 1   2
                               cl-cc::+op2-halt2+ 0 nil nil)))
      (assert-= 0 (cl-cc::run-vm code s)))))

(deftest run-vm-large-immediate
  "run-vm const can hold a large immediate value."
  (let ((s (cl-cc::make-vm2-state)))
    (let ((code (make-bytecode cl-cc::+op2-const+ 0 1000000 nil
                               cl-cc::+op2-halt2+ 0 nil     nil)))
      (assert-= 1000000 (cl-cc::run-vm code s)))))

(deftest run-vm-nil-immediate
  "run-vm const can hold nil as an immediate."
  (let ((s (cl-cc::make-vm2-state)))
    (let ((code (make-bytecode cl-cc::+op2-const+ 0 nil nil
                               cl-cc::+op2-halt2+ 0 nil nil)))
      (assert-null (cl-cc::run-vm code s)))))

(deftest run-vm-move-chain
  "run-vm move can chain: r1 → r2 → r0."
  (let ((s (cl-cc::make-vm2-state)))
    (let ((code (make-bytecode cl-cc::+op2-const+ 1 55  nil
                               cl-cc::+op2-move+  2 1   nil
                               cl-cc::+op2-move+  0 2   nil
                               cl-cc::+op2-halt2+ 0 nil nil)))
      (assert-= 55 (cl-cc::run-vm code s)))))

;;; ------------------------------------------------------------
;;; vm2-state — global-vars API through vm-global-vars shim
;;; ------------------------------------------------------------

(deftest vm2-global-vars-shim
  "vm-global-vars shim returns the global-vars hash table of a vm2-state."
  (let ((s (cl-cc::make-vm2-state)))
    (assert-true (hash-table-p (cl-cc::vm-global-vars s)))))

(deftest vm2-global-vars-features-populated
  "vm2-state global-vars contains *features* key."
  (let ((s (cl-cc::make-vm2-state)))
    (assert-true (not (null (gethash '*features* (cl-cc::vm-global-vars s)))))))

;;; ------------------------------------------------------------
;;; vm-label (CLOS) — passes through to next pc
;;; ------------------------------------------------------------

(deftest vm-label-advances-pc
  "vm-label is a no-op instruction that just increments pc."
  (let ((s (make-clos-vm-state)))
    (multiple-value-bind (next-pc halt-p)
        (exec1 (cl-cc::make-vm-label :name "entry") s)
      (assert-= 1 next-pc)
      (assert-null halt-p))))

;;; ------------------------------------------------------------
;;; vm-cons-cell construction and accessors
;;; ------------------------------------------------------------

(deftest vm-cons-cell-creation
  "make-instance vm-cons-cell stores car and cdr."
  (let ((cell (make-instance 'cl-cc::vm-cons-cell :car 1 :cdr 2)))
    (assert-= 1 (cl-cc::vm-cons-cell-car cell))
    (assert-= 2 (cl-cc::vm-cons-cell-cdr cell))))

(deftest vm-cons-cell-setf-car
  "vm-cons-cell-car is setf-able."
  (let ((cell (make-instance 'cl-cc::vm-cons-cell :car 1 :cdr 2)))
    (setf (cl-cc::vm-cons-cell-car cell) 99)
    (assert-= 99 (cl-cc::vm-cons-cell-car cell))))

(deftest vm-cons-cell-type
  "vm-cons-cell is a subtype of vm-heap-object."
  (let ((cell (make-instance 'cl-cc::vm-cons-cell :car nil :cdr nil)))
    (assert-true (typep cell 'cl-cc::vm-heap-object))))

;;; ------------------------------------------------------------
;;; vm-heap-address wrapper
;;; ------------------------------------------------------------

(deftest vm-heap-address-struct
  "make-vm-heap-address creates a struct with the given value."
  (let ((ha (cl-cc::make-vm-heap-address :value 42)))
    (assert-= 42 (cl-cc::vm-heap-address-value ha))))

(deftest vm-heap-address-predicate
  "vm-heap-address-p recognises vm-heap-address structs."
  (let ((ha (cl-cc::make-vm-heap-address :value 0)))
    (assert-true (cl-cc::vm-heap-address-p ha))))

;;; ------------------------------------------------------------
;;; vm-reg-get default value (CLOS vm-state)
;;; ------------------------------------------------------------

(deftest vm-reg-get-unset-returns-zero
  "vm-reg-get on the CLOS vm-state returns 0 for an unset register."
  (let ((s (make-clos-vm-state)))
    (assert-= 0 (cl-cc::vm-reg-get s :unset-reg))))

;;; ------------------------------------------------------------
;;; vm-state class-registry
;;; ------------------------------------------------------------

(deftest vm-state-class-registry-is-hash-table
  "vm-state class-registry is a hash table."
  (let ((s (make-clos-vm-state)))
    (assert-true (hash-table-p (cl-cc::vm-class-registry s)))))

;;; ------------------------------------------------------------
;;; vm-state function-registry
;;; ------------------------------------------------------------

(deftest vm-state-function-registry-is-hash-table
  "vm-state function-registry is a hash table."
  (let ((s (make-clos-vm-state)))
    (assert-true (hash-table-p (cl-cc::vm-function-registry s)))))

;;; ------------------------------------------------------------
;;; vm-falsep predicate
;;; ------------------------------------------------------------

(deftest-each vm-falsep-cases
  "vm-falsep returns T for nil and 0, NIL for other values."
  :cases (("nil-is-false"  nil  t)
          ("zero-is-false" 0    t)
          ("one-is-true"   1    nil)
          ("string-true"   "x"  nil)
          ("t-is-true"     t    nil)
          ("neg-true"      -1   nil))
  (input expected-false-p)
  (if expected-false-p
      (assert-true  (cl-cc::vm-falsep input))
      (assert-null  (cl-cc::vm-falsep input))))
