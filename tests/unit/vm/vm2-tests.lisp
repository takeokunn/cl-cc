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
;;; make-register allocates unique keyword names
;;; ------------------------------------------------------------

(deftest make-register-allocation
  "make-register allocates :R0, :R1, :R2, ... as keywords in sequential order."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (let ((r0 (cl-cc::make-register ctx))
          (r1 (cl-cc::make-register ctx))
          (r2 (cl-cc::make-register ctx)))
      (assert-true (keywordp r0))
      (assert-true (keywordp r1))
      (assert-true (keywordp r2))
      (assert-eq :r0 r0)
      (assert-eq :r1 r1)
      (assert-eq :r2 r2))))

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

(deftest-each vm-const-stores-value
  "vm-const stores any CL value type into a register."
  :cases (("integer" 42)
          ("string"  "hello")
          ("nil"     nil)
          ("symbol"  'foo)
          ("list"    '(1 2 3)))
  (value)
  (let ((s (make-clos-vm-state)))
    (exec1 (cl-cc::make-vm-const :dst :r0 :value value) s)
    (assert-equal value (cl-cc::vm-reg-get s :r0))))

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

(deftest vm-move-behavior
  "vm-move copies src to dst and leaves src unchanged."
  (let ((s (make-clos-vm-state)))
    (cl-cc::vm-reg-set s :r1 99)
    (exec1 (cl-cc::make-vm-move :dst :r0 :src :r1) s)
    (assert-= 99 (cl-cc::vm-reg-get s :r0))
    (assert-= 99 (cl-cc::vm-reg-get s :r1))))

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

(deftest-each vm-halt-behavior
  "vm-halt sets halt-p to T and returns the register value for any stored value."
  :cases (("integer" 123)
          ("nil"     nil))
  (halt-val)
  (let ((s (make-clos-vm-state)))
    (cl-cc::vm-reg-set s :r0 halt-val)
    (multiple-value-bind (next-pc halt-p result)
        (exec1 (cl-cc::make-vm-halt :reg :r0) s)
      (assert-null next-pc)
      (assert-true halt-p)
      (assert-equal halt-val result))))

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

(deftest-each vm-jump-zero-behavior
  "vm-jump-zero jumps when register is falsy (nil/0), falls through when truthy."
  :cases (("nil-jumps"           nil "skip"        10 10)
          ("zero-jumps"          0   "zero-target"  7  7)
          ("truthy-falls-through" 42 "skip"         10  1))
  (reg-val label table-pc expected-pc)
  (let ((s   (make-clos-vm-state))
        (lbl (make-labels-table label table-pc)))
    (cl-cc::vm-reg-set s :r0 reg-val)
    (multiple-value-bind (next-pc halt-p)
        (cl-cc::execute-instruction
         (cl-cc::make-vm-jump-zero :reg :r0 :label label) s 0 lbl)
      (assert-= expected-pc next-pc)
      (assert-null halt-p))))

;;; ------------------------------------------------------------
;;; vm-set-global / vm-get-global (CLOS)
;;; ------------------------------------------------------------

(deftest vm-globals-operations
  "vm-set-global/vm-get-global: store, retrieve, and round-trip global variables."
  (let ((s (make-clos-vm-state)))
    (cl-cc::vm-reg-set s :r0 42)
    (exec1 (cl-cc::make-vm-set-global :name 'myvar :src :r0) s)
    (assert-= 42 (gethash 'myvar (cl-cc::vm-global-vars s))))
  (let ((s (make-clos-vm-state)))
    (setf (gethash 'myvar2 (cl-cc::vm-global-vars s)) 99)
    (exec1 (cl-cc::make-vm-get-global :dst :r0 :name 'myvar2) s)
    (assert-= 99 (cl-cc::vm-reg-get s :r0)))
  (let ((s (make-clos-vm-state)))
    (cl-cc::vm-reg-set s :r1 "world")
    (exec1 (cl-cc::make-vm-set-global :name 'strvar :src :r1) s)
    (exec1 (cl-cc::make-vm-get-global :dst :r2 :name 'strvar) s)
    (assert-string= "world" (cl-cc::vm-reg-get s :r2))))

(deftest vm2-state-globals-init
  "vm2-state pre-populates *active-restarts* (nil) and *standard-output* (non-nil)."
  (let ((s (cl-cc::make-vm2-state)))
    (multiple-value-bind (val found-p)
        (gethash 'cl-cc::*active-restarts* (cl-cc::vm2-state-global-vars s))
      (assert-true found-p)
      (assert-null val))
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

(deftest vm-mv-bind-behavior
  "vm-mv-bind distributes values-list to registers; pads nil when list is shorter."
  (let ((s (make-clos-vm-state)))
    (setf (cl-cc::vm-values-list s) '(1 2 3))
    (exec1 (cl-cc::make-vm-mv-bind :dst-regs (list :r0 :r1 :r2)) s)
    (assert-= 1 (cl-cc::vm-reg-get s :r0))
    (assert-= 2 (cl-cc::vm-reg-get s :r1))
    (assert-= 3 (cl-cc::vm-reg-get s :r2)))
  (let ((s (make-clos-vm-state)))
    (setf (cl-cc::vm-values-list s) '(42))
    (exec1 (cl-cc::make-vm-mv-bind :dst-regs (list :r0 :r1)) s)
    (assert-= 42 (cl-cc::vm-reg-get s :r0))
    (assert-null (cl-cc::vm-reg-get s :r1))))

(deftest vm-values-list-operations
  "vm-values-to-list, vm-spread-values, and vm-clear-values each manage the values-list slot."
  ;; values-to-list: copies values-list into a register
  (let ((s (make-clos-vm-state)))
    (setf (cl-cc::vm-values-list s) '(7 8 9))
    (exec1 (cl-cc::make-vm-values-to-list :dst :r0) s)
    (assert-equal '(7 8 9) (cl-cc::vm-reg-get s :r0)))
  ;; spread-values: loads values-list from a register holding a list
  (let ((s (make-clos-vm-state)))
    (cl-cc::vm-reg-set s :r1 '(100 200 300))
    (exec1 (cl-cc::make-vm-spread-values :dst :r0 :src :r1) s)
    (assert-= 100 (cl-cc::vm-reg-get s :r0))
    (assert-equal '(100 200 300) (cl-cc::vm-values-list s)))
  ;; clear-values: resets values-list to nil
  (let ((s (make-clos-vm-state)))
    (setf (cl-cc::vm-values-list s) '(1 2 3))
    (exec1 (cl-cc::make-vm-clear-values) s)
    (assert-null (cl-cc::vm-values-list s))))

(deftest vm-ensure-values-behavior
  "vm-ensure-values: initialises from src when nil; no-op when already set."
  (let ((s (make-clos-vm-state)))
    (cl-cc::vm-reg-set s :r0 55)
    (setf (cl-cc::vm-values-list s) nil)
    (exec1 (cl-cc::make-vm-ensure-values :src :r0) s)
    (assert-equal '(55) (cl-cc::vm-values-list s)))
  (let ((s (make-clos-vm-state)))
    (cl-cc::vm-reg-set s :r0 99)
    (setf (cl-cc::vm-values-list s) '(1 2))
    (exec1 (cl-cc::make-vm-ensure-values :src :r0) s)
    (assert-equal '(1 2) (cl-cc::vm-values-list s))))

;;; ------------------------------------------------------------
;;; vm-heap-alloc / vm-heap-get / vm-heap-set
;;; ------------------------------------------------------------

(deftest vm-heap-alloc-operations
  "vm-heap-alloc returns a positive integer; get roundtrips; set overwrites; addresses are unique."
  (let ((s (make-clos-vm-state)))
    (let ((addr (cl-cc::vm-heap-alloc s :some-object)))
      (assert-true (integerp addr))
      (assert-true (> addr 0))))
  (let ((s (make-clos-vm-state)))
    (let ((addr (cl-cc::vm-heap-alloc s "original")))
      (cl-cc::vm-heap-set s addr "replaced")
      (assert-string= "replaced" (cl-cc::vm-heap-get s addr))))
  (let ((s (make-clos-vm-state)))
    (let ((a1 (cl-cc::vm-heap-alloc s 1))
          (a2 (cl-cc::vm-heap-alloc s 2))
          (a3 (cl-cc::vm-heap-alloc s 3)))
      (assert-true (/= a1 a2))
      (assert-true (/= a2 a3))
      (assert-true (/= a1 a3)))))

(deftest-each vm-heap-alloc-roundtrip
  "vm-heap-alloc followed by vm-heap-get retrieves the original object for any value type."
  :cases (("string" "test-payload")
          ("list"   '(a b c)))
  (value)
  (let* ((s    (make-clos-vm-state))
         (addr (cl-cc::vm-heap-alloc s value)))
    (assert-equal value (cl-cc::vm-heap-get s addr))))


;;; ------------------------------------------------------------
;;; vm-closure-object creation and inspection
;;; ------------------------------------------------------------

(deftest vm-closure-object
  "vm-closure-object: entry-label/params/captured-values stored; typep passes."
  ;; basic slots
  (let ((c (make-instance 'cl-cc::vm-closure-object
                           :entry-label "my-fn"
                           :params (list :r1 :r2)
                           :captured-values nil)))
    (assert-string= "my-fn" (cl-cc::vm-closure-entry-label c))
    (assert-equal (list :r1 :r2) (cl-cc::vm-closure-params c))
    (assert-null (cl-cc::vm-closure-captured-values c))
    (assert-true (typep c 'cl-cc::vm-closure-object)))
  ;; captured variables
  (let ((c (make-instance 'cl-cc::vm-closure-object
                           :entry-label "adder"
                           :params (list :r1)
                           :captured-values (list (cons :r0 10)))))
    (assert-equal (list (cons :r0 10)) (cl-cc::vm-closure-captured-values c))))

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

(deftest vm-cons-cell
  "vm-cons-cell: stores car/cdr, car is setf-able, subtype of vm-heap-object."
  (let ((cell (make-instance 'cl-cc::vm-cons-cell :car 1 :cdr 2)))
    (assert-= 1 (cl-cc::vm-cons-cell-car cell))
    (assert-= 2 (cl-cc::vm-cons-cell-cdr cell))
    (setf (cl-cc::vm-cons-cell-car cell) 99)
    (assert-= 99 (cl-cc::vm-cons-cell-car cell))
    (assert-true (typep cell 'cl-cc::vm-heap-object))))

;;; ------------------------------------------------------------
;;; vm-heap-address wrapper
;;; ------------------------------------------------------------

(deftest vm-heap-address-struct
  "make-vm-heap-address creates a struct with correct value; predicate recognizes it."
  (let ((ha (cl-cc::make-vm-heap-address :value 42)))
    (assert-= 42 (cl-cc::vm-heap-address-value ha))
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

(deftest-each vm-state-registries-are-hash-tables
  "vm-state class-registry and function-registry are both hash tables."
  :cases (("class-registry"    #'cl-cc::vm-class-registry)
          ("function-registry" #'cl-cc::vm-function-registry))
  (accessor)
  (let ((s (make-clos-vm-state)))
    (assert-true (hash-table-p (funcall accessor s)))))

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

;;; ─────────────────────────────────────────────────────────────────────────
;;; rt-plist-put
;;; ─────────────────────────────────────────────────────────────────────────

(deftest rt-plist-put
  "rt-plist-put: add new key, replace existing, preserve others, non-destructive."
  ;; add new key
  (assert-equal 42 (getf (cl-cc::rt-plist-put nil :foo 42) :foo))
  ;; replace existing key, leave others
  (let ((result (cl-cc::rt-plist-put '(:foo 1 :bar 2) :foo 99)))
    (assert-equal 99 (getf result :foo))
    (assert-equal 2  (getf result :bar)))
  ;; multiple unrelated keys preserved
  (let ((result (cl-cc::rt-plist-put '(:a 1 :b 2 :c 3) :b 20)))
    (assert-equal 1  (getf result :a))
    (assert-equal 20 (getf result :b))
    (assert-equal 3  (getf result :c)))
  ;; non-destructive
  (let ((orig '(:x 10)))
    (cl-cc::rt-plist-put orig :x 99)
    (assert-equal 10 (getf orig :x))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; vm-list-to-lisp-list
;;; ─────────────────────────────────────────────────────────────────────────

(deftest vm-list-to-lisp-list
  "vm-list-to-lisp-list: nil→nil, native list passthrough, vm-cons chain, atom wrapped."
  ;; nil
  (assert-equal nil (cl-cc::vm-list-to-lisp-list nil nil))
  ;; native CL list passthrough
  (let ((lst '(1 2 3)))
    (assert-equal lst (cl-cc::vm-list-to-lisp-list nil lst)))
  ;; vm-cons-cell chain
  (let* ((tail (make-instance 'cl-cc::vm-cons-cell :car 2 :cdr nil))
         (head (make-instance 'cl-cc::vm-cons-cell :car 1 :cdr tail)))
    (assert-equal '(1 2) (cl-cc::vm-list-to-lisp-list nil head)))
  ;; atom wrapped in list
  (assert-equal '(some-atom) (cl-cc::vm-list-to-lisp-list nil 'some-atom)))
