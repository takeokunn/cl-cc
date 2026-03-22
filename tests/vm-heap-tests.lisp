;;;; tests/vm-heap-tests.lisp - VM Heap Operations Tests
;;;
;;; This module provides tests for VM heap operations including:
;;; - Cons cell allocation and access
;;; - Car/cdr operations
;;; - Rplaca/rplacd mutation
;;; - Closure creation and access

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; Heap Allocation Tests

(test vm-alloc-basic
  "Test basic heap allocation."
  (let* ((state (make-instance 'vm-state))
         (addr (vm-heap-alloc state nil)))
    (is (= addr 1))
    (is (= (vm-heap-counter state) 1))))

(test vm-alloc-multiple
  "Test multiple heap allocations produce unique addresses."
  (let* ((state (make-instance 'vm-state))
         (addr1 (vm-heap-alloc state nil))
         (addr2 (vm-heap-alloc state nil))
         (addr3 (vm-heap-alloc state nil)))
    (is (/= addr1 addr2))
    (is (/= addr2 addr3))
    (is (= (vm-heap-counter state) 3))))

(test vm-heap-get-and-set
  "Test heap get and set operations."
  (let* ((state (make-instance 'vm-state))
         (addr (incf (vm-heap-counter state))))
    (vm-heap-set state addr :test-value)
    (is (eq (vm-heap-get state addr) :test-value))))

;;; Cons Cell Tests

(test vm-cons-creates-cell
  "Test that vm-cons creates a cons cell."
  (let* ((state (make-instance 'vm-state))
         (inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2)))
    (vm-reg-set state :r1 10)
    (vm-reg-set state :r2 20)
    (execute-instruction inst state 0 (make-hash-table))
    (let ((cell (vm-reg-get state :r0)))
      (is (consp cell))
      (is (= (car cell) 10))
      (is (= (cdr cell) 20)))))

(test vm-cons-with-nil-cdr
  "Test that vm-cons works with nil cdr."
  (let* ((state (make-instance 'vm-state))
         (inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2)))
    (vm-reg-set state :r1 42)
    (vm-reg-set state :r2 nil)
    (execute-instruction inst state 0 (make-hash-table))
    (let ((cell (vm-reg-get state :r0)))
      (is (consp cell))
      (is (= (car cell) 42))
      (is (null (cdr cell))))))

(test vm-cons-nested
  "Test that nested cons cells work correctly."
  (let* ((state (make-instance 'vm-state))
         ;; Create first cons: (2 . nil)
         (inst1 (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         ;; Create second cons: (1 . <addr of first cons>)
         (inst2 (make-vm-cons :dst :r3 :car-src :r4 :cdr-src :r0)))
    (vm-reg-set state :r1 2)
    (vm-reg-set state :r2 nil)
    (execute-instruction inst1 state 0 (make-hash-table))
    (vm-reg-set state :r4 1)
    (execute-instruction inst2 state 1 (make-hash-table))
    ;; Verify structure
    (let* ((outer-cell (vm-reg-get state :r3))
           (inner-cell (cdr outer-cell)))
      (is (= (car outer-cell) 1))
      (is (= (car inner-cell) 2))
      (is (null (cdr inner-cell))))))

;;; Car/Cdr Tests

(test vm-car-extracts-value
  "Test that vm-car extracts the car of a cons cell."
  (let* ((state (make-instance 'vm-state))
         (cons-inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (car-inst (make-vm-car :dst :r3 :src :r0)))
    (vm-reg-set state :r1 123)
    (vm-reg-set state :r2 456)
    (execute-instruction cons-inst state 0 (make-hash-table))
    (execute-instruction car-inst state 1 (make-hash-table))
    (is (= (vm-reg-get state :r3) 123))))

(test vm-cdr-extracts-value
  "Test that vm-cdr extracts the cdr of a cons cell."
  (let* ((state (make-instance 'vm-state))
         (cons-inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (cdr-inst (make-vm-cdr :dst :r3 :src :r0)))
    (vm-reg-set state :r1 123)
    (vm-reg-set state :r2 456)
    (execute-instruction cons-inst state 0 (make-hash-table))
    (execute-instruction cdr-inst state 1 (make-hash-table))
    (is (= (vm-reg-get state :r3) 456))))

(test vm-car-on-nested-cons
  "Test car on nested cons cells."
  (let* ((state (make-instance 'vm-state))
         ;; Create (2 . 3)
         (inst1 (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         ;; Create ((2 . 3) . 4)
         (inst2 (make-vm-cons :dst :r3 :car-src :r0 :cdr-src :r4))
         ;; Get car of outer cons (should be address of inner cons)
         (car-inst (make-vm-car :dst :r5 :src :r3))
         ;; Get car of inner cons (should be 2)
         (car-inst2 (make-vm-car :dst :r6 :src :r5)))
    (vm-reg-set state :r1 2)
    (vm-reg-set state :r2 3)
    (vm-reg-set state :r4 4)
    (execute-instruction inst1 state 0 (make-hash-table))
    (execute-instruction inst2 state 1 (make-hash-table))
    (execute-instruction car-inst state 2 (make-hash-table))
    (execute-instruction car-inst2 state 3 (make-hash-table))
    (is (= (vm-reg-get state :r6) 2))))

;;; Rplaca/Rplacd Tests

(test vm-rplaca-modifies-car
  "Test that vm-rplaca modifies the car of a cons cell."
  (let* ((state (make-instance 'vm-state))
         (cons-inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (rplaca-inst (make-vm-rplaca :cons :r0 :val :r3)))
    (vm-reg-set state :r1 10)
    (vm-reg-set state :r2 20)
    (execute-instruction cons-inst state 0 (make-hash-table))
    ;; Verify initial value
    (let ((cell (vm-reg-get state :r0)))
      (is (= (car cell) 10)))
    ;; Modify car
    (vm-reg-set state :r3 99)
    (execute-instruction rplaca-inst state 1 (make-hash-table))
    ;; Verify modification
    (let ((cell (vm-reg-get state :r0)))
      (is (= (car cell) 99))
      (is (= (cdr cell) 20)))))

(test vm-rplacd-modifies-cdr
  "Test that vm-rplacd modifies the cdr of a cons cell."
  (let* ((state (make-instance 'vm-state))
         (cons-inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (rplacd-inst (make-vm-rplacd :cons :r0 :val :r3)))
    (vm-reg-set state :r1 10)
    (vm-reg-set state :r2 20)
    (execute-instruction cons-inst state 0 (make-hash-table))
    ;; Verify initial value
    (let ((cell (vm-reg-get state :r0)))
      (is (= (cdr cell) 20)))
    ;; Modify cdr
    (vm-reg-set state :r3 88)
    (execute-instruction rplacd-inst state 1 (make-hash-table))
    ;; Verify modification
    (let ((cell (vm-reg-get state :r0)))
      (is (= (car cell) 10))
      (is (= (cdr cell) 88)))))

(test vm-rplaca-and-rplacd-together
  "Test that rplaca and rplacd can both be used on the same cons."
  (let* ((state (make-instance 'vm-state))
         (cons-inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (rplaca-inst (make-vm-rplaca :cons :r0 :val :r3))
         (rplacd-inst (make-vm-rplacd :cons :r0 :val :r4)))
    (vm-reg-set state :r1 1)
    (vm-reg-set state :r2 2)
    (execute-instruction cons-inst state 0 (make-hash-table))
    (vm-reg-set state :r3 100)
    (execute-instruction rplaca-inst state 1 (make-hash-table))
    (vm-reg-set state :r4 200)
    (execute-instruction rplacd-inst state 2 (make-hash-table))
    (let ((cell (vm-reg-get state :r0)))
      (is (= (car cell) 100))
      (is (= (cdr cell) 200)))))

;;; Closure Heap Operations Tests

(test vm-make-closure-heap-alloc
  "Test that vm-make-closure allocates on heap."
  (let* ((state (make-instance 'vm-state))
         (labels (make-hash-table))
         (inst (make-vm-make-closure
                              :dst :r0
                              :label :func
                              :params '(:x)
                              :env-regs '(:r1 :r2))))
    (setf (gethash :func labels) 10)
    (vm-reg-set state :r1 42)
    (vm-reg-set state :r2 99)
    (execute-instruction inst state 0 labels)
    (let* ((addr (vm-reg-get state :r0))
           (closure (vm-heap-get state addr)))
      (is (typep closure 'vm-closure-object))
      (is (eq (vm-closure-entry-label closure) :func))
      (is (equal (vm-closure-params closure) '(:x))))))

(test vm-closure-ref-idx-accesses-value
  "Test that vm-closure-ref-idx accesses captured values."
  (let* ((state (make-instance 'vm-state))
         (labels (make-hash-table))
         (make-inst (make-vm-make-closure
                                   :dst :r0
                                   :label :func
                                   :params nil
                                   :env-regs '(:r1 :r2 :r3)))
         (ref-inst-0 (make-vm-closure-ref-idx
                                    :dst :r4
                                    :closure :r0
                                    :index 0))
         (ref-inst-1 (make-vm-closure-ref-idx
                                    :dst :r5
                                    :closure :r0
                                    :index 1))
         (ref-inst-2 (make-vm-closure-ref-idx
                                    :dst :r6
                                    :closure :r0
                                    :index 2)))
    (vm-reg-set state :r1 10)
    (vm-reg-set state :r2 20)
    (vm-reg-set state :r3 30)
    (execute-instruction make-inst state 0 labels)
    (execute-instruction ref-inst-0 state 1 labels)
    (execute-instruction ref-inst-1 state 2 labels)
    (execute-instruction ref-inst-2 state 3 labels)
    (is (= (vm-reg-get state :r4) 10))
    (is (= (vm-reg-get state :r5) 20))
    (is (= (vm-reg-get state :r6) 30))))

(test vm-closure-ref-idx-out-of-bounds
  "Test that vm-closure-ref-idx signals error for invalid index."
  (let* ((state (make-instance 'vm-state))
         (labels (make-hash-table))
         (make-inst (make-vm-make-closure
                                   :dst :r0
                                   :label :func
                                   :params nil
                                   :env-regs '(:r1)))
         (ref-inst (make-vm-closure-ref-idx
                                  :dst :r2
                                  :closure :r0
                                  :index 99)))
    (vm-reg-set state :r1 42)
    (execute-instruction make-inst state 0 labels)
    (handler-case
        (progn
          (execute-instruction ref-inst state 1 labels)
          (fail "Should have signaled an error"))
      (error () (pass)))))

;;; Instruction <-> S-exp Conversion Tests

(test instruction->sexp-vm-cons
  "Test instruction->sexp for vm-cons."
  (let ((inst (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2)))
    (is (equal (instruction->sexp inst) '(:cons :r0 :r1 :r2)))))

(test instruction->sexp-vm-car
  "Test instruction->sexp for vm-car."
  (let ((inst (make-vm-car :dst :r0 :src :r1)))
    (is (equal (instruction->sexp inst) '(:car :r0 :r1)))))

(test instruction->sexp-vm-cdr
  "Test instruction->sexp for vm-cdr."
  (let ((inst (make-vm-cdr :dst :r0 :src :r1)))
    (is (equal (instruction->sexp inst) '(:cdr :r0 :r1)))))

(test instruction->sexp-vm-rplaca
  "Test instruction->sexp for vm-rplaca."
  (let ((inst (make-vm-rplaca :cons :r0 :val :r1)))
    (is (equal (instruction->sexp inst) '(:rplaca :r0 :r1)))))

(test instruction->sexp-vm-rplacd
  "Test instruction->sexp for vm-rplacd."
  (let ((inst (make-vm-rplacd :cons :r0 :val :r1)))
    (is (equal (instruction->sexp inst) '(:rplacd :r0 :r1)))))

(test instruction->sexp-vm-make-closure
  "Test instruction->sexp for vm-make-closure."
  (let ((inst (make-vm-make-closure
                             :dst :r0
                             :label :func
                             :params '(:x :y)
                             :env-regs '(:r1 :r2))))
    (is (equal (instruction->sexp inst) '(:make-closure :r0 :func (:x :y) :r1 :r2)))))

(test instruction->sexp-vm-closure-ref-idx
  "Test instruction->sexp for vm-closure-ref-idx."
  (let ((inst (make-vm-closure-ref-idx
                             :dst :r0
                             :closure :r1
                             :index 2)))
    (is (equal (instruction->sexp inst) '(:closure-ref-idx :r0 :r1 2)))))

(test sexp->instruction-vm-cons
  "Test sexp->instruction for vm-cons."
  (let ((inst (sexp->instruction '(:cons :r0 :r1 :r2))))
    (is (typep inst 'vm-cons))
    (is (eq (vm-dst inst) :r0))
    (is (eq (vm-car-reg inst) :r1))
    (is (eq (vm-cdr-reg inst) :r2))))

(test sexp->instruction-vm-car
  "Test sexp->instruction for vm-car."
  (let ((inst (sexp->instruction '(:car :r0 :r1))))
    (is (typep inst 'vm-car))
    (is (eq (vm-dst inst) :r0))
    (is (eq (vm-src inst) :r1))))

(test sexp->instruction-vm-cdr
  "Test sexp->instruction for vm-cdr."
  (let ((inst (sexp->instruction '(:cdr :r0 :r1))))
    (is (typep inst 'vm-cdr))
    (is (eq (vm-dst inst) :r0))
    (is (eq (vm-src inst) :r1))))

(test sexp->instruction-vm-rplaca
  "Test sexp->instruction for vm-rplaca."
  (let ((inst (sexp->instruction '(:rplaca :r0 :r1))))
    (is (typep inst 'vm-rplaca))
    (is (eq (vm-cons-reg inst) :r0))
    (is (eq (vm-val-reg inst) :r1))))

(test sexp->instruction-vm-rplacd
  "Test sexp->instruction for vm-rplacd."
  (let ((inst (sexp->instruction '(:rplacd :r0 :r1))))
    (is (typep inst 'vm-rplacd))
    (is (eq (vm-cons-reg inst) :r0))
    (is (eq (vm-val-reg inst) :r1))))

(test sexp->instruction-vm-make-closure
  "Test sexp->instruction for vm-make-closure."
  (let ((inst (sexp->instruction '(:make-closure :r0 :func (:x) :r1))))
    (is (typep inst 'vm-make-closure))
    (is (eq (vm-dst inst) :r0))
    (is (eq (vm-label-name inst) :func))
    (is (equal (vm-make-closure-params inst) '(:x)))
    (is (equal (vm-env-regs inst) '(:r1)))))

(test sexp->instruction-vm-closure-ref-idx
  "Test sexp->instruction for vm-closure-ref-idx."
  (let ((inst (sexp->instruction '(:closure-ref-idx :r0 :r1 3))))
    (is (typep inst 'vm-closure-ref-idx))
    (is (eq (vm-dst inst) :r0))
    (is (eq (vm-closure-reg inst) :r1))
    (is (= (vm-closure-index inst) 3))))

;;; Round-trip Tests

(test roundtrip-vm-cons
  "Test roundtrip conversion for vm-cons."
  (let* ((original (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (sexp (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (is (equal (instruction->sexp restored) sexp))))

(test roundtrip-vm-car
  "Test roundtrip conversion for vm-car."
  (let* ((original (make-vm-car :dst :r0 :src :r1))
         (sexp (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (is (equal (instruction->sexp restored) sexp))))

(test roundtrip-vm-cdr
  "Test roundtrip conversion for vm-cdr."
  (let* ((original (make-vm-cdr :dst :r0 :src :r1))
         (sexp (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (is (equal (instruction->sexp restored) sexp))))

(test roundtrip-vm-rplaca
  "Test roundtrip conversion for vm-rplaca."
  (let* ((original (make-vm-rplaca :cons :r0 :val :r1))
         (sexp (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (is (equal (instruction->sexp restored) sexp))))

(test roundtrip-vm-rplacd
  "Test roundtrip conversion for vm-rplacd."
  (let* ((original (make-vm-rplacd :cons :r0 :val :r1))
         (sexp (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (is (equal (instruction->sexp restored) sexp))))

(test roundtrip-vm-make-closure
  "Test roundtrip conversion for vm-make-closure."
  (let* ((original (make-vm-make-closure
                                  :dst :r0
                                  :label :func
                                  :params '(:x :y)
                                  :env-regs '(:r1 :r2)))
         (sexp (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (is (equal (instruction->sexp restored) sexp))))

(test roundtrip-vm-closure-ref-idx
  "Test roundtrip conversion for vm-closure-ref-idx."
  (let* ((original (make-vm-closure-ref-idx
                                  :dst :r0
                                  :closure :r1
                                  :index 5))
         (sexp (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (is (equal (instruction->sexp restored) sexp))))

;;; Integration: Building Lists

(test build-list-of-three-elements
  "Test building a list of three elements using cons."
  (let* ((state (make-instance 'vm-state))
         ;; Create (3 . nil)
         (inst1 (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         ;; Create (2 . <addr1>)
         (inst2 (make-vm-cons :dst :r3 :car-src :r4 :cdr-src :r0))
         ;; Create (1 . <addr2>)
         (inst3 (make-vm-cons :dst :r5 :car-src :r6 :cdr-src :r3)))
    (vm-reg-set state :r1 3)
    (vm-reg-set state :r2 nil)
    (execute-instruction inst1 state 0 (make-hash-table))
    (vm-reg-set state :r4 2)
    (execute-instruction inst2 state 1 (make-hash-table))
    (vm-reg-set state :r6 1)
    (execute-instruction inst3 state 2 (make-hash-table))
    ;; Verify list structure: (1 2 3)
    (let* ((cell1 (vm-reg-get state :r5))
           (cell2 (cdr cell1))
           (cell3 (cdr cell2)))
      (is (= (car cell1) 1))
      (is (= (car cell2) 2))
      (is (= (car cell3) 3))
      (is (null (cdr cell3))))))

(test traverse-list-with-car-cdr
  "Test traversing a list using car and cdr."
  (let* ((state (make-instance 'vm-state))
         ;; Build list (10 20 30)
         (inst1 (make-vm-cons :dst :r0 :car-src :r1 :cdr-src :r2))
         (inst2 (make-vm-cons :dst :r3 :car-src :r4 :cdr-src :r0))
         (inst3 (make-vm-cons :dst :r5 :car-src :r6 :cdr-src :r3)))
    (vm-reg-set state :r1 30)
    (vm-reg-set state :r2 nil)
    (execute-instruction inst1 state 0 (make-hash-table))
    (vm-reg-set state :r4 20)
    (execute-instruction inst2 state 1 (make-hash-table))
    (vm-reg-set state :r6 10)
    (execute-instruction inst3 state 2 (make-hash-table))
    ;; Traverse list using vm-car/vm-cdr instructions
    (let ((list-cell (vm-reg-get state :r5)))
      ;; Get first element (car list)
      (vm-reg-set state :r10 list-cell)
      (execute-instruction (make-vm-car :dst :r11 :src :r10) state 3 (make-hash-table))
      (is (= (vm-reg-get state :r11) 10))
      ;; Move to second element (car (cdr list))
      (execute-instruction (make-vm-cdr :dst :r12 :src :r10) state 4 (make-hash-table))
      (execute-instruction (make-vm-car :dst :r13 :src :r12) state 5 (make-hash-table))
      (is (= (vm-reg-get state :r13) 20))
      ;; Move to third element (car (cdr (cdr list)))
      (execute-instruction (make-vm-cdr :dst :r14 :src :r12) state 6 (make-hash-table))
      (execute-instruction (make-vm-car :dst :r15 :src :r14) state 7 (make-hash-table))
      (is (= (vm-reg-get state :r15) 30)))))
