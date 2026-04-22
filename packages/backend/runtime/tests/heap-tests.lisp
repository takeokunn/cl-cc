;;;; tests/vm-heap-tests.lisp - VM Heap Operations Tests
;;;
;;; This module provides tests for VM heap operations including:
;;; - Cons cell allocation and access
;;; - Car/cdr operations
;;; - Rplaca/rplacd mutation
;;; - Closure creation and access
;;;
;;; Instruction serialization, roundtrip, and integration list-building tests
;;; continue in heap-gc-tests.lisp.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; Heap Allocation Tests

(deftest vm-heap-allocation-cases
  "Basic alloc returns addr 1; multiple allocs yield unique addrs; get/set roundtrip works."
  (let* ((state (make-instance 'vm-io-state))
         (addr (vm-heap-alloc state nil)))
    (assert-= addr 1)
    (assert-= (vm-heap-counter state) 1))
  (let* ((state (make-instance 'vm-io-state))
         (addr1 (vm-heap-alloc state nil))
         (addr2 (vm-heap-alloc state nil))
         (addr3 (vm-heap-alloc state nil)))
    (assert-true (/= addr1 addr2))
    (assert-true (/= addr2 addr3))
    (assert-= (vm-heap-counter state) 3))
  (let* ((state (make-instance 'vm-io-state))
         (addr (incf (vm-heap-counter state))))
    (vm-heap-set state addr :test-value)
    (assert-eq (vm-heap-get state addr) :test-value)))

;;; Cons Cell Tests

(deftest vm-cons-creation-cases
  "vm-cons creates a cons cell with correct car/cdr; nil cdr works correctly."
  (let* ((state (make-instance 'vm-io-state))
         (inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2)))
    (vm-reg-set state 1 10)
    (vm-reg-set state 2 20)
    (execute-instruction inst state 0 (make-hash-table))
    (let ((cell (vm-reg-get state 0)))
      (assert-true (consp cell))
      (assert-= (car cell) 10)
      (assert-= (cdr cell) 20)))
  (let* ((state (make-instance 'vm-io-state))
         (inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2)))
    (vm-reg-set state 1 42)
    (vm-reg-set state 2 nil)
    (execute-instruction inst state 0 (make-hash-table))
    (let ((cell (vm-reg-get state 0)))
      (assert-true (consp cell))
      (assert-= (car cell) 42)
      (assert-null (cdr cell)))))

(deftest vm-cons-nested
  "Test that nested cons cells work correctly."
  (let* ((state (make-instance 'vm-io-state))
         ;; Create first cons: (2 . nil)
         (inst1 (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         ;; Create second cons: (1 . <addr of first cons>)
         (inst2 (make-vm-cons :dst 3 :car-src 4 :cdr-src 0)))
    (vm-reg-set state 1 2)
    (vm-reg-set state 2 nil)
    (execute-instruction inst1 state 0 (make-hash-table))
    (vm-reg-set state 4 1)
    (execute-instruction inst2 state 1 (make-hash-table))
    ;; Verify structure
    (let* ((outer-cell (vm-reg-get state 3))
           (inner-cell (cdr outer-cell)))
      (assert-= (car outer-cell) 1)
      (assert-= (car inner-cell) 2)
      (assert-null (cdr inner-cell)))))

;;; Car/Cdr Tests

(deftest vm-car-cdr-cases
  "vm-car/cdr extract correct half; car on nested cons traverses correctly."
  (let* ((state (make-instance 'vm-io-state))
         (cons-inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (car-inst (make-vm-car :dst 3 :src 0)))
    (vm-reg-set state 1 123)
    (vm-reg-set state 2 456)
    (execute-instruction cons-inst state 0 (make-hash-table))
    (execute-instruction car-inst state 1 (make-hash-table))
    (assert-= (vm-reg-get state 3) 123))
  (let* ((state (make-instance 'vm-io-state))
         (cons-inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (cdr-inst (make-vm-cdr :dst 3 :src 0)))
    (vm-reg-set state 1 123)
    (vm-reg-set state 2 456)
    (execute-instruction cons-inst state 0 (make-hash-table))
    (execute-instruction cdr-inst state 1 (make-hash-table))
    (assert-= (vm-reg-get state 3) 456))
  (let* ((state (make-instance 'vm-io-state))
         (inst1 (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (inst2 (make-vm-cons :dst 3 :car-src 0 :cdr-src 4))
         (car-inst (make-vm-car :dst 5 :src 3))
         (car-inst2 (make-vm-car :dst 6 :src 5)))
    (vm-reg-set state 1 2)
    (vm-reg-set state 2 3)
    (vm-reg-set state 4 4)
    (execute-instruction inst1 state 0 (make-hash-table))
    (execute-instruction inst2 state 1 (make-hash-table))
    (execute-instruction car-inst state 2 (make-hash-table))
    (execute-instruction car-inst2 state 3 (make-hash-table))
    (assert-= (vm-reg-get state 6) 2)))

;;; Rplaca/Rplacd Tests

(deftest vm-rplaca-rplacd-cases
  "rplaca/rplacd each mutate the correct half; both can be used on the same cons."
  ;; Block 1: rplaca on (10 . 20) → (99 . 20)
  (cl-cc/vm::vm-clear-hash-cons-table)
  (let* ((state (make-instance 'vm-io-state))
         (cons-inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (rplaca-inst (make-vm-rplaca :cons 0 :val 3)))
    (vm-reg-set state 1 10)
    (vm-reg-set state 2 20)
    (execute-instruction cons-inst state 0 (make-hash-table))
    (let ((cell (vm-reg-get state 0)))
      (assert-= 10 (car cell)))
    (vm-reg-set state 3 99)
    (execute-instruction rplaca-inst state 1 (make-hash-table))
    (let ((cell (vm-reg-get state 0)))
      (assert-= 99 (car cell))
      (assert-= 20 (cdr cell))))
  ;; Block 2: rplacd on (11 . 21) → (11 . 88), uses different key to avoid cache collision
  (cl-cc/vm::vm-clear-hash-cons-table)
  (let* ((state (make-instance 'vm-io-state))
         (cons-inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (rplacd-inst (make-vm-rplacd :cons 0 :val 3)))
    (vm-reg-set state 1 11)
    (vm-reg-set state 2 21)
    (execute-instruction cons-inst state 0 (make-hash-table))
    (let ((cell (vm-reg-get state 0)))
      (assert-= 21 (cdr cell)))
    (vm-reg-set state 3 88)
    (execute-instruction rplacd-inst state 1 (make-hash-table))
    (let ((cell (vm-reg-get state 0)))
      (assert-= 11 (car cell))
      (assert-= 88 (cdr cell))))
  (let* ((state (make-instance 'vm-io-state))
         (cons-inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (rplaca-inst (make-vm-rplaca :cons 0 :val 3))
         (rplacd-inst (make-vm-rplacd :cons 0 :val 4)))
    (vm-reg-set state 1 1)
    (vm-reg-set state 2 2)
    (execute-instruction cons-inst state 0 (make-hash-table))
    (vm-reg-set state 3 100)
    (execute-instruction rplaca-inst state 1 (make-hash-table))
    (vm-reg-set state 4 200)
    (execute-instruction rplacd-inst state 2 (make-hash-table))
    (let ((cell (vm-reg-get state 0)))
      (assert-= (car cell) 100)
      (assert-= (cdr cell) 200))))

;;; Closure Heap Operations Tests

(deftest vm-make-closure-heap-alloc
  "Test that vm-make-closure allocates on heap."
  (let* ((state (make-instance 'vm-io-state))
         (labels (make-hash-table))
         (inst (make-vm-make-closure
                              :dst 0
                              :label :func
                              :params '(:x)
                              :env-regs '(1 2))))
    (setf (gethash :func labels) 10)
    (vm-reg-set state 1 42)
    (vm-reg-set state 2 99)
    (execute-instruction inst state 0 labels)
    (let* ((addr (vm-reg-get state 0))
           (closure (vm-heap-get state addr)))
      (assert-type vm-closure-object closure)
      (assert-eq (vm-closure-entry-label closure) :func)
      (assert-equal (vm-closure-params closure) '(:x)))))

(deftest vm-closure-ref-idx-accesses-value
  "Test that vm-closure-ref-idx accesses captured values."
  (let* ((state (make-instance 'vm-io-state))
         (labels (make-hash-table))
         (make-inst (make-vm-make-closure
                                   :dst 0
                                   :label :func
                                   :params nil
                                   :env-regs '(1 2 3)))
         (ref-inst-0 (make-vm-closure-ref-idx
                                    :dst 4
                                    :closure 0
                                    :index 0))
         (ref-inst-1 (make-vm-closure-ref-idx
                                    :dst 5
                                    :closure 0
                                    :index 1))
         (ref-inst-2 (make-vm-closure-ref-idx
                                    :dst 6
                                    :closure 0
                                    :index 2)))
    (vm-reg-set state 1 10)
    (vm-reg-set state 2 20)
    (vm-reg-set state 3 30)
    (execute-instruction make-inst state 0 labels)
    (execute-instruction ref-inst-0 state 1 labels)
    (execute-instruction ref-inst-1 state 2 labels)
    (execute-instruction ref-inst-2 state 3 labels)
    (assert-= (vm-reg-get state 4) 10)
    (assert-= (vm-reg-get state 5) 20)
    (assert-= (vm-reg-get state 6) 30)))

(deftest vm-closure-ref-idx-out-of-bounds
  "Test that vm-closure-ref-idx signals error for invalid index."
  (let* ((state (make-instance 'vm-io-state))
         (labels (make-hash-table))
         (make-inst (make-vm-make-closure
                                   :dst 0
                                   :label :func
                                   :params nil
                                   :env-regs '(1)))
         (ref-inst (make-vm-closure-ref-idx
                                  :dst 2
                                  :closure 0
                                  :index 99)))
    (vm-reg-set state 1 42)
    (execute-instruction make-inst state 0 labels)
    (handler-case
        (progn
          (execute-instruction ref-inst state 1 labels)
          (%fail-test "Should have signaled an error"))
      (error () t))))
