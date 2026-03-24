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

(deftest vm-alloc-basic
  "Test basic heap allocation."
  (let* ((state (make-instance 'vm-state))
         (addr (vm-heap-alloc state nil)))
    (assert-= addr 1)
    (assert-= (vm-heap-counter state) 1)))

(deftest vm-alloc-multiple
  "Test multiple heap allocations produce unique addresses."
  (let* ((state (make-instance 'vm-state))
         (addr1 (vm-heap-alloc state nil))
         (addr2 (vm-heap-alloc state nil))
         (addr3 (vm-heap-alloc state nil)))
    (assert-true (/= addr1 addr2))
    (assert-true (/= addr2 addr3))
    (assert-= (vm-heap-counter state) 3)))

(deftest vm-heap-get-and-set
  "Test heap get and set operations."
  (let* ((state (make-instance 'vm-state))
         (addr (incf (vm-heap-counter state))))
    (vm-heap-set state addr :test-value)
    (assert-eq (vm-heap-get state addr) :test-value)))

;;; Cons Cell Tests

(deftest vm-cons-creates-cell
  "Test that vm-cons creates a cons cell."
  (let* ((state (make-instance 'vm-state))
         (inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2)))
    (vm-reg-set state 1 10)
    (vm-reg-set state 2 20)
    (execute-instruction inst state 0 (make-hash-table))
    (let ((cell (vm-reg-get state 0)))
      (assert-true (consp cell))
      (assert-= (car cell) 10)
      (assert-= (cdr cell) 20))))

(deftest vm-cons-with-nil-cdr
  "Test that vm-cons works with nil cdr."
  (let* ((state (make-instance 'vm-state))
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
  (let* ((state (make-instance 'vm-state))
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

(deftest vm-car-extracts-value
  "Test that vm-car extracts the car of a cons cell."
  (let* ((state (make-instance 'vm-state))
         (cons-inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (car-inst (make-vm-car :dst 3 :src 0)))
    (vm-reg-set state 1 123)
    (vm-reg-set state 2 456)
    (execute-instruction cons-inst state 0 (make-hash-table))
    (execute-instruction car-inst state 1 (make-hash-table))
    (assert-= (vm-reg-get state 3) 123)))

(deftest vm-cdr-extracts-value
  "Test that vm-cdr extracts the cdr of a cons cell."
  (let* ((state (make-instance 'vm-state))
         (cons-inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (cdr-inst (make-vm-cdr :dst 3 :src 0)))
    (vm-reg-set state 1 123)
    (vm-reg-set state 2 456)
    (execute-instruction cons-inst state 0 (make-hash-table))
    (execute-instruction cdr-inst state 1 (make-hash-table))
    (assert-= (vm-reg-get state 3) 456)))

(deftest vm-car-on-nested-cons
  "Test car on nested cons cells."
  (let* ((state (make-instance 'vm-state))
         ;; Create (2 . 3)
         (inst1 (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         ;; Create ((2 . 3) . 4)
         (inst2 (make-vm-cons :dst 3 :car-src 0 :cdr-src 4))
         ;; Get car of outer cons (should be address of inner cons)
         (car-inst (make-vm-car :dst 5 :src 3))
         ;; Get car of inner cons (should be 2)
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

(deftest vm-rplaca-modifies-car
  "Test that vm-rplaca modifies the car of a cons cell."
  (let* ((state (make-instance 'vm-state))
         (cons-inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (rplaca-inst (make-vm-rplaca :cons 0 :val 3)))
    (vm-reg-set state 1 10)
    (vm-reg-set state 2 20)
    (execute-instruction cons-inst state 0 (make-hash-table))
    ;; Verify initial value
    (let ((cell (vm-reg-get state 0)))
      (assert-= (car cell) 10))
    ;; Modify car
    (vm-reg-set state 3 99)
    (execute-instruction rplaca-inst state 1 (make-hash-table))
    ;; Verify modification
    (let ((cell (vm-reg-get state 0)))
      (assert-= (car cell) 99)
      (assert-= (cdr cell) 20))))

(deftest vm-rplacd-modifies-cdr
  "Test that vm-rplacd modifies the cdr of a cons cell."
  (let* ((state (make-instance 'vm-state))
         (cons-inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (rplacd-inst (make-vm-rplacd :cons 0 :val 3)))
    (vm-reg-set state 1 10)
    (vm-reg-set state 2 20)
    (execute-instruction cons-inst state 0 (make-hash-table))
    ;; Verify initial value
    (let ((cell (vm-reg-get state 0)))
      (assert-= (cdr cell) 20))
    ;; Modify cdr
    (vm-reg-set state 3 88)
    (execute-instruction rplacd-inst state 1 (make-hash-table))
    ;; Verify modification
    (let ((cell (vm-reg-get state 0)))
      (assert-= (car cell) 10)
      (assert-= (cdr cell) 88))))

(deftest vm-rplaca-and-rplacd-together
  "Test that rplaca and rplacd can both be used on the same cons."
  (let* ((state (make-instance 'vm-state))
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
  (let* ((state (make-instance 'vm-state))
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
  (let* ((state (make-instance 'vm-state))
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
  (let* ((state (make-instance 'vm-state))
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

;;; Instruction <-> S-exp Conversion Tests

(deftest instruction->sexp-vm-cons
  "Test instruction->sexp for vm-cons."
  (let ((inst (make-vm-cons :dst 0 :car-src 1 :cdr-src 2)))
    (assert-equal (instruction->sexp inst) '(:cons 0 1 2))))

(deftest instruction->sexp-vm-car
  "Test instruction->sexp for vm-car."
  (let ((inst (make-vm-car :dst 0 :src 1)))
    (assert-equal (instruction->sexp inst) '(:car 0 1))))

(deftest instruction->sexp-vm-cdr
  "Test instruction->sexp for vm-cdr."
  (let ((inst (make-vm-cdr :dst 0 :src 1)))
    (assert-equal (instruction->sexp inst) '(:cdr 0 1))))

(deftest instruction->sexp-vm-rplaca
  "Test instruction->sexp for vm-rplaca."
  (let ((inst (make-vm-rplaca :cons 0 :val 1)))
    (assert-equal (instruction->sexp inst) '(:rplaca 0 1))))

(deftest instruction->sexp-vm-rplacd
  "Test instruction->sexp for vm-rplacd."
  (let ((inst (make-vm-rplacd :cons 0 :val 1)))
    (assert-equal (instruction->sexp inst) '(:rplacd 0 1))))

(deftest instruction->sexp-vm-make-closure
  "Test instruction->sexp for vm-make-closure."
  (let ((inst (make-vm-make-closure
                             :dst 0
                             :label :func
                             :params '(:x :y)
                             :env-regs '(1 2))))
    (assert-equal (instruction->sexp inst) '(:make-closure 0 :func (:x :y) 1 2))))

(deftest instruction->sexp-vm-closure-ref-idx
  "Test instruction->sexp for vm-closure-ref-idx."
  (let ((inst (make-vm-closure-ref-idx
                             :dst 0
                             :closure 1
                             :index 2)))
    (assert-equal (instruction->sexp inst) '(:closure-ref-idx 0 1 2))))

(deftest sexp->instruction-vm-cons
  "Test sexp->instruction for vm-cons."
  (let ((inst (sexp->instruction '(:cons 0 1 2))))
    (assert-type vm-cons inst)
    (assert-eq (vm-dst inst) 0)
    (assert-eq (vm-car-reg inst) 1)
    (assert-eq (vm-cdr-reg inst) 2)))

(deftest sexp->instruction-vm-car
  "Test sexp->instruction for vm-car."
  (let ((inst (sexp->instruction '(:car 0 1))))
    (assert-type vm-car inst)
    (assert-eq (vm-dst inst) 0)
    (assert-eq (vm-src inst) 1)))

(deftest sexp->instruction-vm-cdr
  "Test sexp->instruction for vm-cdr."
  (let ((inst (sexp->instruction '(:cdr 0 1))))
    (assert-type vm-cdr inst)
    (assert-eq (vm-dst inst) 0)
    (assert-eq (vm-src inst) 1)))

(deftest sexp->instruction-vm-rplaca
  "Test sexp->instruction for vm-rplaca."
  (let ((inst (sexp->instruction '(:rplaca 0 1))))
    (assert-type vm-rplaca inst)
    (assert-eq (vm-cons-reg inst) 0)
    (assert-eq (vm-val-reg inst) 1)))

(deftest sexp->instruction-vm-rplacd
  "Test sexp->instruction for vm-rplacd."
  (let ((inst (sexp->instruction '(:rplacd 0 1))))
    (assert-type vm-rplacd inst)
    (assert-eq (vm-cons-reg inst) 0)
    (assert-eq (vm-val-reg inst) 1)))

(deftest sexp->instruction-vm-make-closure
  "Test sexp->instruction for vm-make-closure."
  (let ((inst (sexp->instruction '(:make-closure 0 :func (:x) 1))))
    (assert-type vm-make-closure inst)
    (assert-eq (vm-dst inst) 0)
    (assert-eq (vm-label-name inst) :func)
    (assert-equal (vm-make-closure-params inst) '(:x))
    (assert-equal (vm-env-regs inst) '(1))))

(deftest sexp->instruction-vm-closure-ref-idx
  "Test sexp->instruction for vm-closure-ref-idx."
  (let ((inst (sexp->instruction '(:closure-ref-idx 0 1 3))))
    (assert-type vm-closure-ref-idx inst)
    (assert-eq (vm-dst inst) 0)
    (assert-eq (vm-closure-reg inst) 1)
    (assert-= (vm-closure-index inst) 3)))

;;; Round-trip Tests

(deftest roundtrip-vm-cons
  "Test roundtrip conversion for vm-cons."
  (let* ((original (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (sexp (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (assert-equal (instruction->sexp restored) sexp)))

(deftest roundtrip-vm-car
  "Test roundtrip conversion for vm-car."
  (let* ((original (make-vm-car :dst 0 :src 1))
         (sexp (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (assert-equal (instruction->sexp restored) sexp)))

(deftest roundtrip-vm-cdr
  "Test roundtrip conversion for vm-cdr."
  (let* ((original (make-vm-cdr :dst 0 :src 1))
         (sexp (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (assert-equal (instruction->sexp restored) sexp)))

(deftest roundtrip-vm-rplaca
  "Test roundtrip conversion for vm-rplaca."
  (let* ((original (make-vm-rplaca :cons 0 :val 1))
         (sexp (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (assert-equal (instruction->sexp restored) sexp)))

(deftest roundtrip-vm-rplacd
  "Test roundtrip conversion for vm-rplacd."
  (let* ((original (make-vm-rplacd :cons 0 :val 1))
         (sexp (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (assert-equal (instruction->sexp restored) sexp)))

(deftest roundtrip-vm-make-closure
  "Test roundtrip conversion for vm-make-closure."
  (let* ((original (make-vm-make-closure
                                  :dst 0
                                  :label :func
                                  :params '(:x :y)
                                  :env-regs '(1 2)))
         (sexp (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (assert-equal (instruction->sexp restored) sexp)))

(deftest roundtrip-vm-closure-ref-idx
  "Test roundtrip conversion for vm-closure-ref-idx."
  (let* ((original (make-vm-closure-ref-idx
                                  :dst 0
                                  :closure 1
                                  :index 5))
         (sexp (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (assert-equal (instruction->sexp restored) sexp)))

;;; Integration: Building Lists

(deftest build-list-of-three-elements
  "Test building a list of three elements using cons."
  (let* ((state (make-instance 'vm-state))
         ;; Create (3 . nil)
         (inst1 (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         ;; Create (2 . <addr1>)
         (inst2 (make-vm-cons :dst 3 :car-src 4 :cdr-src 0))
         ;; Create (1 . <addr2>)
         (inst3 (make-vm-cons :dst 5 :car-src 6 :cdr-src 3)))
    (vm-reg-set state 1 3)
    (vm-reg-set state 2 nil)
    (execute-instruction inst1 state 0 (make-hash-table))
    (vm-reg-set state 4 2)
    (execute-instruction inst2 state 1 (make-hash-table))
    (vm-reg-set state 6 1)
    (execute-instruction inst3 state 2 (make-hash-table))
    ;; Verify list structure: (1 2 3)
    (let* ((cell1 (vm-reg-get state 5))
           (cell2 (cdr cell1))
           (cell3 (cdr cell2)))
      (assert-= (car cell1) 1)
      (assert-= (car cell2) 2)
      (assert-= (car cell3) 3)
      (assert-null (cdr cell3)))))

(deftest traverse-list-with-car-cdr
  "Test traversing a list using car and cdr."
  (let* ((state (make-instance 'vm-state))
         ;; Build list (10 20 30)
         (inst1 (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
         (inst2 (make-vm-cons :dst 3 :car-src 4 :cdr-src 0))
         (inst3 (make-vm-cons :dst 5 :car-src 6 :cdr-src 3)))
    (vm-reg-set state 1 30)
    (vm-reg-set state 2 nil)
    (execute-instruction inst1 state 0 (make-hash-table))
    (vm-reg-set state 4 20)
    (execute-instruction inst2 state 1 (make-hash-table))
    (vm-reg-set state 6 10)
    (execute-instruction inst3 state 2 (make-hash-table))
    ;; Traverse list using vm-car/vm-cdr instructions
    (let ((list-cell (vm-reg-get state 5)))
      ;; Get first element (car list)
      (vm-reg-set state 10 list-cell)
      (execute-instruction (make-vm-car :dst 11 :src 10) state 3 (make-hash-table))
      (assert-= (vm-reg-get state 11) 10)
      ;; Move to second element (car (cdr list))
      (execute-instruction (make-vm-cdr :dst 12 :src 10) state 4 (make-hash-table))
      (execute-instruction (make-vm-car :dst 13 :src 12) state 5 (make-hash-table))
      (assert-= (vm-reg-get state 13) 20)
      ;; Move to third element (car (cdr (cdr list)))
      (execute-instruction (make-vm-cdr :dst 14 :src 12) state 6 (make-hash-table))
      (execute-instruction (make-vm-car :dst 15 :src 14) state 7 (make-hash-table))
      (assert-= (vm-reg-get state 15) 30))))
