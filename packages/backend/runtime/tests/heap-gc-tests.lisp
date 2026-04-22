;;;; tests/vm-heap-gc-tests.lisp — VM Heap Instruction Serialization and Integration Tests
;;;
;;; Covers:
;;; - instruction->sexp / sexp->instruction for heap instructions
;;; - Round-trip: instruction->sexp->sexp->instruction fidelity
;;; - Integration: building and traversing lists via cons/car/cdr sequences
;;;
;;; Helper functions (eval-cps-ast etc.) defined in heap-tests.lisp are
;;; available here because ASDF serial load order ensures heap-tests loads first.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; Instruction <-> S-exp Conversion Tests

(deftest-each instruction->sexp-heap-instructions
  "instruction->sexp produces correct s-expression for each heap instruction type."
  :cases (("vm-cons"         (make-vm-cons :dst 0 :car-src 1 :cdr-src 2)
           '(:cons 0 1 2))
          ("vm-car"          (make-vm-car :dst 0 :src 1)
           '(:car 0 1))
          ("vm-cdr"          (make-vm-cdr :dst 0 :src 1)
           '(:cdr 0 1))
          ("vm-rplaca"       (make-vm-rplaca :cons 0 :val 1)
           '(:rplaca 0 1))
          ("vm-rplacd"       (make-vm-rplacd :cons 0 :val 1)
           '(:rplacd 0 1))
          ("vm-make-closure" (make-vm-make-closure :dst 0 :label :func :params '(:x :y) :env-regs '(1 2))
           '(:make-closure 0 :func (:x :y) 1 2))
          ("vm-closure-ref"  (make-vm-closure-ref-idx :dst 0 :closure 1 :index 2)
           '(:closure-ref-idx 0 1 2)))
  (inst expected)
  (assert-equal expected (instruction->sexp inst)))

(deftest sexp->instruction-vm-cons
  "Test sexp->instruction for vm-cons."
  (let ((inst (sexp->instruction '(:cons 0 1 2))))
    (assert-type vm-cons inst)
    (assert-eq (vm-dst inst) 0)
    (assert-eq (vm-car-reg inst) 1)
    (assert-eq (vm-cdr-reg inst) 2)))

(deftest-each sexp->instruction-dst-src-ops
  "sexp->instruction produces the correct type with :dst and :src for car/cdr."
  :cases (("car" '(:car 0 1) 'vm-car)
          ("cdr" '(:cdr 0 1) 'vm-cdr))
  (sexp expected-type)
  (let ((inst (sexp->instruction sexp)))
    (assert-true (typep inst expected-type))
    (assert-eq (vm-dst inst) 0)
    (assert-eq (vm-src inst) 1)))

(deftest-each sexp->instruction-cons-val-ops
  "sexp->instruction produces correct type with :cons and :val for rplaca/rplacd."
  :cases (("rplaca" '(:rplaca 0 1) 'vm-rplaca)
          ("rplacd" '(:rplacd 0 1) 'vm-rplacd))
  (sexp expected-type)
  (let ((inst (sexp->instruction sexp)))
    (assert-true (typep inst expected-type))
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

(deftest-each roundtrip-heap-instructions
  "instruction->sexp→sexp->instruction roundtrip preserves s-expression for each heap instruction."
  :cases (("vm-cons"         (make-vm-cons :dst 0 :car-src 1 :cdr-src 2))
          ("vm-car"          (make-vm-car :dst 0 :src 1))
          ("vm-cdr"          (make-vm-cdr :dst 0 :src 1))
          ("vm-rplaca"       (make-vm-rplaca :cons 0 :val 1))
          ("vm-rplacd"       (make-vm-rplacd :cons 0 :val 1))
          ("vm-make-closure" (make-vm-make-closure :dst 0 :label :func :params '(:x :y) :env-regs '(1 2)))
          ("vm-closure-ref"  (make-vm-closure-ref-idx :dst 0 :closure 1 :index 5)))
  (original)
  (let* ((sexp     (instruction->sexp original))
         (restored (sexp->instruction sexp)))
    (assert-equal sexp (instruction->sexp restored))))

;;; Integration: Building Lists

(deftest build-list-of-three-elements
  "Test building a list of three elements using cons."
  (let* ((state (make-instance 'vm-io-state))
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
  (let* ((state (make-instance 'vm-io-state))
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
