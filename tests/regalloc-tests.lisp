;;;; tests/regalloc-tests.lisp - Register Allocator Tests

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; Instruction Def/Use Tests

(test regalloc-defs-const
  "vm-const defines its dst register."
  (let ((inst (make-vm-const :dst :r0 :value 42)))
    (is (equal '(:r0) (instruction-defs inst)))
    (is (null (instruction-uses inst)))))

(test regalloc-defs-binop
  "vm-add defines dst, uses lhs and rhs."
  (let ((inst (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)))
    (is (equal '(:r2) (instruction-defs inst)))
    (is (equal '(:r0 :r1) (instruction-uses inst)))))

(test regalloc-defs-call
  "vm-call defines dst, uses func and args."
  (let ((inst (make-vm-call :dst :r3 :func :r0 :args '(:r1 :r2))))
    (is (equal '(:r3) (instruction-defs inst)))
    (is (equal '(:r0 :r1 :r2) (instruction-uses inst)))))

(test regalloc-defs-jump-zero
  "vm-jump-zero uses reg but defines nothing."
  (let ((inst (make-vm-jump-zero :reg :r0 :label "L1")))
    (is (null (instruction-defs inst)))
    (is (equal '(:r0) (instruction-uses inst)))))

(test regalloc-defs-label
  "vm-label defines and uses nothing."
  (let ((inst (make-vm-label :name "L1")))
    (is (null (instruction-defs inst)))
    (is (null (instruction-uses inst)))))

;;; Liveness Analysis Tests

(test regalloc-liveness-simple
  "Simple linear code liveness."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-const :dst :r1 :value 2)
                             (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-halt :reg :r2)))
         (intervals (compute-live-intervals instructions)))
    (is (= 3 (length intervals)))
    ;; R0: defined at 0, last used at 2
    (let ((r0-int (find :r0 intervals :key #'interval-vreg)))
      (is (not (null r0-int)))
      (is (= 0 (interval-start r0-int)))
      (is (= 2 (interval-end r0-int))))
    ;; R1: defined at 1, last used at 2
    (let ((r1-int (find :r1 intervals :key #'interval-vreg)))
      (is (not (null r1-int)))
      (is (= 1 (interval-start r1-int)))
      (is (= 2 (interval-end r1-int))))
    ;; R2: defined at 2, last used at 3
    (let ((r2-int (find :r2 intervals :key #'interval-vreg)))
      (is (not (null r2-int)))
      (is (= 2 (interval-start r2-int)))
      (is (= 3 (interval-end r2-int))))))

(test regalloc-liveness-reuse
  "Register reuse after last use."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-halt :reg :r0)
                             (make-vm-const :dst :r1 :value 2)
                             (make-vm-halt :reg :r1)))
         (intervals (compute-live-intervals instructions)))
    (is (= 2 (length intervals)))
    ;; R0 and R1 don't overlap, so only 1 physical register needed
    (let ((r0-int (find :r0 intervals :key #'interval-vreg))
          (r1-int (find :r1 intervals :key #'interval-vreg)))
      (is (<= (interval-end r0-int) (interval-start r1-int))))))

;;; Linear Scan Allocation Tests

(test regalloc-allocate-simple
  "Simple allocation fits in physical registers."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-const :dst :r1 :value 2)
                             (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-halt :reg :r2)))
         (result (allocate-registers instructions *x86-64-calling-convention*)))
    (is (= 0 (regalloc-spill-count result)))
    ;; All 3 vregs should be assigned physical registers
    (is (not (null (regalloc-lookup result :r0))))
    (is (not (null (regalloc-lookup result :r1))))
    (is (not (null (regalloc-lookup result :r2))))
    ;; Physical registers should be different where intervals overlap
    (is (not (eq (regalloc-lookup result :r0)
                 (regalloc-lookup result :r1))))))

(test regalloc-allocate-reuse
  "Registers can be reused after last use."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-move :dst :r1 :src :r0)
                             (make-vm-const :dst :r2 :value 2)
                             (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)
                             (make-vm-halt :reg :r3)))
         (result (allocate-registers instructions *x86-64-calling-convention*)))
    (is (= 0 (regalloc-spill-count result)))))

(test regalloc-allocate-no-instructions
  "Empty instruction list doesn't crash."
  (let ((result (allocate-registers nil *x86-64-calling-convention*)))
    (is (= 0 (regalloc-spill-count result)))))

(test regalloc-calling-convention-exists
  "Calling conventions are properly defined."
  (is (not (null *x86-64-calling-convention*)))
  (is (not (null *aarch64-calling-convention*)))
  (is (= 13 (length (cc-gpr-pool *x86-64-calling-convention*))))
  (is (eq :rax (cc-return-register *x86-64-calling-convention*)))
  (is (eq :r11 (cc-scratch-register *x86-64-calling-convention*)))
  (is (eq :x0 (cc-return-register *aarch64-calling-convention*))))

;;; Integration Test: compile-expression through regalloc

(test regalloc-compile-and-allocate
  "Compile a simple expression and allocate registers."
  (let* ((result (compile-expression '(+ 1 2) :target :vm))
         (program (compilation-result-program result))
         (instructions (vm-program-instructions program))
         (alloc (allocate-registers instructions *x86-64-calling-convention*)))
    (is (= 0 (regalloc-spill-count alloc)))
    ;; Every vreg should have an assignment
    (let ((all-vregs nil))
      (dolist (inst instructions)
        (dolist (v (instruction-defs inst)) (when v (pushnew v all-vregs)))
        (dolist (v (instruction-uses inst)) (when v (pushnew v all-vregs))))
      (dolist (vreg all-vregs)
        (is (not (null (regalloc-lookup alloc vreg))))))))
