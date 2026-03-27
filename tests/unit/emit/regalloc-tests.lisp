;;;; tests/regalloc-tests.lisp - Register Allocator Tests

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; Instruction Def/Use Tests

(deftest-each regalloc-defs-and-uses
  "instruction-defs and instruction-uses return correct register sets for each instruction type."
  :cases (("vm-const"     (make-vm-const     :dst :r0 :value 42)                 '(:r0) nil)
          ("vm-binop"     (make-vm-add       :dst :r2 :lhs :r0 :rhs :r1)         '(:r2) '(:r0 :r1))
          ("vm-call"      (make-vm-call      :dst :r3 :func :r0 :args '(:r1 :r2)) '(:r3) '(:r0 :r1 :r2))
          ("vm-jump-zero" (make-vm-jump-zero :reg :r0 :label "L1")                nil    '(:r0))
          ("vm-label"     (make-vm-label     :name "L1")                           nil    nil))
  (inst expected-defs expected-uses)
  (assert-equal expected-defs (instruction-defs inst))
  (assert-equal expected-uses (instruction-uses inst)))

;;; Liveness Analysis Tests

(deftest regalloc-liveness-analysis
  "Liveness analysis: 3 overlapping intervals; 2 disjoint intervals prove no overlap."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-const :dst :r1 :value 2)
                             (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-halt :reg :r2)))
         (intervals (compute-live-intervals instructions)))
    (assert-= 3 (length intervals))
    ;; R0: defined at 0, last used at 2
    (let ((r0-int (find :r0 intervals :key #'interval-vreg)))
      (assert-false (null r0-int))
      (assert-= 0 (interval-start r0-int))
      (assert-= 2 (interval-end r0-int)))
    ;; R1: defined at 1, last used at 2
    (let ((r1-int (find :r1 intervals :key #'interval-vreg)))
      (assert-false (null r1-int))
      (assert-= 1 (interval-start r1-int))
      (assert-= 2 (interval-end r1-int)))
    ;; R2: defined at 2, last used at 3
    (let ((r2-int (find :r2 intervals :key #'interval-vreg)))
      (assert-false (null r2-int))
      (assert-= 2 (interval-start r2-int))
      (assert-= 3 (interval-end r2-int))))
  ;; Disjoint intervals: R0 ends before R1 starts, so no overlap
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-halt :reg :r0)
                             (make-vm-const :dst :r1 :value 2)
                             (make-vm-halt :reg :r1)))
         (intervals (compute-live-intervals instructions)))
    (assert-= 2 (length intervals))
    (let ((r0-int (find :r0 intervals :key #'interval-vreg))
          (r1-int (find :r1 intervals :key #'interval-vreg)))
      (assert-true (<= (interval-end r0-int) (interval-start r1-int))))))

;;; Linear Scan Allocation Tests

(deftest regalloc-allocate-simple
  "Simple allocation fits in physical registers."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-const :dst :r1 :value 2)
                             (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-halt :reg :r2)))
         (result (allocate-registers instructions *x86-64-calling-convention*)))
    (assert-= 0 (regalloc-spill-count result))
    ;; All 3 vregs should be assigned physical registers
    (assert-false (null (regalloc-lookup result :r0)))
    (assert-false (null (regalloc-lookup result :r1)))
    (assert-false (null (regalloc-lookup result :r2)))
    ;; Physical registers should be different where intervals overlap
    (assert-false (eq (regalloc-lookup result :r0)
                      (regalloc-lookup result :r1)))))

(deftest regalloc-allocate-reuse
  "Registers can be reused after last use."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-move :dst :r1 :src :r0)
                             (make-vm-const :dst :r2 :value 2)
                             (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)
                             (make-vm-halt :reg :r3)))
         (result (allocate-registers instructions *x86-64-calling-convention*)))
    (assert-= 0 (regalloc-spill-count result))))

(deftest regalloc-allocate-no-instructions
  "Empty instruction list doesn't crash."
  (let ((result (allocate-registers nil *x86-64-calling-convention*)))
    (assert-= 0 (regalloc-spill-count result))))

(deftest regalloc-calling-convention-exists
  "Calling conventions are properly defined."
  (assert-false (null *x86-64-calling-convention*))
  (assert-false (null *aarch64-calling-convention*))
  (assert-= 13 (length (cc-gpr-pool *x86-64-calling-convention*)))
  (assert-eq :rax (cc-return-register *x86-64-calling-convention*))
  (assert-eq :r11 (cc-scratch-register *x86-64-calling-convention*))
  (assert-eq :x0 (cc-return-register *aarch64-calling-convention*)))

;;; Integration Test: compile-expression through regalloc

(deftest regalloc-compile-and-allocate
  "Compile a simple expression and allocate registers."
  (let* ((result (compile-expression '(+ 1 2) :target :vm))
         (program (compilation-result-program result))
         (instructions (vm-program-instructions program))
         (alloc (allocate-registers instructions *x86-64-calling-convention*)))
    (assert-= 0 (regalloc-spill-count alloc))
    ;; Every vreg should have an assignment
    (let ((all-vregs nil))
      (dolist (inst instructions)
        (dolist (v (instruction-defs inst)) (when v (pushnew v all-vregs)))
        (dolist (v (instruction-uses inst)) (when v (pushnew v all-vregs))))
      (dolist (vreg all-vregs)
        (assert-false (null (regalloc-lookup alloc vreg)))))))
