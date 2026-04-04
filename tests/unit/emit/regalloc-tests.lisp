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

(deftest regalloc-liveness-forward-branch
  "Forward conditional branches extend intervals across the target gap."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-jump-zero :reg :r1 :label "L1")
                             (make-vm-const :dst :r2 :value 2)
                             (make-vm-label :name "L1")
                             (make-vm-add :dst :r3 :lhs :r0 :rhs :r2)
                             (make-vm-halt :reg :r3)))
         (intervals (compute-live-intervals instructions))
         (r0-int (find :r0 intervals :key #'interval-vreg)))
    (assert-false (null r0-int))
    (assert-= 0 (interval-start r0-int))
    (assert-= 4 (interval-end r0-int))))

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

(deftest regalloc-coalesces-simple-move
  "A move whose source dies at the move point reuses the same physical register for dst."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-move :dst :r1 :src :r0)
                             (make-vm-halt :reg :r1)))
         (result (allocate-registers instructions *x86-64-calling-convention*)))
    (assert-eq (regalloc-lookup result :r0)
               (regalloc-lookup result :r1))))

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
  (assert-equal '(:xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7)
                (cl-cc::cc-fp-arg-registers *x86-64-calling-convention*))
  (assert-eq :xmm0 (cl-cc::cc-fp-return-register *x86-64-calling-convention*))
  (assert-eq :x0 (cc-return-register *aarch64-calling-convention*))
  (assert-equal '(:v0 :v1 :v2 :v3 :v4 :v5 :v6 :v7)
                (cl-cc::cc-fp-arg-registers *aarch64-calling-convention*))
  (assert-eq :v0 (cl-cc::cc-fp-return-register *aarch64-calling-convention*)))

(deftest regalloc-tail-call-has-no-def
  "vm-tail-call does not reserve a destination register in regalloc."
  (let ((inst (cl-cc:make-vm-tail-call :dst :r9 :func :r0 :args '(:r1 :r2))))
    (assert-equal nil (instruction-defs inst))
    (assert-equal '(:r0 :r1 :r2) (instruction-uses inst))))

(deftest regalloc-prefers-return-register
  "A value returned by vm-ret is preferentially allocated to the ABI return register."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 42)
                             (make-vm-ret :reg :r0)))
         (result (allocate-registers instructions *x86-64-calling-convention*)))
    (assert-eq :rax (regalloc-lookup result :r0))))

(deftest regalloc-prefers-argument-registers
  "Live-in parameter-like virtual registers prefer ABI argument registers when available."
  (let* ((instructions (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-halt :reg :r2)))
         (result (allocate-registers instructions *x86-64-calling-convention*)))
    (assert-eq :rdi (regalloc-lookup result :r0))
    (assert-eq :rsi (regalloc-lookup result :r1))))

(deftest regalloc-recycles-argument-register
  "A dead argument register is reused by a later temporary interval."
  (let* ((instructions (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-const :dst :r3 :value 7)
                             (make-vm-halt :reg :r3)))
         (result (allocate-registers instructions *x86-64-calling-convention*)))
    (assert-eq :rsi (regalloc-lookup result :r1))
    (assert-eq :rsi (regalloc-lookup result :r3))))

(deftest regalloc-prefers-callee-saved-across-call
  "A value live across a call prefers a callee-saved register when one is free."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-call :dst :r2 :func :r3 :args '(:r4))
                             (make-vm-add :dst :r5 :lhs :r0 :rhs :r2)
                             (make-vm-halt :reg :r5)))
         (intervals (compute-live-intervals instructions))
         (r0-int (find :r0 intervals :key #'cl-cc::interval-vreg))
         (result (allocate-registers instructions *x86-64-calling-convention*)))
    (assert-true (cl-cc::interval-crosses-call-p r0-int))
    (assert-true (member (regalloc-lookup result :r0)
                         (cl-cc::cc-callee-saved *x86-64-calling-convention*)
                         :test #'eq))))

(deftest regalloc-biased-spill-selection
  "When spilling is required, the allocator keeps the interval with the nearest next use."
  (let* ((cc (cl-cc::make-calling-convention
              :gpr-pool '(:rdi :rsi)
              :caller-saved '(:rdi :rsi)
              :callee-saved nil
              :arg-registers '(:rdi :rsi)
              :return-register :rax
              :scratch-register :r11))
         (instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-const :dst :r1 :value 2)
                             (make-vm-const :dst :r2 :value 3)
                             (make-vm-move :dst :r3 :src :r0)
                             (make-vm-move :dst :r4 :src :r2)
                             (make-vm-const :dst :r5 :value 5)
                             (make-vm-const :dst :r6 :value 6)
                             (make-vm-move :dst :r7 :src :r1)
                             (make-vm-halt :reg :r0)))
         (result (allocate-registers instructions cc)))
    (assert-false (null (regalloc-lookup result :r0)))
    (assert-false (null (gethash :r1 (cl-cc::regalloc-spill-map result))))
    (assert-= 2 (cl-cc::regalloc-spill-count result))))

(deftest regalloc-spill-rewrite-uses-distinct-scratch-registers
  "Two spilled source operands in one instruction are rewritten to distinct scratch registers."
  (let* ((assignment (make-hash-table :test #'eq))
         (spill-map (make-hash-table :test #'eq))
         (inst (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)))
    (setf (gethash :r3 assignment) :rdi)
    (setf (gethash :r1 spill-map) 1)
    (setf (gethash :r2 spill-map) 2)
    (let* ((out (cl-cc::insert-spill-code (list inst) assignment spill-map *x86-64-calling-convention*))
           (loads (remove-if-not #'cl-cc::vm-spill-load-p out))
           (rewritten (find-if #'cl-cc::vm-add-p out)))
      (assert-equal 2 (length loads))
      (assert-false (eq (cl-cc::vm-spill-dst (first loads)) (cl-cc::vm-spill-dst (second loads))))
      (assert-false (eq (vm-lhs rewritten) (vm-rhs rewritten))))))

(deftest regalloc-spill-rewrite-separates-src-and-dst-scratch
  "A spilled source and spilled destination do not share the same scratch register."
  (let* ((assignment (make-hash-table :test #'eq))
         (spill-map (make-hash-table :test #'eq))
         (inst (make-vm-move :dst :r2 :src :r1)))
    (setf (gethash :r1 spill-map) 1)
    (setf (gethash :r2 spill-map) 2)
    (let* ((out (cl-cc::insert-spill-code (list inst) assignment spill-map *x86-64-calling-convention*))
           (load (find-if #'cl-cc::vm-spill-load-p out))
           (store (find-if #'cl-cc::vm-spill-store-p out))
           (rewritten (find-if #'cl-cc::vm-move-p out)))
      (assert-false (null load))
      (assert-false (null store))
      (assert-false (eq (cl-cc::vm-spill-dst load) (cl-cc::vm-spill-src store)))
      (assert-eq (vm-src rewritten) (cl-cc::vm-spill-dst load))
      (assert-eq (vm-dst rewritten) (cl-cc::vm-spill-src store)))))

(deftest regalloc-rematerializes-spilled-constants
  "A spilled vm-const use is rematerialized as vm-const instead of vm-spill-load."
  (let* ((assignment (make-hash-table :test #'eq))
         (spill-map (make-hash-table :test #'eq))
         (remat-map (make-hash-table :test #'eq))
         (inst (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)))
    (setf (gethash :r3 assignment) :rdi)
    (setf (gethash :r1 spill-map) 1)
    (setf (gethash :r2 spill-map) 2)
    (setf (gethash :r1 remat-map) 42)
    (let* ((out (cl-cc::insert-spill-code (list inst) assignment spill-map *x86-64-calling-convention* remat-map))
           (const-inst (find-if #'cl-cc::vm-const-p out))
           (load-inst (find-if #'cl-cc::vm-spill-load-p out)))
      (assert-false (null const-inst))
      (assert-equal 42 (cl-cc:vm-value const-inst))
      (assert-false (null load-inst))
      (assert-false (eq (cl-cc:vm-dst const-inst) (cl-cc::vm-spill-dst load-inst))))))


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
