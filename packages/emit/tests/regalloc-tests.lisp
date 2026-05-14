;;;; tests/regalloc-tests.lisp - Register Allocator Tests

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; Instruction Def/Use Tests
(deftest-each regalloc-defs-and-uses
  "instruction-defs and instruction-uses return correct register sets for each instruction type."
  :cases (("vm-const"     (make-vm-const     :dst :r0 :value 42)                 '(:r0) nil)
          ("vm-binop"     (make-vm-add       :dst :r2 :lhs :r0 :rhs :r1)         '(:r2) '(:r0 :r1))
          ("vm-call"      (make-vm-call      :dst :r3 :func :r0 :args '(:r1 :r2)) '(:r3) '(:r0 :r1 :r2))
          ("vm-jump-zero" (make-vm-jump-zero :reg :r0 :label "L1")                nil    '(:r0))
          ("vm-label"     (make-vm-label     :name "L1")                           nil    nil)
          ("vm-tail-call" (cl-cc:make-vm-tail-call :dst :r9 :func :r0 :args '(:r1 :r2)) nil '(:r0 :r1 :r2)))
  (inst expected-defs expected-uses)
  (assert-equal expected-defs (instruction-defs inst))
  (assert-equal expected-uses (instruction-uses inst)))

;;; Liveness Analysis Tests

(deftest regalloc-liveness-three-overlapping-intervals
  "Liveness analysis produces 3 intervals with correct start/end positions."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-const :dst :r1 :value 2)
                             (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-halt :reg :r2)))
         (intervals (compute-live-intervals instructions)))
    (assert-= 3 (length intervals))
    (let ((r0-int (find :r0 intervals :key #'interval-vreg)))
      (assert-false (null r0-int))
      (assert-= 0 (interval-start r0-int))
      (assert-= 2 (interval-end r0-int)))
    (let ((r1-int (find :r1 intervals :key #'interval-vreg)))
      (assert-false (null r1-int))
      (assert-= 1 (interval-start r1-int))
      (assert-= 2 (interval-end r1-int)))
    (let ((r2-int (find :r2 intervals :key #'interval-vreg)))
      (assert-false (null r2-int))
      (assert-= 2 (interval-start r2-int))
      (assert-= 3 (interval-end r2-int)))))

(deftest regalloc-liveness-disjoint-intervals-do-not-overlap
  "Disjoint live intervals: r0 ends before r1 begins."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-halt :reg :r0)
                             (make-vm-const :dst :r1 :value 2)
                             (make-vm-halt :reg :r1)))
         (intervals (compute-live-intervals instructions)))
    (assert-= 2 (length intervals))
    (let ((r0-int (find :r0 intervals :key #'interval-vreg))
          (r1-int (find :r1 intervals :key #'interval-vreg)))
      (assert-true (<= (interval-end r0-int) (interval-start r1-int))))))

(deftest regalloc-liveness-forward-branch-extends-interval
  "Forward branch extends r0 interval to cover the jump target at index 4."
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

(deftest regalloc-interprocedural-hints-detect-leaf-and-leaf-callee-chain
  "Interprocedural hint oracle marks leaf functions and leaf-callee chains conservatively."
  (let* ((insts (list
                 ;; leaf callee
                 (make-vm-label :name "leaf")
                 (make-vm-const :dst :r0 :value 1)
                 (make-vm-ret :reg :r0)
                 ;; non-leaf caller that calls leaf via func-ref
                 (make-vm-label :name "caller")
                 (make-vm-func-ref :dst :f :label "leaf")
                 (make-vm-call :dst :r1 :func :f :args nil)
                 (make-vm-ret :reg :r1)
                 ;; root calls caller (callee not leaf)
                 (make-vm-label :name "root")
                 (make-vm-func-ref :dst :g :label "caller")
                 (make-vm-call :dst :r2 :func :g :args nil)
                 (make-vm-ret :reg :r2)))
         (hints (cl-cc/regalloc::regalloc-compute-interprocedural-hints insts)))
    (assert-true (getf (gethash "leaf" hints) :leaf-p))
    (assert-true (getf (gethash "caller" hints) :leaf-callee-chain-p))
    (assert-false (getf (gethash "root" hints) :leaf-callee-chain-p))))

;;; Linear Scan Allocation Tests

(deftest regalloc-allocate-fits-in-physical-regs-with-distinct-assignments
  "3-vreg program fits in physical regs: 0 spills; all vregs assigned distinct physical regs."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-const :dst :r1 :value 2)
                             (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-halt :reg :r2)))
         (result (allocate-registers instructions *x86-64-target*)))
    (assert-= 0 (regalloc-spill-count result))
    (assert-false (null (regalloc-lookup result :r0)))
    (assert-false (null (regalloc-lookup result :r1)))
    (assert-false (null (regalloc-lookup result :r2)))
    (assert-false (eq (regalloc-lookup result :r0)
                      (regalloc-lookup result :r1)))))

(deftest regalloc-allocate-coalesces-move-to-same-physical-reg
  "A vm-move is coalesced: source and destination share the same physical register."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-move :dst :r1 :src :r0)
                             (make-vm-halt :reg :r1)))
         (result (allocate-registers instructions *x86-64-target*)))
    (assert-eq (regalloc-lookup result :r0)
               (regalloc-lookup result :r1))))

(deftest regalloc-allocate-zero-spills-with-register-reuse
  "4-vreg program with move+reuse produces 0 spills."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-move :dst :r1 :src :r0)
                             (make-vm-const :dst :r2 :value 2)
                             (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)
                             (make-vm-halt :reg :r3)))
         (result (allocate-registers instructions *x86-64-target*)))
    (assert-= 0 (regalloc-spill-count result))))

(deftest regalloc-allocate-empty-sequence-has-zero-spills
  "Allocating an empty instruction sequence produces 0 spills."
  (let ((result (allocate-registers nil *x86-64-target*)))
    (assert-= 0 (regalloc-spill-count result))))

(deftest regalloc-x86-and-aarch64-convention-properties
  "Both target descriptors are non-nil with correct GPR counts, return registers, and FP registers."
  (assert-false (null *x86-64-target*))
  (assert-false (null *aarch64-target*))
  (assert-= 13 (length (target-allocatable-regs *x86-64-target*)))
  (assert-eq :rax (target-ret-reg *x86-64-target*))
  (assert-eq :r11 (first (target-scratch-regs *x86-64-target*)))
  (assert-equal '(:xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7)
                (target-fp-arg-regs *x86-64-target*))
  (assert-eq :xmm0 (target-fp-ret-reg *x86-64-target*))
  (assert-eq :x0 (target-ret-reg *aarch64-target*))
  (assert-equal '(:v0 :v1 :v2 :v3 :v4 :v5 :v6 :v7)
                (target-fp-arg-regs *aarch64-target*))
  (assert-eq :v0 (target-fp-ret-reg *aarch64-target*)))

(deftest regalloc-float-vregs-allocated-to-distinct-xmm-registers
  "Float vregs all get XMM registers; all three are distinct."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1.0d0)
                             (make-vm-const :dst :r1 :value 2.0d0)
                             (make-vm-float-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-halt :reg :r2)))
         (float-vregs (let ((ht (make-hash-table :test #'eq)))
                        (setf (gethash :r0 ht) t
                              (gethash :r1 ht) t
                              (gethash :r2 ht) t)
                        ht))
         (result (allocate-registers instructions *x86-64-target* float-vregs)))
    (flet ((xmm-p (k)
             (and (keywordp k)
                  (let ((s (symbol-name k)))
                    (and (>= (length s) 3)
                         (string= "XMM" (subseq s 0 3)))))))
      (let ((p0 (regalloc-lookup result :r0))
            (p1 (regalloc-lookup result :r1))
            (p2 (regalloc-lookup result :r2)))
        (assert-true (xmm-p p0))
        (assert-true (xmm-p p1))
        (assert-true (xmm-p p2))
        (assert-false (eq p0 p1))))))

(deftest regalloc-abi-return-value-prefers-rax
  "A vreg used only in vm-ret is assigned to the ABI return register :rax."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 42)
                             (make-vm-ret :reg :r0)))
         (result (allocate-registers instructions *x86-64-target*)))
    (assert-eq :rax (regalloc-lookup result :r0))))

(deftest regalloc-abi-live-in-params-use-arg-registers
  "Live-in vregs with no definition are assigned to ABI arg registers :rdi and :rsi."
  (let* ((instructions (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-halt :reg :r2)))
         (result (allocate-registers instructions *x86-64-target*)))
    (assert-eq :rdi (regalloc-lookup result :r0))
    (assert-eq :rsi (regalloc-lookup result :r1))))

(deftest regalloc-abi-dead-arg-register-is-recycled
  "An arg register (:rsi) released after its last use is reused for a later vreg."
  (let* ((instructions (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-const :dst :r3 :value 7)
                             (make-vm-halt :reg :r3)))
         (result (allocate-registers instructions *x86-64-target*)))
    (assert-eq :rsi (regalloc-lookup result :r1))
    (assert-eq :rsi (regalloc-lookup result :r3))))

(deftest regalloc-spill-live-across-call-prefers-callee-saved
  "A value live across a vm-call is assigned to a callee-saved register."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-call :dst :r2 :func :r3 :args '(:r4))
                             (make-vm-add :dst :r5 :lhs :r0 :rhs :r2)
                             (make-vm-halt :reg :r5)))
         (intervals (compute-live-intervals instructions))
         (r0-int (find :r0 intervals :key #'cl-cc:interval-vreg))
         (result (allocate-registers instructions *x86-64-target*)))
    (assert-true (cl-cc/regalloc::interval-crosses-call-p r0-int))
    (assert-true (member (regalloc-lookup result :r0)
                         (cl-cc/target:target-callee-saved *x86-64-target*)
                         :test #'eq))))

(deftest regalloc-spill-pressure-exceeds-pool-causes-spills
  "When more vregs than physical registers exist, at least some are spilled."
  (let* ((cc (cl-cc/target:make-target-desc
              :name :x86-64
              :gpr-names #(:rdi :rsi :r11)
              :arg-regs '(:rdi :rsi)
              :ret-reg :rax
              :callee-saved nil
              :scratch-regs '(:r11)))
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
    (assert-false (null (gethash :r1 (cl-cc:regalloc-spill-map result))))
    (assert-= 2 (cl-cc:regalloc-spill-count result))))

(deftest regalloc-spill-rewrite-two-spilled-srcs-use-distinct-scratch-regs
  "Two spilled srcs in vm-add get distinct scratch register loads."
  (let* ((assignment (make-hash-table :test #'eq))
         (spill-map (make-hash-table :test #'eq))
         (inst (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)))
    (setf (gethash :r3 assignment) :rdi)
    (setf (gethash :r1 spill-map) 1)
    (setf (gethash :r2 spill-map) 2)
    (let* ((out (cl-cc/regalloc::insert-spill-code (list inst) assignment spill-map *x86-64-target*))
           (loads (remove-if-not #'cl-cc/regalloc::vm-spill-load-p out))
           (rewritten (find-if #'cl-cc:vm-add-p out)))
      (assert-equal 2 (length loads))
      (assert-false (eq (cl-cc/regalloc::vm-spill-dst (first loads)) (cl-cc/regalloc::vm-spill-dst (second loads))))
      (assert-false (eq (vm-lhs rewritten) (vm-rhs rewritten))))))

(deftest regalloc-spill-rewrite-mul-high-avoids-internal-r11-scratch
  "Spilled vm-integer-mul-high-u operands never rewrite to the emitter's internal R11 scratch."
  (let* ((assignment (make-hash-table :test #'eq))
         (spill-map (make-hash-table :test #'eq))
         (inst (cl-cc:make-vm-integer-mul-high-u :dst :r3 :lhs :r1 :rhs :r2)))
    (setf (gethash :r1 spill-map) 1)
    (setf (gethash :r2 spill-map) 2)
    (setf (gethash :r3 spill-map) 3)
    (let* ((out (cl-cc/regalloc::insert-spill-code (list inst) assignment spill-map *x86-64-target*))
           (rewritten (find-if (lambda (item)
                                 (typep item 'cl-cc/vm::vm-integer-mul-high-u))
                               out)))
      (assert-false (null rewritten))
      (assert-false (eq (vm-lhs rewritten) :r11))
      (assert-false (eq (vm-rhs rewritten) :r11))
      (assert-false (eq (vm-dst rewritten) :r11))
      (assert-false (eq (vm-lhs rewritten) (vm-rhs rewritten))))))

(deftest regalloc-spill-rewrite-spilled-src-and-dst-use-separate-scratch
  "In a spilled vm-move, load scratch and store scratch are different; rewritten src/dst wired correctly."
  (let* ((assignment (make-hash-table :test #'eq))
         (spill-map (make-hash-table :test #'eq))
         (inst (make-vm-move :dst :r2 :src :r1)))
    (setf (gethash :r1 spill-map) 1)
    (setf (gethash :r2 spill-map) 2)
    (let* ((out (cl-cc/regalloc::insert-spill-code (list inst) assignment spill-map *x86-64-target*))
           (load (find-if #'cl-cc/regalloc::vm-spill-load-p out))
           (store (find-if #'cl-cc/regalloc::vm-spill-store-p out))
           (rewritten (find-if #'cl-cc:vm-move-p out)))
      (assert-false (null load))
      (assert-false (null store))
      (assert-false (eq (cl-cc/regalloc::vm-spill-dst load) (cl-cc/regalloc::vm-spill-src store)))
      (assert-eq (vm-src rewritten) (cl-cc/regalloc::vm-spill-dst load))
      (assert-eq (vm-dst rewritten) (cl-cc/regalloc::vm-spill-src store)))))

(deftest regalloc-integration-rematerializes-spilled-constant-as-vm-const
  "A rematerializable spilled vreg gets a vm-const instead of a spill-load for that vreg."
  (let* ((assignment (make-hash-table :test #'eq))
         (spill-map (make-hash-table :test #'eq))
         (remat-map (make-hash-table :test #'eq))
         (inst (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)))
    (setf (gethash :r3 assignment) :rdi)
    (setf (gethash :r1 spill-map) 1)
    (setf (gethash :r2 spill-map) 2)
    (setf (gethash :r1 remat-map) 42)
    (let* ((out (cl-cc/regalloc::insert-spill-code (list inst) assignment spill-map *x86-64-target* remat-map))
           (const-inst (find-if #'cl-cc:vm-const-p out))
           (load-inst (find-if #'cl-cc/regalloc::vm-spill-load-p out)))
      (assert-false (null const-inst))
      (assert-equal 42 (cl-cc:vm-value const-inst))
      (assert-false (null load-inst))
      (assert-false (eq (cl-cc:vm-dst const-inst) (cl-cc/regalloc::vm-spill-dst load-inst))))))

(deftest regalloc-integration-compile-and-allocate-produces-zero-spills
  "compile-expression + allocate-registers on (+ 1 2): 0 spills; all vregs assigned."
  (let* ((result (compile-expression '(+ 1 2) :target :vm))
         (program (compilation-result-program result))
         (instructions (vm-program-instructions program))
         (alloc (allocate-registers instructions *x86-64-target*)))
    (assert-= 0 (regalloc-spill-count alloc))
    (let ((all-vregs nil))
      (dolist (inst instructions)
        (dolist (v (instruction-defs inst)) (when v (pushnew v all-vregs)))
        (dolist (v (instruction-uses inst)) (when v (pushnew v all-vregs))))
      (dolist (vreg all-vregs)
        (assert-false (null (regalloc-lookup alloc vreg)))))))

;;; ─── lsa-state struct + helpers ────────────────────────────────────────────────

(deftest lsa-state-initial-values-are-empty
  "Freshly created lsa-state has empty assignment/spill-map, zero spill-count, nil active, correct pools."
  (let ((s (cl-cc/regalloc::make-lsa-state :free-regs '(:rax :rbx) :free-fp-regs '(:xmm0))))
    (assert-= 0 (hash-table-count (cl-cc/regalloc::lsa-assignment s)))
    (assert-= 0 (hash-table-count (cl-cc/regalloc::lsa-spill-map s)))
    (assert-= 0 (cl-cc/regalloc::lsa-spill-count s))
    (assert-null (cl-cc/regalloc::lsa-active s))
    (assert-equal '(:rax :rbx) (cl-cc/regalloc::lsa-free-regs s))
    (assert-equal '(:xmm0) (cl-cc/regalloc::lsa-free-fp-regs s))))

(deftest lsa-state-pool-selection-and-mutation
  "%lsa-interval-pool selects gpr vs fp pool; %lsa-set-interval-pool mutates only the correct pool."
  (let ((s     (cl-cc/regalloc::make-lsa-state :free-regs '(:rax) :free-fp-regs '(:xmm0)))
        (i-gpr (cl-cc/regalloc::make-live-interval :vreg :r0))
        (i-fp  (cl-cc/regalloc::make-live-interval :vreg :r1 :fp-p t)))
    (assert-equal '(:rax)  (cl-cc/regalloc::%lsa-interval-pool s i-gpr))
    (assert-equal '(:xmm0) (cl-cc/regalloc::%lsa-interval-pool s i-fp))
    (cl-cc/regalloc::%lsa-set-interval-pool s i-gpr '(:rcx))
    (assert-equal '(:rcx)  (cl-cc/regalloc::lsa-free-regs s))
    (assert-equal '(:xmm0) (cl-cc/regalloc::lsa-free-fp-regs s))
    (cl-cc/regalloc::%lsa-set-interval-pool s i-fp '(:xmm1))
    (assert-equal '(:xmm1) (cl-cc/regalloc::lsa-free-fp-regs s))))

(deftest lsa-state-spill-current-increments-count-and-records-slot
  "%lsa-spill-current increments spill-count and records the slot in the spill-map."
  (let ((s   (cl-cc/regalloc::make-lsa-state))
        (int (cl-cc/regalloc::make-live-interval :vreg :r5)))
    (cl-cc/regalloc::%lsa-spill-current s int)
    (assert-= 1 (cl-cc/regalloc::lsa-spill-count s))
    (assert-= 1 (gethash :r5 (cl-cc/regalloc::lsa-spill-map s)))))

(deftest lsa-state-expire-old-removes-finished-intervals
  "%lsa-expire-old removes expired intervals from active and returns their physical regs."
  (let ((s    (cl-cc/regalloc::make-lsa-state :free-regs '()))
        (done (cl-cc/regalloc::make-live-interval :vreg :r0 :start 0 :end 2 :phys-reg :rax))
        (live (cl-cc/regalloc::make-live-interval :vreg :r1 :start 0 :end 5 :phys-reg :rbx))
        (cur  (cl-cc/regalloc::make-live-interval :vreg :r2 :start 3 :end 8)))
    (setf (cl-cc/regalloc::lsa-active s) (list done live))
    (cl-cc/regalloc::%lsa-expire-old s cur)
    (assert-false (member done (cl-cc/regalloc::lsa-active s)))
    (assert-true  (member live (cl-cc/regalloc::lsa-active s)))
    (assert-equal '(:rax) (cl-cc/regalloc::lsa-free-regs s))))

(deftest lsa-state-best-spill-candidate-returns-live-interval
  "%lsa-best-spill-candidate returns a live-interval from the active set."
  (let* ((cur   (cl-cc/regalloc::make-live-interval :vreg :r2 :start 3 :end 10))
         (near  (cl-cc/regalloc::make-live-interval :vreg :r0 :start 0 :end 8
                                                :use-positions '(4) :phys-reg :rax))
         (far   (cl-cc/regalloc::make-live-interval :vreg :r1 :start 0 :end 12
                                                :use-positions '(9) :phys-reg :rbx))
         (s     (cl-cc/regalloc::make-lsa-state :active (list near far))))
    (assert-type cl-cc/regalloc::live-interval
                 (cl-cc/regalloc::%lsa-best-spill-candidate s cur))))

(deftest lsa-state-best-spill-candidate-returns-current-when-active-empty
  "%lsa-best-spill-candidate returns the current interval when active list is empty."
  (let* ((cur (cl-cc/regalloc::make-live-interval :vreg :r0 :start 5 :end 10))
         (s   (cl-cc/regalloc::make-lsa-state :active nil)))
    (assert-eq cur (cl-cc/regalloc::%lsa-best-spill-candidate s cur))))
