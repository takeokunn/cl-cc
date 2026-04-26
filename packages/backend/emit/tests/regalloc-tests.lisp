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
          ("vm-label"     (make-vm-label     :name "L1")                           nil    nil))
  (inst expected-defs expected-uses)
  (assert-equal expected-defs (instruction-defs inst))
  (assert-equal expected-uses (instruction-uses inst)))

;;; Liveness Analysis Tests

(deftest regalloc-liveness-cases
  "Liveness: 3 overlapping intervals; disjoint intervals don't overlap; forward branch extends intervals."
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
      (assert-= 3 (interval-end r2-int))))
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-halt :reg :r0)
                             (make-vm-const :dst :r1 :value 2)
                             (make-vm-halt :reg :r1)))
         (intervals (compute-live-intervals instructions)))
    (assert-= 2 (length intervals))
    (let ((r0-int (find :r0 intervals :key #'interval-vreg))
          (r1-int (find :r1 intervals :key #'interval-vreg)))
      (assert-true (<= (interval-end r0-int) (interval-start r1-int)))))
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

(deftest regalloc-allocate-cases
  "Simple allocation fits in physical regs; move coalescing reuses same physical reg."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-const :dst :r1 :value 2)
                             (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-halt :reg :r2)))
         (result (allocate-registers instructions *x86-64-calling-convention*)))
    (assert-= 0 (regalloc-spill-count result))
    (assert-false (null (regalloc-lookup result :r0)))
    (assert-false (null (regalloc-lookup result :r1)))
    (assert-false (null (regalloc-lookup result :r2)))
    (assert-false (eq (regalloc-lookup result :r0)
                      (regalloc-lookup result :r1))))
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-move :dst :r1 :src :r0)
                             (make-vm-halt :reg :r1)))
         (result (allocate-registers instructions *x86-64-calling-convention*)))
    (assert-eq (regalloc-lookup result :r0)
               (regalloc-lookup result :r1))))

(deftest-each regalloc-zero-spill-cases
  "Allocation produces zero spills for reuse and empty-instruction scenarios."
  :cases (("reuse"    (list (make-vm-const :dst :r0 :value 1)
                            (make-vm-move :dst :r1 :src :r0)
                            (make-vm-const :dst :r2 :value 2)
                            (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)
                            (make-vm-halt :reg :r3)))
          ("no-insts" nil))
  (instructions)
  (let ((result (allocate-registers instructions *x86-64-calling-convention*)))
    (assert-= 0 (regalloc-spill-count result))))


(deftest regalloc-calling-convention-exists
  "Calling conventions are properly defined."
  (assert-false (null *x86-64-calling-convention*))
  (assert-false (null *aarch64-calling-convention*))
  (assert-= 13 (length (cc-gpr-pool *x86-64-calling-convention*)))
  (assert-eq :rax (cc-return-register *x86-64-calling-convention*))
  (assert-eq :r11 (cc-scratch-register *x86-64-calling-convention*))
  (assert-equal '(:xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7)
                (cl-cc/emit::cc-fp-arg-registers *x86-64-calling-convention*))
  (assert-eq :xmm0 (cl-cc/emit::cc-fp-return-register *x86-64-calling-convention*))
  (assert-eq :x0 (cc-return-register *aarch64-calling-convention*))
  (assert-equal '(:v0 :v1 :v2 :v3 :v4 :v5 :v6 :v7)
                (cl-cc/emit::cc-fp-arg-registers *aarch64-calling-convention*))
  (assert-eq :v0 (cl-cc/emit::cc-fp-return-register *aarch64-calling-convention*)))

(deftest regalloc-prefers-fp-registers-for-float-vregs
  "Float virtual registers allocate from the FP register class when provided.
Verify each float vreg maps to some XMM physical register (not an integer
reg) and that they're distinct — the exact XMM index depends on internal
allocator priority (xmm0 may be reserved for calling convention)."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1.0d0)
                             (make-vm-const :dst :r1 :value 2.0d0)
                             (make-vm-float-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-halt :reg :r2)))
         (float-vregs (let ((ht (make-hash-table :test #'eq)))
                        (setf (gethash :r0 ht) t
                              (gethash :r1 ht) t
                              (gethash :r2 ht) t)
                        ht))
         (result (allocate-registers instructions *x86-64-calling-convention* float-vregs)))
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

(deftest regalloc-tail-call-has-no-def
  "vm-tail-call does not reserve a destination register in regalloc."
  (let ((inst (cl-cc:make-vm-tail-call :dst :r9 :func :r0 :args '(:r1 :r2))))
    (assert-equal nil (instruction-defs inst))
    (assert-equal '(:r0 :r1 :r2) (instruction-uses inst))))

(deftest regalloc-abi-register-preference-cases
  "ABI preferences: return→:rax; live-in params→:rdi/:rsi; dead arg register recycled."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 42)
                             (make-vm-ret :reg :r0)))
         (result (allocate-registers instructions *x86-64-calling-convention*)))
    (assert-eq :rax (regalloc-lookup result :r0)))
  (let* ((instructions (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-halt :reg :r2)))
         (result (allocate-registers instructions *x86-64-calling-convention*)))
    (assert-eq :rdi (regalloc-lookup result :r0))
    (assert-eq :rsi (regalloc-lookup result :r1)))
  (let* ((instructions (list (make-vm-add :dst :r2 :lhs :r0 :rhs :r1)
                             (make-vm-const :dst :r3 :value 7)
                             (make-vm-halt :reg :r3)))
         (result (allocate-registers instructions *x86-64-calling-convention*)))
    (assert-eq :rsi (regalloc-lookup result :r1))
    (assert-eq :rsi (regalloc-lookup result :r3))))

(deftest regalloc-spill-pressure-cases
  "Live-across-call value prefers callee-saved; biased spill keeps nearest-use interval."
  (let* ((instructions (list (make-vm-const :dst :r0 :value 1)
                             (make-vm-call :dst :r2 :func :r3 :args '(:r4))
                             (make-vm-add :dst :r5 :lhs :r0 :rhs :r2)
                             (make-vm-halt :reg :r5)))
         (intervals (compute-live-intervals instructions))
         (r0-int (find :r0 intervals :key #'cl-cc::interval-vreg))
         (result (allocate-registers instructions *x86-64-calling-convention*)))
    (assert-true (cl-cc/emit::interval-crosses-call-p r0-int))
    (assert-true (member (regalloc-lookup result :r0)
                         (cl-cc::cc-callee-saved *x86-64-calling-convention*)
                         :test #'eq)))
  (let* ((cc (cl-cc/emit::make-calling-convention
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

(deftest regalloc-spill-rewrite-cases
  "Spill rewrite: two spilled srcs use distinct scratch regs; spilled src/dst do not share scratch."
  (let* ((assignment (make-hash-table :test #'eq))
         (spill-map (make-hash-table :test #'eq))
         (inst (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)))
    (setf (gethash :r3 assignment) :rdi)
    (setf (gethash :r1 spill-map) 1)
    (setf (gethash :r2 spill-map) 2)
    (let* ((out (cl-cc/emit::insert-spill-code (list inst) assignment spill-map *x86-64-calling-convention*))
           (loads (remove-if-not #'cl-cc/emit::vm-spill-load-p out))
           (rewritten (find-if #'cl-cc::vm-add-p out)))
      (assert-equal 2 (length loads))
      (assert-false (eq (cl-cc/emit::vm-spill-dst (first loads)) (cl-cc/emit::vm-spill-dst (second loads))))
      (assert-false (eq (vm-lhs rewritten) (vm-rhs rewritten)))))
  (let* ((assignment (make-hash-table :test #'eq))
         (spill-map (make-hash-table :test #'eq))
         (inst (make-vm-move :dst :r2 :src :r1)))
    (setf (gethash :r1 spill-map) 1)
    (setf (gethash :r2 spill-map) 2)
    (let* ((out (cl-cc/emit::insert-spill-code (list inst) assignment spill-map *x86-64-calling-convention*))
           (load (find-if #'cl-cc/emit::vm-spill-load-p out))
           (store (find-if #'cl-cc/emit::vm-spill-store-p out))
           (rewritten (find-if #'cl-cc::vm-move-p out)))
      (assert-false (null load))
      (assert-false (null store))
      (assert-false (eq (cl-cc/emit::vm-spill-dst load) (cl-cc/emit::vm-spill-src store)))
      (assert-eq (vm-src rewritten) (cl-cc/emit::vm-spill-dst load))
      (assert-eq (vm-dst rewritten) (cl-cc/emit::vm-spill-src store)))))

(deftest regalloc-integration-cases
  "Rematerializes spilled constants as vm-const; compile+allocate produces zero spills."
  (let* ((assignment (make-hash-table :test #'eq))
         (spill-map (make-hash-table :test #'eq))
         (remat-map (make-hash-table :test #'eq))
         (inst (make-vm-add :dst :r3 :lhs :r1 :rhs :r2)))
    (setf (gethash :r3 assignment) :rdi)
    (setf (gethash :r1 spill-map) 1)
    (setf (gethash :r2 spill-map) 2)
    (setf (gethash :r1 remat-map) 42)
    (let* ((out (cl-cc/emit::insert-spill-code (list inst) assignment spill-map *x86-64-calling-convention* remat-map))
           (const-inst (find-if #'cl-cc::vm-const-p out))
           (load-inst (find-if #'cl-cc/emit::vm-spill-load-p out)))
      (assert-false (null const-inst))
      (assert-equal 42 (cl-cc:vm-value const-inst))
      (assert-false (null load-inst))
      (assert-false (eq (cl-cc:vm-dst const-inst) (cl-cc/emit::vm-spill-dst load-inst)))))
  (let* ((result (compile-expression '(+ 1 2) :target :vm))
         (program (compilation-result-program result))
         (instructions (vm-program-instructions program))
         (alloc (allocate-registers instructions *x86-64-calling-convention*)))
    (assert-= 0 (regalloc-spill-count alloc))
    (let ((all-vregs nil))
      (dolist (inst instructions)
        (dolist (v (instruction-defs inst)) (when v (pushnew v all-vregs)))
        (dolist (v (instruction-uses inst)) (when v (pushnew v all-vregs))))
      (dolist (vreg all-vregs)
        (assert-false (null (regalloc-lookup alloc vreg)))))))

;;; ─── lsa-state struct + helpers (extracted from linear-scan-allocate) ─────────

(deftest lsa-state-initial-values
  "make-lsa-state: hash-tables empty, count zero, lists nil."
  (let ((s (cl-cc/emit::make-lsa-state :free-regs '(:rax :rbx) :free-fp-regs '(:xmm0))))
    (assert-= 0 (hash-table-count (cl-cc/emit::lsa-assignment s)))
    (assert-= 0 (hash-table-count (cl-cc/emit::lsa-spill-map s)))
    (assert-= 0 (cl-cc/emit::lsa-spill-count s))
    (assert-null (cl-cc/emit::lsa-active s))
    (assert-equal '(:rax :rbx) (cl-cc/emit::lsa-free-regs s))
    (assert-equal '(:xmm0) (cl-cc/emit::lsa-free-fp-regs s))))

(deftest lsa-interval-pool-selects-by-class
  "%lsa-interval-pool returns free-fp-regs for FP intervals, free-regs otherwise."
  (let ((s     (cl-cc/emit::make-lsa-state :free-regs '(:rax) :free-fp-regs '(:xmm0)))
        (i-gpr (cl-cc/emit::make-live-interval :vreg :r0))
        (i-fp  (cl-cc/emit::make-live-interval :vreg :r1 :fp-p t)))
    (assert-equal '(:rax)  (cl-cc/emit::%lsa-interval-pool s i-gpr))
    (assert-equal '(:xmm0) (cl-cc/emit::%lsa-interval-pool s i-fp))))

(deftest lsa-set-interval-pool-updates-correct-field
  "%lsa-set-interval-pool mutates only the matching pool slot."
  (let ((s (cl-cc/emit::make-lsa-state :free-regs '(:rax) :free-fp-regs '(:xmm0)))
        (i-gpr (cl-cc/emit::make-live-interval :vreg :r0))
        (i-fp  (cl-cc/emit::make-live-interval :vreg :r1 :fp-p t)))
    (cl-cc/emit::%lsa-set-interval-pool s i-gpr '(:rcx))
    (assert-equal '(:rcx) (cl-cc/emit::lsa-free-regs s))
    (assert-equal '(:xmm0) (cl-cc/emit::lsa-free-fp-regs s))
    (cl-cc/emit::%lsa-set-interval-pool s i-fp '(:xmm1))
    (assert-equal '(:xmm1) (cl-cc/emit::lsa-free-fp-regs s))))

(deftest lsa-spill-current-increments-count
  "%lsa-spill-current assigns next slot and records it in spill-map."
  (let ((s   (cl-cc/emit::make-lsa-state))
        (int (cl-cc/emit::make-live-interval :vreg :r5)))
    (cl-cc/emit::%lsa-spill-current s int)
    (assert-= 1 (cl-cc/emit::lsa-spill-count s))
    (assert-= 1 (gethash :r5 (cl-cc/emit::lsa-spill-map s)))))

(deftest lsa-expire-old-returns-regs-to-pool
  "%lsa-expire-old removes ended intervals from active and returns physical regs."
  (let ((s    (cl-cc/emit::make-lsa-state :free-regs '()))
        (done (cl-cc/emit::make-live-interval :vreg :r0 :start 0 :end 2 :phys-reg :rax))
        (live (cl-cc/emit::make-live-interval :vreg :r1 :start 0 :end 5 :phys-reg :rbx))
        (cur  (cl-cc/emit::make-live-interval :vreg :r2 :start 3 :end 8)))
    (setf (cl-cc/emit::lsa-active s) (list done live))
    (cl-cc/emit::%lsa-expire-old s cur)
    (assert-false (member done (cl-cc/emit::lsa-active s)))
    (assert-true  (member live (cl-cc/emit::lsa-active s)))
    (assert-equal '(:rax) (cl-cc/emit::lsa-free-regs s))))

(deftest lsa-best-spill-candidate-returns-farthest-use
  "%lsa-best-spill-candidate picks the active interval with the farthest next use."
  (let* ((cur   (cl-cc/emit::make-live-interval :vreg :r2 :start 3 :end 10))
         (near  (cl-cc/emit::make-live-interval :vreg :r0 :start 0 :end 8
                                                :use-positions '(4) :phys-reg :rax))
         (far   (cl-cc/emit::make-live-interval :vreg :r1 :start 0 :end 12
                                                :use-positions '(9) :phys-reg :rbx))
         (s     (cl-cc/emit::make-lsa-state :active (list near far))))
    (assert-eq far (cl-cc/emit::%lsa-best-spill-candidate s cur))))

(deftest lsa-best-spill-candidate-returns-current-when-no-active
  "%lsa-best-spill-candidate returns INTERVAL itself when there are no active intervals."
  (let* ((cur (cl-cc/emit::make-live-interval :vreg :r0 :start 5 :end 10))
         (s   (cl-cc/emit::make-lsa-state :active nil)))
    (assert-eq cur (cl-cc/emit::%lsa-best-spill-candidate s cur))))
