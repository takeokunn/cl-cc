;;;; tests/unit/emit/x86-64-emit-tests.lisp — x86-64 Assembly Emit Tests
;;;;
;;;; Tests for src/emit/x86-64.lisp:
;;;; target base class, x86-64-target, target-register, emit-instruction methods,
;;;; *phys-reg-to-asm-string*, spill-store/spill-load emission

(in-package :cl-cc/test)

(defsuite x86-64-emit-suite :description "x86-64 assembly emit tests"
  :parent cl-cc-unit-suite)


(in-suite x86-64-emit-suite)
;;; ─── Helper: emit an instruction to string ─────────────────────────────────────

(defun %x86-emit (target inst)
  "Emit INST to a string using TARGET and return the result."
  (let ((s (make-string-output-stream)))
    (cl-cc/codegen::emit-instruction target inst s)
    (get-output-stream-string s)))

(defun %make-x86-target ()
  "Create a plain x86-64-target with no regalloc (fallback mapping)."
  (make-instance 'cl-cc/codegen::x86-64-target))

;;; ─── *phys-reg-to-asm-string* table ─────────────────────────────────────────

(deftest x86-phys-reg-table-has-14-entries
  "Physical register table maps all 14 x86-64 GP registers."
  (assert-equal 14 (length cl-cc/codegen::*phys-reg-to-asm-string*)))

(deftest-each x86-phys-reg-table-entries
  "Physical register table contains correct mappings."
  :cases (("rax" :rax "rax")
          ("rcx" :rcx "rcx")
          ("rdx" :rdx "rdx")
          ("rbx" :rbx "rbx")
          ("rsi" :rsi "rsi")
          ("rdi" :rdi "rdi")
          ("r8"  :r8  "r8")
          ("r15" :r15 "r15"))
  (phys-key expected-str)
  (assert-equal expected-str (cdr (assoc phys-key cl-cc/codegen::*phys-reg-to-asm-string*))))

;;; ─── target-register: fallback (no regalloc) ──────────────────────────────────

(deftest-each x86-target-register-fallback
  "Fallback target-register maps :R0..:R7 to the naive pool."
  :cases (("r0" :r0 "rax")
          ("r1" :r1 "rbx")
          ("r2" :r2 "rcx")
          ("r3" :r3 "rdx")
          ("r4" :r4 "r8")
          ("r5" :r5 "r9")
          ("r6" :r6 "r10")
          ("r7" :r7 "r11"))
  (vreg expected)
  (let ((tgt (%make-x86-target)))
    (assert-equal expected (cl-cc/codegen::target-register tgt vreg))))

(deftest x86-target-register-fallback-signals-error-for-r8-plus
  "Fallback (no regalloc) signals error for :R8 and above."
  (let ((tgt (%make-x86-target)))
    (assert-signals error (cl-cc/codegen::target-register tgt :r8))))

(deftest x86-target-register-with-regalloc-uses-assignment
  "With regalloc, known vreg returns mapped string; unknown vreg signals error."
  (let* ((ht (make-hash-table))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment ht))
         (tgt (make-instance 'cl-cc/codegen::x86-64-target :regalloc ra)))
    (setf (gethash :r0 ht) :rax)
    (assert-equal "rax" (cl-cc/codegen::target-register tgt :r0))
    (assert-signals error (cl-cc/codegen::target-register tgt :r99))))

;;; ─── emit-instruction methods ──────────────────────────────────────────────────

(deftest x86-emit-const-emits-mov-with-value
  "vm-const emits mov to rax with the immediate value 42."
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt (make-vm-const :dst :r0 :value 42))))
    (assert-true (search "mov" asm))
    (assert-true (search "rax" asm))
    (assert-true (search "42" asm))))

(deftest x86-emit-move-emits-mov-between-regs
  "vm-move emits mov from rbx (r1) to rax (r0)."
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt (make-vm-move :dst :r0 :src :r1))))
    (assert-true (search "mov" asm))
    (assert-true (search "rax" asm))
    (assert-true (search "rbx" asm))))

(deftest x86-emit-add-emits-mov-and-add-mnemonic
  "vm-add emits mov followed by add mnemonic."
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt (make-vm-add :dst :r0 :lhs :r1 :rhs :r2))))
    (assert-true (search "mov" asm))
    (assert-true (search "add" asm))))

(deftest-each x86-emit-arithmetic-mnemonics
  "vm-sub emits sub mnemonic; vm-mul emits imul mnemonic."
  :cases (("sub" (make-vm-sub :dst :r0 :lhs :r1 :rhs :r2) "sub")
          ("mul" (make-vm-mul :dst :r0 :lhs :r1 :rhs :r2) "imul"))
  (inst expected-mnemonic)
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt inst)))
    (assert-true (search expected-mnemonic asm))))

(deftest x86-emit-label-emits-align-and-colon-name
  "vm-label emits .align 4 followed by the label name with colon."
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt (make-vm-label :name "loop"))))
    (assert-true (search ".align 4" asm))
    (assert-true (search "loop:" asm))))

(deftest x86-emit-jump-emits-jmp-mnemonic
  "vm-jump emits jmp with the target label."
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt (make-vm-jump :label "done"))))
    (assert-true (search "jmp" asm))
    (assert-true (search "done" asm))))

(deftest x86-emit-jump-zero-emits-cmp-and-je
  "vm-jump-zero emits cmp then je with the target label."
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt (make-vm-jump-zero :reg :r0 :label "else"))))
    (assert-true (search "cmp" asm))
    (assert-true (search "je" asm))
    (assert-true (search "else" asm))))

(deftest x86-emit-halt-emits-mov-rax-and-ret
  "vm-halt emits mov rax and ret."
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt (make-vm-halt :reg :r0))))
    (assert-true (search "mov rax" asm))
    (assert-true (search "ret" asm))))

(deftest x86-emit-print-calls-rt-print-with-rdi
  "vm-print emits a call to rt-print with rdi as the argument."
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt (make-vm-print :reg :r0))))
    (assert-true (search "call rt-print" asm))
    (assert-true (search "rdi" asm))))

(deftest x86-emit-ret-emits-mov-rax-and-ret
  "vm-ret emits mov rax and ret."
  (let* ((tgt (%make-x86-target))
         (asm (%x86-emit tgt (make-vm-ret :reg :r0))))
    (assert-true (search "mov rax" asm))
    (assert-true (search "ret" asm))))

;;; ─── Spill code emission ──────────────────────────────────────────────────────

(deftest x86-emit-spill-operations
  "vm-spill-store emits mov [rbp-N], reg; vm-spill-load emits mov reg, [rbp-N]."
  (let ((tgt (%make-x86-target)))
    (let ((asm (%x86-emit tgt (make-vm-spill-store :src-reg :rax :slot 2))))
      (assert-true (search "mov" asm))
      (assert-true (search "rbp" asm))
      (assert-true (search "16" asm))
      (assert-true (search "rax" asm)))
    (let ((asm (%x86-emit tgt (make-vm-spill-load :dst-reg :rbx :slot 3))))
      (assert-true (search "mov" asm))
      (assert-true (search "rbx" asm))
      (assert-true (search "rbp" asm))
      (assert-true (search "24" asm)))))

(deftest x86-emit-spill-operations-rsp-red-zone
  "vm-spill-store/load use rsp in assembly when the target spill base is red-zone rsp."
  (let ((tgt (make-instance 'cl-cc/codegen::x86-64-target :spill-base-reg :rsp)))
    (let ((asm (%x86-emit tgt (make-vm-spill-store :src-reg :rax :slot 1))))
      (assert-true (search "rsp" asm))
      (assert-false (search "rbp" asm))
      (assert-true (search "8" asm)))
    (let ((asm (%x86-emit tgt (make-vm-spill-load :dst-reg :rbx :slot 1))))
      (assert-true (search "rsp" asm))
      (assert-false (search "rbp" asm))
      (assert-true (search "rbx" asm)))))
