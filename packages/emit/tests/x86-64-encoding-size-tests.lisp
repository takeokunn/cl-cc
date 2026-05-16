;;;; tests/unit/emit/x86-64-encoding-size-tests.lisp — x86-64 Instruction Size & Layout Tests
;;;;
;;;; Continuation of x86-64-encoding-tests.lisp.
;;;; Tests for instruction-size dispatch, label offset calculation,
;;;; emitter table integrity, memory displacement encoding, and
;;;; full program output verification.

(in-package :cl-cc/test)

(in-suite x86-64-encoding-suite)

(deftest-each x86-instruction-size-zero-cases
  "vm-label and unknown types both report instruction-size = 0."
  :cases (("vm-label"   (cl-cc:make-vm-label :name "test"))
          ("unknown"    (list :not-a-real-inst)))
  (inst)
  (assert-equal 0 (cl-cc/codegen::instruction-size inst)))

;;; ─── build-label-offsets ────────────────────────────────────────────────

(deftest-each x86-build-label-offsets-simple
  "build-label-offsets: label offset equals prologue-size plus preceding instruction bytes."
  :cases (("single-at-start"
           (list (cl-cc:make-vm-label :name "L"))
           6 "L" 6)
          ("after-vm-const"
           (list (cl-cc:make-vm-const :dst :R0 :value 42)
                 (cl-cc:make-vm-label :name "L"))
           6 "L" 16)
          ("after-vm-add"
           (list (cl-cc:make-vm-add :dst :R0 :lhs :R1 :rhs :R2)
                 (cl-cc:make-vm-label :name "L"))
           0 "L" 6))
  (insts prologue label expected-offset)
  (assert-equal expected-offset (gethash label (cl-cc/codegen::build-label-offsets insts prologue))))

(deftest x86-build-label-offsets-multi
  "Empty list → 0 table entries; multiple labels track correct byte positions."
  (assert-equal 0 (hash-table-count (cl-cc/codegen::build-label-offsets nil 6)))
  (let* ((insts (list (cl-cc:make-vm-label :name "L0")
                      (cl-cc:make-vm-move :dst :R0 :src :R1)    ; 3 bytes
                      (cl-cc:make-vm-label :name "L1")
                      (cl-cc:make-vm-add :dst :R0 :lhs :R1 :rhs :R2) ; 6 bytes
                      (cl-cc:make-vm-label :name "L2")))
         (offsets (cl-cc/codegen::build-label-offsets insts 0)))
    (assert-equal 0 (gethash "L0" offsets))
    (assert-equal 3 (gethash "L1" offsets))
    (assert-equal 9 (gethash "L2" offsets))))

;;; ─── *x86-64-emitter-table* completeness ────────────────────────────────

(deftest x86-emitter-table-integrity
  "Emitter table has >= 40 entries and every value is a function."
  (assert-true (>= (hash-table-count cl-cc/codegen::*x86-64-emitter-table*) 40))
  (let ((all-ok t))
    (maphash (lambda (key fn)
               (declare (ignore key))
               (unless (functionp fn)
                 (setf all-ok nil)))
             cl-cc/codegen::*x86-64-emitter-table*)
    (assert-true all-ok)))

;;; ─── High-register encoding (REX.R / REX.B) and memory displacement ────

(deftest-each x86-mov-mem-displacement-sizes
  "Memory load/store displacement: zero-disp uses mod=00 (3 bytes); byte-disp uses mod=01 (4 bytes)."
  :cases (("rm64-no-disp"   (lambda (s) (cl-cc/codegen::emit-mov-rm64 cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+  0 s)) 3)
          ("rm64-disp8"     (lambda (s) (cl-cc/codegen::emit-mov-rm64 cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+  8 s)) 4)
          ("mr64-disp8-16"  (lambda (s) (cl-cc/codegen::emit-mov-mr64 cl-cc/codegen::+rcx+ 16 cl-cc/codegen::+rax+ s)) 4))
  (emit-fn expected-len)
  (assert-equal expected-len (length (%x86-collect-bytes emit-fn))))

(deftest-each x86-mov-exact-bytes
  "MOV load/store instructions (indexed, RSP-based SIB) produce exact byte sequences."
  :cases (("rm64-indexed-disp8"
           (lambda (s) (cl-cc/codegen::emit-mov-rm64-indexed cl-cc/codegen::+rax+ cl-cc/codegen::+rbx+ cl-cc/codegen::+rcx+ 8 16 s))
           '(#x48 #x8B #x44 #xCB #x10))
          ("mr64-indexed-disp8"
           (lambda (s) (cl-cc/codegen::emit-mov-mr64-indexed cl-cc/codegen::+rbx+ cl-cc/codegen::+rcx+ 4 8 cl-cc/codegen::+rax+ s))
           '(#x48 #x89 #x44 #x8B #x08))
          ("rm64-indexed-scale1-zero"
           (lambda (s) (cl-cc/codegen::emit-mov-rm64-indexed cl-cc/codegen::+rax+ cl-cc/codegen::+rbx+ cl-cc/codegen::+rcx+ 1 0 s))
           '(#x48 #x8B #x04 #x0B))
          ("load-rsp-disp8"
           (lambda (s) (cl-cc/codegen::emit-mov-rm64 cl-cc/codegen::+rax+ cl-cc/codegen::+rsp+ -8 s))
           '(#x48 #x8B #x44 #x24 #xF8))
          ("store-rsp-disp8"
           (lambda (s) (cl-cc/codegen::emit-mov-mr64 cl-cc/codegen::+rsp+ -16 cl-cc/codegen::+rax+ s))
           '(#x48 #x89 #x44 #x24 #xF0)))
  (emit-fn expected-bytes)
  (assert-equal expected-bytes (%x86-collect-bytes emit-fn)))

;;; ─── emit-mov-rr64 ModR/M correctness ──────────────────────────────────

(deftest-each x86-mov-rr64-modrm-encoding
  "emit-mov-rr64: REX.W=#x48, opcode=#x89, ModR/M byte varies by register pair."
  :cases (("rax-rcx" cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ #xC8)
          ("rax-rbx" cl-cc/codegen::+rax+ cl-cc/codegen::+rbx+ #xD8)
          ("rax-rax" cl-cc/codegen::+rax+ cl-cc/codegen::+rax+ #xC0))
  (dst src modrm)
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-mov-rr64 dst src s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #x48 (first bytes))
    (assert-equal #x89 (second bytes))
    (assert-equal modrm (third bytes))))

;;; ─── PUSH/POP single-byte opcode coverage ───────────────────────────────

(deftest-each x86-push-pop-single-byte-opcodes
  "PUSH/POP RAX-RDI (reg < 8) emit single-byte opcodes."
  :cases (("push-rax" cl-cc/codegen::+rax+ #'cl-cc/codegen::emit-push-r64 #x50)
          ("push-rcx" cl-cc/codegen::+rcx+ #'cl-cc/codegen::emit-push-r64 #x51)
          ("pop-rax"  cl-cc/codegen::+rax+ #'cl-cc/codegen::emit-pop-r64  #x58)
          ("push-rbx" cl-cc/codegen::+rbx+ #'cl-cc/codegen::emit-push-r64 #x53)
          ("pop-rcx"  cl-cc/codegen::+rcx+ #'cl-cc/codegen::emit-pop-r64  #x59)
          ("pop-rdx"  cl-cc/codegen::+rdx+ #'cl-cc/codegen::emit-pop-r64  #x5A))
  (reg emit-fn opcode)
  (let ((bytes (%x86-collect-bytes (lambda (s) (funcall emit-fn reg s)))))
    (assert-equal 1 (length bytes))
    (assert-equal opcode (first bytes))))

(deftest-each x86-push-pop-extended-regs-two-byte-encoding
  "PUSH/POP R8-R15 (reg >= 8) emit REX.B prefix (#x41) followed by opcode byte."
  :cases (("push-r8"  8  #'cl-cc/codegen::emit-push-r64 #x50)
          ("push-r12" 12 #'cl-cc/codegen::emit-push-r64 #x54)
          ("push-r13" 13 #'cl-cc/codegen::emit-push-r64 #x55)
          ("push-r14" 14 #'cl-cc/codegen::emit-push-r64 #x56)
          ("push-r15" 15 #'cl-cc/codegen::emit-push-r64 #x57)
          ("pop-r12"  12 #'cl-cc/codegen::emit-pop-r64  #x5C)
          ("pop-r13"  13 #'cl-cc/codegen::emit-pop-r64  #x5D)
          ("pop-r15"  15 #'cl-cc/codegen::emit-pop-r64  #x5F))
  (reg emit-fn opcode)
  (let ((bytes (%x86-collect-bytes (lambda (s) (funcall emit-fn reg s)))))
    (assert-equal 2 (length bytes))
    (assert-equal #x41 (first bytes))
    (assert-equal opcode (second bytes))))

(deftest-each x86-push-r64-byte-size
  "push-r64-byte-size returns 1 for RAX-RDI (reg < 8) and 2 for R8-R15 (reg >= 8)."
  :cases (("rax" 0 1)
          ("rbp" 5 1)
          ("rdi" 7 1)
          ("r8"  8 2)
          ("r12" 12 2)
          ("r15" 15 2))
  (reg expected)
  (assert-equal expected (cl-cc/codegen::push-r64-byte-size reg)))

(deftest-each x86-pop-r64-byte-size
  "pop-r64-byte-size returns 1 for RAX-RDI (reg < 8) and 2 for R8-R15 (reg >= 8)."
  :cases (("rax" 0 1)
          ("rbp" 5 1)
          ("rdi" 7 1)
          ("r8"  8 2)
          ("r12" 12 2)
          ("r15" 15 2))
  (reg expected)
  (assert-equal expected (cl-cc/codegen::pop-r64-byte-size reg)))

(deftest x86-prologue-with-r12-callee-saved-uses-two-byte-push
  "A program whose regalloc assigns :r12 emits 2-byte PUSH R12 (#x41 #x54) and POP R12 (#x41 #x5C) in prologue/epilogue."
  (let* ((assignment (let ((ht (make-hash-table :test #'eq)))
                       (setf (gethash :R0 ht) :r12)
                       ht))
         (ra (cl-cc/regalloc::make-regalloc-result
              :assignment assignment
              :spill-count 0
              :instructions nil))
         (prog (cl-cc/vm::make-vm-program
                :instructions nil
                :result-register :R0
                :leaf-p nil))
         (bytes (let ((cl-cc/codegen::*current-regalloc* ra))
                  (%x86-collect-bytes
                   (lambda (s) (cl-cc/codegen::emit-vm-program prog s))))))
    ;; Prologue: PUSH R12 must be 2-byte encoding with REX.B prefix
    (assert-equal #x41 (first bytes))
    (assert-equal #x54 (second bytes))
    ;; Epilogue: POP R12 must be 2-byte encoding with REX.B prefix
    (assert-equal #x41 (third bytes))
    (assert-equal #x5C (fourth bytes))
    ;; RET follows immediately
    (assert-equal #xC3 (fifth bytes))
    (assert-equal 5 (length bytes))))

;;; ─── SETcc opcode2 values for each comparison ───────────────────────────

(deftest-each x86-setcc-opcode2-values
  "SETcc emitters use correct opcode2 second byte."
  :cases (("setl"  #x9C)   ; SETL  (signed less-than)
          ("setge" #x9D)   ; SETGE (signed greater-or-equal)
          ("setle" #x9E)   ; SETLE (signed less-or-equal)
          ("setg"  #x9F)   ; SETG  (signed greater-than)
          ("sete"  #x94)   ; SETE  (equal / zero)
          ("setne" #x95))  ; SETNE (not-equal)
  (opcode2)
  ;; emit-setcc with RAX (reg 0, no REX needed): emits 0F <opcode2> ModRM
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-setcc opcode2 cl-cc/codegen::+rax+ s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #x0F (first bytes))
    (assert-equal opcode2 (second bytes))))

;;; ─── vm-reg-to-x86 mapping ───────────────────────────────────────────────

(deftest-each x86-vm-reg-to-x86-mapping
  "vm-reg-to-x86 maps VM keyword registers to correct x86-64 codes."
  :cases (("R0" :R0 0)    ; RAX
          ("R1" :R1 1)    ; RCX
          ("R2" :R2 2)    ; RDX
          ("R3" :R3 3)    ; RBX
          ("R4" :R4 6)    ; RSI
          ("R5" :R5 7)    ; RDI
          ("R6" :R6 8)    ; R8
          ("R7" :R7 9))   ; R9
  (vm-reg expected-code)
  (assert-equal expected-code (cl-cc/codegen::vm-reg-to-x86 vm-reg)))

;;; ─── *phys-reg-to-x86-code* completeness ────────────────────────────────

(deftest x86-phys-reg-map
  "Physical register alist: :rax=0, :rbp=5, :r15=15, covers 15 registers."
  (assert-equal 0  (cdr (assoc :rax cl-cc/codegen::*phys-reg-to-x86-code*)))
  (assert-equal 5  (cdr (assoc :rbp cl-cc/codegen::*phys-reg-to-x86-code*)))
  (assert-equal 15 (cdr (assoc :r15 cl-cc/codegen::*phys-reg-to-x86-code*)))
  (assert-equal 15 (length cl-cc/codegen::*phys-reg-to-x86-code*)))

;;; ─── emit-vm-const and emit-vm-move sizes ───────────────────────────────

(deftest-each x86-vm-const-bool-immediate
  "vm-const always emits 10 bytes; nil→0, t→1 encodes correct LE immediate bytes."
  :cases (("nil" nil 0)
          ("t"   t   1))
  (val expected-byte)
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-vm-const
                             (cl-cc:make-vm-const :dst :R0 :value val) s)))))
    (assert-equal 10 (length bytes))
    (assert-equal expected-byte (third bytes))
    (assert-equal 0 (fourth bytes))))

(deftest-each x86-vm-move-halt-elision
  "No-op moves (same src/dst or result already in RAX) emit 0 bytes; cross-register emit 3 bytes with opcode #x89."
  :cases (("move-cross" (lambda (s) (cl-cc/codegen::emit-vm-move
                                     (cl-cc:make-vm-move :dst :R0 :src :R1) s))
                        (lambda (s) (cl-cc/codegen::emit-vm-move
                                     (cl-cc:make-vm-move :dst :R0 :src :R0) s)))
          ("halt"       (lambda (s) (cl-cc/codegen::emit-vm-halt-inst
                                     (cl-cc:make-vm-halt :reg :R1) s))
                        (lambda (s) (cl-cc/codegen::emit-vm-halt-inst
                                     (cl-cc:make-vm-halt :reg :R0) s))))
  (cross-emitter zero-emitter)
  (let ((cross-bytes (%x86-collect-bytes cross-emitter))
        (zero-bytes  (%x86-collect-bytes zero-emitter)))
    (assert-equal 3   (length cross-bytes))
    (assert-equal #x89 (second cross-bytes))
    (assert-equal 0   (length zero-bytes))))

;;; ─── emit-idiv-sequence ──────────────────────────────────────────────────

(deftest-each x86-idiv-sequence-size
  "emit-idiv-sequence emits exactly 18 bytes for both quotient and remainder modes."
  :cases (("quotient"  nil)
          ("remainder" t))
  (remainder-p)
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-idiv-sequence cl-cc/codegen::+rcx+ cl-cc/codegen::+rdx+ remainder-p s)))))
    (assert-equal 18 (length bytes))))

;;; ─── emit-vm-program prologue/epilogue ───────────────────────────────────

(deftest x86-vm-program-output
  "compile-to-x86-64-bytes produces non-empty bytes for non-empty programs
and emits a short prologue+return sequence for empty programs.
Use compile-to-x86-64-bytes (which sets up the regalloc context internally)
rather than emit-vm-program directly — the latter expects *current-regalloc*
to be bound, which fails with NIL under raw invocation."
  (let* ((non-empty-insts (list (cl-cc:make-vm-halt :reg :R0)
                                (cl-cc:make-vm-ret :reg :R0)))
         (full-prog (cl-cc/vm::make-vm-program :instructions non-empty-insts :result-register :R0))
         (full-bytes (cl-cc/codegen::compile-to-x86-64-bytes full-prog))
         (empty-prog (cl-cc/vm::make-vm-program :instructions nil :result-register :R0))
         (empty-bytes (cl-cc/codegen::compile-to-x86-64-bytes empty-prog)))
    (assert-true (> (length full-bytes) 0))
    (assert-true (> (length empty-bytes) 0))))

(deftest x86-vm-program-leaf-red-zone-spills-skip-rbp-frame
  "Leaf programs with small spill counts use RSP-based red-zone spill slots and skip PUSH/POP RBP."
  (let* ((prog (cl-cc/vm::make-vm-program
                :instructions (list (cl-cc:make-vm-spill-store :src-reg :rax :slot 1)
                                    (cl-cc:make-vm-spill-load :dst-reg :rbx :slot 1))
                :result-register :R0
                :leaf-p t))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment (make-hash-table :test #'eq)
                                           :spill-count 1
                                           :instructions (cl-cc/vm::vm-program-instructions prog)))
         (bytes (let ((cl-cc/codegen::*current-regalloc* ra))
                   (%x86-collect-bytes (lambda (s) (cl-cc/codegen::emit-vm-program prog s))))))
    (assert-false (= #x55 (first bytes)))
    (assert-equal '(#x48 #x89 #x44 #x24 #xF8) (subseq bytes 0 5))
    (assert-equal '(#x48 #x8B #x5C #x24 #xF8) (subseq bytes 5 10))
    (assert-equal #xC3 (car (last bytes)))))

(deftest x86-vm-program-default-fpe-allocates-rsp-spill-frame
  "Default x86-64 native code omits RBP and allocates an RSP-relative spill frame for non-leaf spills."
  (let* ((prog (cl-cc/vm::make-vm-program
                :instructions (list (cl-cc:make-vm-spill-store :src-reg :rax :slot 1)
                                    (cl-cc:make-vm-spill-load :dst-reg :rbx :slot 1))
                :result-register :R0
                :leaf-p nil))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment (make-hash-table :test #'eq)
                                                   :spill-count 1
                                                   :instructions (cl-cc/vm::vm-program-instructions prog)))
         (bytes (let ((cl-cc/codegen::*current-regalloc* ra))
                  (%x86-collect-bytes (lambda (s) (cl-cc/codegen::emit-vm-program prog s))))))
    (assert-false (= #x55 (first bytes)))
    (assert-equal '(#x48 #x81 #xEC #x08 #x00 #x00 #x00) (subseq bytes 0 7))
    (assert-equal '(#x48 #x89 #x04 #x24) (subseq bytes 7 11))
    (assert-equal '(#x48 #x8B #x1C #x24) (subseq bytes 11 15))
    (assert-equal '(#x48 #x81 #xC4 #x08 #x00 #x00 #x00) (subseq bytes 15 22))
    (assert-equal #xC3 (nth 22 bytes))))

(deftest x86-vm-program-debug-opt-out-keeps-rbp-spills
  "Disabling x86-64 FPE keeps the existing RBP-based spill layout for non-leaf spills."
  (let* ((prog (cl-cc/vm::make-vm-program
                :instructions (list (cl-cc:make-vm-spill-store :src-reg :rax :slot 1)
                                    (cl-cc:make-vm-spill-load :dst-reg :rbx :slot 1))
                :result-register :R0
                :leaf-p nil))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment (make-hash-table :test #'eq)
                                                   :spill-count 1
                                                   :instructions (cl-cc/vm::vm-program-instructions prog)))
         (bytes (let ((cl-cc/codegen::*current-regalloc* ra)
                      (cl-cc/codegen::*x86-64-omit-frame-pointer* nil))
                  (%x86-collect-bytes (lambda (s) (cl-cc/codegen::emit-vm-program prog s))))))
    (assert-equal #x55 (first bytes))
    (assert-equal '(#x48 #x89 #xE5) (subseq bytes 1 4))
    (assert-equal '(#x48 #x89 #x45 #xF8) (subseq bytes 4 8))
    (assert-equal '(#x48 #x8B #x5D #xF8) (subseq bytes 8 12))
    (assert-equal #x5D (nth 12 bytes))
    (assert-equal #xC3 (nth 13 bytes))))

(deftest-each x86-stack-probe-count-thresholds
  "stack-probe-count emits one probe per 4096-byte frame page."
  :cases (("below-page" 4095 0)
          ("one-page" 4096 1)
          ("two-pages" 8192 2))
  (frame-size expected)
  (assert-= expected (cl-cc/codegen::stack-probe-count frame-size)))

(deftest x86-stack-probe-emits-non-mutating-rsp-page-touch
  "emit-or-mem-rsp-disp32-imm8 encodes OR qword ptr [rsp-4096], 0."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-or-mem-rsp-disp32-imm8 -4096 0 s)))))
    (assert-equal '(#x48 #x83 #x8C #x24 #x00 #xF0 #xFF #xFF #x00) bytes)))

(deftest x86-large-spill-frame-inserts-stack-probe-before-rsp-allocation
  "Large native frames emit stack probes before the RSP spill-frame allocation sequence."
  (let* ((prog (cl-cc/vm::make-vm-program
                :instructions nil
                :result-register :R0
                :leaf-p t))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment (make-hash-table :test #'eq)
                                                    :spill-count 513
                                                    :instructions nil))
         (bytes (let ((cl-cc/codegen::*current-regalloc* ra))
                   (%x86-collect-bytes (lambda (s) (cl-cc/codegen::emit-vm-program prog s))))))
    (assert-equal '(#x48 #x83 #x8C #x24 #x00 #xF0 #xFF #xFF #x00) (subseq bytes 0 9))
    (assert-equal '(#x48 #x81 #xEC #x08 #x10 #x00 #x00) (subseq bytes 9 16))
    (assert-equal '(#x48 #x81 #xC4 #x08 #x10 #x00 #x00) (subseq bytes 16 23))
    (assert-= #xC3 (nth 23 bytes))))

;;; ─── instruction-size for specific types ────────────────────────────────

(deftest-each x86-instruction-size-values
  "instruction-size returns the documented byte count for each instruction type."
  :cases (("vm-const"    (cl-cc:make-vm-const    :dst :R0 :value 0)           10)
          ("vm-move"     (cl-cc:make-vm-move     :dst :R0 :src :R1)            3)
          ("vm-add"      (cl-cc:make-vm-add      :dst :R0 :lhs :R1 :rhs :R2)  6)
          ("vm-sub"      (cl-cc:make-vm-sub      :dst :R0 :lhs :R1 :rhs :R2)  6)
          ("vm-mul"      (cl-cc:make-vm-mul      :dst :R0 :lhs :R1 :rhs :R2)  7)
          ("vm-integer-mul-high-u" (cl-cc:make-vm-integer-mul-high-u :dst :R0 :lhs :R1 :rhs :R2) 19)
          ("vm-integer-mul-high-s" (cl-cc:make-vm-integer-mul-high-s :dst :R0 :lhs :R1 :rhs :R2) 19)
          ("vm-jump"     (cl-cc:make-vm-jump     :label "L")                    5)
          ("vm-ret"      (cl-cc:make-vm-ret)                                   1)
          ("vm-abs"      (make-vm-abs             :dst :R0 :src :R1)           15)
          ("vm-ash"      (make-vm-ash             :dst :R0 :lhs :R1 :rhs :R2) 24)
          ("vm-div"      (cl-cc:make-vm-div      :dst :R0 :lhs :R1 :rhs :R2) 34)
          ("vm-mod"      (cl-cc:make-vm-mod      :dst :R0 :lhs :R1 :rhs :R2) 37)
          ("vm-logtest"  (cl-cc:make-vm-logtest  :dst :R0 :lhs :R1 :rhs :R2) 14)
          ("vm-logbitp"  (cl-cc:make-vm-logbitp  :dst :R0 :lhs :R1 :rhs :R2) 15))
  (inst expected)
  (assert-equal expected (cl-cc/codegen::instruction-size inst)))
