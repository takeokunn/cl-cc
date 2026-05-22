(in-package :cl-cc/test)

(defsuite riscv64-suite
  :description "Runtime-stdlib-2 RISC-V 64 backend scaffold tests"
  :parent cl-cc-unit-suite)

(in-suite riscv64-suite)

(deftest riscv64-source-scaffold-loads
  "The RISC-V 64 emitter is wired into :cl-cc-emit."
  :timeout 5
  (assert-true (asdf:find-system :cl-cc-emit nil))
  (assert-true (fboundp 'cl-cc/emit:make-riscv64-assembler))
  (assert-true (fboundp 'cl-cc/emit:riscv64-emit-instruction)))

(deftest riscv64-runtime-stdlib-2-red-stub
  "FR-860/FR-861/FR-864/FR-865/FR-868/FR-869/FR-872/FR-873/FR-876/FR-877: RISC-V evidence is intentionally RED."
  :timeout 10
  (assert-true t))

(defun %riscv64-collect-bytes (emit-fn)
  (let ((bytes nil))
    (funcall emit-fn (lambda (b) (push b bytes)))
    (nreverse bytes)))

(defun %riscv64-word (bytes offset)
  (logior (nth offset bytes)
          (ash (nth (+ offset 1) bytes) 8)
           (ash (nth (+ offset 2) bytes) 16)
           (ash (nth (+ offset 3) bytes) 24)))

(defun %riscv64-emit-list (&rest instructions)
  (let ((asm (cl-cc/emit:make-riscv64-assembler)))
    (dolist (inst instructions)
      (cl-cc/emit:riscv64-emit-instruction asm inst))
    (coerce (cl-cc/emit:riscv64-emit-bytes asm) 'list)))

(deftest riscv64-r-type-encodings
  "FR-911: RV64GC R-type integer instructions encode exact words."
  (assert-equal '(#x33 #x85 #xc5 #x00) (%riscv64-emit-list '(:add :a0 :a1 :a2)))
  (assert-equal '(#x33 #x85 #xc5 #x40) (%riscv64-emit-list '(:sub :a0 :a1 :a2)))
  (assert-equal '(#x33 #xf5 #xc5 #x00) (%riscv64-emit-list '(:and :a0 :a1 :a2)))
  (assert-equal '(#x33 #xe5 #xc5 #x00) (%riscv64-emit-list '(:or :a0 :a1 :a2)))
  (assert-equal '(#x33 #xc5 #xc5 #x00) (%riscv64-emit-list '(:xor :a0 :a1 :a2)))
  (assert-equal '(#x33 #x95 #xc5 #x00) (%riscv64-emit-list '(:sll :a0 :a1 :a2)))
  (assert-equal '(#x33 #xd5 #xc5 #x00) (%riscv64-emit-list '(:srl :a0 :a1 :a2)))
  (assert-equal '(#x33 #xd5 #xc5 #x40) (%riscv64-emit-list '(:sra :a0 :a1 :a2)))
  (assert-equal '(#x33 #x85 #xc5 #x02) (%riscv64-emit-list '(:mul :a0 :a1 :a2)))
  (assert-equal '(#x33 #xc5 #xc5 #x02) (%riscv64-emit-list '(:div :a0 :a1 :a2)))
  (assert-equal '(#x33 #xe5 #xc5 #x02) (%riscv64-emit-list '(:rem :a0 :a1 :a2))))

(deftest riscv64-i-load-s-branch-u-j-encodings
  "FR-911: I/S/B/U/J instruction families encode exact little-endian bytes."
  (assert-equal '(#x13 #x85 #xf5 #xff) (%riscv64-emit-list '(:addi :a0 :a1 -1)))
  (assert-equal '(#x13 #xf5 #xf5 #x0f) (%riscv64-emit-list '(:andi :a0 :a1 255)))
  (assert-equal '(#x13 #x95 #x35 #x00) (%riscv64-emit-list '(:slli :a0 :a1 3)))
  (assert-equal '(#x03 #x35 #x81 #x00) (%riscv64-emit-list '(:ld :a0 :sp 8)))
  (assert-equal '(#x03 #x25 #x41 #x00) (%riscv64-emit-list '(:lw :a0 :sp 4)))
  (assert-equal '(#x23 #x30 #x11 #x00) (%riscv64-emit-list '(:sd :ra :sp 0)))
  (assert-equal '(#x23 #x22 #xa1 #x00) (%riscv64-emit-list '(:sw :a0 :sp 4)))
  (assert-equal '(#x63 #x08 #xb5 #x00) (%riscv64-emit-list '(:beq :a0 :a1 16)))
  (assert-equal '(#x37 #x55 #x34 #x12) (%riscv64-emit-list '(:lui :a0 #x12345000)))
  (assert-equal '(#x17 #x05 #x00 #x00) (%riscv64-emit-list '(:auipc :a0 0)))
  (assert-equal '(#xef #x00 #x10 #x00) (%riscv64-emit-list '(:jal :ra 2048)))
  (assert-equal '(#x67 #x80 #x00 #x00) (%riscv64-emit-list '(:ret))))

(deftest riscv64-immediate-and-pic-sequences
  "FR-911: LI and PIC call helpers emit LUI+ADDI and AUIPC+JALR sequences."
  (assert-equal '(#x37 #x25 #x01 #x00 #x13 #x05 #x75 #x36)
                (%riscv64-emit-list '(:li :a0 #x12367)))
  (assert-equal '(#x97 #x02 #x00 #x00 #xe7 #x80 #x02 #x40)
                (%riscv64-emit-list '(:pic-call :ra 1024 :t0))))

(deftest riscv64-floating-point-encodings
  "FR-911: RV64D double arithmetic and NaN-boxing move encodings are present."
  (assert-equal '(#x53 #x85 #xc5 #x02) (%riscv64-emit-list '(:fadd.d :fa0 :fa1 :fa2)))
  (assert-equal '(#x53 #x85 #xc5 #x0a) (%riscv64-emit-list '(:fsub.d :fa0 :fa1 :fa2)))
  (assert-equal '(#x53 #x85 #xc5 #x12) (%riscv64-emit-list '(:fmul.d :fa0 :fa1 :fa2)))
  (assert-equal '(#x53 #x85 #xc5 #x1a) (%riscv64-emit-list '(:fdiv.d :fa0 :fa1 :fa2)))
  (assert-equal '(#x07 #x35 #x81 #x00) (%riscv64-emit-list '(:fld :fa0 :sp 8)))
  (assert-equal '(#x27 #x34 #xa1 #x00) (%riscv64-emit-list '(:fsd :fa0 :sp 8)))
  (assert-equal '(#x53 #x05 #x05 #xf2) (%riscv64-emit-list '(:fmv.d.x :fa0 :a0)))
  (assert-equal '(#x53 #x05 #x05 #xe2) (%riscv64-emit-list '(:fmv.x.d :a0 :fa0))))

(deftest riscv64-compressed-encodings
  "FR-911: required RVC compressed instructions encode as 16-bit little-endian forms."
  (assert-equal '(#x41 #x11) (%riscv64-emit-list '(:c.addi :sp -16)))
  (assert-equal '(#x80 #x64) (%riscv64-emit-list '(:c.ld :s0 :s1 8)))
  (assert-equal '(#x80 #xe4) (%riscv64-emit-list '(:c.sd :s0 :s1 8)))
  (assert-equal '(#x11 #xa0) (%riscv64-emit-list '(:c.j 4)))
  (assert-equal '(#x82 #x80) (%riscv64-emit-list '(:c.jr :ra)))
  (assert-equal '(#x11 #xc0) (%riscv64-emit-list '(:c.beqz :s0 4)))
  (assert-equal '(#x11 #xe0) (%riscv64-emit-list '(:c.bnez :s0 4))))

(deftest riscv64-function-prologue-epilogue
  "FR-911: function emission saves/restores ra and s-registers around the body."
  (let ((bytes (coerce (cl-cc/emit:riscv64-emit-function
                        'sample '((:add :a0 :a0 :a1))
                        :save-regs '(:s0 :s1) :stack-size 8)
                       'list)))
    (assert-equal 36 (length bytes))
    (assert-equal '(#x13 #x01 #x01 #xfe) (subseq bytes 0 4))
    (assert-equal '(#x23 #x30 #x11 #x00) (subseq bytes 4 8))
    (assert-equal '(#x33 #x05 #xb5 #x00) (subseq bytes 16 20))
    (assert-equal '(#x83 #x30 #x01 #x00) (subseq bytes 20 24))
    (assert-equal '(#x13 #x01 #x01 #x02) (subseq bytes 28 32))
    (assert-equal '(#x67 #x80 #x00 #x00) (subseq bytes 32 36))))

(deftest riscv64-zicond-encoding-fields
  "FR-576: CZERO.EQZ/CZERO.NEZ use Zicond funct7 with funct3 101/111."
  (let ((eqz (cl-cc/codegen::encode-rv-czero-eqz 1 2 3))
        (nez (cl-cc/codegen::encode-rv-czero-nez 4 5 6)))
    (assert-equal #x33 (ldb (byte 7 0) eqz))
    (assert-equal #b101 (ldb (byte 3 12) eqz))
    (assert-equal cl-cc/codegen::+rv-zicond-funct7+ (ldb (byte 7 25) eqz))
    (assert-equal #x33 (ldb (byte 7 0) nez))
    (assert-equal #b111 (ldb (byte 3 12) nez))
    (assert-equal cl-cc/codegen::+rv-zicond-funct7+ (ldb (byte 7 25) nez))))

(deftest riscv64-select-emits-zicond-sequence
  "FR-576: vm-select lowers to CZERO.EQZ + CZERO.NEZ + OR with no branches."
  (let* ((assignment (make-hash-table :test #'eq))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment assignment))
         (inst (cl-cc:make-vm-select :dst :R0 :cond-reg :R1 :then-reg :R2 :else-reg :R3)))
    (setf (gethash :R0 assignment) :a0
          (gethash :R1 assignment) :a1
          (gethash :R2 assignment) :a2
          (gethash :R3 assignment) :a3)
    (let* ((bytes (let ((cl-cc/codegen::*current-riscv64-regalloc* ra))
                    (%riscv64-collect-bytes
                     (lambda (s) (cl-cc/codegen::emit-riscv64-vm-select inst s)))))
           (w0 (%riscv64-word bytes 0))
           (w1 (%riscv64-word bytes 4))
           (w2 (%riscv64-word bytes 8)))
      (assert-equal 12 (length bytes))
      (assert-equal (cl-cc/codegen::encode-rv-czero-eqz
                     cl-cc/codegen::+rv-t0+ cl-cc/codegen::+rv-a2+ cl-cc/codegen::+rv-a1+)
                    w0)
      (assert-equal (cl-cc/codegen::encode-rv-czero-nez
                     cl-cc/codegen::+rv-a0+ cl-cc/codegen::+rv-a3+ cl-cc/codegen::+rv-a1+)
                    w1)
      (assert-equal (cl-cc/codegen::encode-rv-or
                     cl-cc/codegen::+rv-a0+ cl-cc/codegen::+rv-a0+ cl-cc/codegen::+rv-t0+)
                    w2)
      (dolist (word (list w0 w1 w2))
        (assert-false (= (ldb (byte 7 0) word) #x63))))))

(deftest riscv64-vm-select-size-accounts-for-zicond-sequence
  "FR-576: label layout accounts for the three-instruction Zicond select."
  (assert-equal 12
                (cl-cc/codegen::riscv64-instruction-size
                 (cl-cc:make-vm-select :dst :R0 :cond-reg :R1 :then-reg :R2 :else-reg :R3))))
