;;;; packages/codegen/src/riscv64-codegen.lisp - RV64IMAFDC Machine Code Generation
;;;
;;; Emits little-endian RISC-V RV64IMAFDC instruction bytes from VM instructions.
;;; The implementation mirrors the compact AArch64 backend shape: register maps,
;;; instruction encoders, per-VM emitters, program emission, and public compile API
;;; are kept in one file until the backend grows enough to split safely.

(in-package :cl-cc/codegen)

;;; Physical registers and ABI metadata

(defconstant +rv-zero+ 0 "Hard-wired zero register X0.")
(defconstant +rv-ra+ 1 "Return address register X1.")
(defconstant +rv-sp+ 2 "Stack pointer register X2.")
(defconstant +rv-gp+ 3 "Global pointer register X3.")
(defconstant +rv-tp+ 4 "Thread pointer register X4.")
(defconstant +rv-t0+ 5 "Scratch register X5 used for spill and long-immediate lowering.")
(defconstant +rv-s0+ 8 "Callee-saved frame-pointer register X8.")
(defconstant +rv-fp+ 8 "ABI frame-pointer alias for X8/S0.")
(defconstant +rv-a0+ 10 "First integer argument and return register X10.")
(defconstant +rv-a1+ 11 "Second integer argument and second return register X11.")
(defconstant +rv-a2+ 12 "Third integer argument register X12.")
(defconstant +rv-a3+ 13 "Fourth integer argument register X13.")
(defconstant +rv-zicond-funct7+ #b0000111 "Zicond extension funct7 for CZERO.EQZ and CZERO.NEZ.")

(defparameter *riscv64-reg-number*
  '((:zero . 0) (:ra . 1) (:sp . 2) (:gp . 3) (:tp . 4)
    (:t0 . 5) (:t1 . 6) (:t2 . 7) (:s0 . 8) (:fp . 8) (:s1 . 9)
    (:a0 . 10) (:a1 . 11) (:a2 . 12) (:a3 . 13) (:a4 . 14) (:a5 . 15)
    (:a6 . 16) (:a7 . 17) (:s2 . 18) (:s3 . 19) (:s4 . 20) (:s5 . 21)
    (:s6 . 22) (:s7 . 23) (:s8 . 24) (:s9 . 25) (:s10 . 26) (:s11 . 27)
    (:t3 . 28) (:t4 . 29) (:t5 . 30) (:t6 . 31))
  "RISC-V integer register keyword to architectural register number map.")

(defparameter *riscv64-fp-reg-number*
  '((:f0 . 0) (:ft0 . 0) (:f1 . 1) (:ft1 . 1) (:f2 . 2) (:ft2 . 2)
    (:f3 . 3) (:ft3 . 3) (:f4 . 4) (:ft4 . 4) (:f5 . 5) (:ft5 . 5)
    (:f6 . 6) (:ft6 . 6) (:f7 . 7) (:ft7 . 7) (:f8 . 8) (:fs0 . 8)
    (:f9 . 9) (:fs1 . 9) (:f10 . 10) (:fa0 . 10) (:f11 . 11) (:fa1 . 11)
    (:f12 . 12) (:fa2 . 12) (:f13 . 13) (:fa3 . 13) (:f14 . 14) (:fa4 . 14)
    (:f15 . 15) (:fa5 . 15) (:f16 . 16) (:fa6 . 16) (:f17 . 17) (:fa7 . 17)
    (:f18 . 18) (:fs2 . 18) (:f19 . 19) (:fs3 . 19) (:f20 . 20) (:fs4 . 20)
    (:f21 . 21) (:fs5 . 21) (:f22 . 22) (:fs6 . 22) (:f23 . 23) (:fs7 . 23)
    (:f24 . 24) (:fs8 . 24) (:f25 . 25) (:fs9 . 25) (:f26 . 26) (:fs10 . 26)
    (:f27 . 27) (:fs11 . 27) (:f28 . 28) (:ft8 . 28) (:f29 . 29) (:ft9 . 29)
    (:f30 . 30) (:ft10 . 30) (:f31 . 31) (:ft11 . 31))
  "RISC-V floating-point register keyword and ABI alias map.")

(defparameter *current-riscv64-regalloc* nil
  "Current regalloc-result during RISC-V code generation.")

(defparameter *riscv64-omit-frame-pointer* t
  "When true, prefer SP-relative spill slots over reserving S0 as frame pointer.")

(defparameter *current-riscv64-spill-base-reg* +rv-fp+
  "Base register used for RISC-V spill load/store emission in the current function.")

(defparameter *current-riscv64-spill-offset-bias* 0
  "Additional byte bias applied before translating RISC-V spill slots to stack offsets.")

(defparameter *current-riscv64-float-vregs* nil
  "When non-nil, hash table of virtual registers known to hold unboxed floats.")

(defparameter *riscv64-zicond-enabled* t
  "When true, lower integer select through Zicond CZERO.EQZ/CZERO.NEZ.")

(defun riscv64-codegen-target ()
  "Return the RISC-V target descriptor for the active frame-pointer policy."
  (if *riscv64-omit-frame-pointer*
      (make-target-desc
       :name (target-name *riscv64-target*)
       :word-size (target-word-size *riscv64-target*)
       :endianness (target-endianness *riscv64-target*)
       :gpr-count (target-gpr-count *riscv64-target*)
       :gpr-names (target-gpr-names *riscv64-target*)
       :arg-regs (target-arg-regs *riscv64-target*)
       :ret-reg (target-ret-reg *riscv64-target*)
       :fp-arg-regs (target-fp-arg-regs *riscv64-target*)
       :fp-ret-reg (target-fp-ret-reg *riscv64-target*)
       :callee-saved '(:s0 :s1 :s2 :s3 :s4 :s5 :s6 :s7 :s8 :s9 :s10 :s11)
       :scratch-regs (cons :s0 (target-scratch-regs *riscv64-target*))
       :stack-alignment (target-stack-alignment *riscv64-target*)
       :legal-ops (target-legal-ops *riscv64-target*)
       :features (target-features *riscv64-target*))
      *riscv64-target*))

(defun riscv64-spill-slot-offset (slot)
  "Return the byte offset for spill SLOT relative to the current spill base."
  (- *current-riscv64-spill-offset-bias* (* slot 8)))

(defun riscv64-reg-number (phys)
  "Return integer register number for physical register keyword PHYS."
  (or (cdr (assoc phys *riscv64-reg-number*))
      (error "Unknown RISC-V integer register: ~A" phys)))

(defun riscv64-fp-reg-number (phys)
  "Return floating-point register number for physical register keyword PHYS."
  (or (cdr (assoc phys *riscv64-fp-reg-number*))
      (error "Unknown RISC-V floating-point register: ~A" phys)))

(defun riscv64-reg (vm-reg)
  "Map VM register or physical keyword to an integer RISC-V GPR number."
  (let ((phys-entry (assoc vm-reg *riscv64-reg-number*)))
    (cond (phys-entry (cdr phys-entry))
          (*current-riscv64-regalloc*
           (let ((phys (gethash vm-reg (regalloc-assignment *current-riscv64-regalloc*))))
             (unless phys
               (error "Virtual register ~A not allocated for RISC-V" vm-reg))
             (riscv64-reg-number phys)))
          (t (error "VM register ~A has no RISC-V mapping" vm-reg)))))

(defun riscv64-freg (vm-reg)
  "Map VM register or physical keyword to an integer RISC-V FPR number."
  (let ((phys-entry (assoc vm-reg *riscv64-fp-reg-number*)))
    (cond (phys-entry (cdr phys-entry))
          (*current-riscv64-regalloc*
           (let* ((phys (gethash vm-reg (regalloc-assignment *current-riscv64-regalloc*)))
                  (entry (and phys (assoc phys *riscv64-fp-reg-number*))))
             (if entry
                 (cdr entry)
                 (riscv64-reg vm-reg))))
          (t (riscv64-reg vm-reg)))))

;;; Byte and instruction emission helpers

(defun emit-riscv16 (halfword stream)
  "Emit a 16-bit compressed RISC-V instruction in little-endian order."
  (funcall stream (logand halfword #xFF))
  (funcall stream (logand (ash halfword -8) #xFF)))

(defun emit-riscv32 (word stream)
  "Emit a 32-bit RISC-V instruction in little-endian order."
  (funcall stream (logand word #xFF))
  (funcall stream (logand (ash word -8) #xFF))
  (funcall stream (logand (ash word -16) #xFF))
  (funcall stream (logand (ash word -24) #xFF)))

(defun riscv-signed-field (value bits)
  "Return VALUE masked to a signed immediate field of BITS bits."
  (logand value (1- (ash 1 bits))))

(defun riscv-check-signed (value bits context)
  "Signal if VALUE does not fit in signed BITS bits."
  (unless (<= (- (ash 1 (1- bits))) value (1- (ash 1 (1- bits))))
    (error "~A immediate ~D does not fit signed ~D-bit field" context value bits))
  value)

(defun riscv-check-branch-aligned (offset context)
  "Signal if branch byte OFFSET is not halfword aligned."
  (unless (zerop (logand offset 1))
    (error "~A offset must be 2-byte aligned: ~D" context offset))
  offset)

;;; Instruction encoding formats

(defun encode-rv-r (opcode rd funct3 rs1 rs2 funct7)
  "Encode R-type: funct7 rs2 rs1 funct3 rd opcode."
  (logior opcode (ash (logand rd #x1F) 7) (ash (logand funct3 #x7) 12)
          (ash (logand rs1 #x1F) 15) (ash (logand rs2 #x1F) 20)
          (ash (logand funct7 #x7F) 25)))

(defun encode-rv-i (opcode rd funct3 rs1 imm)
  "Encode I-type: imm[11:0] rs1 funct3 rd opcode."
  (riscv-check-signed imm 12 "I-type")
  (logior opcode (ash (logand rd #x1F) 7) (ash (logand funct3 #x7) 12)
          (ash (logand rs1 #x1F) 15)
          (ash (riscv-signed-field imm 12) 20)))

(defun encode-rv-s (opcode funct3 rs1 rs2 imm)
  "Encode S-type: imm[11:5] rs2 rs1 funct3 imm[4:0] opcode."
  (riscv-check-signed imm 12 "S-type")
  (let ((uimm (riscv-signed-field imm 12)))
    (logior opcode (ash (ldb (byte 5 0) uimm) 7) (ash (logand funct3 #x7) 12)
            (ash (logand rs1 #x1F) 15) (ash (logand rs2 #x1F) 20)
            (ash (ldb (byte 7 5) uimm) 25))))

(defun encode-rv-b (opcode funct3 rs1 rs2 offset)
  "Encode B-type: imm[12|10:5] rs2 rs1 funct3 imm[4:1|11] opcode."
  (riscv-check-branch-aligned offset "B-type")
  (riscv-check-signed offset 13 "B-type")
  (let ((uimm (riscv-signed-field offset 13)))
    (logior opcode
            (ash (ldb (byte 1 11) uimm) 7)
            (ash (ldb (byte 4 1) uimm) 8)
            (ash (logand funct3 #x7) 12)
            (ash (logand rs1 #x1F) 15)
            (ash (logand rs2 #x1F) 20)
            (ash (ldb (byte 6 5) uimm) 25)
            (ash (ldb (byte 1 12) uimm) 31))))

(defun encode-rv-u (opcode rd imm)
  "Encode U-type: imm[31:12] rd opcode. IMM is the full upper immediate value."
  (logior opcode (ash (logand rd #x1F) 7) (logand imm #xFFFFF000)))

(defun encode-rv-j (opcode rd offset)
  "Encode J-type: imm[20|10:1|11|19:12] rd opcode."
  (riscv-check-branch-aligned offset "J-type")
  (riscv-check-signed offset 21 "J-type")
  (let ((uimm (riscv-signed-field offset 21)))
    (logior opcode
            (ash (logand rd #x1F) 7)
            (ash (ldb (byte 8 12) uimm) 12)
            (ash (ldb (byte 1 11) uimm) 20)
            (ash (ldb (byte 10 1) uimm) 21)
            (ash (ldb (byte 1 20) uimm) 31))))

;;; RV64IMAFDC instruction encoders

(defconstant +rv-op+ #x33)
(defconstant +rv-op-imm+ #x13)
(defconstant +rv-load+ #x03)
(defconstant +rv-store+ #x23)
(defconstant +rv-branch+ #x63)
(defconstant +rv-jalr+ #x67)
(defconstant +rv-jal+ #x6F)
(defconstant +rv-lui+ #x37)
(defconstant +rv-auipc+ #x17)
(defconstant +rv-amo+ #x2F)
(defconstant +rv-load-fp+ #x07)
(defconstant +rv-store-fp+ #x27)
(defconstant +rv-op-fp+ #x53)
(defconstant +rv-misc-mem+ #x0F)

(defun encode-rv-add (rd rs1 rs2) (encode-rv-r +rv-op+ rd 0 rs1 rs2 0))
(defun encode-rv-sub (rd rs1 rs2) (encode-rv-r +rv-op+ rd 0 rs1 rs2 #x20))
(defun encode-rv-or  (rd rs1 rs2) (encode-rv-r +rv-op+ rd 6 rs1 rs2 0))
(defun encode-rv-mul (rd rs1 rs2) (encode-rv-r +rv-op+ rd 0 rs1 rs2 1))
(defun encode-rv-div (rd rs1 rs2) (encode-rv-r +rv-op+ rd 4 rs1 rs2 1))
(defun encode-rv-rem (rd rs1 rs2) (encode-rv-r +rv-op+ rd 6 rs1 rs2 1))
(defun encode-rv-slt (rd rs1 rs2) (encode-rv-r +rv-op+ rd 2 rs1 rs2 0))
(defun encode-rv-addi (rd rs1 imm) (encode-rv-i +rv-op-imm+ rd 0 rs1 imm))
(defun encode-rv-slli (rd rs1 shamt) (encode-rv-i +rv-op-imm+ rd 1 rs1 shamt))
(defun encode-rv-slti (rd rs1 imm) (encode-rv-i +rv-op-imm+ rd 2 rs1 imm))
(defun encode-rv-ld (rd rs1 imm) (encode-rv-i +rv-load+ rd 3 rs1 imm))
(defun encode-rv-sd (rs2 rs1 imm) (encode-rv-s +rv-store+ 3 rs1 rs2 imm))
(defun encode-rv-beq (rs1 rs2 offset) (encode-rv-b +rv-branch+ 0 rs1 rs2 offset))
(defun encode-rv-bne (rs1 rs2 offset) (encode-rv-b +rv-branch+ 1 rs1 rs2 offset))
(defun encode-rv-blt (rs1 rs2 offset) (encode-rv-b +rv-branch+ 4 rs1 rs2 offset))
(defun encode-rv-bge (rs1 rs2 offset) (encode-rv-b +rv-branch+ 5 rs1 rs2 offset))
(defun encode-rv-jal (rd offset) (encode-rv-j +rv-jal+ rd offset))
(defun encode-rv-jalr (rd rs1 imm) (encode-rv-i +rv-jalr+ rd 0 rs1 imm))
(defun encode-rv-lui (rd imm) (encode-rv-u +rv-lui+ rd imm))
(defun encode-rv-auipc (rd imm) (encode-rv-u +rv-auipc+ rd imm))
(defun encode-rv-fence (pred succ) (encode-rv-i +rv-misc-mem+ 0 0 0 (logior (ash pred 4) succ)))

(defun encode-rv-fld (rd rs1 imm) (encode-rv-i +rv-load-fp+ rd 3 rs1 imm))
(defun encode-rv-fsd (rs2 rs1 imm) (encode-rv-s +rv-store-fp+ 3 rs1 rs2 imm))
(defun encode-rv-fadd-s (rd rs1 rs2 &optional (rm 0)) (encode-rv-r +rv-op-fp+ rd rm rs1 rs2 0))
(defun encode-rv-fsub-s (rd rs1 rs2 &optional (rm 0)) (encode-rv-r +rv-op-fp+ rd rm rs1 rs2 4))
(defun encode-rv-fmul-s (rd rs1 rs2 &optional (rm 0)) (encode-rv-r +rv-op-fp+ rd rm rs1 rs2 8))
(defun encode-rv-fdiv-s (rd rs1 rs2 &optional (rm 0)) (encode-rv-r +rv-op-fp+ rd rm rs1 rs2 12))
(defun encode-rv-fadd-d (rd rs1 rs2 &optional (rm 0)) (encode-rv-r +rv-op-fp+ rd rm rs1 rs2 1))
(defun encode-rv-fsub-d (rd rs1 rs2 &optional (rm 0)) (encode-rv-r +rv-op-fp+ rd rm rs1 rs2 5))
(defun encode-rv-fmul-d (rd rs1 rs2 &optional (rm 0)) (encode-rv-r +rv-op-fp+ rd rm rs1 rs2 9))
(defun encode-rv-fdiv-d (rd rs1 rs2 &optional (rm 0)) (encode-rv-r +rv-op-fp+ rd rm rs1 rs2 13))
(defun encode-rv-fcvt-d-s (rd rs1 &optional (rm 0)) (encode-rv-r +rv-op-fp+ rd rm rs1 0 #x21))
(defun encode-rv-fcvt-s-d (rd rs1 &optional (rm 0)) (encode-rv-r +rv-op-fp+ rd rm rs1 1 #x20))
(defun encode-rv-fcvt-l-d (rd rs1 &optional (rm 0)) (encode-rv-r +rv-op-fp+ rd rm rs1 2 #x61))
(defun encode-rv-fcvt-d-l (rd rs1 &optional (rm 0)) (encode-rv-r +rv-op-fp+ rd rm rs1 2 #x69))

(defun encode-rv-amo (funct5 rd rs1 rs2 &key (aq 0) (rl 0))
  "Encode A-extension LR/SC/AMO instruction."
  (encode-rv-r +rv-amo+ rd 3 rs1 rs2 (logior (ash funct5 2) (ash aq 1) rl)))

(defun encode-rv-lr-d (rd rs1 &key (aq 0) (rl 0)) (encode-rv-amo #b00010 rd rs1 0 :aq aq :rl rl))
(defun encode-rv-sc-d (rd rs1 rs2 &key (aq 0) (rl 0)) (encode-rv-amo #b00011 rd rs1 rs2 :aq aq :rl rl))
(defun encode-rv-amoadd-d (rd rs1 rs2 &key (aq 0) (rl 0)) (encode-rv-amo #b00000 rd rs1 rs2 :aq aq :rl rl))
(defun encode-rv-amoswap-d (rd rs1 rs2 &key (aq 0) (rl 0)) (encode-rv-amo #b00001 rd rs1 rs2 :aq aq :rl rl))
(defun encode-rv-amoand-d (rd rs1 rs2 &key (aq 0) (rl 0)) (encode-rv-amo #b01100 rd rs1 rs2 :aq aq :rl rl))
(defun encode-rv-amoor-d (rd rs1 rs2 &key (aq 0) (rl 0)) (encode-rv-amo #b01000 rd rs1 rs2 :aq aq :rl rl))

(defun encode-rv-czero-eqz (rd rs1 rs2)
  "Encode Zicond CZERO.EQZ rd, rs1, rs2."
  (encode-rv-r +rv-op+ rd #b101 rs1 rs2 #b0000111))

(defun encode-rv-czero-nez (rd rs1 rs2)
  "Encode Zicond CZERO.NEZ rd, rs1, rs2."
  (encode-rv-r +rv-op+ rd #b111 rs1 rs2 #b0000111))

;;; Compressed extension helpers for common patterns

(defun riscv-compressed-reg-p (reg)
  "Return true when REG is encodable as compressed register x8..x15."
  (<= 8 reg 15))

(defun riscv-compressed-reg3 (reg)
  "Return the compressed 3-bit register code for x8..x15."
  (unless (riscv-compressed-reg-p reg)
    (error "Register X~D is not encodable in compressed 3-bit register class" reg))
  (- reg 8))

(defun encode-rvc-nop () #x0001)

(defun encode-rvc-addi (rd imm)
  "Encode C.ADDI rd, imm for non-zero RD and signed 6-bit IMM."
  (riscv-check-signed imm 6 "C.ADDI")
  (let ((uimm (riscv-signed-field imm 6)))
    (logior #b01 (ash #b000 13) (ash (ldb (byte 1 5) uimm) 12)
            (ash rd 7) (ash (ldb (byte 5 0) uimm) 2))))

(defun encode-rvc-li (rd imm)
  "Encode C.LI rd, imm for signed 6-bit IMM."
  (riscv-check-signed imm 6 "C.LI")
  (let ((uimm (riscv-signed-field imm 6)))
    (logior #b01 (ash #b010 13) (ash (ldb (byte 1 5) uimm) 12)
            (ash rd 7) (ash (ldb (byte 5 0) uimm) 2))))

(defun encode-rvc-ldsp (rd offset)
  "Encode C.LDSP rd, offset(sp). OFFSET must be an unsigned multiple of 8."
  (unless (and (plusp rd) (not (minusp offset)) (zerop (mod offset 8)) (< offset 512))
    (error "C.LDSP requires rd!=x0 and unsigned 8-byte offset <512, got rd=~D offset=~D" rd offset))
  (let ((imm (ash offset -3)))
    (logior #b10 (ash #b011 13) (ash (ldb (byte 1 5) imm) 12)
            (ash rd 7) (ash (ldb (byte 2 3) imm) 5) (ash (ldb (byte 3 0) imm) 2))))

(defun encode-rvc-sdsp (rs2 offset)
  "Encode C.SDSP rs2, offset(sp). OFFSET must be an unsigned multiple of 8."
  (unless (and (not (minusp offset)) (zerop (mod offset 8)) (< offset 512))
    (error "C.SDSP requires unsigned 8-byte offset <512, got ~D" offset))
  (let ((imm (ash offset -3)))
    (logior #b10 (ash #b111 13) (ash (ldb (byte 3 3) imm) 10)
            (ash rs2 2) (ash (ldb (byte 3 0) imm) 7))))

(defun encode-rvc-j (offset)
  "Encode C.J offset for signed 12-bit halfword-aligned OFFSET."
  (riscv-check-branch-aligned offset "C.J")
  (riscv-check-signed offset 12 "C.J")
  (let ((imm (riscv-signed-field offset 12)))
    (logior #b01 (ash #b101 13)
            (ash (ldb (byte 1 11) imm) 12)
            (ash (ldb (byte 1 4) imm) 11)
            (ash (ldb (byte 2 8) imm) 9)
            (ash (ldb (byte 1 10) imm) 8)
            (ash (ldb (byte 1 6) imm) 7)
            (ash (ldb (byte 1 7) imm) 6)
            (ash (ldb (byte 3 1) imm) 3)
            (ash (ldb (byte 1 5) imm) 2))))

;;; Immediate lowering and VM emitters

(defun riscv64-small-immediate-p (value)
  (<= -2048 value 2047))

(defun emit-riscv64-li (rd value stream)
  "Emit a conservative 64-bit integer materialization sequence into RD."
  (let ((signed (if (and (integerp value) (> value #x7FFFFFFFFFFFFFFF))
                    (- value #x10000000000000000)
                    value)))
    (cond
      ((riscv64-small-immediate-p signed)
       (emit-riscv32 (encode-rv-addi rd +rv-zero+ signed) stream))
      ((typep signed '(signed-byte 32))
       (let* ((upper (ash (+ signed #x800) -12))
              (lower (- signed (ash upper 12))))
         (emit-riscv32 (encode-rv-lui rd (ash upper 12)) stream)
         (unless (zerop lower)
           (emit-riscv32 (encode-rv-addi rd rd lower) stream))))
       (t
        ;; Full 64-bit constant via 6-instruction sequence: LUI+ADDI+SLLI+ADDI+SLLI+ADDI
        (let* ((b0 (ldb (byte 12 0) value))
               (b1 (ldb (byte 11 12) value))
               (b2 (ldb (byte 11 23) value))
               (b3 (ldb (byte 11 34) value))
               (b4 (ldb (byte 10 45) value))
               (b5 (ldb (byte 9 54) value)))
          (emit-riscv32 (encode-rv-lui rd (+ b2 (if (logbitp 11 b2) 1 0)
                                            (ash (+ b3 (if (logbitp 11 b3) 1 0)) 11)
                                            (ash (+ b4 (if (logbitp 10 b4) 1 0)) 22)
                                            (ash b5 32))) stream)
          (when (or (not (zerop b0)) (not (zerop b1)))
            (emit-riscv32 (encode-rv-addi rd rd (logior b0 (ash b1 12))) stream))
          (emit-riscv32 (encode-rv-slli rd rd 11) stream)
          (emit-riscv32 (encode-rv-addi rd rd (+ b1 (if (logbitp 11 b1) 1 0))) stream)
          (emit-riscv32 (encode-rv-slli rd rd 11) stream)
          (emit-riscv32 (encode-rv-addi rd rd b0) stream))))))

(defmacro define-riscv64-binary-emitter (fn-name encode-fn)
  "Define a RISC-V integer binary VM emitter using ENCODE-FN(rd rs1 rs2)."
  `(defun ,fn-name (inst stream)
     (emit-riscv32 (,encode-fn (riscv64-reg (vm-dst inst))
                               (riscv64-reg (vm-lhs inst))
                               (riscv64-reg (vm-rhs inst)))
                   stream)))

(defmacro define-riscv64-fp-binary-emitter (fn-name encode-fn)
  "Define a RISC-V double-precision FP binary VM emitter."
  `(defun ,fn-name (inst stream)
     (emit-riscv32 (,encode-fn (riscv64-freg (vm-dst inst))
                               (riscv64-freg (vm-lhs inst))
                               (riscv64-freg (vm-rhs inst)))
                   stream)))

(defun emit-riscv64-vm-const (inst stream)
  (emit-riscv64-li (riscv64-reg (vm-dst inst))
                   (vm-const-to-integer (vm-value inst))
                   stream))

(defun emit-riscv64-vm-move (inst stream)
  (let ((rd (riscv64-reg (vm-dst inst)))
        (rs (riscv64-reg (vm-src inst))))
    (unless (= rd rs)
      (emit-riscv32 (encode-rv-addi rd rs 0) stream))))

(define-riscv64-binary-emitter emit-riscv64-vm-add encode-rv-add)
(define-riscv64-binary-emitter emit-riscv64-vm-sub encode-rv-sub)
(define-riscv64-binary-emitter emit-riscv64-vm-mul encode-rv-mul)
(define-riscv64-binary-emitter emit-riscv64-vm-div encode-rv-div)
(define-riscv64-binary-emitter emit-riscv64-vm-rem encode-rv-rem)
(define-riscv64-fp-binary-emitter emit-riscv64-vm-float-add encode-rv-fadd-d)
(define-riscv64-fp-binary-emitter emit-riscv64-vm-float-sub encode-rv-fsub-d)
(define-riscv64-fp-binary-emitter emit-riscv64-vm-float-mul encode-rv-fmul-d)
(define-riscv64-fp-binary-emitter emit-riscv64-vm-float-div encode-rv-fdiv-d)

(defun emit-riscv64-vm-halt (inst stream)
  "Move VM result to the psABI integer return register A0."
  (let ((rs (riscv64-reg (vm-reg inst))))
    (unless (= rs +rv-a0+)
      (emit-riscv32 (encode-rv-addi +rv-a0+ rs 0) stream))))

(defun emit-riscv64-vm-ret (inst stream)
  "Emit a direct function return via JALR x0, ra, 0."
  (declare (ignore inst))
  (emit-riscv32 (encode-rv-jalr +rv-zero+ +rv-ra+ 0) stream))

(defun emit-riscv64-vm-spill-store (inst stream)
  "Store a physical register to a spill slot."
  (let ((src (riscv64-reg-number (vm-spill-src inst)))
        (offset (riscv64-spill-slot-offset (vm-spill-slot inst))))
    (emit-riscv32 (encode-rv-sd src *current-riscv64-spill-base-reg* offset) stream)))

(defun emit-riscv64-vm-spill-load (inst stream)
  "Load a physical register from a spill slot."
  (let ((dst (riscv64-reg-number (vm-spill-dst inst)))
        (offset (riscv64-spill-slot-offset (vm-spill-slot inst))))
    (emit-riscv32 (encode-rv-ld dst *current-riscv64-spill-base-reg* offset) stream)))

(defun emit-riscv64-vm-jump (inst stream current-pos label-offsets)
  "Emit JAL x0, target for unconditional jump."
  (let* ((target-pos (or (gethash (vm-label-name inst) label-offsets)
                         (error "Unknown RISC-V branch target: ~A" (vm-label-name inst))))
         (offset (- target-pos current-pos)))
    (emit-riscv32 (encode-rv-jal +rv-zero+ offset) stream)))

(defun emit-riscv64-vm-jump-zero (inst stream current-pos label-offsets)
  "Emit BEQ reg, zero, target for conditional branch on zero."
  (let* ((rs (riscv64-reg (vm-reg inst)))
         (target-pos (or (gethash (vm-label-name inst) label-offsets)
                         (error "Unknown RISC-V branch target: ~A" (vm-label-name inst))))
         (offset (- target-pos current-pos)))
    (emit-riscv32 (encode-rv-beq rs +rv-zero+ offset) stream)))

(defun emit-riscv64-vm-select (inst stream)
  "Emit select; use Zicond when enabled, otherwise preserve existing placeholder."
  (let ((rd (riscv64-reg (vm-dst inst)))
        (cond-reg (riscv64-reg (vm-select-cond-reg inst)))
        (then (riscv64-reg (vm-select-then-reg inst)))
        (else (riscv64-reg (vm-select-else-reg inst))))
    (declare (ignorable cond-reg else))
    (if *riscv64-zicond-enabled*
        (progn
          ;; rd = (cond != 0) ? then : else
          (emit-riscv32 (encode-rv-czero-eqz +rv-t0+ then cond-reg) stream)
          (emit-riscv32 (encode-rv-czero-nez rd else cond-reg) stream)
          (emit-riscv32 (encode-rv-or rd rd +rv-t0+) stream))
        (progn
          (emit-riscv32 (encode-rv-addi rd then 0) stream)))))

(defparameter *riscv64-emitter-table*
  (let ((ht (make-hash-table :test #'eq)))
    (setf (gethash 'vm-const ht) #'emit-riscv64-vm-const)
    (setf (gethash 'vm-move ht) #'emit-riscv64-vm-move)
    (setf (gethash 'vm-add ht) #'emit-riscv64-vm-add)
    (setf (gethash 'vm-integer-add ht) #'emit-riscv64-vm-add)
    (setf (gethash 'vm-sub ht) #'emit-riscv64-vm-sub)
    (setf (gethash 'vm-integer-sub ht) #'emit-riscv64-vm-sub)
    (setf (gethash 'vm-mul ht) #'emit-riscv64-vm-mul)
    (setf (gethash 'vm-integer-mul ht) #'emit-riscv64-vm-mul)
    (setf (gethash 'vm-div ht) #'emit-riscv64-vm-div)
    (setf (gethash 'vm-truncate ht) #'emit-riscv64-vm-div)
    (setf (gethash 'vm-rem ht) #'emit-riscv64-vm-rem)
    (setf (gethash 'vm-float-add ht) #'emit-riscv64-vm-float-add)
    (setf (gethash 'vm-float-sub ht) #'emit-riscv64-vm-float-sub)
    (setf (gethash 'vm-float-mul ht) #'emit-riscv64-vm-float-mul)
    (setf (gethash 'vm-float-div ht) #'emit-riscv64-vm-float-div)
    (setf (gethash 'vm-halt ht) #'emit-riscv64-vm-halt)
    (setf (gethash 'vm-ret ht) #'emit-riscv64-vm-ret)
    (setf (gethash 'vm-select ht) #'emit-riscv64-vm-select)
    (setf (gethash 'vm-spill-store ht) #'emit-riscv64-vm-spill-store)
    (setf (gethash 'vm-spill-load ht) #'emit-riscv64-vm-spill-load)
    ht)
  "Maps VM instruction type symbols to RISC-V emitter functions.")

(defun riscv64-instruction-size (inst)
  "Return encoded byte size of INST for conservative fixed-width layout."
  (case (type-of inst)
    ((vm-label vm-print vm-closure vm-register-function vm-set-global) 0)
    (vm-select (if *riscv64-zicond-enabled* 12 4))
    (otherwise 4)))

(defun build-riscv64-label-offsets (instructions prologue-size)
  "Build label-name to byte-offset table for RISC-V emission."
  (let ((offsets (make-hash-table :test #'equal))
        (pos prologue-size))
    (dolist (inst instructions offsets)
      (when (typep inst 'vm-label)
        (setf (gethash (vm-name inst) offsets) pos))
      (incf pos (riscv64-instruction-size inst)))))

(defun emit-riscv64-instruction (inst stream current-pos label-offsets)
  "Emit RISC-V machine code for one VM instruction."
  (let ((tp (type-of inst)))
    (cond
      ((member tp '(vm-label vm-print vm-closure vm-register-function vm-set-global) :test #'eq)
       nil)
      ((eq tp 'vm-jump)
       (emit-riscv64-vm-jump inst stream current-pos label-offsets))
      ((eq tp 'vm-jump-zero)
       (emit-riscv64-vm-jump-zero inst stream current-pos label-offsets))
      ((eq tp 'vm-call)
       (emit-riscv32 (encode-rv-jalr +rv-ra+ (riscv64-reg (vm-func-reg inst)) 0) stream))
      ((eq tp 'vm-tail-call)
       (emit-riscv32 (encode-rv-jalr +rv-zero+ (riscv64-reg (vm-func-reg inst)) 0) stream))
      (t
       (let ((emitter (gethash tp *riscv64-emitter-table*)))
         (if emitter
             (funcall emitter inst stream)
             (error "Unsupported RISC-V instruction: ~A" tp)))))))

(defun riscv64-used-callee-saved-regs (ra &key frame-pointer-p)
  "Return numeric callee-saved registers used by RA, optionally forcing S0/FP."
  (let ((phys-regs (loop for phys being the hash-values of (regalloc-assignment ra)
                         when (assoc phys *riscv64-reg-number*)
                           collect (riscv64-reg-number phys))))
    (remove-duplicates
     (append (when frame-pointer-p (list +rv-fp+ +rv-ra+))
             (remove-if-not (lambda (reg) (member reg phys-regs))
                            '(8 9 18 19 20 21 22 23 24 25 26 27)))
     :test #'=)))

(defun emit-riscv64-prologue (stream save-regs spill-frame-size frame-pointer-p)
  "Emit RISC-V psABI prologue for SAVE-REGS and spill frame."
  (let* ((save-bytes (* 8 (length save-regs)))
         (frame-size (+ save-bytes spill-frame-size)))
    (when (plusp frame-size)
      (emit-riscv32 (encode-rv-addi +rv-sp+ +rv-sp+ (- frame-size)) stream))
    (loop for reg in save-regs
          for offset from 0 by 8
          do (emit-riscv32 (encode-rv-sd reg +rv-sp+ offset) stream))
    (when frame-pointer-p
      (emit-riscv32 (encode-rv-addi +rv-fp+ +rv-sp+ frame-size) stream))))

(defun emit-riscv64-epilogue (stream save-regs spill-frame-size)
  "Emit RISC-V psABI epilogue and return."
  (let* ((save-bytes (* 8 (length save-regs)))
         (frame-size (+ save-bytes spill-frame-size)))
    (loop for reg in save-regs
          for offset from 0 by 8
          do (emit-riscv32 (encode-rv-ld reg +rv-sp+ offset) stream))
    (when (plusp frame-size)
      (emit-riscv32 (encode-rv-addi +rv-sp+ +rv-sp+ frame-size) stream))
    (emit-riscv32 (encode-rv-jalr +rv-zero+ +rv-ra+ 0) stream)))

(defun riscv64-compute-float-vregs (instructions)
  "Conservatively mark VM virtual registers that hold unboxed floats."
  (let ((float-vregs (make-hash-table :test #'eq)))
    (flet ((mark (reg)
             (when reg
               (setf (gethash reg float-vregs) t))))
      (dolist (inst instructions)
        (typecase inst
          (vm-const
           (when (floatp (vm-value inst))
             (mark (vm-dst inst))))
          ((or vm-float-add vm-float-sub vm-float-mul vm-float-div)
           (mark (vm-dst inst))
           (mark (vm-lhs inst))
           (mark (vm-rhs inst))))))
    float-vregs))

(defun emit-riscv64-program (program stream)
  "Emit RISC-V machine code for the entire VM program."
  (let* ((instructions (vm-program-instructions program))
         (leaf-p (vm-program-leaf-p program))
         (spill-count (regalloc-spill-count *current-riscv64-regalloc*))
         (frame-pointer-p (and (not *riscv64-omit-frame-pointer*)
                               (or (not leaf-p) (plusp spill-count))))
         (spill-frame-size (* 8 spill-count))
         (*current-riscv64-spill-base-reg* (if frame-pointer-p +rv-fp+ +rv-sp+))
         (*current-riscv64-spill-offset-bias* (if frame-pointer-p 0 spill-frame-size))
         (save-regs (riscv64-used-callee-saved-regs *current-riscv64-regalloc*
                                                    :frame-pointer-p frame-pointer-p))
         (prologue-size (+ (if (plusp (+ (* 8 (length save-regs)) spill-frame-size)) 4 0)
                           (* 4 (length save-regs))
                           (if frame-pointer-p 4 0)))
         (ordered-instructions (codegen-hot-cold-ordered-instructions instructions))
         (label-offsets (build-riscv64-label-offsets ordered-instructions prologue-size)))
    (emit-riscv64-prologue stream save-regs spill-frame-size frame-pointer-p)
    (let ((pos prologue-size))
      (dolist (inst ordered-instructions)
        (emit-riscv64-instruction inst stream pos label-offsets)
        (incf pos (riscv64-instruction-size inst))))
    (emit-riscv64-epilogue stream save-regs spill-frame-size)))

;;; Public API

(defun compile-to-riscv64-bytes (program &key retpoline stack-protector shadow-stack
                                         asan msan tsan ubsan hwasan)
  "Compile VM PROGRAM to RV64IMAFDC machine code bytes."
  (declare (ignore retpoline stack-protector shadow-stack asan msan tsan ubsan hwasan))
  (let* ((instructions (schedule-pre-ra (vm-program-instructions program)))
          (float-vregs (riscv64-compute-float-vregs instructions))
          (ra (allocate-registers instructions (riscv64-codegen-target) float-vregs))
          (post-ra-instructions (schedule-post-ra (regalloc-instructions ra) ra))
          (allocated-program (make-vm-program
                              :instructions post-ra-instructions
                              :result-register (vm-program-result-register program)
                             :leaf-p (vm-program-leaf-p program))))
    (let ((*current-riscv64-regalloc* ra)
          (*current-riscv64-float-vregs* float-vregs))
      (with-output-to-vector (stream)
        (emit-riscv64-program allocated-program stream)))))
