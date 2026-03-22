;;;; src/backend/x86-64-codegen.lisp - x86-64 Machine Code Generation
;;;
;;; Generates native x86-64 machine code bytes from VM instructions.
;;; Uses REX prefixes, ModR/M encoding, and proper instruction encoding.

(in-package :cl-cc)

;;; x86-64 Register Encoding

;; Register codes (4-bit values)
;; Low registers (0-7): RAX-RDI, REX.B = 0
;; High registers (8-15): R8-R15, REX.B = 1
(defconstant +rax+ 0)
(defconstant +rcx+ 1)
(defconstant +rdx+ 2)
(defconstant +rbx+ 3)
(defconstant +rsp+ 4)
(defconstant +rbp+ 5)
(defconstant +rsi+ 6)
(defconstant +rdi+ 7)
(defconstant +r8+  8)
(defconstant +r9+  9)
(defconstant +r10+ 10)
(defconstant +r11+ 11)
(defconstant +r12+ 12)
(defconstant +r13+ 13)
(defconstant +r14+ 14)
(defconstant +r15+ 15)

;; Calling convention (System V AMD64 ABI)
;; Arguments: RDI, RSI, RDX, RCX, R8, R9
;; Return: RAX
;; Preserved: RBX, RBP, R12-R15
;; Temp: RAX, RCX, RDX, RSI, RDI, R8-R11

;;; Output Stream Utilities

(defmacro with-output-to-vector ((stream-var) &body body)
  "Execute BODY with STREAM-VAR bound to an output stream that collects bytes into a vector."
  `(let ((bytes '()))
      (labels ((stream-write-byte (byte)
                (push byte bytes)))
        (let ((,stream-var #'stream-write-byte))
          ,@body
          (coerce (nreverse bytes) '(simple-array (unsigned-byte 8) (*)))))))

(defun emit-byte (byte stream)
  "Write single byte to stream (which is a function that takes a byte)."
  (funcall stream (logand byte #xFF)))

(defun emit-word (word stream)
  "Write 16-bit value (little-endian)."
  (emit-byte (logand word #xFF) stream)
  (emit-byte (logand (ash word -8) #xFF) stream))

(defun emit-dword (dword stream)
  "Write 32-bit value (little-endian)."
  (emit-byte (logand dword #xFF) stream)
  (emit-byte (logand (ash dword -8) #xFF) stream)
  (emit-byte (logand (ash dword -16) #xFF) stream)
  (emit-byte (logand (ash dword -24) #xFF) stream))

(defun emit-qword (qword stream)
  "Write 64-bit value (little-endian)."
  (emit-dword (logand qword #xFFFFFFFF) stream)
  (emit-dword (ash qword -32) stream))

;;; REX Prefix

(defun rex-prefix (&key (w 0) (r 0) (x 0) (b 0))
  "Build REX prefix byte.
   W=1: 64-bit operand, R: ModR/M reg extension, B: ModR/M r/m extension"
  (+ #x40 (ash w 3) (ash r 2) (ash x 1) b))

;;; ModR/M Encoding

(defun modrm (mod reg rm)
  "Build ModR/M byte.
   MOD: 2 bits (00=mem, 01=disp8, 10=disp32, 11=reg)
   REG: 3 bits (register or opcode extension)
   RM: 3 bits (register or addressing mode)"
  (+ (ash (logand mod #x3) 6)
     (ash (logand reg #x7) 3)
     (logand rm #x7)))

;;; Instructions

(defun emit-mov-rr64 (dst src stream)
  "MOV dst, src (64-bit register to register).

   Encoding: REX.W + 89 /r (if src first)
             REX.W + 8B /r (if dst first)"
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash src -3)) stream)
  (emit-byte #x89 stream)
  (emit-byte (modrm 3 src dst) stream))

(defun emit-mov-ri64 (dst imm stream)
  "MOV dst, imm64 (64-bit immediate to register).

   Encoding: REX.W + B8+ rd"
  (emit-byte (rex-prefix :w 1 :b (ash dst -3)) stream)
  (emit-byte (+ #xB8 (logand dst #x7)) stream)
  (emit-qword imm stream))

(defun emit-mov-rm64 (dst base offset stream)
  "MOV dst, [base + offset] (load from memory).

   For offset = 0: REX.W + 8B /r (mod=00)
   For offset fits in byte: REX.W + 8B /r (mod=01)"
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash base -3)) stream)
  (emit-byte #x8B stream)
  (if (zerop offset)
      (emit-byte (modrm 0 dst base) stream)
      (progn
        (emit-byte (modrm 1 dst base) stream)
        (emit-byte (logand offset #xFF) stream))))

(defun emit-mov-mr64 (base offset src stream)
  "MOV [base + offset], src (store to memory)."
  (emit-byte (rex-prefix :w 1 :r (ash src -3) :b (ash base -3)) stream)
  (emit-byte #x89 stream)
  (if (zerop offset)
      (emit-byte (modrm 0 src base) stream)
      (progn
        (emit-byte (modrm 1 src base) stream)
        (emit-byte (logand offset #xFF) stream))))

(defun emit-add-rr64 (dst src stream)
  "ADD dst, src (64-bit).

   Encoding: REX.W + 01 /r"
  (emit-byte (rex-prefix :w 1 :r (ash src -3) :b (ash dst -3)) stream)
  (emit-byte #x01 stream)
  (emit-byte (modrm 3 src dst) stream))

(defun emit-sub-rr64 (dst src stream)
  "SUB dst, src (64-bit).

   Encoding: REX.W + 29 /r"
  (emit-byte (rex-prefix :w 1 :r (ash src -3) :b (ash dst -3)) stream)
  (emit-byte #x29 stream)
  (emit-byte (modrm 3 src dst) stream))

(defun emit-imul-rr64 (dst src stream)
  "IMUL dst, src (64-bit signed multiply).

   Encoding: REX.W + 0F AF /r"
  (emit-byte (rex-prefix :w 1 :r (ash dst -3) :b (ash src -3)) stream)
  (emit-byte #x0F stream)
  (emit-byte #xAF stream)
  (emit-byte (modrm 3 dst src) stream))

(defun emit-cmp-rr64 (op1 op2 stream)
  "CMP op1, op2 (64-bit compare).

   Encoding: REX.W + 39 /r"
  (emit-byte (rex-prefix :w 1 :r (ash op2 -3) :b (ash op1 -3)) stream)
  (emit-byte #x39 stream)
  (emit-byte (modrm 3 op2 op1) stream))

(defun emit-push-r64 (reg stream)
  "PUSH reg (64-bit).

   Encoding: 50+ rd"
  (emit-byte (+ #x50 (logand reg #x7)) stream))

(defun emit-pop-r64 (reg stream)
  "POP reg (64-bit).

   Encoding: 58+ rd"
  (emit-byte (+ #x58 (logand reg #x7)) stream))

(defun emit-call-r64 (reg stream)
  "CALL reg (indirect call).

   Encoding: FF /2"
  (emit-byte #xFF stream)
  (emit-byte (modrm 3 2 reg) stream))

(defun emit-ret (stream)
  "RET (return).

   Encoding: C3"
  (emit-byte #xC3 stream))

(defun emit-nop (stream)
  "NOP (no operation).

   Encoding: 90"
  (emit-byte #x90 stream))

(defun emit-jmp-rel32 (offset stream)
  "JMP rel32 (near jump).

   Encoding: E9 cd"
  (emit-byte #xE9 stream)
  (emit-dword offset stream))

(defun emit-je-rel32 (offset stream)
  "JE rel32 (jump if equal).

   Encoding: 0F 84 cd"
  (emit-byte #x0F stream)
  (emit-byte #x84 stream)
  (emit-dword offset stream))

(defun emit-jne-rel32 (offset stream)
  "JNE rel32 (jump if not equal).

   Encoding: 0F 85 cd"
  (emit-byte #x0F stream)
  (emit-byte #x85 stream)
  (emit-dword offset stream))

;;; VM to Machine Code Translation

(defvar *current-regalloc* nil
  "When non-nil, the current regalloc-result used during code generation.")

(defparameter *vm-reg-map*
  `((:R0 . ,+rax+)
    (:R1 . ,+rcx+)
    (:R2 . ,+rdx+)
    (:R3 . ,+rbx+)
    (:R4 . ,+rsi+)
    (:R5 . ,+rdi+)
    (:R6 . ,+r8+)
    (:R7 . ,+r9+))
  "Mapping from VM keyword registers to x86-64 register codes.")

(defun vm-reg-to-x86 (vm-reg)
  "Map VM register to x86-64 register code.
   When *current-regalloc* is set, uses register allocation results.
   Otherwise falls back to naive mapping."
  (if *current-regalloc*
      (vm-reg-to-x86-with-alloc *current-regalloc* vm-reg)
      (let ((entry (assoc vm-reg *vm-reg-map*)))
        (unless entry
          (error "VM register ~A has no x86-64 mapping (only R0-R7 supported)" vm-reg))
        (cdr entry))))

(defparameter *phys-reg-to-x86-code*
  `((:rax . ,+rax+) (:rcx . ,+rcx+) (:rdx . ,+rdx+) (:rbx . ,+rbx+)
    (:rsi . ,+rsi+) (:rdi . ,+rdi+) (:r8 . ,+r8+) (:r9 . ,+r9+)
    (:r10 . ,+r10+) (:r11 . ,+r11+) (:r12 . ,+r12+) (:r13 . ,+r13+)
    (:r14 . ,+r14+) (:r15 . ,+r15+))
  "Mapping from physical register keywords to x86-64 register codes.")

(defun vm-reg-to-x86-with-alloc (ra vm-reg)
  "Map VM register to x86-64 register code using register allocation result.
   RA is a regalloc-result, VM-REG is a virtual register keyword.
   Returns the integer register code (e.g., +rax+ = 0)."
  (let ((phys (gethash vm-reg (regalloc-assignment ra))))
    (unless phys
      (error "Virtual register ~A not allocated (possibly spilled)" vm-reg))
    (let ((entry (assoc phys *phys-reg-to-x86-code*)))
      (unless entry
        (error "Unknown physical register: ~A" phys))
      (cdr entry))))

(defun emit-vm-const (inst stream)
  "Emit code for VM CONST instruction."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (value (vm-value inst)))
    (emit-mov-ri64 dst value stream)))

(defun emit-vm-move (inst stream)
  "Emit code for VM MOVE instruction."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (src (vm-reg-to-x86 (vm-src inst))))
    (emit-mov-rr64 dst src stream)))

(defun emit-vm-add (inst stream)
  "Emit code for VM ADD instruction."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    ;; Move lhs to dst, then add rhs
    (emit-mov-rr64 dst lhs stream)
    (emit-add-rr64 dst rhs stream)))

(defun emit-vm-sub (inst stream)
  "Emit code for VM SUB instruction."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-mov-rr64 dst lhs stream)
    (emit-sub-rr64 dst rhs stream)))

(defun emit-vm-mul (inst stream)
  "Emit code for VM MUL instruction."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-mov-rr64 dst lhs stream)
    (emit-imul-rr64 dst rhs stream)))

;;; Compare with Zero

(defun emit-cmp-ri64 (reg imm stream)
  "CMP reg, imm32 (compare register with 32-bit sign-extended immediate).

   Encoding: REX.W + 81 /7 id"
  (emit-byte (rex-prefix :w 1 :b (ash reg -3)) stream)
  (emit-byte #x81 stream)
  (emit-byte (modrm 3 7 reg) stream)
  (emit-dword imm stream))

(defun emit-test-rr64 (reg1 reg2 stream)
  "TEST reg1, reg2 (bitwise AND, set flags, discard result).

   Encoding: REX.W + 85 /r"
  (emit-byte (rex-prefix :w 1 :r (ash reg2 -3) :b (ash reg1 -3)) stream)
  (emit-byte #x85 stream)
  (emit-byte (modrm 3 reg2 reg1) stream))

;;; Two-Pass Code Generation (Labels + Jumps)

(defun instruction-size (inst)
  "Estimate the size in bytes of the x86-64 encoding for a VM instruction.
   Used in first pass to build label offset table."
  (typecase inst
    (vm-const 10)    ; REX + opcode + 8-byte immediate
    (vm-move 3)      ; REX + opcode + ModR/M
    (vm-add 6)       ; mov + add (3+3)
    (vm-sub 6)       ; mov + sub (3+3)
    (vm-mul 7)       ; mov + imul (3+4, 0F AF)
    (vm-halt 3)      ; mov result to RAX (3 bytes, or 0 if already RAX)
    (vm-label 0)     ; Labels emit no code
    (vm-jump 5)      ; JMP rel32
    (vm-jump-zero 9) ; TEST + JE rel32 (3 + 6)
    (vm-print 0)     ; No-op in native code
    (vm-closure 0)   ; Skip in basic codegen
    (vm-call 0)      ; Skip in basic codegen
    (vm-ret 1)       ; RET
    (vm-spill-store 4) ; MOV [rbp-disp8], reg (REX + opcode + ModRM + disp8)
    (vm-spill-load 4)  ; MOV reg, [rbp-disp8] (REX + opcode + ModRM + disp8)
    (t 0)))

(defun build-label-offsets (instructions prologue-size)
  "Build a hash table mapping label names to byte offsets.
   First pass: walk instructions, accumulate sizes."
  (let ((offsets (make-hash-table :test #'equal))
        (pos prologue-size))
    (dolist (inst instructions)
      (when (typep inst 'vm-label)
        (setf (gethash (vm-name inst) offsets) pos))
      (incf pos (instruction-size inst)))
    offsets))

;;; VM Instruction Emitters (with label support)

(defun emit-vm-halt-inst (inst stream)
  "Emit code for VM HALT instruction.
   Moves the result register to RAX for the return value."
  (let ((result-reg (vm-reg-to-x86 (vm-reg inst))))
    (unless (= result-reg +rax+)
      (emit-mov-rr64 +rax+ result-reg stream))))

(defun emit-vm-jump-inst (inst stream current-pos label-offsets)
  "Emit code for VM JUMP instruction (unconditional jump)."
  (let* ((target-label (vm-label-name inst))
         (target-pos (gethash target-label label-offsets))
         ;; JMP rel32 is 5 bytes, offset is relative to end of instruction
         (offset (- target-pos (+ current-pos 5))))
    (emit-jmp-rel32 offset stream)))

(defun emit-vm-jump-zero-inst (inst stream current-pos label-offsets)
  "Emit code for VM JUMP-ZERO instruction (jump if register is zero).
   TEST reg, reg + JE rel32"
  (let* ((reg (vm-reg-to-x86 (vm-reg inst)))
         (target-label (vm-label-name inst))
         (target-pos (gethash target-label label-offsets))
         ;; TEST is 3 bytes, JE rel32 is 6 bytes, total 9 bytes
         (offset (- target-pos (+ current-pos 9))))
    ;; TEST reg, reg (sets ZF if reg is 0)
    (emit-test-rr64 reg reg stream)
    ;; JE rel32 (jump if zero flag set)
    (emit-je-rel32 offset stream)))

(defun emit-vm-ret-inst (inst stream)
  "Emit code for VM RET instruction."
  (declare (ignore inst))
  (emit-ret stream))

(defun emit-vm-spill-store-inst (inst stream)
  "Emit code for VM SPILL-STORE instruction: MOV [RBP - slot*8], src."
  (let* ((src-reg (vm-spill-src inst))
         (src-code (let ((entry (assoc src-reg *phys-reg-to-x86-code*)))
                     (if entry (cdr entry)
                         (error "Unknown physical register for spill store: ~A" src-reg))))
         (offset (- (* (vm-spill-slot inst) 8))))
    (emit-mov-mr64 +rbp+ offset src-code stream)))

(defun emit-vm-spill-load-inst (inst stream)
  "Emit code for VM SPILL-LOAD instruction: MOV dst, [RBP - slot*8]."
  (let* ((dst-reg (vm-spill-dst inst))
         (dst-code (let ((entry (assoc dst-reg *phys-reg-to-x86-code*)))
                     (if entry (cdr entry)
                         (error "Unknown physical register for spill load: ~A" dst-reg))))
         (offset (- (* (vm-spill-slot inst) 8))))
    (emit-mov-rm64 dst-code +rbp+ offset stream)))

(defun emit-vm-instruction-with-labels (inst stream current-pos label-offsets)
  "Emit machine code for a VM instruction, with label/jump support."
  (typecase inst
    (vm-const (emit-vm-const inst stream))
    (vm-move (emit-vm-move inst stream))
    (vm-add (emit-vm-add inst stream))
    (vm-sub (emit-vm-sub inst stream))
    (vm-mul (emit-vm-mul inst stream))
    (vm-halt (emit-vm-halt-inst inst stream))
    (vm-label nil)
    (vm-jump (emit-vm-jump-inst inst stream current-pos label-offsets))
    (vm-jump-zero (emit-vm-jump-zero-inst inst stream current-pos label-offsets))
    (vm-ret (emit-vm-ret-inst inst stream))
    (vm-print nil)
    (vm-spill-store (emit-vm-spill-store-inst inst stream))
    (vm-spill-load (emit-vm-spill-load-inst inst stream))
    (t (warn "Skipping unsupported VM instruction: ~A" (type-of inst)))))

(defun emit-vm-program (program stream)
  "Emit machine code for entire VM program.
   Uses two-pass approach: first pass builds label offset table,
   second pass emits code with resolved jump targets."
  (let* ((instructions (vm-program-instructions program))
         ;; Prologue: 6 PUSH instructions, 1 byte each
         (prologue-size 6)
         ;; First pass: build label offset table
         (label-offsets (build-label-offsets instructions prologue-size)))

    ;; Prologue: save callee-saved registers
    (emit-push-r64 +rbx+ stream)
    (emit-push-r64 +rbp+ stream)
    (emit-push-r64 +r12+ stream)
    (emit-push-r64 +r13+ stream)
    (emit-push-r64 +r14+ stream)
    (emit-push-r64 +r15+ stream)

    ;; Second pass: emit instructions with resolved jumps
    (let ((pos prologue-size))
      (dolist (inst instructions)
        (emit-vm-instruction-with-labels inst stream pos label-offsets)
        (incf pos (instruction-size inst))))

    ;; Epilogue: restore callee-saved registers
    (emit-pop-r64 +r15+ stream)
    (emit-pop-r64 +r14+ stream)
    (emit-pop-r64 +r13+ stream)
    (emit-pop-r64 +r12+ stream)
    (emit-pop-r64 +rbp+ stream)
    (emit-pop-r64 +rbx+ stream)

    ;; Return
    (emit-ret stream)))

;;; Public API

(defun compile-to-x86-64-bytes (program)
  "Compile VM program to x86-64 machine code bytes.

   Returns: (simple-array (unsigned-byte 8) (*))"
  ;; Run register allocation before emitting machine code
  (let* ((instructions (vm-program-instructions program))
         (ra (allocate-registers instructions *x86-64-calling-convention*))
         (allocated-program (make-vm-program
                             :instructions (regalloc-instructions ra)
                             :result-register (vm-program-result-register program))))
    ;; Store the regalloc result for use during code generation
    (let ((*current-regalloc* ra))
      (with-output-to-vector (stream)
        (emit-vm-program allocated-program stream)))))
