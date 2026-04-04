;;;; src/emit/x86-64-codegen.lisp - x86-64 Machine Code Generation
;;;
;;; Generates native x86-64 machine code bytes from VM instructions.
;;; Uses REX prefixes, ModR/M encoding, and proper instruction encoding.

(in-package :cl-cc)

;;; Low-level encoding primitives are in x86-64-encoding.lisp

;;; VM to Machine Code Translation

(defvar *current-regalloc* nil
  "When non-nil, the current regalloc-result used during code generation.")

(defparameter *current-spill-base-reg* +rbp+
  "Base register used for spill load/store emission in the current function.")

(defvar *current-float-vregs* nil
  "When non-nil, hash table of virtual registers currently known to hold unboxed floats.")

(defun x86-64-red-zone-spill-p (leaf-p spill-count)
  "Return true when a leaf function can keep spill slots in the SysV red zone."
  (and leaf-p
       (plusp spill-count)
       (<= spill-count 16)))

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

(defparameter *vm-fp-reg-map*
  `((:R0 . ,+xmm0+)
    (:R1 . ,+xmm1+)
    (:R2 . ,+xmm2+)
    (:R3 . ,+xmm3+)
    (:R4 . ,+xmm4+)
    (:R5 . ,+xmm5+)
    (:R6 . ,+xmm6+)
    (:R7 . ,+xmm7+))
  "Naive mapping from VM keyword registers to XMM register codes.")

(declaim (special *phys-reg-to-x86-code* *phys-fp-reg-to-x86-code*))

(defun vm-reg-to-x86 (vm-reg)
  "Map VM register to x86-64 register code.
   When *current-regalloc* is set, uses register allocation results.
   Otherwise falls back to naive mapping."
  (let ((phys-entry (assoc vm-reg *phys-reg-to-x86-code*)))
    (if phys-entry
        (cdr phys-entry)
        (if *current-regalloc*
            (vm-reg-to-x86-with-alloc *current-regalloc* vm-reg)
            (let ((entry (assoc vm-reg *vm-reg-map*)))
              (unless entry
                (error "VM register ~A has no x86-64 mapping (only R0-R7 supported)" vm-reg))
              (cdr entry))))))

(defparameter *phys-fp-reg-to-x86-code*
  '((:xmm0 . 0) (:xmm1 . 1) (:xmm2 . 2) (:xmm3 . 3)
    (:xmm4 . 4) (:xmm5 . 5) (:xmm6 . 6) (:xmm7 . 7)
    (:xmm8 . 8) (:xmm9 . 9) (:xmm10 . 10) (:xmm11 . 11)
    (:xmm12 . 12) (:xmm13 . 13) (:xmm14 . 14) (:xmm15 . 15))
  "Mapping from physical FP register keywords to XMM register codes.")

(defun vm-reg-to-xmm (vm-reg)
  "Map VM register to XMM register code for native float operations.
Falls back to the naive R0..R7 -> XMM0..XMM7 mapping when regalloc has not
assigned dedicated FP registers yet."
  (let ((phys-entry (assoc vm-reg *phys-fp-reg-to-x86-code*)))
    (if phys-entry
        (cdr phys-entry)
        (if *current-regalloc*
            (let* ((phys (gethash vm-reg (regalloc-assignment *current-regalloc*)))
                   (entry (and phys (assoc phys *phys-fp-reg-to-x86-code*))))
              (if entry
                  (cdr entry)
                  (let ((fallback (assoc vm-reg *vm-fp-reg-map*)))
                    (unless fallback
                      (error "VM register ~A has no XMM mapping" vm-reg))
                    (cdr fallback))))
            (let ((entry (assoc vm-reg *vm-fp-reg-map*)))
              (unless entry
                (error "VM register ~A has no XMM mapping" vm-reg))
              (cdr entry))))))

(defun x86-64-float-vreg-p (vreg)
  (and *current-float-vregs* (gethash vreg *current-float-vregs*)))

(defun x86-64-compute-float-vregs (instructions)
  "Conservatively mark VM virtual registers that hold unboxed floats." 
  (let ((float-vregs (make-hash-table :test #'eq)))
    (labels ((mark (reg)
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
           (mark (vm-rhs inst)))))
      (loop with changed = t
            while changed
            do (setf changed nil)
               (dolist (inst instructions)
                 (when (typep inst 'vm-move)
                   (let ((dst (vm-dst inst))
                         (src (vm-src inst)))
                     (cond
                       ((and (gethash src float-vregs)
                             (not (gethash dst float-vregs)))
                        (setf (gethash dst float-vregs) t)
                        (setf changed t))
                       ((and (gethash dst float-vregs)
                             (not (gethash src float-vregs)))
                        (setf (gethash src float-vregs) t)
                        (setf changed t))))))))
    float-vregs))

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

(defun vm-const-to-integer (value)
  "Coerce a VM constant value to an integer for native code emission.
   In integer-only binary mode: nil->0, t->1, integers pass through, other->0."
  (cond ((null value) 0)
        ((eq value t) 1)
        ((integerp value) value)
        (t 0)))

(defun x86-64-double-float-bits (value)
  "Return the IEEE754 bit pattern for VALUE as an unsigned 64-bit integer."
  (logand #xFFFFFFFFFFFFFFFF
          #+sbcl (sb-kernel:double-float-bits (float value 1.0d0))
          #-sbcl (error "x86-64-double-float-bits currently requires SBCL")))

(defmacro define-binary-alu-emitter (fn-name asm-op description)
  "Define an emitter for a binary VM instruction: MOV dst←lhs, then ASM-OP dst←rhs."
  `(defun ,fn-name (inst stream)
     ,description
     (let ((dst (vm-reg-to-x86 (vm-dst inst)))
           (lhs (vm-reg-to-x86 (vm-lhs inst)))
           (rhs (vm-reg-to-x86 (vm-rhs inst))))
       (emit-mov-rr64 dst lhs stream)
       (,asm-op dst rhs stream))))

(defmacro define-cmov-emitter (fn-name cmov-op description)
  "Define an emitter for a CMOVcc-based min/max: MOV dst←lhs, CMP dst rhs, CMOVcc dst←rhs."
  `(defun ,fn-name (inst stream)
     ,description
     (let ((dst (vm-reg-to-x86 (vm-dst inst)))
           (lhs (vm-reg-to-x86 (vm-lhs inst)))
           (rhs (vm-reg-to-x86 (vm-rhs inst))))
       (emit-mov-rr64 dst lhs stream)
       (emit-cmp-rr64 dst rhs stream)
       (,cmov-op dst rhs stream))))

(defmacro define-cmp-emitter (fn-name setcc-opcode description)
  "Define a comparison emitter: CMP lhs,rhs → SETcc dst → MOVZX dst,dst."
  `(defun ,fn-name (inst stream)
     ,description
     (let ((dst (vm-reg-to-x86 (vm-dst inst)))
           (lhs (vm-reg-to-x86 (vm-lhs inst)))
           (rhs (vm-reg-to-x86 (vm-rhs inst))))
       (emit-cmp-rr64 lhs rhs stream)
       (emit-setcc ,setcc-opcode dst stream)
       (emit-movzx-r64-r8 dst dst stream))))

(defmacro define-unary-mov-emitter (fn-name asm-op description)
  "Define a unary emitter: MOV dst←src, then ASM-OP dst."
  `(defun ,fn-name (inst stream)
     ,description
     (let ((dst (vm-reg-to-x86 (vm-dst inst)))
           (src (vm-reg-to-x86 (vm-src inst))))
       (emit-mov-rr64 dst src stream)
       (,asm-op dst stream))))

(defun emit-vm-const (inst stream)
  "Emit code for VM CONST instruction."
  (let ((value (vm-value inst)))
    (if (floatp value)
        (let ((dst (vm-reg-to-xmm (vm-dst inst))))
          (emit-mov-ri64 +r11+ (x86-64-double-float-bits value) stream)
          (emit-movq-xmm-r64 dst +r11+ stream))
        (let ((dst (vm-reg-to-x86 (vm-dst inst)))
              (int-value (vm-const-to-integer value)))
          (emit-mov-ri64 dst int-value stream)))))

(defun emit-vm-move (inst stream)
  "Emit code for VM MOVE instruction."
  (if (or (x86-64-float-vreg-p (vm-dst inst))
          (x86-64-float-vreg-p (vm-src inst)))
      (let ((dst (vm-reg-to-xmm (vm-dst inst)))
            (src (vm-reg-to-xmm (vm-src inst))))
        (unless (= dst src)
          (emit-movsd-xx dst src stream)))
      (let ((dst (vm-reg-to-x86 (vm-dst inst)))
            (src (vm-reg-to-x86 (vm-src inst))))
        (unless (= dst src)
          (emit-mov-rr64 dst src stream)))))

(defun emit-vm-float-add (inst stream)
  (let ((dst (vm-reg-to-xmm (vm-dst inst)))
        (lhs (vm-reg-to-xmm (vm-lhs inst)))
        (rhs (vm-reg-to-xmm (vm-rhs inst))))
    (emit-movsd-xx dst lhs stream)
    (emit-addsd-xx dst rhs stream)))

(defun emit-vm-float-sub (inst stream)
  (let ((dst (vm-reg-to-xmm (vm-dst inst)))
        (lhs (vm-reg-to-xmm (vm-lhs inst)))
        (rhs (vm-reg-to-xmm (vm-rhs inst))))
    (emit-movsd-xx dst lhs stream)
    (emit-subsd-xx dst rhs stream)))

(defun emit-vm-float-mul (inst stream)
  (let ((dst (vm-reg-to-xmm (vm-dst inst)))
        (lhs (vm-reg-to-xmm (vm-lhs inst)))
        (rhs (vm-reg-to-xmm (vm-rhs inst))))
    (emit-movsd-xx dst lhs stream)
    (emit-mulsd-xx dst rhs stream)))

(defun emit-vm-float-div (inst stream)
  (let ((dst (vm-reg-to-xmm (vm-dst inst)))
        (lhs (vm-reg-to-xmm (vm-lhs inst)))
        (rhs (vm-reg-to-xmm (vm-rhs inst))))
    (emit-movsd-xx dst lhs stream)
    (emit-divsd-xx dst rhs stream)))

(define-binary-alu-emitter emit-vm-add    emit-add-rr64  "vm-add: dst = lhs + rhs.")
(define-binary-alu-emitter emit-vm-sub    emit-sub-rr64  "vm-sub: dst = lhs - rhs.")
(define-binary-alu-emitter emit-vm-mul    emit-imul-rr64 "vm-mul: dst = lhs * rhs.")

(defun emit-vm-truncate (inst stream)
  "vm-truncate: dst = truncate(lhs / rhs)  -- quotient, truncate-toward-zero."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-idiv-sequence lhs rhs nil stream)                 ; [0..17]
    (emit-mov-rr64 dst +r11+ stream)))                      ; [18 +3] MOV dst, R11

(defun emit-vm-rem (inst stream)
  "vm-rem: dst = rem(lhs, rhs)  -- remainder, truncate semantics (same sign as lhs)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-idiv-sequence lhs rhs t stream)                   ; [0..17]
    (emit-mov-rr64 dst +r11+ stream)))                      ; [18 +3] MOV dst, R11

;;; Floor Division: vm-div, vm-mod
;;;
;;; x86-64 IDIV gives truncation-towards-zero.  CL's floor/mod differ when
;;; lhs and rhs have opposite signs and the remainder is non-zero:
;;;   floor quotient  = truncate quotient - 1   (iff rem!=0 && sign(rem)!=sign(div))
;;;   floor remainder = truncate remainder + div (iff rem!=0 && sign(rem)!=sign(div))
;;;
;;; vm-div -- floor quotient (34 bytes):
;;;   [0  +3] MOV  R11, rhs        -- R11 = divisor (IDIV preserves R11)
;;;   [3  +1] PUSH RAX
;;;   [4  +1] PUSH RDX
;;;   [5  +3] MOV  RAX, lhs
;;;   [8  +2] CQO
;;;   [10 +3] IDIV R11             -- RAX=q(trunc), RDX=rem, R11=divisor (unchanged)
;;;   [13 +3] TEST RDX, RDX        -- rem == 0?
;;;   [16 +2] JE   +8              -- if 0, skip to [26] (no correction)
;;;   [18 +3] XOR  R11, RDX        -- R11[63]=1 iff divisor and rem have different signs
;;;   [21 +2] JNS  +3              -- if same signs, skip to [26]
;;;   [23 +3] DEC  RAX             -- floor correction: quotient--
;;;   [26 +3] MOV  R11, RAX        -- R11 = floor quotient
;;;   [29 +1] POP  RDX
;;;   [30 +1] POP  RAX
;;;   [31 +3] MOV  dst, R11
;;;   Total: 34 bytes
;;;
;;; vm-mod -- floor remainder (37 bytes):
;;;   [0  +3] MOV  R11, rhs        -- R11 = divisor
;;;   [3  +1] PUSH RAX
;;;   [4  +1] PUSH RDX
;;;   [5  +3] MOV  RAX, lhs
;;;   [8  +2] CQO
;;;   [10 +3] IDIV R11             -- RAX=q(trunc, unused), RDX=rem, R11=divisor
;;;   [13 +3] TEST RDX, RDX        -- rem == 0?
;;;   [16 +2] JE   +11             -- if 0, skip to [29] (result = 0)
;;;   [18 +3] MOV  RAX, R11        -- RAX = divisor (reuse quotient slot for sign check)
;;;   [21 +3] XOR  RAX, RDX        -- RAX[63]=1 iff divisor and rem have different signs
;;;   [24 +2] JNS  +3              -- if same signs, skip to [29]
;;;   [26 +3] ADD  RDX, R11        -- floor correction: rem += divisor
;;;   [29 +3] MOV  R11, RDX        -- R11 = floor remainder
;;;   [32 +1] POP  RDX
;;;   [33 +1] POP  RAX
;;;   [34 +3] MOV  dst, R11
;;;   Total: 37 bytes

(defun emit-vm-div (inst stream)
  "vm-div: dst = floor(lhs / rhs)  -- floor division (CL semantics)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-mov-rr64 +r11+ rhs stream)                         ; [0  +3]
    (emit-push-r64 +rax+ stream)                             ; [3  +1]
    (emit-push-r64 +rdx+ stream)                             ; [4  +1]
    (emit-mov-rr64 +rax+ lhs stream)                         ; [5  +3]
    (emit-cqo stream)                                        ; [8  +2]
    (emit-idiv-r11 stream)                                   ; [10 +3]
    (emit-test-rr64 +rdx+ +rdx+ stream)                      ; [13 +3]
    (emit-byte #x74 stream) (emit-byte 8 stream)             ; [16 +2] JE +8 -> [26]
    (emit-xor-rr64 +r11+ +rdx+ stream)                       ; [18 +3] R11 ^= RDX
    (emit-byte #x79 stream) (emit-byte 3 stream)             ; [21 +2] JNS +3 -> [26]
    (emit-dec-r64 +rax+ stream)                              ; [23 +3] floor correction
    (emit-mov-rr64 +r11+ +rax+ stream)                       ; [26 +3]
    (emit-pop-r64 +rdx+ stream)                              ; [29 +1]
    (emit-pop-r64 +rax+ stream)                              ; [30 +1]
    (emit-mov-rr64 dst +r11+ stream)))                       ; [31 +3]

(defun emit-vm-mod (inst stream)
  "vm-mod: dst = mod(lhs, rhs)  -- floor modulo (CL semantics)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-mov-rr64 +r11+ rhs stream)                         ; [0  +3]
    (emit-push-r64 +rax+ stream)                             ; [3  +1]
    (emit-push-r64 +rdx+ stream)                             ; [4  +1]
    (emit-mov-rr64 +rax+ lhs stream)                         ; [5  +3]
    (emit-cqo stream)                                        ; [8  +2]
    (emit-idiv-r11 stream)                                   ; [10 +3]
    (emit-test-rr64 +rdx+ +rdx+ stream)                      ; [13 +3]
    (emit-byte #x74 stream) (emit-byte 11 stream)            ; [16 +2] JE +11 -> [29]
    (emit-mov-rr64 +rax+ +r11+ stream)                       ; [18 +3] RAX = divisor
    (emit-xor-rr64 +rax+ +rdx+ stream)                       ; [21 +3] RAX ^= RDX
    (emit-byte #x79 stream) (emit-byte 3 stream)             ; [24 +2] JNS +3 -> [29]
    (emit-add-rr64 +rdx+ +r11+ stream)                       ; [26 +3] floor correction
    (emit-mov-rr64 +r11+ +rdx+ stream)                       ; [29 +3]
    (emit-pop-r64 +rdx+ stream)                              ; [32 +1]
    (emit-pop-r64 +rax+ stream)                              ; [33 +1]
    (emit-mov-rr64 dst +r11+ stream)))                       ; [34 +3]

;;; Comparison instruction emitters (CMP lhs, rhs -> SETcc dst8 -> MOVZX dst64)
;;; SETcc opcode bytes: SETL=#x9C, SETG=#x9F, SETLE=#x9E, SETGE=#x9D, SETE=#x94

(define-cmp-emitter emit-vm-lt    #x9C "vm-lt: dst = (lhs < rhs) ? 1 : 0  -- signed.")
(define-cmp-emitter emit-vm-gt    #x9F "vm-gt: dst = (lhs > rhs) ? 1 : 0  -- signed.")
(define-cmp-emitter emit-vm-le    #x9E "vm-le: dst = (lhs <= rhs) ? 1 : 0  -- signed.")
(define-cmp-emitter emit-vm-ge    #x9D "vm-ge: dst = (lhs >= rhs) ? 1 : 0  -- signed.")
(define-cmp-emitter emit-vm-num-eq #x94 "vm-num-eq: dst = (lhs == rhs) ? 1 : 0  -- integer equality.")
(define-cmp-emitter emit-vm-eq    #x94 "vm-eq: dst = (lhs == rhs) ? 1 : 0  -- general equality (same x86 encoding).")

;;; Unary arithmetic/logical instruction emitters

(define-unary-mov-emitter emit-vm-neg    emit-neg-r64 "vm-neg: dst = -src  (two's complement negation).")

(defun emit-vm-not (inst stream)
  "vm-not: dst = (src == 0) ? 1 : 0  (logical NOT -- zero -> 1, nonzero -> 0)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (src (vm-reg-to-x86 (vm-src inst))))
    ;; TEST src, src sets ZF iff src == 0; SETE captures that
    (emit-test-rr64 src src stream)
    (emit-setcc #x94 dst stream)           ; SETE
    (emit-movzx-r64-r8 dst dst stream)))

(define-unary-mov-emitter emit-vm-lognot emit-not-r64 "vm-lognot: dst = ~src  (bitwise complement).")

(defun emit-vm-logcount (inst stream)
  "vm-logcount: dst = popcount(src)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (src (vm-reg-to-x86 (vm-src inst))))
    (emit-popcnt-rr64 dst src stream)))

(defun emit-vm-integer-length (inst stream)
  "vm-integer-length: dst = integer-length(src).
   Zero case returns 0; otherwise 1 + bsr(src)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (src (vm-reg-to-x86 (vm-src inst))))
    (emit-xor-rr64 dst dst stream)     ; dst = 0 for zero case
    (emit-test-rr64 src src stream)    ; set ZF when src = 0
    (emit-je-short 8 stream)           ; skip BSR + ADD when zero
    (emit-bsr-rr64 dst src stream)
    (emit-add-ri8 dst 1 stream)))

(defun emit-vm-bswap (inst stream)
  "vm-bswap: dst = byte-swap(low32(src))  (network-order byte reversal)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (src (vm-reg-to-x86 (vm-src inst))))
    (emit-mov-rr64 dst src stream)
    (emit-bswap-r32 dst stream)))

;;; Increment / Decrement

(defun emit-vm-inc (inst stream)
  "vm-inc: dst = src + 1."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (src (vm-reg-to-x86 (vm-src inst))))
    (emit-mov-rr64 dst src stream)
    (emit-add-ri8 dst 1 stream)))

(defun emit-vm-dec (inst stream)
  "vm-dec: dst = src - 1."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (src (vm-reg-to-x86 (vm-src inst))))
    (emit-mov-rr64 dst src stream)
    (emit-sub-ri8 dst 1 stream)))

;;; Absolute Value
;;;
;;; Strategy (branch-free at the cost of 1 extra MOV):
;;;   MOV dst, src          ; copy value
;;;   CMP dst, 0            ; set flags
;;;   JGE +4                ; skip NEG if already >= 0  (short jump, 2 bytes)
;;;   NEG dst               ; negate if negative

(defun emit-vm-abs (inst stream)
  "vm-abs: dst = |src|  (two's complement absolute value)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (src (vm-reg-to-x86 (vm-src inst))))
    ;; Layout: MOV(3) + CMP-imm32(7) + JGE(2) + NEG(3) = 15 bytes
    ;; JGE skips only the NEG (3 bytes ahead of JGE's end)
    (emit-mov-rr64 dst src stream)               ; 3 bytes
    (emit-cmp-ri64 dst 0 stream)                 ; 7 bytes: REX+0x81+ModRM+imm32
    (emit-jge-short 3 stream)                    ; 2 bytes: 7D 03 -- skip NEG (3 bytes)
    (emit-neg-r64 dst stream)))                  ; 3 bytes

;;; Arithmetic Shift (vm-ash)
;;;
;;; Fixed 24-byte layout (all sub-instructions are exactly 3 or 1 bytes):
;;;
;;;   [0  +1] PUSH RCX                -- save CL/RCX
;;;   [1  +3] MOV  RCX, rhs           -- load shift count
;;;   [4  +3] MOV  dst,  lhs          -- copy value
;;;   [7  +3] TEST RCX, RCX           -- set flags for sign check
;;;   [10 +2] JGE  +8                 -- if count>=0 jump to SAL at [20]
;;;   [12 +3] NEG  RCX                -- negate for right-shift
;;;   [15 +3] SAR  dst,  CL           -- arithmetic right shift
;;;   [18 +2] JMP  +3                 -- skip SAL, jump to POP at [23]
;;;   [20 +3] SAL  dst,  CL           -- arithmetic left shift
;;;   [23 +1] POP  RCX                -- restore CL/RCX
;;;   Total: 24 bytes (independent of which physical registers are used)

(defun emit-vm-ash (inst stream)
  "vm-ash: dst = (ash lhs rhs)
   Positive rhs = left shift, negative rhs = right shift.
   RCX is saved/restored; CL carries the shift count."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-push-r64 +rcx+ stream)                  ; 1 byte
    (emit-mov-rr64 +rcx+ rhs stream)              ; 3 bytes
    (emit-mov-rr64 dst lhs stream)                ; 3 bytes
    (emit-test-rr64 +rcx+ +rcx+ stream)           ; 3 bytes
    (emit-byte #x7D stream) (emit-byte 8 stream)  ; 2 bytes: JGE +8 -> [20]=SAL
    (emit-neg-r64 +rcx+ stream)                   ; 3 bytes
    (emit-sar-r64-cl dst stream)                  ; 3 bytes
    (emit-byte #xEB stream) (emit-byte 3 stream)  ; 2 bytes: JMP +3 -> [23]=POP
    (emit-sal-r64-cl dst stream)                  ; 3 bytes
    (emit-pop-r64 +rcx+ stream)))                 ; 1 byte

(defun emit-vm-rotate (inst stream)
  "vm-rotate: dst = rotr(lhs, rhs)
   RCX is saved/restored; CL carries the rotate count."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-push-r64 +rcx+ stream)
    (emit-mov-rr64 +rcx+ rhs stream)
    (emit-mov-rr64 dst lhs stream)
    (emit-ror-r64-cl dst stream)
    (emit-pop-r64 +rcx+ stream)))

;;; Min / Max via CMOVcc

(define-cmov-emitter emit-vm-min emit-cmovg-rr64
  "vm-min: dst = min(lhs, rhs)  -- signed, branchless via CMOVG.")
(define-cmov-emitter emit-vm-max emit-cmovl-rr64
  "vm-max: dst = max(lhs, rhs)  -- signed, branchless via CMOVL.")

(defun emit-vm-select (inst stream)
  "vm-select: dst = cond ? then : else  (branchless via TEST + CMOVNE)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (cond (vm-reg-to-x86 (vm-select-cond-reg inst)))
        (then (vm-reg-to-x86 (vm-select-then-reg inst)))
        (else (vm-reg-to-x86 (vm-select-else-reg inst))))
    (emit-mov-rr64 dst else stream)
    (emit-test-rr64 cond cond stream)
    (emit-cmovne-rr64 dst then stream)))

;;; Type Predicate Instruction Emitters
;;;
;;; In the integer-only binary codegen domain:
;;;   null-p    -- zero check (nil = 0)
;;;   number-p, integer-p -- always 1 (all values are integers)
;;;   symbol-p, cons-p, function-p -- always 0 (no heap in binary mode)

(defun emit-vm-null-p (inst stream)
  "vm-null-p: dst = (src == 0) ? 1 : 0  (nil test -- same encoding as vm-not)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (src (vm-reg-to-x86 (vm-src inst))))
    (emit-test-rr64 src src stream)
    (emit-setcc #x94 dst stream)               ; SETE
    (emit-movzx-r64-r8 dst dst stream)))

(defun emit-vm-true-pred (inst stream)
  "Emit dst = 1 for predicates that are always true in integer-only mode."
  (emit-mov-ri64 (vm-reg-to-x86 (vm-dst inst)) 1 stream))

(defun emit-vm-false-pred (inst stream)
  "Emit dst = 0 for predicates that are always false in integer-only mode."
  (emit-mov-ri64 (vm-reg-to-x86 (vm-dst inst)) 0 stream))

;;; Boolean Logical Instruction Emitters (vm-and, vm-or)
;;;
;;; vm-and: dst = (lhs != 0 AND rhs != 0) ? 1 : 0    (17 bytes)
;;;   [0  +3] XOR dst, dst     -- dst = 0
;;;   [3  +3] TEST lhs, lhs
;;;   [6  +2] JE   +9          -- if lhs==0: jump to done [17]
;;;   [8  +3] TEST rhs, rhs
;;;   [11 +2] JE   +4          -- if rhs==0: jump to done [17]
;;;   [13 +4] ADD  dst, 1      -- both nonzero: dst = 1
;;;   [17] done
;;;
;;; vm-or: dst = (lhs != 0 OR rhs != 0) ? 1 : 0      (17 bytes)
;;;   [0  +3] XOR dst, dst     -- dst = 0
;;;   [3  +3] TEST lhs, lhs
;;;   [6  +2] JNE  +5          -- if lhs!=0: jump to add [13]
;;;   [8  +3] TEST rhs, rhs
;;;   [11 +2] JE   +4          -- if rhs==0: jump to done [17]
;;;   [13 +4] ADD  dst, 1      -- at least one nonzero: dst = 1
;;;   [17] done

(defun emit-vm-and (inst stream)
  "vm-and: dst = (lhs && rhs) ? 1 : 0  (logical AND, integer-only mode)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-xor-rr64 dst dst stream)                           ; [0  +3] dst = 0
    (emit-test-rr64 lhs lhs stream)                          ; [3  +3]
    (emit-byte #x74 stream) (emit-byte 9 stream)             ; [6  +2] JE +9 -> [17]
    (emit-test-rr64 rhs rhs stream)                          ; [8  +3]
    (emit-byte #x74 stream) (emit-byte 4 stream)             ; [11 +2] JE +4 -> [17]
    (emit-add-ri8 dst 1 stream)))                            ; [13 +4] dst = 1

(defun emit-vm-or (inst stream)
  "vm-or: dst = (lhs || rhs) ? 1 : 0  (logical OR, integer-only mode)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-xor-rr64 dst dst stream)                           ; [0  +3] dst = 0
    (emit-test-rr64 lhs lhs stream)                          ; [3  +3]
    (emit-byte #x75 stream) (emit-byte 5 stream)             ; [6  +2] JNE +5 -> [13]
    (emit-test-rr64 rhs rhs stream)                          ; [8  +3]
    (emit-byte #x74 stream) (emit-byte 4 stream)             ; [11 +2] JE +4 -> [17]
    (emit-add-ri8 dst 1 stream)))                            ; [13 +4] dst = 1

;;; Binary logical instruction emitters

(define-binary-alu-emitter emit-vm-logand emit-and-rr64  "vm-logand: dst = lhs & rhs  (bitwise AND).")
(define-binary-alu-emitter emit-vm-logior emit-or-rr64   "vm-logior: dst = lhs | rhs  (bitwise OR).")
(define-binary-alu-emitter emit-vm-logxor emit-xor-rr64  "vm-logxor: dst = lhs ^ rhs  (bitwise XOR).")

(defun emit-vm-logeqv (inst stream)
  "vm-logeqv: dst = ~(lhs ^ rhs)  (bitwise XNOR / logical equivalence).
   Layout (9 bytes): MOV(3) + XOR(3) + NOT(3)"
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-mov-rr64 dst lhs stream)       ; [0 +3]
    (emit-xor-rr64 dst rhs stream)       ; [3 +3]
    (emit-not-r64 dst stream)))          ; [6 +3]

;;; vm-logtest -- (logand lhs rhs) /= 0 -> 0 or 1           (14 bytes)
;;;   [0  +3] MOV  dst, lhs
;;;   [3  +3] AND  dst, rhs   -- sets ZF=1 iff (lhs & rhs) == 0
;;;   [6 +4*] SETNE dst8      -- dst8 = (ZF==0) ? 1 : 0  (*4 bytes for reg>=4)
;;;   [10 +4] MOVZX dst, dst8
;;;   Total: conservative 14 bytes

(defun emit-vm-logtest (inst stream)
  "vm-logtest: dst = ((logand lhs rhs) /= 0) ? 1 : 0  (boolean AND test)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-mov-rr64 dst lhs stream)            ; [0  +3]
    (emit-and-rr64 dst rhs stream)            ; [3  +3] sets ZF
    (emit-setcc #x95 dst stream)              ; [6 +3/4] SETNE
    (emit-movzx-r64-r8 dst dst stream)))      ; [9/10 +4]

;;; vm-logbitp -- test bit LHS of integer RHS -> 0 or 1      (15 bytes)
;;;   [0  +1] PUSH RCX
;;;   [1  +3] MOV  RCX, lhs   -- bit position
;;;   [4  +3] MOV  dst, rhs   -- integer to test
;;;   [7  +3] SAR  dst, CL    -- shift right by bit position
;;;   [10 +4] AND  dst, 1     -- isolate low bit
;;;   [14 +1] POP  RCX
;;;   Total: 15 bytes

(defun emit-vm-logbitp (inst stream)
  "vm-logbitp: dst = (logbitp lhs rhs) -- test bit LHS of integer RHS."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-push-r64 +rcx+ stream)              ; [0  +1]
    (emit-mov-rr64 +rcx+ lhs stream)          ; [1  +3] bit position -> RCX
    (emit-mov-rr64 dst rhs stream)            ; [4  +3] integer -> dst
    (emit-sar-r64-cl dst stream)              ; [7  +3] shift right by CL
    (emit-and-ri8 dst 1 stream)               ; [10 +4] isolate bit
    (emit-pop-r64 +rcx+ stream)))             ; [14 +1]

;;; Two-Pass Code Generation (Labels + Jumps)

(defparameter *x86-64-instruction-sizes*
  (let ((ht (make-hash-table :test #'eq)))
    ;; Constants and copies
    (setf (gethash 'vm-const ht) 10)       ; REX + opcode + 8-byte immediate
    (setf (gethash 'vm-move ht) 3)         ; REX + opcode + ModR/M
    ;; Arithmetic: mov + op
    (setf (gethash 'vm-add ht) 6)          ; mov + add (3+3)
    (setf (gethash 'vm-integer-add ht) 6)  ; mov + add
    (setf (gethash 'vm-sub ht) 6)          ; mov + sub (3+3)
    (setf (gethash 'vm-integer-sub ht) 6)  ; mov + sub
    (setf (gethash 'vm-mul ht) 7)          ; mov + imul (3+4, 0F AF)
    (setf (gethash 'vm-integer-mul ht) 7)  ; mov + imul
    ;; Control flow
    (setf (gethash 'vm-halt ht) 3)         ; mov result to RAX
    (setf (gethash 'vm-label ht) 0)        ; Labels emit no code
    (setf (gethash 'vm-jump ht) 5)         ; JMP rel32
    (setf (gethash 'vm-jump-zero ht) 9)    ; TEST + JE rel32 (3 + 6)
    (setf (gethash 'vm-ret ht) 1)          ; RET
    ;; No-ops in native codegen
    (setf (gethash 'vm-print ht) 0)
    (setf (gethash 'vm-closure ht) 0)
    (setf (gethash 'vm-call ht) 6)
    (setf (gethash 'vm-tail-call ht) 3)
    ;; Register spilling
    (setf (gethash 'vm-spill-store ht) 4)  ; MOV [rbp-disp8], reg
    (setf (gethash 'vm-spill-load ht) 4)   ; MOV reg, [rbp-disp8]
    ;; Comparison: CMP(3) + SETcc(3-4) + MOVZX(4) = 12 max
    (dolist (tp '(vm-lt vm-gt vm-le vm-ge vm-num-eq vm-eq))
      (setf (gethash tp ht) 12))
    ;; Logical NOT: TEST+SETE+MOVZX = 11→12; bitwise NOT: MOV+NOT = 7
    (setf (gethash 'vm-not ht) 12)
    (setf (gethash 'vm-lognot ht) 7)
    (setf (gethash 'vm-logcount ht) 5)
    (setf (gethash 'vm-integer-length ht) 16)
    (setf (gethash 'vm-bswap ht) 6)
    ;; Unary arithmetic: MOV(3) + op(3-4) = 7
    (setf (gethash 'vm-neg ht) 7)
    (setf (gethash 'vm-inc ht) 7)
    (setf (gethash 'vm-dec ht) 7)
    ;; Abs: MOV + CMP-imm32 + JGE-short + NEG = 15
    (setf (gethash 'vm-abs ht) 15)
    ;; Min/max: MOV + CMP + CMOV = 10
    (setf (gethash 'vm-min ht) 10)
    (setf (gethash 'vm-max ht) 10)
    (setf (gethash 'vm-select ht) 10)
    ;; Ash: fixed 24-byte sequence
    (setf (gethash 'vm-ash ht) 24)
    ;; Rotate: MOV + MOV + ROR + save/restore RCX = 11 bytes
    (setf (gethash 'vm-rotate ht) 11)
    ;; IDIV-based: truncate/rem = 21, floor-div = 34, floor-mod = 37
    (setf (gethash 'vm-truncate ht) 21)
    (setf (gethash 'vm-rem ht) 21)
    (setf (gethash 'vm-div ht) 34)
    (setf (gethash 'vm-mod ht) 37)
    ;; Boolean logical: XOR+TEST+JE+TEST+JE+ADD = 17
    (setf (gethash 'vm-and ht) 17)
    (setf (gethash 'vm-or ht) 17)
    ;; Binary logical: MOV + op = 6
    (dolist (tp '(vm-logand vm-logior vm-logxor))
      (setf (gethash tp ht) 6))
    ;; Scalar float ops: MOVSD + op = 8
    (dolist (tp '(vm-float-add vm-float-sub vm-float-mul vm-float-div))
      (setf (gethash tp ht) 8))
    ;; XNOR = 9, logtest = 14, logbitp = 15
    (setf (gethash 'vm-logeqv ht) 9)
    (setf (gethash 'vm-logtest ht) 14)
    (setf (gethash 'vm-logbitp ht) 15)
    ;; Type predicates: null-p = 11; others = 10 (MOV imm64)
    (setf (gethash 'vm-null-p ht) 11)
    (dolist (tp '(vm-number-p vm-integer-p vm-cons-p vm-symbol-p vm-function-p))
      (setf (gethash tp ht) 10))
    ht)
  "Maps VM instruction struct-type symbols to their x86-64 encoded byte sizes.
   Used by the first pass of two-pass code generation to build label offset tables.")

(defun instruction-size (inst)
  "Estimate the size in bytes of the x86-64 encoding for a VM instruction.
   Used in first pass to build label offset table."
  (cond
    ((typep inst 'vm-const)
     (if (floatp (vm-value inst)) 15 10))
    ((typep inst 'vm-move)
     (if (or (x86-64-float-vreg-p (vm-dst inst))
             (x86-64-float-vreg-p (vm-src inst)))
         (let ((dst (vm-reg-to-xmm (vm-dst inst)))
               (src (vm-reg-to-xmm (vm-src inst))))
           (if (= dst src) 0 4))
         (let ((dst (vm-reg-to-x86 (vm-dst inst)))
               (src (vm-reg-to-x86 (vm-src inst))))
           (if (= dst src) 0 3))))
    ((typep inst 'vm-halt)
     (if (x86-64-float-vreg-p (vm-reg inst))
         (let ((result-reg (vm-reg-to-xmm (vm-reg inst))))
           (if (= result-reg +xmm0+) 0 4))
         (let ((result-reg (vm-reg-to-x86 (vm-reg inst))))
           (if (= result-reg +rax+) 0 3))))
    (t
     (or (gethash (type-of inst) *x86-64-instruction-sizes*) 0))))

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
  (if (x86-64-float-vreg-p (vm-reg inst))
      (let ((result-reg (vm-reg-to-xmm (vm-reg inst))))
        (unless (= result-reg +xmm0+)
          (emit-movsd-xx +xmm0+ result-reg stream)))
      (let ((result-reg (vm-reg-to-x86 (vm-reg inst))))
        (unless (= result-reg +rax+)
          (emit-mov-rr64 +rax+ result-reg stream)))))

(defun emit-vm-call-like-inst (inst stream)
  "Emit code for VM CALL / VM TAIL-CALL instructions.

   The function designator is already in a register. We perform an indirect
   CALL through that register and then copy the return value from RAX into the
   destination register."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (func (vm-reg-to-x86 (vm-func-reg inst))))
    (emit-call-r64 func stream)
    (emit-mov-rr64 dst +rax+ stream)))

(defun emit-vm-tail-call-inst (inst stream)
  "Emit code for VM TAIL-CALL instruction.

   Tail calls transfer control directly to the callee so the current stack
   frame is not extended. The callee returns to the original caller, so no
   destination move is emitted here."
  (let ((dst (vm-dst inst)))
    (declare (ignore dst))
    (emit-jmp-r64 (vm-reg-to-x86 (vm-func-reg inst)) stream)))

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

(defun x86-64-used-callee-saved-regs (ra cc)
  "Return the callee-saved physical registers used by RA, in ABI order.
   RBP is always preserved separately as the frame pointer."
  (let ((callee-saved (cc-callee-saved cc))
        (used nil))
    (loop for phys being the hash-values of (regalloc-assignment ra)
          when (member phys callee-saved)
          do (pushnew phys used :test #'eq))
    (loop for reg in callee-saved
          when (member reg used :test #'eq)
          collect reg)))

(defun emit-vm-spill-store-inst (inst stream)
  "Emit code for VM SPILL-STORE instruction using the active spill base register."
  (let* ((src-reg (vm-spill-src inst))
         (src-code (let ((entry (assoc src-reg *phys-reg-to-x86-code*)))
                     (if entry (cdr entry)
                          (error "Unknown physical register for spill store: ~A" src-reg))))
          (offset (- (* (vm-spill-slot inst) 8))))
    (emit-mov-mr64 *current-spill-base-reg* offset src-code stream)))

(defun emit-vm-spill-load-inst (inst stream)
  "Emit code for VM SPILL-LOAD instruction using the active spill base register."
  (let* ((dst-reg (vm-spill-dst inst))
         (dst-code (let ((entry (assoc dst-reg *phys-reg-to-x86-code*)))
                     (if entry (cdr entry)
                          (error "Unknown physical register for spill load: ~A" dst-reg))))
          (offset (- (* (vm-spill-slot inst) 8))))
    (emit-mov-rm64 dst-code *current-spill-base-reg* offset stream)))

(defparameter *x86-64-emitter-entries*
  '(;; Core instructions
    (vm-const        . emit-vm-const)
    (vm-move         . emit-vm-move)
    (vm-add          . emit-vm-add)
    (vm-integer-add  . emit-vm-add)
    (vm-float-add    . emit-vm-float-add)
    (vm-sub          . emit-vm-sub)
    (vm-integer-sub  . emit-vm-sub)
    (vm-float-sub    . emit-vm-float-sub)
    (vm-mul          . emit-vm-mul)
    (vm-integer-mul  . emit-vm-mul)
    (vm-float-mul    . emit-vm-float-mul)
    (vm-float-div    . emit-vm-float-div)
    (vm-halt         . emit-vm-halt-inst)
     (vm-call         . emit-vm-call-like-inst)
     (vm-tail-call    . emit-vm-tail-call-inst)
    (vm-ret          . emit-vm-ret-inst)
    (vm-spill-store  . emit-vm-spill-store-inst)
    (vm-spill-load   . emit-vm-spill-load-inst)
    ;; Comparison
    (vm-lt           . emit-vm-lt)
    (vm-gt           . emit-vm-gt)
    (vm-le           . emit-vm-le)
    (vm-ge           . emit-vm-ge)
    (vm-num-eq       . emit-vm-num-eq)
    (vm-eq           . emit-vm-eq)
    ;; Unary arithmetic/logical
    (vm-neg          . emit-vm-neg)
     (vm-not          . emit-vm-not)
     (vm-lognot       . emit-vm-lognot)
     (vm-logcount     . emit-vm-logcount)
     (vm-integer-length . emit-vm-integer-length)
     (vm-bswap        . emit-vm-bswap)
    (vm-inc          . emit-vm-inc)
    (vm-dec          . emit-vm-dec)
    (vm-abs          . emit-vm-abs)
    ;; Min/max/ash
    (vm-min          . emit-vm-min)
    (vm-max          . emit-vm-max)
    (vm-select       . emit-vm-select)
    (vm-ash          . emit-vm-ash)
    (vm-rotate       . emit-vm-rotate)
    ;; Integer division (IDIV-based)
    (vm-truncate     . emit-vm-truncate)
    (vm-rem          . emit-vm-rem)
    (vm-div          . emit-vm-div)
    (vm-mod          . emit-vm-mod)
    ;; Boolean logical
    (vm-and          . emit-vm-and)
    (vm-or           . emit-vm-or)
    ;; Binary logical
    (vm-logand       . emit-vm-logand)
    (vm-logior       . emit-vm-logior)
    (vm-logxor       . emit-vm-logxor)
    (vm-logeqv       . emit-vm-logeqv)
    (vm-logtest      . emit-vm-logtest)
    (vm-logbitp      . emit-vm-logbitp)
    ;; Type predicates
    (vm-null-p       . emit-vm-null-p)
    (vm-number-p     . emit-vm-true-pred)
    (vm-integer-p    . emit-vm-true-pred)
    (vm-cons-p       . emit-vm-false-pred)
    (vm-symbol-p     . emit-vm-false-pred)
    (vm-function-p   . emit-vm-false-pred))
  "Alist mapping VM instruction type symbols to emitter function names.")

(defparameter *x86-64-emitter-table*
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (entry *x86-64-emitter-entries* ht)
      (setf (gethash (car entry) ht) (symbol-function (cdr entry)))))
  "Maps VM instruction type symbols to emitter functions (inst stream).")

(defun emit-vm-instruction-with-labels (inst stream current-pos label-offsets)
  "Emit machine code for a VM instruction, with label/jump support."
  (let ((tp (type-of inst)))
    (cond
      ;; No-op instructions
      ((or (eq tp 'vm-label) (eq tp 'vm-print)) nil)
      ;; Jump instructions need extra args (current-pos, label-offsets)
      ((eq tp 'vm-jump)
       (emit-vm-jump-inst inst stream current-pos label-offsets))
      ((eq tp 'vm-jump-zero)
       (emit-vm-jump-zero-inst inst stream current-pos label-offsets))
      ;; Table-driven dispatch for all (inst stream) emitters
      (t (let ((emitter (gethash tp *x86-64-emitter-table*)))
           (if emitter
               (funcall emitter inst stream)
               (warn "Skipping unsupported VM instruction: ~A" tp)))))))

(defun emit-vm-program (program stream)
  "Emit machine code for entire VM program.
   Uses two-pass approach: first pass builds label offset table,
   second pass emits code with resolved jump targets."
  (let* ((instructions (vm-program-instructions program))
         (cfg (cfg-build instructions))
         (leaf-p (vm-program-leaf-p program))
         (spill-count (regalloc-spill-count *current-regalloc*))
         (red-zone-spill-p (x86-64-red-zone-spill-p leaf-p spill-count))
         (callee-saved (x86-64-used-callee-saved-regs *current-regalloc*
                                                         *x86-64-calling-convention*))
         (save-regs (if (or (and leaf-p (zerop spill-count))
                            red-zone-spill-p)
                        callee-saved
                         (cons +rbp+ callee-saved)))
         (*current-spill-base-reg* (if red-zone-spill-p +rsp+ +rbp+))
         ;; Each push/pop is 1 byte in the current encoder.
         (prologue-size (length save-regs))
           (ordered-instructions (if (cfg-entry cfg)
                                     (progn
                                      (cfg-compute-dominators cfg)
                                      (cfg-compute-loop-depths cfg)
                                      (cfg-flatten-hot-cold cfg))
                                    instructions))
          ;; First pass: build label offset table
          (label-offsets (build-label-offsets ordered-instructions prologue-size)))

    ;; Prologue: save only the callee-saved registers actually used
    (dolist (reg save-regs)
      (emit-push-r64 reg stream))

    ;; Second pass: emit instructions with resolved jumps
    (let ((pos prologue-size))
      (dolist (inst ordered-instructions)
        (emit-vm-instruction-with-labels inst stream pos label-offsets)
        (incf pos (instruction-size inst))))

    ;; Epilogue: restore callee-saved registers in reverse order
    (dolist (reg (reverse save-regs))
      (emit-pop-r64 reg stream))

    ;; Return
    (emit-ret stream)))

;;; Public API

(defun compile-to-x86-64-bytes (program)
  "Compile VM program to x86-64 machine code bytes.

   Returns: (simple-array (unsigned-byte 8) (*))"
  ;; Run register allocation before emitting machine code
  (let* ((instructions (vm-program-instructions program))
         (float-vregs (x86-64-compute-float-vregs instructions))
         (ra (allocate-registers instructions *x86-64-calling-convention* float-vregs))
         (allocated-program (make-vm-program
                               :instructions (regalloc-instructions ra)
                               :result-register (vm-program-result-register program)
                               :leaf-p (vm-program-leaf-p program))))
    ;; Store the regalloc result for use during code generation
    (let ((*current-regalloc* ra)
          (*current-float-vregs* float-vregs))
      (with-output-to-vector (stream)
        (emit-vm-program allocated-program stream)))))
