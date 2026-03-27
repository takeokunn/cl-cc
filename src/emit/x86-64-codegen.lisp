;;;; src/emit/x86-64-codegen.lisp - x86-64 Machine Code Generation
;;;
;;; Generates native x86-64 machine code bytes from VM instructions.
;;; Uses REX prefixes, ModR/M encoding, and proper instruction encoding.

(in-package :cl-cc)

;;; Low-level encoding primitives are in x86-64-encoding.lisp

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

(defun vm-const-to-integer (value)
  "Coerce a VM constant value to an integer for native code emission.
   In integer-only binary mode: nil->0, t->1, integers pass through, other->0."
  (cond ((null value) 0)
        ((eq value t) 1)
        ((integerp value) value)
        (t 0)))

(defun emit-vm-const (inst stream)
  "Emit code for VM CONST instruction."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (value (vm-const-to-integer (vm-value inst))))
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

(defun emit-vm-lt (inst stream)
  "vm-lt: dst = (lhs < rhs) ? 1 : 0  -- signed."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-cmp-rr64 lhs rhs stream)
    (emit-setcc #x9C dst stream)
    (emit-movzx-r64-r8 dst dst stream)))

(defun emit-vm-gt (inst stream)
  "vm-gt: dst = (lhs > rhs) ? 1 : 0  -- signed."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-cmp-rr64 lhs rhs stream)
    (emit-setcc #x9F dst stream)
    (emit-movzx-r64-r8 dst dst stream)))

(defun emit-vm-le (inst stream)
  "vm-le: dst = (lhs <= rhs) ? 1 : 0  -- signed."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-cmp-rr64 lhs rhs stream)
    (emit-setcc #x9E dst stream)
    (emit-movzx-r64-r8 dst dst stream)))

(defun emit-vm-ge (inst stream)
  "vm-ge: dst = (lhs >= rhs) ? 1 : 0  -- signed."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-cmp-rr64 lhs rhs stream)
    (emit-setcc #x9D dst stream)
    (emit-movzx-r64-r8 dst dst stream)))

(defun emit-vm-num-eq (inst stream)
  "vm-num-eq: dst = (lhs == rhs) ? 1 : 0  -- integer equality."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-cmp-rr64 lhs rhs stream)
    (emit-setcc #x94 dst stream)
    (emit-movzx-r64-r8 dst dst stream)))

(defun emit-vm-eq (inst stream)
  "vm-eq: dst = (lhs == rhs) ? 1 : 0  -- general equality (same x86 encoding)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-cmp-rr64 lhs rhs stream)
    (emit-setcc #x94 dst stream)
    (emit-movzx-r64-r8 dst dst stream)))

;;; Unary arithmetic/logical instruction emitters

(defun emit-vm-neg (inst stream)
  "vm-neg: dst = -src  (two's complement negation)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (src (vm-reg-to-x86 (vm-src inst))))
    (emit-mov-rr64 dst src stream)
    (emit-neg-r64 dst stream)))

(defun emit-vm-not (inst stream)
  "vm-not: dst = (src == 0) ? 1 : 0  (logical NOT -- zero -> 1, nonzero -> 0)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (src (vm-reg-to-x86 (vm-src inst))))
    ;; TEST src, src sets ZF iff src == 0; SETE captures that
    (emit-test-rr64 src src stream)
    (emit-setcc #x94 dst stream)           ; SETE
    (emit-movzx-r64-r8 dst dst stream)))

(defun emit-vm-lognot (inst stream)
  "vm-lognot: dst = ~src  (bitwise complement)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (src (vm-reg-to-x86 (vm-src inst))))
    (emit-mov-rr64 dst src stream)
    (emit-not-r64 dst stream)))

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

;;; Min / Max via CMOVcc

(defun emit-vm-min (inst stream)
  "vm-min: dst = min(lhs, rhs)  -- signed, branchless via CMOVG."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    ;; dst = lhs; if dst > rhs then dst = rhs  -> dst = min(lhs, rhs)
    (emit-mov-rr64 dst lhs stream)
    (emit-cmp-rr64 dst rhs stream)
    (emit-cmovg-rr64 dst rhs stream)))

(defun emit-vm-max (inst stream)
  "vm-max: dst = max(lhs, rhs)  -- signed, branchless via CMOVL."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    ;; dst = lhs; if dst < rhs then dst = rhs  -> dst = max(lhs, rhs)
    (emit-mov-rr64 dst lhs stream)
    (emit-cmp-rr64 dst rhs stream)
    (emit-cmovl-rr64 dst rhs stream)))

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

(defun emit-vm-logand (inst stream)
  "vm-logand: dst = lhs & rhs  (bitwise AND)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-mov-rr64 dst lhs stream)
    (emit-and-rr64 dst rhs stream)))

(defun emit-vm-logior (inst stream)
  "vm-logior: dst = lhs | rhs  (bitwise OR)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-mov-rr64 dst lhs stream)
    (emit-or-rr64 dst rhs stream)))

(defun emit-vm-logxor (inst stream)
  "vm-logxor: dst = lhs ^ rhs  (bitwise XOR)."
  (let ((dst (vm-reg-to-x86 (vm-dst inst)))
        (lhs (vm-reg-to-x86 (vm-lhs inst)))
        (rhs (vm-reg-to-x86 (vm-rhs inst))))
    (emit-mov-rr64 dst lhs stream)
    (emit-xor-rr64 dst rhs stream)))

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
    (setf (gethash 'vm-sub ht) 6)          ; mov + sub (3+3)
    (setf (gethash 'vm-mul ht) 7)          ; mov + imul (3+4, 0F AF)
    ;; Control flow
    (setf (gethash 'vm-halt ht) 3)         ; mov result to RAX
    (setf (gethash 'vm-label ht) 0)        ; Labels emit no code
    (setf (gethash 'vm-jump ht) 5)         ; JMP rel32
    (setf (gethash 'vm-jump-zero ht) 9)    ; TEST + JE rel32 (3 + 6)
    (setf (gethash 'vm-ret ht) 1)          ; RET
    ;; No-ops in native codegen
    (setf (gethash 'vm-print ht) 0)
    (setf (gethash 'vm-closure ht) 0)
    (setf (gethash 'vm-call ht) 0)
    ;; Register spilling
    (setf (gethash 'vm-spill-store ht) 4)  ; MOV [rbp-disp8], reg
    (setf (gethash 'vm-spill-load ht) 4)   ; MOV reg, [rbp-disp8]
    ;; Comparison: CMP(3) + SETcc(3-4) + MOVZX(4) = 12 max
    (dolist (tp '(vm-lt vm-gt vm-le vm-ge vm-num-eq vm-eq))
      (setf (gethash tp ht) 12))
    ;; Logical NOT: TEST+SETE+MOVZX = 11→12; bitwise NOT: MOV+NOT = 7
    (setf (gethash 'vm-not ht) 12)
    (setf (gethash 'vm-lognot ht) 7)
    ;; Unary arithmetic: MOV(3) + op(3-4) = 7
    (setf (gethash 'vm-neg ht) 7)
    (setf (gethash 'vm-inc ht) 7)
    (setf (gethash 'vm-dec ht) 7)
    ;; Abs: MOV + CMP-imm32 + JGE-short + NEG = 15
    (setf (gethash 'vm-abs ht) 15)
    ;; Min/max: MOV + CMP + CMOV = 10
    (setf (gethash 'vm-min ht) 10)
    (setf (gethash 'vm-max ht) 10)
    ;; Ash: fixed 24-byte sequence
    (setf (gethash 'vm-ash ht) 24)
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
  (or (gethash (type-of inst) *x86-64-instruction-sizes*) 0))

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

(defparameter *x86-64-emitter-entries*
  '(;; Core instructions
    (vm-const        . emit-vm-const)
    (vm-move         . emit-vm-move)
    (vm-add          . emit-vm-add)
    (vm-sub          . emit-vm-sub)
    (vm-mul          . emit-vm-mul)
    (vm-halt         . emit-vm-halt-inst)
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
    (vm-inc          . emit-vm-inc)
    (vm-dec          . emit-vm-dec)
    (vm-abs          . emit-vm-abs)
    ;; Min/max/ash
    (vm-min          . emit-vm-min)
    (vm-max          . emit-vm-max)
    (vm-ash          . emit-vm-ash)
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
