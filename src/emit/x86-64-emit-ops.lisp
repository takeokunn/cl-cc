;;;; src/emit/x86-64-emit-ops.lisp — x86-64 Arithmetic and Comparison Emitters
;;;;
;;;; Emitter macro DSL and emit-vm-* functions for:
;;;;   Macro DSL: define-binary-alu-emitter, define-cmov-emitter, define-cmp-emitter,
;;;;              define-unary-mov-emitter, define-float-binary-emitter
;;;;   const, move, float arith, int arith (add/sub/mul), div/rem, floor-div/mod
;;;;   comparisons: lt/gt/le/ge/num-eq/eq
;;;;   unary: neg, not, lognot, logcount, integer-length, bswap
;;;;   inc/dec, abs, ash, rotate, min/max, select
;;;;
;;;; Type predicates, boolean logical, and bitwise emitters are in
;;;; x86-64-emit-ops-logical.lisp (loads after).
;;;;
;;;; Load order: after x86-64-regs.lisp, before x86-64-emit-ops-logical.lisp.

(in-package :cl-cc)

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

(defmacro define-float-binary-emitter (fn-name asm-op)
  "Define an XMM binary emitter: MOVSD dst←lhs, then ASM-OP dst←rhs."
  `(defun ,fn-name (inst stream)
     (let ((dst (vm-reg-to-xmm (vm-dst inst)))
           (lhs (vm-reg-to-xmm (vm-lhs inst)))
           (rhs (vm-reg-to-xmm (vm-rhs inst))))
       (emit-movsd-xx dst lhs stream)
       (,asm-op dst rhs stream))))

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

(define-float-binary-emitter emit-vm-float-add emit-addsd-xx)
(define-float-binary-emitter emit-vm-float-sub emit-subsd-xx)
(define-float-binary-emitter emit-vm-float-mul emit-mulsd-xx)
(define-float-binary-emitter emit-vm-float-div emit-divsd-xx)

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

;; Bit manipulation + shift + select emitters (emit-vm-not through emit-vm-select)
;; are in x86-64-emit-ops-bits.lisp (loaded next).
;; Type predicate, boolean logical, and bitwise emitters are in
;; x86-64-emit-ops-logical.lisp (loaded after x86-64-emit-ops-bits.lisp).
