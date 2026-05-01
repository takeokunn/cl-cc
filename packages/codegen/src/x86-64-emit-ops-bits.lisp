;;;; x86-64-emit-ops-bits.lisp — Bit manipulation, shift, and select emitters
;;;
;;; Extracted from x86-64-emit-ops.lisp.
;;; Contains: emit-vm-not, emit-vm-lognot, emit-vm-logcount,
;;;           emit-vm-integer-length, emit-vm-bswap, emit-vm-inc, emit-vm-dec,
;;;           emit-vm-abs, emit-vm-ash, emit-vm-rotate, emit-vm-min,
;;;           emit-vm-max, emit-vm-select.
;;;
;;; Depends on x86-64-emit-ops.lisp (define-cmov-emitter, define-unary-mov-emitter,
;;;   emit-* helpers from x86-64-encoding.lisp).
;;; Load order: immediately after x86-64-emit-ops.lisp.

(in-package :cl-cc/codegen)

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
