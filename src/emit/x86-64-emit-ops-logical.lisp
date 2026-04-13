;;;; src/emit/x86-64-emit-ops-logical.lisp — x86-64 Logical, Bitwise, and Type-Predicate Emitters
;;;;
;;;; Per-instruction native code emitters for:
;;;;   Type predicates: emit-vm-null-p, emit-vm-true-pred, emit-vm-false-pred
;;;;   Boolean logical: emit-vm-and, emit-vm-or
;;;;   Binary bitwise: emit-vm-logand, emit-vm-logior, emit-vm-logxor,
;;;;                   emit-vm-logeqv, emit-vm-logtest, emit-vm-logbitp
;;;;
;;;; Emitter DSL macros (define-binary-alu-emitter, etc.) and arithmetic/comparison
;;;; emitters are in x86-64-emit-ops.lisp (loads before this file).
;;;;
;;;; Load order: after x86-64-emit-ops.lisp, before x86-64-program.lisp.

(in-package :cl-cc)

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
