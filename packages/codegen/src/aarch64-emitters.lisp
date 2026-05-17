;;;; packages/emit/src/aarch64-emitters.lisp - AArch64 VM Instruction Emitters
;;;;
;;;; Per-VM-instruction emit-a64-vm-* functions that translate individual
;;;; VM IR instructions into AArch64 machine code via the encode-* layer.
;;;; Includes: define-a64-binary-emitter / define-a64-csel-emitter macros,
;;;; emit-a64-vm-{const,move,select,bswap,rotate,halt,jump,jump-zero,
;;;; spill-store,spill-load,ret} and all arithmetic/comparison/logic emitters.
;;;;
;;;; Register mapping, instruction encoders, and the label-offset builder are
;;;; in aarch64-codegen.lisp (loads before this file).
;;;;
;;;; Load order: after aarch64-codegen.lisp, before aarch64-program.lisp.

(in-package :cl-cc/codegen)

(defmacro define-a64-binary-emitter (fn-name encode-fn)
  "Define an AArch64 binary VM instruction emitter using ENCODE-FN(rd rn rm)."
  `(defun ,fn-name (inst stream)
     (let ((rd (a64-reg (vm-dst inst)))
           (rn (a64-reg (vm-lhs inst)))
           (rm (a64-reg (vm-rhs inst))))
       (emit-a64-instr (,encode-fn rd rn rm) stream))))

(defmacro define-a64-csel-emitter (fn-name description cond-code)
  "Define an AArch64 CSEL-based min/max emitter with COND-CODE."
  `(defun ,fn-name (inst stream)
     ,description
     (let ((rd (a64-reg (vm-dst inst)))
           (rn (a64-reg (vm-lhs inst)))
           (rm (a64-reg (vm-rhs inst))))
        (emit-a64-instr (encode-cmp rn rm) stream)
        (emit-a64-instr (encode-csel rd rn rm ,cond-code) stream))))

(defmacro define-a64-unary-emitter (fn-name encode-fn)
  "Define an AArch64 unary VM instruction emitter using ENCODE-FN(rd rn)."
  `(defun ,fn-name (inst stream)
     (let ((rd (a64-reg (vm-dst inst)))
           (rn (a64-reg (vm-src inst))))
       (emit-a64-instr (,encode-fn rd rn) stream))))

(defun emit-a64-vm-const (inst stream)
  (let ((rd (a64-reg (vm-dst inst)))
        (value (vm-value inst)))
    ;; Handle negative values via logical masking to 64-bit
    (emit-a64-mov-imm64 rd (logand value #xFFFFFFFFFFFFFFFF) stream)))

(defun emit-a64-vm-move (inst stream)
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-src inst))))
    (unless (= rd rn)
      (emit-a64-instr (encode-mov-rr rd rn) stream))))

(define-a64-binary-emitter emit-a64-vm-add encode-add)
(define-a64-binary-emitter emit-a64-vm-sub encode-sub)
(define-a64-binary-emitter emit-a64-vm-mul encode-mul)
(define-a64-binary-emitter emit-a64-vm-integer-mul-high-u encode-umulh)
(define-a64-binary-emitter emit-a64-vm-integer-mul-high-s encode-smulh)

;;; Checked arithmetic emitters (FR-303 overflow detection)
;;;
;;; AArch64 pattern: ADDS/SUBS rd,rn,rm + B.cond VC(cond=7) #2 + BRK #1
;;; ADDS/SUBS set the V (overflow) flag in NZCV.
;;; B.cond VC (overflow clear, cond=7) branches past BRK when no overflow.
;;; BRK #1 traps on overflow.
;;; Total: 3 instructions = 12 bytes for add-checked and sub-checked.

(defun emit-a64-vm-add-checked (inst stream)
  "vm-add-checked: dst = lhs + rhs with hardware overflow trap (FR-303).
   ADDS sets V on overflow; B.cond VC skips BRK; BRK traps."
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-lhs inst)))
        (rm (a64-reg (vm-rhs inst))))
    (emit-a64-instr (encode-adds rd rn rm) stream)
    (emit-a64-instr (encode-b-cond 2 7) stream)     ; B.VC #2 — skip BRK if no overflow
    (emit-a64-instr (encode-brk 1) stream)))          ; BRK #1 — overflow trap

(defun emit-a64-vm-sub-checked (inst stream)
  "vm-sub-checked: dst = lhs - rhs with hardware overflow trap (FR-303).
   SUBS sets V on overflow; B.cond VC skips BRK; BRK traps."
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-lhs inst)))
        (rm (a64-reg (vm-rhs inst))))
    (emit-a64-instr (encode-subs rd rn rm) stream)
    (emit-a64-instr (encode-b-cond 2 7) stream)     ; B.VC #2 — skip BRK if no overflow
    (emit-a64-instr (encode-brk 1) stream)))          ; BRK #1 — overflow trap

(defun emit-a64-vm-mul-checked (inst stream)
  "vm-mul-checked: dst = lhs * rhs with overflow trap (FR-303).
   MUL does not set flags on AArch64. Uses SMULH to detect overflow:
   MUL rd + SMULH X16 + ASR X17,rd,#63 + CMP X16,X17 + B.EQ + BRK.
   Uses X16 (IP0) and X17 (IP1) as temporary registers.
   6 instructions = 24 bytes."
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-lhs inst)))
        (rm (a64-reg (vm-rhs inst)))
        (tmp 16)                                       ; X16 = IP0 scratch
        (sext 17))                                      ; X17 = IP1 scratch
    (emit-a64-instr (encode-mul rd rn rm) stream)       ; rd = low 64 bits
    (emit-a64-instr (encode-smulh tmp rn rm) stream)    ; tmp = high 64 bits
    (emit-a64-instr (encode-asr sext rd 63) stream)     ; sext = sign_extend(rd) = 0 or -1
    (emit-a64-instr (encode-cmp tmp sext) stream)       ; CMP high, expected
    (emit-a64-instr (encode-b-cond 1 0) stream)         ; B.EQ #1 — skip if no overflow
    (emit-a64-instr (encode-brk 1) stream)))             ; BRK #1 — overflow trap
(define-a64-unary-emitter emit-a64-vm-sqrt encode-fsqrt)

;;; Libm call emitters for sin/cos/exp/log (FR-286)
;;;
;;; AArch64 libm call sequence (worst-case 36 bytes = 9 instructions):
;;;   FMOV D0, Dsrc          — move float arg to AAPCS64 D0
;;;   STP X30, X31, [SP,#-16]! — save LR (clobbered by BLR)
;;;   MOVZ/MOVK X16, #addr   — load 64-bit libm address (up to 4 instrs)
;;;   BLR X16                — indirect call
;;;   LDP X30, X31, [SP], #16 — restore LR
;;;   FMOV Ddst, D0          — move float result from D0

(defmacro define-a64-float-libm-unary-emitter (fn-name libm-fn)
  "Define an AArch64 emitter that calls libm function LIBM-FN via BLR for a VM unary float instruction."
  `(defun ,fn-name (inst stream)
     (let ((rd (a64-reg (vm-dst inst)))
           (rn (a64-reg (vm-src inst))))
       ;; FMOV D0, Dsrc — move arg to AAPCS64 float arg register
       (emit-a64-instr (encode-fmov-dd 0 rn) stream)
       ;; STP X30, X31(ZR), [SP, #-16]! — save LR on stack
       (emit-a64-instr (encode-stp-pre +a64-lr+ +a64-zr+ +a64-sp+ -2) stream)
       ;; MOVZ/MOVK X16, #addr — load libm function address
       (emit-a64-mov-imm64 16
         (load-time-value
          (sb-sys:sap-int
           (sb-alien:alien-sap
            (sb-alien:extern-alien ,libm-fn (function double-float double-float)))))
         stream)
       ;; BLR X16 — call libm
       (emit-a64-instr (encode-blr 16) stream)
       ;; LDP X30, X31(ZR), [SP], #16 — restore LR
       (emit-a64-instr (encode-ldp-post +a64-lr+ +a64-zr+ +a64-sp+ 2) stream)
       ;; FMOV Ddst, D0 — move result from AAPCS64 return register
       (emit-a64-instr (encode-fmov-dd rd 0) stream))))

(define-a64-float-libm-unary-emitter emit-a64-vm-sin "sin")
(define-a64-float-libm-unary-emitter emit-a64-vm-cos "cos")
(define-a64-float-libm-unary-emitter emit-a64-vm-exp "exp")
(define-a64-float-libm-unary-emitter emit-a64-vm-log "log")
(define-a64-float-libm-unary-emitter emit-a64-vm-tan "tan")
(define-a64-float-libm-unary-emitter emit-a64-vm-asin "asin")
(define-a64-float-libm-unary-emitter emit-a64-vm-acos "acos")
(define-a64-float-libm-unary-emitter emit-a64-vm-atan "atan")
(define-a64-csel-emitter emit-a64-vm-min "vm-min: dst = min(lhs, rhs) — signed, branchless via CSEL LT." 11)
(define-a64-csel-emitter emit-a64-vm-max "vm-max: dst = max(lhs, rhs) — signed, branchless via CSEL GT." 12)

(defun emit-a64-vm-select (inst stream)
  "vm-select: dst = cond ? then : else  (branchless via CSEL NE)."
  (let ((rd (a64-reg (vm-dst inst)))
        (cond (a64-reg (vm-select-cond-reg inst)))
        (then (a64-reg (vm-select-then-reg inst)))
        (else (a64-reg (vm-select-else-reg inst))))
    (emit-a64-instr (encode-mov-rr rd else) stream)
    (emit-a64-instr (encode-cmp cond 31) stream)
    (emit-a64-instr (encode-csel rd then rd 1) stream)))

(defun emit-a64-vm-bswap (inst stream)
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-src inst))))
    (emit-a64-instr (encode-rev32 rd rn) stream)))

(defun emit-a64-vm-rotate (inst stream)
  (let ((rd (a64-reg (vm-dst inst)))
        (rn (a64-reg (vm-lhs inst)))
        (rm (a64-reg (vm-rhs inst))))
    (emit-a64-instr (encode-mov-rr rd rn) stream)
    (emit-a64-instr (encode-rorv rd rd rm) stream)))

(defun emit-a64-vm-halt (inst stream)
  "Move result to X0 (return register). Always emits exactly one MOV instruction."
  (let ((result-reg (a64-reg (vm-reg inst))))
    (emit-a64-instr (encode-mov-rr 0 result-reg) stream)))

(defun emit-a64-vm-jump (inst stream current-pos label-offsets)
  "Emit B #imm26 for unconditional jump."
  (let* ((target-label (vm-label-name inst))
         (target-pos (gethash target-label label-offsets))
         ;; offset in instruction units: (target_byte_offset - current_byte_offset) / 4
         (byte-offset (- target-pos current-pos))
         (imm26 (ash byte-offset -2)))
    (emit-a64-instr (encode-b imm26) stream)))

(defun emit-a64-vm-jump-zero (inst stream current-pos label-offsets)
  "Emit CBZ Xn, #imm19 for conditional branch on zero."
  (let* ((rn (a64-reg (vm-reg inst)))
         (target-label (vm-label-name inst))
         (target-pos (gethash target-label label-offsets))
         (byte-offset (- target-pos current-pos))
         (imm19 (ash byte-offset -2)))
    (emit-a64-instr (encode-cbz rn imm19) stream)))

(defun emit-a64-vm-spill-store (inst stream)
  "STUR Xs, [BASE, OFFSET] using the active spill base register and slot bias."
  (let* ((src-phys (vm-spill-src inst))
         (src-num (cdr (assoc src-phys *aarch64-reg-number*)))
         (offset (a64-spill-slot-offset (vm-spill-slot inst))))
    (emit-a64-instr (encode-stur src-num *current-a64-spill-base-reg* offset) stream)))

(defun emit-a64-vm-spill-load (inst stream)
  "LDUR Xd, [BASE, OFFSET] using the active spill base register and slot bias."
  (let* ((dst-phys (vm-spill-dst inst))
         (dst-num (cdr (assoc dst-phys *aarch64-reg-number*)))
         (offset (a64-spill-slot-offset (vm-spill-slot inst))))
    (emit-a64-instr (encode-ldur dst-num *current-a64-spill-base-reg* offset) stream)))

(defun emit-a64-vm-ret (inst stream)
  "Emit inline epilogue (restore all callee-saved registers) then RET.
   Must stay in sync with emit-a64-prologue and a64-instruction-size for vm-ret."
  (declare (ignore inst))
  (dolist (pair (reverse (a64-used-callee-saved-pairs *current-a64-regalloc*)))
    (destructuring-bind (rn rm) pair
      (emit-a64-instr (encode-ldp-post rn rm +a64-sp+ 2) stream)))
  (emit-a64-instr +a64-ret+ stream))

;;; FR-268: AArch64 Constant Islands / Literal Pools
;;;
;;; ADR/ADRP + LDR for PC-relative constant pool loading.  Large immediates
;;; (beyond MOVZ/MOVK 16-bit range) are loaded from a literal pool placed
;;; at the end of the function.

(defun encode-adr (rd offset)
  "ADR Rd, #offset  — PC-relative address (1 MiB range, page-relative).
   Encoding: 0xx10000 iiiii iiiiii iiiii iiiii xxxxx"
  (logior (ash (ldb (byte 2 29) (logand offset #x1FFFFF)) 29) ; immlo[1:0]
          #x10000000
          (ash (ldb (byte 3 5) (logand offset #x1FFFFF)) 8)  ; immhi[18:16]
          (ash rd 0)))

(defun encode-adrp (rd offset)
  "ADRP Rd, #offset  — PC-relative page address (4 GiB range).
   Encoding: 1xx10000 iiiii iiiiii iiiii iiiii xxxxx"
  (logior (ash 1 31)
          (ash (ldb (byte 2 29) (logand offset #x1FFFFF)) 29)
          #x10000000
          (ash (ldb (byte 3 5) (logand offset #x1FFFFF)) 8)
          (ash rd 0)))

(defun encode-ldr-literal (rt offset)
  "LDR Rt, [PC, #offset]  — load from literal pool (word, scaled by 4).
   Encoding: 0xx11000 iiiii iiiiii iiiii iiiii xxxxx"
  (declare (type fixnum rt offset))
  (let ((imm19 (ash offset -2)))  ; 19-bit signed immediate / 4
    (logior #x18000000
            (ash (ldb (byte 19 0) (logand imm19 #x7FFFF)) 5)
            (ash rt 0))))

;;; FR-268: Literal pool builder
;;;
;;; Accumulates 64-bit constants and emits them at function end.  The pool is
;;; placed after the RET instruction, aligned to 8 bytes.

(defstruct a64-literal-pool
  "Accumulates 64-bit constant values for PC-relative loading."
  (entries nil :type list))  ; list of u64 values

(defun a64-pool-add (pool value)
  "Add a 64-bit VALUE to the literal pool.  Returns the 0-based entry index."
  (let ((idx (length (a64-literal-pool-entries pool))))
    (push value (a64-literal-pool-entries pool))
    idx))

(defun a64-pool-emit (pool stream)
  "Emit all pool entries as 8-byte little-endian values, 8-byte aligned."
  (let* ((entries (nreverse (a64-literal-pool-entries pool)))
         (pad (mod (- 8 (mod (* 4 (length entries)) 8)) 8)))
    (dolist (val entries)
      ;; Emit 8 bytes little-endian
      (emit-a64-instr (logand val #xFFFFFFFF) stream)
      (emit-a64-instr (ash val -32) stream))
    ;; Align to 8 bytes
    (loop repeat (/ pad 4) do (emit-a64-instr 0 stream))))
