;;;; packages/emit/src/x86-64-codegen.lisp — Two-pass label resolution, dispatch, and public API
;;;
;;; Instruction size table, label offset computation, jump-emitting dispatch,
;;; and the top-level emit-vm-program / compile-to-x86-64-bytes entry points.
;;; Load order: after x86-64-emit-ops.lisp

(in-package :cl-cc/codegen)

;;; Two-Pass Code Generation (Labels + Jumps)

;;; ============================================================
;;; x86-64 Instruction Size Table — Data / Logic Separation
;;; ============================================================
;;;
;;; Encoding DATA is declared in *X86-64-INSTRUCTION-SIZE-SPECS*.
;;; The LOGIC that populates the hash table is in POPULATE-SIZE-TABLE.
;;; Each spec entry: (type-spec size)
;;;   type-spec = symbol   → single mapping
;;;   type-spec = (s1 s2…) → group mapping (all get same size)

(defparameter *x86-64-instruction-size-specs*
  '(
    ;; Constants and copies
    (vm-const                          10)  ; REX + opcode + 8-byte immediate
    (vm-move                            3)  ; REX + opcode + ModR/M
    ;; Arithmetic: mov + op
    ((vm-add vm-integer-add)            6)  ; mov + add (3+3)
    ((vm-sub vm-integer-sub)            6)  ; mov + sub
    ((vm-mul vm-integer-mul)            7)  ; mov + imul (3+4, 0F AF)
    ;; Checked arithmetic (FR-303): MOV + ALU + JO(rel32) + UD2
    ((vm-add-checked vm-sub-checked)   14)  ; mov + add/sub + jo + ud2 (3+3+6+2)
    (vm-mul-checked                    15)  ; mov + imul + jo + ud2 (3+4+6+2)
    ((vm-integer-mul-high-u vm-integer-mul-high-s) 19) ; mul-high sequence + mov
    ;; Control flow
    (vm-halt                            3)  ; mov result to RAX
    (vm-label                           0)  ; Labels emit no code
    (vm-jump                            5)  ; JMP rel32
    (vm-jump-zero                       9)  ; TEST + JE rel32 (3 + 6)
    (vm-ret                             1)  ; RET
    ;; No-ops in native codegen
    ;; I/O: vm-print = MOV RDI(3) + MOV R11(10) + CALL R11(3) = 16 bytes (FR-298)
    (vm-print                           16)
    (vm-closure                          0)
    (vm-call                            6)
    (vm-tail-call                       3)
    ;; Register spilling
    (vm-spill-store                     4)  ; MOV [rbp-disp8], reg
    (vm-spill-load                      4)  ; MOV reg, [rbp-disp8]
    ;; Comparison: CMP(3) + SETcc(3-4) + MOVZX(4) = 12 max
    ((vm-lt vm-gt vm-le vm-ge vm-num-eq vm-eq) 12)
    ;; Logical NOT / bitwise NOT
    (vm-not                            12)  ; TEST+SETE+MOVZX
    (vm-lognot                          7)  ; MOV+NOT
    (vm-logcount                        5)
    (vm-integer-length                 16)
    (vm-bswap                           6)
    ;; Unary arithmetic: MOV(3) + op(3-4) = 7
    ((vm-neg vm-inc vm-dec)             7)
    ;; Abs: MOV + CMP-imm32 + JGE-short + NEG = 15
    (vm-abs                            15)
    ;; Min/max/select: MOV + CMP + CMOV = 10
    ((vm-min vm-max vm-select)         10)
    ;; Ash: fixed 24-byte sequence
    (vm-ash                            24)
    ;; Rotate: MOV + MOV + ROR + save/restore RCX = 11 bytes
    (vm-rotate                         11)
    ;; IDIV-based: truncate/rem = 21, floor-div = 34, floor-mod = 37
    ((vm-truncate vm-rem)              21)
    (vm-div                            34)
    (vm-mod                            37)
    ;; Boolean logical: XOR+TEST+JE+TEST+JE+ADD = 17
    ((vm-and vm-or)                    17)
    ;; Binary logical: MOV + op = 6
    ((vm-logand vm-logior vm-logxor)    6)
    ;; Scalar float ops: MOVSD + op = 8
    ((vm-float-add vm-float-sub vm-float-mul vm-float-div vm-sqrt) 8)
    ;; Libm-call transcendental ops (FR-286): MOVSD + MOV+addr + CALL + MOVSD = 21
    ((vm-sin-inst vm-cos-inst vm-exp-inst vm-log-inst
      vm-tan-inst vm-asin-inst vm-acos-inst vm-atan-inst) 21)
    ;; Misc bitwise
    (vm-logeqv                          9)
    (vm-logtest                        14)
    (vm-logbitp                        15)
    ;; Type predicates: null-p = 11; others = 10 (MOV imm64)
    (vm-null-p                         11)
    ((vm-number-p vm-integer-p vm-cons-p vm-symbol-p vm-function-p) 10)
    ;; FR-318 staged path: non-local control instructions use conditional
    ;; shadow-stack marker sequences (enabled=6 bytes / disabled=2 bytes).
    ((cl-cc/vm::vm-push-handler cl-cc/vm::vm-pop-handler
      cl-cc/vm::vm-bind-restart cl-cc/vm::vm-invoke-restart
      cl-cc/vm::vm-signal cl-cc/vm::vm-error-instruction
      cl-cc/vm::vm-cerror cl-cc/vm::vm-warn
      cl-cc/vm::vm-establish-handler cl-cc/vm::vm-remove-handler
      cl-cc/vm::vm-sync-handler-regs cl-cc/vm::vm-signal-error
      cl-cc/vm::vm-establish-catch cl-cc/vm::vm-throw) 2))
  "Declarative spec: VM instruction types → x86-64 encoded byte sizes.
   Each entry is (type-spec size) where type-spec is a symbol or list of symbols.")

(defparameter *x86-64-instruction-sizes*
  (populate-size-table *x86-64-instruction-size-specs*)
  "Maps VM instruction struct-type symbols to their x86-64 encoded byte sizes.
   Used by the first pass of two-pass code generation to build label offset tables.")

(defconstant +stack-probe-page-size+ 4096
  "Page stride used by native backend stack probing.")

(defconstant +x86-64-stack-probe-size+ 9
  "Byte size of one x86-64 OR [RSP-disp32], imm8 stack probe.")

(defconstant +x86-64-tls-canary-disp32+ #x28
  "Linux/x86-64 TLS canary offset in FS segment.")
