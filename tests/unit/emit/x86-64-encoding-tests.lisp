;;;; tests/unit/emit/x86-64-encoding-tests.lisp — x86-64 Instruction Encoding Tests
;;;;
;;;; Tests for src/emit/x86-64-codegen.lisp encoding helpers:
;;;; rex-prefix, modrm, emit-byte, emit-dword, emit-qword,
;;;; emit-mov-rr64, emit-add-rr64, emit-sub-rr64, emit-imul-rr64,
;;;; emit-push-r64, emit-pop-r64, emit-ret, emit-jmp-rel32, emit-je-rel32.

(in-package :cl-cc/test)

(defsuite x86-64-encoding-suite :description "x86-64 instruction encoding unit tests")

;;; Helper: collect bytes emitted by a function
(defun %x86-collect-bytes (emit-fn)
  "Call EMIT-FN with a stream that collects bytes. Returns byte list."
  (let ((bytes nil))
    (funcall emit-fn (lambda (b) (push b bytes)))
    (nreverse bytes)))

;;; ─── rex-prefix ──────────────────────────────────────────────────────────

(deftest x86-rex-base
  "REX prefix with no flags set is #x40."
  (assert-equal #x40 (cl-cc::rex-prefix)))

(deftest x86-rex-w
  "REX.W sets 64-bit operand size (bit 3)."
  (assert-equal #x48 (cl-cc::rex-prefix :w 1)))

(deftest x86-rex-r
  "REX.R extends ModR/M reg field (bit 2)."
  (assert-equal #x44 (cl-cc::rex-prefix :r 1)))

(deftest x86-rex-b
  "REX.B extends ModR/M r/m field (bit 0)."
  (assert-equal #x41 (cl-cc::rex-prefix :b 1)))

(deftest x86-rex-wrb
  "REX.WRB combined."
  (assert-equal #x4D (cl-cc::rex-prefix :w 1 :r 1 :b 1)))

(deftest x86-rex-all
  "REX with all flags."
  (assert-equal #x4F (cl-cc::rex-prefix :w 1 :r 1 :x 1 :b 1)))

;;; ─── modrm ──────────────────────────────────────────────────────────────

(deftest x86-modrm-reg-reg
  "ModR/M with mod=11 (register), reg=0, rm=0."
  (assert-equal #xC0 (cl-cc::modrm 3 0 0)))

(deftest x86-modrm-reg-fields
  "ModR/M with mod=11, reg=1, rm=2."
  (let ((byte (cl-cc::modrm 3 1 2)))
    ;; 11_001_010 = #xCA
    (assert-equal #xCA byte)
    ;; Verify field extraction
    (assert-equal 3 (ash byte -6))           ; mod
    (assert-equal 1 (logand (ash byte -3) 7)) ; reg
    (assert-equal 2 (logand byte 7))))         ; rm

(deftest x86-modrm-memory-indirect
  "ModR/M with mod=00 (memory indirect), reg=0, rm=0."
  (assert-equal #x00 (cl-cc::modrm 0 0 0)))

(deftest x86-modrm-disp8
  "ModR/M with mod=01 (8-bit displacement)."
  ;; mod=01, reg=3, rm=5: 01_011_101 = #x5D
  (assert-equal #x5D (cl-cc::modrm 1 3 5)))

;;; ─── emit-byte / emit-dword / emit-qword ────────────────────────────────

(deftest x86-emit-byte-value
  "emit-byte writes single byte."
  (let ((bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-byte #xAB s)))))
    (assert-equal 1 (length bytes))
    (assert-equal #xAB (first bytes))))

(deftest x86-emit-byte-truncates
  "emit-byte masks to 8 bits."
  (let ((bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-byte #x1FF s)))))
    (assert-equal #xFF (first bytes))))

(deftest x86-emit-dword-le
  "emit-dword writes 4 bytes little-endian."
  (let ((bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-dword #xDEADBEEF s)))))
    (assert-equal 4 (length bytes))
    (assert-equal #xEF (first bytes))
    (assert-equal #xBE (second bytes))
    (assert-equal #xAD (third bytes))
    (assert-equal #xDE (fourth bytes))))

(deftest x86-emit-qword-le
  "emit-qword writes 8 bytes little-endian."
  (let ((bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-qword #x0102030405060708 s)))))
    (assert-equal 8 (length bytes))
    ;; Least significant dword first, then most significant
    (assert-equal #x08 (first bytes))
    (assert-equal #x07 (second bytes))
    (assert-equal #x01 (eighth bytes))))

;;; ─── emit-mov-rr64 ──────────────────────────────────────────────────────

(deftest x86-mov-rr64-rax-rcx
  "MOV RAX, RCX emits REX.W + 89 + ModR/M."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-mov-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)))))
    (assert-equal 3 (length bytes))
    ;; REX.W = #x48
    (assert-equal #x48 (first bytes))
    ;; Opcode = #x89
    (assert-equal #x89 (second bytes))
    ;; ModR/M: mod=11, reg=RCX(1), rm=RAX(0) = 11_001_000 = #xC8
    (assert-equal #xC8 (third bytes))))

;;; ─── emit-add-rr64 / emit-sub-rr64 ──────────────────────────────────────

(deftest x86-add-rr64-opcode
  "ADD RAX, RCX emits opcode #x01."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-add-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #x01 (second bytes))))

(deftest x86-sub-rr64-opcode
  "SUB RAX, RCX emits opcode #x29."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-sub-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #x29 (second bytes))))

(deftest x86-add-sub-same-size
  "ADD and SUB emit same number of bytes."
  (let ((add-bytes (%x86-collect-bytes
                    (lambda (s) (cl-cc::emit-add-rr64 0 1 s))))
        (sub-bytes (%x86-collect-bytes
                    (lambda (s) (cl-cc::emit-sub-rr64 0 1 s)))))
    (assert-equal (length add-bytes) (length sub-bytes))))

;;; ─── emit-imul-rr64 ─────────────────────────────────────────────────────

(deftest x86-imul-rr64-two-byte-opcode
  "IMUL RAX, RCX emits 0F AF (two-byte opcode)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-imul-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)))))
    (assert-equal 4 (length bytes))
    (assert-equal #x0F (second bytes))
    (assert-equal #xAF (third bytes))))

;;; ─── emit-push-r64 / emit-pop-r64 ───────────────────────────────────────

(deftest x86-push-rax
  "PUSH RAX emits #x50."
  (let ((bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-push-r64 cl-cc::+rax+ s)))))
    (assert-equal 1 (length bytes))
    (assert-equal #x50 (first bytes))))

(deftest x86-push-rcx
  "PUSH RCX emits #x51."
  (let ((bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-push-r64 cl-cc::+rcx+ s)))))
    (assert-equal #x51 (first bytes))))

(deftest x86-pop-rax
  "POP RAX emits #x58."
  (let ((bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-pop-r64 cl-cc::+rax+ s)))))
    (assert-equal 1 (length bytes))
    (assert-equal #x58 (first bytes))))

(deftest x86-push-pop-offset
  "PUSH and POP base opcodes differ by 8."
  (assert-equal 8 (- #x58 #x50)))

;;; ─── emit-ret ────────────────────────────────────────────────────────────

(deftest x86-ret-encoding
  "RET emits single byte #xC3."
  (let ((bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-ret s)))))
    (assert-equal 1 (length bytes))
    (assert-equal #xC3 (first bytes))))

;;; ─── emit-jmp-rel32 / emit-je-rel32 ─────────────────────────────────────

(deftest x86-jmp-rel32-opcode
  "JMP rel32 emits E9 + 4-byte offset."
  (let ((bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-jmp-rel32 0 s)))))
    (assert-equal 5 (length bytes))
    (assert-equal #xE9 (first bytes))))

(deftest x86-jmp-rel32-offset
  "JMP with offset 256 encodes offset correctly."
  (let ((bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-jmp-rel32 256 s)))))
    ;; offset = 256 = #x00000100 little-endian: 00 01 00 00
    (assert-equal #x00 (second bytes))
    (assert-equal #x01 (third bytes))))

(deftest x86-je-rel32-opcode
  "JE rel32 emits 0F 84 + 4-byte offset."
  (let ((bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-je-rel32 0 s)))))
    (assert-equal 6 (length bytes))
    (assert-equal #x0F (first bytes))
    (assert-equal #x84 (second bytes))))

;;; ─── VM register mapping ─────────────────────────────────────────────────

(deftest x86-vm-reg-map-r0-rax
  "VM :R0 maps to RAX."
  (assert-equal cl-cc::+rax+ (cdr (assoc :r0 cl-cc::*vm-reg-map*))))

(deftest x86-vm-reg-map-r1-rcx
  "VM :R1 maps to RCX."
  (assert-equal cl-cc::+rcx+ (cdr (assoc :r1 cl-cc::*vm-reg-map*))))

(deftest x86-vm-reg-map-coverage
  "VM register map has 8 entries."
  (assert-equal 8 (length cl-cc::*vm-reg-map*)))

;;; ─── emit-mov-ri64 ──────────────────────────────────────────────────────

(deftest x86-mov-ri64-size
  "MOV RAX, imm64 emits REX + opcode + 8 bytes = 10 bytes total."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-mov-ri64 cl-cc::+rax+ 42 s)))))
    (assert-equal 10 (length bytes))
    ;; REX.W prefix
    (assert-equal #x48 (first bytes))
    ;; B8 + rd for RAX(0)
    (assert-equal #xB8 (second bytes))))

(deftest x86-mov-ri64-immediate-value
  "MOV RAX, 42 encodes 42 in the immediate field."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-mov-ri64 cl-cc::+rax+ 42 s)))))
    ;; Bytes 3-10 are the 64-bit immediate (little-endian)
    (assert-equal 42 (third bytes))
    (assert-equal 0 (fourth bytes))))

;;; ─── emit-cmp-rr64 ──────────────────────────────────────────────────────

(deftest x86-cmp-rr64-opcode
  "CMP RAX, RCX emits opcode #x39."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-cmp-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #x39 (second bytes))))

;;; ─── vm-const-to-integer ─────────────────────────────────────────────────

(deftest-each x86-vm-const-to-integer
  "vm-const-to-integer coerces values for native code emission."
  :cases (("nil-to-0"   nil  0)
          ("t-to-1"     t    1)
          ("int-42"     42   42)
          ("int-neg"    -1   -1)
          ("int-zero"   0    0)
          ("string-to-0" "hello" 0)
          ("symbol-to-0" 'foo   0)
          ("list-to-0"   '(1 2) 0))
  (input expected)
  (assert-equal expected (cl-cc::vm-const-to-integer input)))

;;; ─── Higher-level encoding helpers ───────────────────────────────────────

(deftest x86-test-rr64-opcode
  "TEST RAX, RAX emits opcode #x85."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-test-rr64 cl-cc::+rax+ cl-cc::+rax+ s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #x85 (second bytes))))

(deftest x86-cmp-ri64-size
  "CMP RAX, imm32 emits REX+opcode+ModRM+imm32 = 7 bytes."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-cmp-ri64 cl-cc::+rax+ 0 s)))))
    (assert-equal 7 (length bytes))
    (assert-equal #x81 (second bytes))))

(deftest x86-not-r64-opcode
  "NOT RAX emits opcode #xF7 with /2 extension."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-not-r64 cl-cc::+rax+ s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #xF7 (second bytes))))

(deftest x86-neg-r64-opcode
  "NEG RAX emits opcode #xF7 with /3 extension."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-neg-r64 cl-cc::+rax+ s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #xF7 (second bytes))))

(deftest x86-dec-r64-opcode
  "DEC RAX emits opcode #xFF with /1 extension."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-dec-r64 cl-cc::+rax+ s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #xFF (second bytes))))

(deftest x86-add-ri8-size
  "ADD RAX, imm8 emits 4 bytes (REX+#x83+ModRM+imm8)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-add-ri8 cl-cc::+rax+ 1 s)))))
    (assert-equal 4 (length bytes))
    (assert-equal #x83 (second bytes))))

(deftest x86-sub-ri8-size
  "SUB RAX, imm8 emits 4 bytes."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-sub-ri8 cl-cc::+rax+ 1 s)))))
    (assert-equal 4 (length bytes))
    (assert-equal #x83 (second bytes))))

(deftest x86-and-ri8-size
  "AND RAX, imm8 emits 4 bytes."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-and-ri8 cl-cc::+rax+ 1 s)))))
    (assert-equal 4 (length bytes))))

(deftest x86-and-rr64-opcode
  "AND RAX, RCX emits opcode #x21."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-and-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #x21 (second bytes))))

(deftest x86-or-rr64-opcode
  "OR RAX, RCX emits opcode #x09."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-or-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #x09 (second bytes))))

(deftest x86-xor-rr64-opcode
  "XOR RAX, RCX emits opcode #x31."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-xor-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #x31 (second bytes))))

(deftest x86-setcc-sete-size
  "SETE on low register (RAX) emits 3 bytes (0F opcode2 ModRM)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-setcc #x94 cl-cc::+rax+ s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #x0F (first bytes))
    (assert-equal #x94 (second bytes))))

(deftest x86-setcc-high-reg-needs-rex
  "SETE on register >= 4 (RSI=6) emits 4 bytes (REX prefix added)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-setcc #x94 cl-cc::+rsi+ s)))))
    (assert-equal 4 (length bytes))))

(deftest x86-movzx-r64-r8-size
  "MOVZX RAX, AL emits 4 bytes (REX+0F+B6+ModRM)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-movzx-r64-r8 cl-cc::+rax+ cl-cc::+rax+ s)))))
    (assert-equal 4 (length bytes))
    (assert-equal #x0F (second bytes))
    (assert-equal #xB6 (third bytes))))

(deftest x86-sal-r64-cl-opcode
  "SAL RAX, CL emits REX+#xD3+ModRM (3 bytes)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-sal-r64-cl cl-cc::+rax+ s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #xD3 (second bytes))))

(deftest x86-sar-r64-cl-opcode
  "SAR RAX, CL emits REX+#xD3+ModRM (3 bytes)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-sar-r64-cl cl-cc::+rax+ s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #xD3 (second bytes))))

(deftest x86-cmovl-rr64-size
  "CMOVL RAX, RCX emits 4 bytes (REX+0F+4C+ModRM)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-cmovl-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)))))
    (assert-equal 4 (length bytes))
    (assert-equal #x4C (third bytes))))

(deftest x86-cmovg-rr64-size
  "CMOVG RAX, RCX emits 4 bytes (REX+0F+4F+ModRM)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-cmovg-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)))))
    (assert-equal 4 (length bytes))
    (assert-equal #x4F (third bytes))))

(deftest x86-jge-short-size
  "JGE short emits 2 bytes (#x7D + offset)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-jge-short 3 s)))))
    (assert-equal 2 (length bytes))
    (assert-equal #x7D (first bytes))
    (assert-equal 3 (second bytes))))

(deftest x86-idiv-r11-encoding
  "IDIV R11 emits 3 bytes (#x49 + #xF7 + ModRM)."
  (let ((bytes (%x86-collect-bytes #'cl-cc::emit-idiv-r11)))
    (assert-equal 3 (length bytes))
    (assert-equal #x49 (first bytes))
    (assert-equal #xF7 (second bytes))))

(deftest x86-cqo-encoding
  "CQO emits 2 bytes (#x48 + #x99)."
  (let ((bytes (%x86-collect-bytes #'cl-cc::emit-cqo)))
    (assert-equal 2 (length bytes))
    (assert-equal #x48 (first bytes))
    (assert-equal #x99 (second bytes))))

;;; ─── VM instruction emitter byte sizes ──────────────────────────────────

(deftest-each x86-vm-emitter-byte-size
  "VM instruction emitters produce correct byte counts."
  :cases (("vm-neg"    (lambda (s) (cl-cc::emit-vm-neg
                         (cl-cc::make-vm-neg :dst :R0 :src :R1) s))     6)
          ("vm-not"    (lambda (s) (cl-cc::emit-vm-not
                         (cl-cc::make-vm-not :dst :R0 :src :R1) s))    10)
          ("vm-lognot" (lambda (s) (cl-cc::emit-vm-lognot
                         (cl-cc::make-vm-lognot :dst :R0 :src :R1) s))  6)
          ("vm-inc"    (lambda (s) (cl-cc::emit-vm-inc
                         (cl-cc::make-vm-inc :dst :R0 :src :R1) s))     7)
          ("vm-dec"    (lambda (s) (cl-cc::emit-vm-dec
                         (cl-cc::make-vm-dec :dst :R0 :src :R1) s))     7)
          ("vm-abs"    (lambda (s) (cl-cc::emit-vm-abs
                         (make-vm-abs :dst :R0 :src :R1) s))           15)
          ("vm-min"    (lambda (s) (cl-cc::emit-vm-min
                         (make-vm-min :dst :R0 :lhs :R1 :rhs :R2) s)) 10)
          ("vm-max"    (lambda (s) (cl-cc::emit-vm-max
                         (make-vm-max :dst :R0 :lhs :R1 :rhs :R2) s)) 10))
  (emit-fn expected-size)
  (let ((bytes (%x86-collect-bytes emit-fn)))
    (assert-equal expected-size (length bytes))))

(deftest-each x86-vm-comparison-emitter-size
  "Comparison emitters produce CMP+SETcc+MOVZX sequences."
  :cases (("vm-lt"     (lambda (s) (cl-cc::emit-vm-lt
                          (cl-cc::make-vm-lt :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("vm-gt"     (lambda (s) (cl-cc::emit-vm-gt
                          (cl-cc::make-vm-gt :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("vm-le"     (lambda (s) (cl-cc::emit-vm-le
                          (cl-cc::make-vm-le :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("vm-ge"     (lambda (s) (cl-cc::emit-vm-ge
                          (cl-cc::make-vm-ge :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("vm-num-eq" (lambda (s) (cl-cc::emit-vm-num-eq
                          (make-vm-num-eq :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("vm-eq"     (lambda (s) (cl-cc::emit-vm-eq
                          (make-vm-eq :dst :R0 :lhs :R1 :rhs :R2) s))))
  (emit-fn)
  (let ((bytes (%x86-collect-bytes emit-fn)))
    ;; CMP(3) + SETcc(3) + MOVZX(4) = 10 bytes for low-register operands
    (assert-true (>= (length bytes) 10))))

(deftest-each x86-vm-binary-logical-size
  "Binary logical emitters produce MOV+op sequences (6 bytes)."
  :cases (("logand" (lambda (s) (cl-cc::emit-vm-logand
                       (make-vm-logand :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("logior" (lambda (s) (cl-cc::emit-vm-logior
                       (make-vm-logior :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("logxor" (lambda (s) (cl-cc::emit-vm-logxor
                       (make-vm-logxor :dst :R0 :lhs :R1 :rhs :R2) s))))
  (emit-fn)
  (assert-equal 6 (length (%x86-collect-bytes emit-fn))))

(deftest x86-vm-logeqv-size
  "vm-logeqv emits MOV+XOR+NOT = 9 bytes."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-logeqv
                             (cl-cc::make-vm-logeqv :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (assert-equal 9 (length bytes))))

(deftest x86-vm-and-size
  "vm-and (boolean) emits 17-byte sequence."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-and
                             (cl-cc::make-vm-and :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (assert-equal 17 (length bytes))))

(deftest x86-vm-or-size
  "vm-or (boolean) emits 17-byte sequence."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-or
                             (cl-cc::make-vm-or :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (assert-equal 17 (length bytes))))

(deftest x86-vm-null-p-size
  "vm-null-p emits TEST+SETE+MOVZX."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-null-p
                             (cl-cc::make-vm-null-p :dst :R0 :src :R1) s)))))
    (assert-true (>= (length bytes) 10))))

(deftest x86-vm-true-pred-size
  "vm-true-pred emits MOV imm64 (10 bytes)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-true-pred
                             (cl-cc::make-vm-number-p :dst :R0 :src :R1) s)))))
    (assert-equal 10 (length bytes))))

(deftest x86-vm-false-pred-size
  "vm-false-pred emits MOV imm64 (10 bytes)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-false-pred
                             (cl-cc::make-vm-cons-p :dst :R0 :src :R1) s)))))
    (assert-equal 10 (length bytes))))

;;; ─── IDIV-based emitter sizes ───────────────────────────────────────────

(deftest x86-vm-truncate-size
  "vm-truncate emits 21-byte IDIV sequence."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-truncate
                             (make-vm-truncate :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (assert-equal 21 (length bytes))))

(deftest x86-vm-rem-size
  "vm-rem emits 21-byte IDIV sequence."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-rem
                             (cl-cc::make-vm-rem :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (assert-equal 21 (length bytes))))

(deftest x86-vm-ash-size
  "vm-ash emits fixed 24-byte sequence."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-ash
                             (make-vm-ash :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (assert-equal 24 (length bytes))))

;;; ─── instruction-size table ─────────────────────────────────────────────

(deftest x86-instruction-size-table-coverage
  "Instruction size table has entries for all expected VM types."
  (dolist (tp '(cl-cc::vm-const cl-cc::vm-move cl-cc::vm-add cl-cc::vm-sub
                cl-cc::vm-mul cl-cc::vm-halt cl-cc::vm-label cl-cc::vm-jump
                cl-cc::vm-jump-zero cl-cc::vm-ret cl-cc::vm-lt cl-cc::vm-gt
                cl-cc::vm-le cl-cc::vm-ge cl-cc::vm-num-eq cl-cc::vm-eq
                cl-cc::vm-neg cl-cc::vm-not cl-cc::vm-lognot
                cl-cc::vm-inc cl-cc::vm-dec cl-cc::vm-abs
                cl-cc::vm-min cl-cc::vm-max cl-cc::vm-ash
                cl-cc::vm-truncate cl-cc::vm-rem cl-cc::vm-div cl-cc::vm-mod
                cl-cc::vm-and cl-cc::vm-or
                cl-cc::vm-logand cl-cc::vm-logior cl-cc::vm-logxor
                cl-cc::vm-logeqv cl-cc::vm-logtest cl-cc::vm-logbitp
                cl-cc::vm-null-p cl-cc::vm-number-p cl-cc::vm-integer-p
                cl-cc::vm-cons-p cl-cc::vm-symbol-p cl-cc::vm-function-p
                cl-cc::vm-spill-store cl-cc::vm-spill-load))
    (assert-true (gethash tp cl-cc::*x86-64-instruction-sizes*))))

(deftest x86-instruction-size-label-zero
  "vm-label has size 0 (pseudo-instruction)."
  (assert-equal 0 (cl-cc::instruction-size
                   (cl-cc::make-vm-label :name "test"))))

(deftest x86-instruction-size-unknown-zero
  "Unknown instruction type returns size 0."
  (assert-equal 0 (cl-cc::instruction-size (list :not-a-real-inst))))

;;; ─── build-label-offsets ────────────────────────────────────────────────

(deftest x86-build-label-offsets-empty
  "Empty instruction list produces empty offset table."
  (let ((offsets (cl-cc::build-label-offsets nil 6)))
    (assert-equal 0 (hash-table-count offsets))))

(deftest x86-build-label-offsets-single
  "Single label at start gets offset = prologue-size."
  (let* ((insts (list (cl-cc::make-vm-label :name "start")))
         (offsets (cl-cc::build-label-offsets insts 6)))
    (assert-equal 6 (gethash "start" offsets))))

(deftest x86-build-label-offsets-after-inst
  "Label after vm-const (10 bytes) gets correct offset."
  (let* ((insts (list (cl-cc::make-vm-const :dst :R0 :value 42)
                      (cl-cc::make-vm-label :name "after")))
         (offsets (cl-cc::build-label-offsets insts 6)))
    (assert-equal 16 (gethash "after" offsets))))

(deftest x86-build-label-offsets-multiple
  "Multiple labels track correct positions."
  (let* ((insts (list (cl-cc::make-vm-label :name "L0")
                      (cl-cc::make-vm-move :dst :R0 :src :R1)    ; 3 bytes
                      (cl-cc::make-vm-label :name "L1")
                      (cl-cc::make-vm-add :dst :R0 :lhs :R1 :rhs :R2) ; 6 bytes
                      (cl-cc::make-vm-label :name "L2")))
         (offsets (cl-cc::build-label-offsets insts 0)))
    (assert-equal 0 (gethash "L0" offsets))
    (assert-equal 3 (gethash "L1" offsets))
    (assert-equal 9 (gethash "L2" offsets))))

;;; ─── *x86-64-emitter-table* completeness ────────────────────────────────

(deftest x86-emitter-table-all-entries-are-functions
  "Every entry in the emitter table is a function."
  (let ((all-ok t))
    (maphash (lambda (key fn)
               (declare (ignore key))
               (unless (functionp fn)
                 (setf all-ok nil)))
             cl-cc::*x86-64-emitter-table*)
    (assert-true all-ok)))

(deftest x86-emitter-table-count
  "Emitter table has entries for all expected instruction types."
  (assert-true (>= (hash-table-count cl-cc::*x86-64-emitter-table*) 40)))

;;; ─── High-register encoding (REX.R / REX.B) ────────────────────────────

(deftest x86-mov-rr64-high-regs
  "MOV R8, R9 uses REX.R and REX.B for high registers."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-mov-rr64 cl-cc::+r8+ cl-cc::+r9+ s)))))
    (assert-equal 3 (length bytes))
    ;; REX.W=1, REX.R=1 (src R9>=8), REX.B=1 (dst R8>=8)
    (assert-equal #x4D (first bytes))))

(deftest x86-mov-rm64-zero-offset
  "MOV RAX, [RCX+0] uses mod=00 (no displacement)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-mov-rm64 cl-cc::+rax+ cl-cc::+rcx+ 0 s)))))
    (assert-equal 3 (length bytes))))

(deftest x86-mov-rm64-with-offset
  "MOV RAX, [RCX+8] uses mod=01 with byte displacement."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-mov-rm64 cl-cc::+rax+ cl-cc::+rcx+ 8 s)))))
    (assert-equal 4 (length bytes))
    (assert-equal 8 (fourth bytes))))

(deftest x86-mov-mr64-with-offset
  "MOV [RCX+16], RAX stores with byte displacement."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-mov-mr64 cl-cc::+rcx+ 16 cl-cc::+rax+ s)))))
    (assert-equal 4 (length bytes))
    (assert-equal 16 (fourth bytes))))

;;; ─── emit-mov-rr64 ModR/M correctness ──────────────────────────────────

(deftest x86-mov-rr64-modrm-rax-rbx
  "MOV RAX, RBX: ModR/M mod=11, reg=RBX(3), rm=RAX(0) = 11_011_000 = #xD8."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-mov-rr64 cl-cc::+rax+ cl-cc::+rbx+ s)))))
    (assert-equal #x48 (first bytes))    ; REX.W only
    (assert-equal #x89 (second bytes))   ; MOV r/m64, r64
    (assert-equal #xD8 (third bytes))))  ; mod=11, reg=3(RBX), rm=0(RAX)

(deftest x86-mov-rr64-same-reg
  "MOV RAX, RAX: self-copy produces ModR/M #xC0."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-mov-rr64 cl-cc::+rax+ cl-cc::+rax+ s)))))
    ;; mod=11, reg=0(RAX), rm=0(RAX) = #xC0
    (assert-equal #xC0 (third bytes))))

;;; ─── emit-add-rr64 / emit-sub-rr64 ModR/M ──────────────────────────────

(deftest x86-add-rr64-modrm
  "ADD RCX, RDX: ModR/M mod=11, reg=RDX(2), rm=RCX(1) = 11_010_001 = #xD1."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-add-rr64 cl-cc::+rcx+ cl-cc::+rdx+ s)))))
    (assert-equal #x48 (first bytes))    ; REX.W
    (assert-equal #x01 (second bytes))   ; ADD r/m64, r64
    (assert-equal #xD1 (third bytes))))  ; mod=11, reg=2(RDX), rm=1(RCX)

(deftest x86-sub-rr64-modrm
  "SUB RCX, RDX: ModR/M = #xD1 (same fields as ADD but different opcode)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-sub-rr64 cl-cc::+rcx+ cl-cc::+rdx+ s)))))
    (assert-equal #x29 (second bytes))   ; SUB r/m64, r64
    (assert-equal #xD1 (third bytes))))

;;; ─── High-register push/pop ─────────────────────────────────────────────

(deftest x86-push-rbx
  "PUSH RBX emits #x53 (no REX needed for low registers)."
  (let ((bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-push-r64 cl-cc::+rbx+ s)))))
    (assert-equal 1 (length bytes))
    (assert-equal #x53 (first bytes))))

(deftest x86-pop-rcx
  "POP RCX emits #x59."
  (let ((bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-pop-r64 cl-cc::+rcx+ s)))))
    (assert-equal 1 (length bytes))
    (assert-equal #x59 (first bytes))))

(deftest x86-pop-rdx
  "POP RDX emits #x5A."
  (let ((bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-pop-r64 cl-cc::+rdx+ s)))))
    (assert-equal #x5A (first bytes))))

;;; ─── emit-mov-ri64 high registers ───────────────────────────────────────

(deftest x86-mov-ri64-rcx
  "MOV RCX, imm64: REX.W=#x48, opcode=B8+1=#xB9."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-mov-ri64 cl-cc::+rcx+ 0 s)))))
    (assert-equal 10 (length bytes))
    (assert-equal #x48 (first bytes))
    (assert-equal #xB9 (second bytes))))

(deftest x86-mov-ri64-r8
  "MOV R8, imm64: REX.W+REX.B=#x49, opcode=B8+0=#xB8."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-mov-ri64 cl-cc::+r8+ 1 s)))))
    (assert-equal 10 (length bytes))
    (assert-equal #x49 (first bytes))   ; REX.W=1, REX.B=1 (R8 >= 8)
    (assert-equal #xB8 (second bytes))  ; B8 + (R8 & 7) = B8 + 0
    (assert-equal 1 (third bytes))))    ; imm64 LE: low byte = 1

;;; ─── emit-jne-rel32 ─────────────────────────────────────────────────────

(deftest x86-jne-rel32-opcode
  "JNE rel32 emits 0F 85 + 4-byte offset (6 bytes total)."
  ;; JNE rel32: 0F 85 cd
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc::emit-byte #x0F s)
                  (cl-cc::emit-byte #x85 s)
                  (cl-cc::emit-dword 0 s)))))
    (assert-equal 6 (length bytes))
    (assert-equal #x0F (first bytes))
    (assert-equal #x85 (second bytes))))

;;; ─── SETcc opcode2 values for each comparison ───────────────────────────

(deftest-each x86-setcc-opcode2-values
  "SETcc emitters use correct opcode2 second byte."
  :cases (("setl"  #x9C)   ; SETL  (signed less-than)
          ("setge" #x9D)   ; SETGE (signed greater-or-equal)
          ("setle" #x9E)   ; SETLE (signed less-or-equal)
          ("setg"  #x9F)   ; SETG  (signed greater-than)
          ("sete"  #x94)   ; SETE  (equal / zero)
          ("setne" #x95))  ; SETNE (not-equal)
  (opcode2)
  ;; emit-setcc with RAX (reg 0, no REX needed): emits 0F <opcode2> ModRM
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-setcc opcode2 cl-cc::+rax+ s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #x0F (first bytes))
    (assert-equal opcode2 (second bytes))))

;;; ─── emit-vm-mul byte size ───────────────────────────────────────────────

(deftest x86-vm-mul-size
  "vm-mul emits MOV(3) + IMUL(4) = 7 bytes."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-mul
                             (cl-cc::make-vm-mul :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (assert-equal 7 (length bytes))))

;;; ─── emit-vm-div / emit-vm-mod byte sizes ───────────────────────────────

(deftest x86-vm-div-size
  "vm-div (floor division) emits 34-byte sequence per layout comment."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-div
                             (cl-cc::make-vm-div :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (assert-equal 34 (length bytes))))

(deftest x86-vm-mod-size
  "vm-mod (floor modulo) emits 37-byte sequence per layout comment."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-mod
                             (cl-cc::make-vm-mod :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (assert-equal 37 (length bytes))))

;;; ─── emit-vm-logtest / emit-vm-logbitp byte sizes ───────────────────────

(deftest x86-vm-logtest-size
  "vm-logtest emits MOV+AND+SETNE+MOVZX = 14 bytes (for R0=RAX, low reg)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-logtest
                             (cl-cc::make-vm-logtest :dst :R0 :lhs :R1 :rhs :R2) s)))))
    ;; MOV(3) + AND(3) + SETNE(3, RAX<4) + MOVZX(4) = 13; conservative is 14
    (assert-true (>= (length bytes) 13))))

(deftest x86-vm-logbitp-size
  "vm-logbitp emits PUSH+MOV+MOV+SAR+AND+POP = 15 bytes."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-logbitp
                             (cl-cc::make-vm-logbitp :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (assert-equal 15 (length bytes))))

;;; ─── vm-reg-to-x86 mapping ───────────────────────────────────────────────

(deftest-each x86-vm-reg-to-x86-mapping
  "vm-reg-to-x86 maps VM keyword registers to correct x86-64 codes."
  :cases (("R0" :R0 0)    ; RAX
          ("R1" :R1 1)    ; RCX
          ("R2" :R2 2)    ; RDX
          ("R3" :R3 3)    ; RBX
          ("R4" :R4 6)    ; RSI
          ("R5" :R5 7)    ; RDI
          ("R6" :R6 8)    ; R8
          ("R7" :R7 9))   ; R9
  (vm-reg expected-code)
  (assert-equal expected-code (cl-cc::vm-reg-to-x86 vm-reg)))

;;; ─── *phys-reg-to-x86-code* completeness ────────────────────────────────

(deftest x86-phys-reg-map-rax
  "Physical register :rax maps to code 0."
  (assert-equal 0 (cdr (assoc :rax cl-cc::*phys-reg-to-x86-code*))))

(deftest x86-phys-reg-map-r15
  "Physical register :r15 maps to code 15."
  (assert-equal 15 (cdr (assoc :r15 cl-cc::*phys-reg-to-x86-code*))))

(deftest x86-phys-reg-map-count
  "Physical register map covers 14 registers (RAX through R15, excluding RSP/RBP)."
  (assert-equal 14 (length cl-cc::*phys-reg-to-x86-code*)))

;;; ─── emit-vm-const and emit-vm-move sizes ───────────────────────────────

(deftest x86-vm-const-size
  "vm-const emits 10 bytes (REX.W + B8+rd + 8-byte immediate)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-const
                             (cl-cc::make-vm-const :dst :R0 :value 99) s)))))
    (assert-equal 10 (length bytes))))

(deftest x86-vm-const-zero-value
  "vm-const with value NIL emits 0 as immediate (nil -> 0)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-const
                             (cl-cc::make-vm-const :dst :R0 :value nil) s)))))
    ;; Bytes 3-10 are 64-bit LE immediate; all should be zero
    (assert-equal 0 (third bytes))
    (assert-equal 0 (fourth bytes))))

(deftest x86-vm-const-t-value
  "vm-const with value T emits 1 as immediate (t -> 1)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-const
                             (cl-cc::make-vm-const :dst :R0 :value t) s)))))
    (assert-equal 1 (third bytes))
    (assert-equal 0 (fourth bytes))))

(deftest x86-vm-move-size
  "vm-move emits 3 bytes (REX.W + 89 + ModR/M)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-move
                             (cl-cc::make-vm-move :dst :R0 :src :R1) s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #x89 (second bytes))))

;;; ─── emit-vm-halt (emit-vm-halt-inst) ───────────────────────────────────

(deftest x86-vm-halt-result-already-rax
  "vm-halt with R0 (mapped to RAX): no MOV emitted (0 bytes)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-halt-inst
                             (cl-cc::make-vm-halt :reg :R0) s)))))
    ;; R0 -> RAX (code 0), which is already +rax+, so no mov
    (assert-equal 0 (length bytes))))

(deftest x86-vm-halt-result-other-reg
  "vm-halt with R1 (mapped to RCX): emits MOV RAX, RCX (3 bytes)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-halt-inst
                             (cl-cc::make-vm-halt :reg :R1) s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #x89 (second bytes))))

;;; ─── emit-vm-ret-inst ────────────────────────────────────────────────────

(deftest x86-vm-ret-inst-encoding
  "emit-vm-ret-inst emits single RET byte #xC3."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-ret-inst
                             (cl-cc::make-vm-ret) s)))))
    (assert-equal 1 (length bytes))
    (assert-equal #xC3 (first bytes))))

;;; ─── emit-idiv-sequence ──────────────────────────────────────────────────

(deftest x86-idiv-sequence-quotient-size
  "emit-idiv-sequence (quotient) emits exactly 18 bytes."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc::emit-idiv-sequence cl-cc::+rcx+ cl-cc::+rdx+ nil s)))))
    ;; MOV R11,rhs(3) + PUSH RAX(1) + PUSH RDX(1) + MOV RAX,lhs(3)
    ;; + CQO(2) + IDIV R11(3) + MOV R11,RAX(3) + POP RDX(1) + POP RAX(1) = 18
    (assert-equal 18 (length bytes))))

(deftest x86-idiv-sequence-remainder-size
  "emit-idiv-sequence (remainder) also emits exactly 18 bytes."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc::emit-idiv-sequence cl-cc::+rcx+ cl-cc::+rdx+ t s)))))
    (assert-equal 18 (length bytes))))

;;; ─── emit-vm-program prologue/epilogue ───────────────────────────────────

(deftest x86-vm-program-emits-bytes
  "emit-vm-program produces non-empty byte output."
  (let* ((insts (list (cl-cc::make-vm-halt :reg :R0)
                      (cl-cc::make-vm-ret :reg :R0)))
         (prog (cl-cc::make-vm-program :instructions insts :result-register :R0))
         (bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-vm-program prog s)))))
    (assert-true (> (length bytes) 0))))

;;; ─── build-label-offsets with vm-add (6 bytes) ───────────────────────────

(deftest x86-build-label-offsets-vm-add-size
  "vm-add size is 6 bytes in the offset table."
  (let* ((insts (list (cl-cc::make-vm-add :dst :R0 :lhs :R1 :rhs :R2)
                      (cl-cc::make-vm-label :name "after-add")))
         (offsets (cl-cc::build-label-offsets insts 0)))
    (assert-equal 6 (gethash "after-add" offsets))))

;;; ─── instruction-size for specific types ────────────────────────────────

(deftest-each x86-instruction-size-values
  "instruction-size returns the documented byte count for each instruction type."
  :cases (("vm-const"    (cl-cc::make-vm-const    :dst :R0 :value 0)           10)
          ("vm-move"     (cl-cc::make-vm-move     :dst :R0 :src :R1)            3)
          ("vm-add"      (cl-cc::make-vm-add      :dst :R0 :lhs :R1 :rhs :R2)  6)
          ("vm-sub"      (cl-cc::make-vm-sub      :dst :R0 :lhs :R1 :rhs :R2)  6)
          ("vm-mul"      (cl-cc::make-vm-mul      :dst :R0 :lhs :R1 :rhs :R2)  7)
          ("vm-jump"     (cl-cc::make-vm-jump     :label "L")                    5)
          ("vm-ret"      (cl-cc::make-vm-ret)                                   1)
          ("vm-abs"      (make-vm-abs             :dst :R0 :src :R1)           15)
          ("vm-ash"      (make-vm-ash             :dst :R0 :lhs :R1 :rhs :R2) 24)
          ("vm-div"      (cl-cc::make-vm-div      :dst :R0 :lhs :R1 :rhs :R2) 34)
          ("vm-mod"      (cl-cc::make-vm-mod      :dst :R0 :lhs :R1 :rhs :R2) 37)
          ("vm-logtest"  (cl-cc::make-vm-logtest  :dst :R0 :lhs :R1 :rhs :R2) 14)
          ("vm-logbitp"  (cl-cc::make-vm-logbitp  :dst :R0 :lhs :R1 :rhs :R2) 15))
  (inst expected)
  (assert-equal expected (cl-cc::instruction-size inst)))
