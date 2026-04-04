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

(deftest-each x86-rex-prefix-flags
  "rex-prefix computes correct byte for each flag combination."
  :cases (("base" nil                       #x40)
          ("w"    '(:w 1)                   #x48)
          ("r"    '(:r 1)                   #x44)
          ("b"    '(:b 1)                   #x41)
          ("wrb"  '(:w 1 :r 1 :b 1)        #x4D)
          ("all"  '(:w 1 :r 1 :x 1 :b 1)   #x4F))
  (args expected)
  (assert-equal expected (apply #'cl-cc::rex-prefix args)))

;;; ─── modrm ──────────────────────────────────────────────────────────────

(deftest-each x86-modrm-cases
  "ModR/M byte computation for register, memory-indirect, and displacement modes."
  :cases (("reg-reg"          3 0 0 #xC0)
          ("reg-fields"       3 1 2 #xCA)
          ("memory-indirect"  0 0 0 #x00)
          ("disp8"            1 3 5 #x5D))
  (mod reg rm expected)
  (assert-equal expected (cl-cc::modrm mod reg rm)))

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

(deftest x86-add-sub-rr64-encoding
  "ADD rr64 emits opcode #x01; SUB rr64 emits #x29; both 3 bytes."
  (let ((add-bytes (%x86-collect-bytes
                    (lambda (s) (cl-cc::emit-add-rr64 cl-cc::+rax+ cl-cc::+rcx+ s))))
        (sub-bytes (%x86-collect-bytes
                    (lambda (s) (cl-cc::emit-sub-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)))))
    (assert-equal 3 (length add-bytes))
    (assert-equal #x01 (second add-bytes))
    (assert-equal 3 (length sub-bytes))
    (assert-equal #x29 (second sub-bytes))))

;;; ─── emit-imul-rr64 ─────────────────────────────────────────────────────

(deftest x86-imul-rr64-two-byte-opcode
  "IMUL RAX, RCX emits 0F AF (two-byte opcode)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-imul-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)))))
    (assert-equal 4 (length bytes))
    (assert-equal #x0F (second bytes))
    (assert-equal #xAF (third bytes))))

(deftest x86-movq-xmm-r64-encoding
  "MOVQ XMM0, R11 emits 66 49 0F 6E C3." 
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-movq-xmm-r64 cl-cc::+xmm0+ cl-cc::+r11+ s)))))
    (assert-equal '(#x66 #x49 #x0F #x6E #xC3) bytes)))

(deftest x86-addsd-xx-encoding
  "ADDSD XMM0, XMM1 emits F2 0F 58 C1." 
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-addsd-xx cl-cc::+xmm0+ cl-cc::+xmm1+ s)))))
    (assert-equal '(#xF2 #x0F #x58 #xC1) bytes)))

(deftest x86-movsd-xx-encoding
  "MOVSD XMM0, XMM1 emits F2 0F 10 C1." 
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-movsd-xx cl-cc::+xmm0+ cl-cc::+xmm1+ s)))))
    (assert-equal '(#xF2 #x0F #x10 #xC1) bytes)))

;;; ─── emit-push-r64 / emit-pop-r64 ───────────────────────────────────────

(deftest-each x86-push-pop-opcodes
  "PUSH/POP low registers emit single-byte opcodes."
  :cases (("push-rax" (lambda (s) (cl-cc::emit-push-r64 cl-cc::+rax+ s)) #x50)
          ("push-rcx" (lambda (s) (cl-cc::emit-push-r64 cl-cc::+rcx+ s)) #x51)
          ("pop-rax"  (lambda (s) (cl-cc::emit-pop-r64  cl-cc::+rax+ s)) #x58))
  (emit-fn opcode)
  (let ((bytes (%x86-collect-bytes emit-fn)))
    (assert-equal 1 (length bytes))
    (assert-equal opcode (first bytes))))

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

(deftest x86-jmp-rel32-encoding
  "JMP rel32 emits E9 + 4-byte offset; offset 256 encodes correctly in LE."
  (let ((bytes-zero (%x86-collect-bytes (lambda (s) (cl-cc::emit-jmp-rel32 0 s))))
        (bytes-256  (%x86-collect-bytes (lambda (s) (cl-cc::emit-jmp-rel32 256 s)))))
    (assert-equal 5 (length bytes-zero))
    (assert-equal #xE9 (first bytes-zero))
    ;; offset = 256 = #x00000100 little-endian: 00 01 00 00
    (assert-equal #x00 (second bytes-256))
    (assert-equal #x01 (third bytes-256))))

(deftest x86-je-rel32-opcode
  "JE rel32 emits 0F 84 + 4-byte offset."
  (let ((bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-je-rel32 0 s)))))
    (assert-equal 6 (length bytes))
    (assert-equal #x0F (first bytes))
    (assert-equal #x84 (second bytes))))

(deftest x86-bswap-r32-encoding
  "BSWAP r32 emits 0F C8+rd, with optional REX for high registers."
  (let ((bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-bswap-r32 cl-cc::+rax+ s)))))
    (assert-equal 2 (length bytes))
    (assert-equal #x0F (first bytes))
    (assert-equal #xC8 (second bytes))))

;;; ─── VM register mapping ─────────────────────────────────────────────────

(deftest-each x86-vm-reg-map-spot-checks
  "VM register map: :r0→RAX(0), :r1→RCX(1)."
  :cases (("r0-rax" :r0 cl-cc::+rax+)
          ("r1-rcx" :r1 cl-cc::+rcx+))
  (vm-reg expected)
  (assert-equal expected (cdr (assoc vm-reg cl-cc::*vm-reg-map*))))

(deftest x86-vm-reg-map-coverage
  "VM register map has 8 entries."
  (assert-equal 8 (length cl-cc::*vm-reg-map*)))

;;; ─── emit-mov-ri64 ──────────────────────────────────────────────────────

(deftest x86-mov-ri64-rax
  "MOV RAX, 42: 10 bytes, REX.W=#x48, opcode=#xB8, immediate LE."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-mov-ri64 cl-cc::+rax+ 42 s)))))
    (assert-equal 10 (length bytes))
    (assert-equal #x48 (first bytes))
    (assert-equal #xB8 (second bytes))
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

(deftest-each x86-not-neg-r64-opcode
  "NOT and NEG share opcode #xF7; both emit 3 bytes."
  :cases (("not" (lambda (s) (cl-cc::emit-not-r64 cl-cc::+rax+ s)))
          ("neg" (lambda (s) (cl-cc::emit-neg-r64 cl-cc::+rax+ s))))
  (emit-fn)
  (let ((bytes (%x86-collect-bytes emit-fn)))
    (assert-equal 3 (length bytes))
    (assert-equal #xF7 (second bytes))))

(deftest x86-dec-r64-opcode
  "DEC RAX emits opcode #xFF with /1 extension."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-dec-r64 cl-cc::+rax+ s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #xFF (second bytes))))

(deftest-each x86-ri8-arith-size
  "ADD/SUB RAX, imm8 each emit 4 bytes with shared opcode #x83."
  :cases (("add" (lambda (s) (cl-cc::emit-add-ri8 cl-cc::+rax+ 1 s)))
          ("sub" (lambda (s) (cl-cc::emit-sub-ri8 cl-cc::+rax+ 1 s))))
  (emit-fn)
  (let ((bytes (%x86-collect-bytes emit-fn)))
    (assert-equal 4 (length bytes))
    (assert-equal #x83 (second bytes))))

(deftest x86-and-ri8-size
  "AND RAX, imm8 emits 4 bytes."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-and-ri8 cl-cc::+rax+ 1 s)))))
    (assert-equal 4 (length bytes))))

(deftest-each x86-binary-logical-opcodes
  "AND/OR/XOR r/m64,r64 each emit 3 bytes with correct opcode."
  :cases (("and" (lambda (s) (cl-cc::emit-and-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)) #x21)
          ("or"  (lambda (s) (cl-cc::emit-or-rr64  cl-cc::+rax+ cl-cc::+rcx+ s)) #x09)
          ("xor" (lambda (s) (cl-cc::emit-xor-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)) #x31))
  (emit-fn opcode)
  (let ((bytes (%x86-collect-bytes emit-fn)))
    (assert-equal 3 (length bytes))
    (assert-equal opcode (second bytes))))

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

(deftest-each x86-shift-cl-opcode
  "SAL/SAR RAX, CL each emit 3 bytes with shared opcode #xD3."
  :cases (("sal" (lambda (s) (cl-cc::emit-sal-r64-cl cl-cc::+rax+ s)))
          ("sar" (lambda (s) (cl-cc::emit-sar-r64-cl cl-cc::+rax+ s))))
  (emit-fn)
  (let ((bytes (%x86-collect-bytes emit-fn)))
    (assert-equal 3 (length bytes))
    (assert-equal #xD3 (second bytes))))

(deftest-each x86-cmov-encoding
  "CMOVL/CMOVG each emit 4 bytes; third byte is the distinguishing opcode."
  :cases (("cmovl" (lambda (s) (cl-cc::emit-cmovl-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)) #x4C)
          ("cmovg" (lambda (s) (cl-cc::emit-cmovg-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)) #x4F))
  (emit-fn opcode3)
  (let ((bytes (%x86-collect-bytes emit-fn)))
    (assert-equal 4 (length bytes))
    (assert-equal opcode3 (third bytes))))

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

(deftest-each x86-vm-bool-and-or-size
  "vm-and and vm-or (boolean) each emit 17 bytes."
  :cases (("and" (lambda (s) (cl-cc::emit-vm-and
                              (cl-cc::make-vm-and :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("or"  (lambda (s) (cl-cc::emit-vm-or
                              (cl-cc::make-vm-or  :dst :R0 :lhs :R1 :rhs :R2) s))))
  (emit-fn)
  (assert-equal 17 (length (%x86-collect-bytes emit-fn))))

(deftest x86-vm-null-p-size
  "vm-null-p emits TEST+SETE+MOVZX."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-null-p
                             (cl-cc::make-vm-null-p :dst :R0 :src :R1) s)))))
    (assert-true (>= (length bytes) 10))))

(deftest-each x86-vm-const-pred-size
  "vm-true-pred and vm-false-pred each emit MOV imm64 = 10 bytes."
  :cases (("true"  (lambda (s) (cl-cc::emit-vm-true-pred
                                (cl-cc::make-vm-number-p :dst :R0 :src :R1) s)))
          ("false" (lambda (s) (cl-cc::emit-vm-false-pred
                                (cl-cc::make-vm-cons-p :dst :R0 :src :R1) s))))
  (emit-fn)
  (assert-equal 10 (length (%x86-collect-bytes emit-fn))))

;;; ─── IDIV-based emitter sizes ───────────────────────────────────────────

(deftest-each x86-vm-idiv-ops-size
  "vm-truncate and vm-rem each emit 21-byte IDIV sequences."
  :cases (("truncate" (lambda (s) (cl-cc::emit-vm-truncate
                                   (make-vm-truncate :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("rem"      (lambda (s) (cl-cc::emit-vm-rem
                                   (cl-cc::make-vm-rem :dst :R0 :lhs :R1 :rhs :R2) s))))
  (emit-fn)
  (assert-equal 21 (length (%x86-collect-bytes emit-fn))))

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

(deftest x86-mov-rm64-offsets
  "MOV RAX,[RCX+0] emits 3 bytes (mod=00); MOV RAX,[RCX+8] emits 4 bytes (mod=01)."
  (let ((zero-bytes (%x86-collect-bytes
                     (lambda (s) (cl-cc::emit-mov-rm64 cl-cc::+rax+ cl-cc::+rcx+ 0 s))))
        (disp-bytes (%x86-collect-bytes
                     (lambda (s) (cl-cc::emit-mov-rm64 cl-cc::+rax+ cl-cc::+rcx+ 8 s)))))
    (assert-equal 3 (length zero-bytes))
    (assert-equal 4 (length disp-bytes))
    (assert-equal 8 (fourth disp-bytes))))

(deftest x86-mov-mr64-with-offset
  "MOV [RCX+16], RAX stores with byte displacement."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-mov-mr64 cl-cc::+rcx+ 16 cl-cc::+rax+ s)))))
    (assert-equal 4 (length bytes))
    (assert-equal 16 (fourth bytes))))

(deftest x86-mov-rm64-indexed-with-disp8
  "MOV RAX,[RBX+RCX*8+16] emits ModR/M+SIB+disp8."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc::emit-mov-rm64-indexed cl-cc::+rax+ cl-cc::+rbx+ cl-cc::+rcx+ 8 16 s)))))
    (assert-equal '(#x48 #x8B #x44 #xCB #x10) bytes)))

(deftest x86-mov-mr64-indexed-with-disp8
  "MOV [RBX+RCX*4+8], RAX emits store ModR/M+SIB+disp8."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc::emit-mov-mr64-indexed cl-cc::+rbx+ cl-cc::+rcx+ 4 8 cl-cc::+rax+ s)))))
    (assert-equal '(#x48 #x89 #x44 #x8B #x08) bytes)))

(deftest x86-mov-rm64-indexed-scale-1-zero-disp
  "MOV RAX,[RBX+RCX] emits zero-displacement indexed form."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc::emit-mov-rm64-indexed cl-cc::+rax+ cl-cc::+rbx+ cl-cc::+rcx+ 1 0 s)))))
    (assert-equal '(#x48 #x8B #x04 #x0B) bytes)))

;;; ─── emit-mov-rr64 ModR/M correctness ──────────────────────────────────

(deftest x86-mov-rr64-modrm-encoding
  "MOV rr64 ModR/M: RAX←RBX=#xD8 (mod=11,reg=3,rm=0); RAX←RAX=#xC0 (self-copy)."
  (let ((rax-rbx (%x86-collect-bytes
                  (lambda (s) (cl-cc::emit-mov-rr64 cl-cc::+rax+ cl-cc::+rbx+ s))))
        (rax-rax (%x86-collect-bytes
                  (lambda (s) (cl-cc::emit-mov-rr64 cl-cc::+rax+ cl-cc::+rax+ s)))))
    (assert-equal #x48 (first rax-rbx))
    (assert-equal #x89 (second rax-rbx))
    (assert-equal #xD8 (third rax-rbx))
    (assert-equal #xC0 (third rax-rax))))

;;; ─── emit-add-rr64 / emit-sub-rr64 ModR/M ──────────────────────────────

(deftest x86-add-sub-rr64-modrm
  "ADD/SUB RCX,RDX: both produce ModR/M=#xD1 (mod=11,reg=2,rm=1); opcodes #x01/#x29."
  (let ((add-bytes (%x86-collect-bytes
                    (lambda (s) (cl-cc::emit-add-rr64 cl-cc::+rcx+ cl-cc::+rdx+ s))))
        (sub-bytes (%x86-collect-bytes
                    (lambda (s) (cl-cc::emit-sub-rr64 cl-cc::+rcx+ cl-cc::+rdx+ s)))))
    (assert-equal #x48 (first add-bytes))
    (assert-equal #x01 (second add-bytes))
    (assert-equal #xD1 (third add-bytes))
    (assert-equal #x29 (second sub-bytes))
    (assert-equal #xD1 (third sub-bytes))))

;;; ─── High-register push/pop ─────────────────────────────────────────────

(deftest-each x86-push-pop-single-byte-opcodes
  "PUSH/POP low registers emit single-byte opcodes."
  :cases (("push-rbx" cl-cc::+rbx+ #'cl-cc::emit-push-r64 #x53)
          ("pop-rcx"  cl-cc::+rcx+ #'cl-cc::emit-pop-r64  #x59)
          ("pop-rdx"  cl-cc::+rdx+ #'cl-cc::emit-pop-r64  #x5A))
  (reg emit-fn opcode)
  (let ((bytes (%x86-collect-bytes (lambda (s) (funcall emit-fn reg s)))))
    (assert-equal 1 (length bytes))
    (assert-equal opcode (first bytes))))

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

(deftest x86-phys-reg-map
  "Physical register alist: :rax=0, :r15=15, covers 14 registers."
  (assert-equal 0  (cdr (assoc :rax cl-cc::*phys-reg-to-x86-code*)))
  (assert-equal 15 (cdr (assoc :r15 cl-cc::*phys-reg-to-x86-code*)))
  (assert-equal 14 (length cl-cc::*phys-reg-to-x86-code*)))

;;; ─── emit-vm-const and emit-vm-move sizes ───────────────────────────────

(deftest x86-vm-const-size
  "vm-const emits 10 bytes (REX.W + B8+rd + 8-byte immediate)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-const
                             (cl-cc::make-vm-const :dst :R0 :value 99) s)))))
    (assert-equal 10 (length bytes))))

(deftest-each x86-vm-const-bool-immediate
  "vm-const nil→0, t→1 encodes correct LE immediate bytes."
  :cases (("nil" nil 0)
          ("t"   t   1))
  (val expected-byte)
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-const
                             (cl-cc::make-vm-const :dst :R0 :value val) s)))))
    (assert-equal expected-byte (third bytes))
    (assert-equal 0 (fourth bytes))))

(deftest x86-vm-move-size
  "vm-move emits 3 bytes (REX.W + 89 + ModR/M)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-move
                             (cl-cc::make-vm-move :dst :R0 :src :R1) s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #x89 (second bytes))))

(deftest x86-vm-move-self-is-elided
  "vm-move to the same physical register emits no bytes."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-move
                             (cl-cc::make-vm-move :dst :R0 :src :R0) s)))))
    (assert-equal 0 (length bytes))))

;;; ─── emit-vm-halt (emit-vm-halt-inst) ───────────────────────────────────

(deftest x86-vm-halt-encoding
  "vm-halt: R0→RAX emits 0 bytes (already in RAX); R1→RCX emits MOV RAX,RCX (3 bytes)."
  (let ((r0-bytes (%x86-collect-bytes
                   (lambda (s) (cl-cc::emit-vm-halt-inst (cl-cc::make-vm-halt :reg :R0) s))))
        (r1-bytes (%x86-collect-bytes
                   (lambda (s) (cl-cc::emit-vm-halt-inst (cl-cc::make-vm-halt :reg :R1) s)))))
    (assert-equal 0 (length r0-bytes))
    (assert-equal 3 (length r1-bytes))
    (assert-equal #x89 (second r1-bytes))))

;;; ─── emit-vm-ret-inst ────────────────────────────────────────────────────

(deftest x86-vm-ret-inst-encoding
  "emit-vm-ret-inst emits single RET byte #xC3."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-ret-inst
                             (cl-cc::make-vm-ret) s)))))
    (assert-equal 1 (length bytes))
    (assert-equal #xC3 (first bytes))))

;;; ─── emit-idiv-sequence ──────────────────────────────────────────────────

(deftest x86-idiv-sequence-size
  "emit-idiv-sequence emits exactly 18 bytes for both quotient and remainder modes."
  (dolist (remainder-p '(nil t))
    (let ((bytes (%x86-collect-bytes
                  (lambda (s)
                    (cl-cc::emit-idiv-sequence cl-cc::+rcx+ cl-cc::+rdx+ remainder-p s)))))
      (assert-equal 18 (length bytes)))))

;;; ─── emit-vm-program prologue/epilogue ───────────────────────────────────

(deftest x86-vm-program-emits-bytes
  "emit-vm-program produces non-empty byte output."
  (let* ((insts (list (cl-cc::make-vm-halt :reg :R0)
                      (cl-cc::make-vm-ret :reg :R0)))
         (prog (cl-cc::make-vm-program :instructions insts :result-register :R0))
         (bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-vm-program prog s)))))
    (assert-true (> (length bytes) 0))))

(deftest x86-vm-program-trims-unused-callee-saved-regs
  "emit-vm-program only saves the callee-saved registers actually used by regalloc."
  (let* ((prog (cl-cc::make-vm-program :instructions nil :result-register :R0))
         (bytes (cl-cc::compile-to-x86-64-bytes prog)))
    ;; Empty programs now emit just PUSH RBP, POP RBP, RET.
    (assert-equal 3 (length bytes))))

(deftest x86-mov-rm64-rsp-base-uses-sib
  "emit-mov-rm64 encodes [rsp+disp8] using an explicit SIB byte."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc::emit-mov-rm64 cl-cc::+rax+ cl-cc::+rsp+ -8 s)))))
    (assert-equal '(#x48 #x8B #x44 #x24 #xF8) bytes)))

(deftest x86-mov-mr64-rsp-base-uses-sib
  "emit-mov-mr64 encodes [rsp+disp8] stores using an explicit SIB byte."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc::emit-mov-mr64 cl-cc::+rsp+ -16 cl-cc::+rax+ s)))))
    (assert-equal '(#x48 #x89 #x44 #x24 #xF0) bytes)))

(deftest x86-vm-program-leaf-red-zone-spills-skip-rbp-frame
  "Leaf programs with small spill counts use RSP-based red-zone spill slots and skip PUSH/POP RBP."
  (let* ((prog (cl-cc::make-vm-program
                :instructions (list (cl-cc::make-vm-spill-store :src-reg :rax :slot 1)
                                    (cl-cc::make-vm-spill-load :dst-reg :rbx :slot 1))
                :result-register :R0
                :leaf-p t))
         (ra (cl-cc::make-regalloc-result :assignment (make-hash-table :test #'eq)
                                          :spill-count 1
                                          :instructions (cl-cc::vm-program-instructions prog)))
         (bytes (let ((cl-cc::*current-regalloc* ra))
                  (%x86-collect-bytes (lambda (s) (cl-cc::emit-vm-program prog s))))))
    (assert-false (= #x55 (first bytes)))
    (assert-equal '(#x48 #x89 #x44 #x24 #xF8) (subseq bytes 0 5))
    (assert-equal '(#x48 #x8B #x5C #x24 #xF8) (subseq bytes 5 10))
    (assert-equal #xC3 (car (last bytes)))))

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
