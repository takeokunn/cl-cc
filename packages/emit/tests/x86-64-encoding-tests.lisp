;;;; tests/unit/emit/x86-64-encoding-tests.lisp — x86-64 Instruction Encoding Tests
;;;;
;;;; Tests for src/emit/x86-64-codegen.lisp encoding helpers:
;;;; rex-prefix, modrm, emit-byte, emit-dword, emit-qword,
;;;; emit-mov-rr64, emit-add-rr64, emit-sub-rr64, emit-imul-rr64,
;;;; emit-mul-rm64, emit-imul-rm64,
;;;; emit-push-r64, emit-pop-r64, emit-ret, emit-jmp-rel32, emit-je-rel32.
;;;; VM emitter byte sizes → x86-64-vm-emitter-tests.lisp.

(in-package :cl-cc/test)

(defsuite x86-64-encoding-suite :description "x86-64 instruction encoding unit tests"
  :parent cl-cc-unit-suite)

(in-suite x86-64-encoding-suite)
;;; Helper: collect bytes emitted by a function
(defun %x86-encoding-collect-bytes (emit-fn)
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
  (assert-equal expected (apply #'cl-cc/codegen::rex-prefix args)))

;;; ─── modrm ──────────────────────────────────────────────────────────────

(deftest-each x86-modrm-cases
  "ModR/M byte computation for register, memory-indirect, and displacement modes."
  :cases (("reg-reg"          3 0 0 #xC0)
          ("reg-fields"       3 1 2 #xCA)
          ("memory-indirect"  0 0 0 #x00)
          ("disp8"            1 3 5 #x5D))
  (mod reg rm expected)
  (assert-equal expected (cl-cc/codegen::modrm mod reg rm)))

;;; ─── emit-byte / emit-dword / emit-qword ────────────────────────────────

(deftest-each x86-emit-byte-cases
  "emit-byte writes a single byte and masks to 8 bits."
  :cases (("normal-value" #xAB #xAB)
          ("mask-to-8bit" #x1FF #xFF))
  (input expected)
  (let ((bytes (%x86-encoding-collect-bytes (lambda (s) (cl-cc/codegen::emit-byte input s)))))
    (assert-equal 1 (length bytes))
    (assert-equal expected (first bytes))))

(deftest-each x86-emit-multi-byte-le
  "emit-dword/emit-qword write little-endian bytes."
  :cases (("dword" (lambda (s) (cl-cc/codegen::emit-dword #xDEADBEEF s))
                   '(#xEF #xBE #xAD #xDE))
          ("qword" (lambda (s) (cl-cc/codegen::emit-qword #x0102030405060708 s))
                   '(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01)))
  (emit-fn expected-bytes)
  (assert-equal expected-bytes (%x86-encoding-collect-bytes emit-fn)))

;;; ─── emit-add-rr64 / emit-sub-rr64 ──────────────────────────────────────

(deftest-each x86-add-sub-rr64-encoding
  "ADD/SUB rr64 emit an exact 3-byte REX+opcode+ModRM sequence."
  :cases (("add-rax-rcx" (lambda (s) (cl-cc/codegen::emit-add-rr64 cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s))
           '(#x48 #x01 #xC8))
          ("sub-rax-rcx" (lambda (s) (cl-cc/codegen::emit-sub-rr64 cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s))
           '(#x48 #x29 #xC8))
          ("add-rcx-rdx" (lambda (s) (cl-cc/codegen::emit-add-rr64 cl-cc/codegen::+rcx+ cl-cc/codegen::+rdx+ s))
           '(#x48 #x01 #xD1))
          ("sub-rcx-rdx" (lambda (s) (cl-cc/codegen::emit-sub-rr64 cl-cc/codegen::+rcx+ cl-cc/codegen::+rdx+ s))
           '(#x48 #x29 #xD1)))
  (emit-fn expected-bytes)
  (assert-equal expected-bytes (%x86-encoding-collect-bytes emit-fn)))

(deftest x86-mov-memory-displacement-widths
  "MOV memory operands use disp8 for small offsets and disp32 for larger RSP-relative FPE slots."
  (let ((small-store (%x86-encoding-collect-bytes
                      (lambda (s)
                        (cl-cc/codegen::emit-mov-mr64 cl-cc/codegen::+rsp+ 120 cl-cc/codegen::+rax+ s))))
        (large-store (%x86-encoding-collect-bytes
                      (lambda (s)
                        (cl-cc/codegen::emit-mov-mr64 cl-cc/codegen::+rsp+ 248 cl-cc/codegen::+rax+ s))))
        (zero-rbp-load (%x86-encoding-collect-bytes
                        (lambda (s)
                          (cl-cc/codegen::emit-mov-rm64 cl-cc/codegen::+rax+ cl-cc/codegen::+rbp+ 0 s)))))
    (assert-equal '(#x48 #x89 #x44 #x24 #x78) small-store)
    (assert-equal '(#x48 #x89 #x84 #x24 #xF8 #x00 #x00 #x00) large-store)
    (assert-equal '(#x48 #x8B #x45 #x00) zero-rbp-load)))

;;; ─── emit-imul-rr64 / two-byte opcodes ──────────────────────────────────

(deftest-each x86-two-byte-opcode-instructions
  "REX+0F+opcode2+ModRM: 4 bytes, second=0F, third=opcode2."
  :cases (("imul-rr64"    (lambda (s) (cl-cc/codegen::emit-imul-rr64 cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)) #xAF)
          ("movzx-r64-r8" (lambda (s) (cl-cc/codegen::emit-movzx-r64-r8 cl-cc/codegen::+rax+ cl-cc/codegen::+rax+ s)) #xB6))
  (emit-fn opcode2)
  (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (assert-equal 4 (length bytes))
    (assert-equal #x0F (second bytes))
    (assert-equal opcode2 (third bytes))))

(deftest x86-mul-rm64-high-encodings
  "One-operand MUL/IMUL for multiply-high use F7 /4 and F7 /5 with R11 as the source register."
  (let ((mul-bytes (%x86-encoding-collect-bytes
                    (lambda (s)
                      (cl-cc/codegen::emit-mul-rm64 cl-cc/codegen::+r11+ s))))
        (imul-bytes (%x86-encoding-collect-bytes
                     (lambda (s)
                       (cl-cc/codegen::emit-imul-rm64 cl-cc/codegen::+r11+ s)))))
    (assert-equal '(#x49 #xF7 #xE3) mul-bytes)
    (assert-equal '(#x49 #xF7 #xEB) imul-bytes)))

(deftest-each x86-xmm-instruction-encoding
  "XMM/SSE instruction encodings produce exact byte sequences."
  :cases (("movq-xmm0-r11" (lambda (s) (cl-cc/codegen::emit-movq-xmm-r64 cl-cc/codegen::+xmm0+ cl-cc/codegen::+r11+ s))
           '(#x66 #x49 #x0F #x6E #xC3))
          ("addsd-xmm0-xmm1" (lambda (s) (cl-cc/codegen::emit-addsd-xx cl-cc/codegen::+xmm0+ cl-cc/codegen::+xmm1+ s))
           '(#xF2 #x0F #x58 #xC1))
           ("movsd-xmm0-xmm1" (lambda (s) (cl-cc/codegen::emit-movsd-xx cl-cc/codegen::+xmm0+ cl-cc/codegen::+xmm1+ s))
            '(#xF2 #x0F #x10 #xC1))
           ("sqrtsd-xmm0-xmm1" (lambda (s) (cl-cc/codegen::emit-sqrtsd-xx cl-cc/codegen::+xmm0+ cl-cc/codegen::+xmm1+ s))
            '(#xF2 #x0F #x51 #xC1)))
  (emit-fn expected-bytes)
  (assert-equal expected-bytes (%x86-encoding-collect-bytes emit-fn)))

;;; ─── emit-push-r64 / emit-pop-r64 / emit-ret / emit-vm-ret-inst ─────────

(deftest-each x86-ret-encoding
  "Both emit-ret and emit-vm-ret-inst produce a single #xC3 byte."
  :cases (("emit-ret"      (lambda (s) (cl-cc/codegen::emit-ret s)))
          ("vm-ret-inst"   (lambda (s) (cl-cc/codegen::emit-vm-ret-inst (cl-cc:make-vm-ret) s))))
  (emit-fn)
  (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (assert-equal 1 (length bytes))
    (assert-equal #xC3 (first bytes))))

(deftest x86-jmp-rel32-le-offset
  "JMP rel32 encodes offset 256 = #x100 as LE bytes [#x00 #x01 #x00 #x00] starting at byte 2."
  (let ((bytes (%x86-encoding-collect-bytes (lambda (s) (cl-cc/codegen::emit-jmp-rel32 256 s)))))
    (assert-equal #x00 (second bytes))
    (assert-equal #x01 (third bytes))))

(deftest-each x86-fixed-encoding-spot-checks
  "Fixed-format instructions: length, first byte, second byte."
  :cases (("jmp-zero"   (lambda (s) (cl-cc/codegen::emit-jmp-rel32 0 s))              5 #xE9 #x00)
          ("bswap-rax"  (lambda (s) (cl-cc/codegen::emit-bswap-r32 cl-cc/codegen::+rax+ s))  2 #x0F #xC8)
          ("jge-short"  (lambda (s) (cl-cc/codegen::emit-jge-short 3 s))              2 #x7D    3)
          ("idiv-r11"   #'cl-cc/codegen::emit-idiv-r11                                3 #x49 #xF7)
          ("cqo"        #'cl-cc/codegen::emit-cqo                                     2 #x48 #x99)
          ("je-rel32"   (lambda (s) (cl-cc/codegen::emit-je-rel32 0 s))               6 #x0F #x84)
          ("jne-rel32"  (lambda (s) (cl-cc/codegen::emit-byte #x0F s)
                          (cl-cc/codegen::emit-byte #x85 s)
                          (cl-cc/codegen::emit-dword 0 s))                            6 #x0F #x85)
          ("cmp-rax-0"  (lambda (s) (cl-cc/codegen::emit-cmp-ri64 cl-cc/codegen::+rax+ 0 s)) 7 #x48 #x81)
          ("mov-r8-r9"  (lambda (s) (cl-cc/codegen::emit-mov-rr64 cl-cc/codegen::+r8+ cl-cc/codegen::+r9+ s))
                                                                              3 #x4D #x89)
          ("not-rax"    (lambda (s) (cl-cc/codegen::emit-not-r64    cl-cc/codegen::+rax+ s)) 3 #x48 #xF7)
          ("neg-rax"    (lambda (s) (cl-cc/codegen::emit-neg-r64    cl-cc/codegen::+rax+ s)) 3 #x48 #xF7)
          ("dec-rax"    (lambda (s) (cl-cc/codegen::emit-dec-r64    cl-cc/codegen::+rax+ s)) 3 #x48 #xFF)
          ("cmp-rr64"   (lambda (s) (cl-cc/codegen::emit-cmp-rr64   cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)) 3 #x48 #x39)
          ("test-rr64"  (lambda (s) (cl-cc/codegen::emit-test-rr64  cl-cc/codegen::+rax+ cl-cc/codegen::+rax+ s)) 3 #x48 #x85)
          ("and-rr64"   (lambda (s) (cl-cc/codegen::emit-and-rr64   cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)) 3 #x48 #x21)
          ("or-rr64"    (lambda (s) (cl-cc/codegen::emit-or-rr64    cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)) 3 #x48 #x09)
          ("xor-rr64"   (lambda (s) (cl-cc/codegen::emit-xor-rr64   cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)) 3 #x48 #x31)
          ("sal-cl"     (lambda (s) (cl-cc/codegen::emit-sal-r64-cl cl-cc/codegen::+rax+ s)) 3 #x48 #xD3)
          ("sar-cl"     (lambda (s) (cl-cc/codegen::emit-sar-r64-cl cl-cc/codegen::+rax+ s)) 3 #x48 #xD3)
          ("add-ri8"    (lambda (s) (cl-cc/codegen::emit-add-ri8    cl-cc/codegen::+rax+ 1 s)) 4 #x48 #x83)
          ("sub-ri8"    (lambda (s) (cl-cc/codegen::emit-sub-ri8    cl-cc/codegen::+rax+ 1 s)) 4 #x48 #x83)
          ("and-ri8"    (lambda (s) (cl-cc/codegen::emit-and-ri8    cl-cc/codegen::+rax+ 1 s)) 4 #x48 #x83))
  (emit-fn expected-len byte0 byte1)
  (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (assert-equal expected-len (length bytes))
    (assert-equal byte0 (first bytes))
    (assert-equal byte1 (second bytes))))

;;; ─── VM register mapping ─────────────────────────────────────────────────

(deftest x86-vm-reg-map
  "VM register map has 8 entries; :r0→RAX, :r1→RCX spot checks pass."
  (assert-equal 8 (length cl-cc/codegen::*vm-reg-map*))
  (assert-equal cl-cc/codegen::+rax+ (cdr (assoc :r0 cl-cc/codegen::*vm-reg-map*)))
  (assert-equal cl-cc/codegen::+rcx+ (cdr (assoc :r1 cl-cc/codegen::*vm-reg-map*))))

;;; ─── emit-mov-ri64 ──────────────────────────────────────────────────────

(deftest-each x86-mov-ri64-encoding
  "emit-mov-ri64: always 10 bytes; REX byte, opcode, and immediate vary by register."
  :cases (("rax-42" cl-cc/codegen::+rax+ 42 #x48 #xB8 42)
          ("rcx-0"  cl-cc/codegen::+rcx+  0 #x48 #xB9  0)
          ("r8-1"   cl-cc/codegen::+r8+   1 #x49 #xB8  1))
  (reg imm rex opcode imm-byte)
  (let ((bytes (%x86-encoding-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-mov-ri64 reg imm s)))))
    (assert-equal 10 (length bytes))
    (assert-equal rex    (first bytes))
    (assert-equal opcode (second bytes))
    (assert-equal imm-byte (third bytes))))

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
  (assert-equal expected (cl-cc/codegen::vm-const-to-integer input)))

(deftest-each x86-setcc-register-size
  "SETE on low register emits 3 bytes; high register (RSI=6) emits 4 bytes (REX added)."
  :cases (("low-reg-rax"  cl-cc/codegen::+rax+ 3)
          ("high-reg-rsi" cl-cc/codegen::+rsi+ 4))
  (reg expected-len)
  (let ((bytes (%x86-encoding-collect-bytes
                (lambda (s) (cl-cc/codegen::emit-setcc #x94 reg s)))))
    (assert-equal expected-len (length bytes))))

(deftest-each x86-cmov-encoding
  "CMOVL/CMOVG each emit 4 bytes; third byte is the distinguishing opcode."
  :cases (("cmovl" (lambda (s) (cl-cc/codegen::emit-cmovl-rr64 cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)) #x4C)
          ("cmovg" (lambda (s) (cl-cc/codegen::emit-cmovg-rr64 cl-cc/codegen::+rax+ cl-cc/codegen::+rcx+ s)) #x4F))
  (emit-fn opcode3)
  (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (assert-equal 4 (length bytes))
    (assert-equal opcode3 (third bytes))))
