;;;; tests/unit/emit/x86-64-encoding-tests.lisp — x86-64 Instruction Encoding Tests
;;;;
;;;; Tests for src/emit/x86-64-codegen.lisp encoding helpers:
;;;; rex-prefix, modrm, emit-byte, emit-dword, emit-qword,
;;;; emit-mov-rr64, emit-add-rr64, emit-sub-rr64, emit-imul-rr64,
;;;; emit-push-r64, emit-pop-r64, emit-ret, emit-jmp-rel32, emit-je-rel32.

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
  (assert-equal expected (apply #'cl-cc/emit::rex-prefix args)))

;;; ─── modrm ──────────────────────────────────────────────────────────────

(deftest-each x86-modrm-cases
  "ModR/M byte computation for register, memory-indirect, and displacement modes."
  :cases (("reg-reg"          3 0 0 #xC0)
          ("reg-fields"       3 1 2 #xCA)
          ("memory-indirect"  0 0 0 #x00)
          ("disp8"            1 3 5 #x5D))
  (mod reg rm expected)
  (assert-equal expected (cl-cc/emit::modrm mod reg rm)))

;;; ─── emit-byte / emit-dword / emit-qword ────────────────────────────────

(deftest-each x86-emit-byte-cases
  "emit-byte writes a single byte and masks to 8 bits."
  :cases (("normal-value" #xAB #xAB)
          ("mask-to-8bit" #x1FF #xFF))
  (input expected)
  (let ((bytes (%x86-encoding-collect-bytes (lambda (s) (cl-cc/emit::emit-byte input s)))))
    (assert-equal 1 (length bytes))
    (assert-equal expected (first bytes))))

(deftest x86-emit-multi-byte-le
  "emit-dword writes 4 LE bytes; emit-qword writes 8 LE bytes."
  (let ((dw (%x86-encoding-collect-bytes (lambda (s) (cl-cc/emit::emit-dword #xDEADBEEF s))))
        (qw (%x86-encoding-collect-bytes (lambda (s) (cl-cc/emit::emit-qword #x0102030405060708 s)))))
    (assert-equal 4 (length dw))
    (assert-equal '(#xEF #xBE #xAD #xDE) dw)
    (assert-equal 8 (length qw))
    (assert-equal #x08 (first qw))
    (assert-equal #x07 (second qw))
    (assert-equal #x01 (eighth qw))))

;;; ─── emit-add-rr64 / emit-sub-rr64 ──────────────────────────────────────

(deftest-each x86-add-sub-rr64-encoding
  "ADD/SUB rr64 emit an exact 3-byte REX+opcode+ModRM sequence."
  :cases (("add-rax-rcx" (lambda (s) (cl-cc/emit::emit-add-rr64 cl-cc/emit::+rax+ cl-cc/emit::+rcx+ s))
           '(#x48 #x01 #xC8))
          ("sub-rax-rcx" (lambda (s) (cl-cc/emit::emit-sub-rr64 cl-cc/emit::+rax+ cl-cc/emit::+rcx+ s))
           '(#x48 #x29 #xC8))
          ("add-rcx-rdx" (lambda (s) (cl-cc/emit::emit-add-rr64 cl-cc/emit::+rcx+ cl-cc/emit::+rdx+ s))
           '(#x48 #x01 #xD1))
          ("sub-rcx-rdx" (lambda (s) (cl-cc/emit::emit-sub-rr64 cl-cc/emit::+rcx+ cl-cc/emit::+rdx+ s))
           '(#x48 #x29 #xD1)))
  (emit-fn expected-bytes)
  (assert-equal expected-bytes (%x86-encoding-collect-bytes emit-fn)))

;;; ─── emit-imul-rr64 / two-byte opcodes ──────────────────────────────────

(deftest-each x86-two-byte-opcode-instructions
  "REX+0F+opcode2+ModRM: 4 bytes, second=0F, third=opcode2."
  :cases (("imul-rr64"    (lambda (s) (cl-cc/emit::emit-imul-rr64 cl-cc/emit::+rax+ cl-cc/emit::+rcx+ s)) #xAF)
          ("movzx-r64-r8" (lambda (s) (cl-cc/emit::emit-movzx-r64-r8 cl-cc/emit::+rax+ cl-cc/emit::+rax+ s)) #xB6))
  (emit-fn opcode2)
  (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (assert-equal 4 (length bytes))
    (assert-equal #x0F (second bytes))
    (assert-equal opcode2 (third bytes))))

(deftest-each x86-xmm-instruction-encoding
  "XMM/SSE instruction encodings produce exact byte sequences."
  :cases (("movq-xmm0-r11" (lambda (s) (cl-cc/emit::emit-movq-xmm-r64 cl-cc/emit::+xmm0+ cl-cc/emit::+r11+ s))
           '(#x66 #x49 #x0F #x6E #xC3))
          ("addsd-xmm0-xmm1" (lambda (s) (cl-cc/emit::emit-addsd-xx cl-cc/emit::+xmm0+ cl-cc/emit::+xmm1+ s))
           '(#xF2 #x0F #x58 #xC1))
          ("movsd-xmm0-xmm1" (lambda (s) (cl-cc/emit::emit-movsd-xx cl-cc/emit::+xmm0+ cl-cc/emit::+xmm1+ s))
           '(#xF2 #x0F #x10 #xC1)))
  (emit-fn expected-bytes)
  (assert-equal expected-bytes (%x86-encoding-collect-bytes emit-fn)))

;;; ─── emit-push-r64 / emit-pop-r64 / emit-ret / emit-vm-ret-inst ─────────

(deftest-each x86-ret-encoding
  "Both emit-ret and emit-vm-ret-inst produce a single #xC3 byte."
  :cases (("emit-ret"      (lambda (s) (cl-cc/emit::emit-ret s)))
          ("vm-ret-inst"   (lambda (s) (cl-cc/emit::emit-vm-ret-inst (cl-cc::make-vm-ret) s))))
  (emit-fn)
  (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (assert-equal 1 (length bytes))
    (assert-equal #xC3 (first bytes))))

(deftest x86-jmp-rel32-le-offset
  "JMP rel32 encodes offset 256 = #x100 as LE bytes [#x00 #x01 #x00 #x00] starting at byte 2."
  (let ((bytes (%x86-encoding-collect-bytes (lambda (s) (cl-cc/emit::emit-jmp-rel32 256 s)))))
    (assert-equal #x00 (second bytes))
    (assert-equal #x01 (third bytes))))

(deftest-each x86-fixed-encoding-spot-checks
  "Fixed-format instructions: length, first byte, second byte."
  :cases (("jmp-zero"   (lambda (s) (cl-cc/emit::emit-jmp-rel32 0 s))              5 #xE9 #x00)
          ("bswap-rax"  (lambda (s) (cl-cc/emit::emit-bswap-r32 cl-cc/emit::+rax+ s))  2 #x0F #xC8)
          ("jge-short"  (lambda (s) (cl-cc/emit::emit-jge-short 3 s))              2 #x7D    3)
          ("idiv-r11"   #'cl-cc/emit::emit-idiv-r11                                3 #x49 #xF7)
          ("cqo"        #'cl-cc/emit::emit-cqo                                     2 #x48 #x99)
          ("je-rel32"   (lambda (s) (cl-cc/emit::emit-je-rel32 0 s))               6 #x0F #x84)
          ("jne-rel32"  (lambda (s) (cl-cc/emit::emit-byte #x0F s)
                          (cl-cc/emit::emit-byte #x85 s)
                          (cl-cc/emit::emit-dword 0 s))                            6 #x0F #x85)
          ("cmp-rax-0"  (lambda (s) (cl-cc/emit::emit-cmp-ri64 cl-cc/emit::+rax+ 0 s)) 7 #x48 #x81)
          ("mov-r8-r9"  (lambda (s) (cl-cc/emit::emit-mov-rr64 cl-cc/emit::+r8+ cl-cc/emit::+r9+ s))
                                                                              3 #x4D #x89)
          ("not-rax"    (lambda (s) (cl-cc/emit::emit-not-r64    cl-cc/emit::+rax+ s)) 3 #x48 #xF7)
          ("neg-rax"    (lambda (s) (cl-cc/emit::emit-neg-r64    cl-cc/emit::+rax+ s)) 3 #x48 #xF7)
          ("dec-rax"    (lambda (s) (cl-cc/emit::emit-dec-r64    cl-cc/emit::+rax+ s)) 3 #x48 #xFF)
          ("cmp-rr64"   (lambda (s) (cl-cc/emit::emit-cmp-rr64   cl-cc/emit::+rax+ cl-cc/emit::+rcx+ s)) 3 #x48 #x39)
          ("test-rr64"  (lambda (s) (cl-cc/emit::emit-test-rr64  cl-cc/emit::+rax+ cl-cc/emit::+rax+ s)) 3 #x48 #x85)
          ("and-rr64"   (lambda (s) (cl-cc/emit::emit-and-rr64   cl-cc/emit::+rax+ cl-cc/emit::+rcx+ s)) 3 #x48 #x21)
          ("or-rr64"    (lambda (s) (cl-cc/emit::emit-or-rr64    cl-cc/emit::+rax+ cl-cc/emit::+rcx+ s)) 3 #x48 #x09)
          ("xor-rr64"   (lambda (s) (cl-cc/emit::emit-xor-rr64   cl-cc/emit::+rax+ cl-cc/emit::+rcx+ s)) 3 #x48 #x31)
          ("sal-cl"     (lambda (s) (cl-cc/emit::emit-sal-r64-cl cl-cc/emit::+rax+ s)) 3 #x48 #xD3)
          ("sar-cl"     (lambda (s) (cl-cc/emit::emit-sar-r64-cl cl-cc/emit::+rax+ s)) 3 #x48 #xD3)
          ("add-ri8"    (lambda (s) (cl-cc/emit::emit-add-ri8    cl-cc/emit::+rax+ 1 s)) 4 #x48 #x83)
          ("sub-ri8"    (lambda (s) (cl-cc/emit::emit-sub-ri8    cl-cc/emit::+rax+ 1 s)) 4 #x48 #x83)
          ("and-ri8"    (lambda (s) (cl-cc/emit::emit-and-ri8    cl-cc/emit::+rax+ 1 s)) 4 #x48 #x83))
  (emit-fn expected-len byte0 byte1)
  (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (assert-equal expected-len (length bytes))
    (assert-equal byte0 (first bytes))
    (assert-equal byte1 (second bytes))))

;;; ─── VM register mapping ─────────────────────────────────────────────────

(deftest x86-vm-reg-map
  "VM register map has 8 entries; :r0→RAX, :r1→RCX spot checks pass."
  (assert-equal 8 (length cl-cc/emit::*vm-reg-map*))
  (assert-equal cl-cc/emit::+rax+ (cdr (assoc :r0 cl-cc/emit::*vm-reg-map*)))
  (assert-equal cl-cc/emit::+rcx+ (cdr (assoc :r1 cl-cc/emit::*vm-reg-map*))))

;;; ─── emit-mov-ri64 ──────────────────────────────────────────────────────

(deftest-each x86-mov-ri64-encoding
  "emit-mov-ri64: always 10 bytes; REX byte, opcode, and immediate vary by register."
  :cases (("rax-42" cl-cc/emit::+rax+ 42 #x48 #xB8 42)
          ("rcx-0"  cl-cc/emit::+rcx+  0 #x48 #xB9  0)
          ("r8-1"   cl-cc/emit::+r8+   1 #x49 #xB8  1))
  (reg imm rex opcode imm-byte)
  (let ((bytes (%x86-encoding-collect-bytes
                (lambda (s) (cl-cc/emit::emit-mov-ri64 reg imm s)))))
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
  (assert-equal expected (cl-cc/emit::vm-const-to-integer input)))

(deftest-each x86-setcc-register-size
  "SETE on low register emits 3 bytes; high register (RSI=6) emits 4 bytes (REX added)."
  :cases (("low-reg-rax"  cl-cc/emit::+rax+ 3)
          ("high-reg-rsi" cl-cc/emit::+rsi+ 4))
  (reg expected-len)
  (let ((bytes (%x86-encoding-collect-bytes
                (lambda (s) (cl-cc/emit::emit-setcc #x94 reg s)))))
    (assert-equal expected-len (length bytes))))

(deftest-each x86-cmov-encoding
  "CMOVL/CMOVG each emit 4 bytes; third byte is the distinguishing opcode."
  :cases (("cmovl" (lambda (s) (cl-cc/emit::emit-cmovl-rr64 cl-cc/emit::+rax+ cl-cc/emit::+rcx+ s)) #x4C)
          ("cmovg" (lambda (s) (cl-cc/emit::emit-cmovg-rr64 cl-cc/emit::+rax+ cl-cc/emit::+rcx+ s)) #x4F))
  (emit-fn opcode3)
  (let ((bytes (%x86-encoding-collect-bytes emit-fn)))
    (assert-equal 4 (length bytes))
    (assert-equal opcode3 (third bytes))))

;;; ─── VM instruction emitter byte sizes ──────────────────────────────────

(deftest-each x86-vm-emitter-byte-size
  "VM instruction emitters produce the documented exact byte count."
  :cases (("vm-neg"      (lambda (s) (cl-cc/emit::emit-vm-neg
                           (cl-cc::make-vm-neg :dst :R0 :src :R1) s))      6)
          ("vm-not"      (lambda (s) (cl-cc/emit::emit-vm-not
                           (cl-cc::make-vm-not :dst :R0 :src :R1) s))     10)
          ("vm-lognot"   (lambda (s) (cl-cc/emit::emit-vm-lognot
                           (cl-cc::make-vm-lognot :dst :R0 :src :R1) s))   6)
          ("vm-inc"      (lambda (s) (cl-cc/emit::emit-vm-inc
                           (cl-cc::make-vm-inc :dst :R0 :src :R1) s))      7)
          ("vm-dec"      (lambda (s) (cl-cc/emit::emit-vm-dec
                           (cl-cc::make-vm-dec :dst :R0 :src :R1) s))      7)
          ("vm-abs"      (lambda (s) (cl-cc/emit::emit-vm-abs
                           (make-vm-abs :dst :R0 :src :R1) s))            15)
          ("vm-min"      (lambda (s) (cl-cc/emit::emit-vm-min
                           (make-vm-min :dst :R0 :lhs :R1 :rhs :R2) s))  10)
          ("vm-max"      (lambda (s) (cl-cc/emit::emit-vm-max
                           (make-vm-max :dst :R0 :lhs :R1 :rhs :R2) s))  10)
          ("vm-logand"   (lambda (s) (cl-cc/emit::emit-vm-logand
                           (make-vm-logand :dst :R0 :lhs :R1 :rhs :R2) s)) 6)
          ("vm-logior"   (lambda (s) (cl-cc/emit::emit-vm-logior
                           (make-vm-logior :dst :R0 :lhs :R1 :rhs :R2) s)) 6)
          ("vm-logxor"   (lambda (s) (cl-cc/emit::emit-vm-logxor
                           (make-vm-logxor :dst :R0 :lhs :R1 :rhs :R2) s)) 6)
          ("vm-logeqv"   (lambda (s) (cl-cc/emit::emit-vm-logeqv
                           (cl-cc::make-vm-logeqv :dst :R0 :lhs :R1 :rhs :R2) s))  9)
          ("vm-and"      (lambda (s) (cl-cc/emit::emit-vm-and
                           (cl-cc::make-vm-and :dst :R0 :lhs :R1 :rhs :R2) s))    17)
          ("vm-or"       (lambda (s) (cl-cc/emit::emit-vm-or
                           (cl-cc::make-vm-or  :dst :R0 :lhs :R1 :rhs :R2) s))    17)
          ("vm-true-pred"  (lambda (s) (cl-cc/emit::emit-vm-true-pred
                             (cl-cc::make-vm-number-p :dst :R0 :src :R1) s))       10)
          ("vm-false-pred" (lambda (s) (cl-cc/emit::emit-vm-false-pred
                             (cl-cc::make-vm-cons-p :dst :R0 :src :R1) s))         10)
          ("vm-truncate" (lambda (s) (cl-cc/emit::emit-vm-truncate
                           (make-vm-truncate :dst :R0 :lhs :R1 :rhs :R2) s))       21)
          ("vm-rem"      (lambda (s) (cl-cc/emit::emit-vm-rem
                           (cl-cc::make-vm-rem :dst :R0 :lhs :R1 :rhs :R2) s))     21)
          ("vm-ash"      (lambda (s) (cl-cc/emit::emit-vm-ash
                           (make-vm-ash :dst :R0 :lhs :R1 :rhs :R2) s))            24)
          ("vm-mul"      (lambda (s) (cl-cc/emit::emit-vm-mul
                           (cl-cc::make-vm-mul :dst :R0 :lhs :R1 :rhs :R2) s))      7)
          ("vm-div"      (lambda (s) (cl-cc/emit::emit-vm-div
                           (cl-cc::make-vm-div :dst :R0 :lhs :R1 :rhs :R2) s))     34)
          ("vm-mod"      (lambda (s) (cl-cc/emit::emit-vm-mod
                           (cl-cc::make-vm-mod :dst :R0 :lhs :R1 :rhs :R2) s))     37)
          ("vm-lt"       (lambda (s) (cl-cc/emit::emit-vm-lt
                           (cl-cc::make-vm-lt :dst :R0 :lhs :R1 :rhs :R2) s))      10)
          ("vm-gt"       (lambda (s) (cl-cc/emit::emit-vm-gt
                           (cl-cc::make-vm-gt :dst :R0 :lhs :R1 :rhs :R2) s))      10)
          ("vm-le"       (lambda (s) (cl-cc/emit::emit-vm-le
                           (cl-cc::make-vm-le :dst :R0 :lhs :R1 :rhs :R2) s))      10)
          ("vm-ge"       (lambda (s) (cl-cc/emit::emit-vm-ge
                           (cl-cc::make-vm-ge :dst :R0 :lhs :R1 :rhs :R2) s))      10)
          ("vm-num-eq"   (lambda (s) (cl-cc/emit::emit-vm-num-eq
                           (make-vm-num-eq :dst :R0 :lhs :R1 :rhs :R2) s))         10)
          ("vm-eq"       (lambda (s) (cl-cc/emit::emit-vm-eq
                           (make-vm-eq :dst :R0 :lhs :R1 :rhs :R2) s))             10)
          ("vm-null-p"   (lambda (s) (cl-cc/emit::emit-vm-null-p
                           (cl-cc::make-vm-null-p :dst :R0 :src :R1) s))           10)
          ("vm-logtest"  (lambda (s) (cl-cc/emit::emit-vm-logtest
                           (cl-cc::make-vm-logtest :dst :R0 :lhs :R1 :rhs :R2) s)) 13)
          ("vm-logbitp"  (lambda (s) (cl-cc/emit::emit-vm-logbitp
                           (cl-cc::make-vm-logbitp :dst :R0 :lhs :R1 :rhs :R2) s)) 15))
  (emit-fn expected-size)
  (assert-equal expected-size (length (%x86-encoding-collect-bytes emit-fn))))

;;; ─── instruction-size table ─────────────────────────────────────────────

(deftest x86-instruction-size-table-coverage
  "Instruction size table has entries for all expected VM types."
  (dolist (tp '(cl-cc/vm::vm-const cl-cc/vm::vm-move cl-cc/vm::vm-add cl-cc/vm::vm-sub cl-cc/vm::vm-mul
                cl-cc/vm::vm-halt cl-cc/vm::vm-label cl-cc/vm::vm-jump cl-cc/vm::vm-jump-zero cl-cc/vm::vm-ret
                cl-cc/vm::vm-lt cl-cc/vm::vm-gt cl-cc/vm::vm-le cl-cc/vm::vm-ge cl-cc/vm::vm-num-eq cl-cc/vm::vm-eq
                cl-cc/vm::vm-neg cl-cc/vm::vm-not cl-cc/vm::vm-lognot cl-cc/vm::vm-inc cl-cc/vm::vm-dec
                cl-cc/vm::vm-abs cl-cc/vm::vm-min cl-cc/vm::vm-max cl-cc/vm::vm-ash
                cl-cc/vm::vm-truncate cl-cc/vm::vm-rem cl-cc/vm::vm-div cl-cc/vm::vm-mod
                cl-cc/vm::vm-and cl-cc/vm::vm-or cl-cc/vm::vm-logand cl-cc/vm::vm-logior cl-cc/vm::vm-logxor
                cl-cc/vm::vm-logeqv cl-cc/vm::vm-logtest cl-cc/vm::vm-logbitp
                cl-cc/vm::vm-null-p cl-cc/vm::vm-number-p cl-cc/vm::vm-integer-p
                cl-cc/vm::vm-cons-p cl-cc/vm::vm-symbol-p cl-cc/vm::vm-function-p
                cl-cc/emit::vm-spill-store cl-cc/emit::vm-spill-load))
    (assert-true (gethash tp cl-cc/emit::*x86-64-instruction-sizes*))))
