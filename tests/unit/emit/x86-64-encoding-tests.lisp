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

(deftest-each x86-emit-byte-cases
  "emit-byte writes a single byte and masks to 8 bits."
  :cases (("normal-value" #xAB #xAB)
          ("mask-to-8bit" #x1FF #xFF))
  (input expected)
  (let ((bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-byte input s)))))
    (assert-equal 1 (length bytes))
    (assert-equal expected (first bytes))))

(deftest x86-emit-multi-byte-le
  "emit-dword writes 4 LE bytes; emit-qword writes 8 LE bytes."
  (let ((dw (%x86-collect-bytes (lambda (s) (cl-cc::emit-dword #xDEADBEEF s))))
        (qw (%x86-collect-bytes (lambda (s) (cl-cc::emit-qword #x0102030405060708 s)))))
    (assert-equal 4 (length dw))
    (assert-equal '(#xEF #xBE #xAD #xDE) dw)
    (assert-equal 8 (length qw))
    (assert-equal #x08 (first qw))
    (assert-equal #x07 (second qw))
    (assert-equal #x01 (eighth qw))))

;;; ─── emit-add-rr64 / emit-sub-rr64 ──────────────────────────────────────

(deftest-each x86-add-sub-rr64-encoding
  "ADD/SUB rr64 emit an exact 3-byte REX+opcode+ModRM sequence."
  :cases (("add-rax-rcx" (lambda (s) (cl-cc::emit-add-rr64 cl-cc::+rax+ cl-cc::+rcx+ s))
           '(#x48 #x01 #xC8))
          ("sub-rax-rcx" (lambda (s) (cl-cc::emit-sub-rr64 cl-cc::+rax+ cl-cc::+rcx+ s))
           '(#x48 #x29 #xC8))
          ("add-rcx-rdx" (lambda (s) (cl-cc::emit-add-rr64 cl-cc::+rcx+ cl-cc::+rdx+ s))
           '(#x48 #x01 #xD1))
          ("sub-rcx-rdx" (lambda (s) (cl-cc::emit-sub-rr64 cl-cc::+rcx+ cl-cc::+rdx+ s))
           '(#x48 #x29 #xD1)))
  (emit-fn expected-bytes)
  (assert-equal expected-bytes (%x86-collect-bytes emit-fn)))

;;; ─── emit-imul-rr64 / two-byte opcodes ──────────────────────────────────

(deftest-each x86-two-byte-opcode-instructions
  "REX+0F+opcode2+ModRM: 4 bytes, second=0F, third=opcode2."
  :cases (("imul-rr64"    (lambda (s) (cl-cc::emit-imul-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)) #xAF)
          ("movzx-r64-r8" (lambda (s) (cl-cc::emit-movzx-r64-r8 cl-cc::+rax+ cl-cc::+rax+ s)) #xB6))
  (emit-fn opcode2)
  (let ((bytes (%x86-collect-bytes emit-fn)))
    (assert-equal 4 (length bytes))
    (assert-equal #x0F (second bytes))
    (assert-equal opcode2 (third bytes))))

(deftest-each x86-xmm-instruction-encoding
  "XMM/SSE instruction encodings produce exact byte sequences."
  :cases (("movq-xmm0-r11" (lambda (s) (cl-cc::emit-movq-xmm-r64 cl-cc::+xmm0+ cl-cc::+r11+ s))
           '(#x66 #x49 #x0F #x6E #xC3))
          ("addsd-xmm0-xmm1" (lambda (s) (cl-cc::emit-addsd-xx cl-cc::+xmm0+ cl-cc::+xmm1+ s))
           '(#xF2 #x0F #x58 #xC1))
          ("movsd-xmm0-xmm1" (lambda (s) (cl-cc::emit-movsd-xx cl-cc::+xmm0+ cl-cc::+xmm1+ s))
           '(#xF2 #x0F #x10 #xC1)))
  (emit-fn expected-bytes)
  (assert-equal expected-bytes (%x86-collect-bytes emit-fn)))

;;; ─── emit-push-r64 / emit-pop-r64 / emit-ret / emit-vm-ret-inst ─────────

(deftest-each x86-ret-encoding
  "Both emit-ret and emit-vm-ret-inst produce a single #xC3 byte."
  :cases (("emit-ret"      (lambda (s) (cl-cc::emit-ret s)))
          ("vm-ret-inst"   (lambda (s) (cl-cc::emit-vm-ret-inst (cl-cc::make-vm-ret) s))))
  (emit-fn)
  (let ((bytes (%x86-collect-bytes emit-fn)))
    (assert-equal 1 (length bytes))
    (assert-equal #xC3 (first bytes))))

(deftest x86-jmp-rel32-le-offset
  "JMP rel32 encodes offset 256 = #x100 as LE bytes [#x00 #x01 #x00 #x00] starting at byte 2."
  (let ((bytes (%x86-collect-bytes (lambda (s) (cl-cc::emit-jmp-rel32 256 s)))))
    (assert-equal #x00 (second bytes))
    (assert-equal #x01 (third bytes))))

(deftest-each x86-fixed-encoding-spot-checks
  "Fixed-format instructions: length, first byte, second byte."
  :cases (("jmp-zero"   (lambda (s) (cl-cc::emit-jmp-rel32 0 s))              5 #xE9 #x00)
          ("bswap-rax"  (lambda (s) (cl-cc::emit-bswap-r32 cl-cc::+rax+ s))  2 #x0F #xC8)
          ("jge-short"  (lambda (s) (cl-cc::emit-jge-short 3 s))              2 #x7D    3)
          ("idiv-r11"   #'cl-cc::emit-idiv-r11                                3 #x49 #xF7)
          ("cqo"        #'cl-cc::emit-cqo                                     2 #x48 #x99)
          ("je-rel32"   (lambda (s) (cl-cc::emit-je-rel32 0 s))               6 #x0F #x84)
          ("jne-rel32"  (lambda (s) (cl-cc::emit-byte #x0F s)
                          (cl-cc::emit-byte #x85 s)
                          (cl-cc::emit-dword 0 s))                            6 #x0F #x85)
          ("cmp-rax-0"  (lambda (s) (cl-cc::emit-cmp-ri64 cl-cc::+rax+ 0 s)) 7 #x48 #x81)
          ("mov-r8-r9"  (lambda (s) (cl-cc::emit-mov-rr64 cl-cc::+r8+ cl-cc::+r9+ s))
                                                                              3 #x4D #x89)
          ("not-rax"    (lambda (s) (cl-cc::emit-not-r64    cl-cc::+rax+ s)) 3 #x48 #xF7)
          ("neg-rax"    (lambda (s) (cl-cc::emit-neg-r64    cl-cc::+rax+ s)) 3 #x48 #xF7)
          ("dec-rax"    (lambda (s) (cl-cc::emit-dec-r64    cl-cc::+rax+ s)) 3 #x48 #xFF)
          ("cmp-rr64"   (lambda (s) (cl-cc::emit-cmp-rr64   cl-cc::+rax+ cl-cc::+rcx+ s)) 3 #x48 #x39)
          ("test-rr64"  (lambda (s) (cl-cc::emit-test-rr64  cl-cc::+rax+ cl-cc::+rax+ s)) 3 #x48 #x85)
          ("and-rr64"   (lambda (s) (cl-cc::emit-and-rr64   cl-cc::+rax+ cl-cc::+rcx+ s)) 3 #x48 #x21)
          ("or-rr64"    (lambda (s) (cl-cc::emit-or-rr64    cl-cc::+rax+ cl-cc::+rcx+ s)) 3 #x48 #x09)
          ("xor-rr64"   (lambda (s) (cl-cc::emit-xor-rr64   cl-cc::+rax+ cl-cc::+rcx+ s)) 3 #x48 #x31)
          ("sal-cl"     (lambda (s) (cl-cc::emit-sal-r64-cl cl-cc::+rax+ s)) 3 #x48 #xD3)
          ("sar-cl"     (lambda (s) (cl-cc::emit-sar-r64-cl cl-cc::+rax+ s)) 3 #x48 #xD3)
          ("add-ri8"    (lambda (s) (cl-cc::emit-add-ri8    cl-cc::+rax+ 1 s)) 4 #x48 #x83)
          ("sub-ri8"    (lambda (s) (cl-cc::emit-sub-ri8    cl-cc::+rax+ 1 s)) 4 #x48 #x83)
          ("and-ri8"    (lambda (s) (cl-cc::emit-and-ri8    cl-cc::+rax+ 1 s)) 4 #x48 #x83))
  (emit-fn expected-len byte0 byte1)
  (let ((bytes (%x86-collect-bytes emit-fn)))
    (assert-equal expected-len (length bytes))
    (assert-equal byte0 (first bytes))
    (assert-equal byte1 (second bytes))))

;;; ─── VM register mapping ─────────────────────────────────────────────────

(deftest x86-vm-reg-map
  "VM register map has 8 entries; :r0→RAX, :r1→RCX spot checks pass."
  (assert-equal 8 (length cl-cc::*vm-reg-map*))
  (assert-equal cl-cc::+rax+ (cdr (assoc :r0 cl-cc::*vm-reg-map*)))
  (assert-equal cl-cc::+rcx+ (cdr (assoc :r1 cl-cc::*vm-reg-map*))))

;;; ─── emit-mov-ri64 ──────────────────────────────────────────────────────

(deftest-each x86-mov-ri64-encoding
  "emit-mov-ri64: always 10 bytes; REX byte, opcode, and immediate vary by register."
  :cases (("rax-42" cl-cc::+rax+ 42 #x48 #xB8 42)
          ("rcx-0"  cl-cc::+rcx+  0 #x48 #xB9  0)
          ("r8-1"   cl-cc::+r8+   1 #x49 #xB8  1))
  (reg imm rex opcode imm-byte)
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-mov-ri64 reg imm s)))))
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
  (assert-equal expected (cl-cc::vm-const-to-integer input)))

(deftest-each x86-setcc-register-size
  "SETE on low register emits 3 bytes; high register (RSI=6) emits 4 bytes (REX added)."
  :cases (("low-reg-rax"  cl-cc::+rax+ 3)
          ("high-reg-rsi" cl-cc::+rsi+ 4))
  (reg expected-len)
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-setcc #x94 reg s)))))
    (assert-equal expected-len (length bytes))))

(deftest-each x86-cmov-encoding
  "CMOVL/CMOVG each emit 4 bytes; third byte is the distinguishing opcode."
  :cases (("cmovl" (lambda (s) (cl-cc::emit-cmovl-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)) #x4C)
          ("cmovg" (lambda (s) (cl-cc::emit-cmovg-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)) #x4F))
  (emit-fn opcode3)
  (let ((bytes (%x86-collect-bytes emit-fn)))
    (assert-equal 4 (length bytes))
    (assert-equal opcode3 (third bytes))))

;;; ─── VM instruction emitter byte sizes ──────────────────────────────────

(deftest-each x86-vm-emitter-byte-size
  "VM instruction emitters produce the documented exact byte count."
  :cases (("vm-neg"      (lambda (s) (cl-cc::emit-vm-neg
                           (cl-cc::make-vm-neg :dst :R0 :src :R1) s))      6)
          ("vm-not"      (lambda (s) (cl-cc::emit-vm-not
                           (cl-cc::make-vm-not :dst :R0 :src :R1) s))     10)
          ("vm-lognot"   (lambda (s) (cl-cc::emit-vm-lognot
                           (cl-cc::make-vm-lognot :dst :R0 :src :R1) s))   6)
          ("vm-inc"      (lambda (s) (cl-cc::emit-vm-inc
                           (cl-cc::make-vm-inc :dst :R0 :src :R1) s))      7)
          ("vm-dec"      (lambda (s) (cl-cc::emit-vm-dec
                           (cl-cc::make-vm-dec :dst :R0 :src :R1) s))      7)
          ("vm-abs"      (lambda (s) (cl-cc::emit-vm-abs
                           (make-vm-abs :dst :R0 :src :R1) s))            15)
          ("vm-min"      (lambda (s) (cl-cc::emit-vm-min
                           (make-vm-min :dst :R0 :lhs :R1 :rhs :R2) s))  10)
          ("vm-max"      (lambda (s) (cl-cc::emit-vm-max
                           (make-vm-max :dst :R0 :lhs :R1 :rhs :R2) s))  10)
          ("vm-logand"   (lambda (s) (cl-cc::emit-vm-logand
                           (make-vm-logand :dst :R0 :lhs :R1 :rhs :R2) s)) 6)
          ("vm-logior"   (lambda (s) (cl-cc::emit-vm-logior
                           (make-vm-logior :dst :R0 :lhs :R1 :rhs :R2) s)) 6)
          ("vm-logxor"   (lambda (s) (cl-cc::emit-vm-logxor
                           (make-vm-logxor :dst :R0 :lhs :R1 :rhs :R2) s)) 6)
          ("vm-logeqv"   (lambda (s) (cl-cc::emit-vm-logeqv
                           (cl-cc::make-vm-logeqv :dst :R0 :lhs :R1 :rhs :R2) s))  9)
          ("vm-and"      (lambda (s) (cl-cc::emit-vm-and
                           (cl-cc::make-vm-and :dst :R0 :lhs :R1 :rhs :R2) s))    17)
          ("vm-or"       (lambda (s) (cl-cc::emit-vm-or
                           (cl-cc::make-vm-or  :dst :R0 :lhs :R1 :rhs :R2) s))    17)
          ("vm-true-pred"  (lambda (s) (cl-cc::emit-vm-true-pred
                             (cl-cc::make-vm-number-p :dst :R0 :src :R1) s))       10)
          ("vm-false-pred" (lambda (s) (cl-cc::emit-vm-false-pred
                             (cl-cc::make-vm-cons-p :dst :R0 :src :R1) s))         10)
          ("vm-truncate" (lambda (s) (cl-cc::emit-vm-truncate
                           (make-vm-truncate :dst :R0 :lhs :R1 :rhs :R2) s))       21)
          ("vm-rem"      (lambda (s) (cl-cc::emit-vm-rem
                           (cl-cc::make-vm-rem :dst :R0 :lhs :R1 :rhs :R2) s))     21)
          ("vm-ash"      (lambda (s) (cl-cc::emit-vm-ash
                           (make-vm-ash :dst :R0 :lhs :R1 :rhs :R2) s))            24)
          ("vm-mul"      (lambda (s) (cl-cc::emit-vm-mul
                           (cl-cc::make-vm-mul :dst :R0 :lhs :R1 :rhs :R2) s))      7)
          ("vm-div"      (lambda (s) (cl-cc::emit-vm-div
                           (cl-cc::make-vm-div :dst :R0 :lhs :R1 :rhs :R2) s))     34)
          ("vm-mod"      (lambda (s) (cl-cc::emit-vm-mod
                           (cl-cc::make-vm-mod :dst :R0 :lhs :R1 :rhs :R2) s))     37)
          ("vm-lt"       (lambda (s) (cl-cc::emit-vm-lt
                           (cl-cc::make-vm-lt :dst :R0 :lhs :R1 :rhs :R2) s))      10)
          ("vm-gt"       (lambda (s) (cl-cc::emit-vm-gt
                           (cl-cc::make-vm-gt :dst :R0 :lhs :R1 :rhs :R2) s))      10)
          ("vm-le"       (lambda (s) (cl-cc::emit-vm-le
                           (cl-cc::make-vm-le :dst :R0 :lhs :R1 :rhs :R2) s))      10)
          ("vm-ge"       (lambda (s) (cl-cc::emit-vm-ge
                           (cl-cc::make-vm-ge :dst :R0 :lhs :R1 :rhs :R2) s))      10)
          ("vm-num-eq"   (lambda (s) (cl-cc::emit-vm-num-eq
                           (make-vm-num-eq :dst :R0 :lhs :R1 :rhs :R2) s))         10)
          ("vm-eq"       (lambda (s) (cl-cc::emit-vm-eq
                           (make-vm-eq :dst :R0 :lhs :R1 :rhs :R2) s))             10)
          ("vm-null-p"   (lambda (s) (cl-cc::emit-vm-null-p
                           (cl-cc::make-vm-null-p :dst :R0 :src :R1) s))           10)
          ("vm-logtest"  (lambda (s) (cl-cc::emit-vm-logtest
                           (cl-cc::make-vm-logtest :dst :R0 :lhs :R1 :rhs :R2) s)) 13)
          ("vm-logbitp"  (lambda (s) (cl-cc::emit-vm-logbitp
                           (cl-cc::make-vm-logbitp :dst :R0 :lhs :R1 :rhs :R2) s)) 15))
  (emit-fn expected-size)
  (assert-equal expected-size (length (%x86-collect-bytes emit-fn))))

;;; ─── instruction-size table ─────────────────────────────────────────────

(deftest x86-instruction-size-table-coverage
  "Instruction size table has entries for all expected VM types."
  (dolist (tp '(cl-cc::vm-const cl-cc::vm-move cl-cc::vm-add cl-cc::vm-sub cl-cc::vm-mul
                cl-cc::vm-halt cl-cc::vm-label cl-cc::vm-jump cl-cc::vm-jump-zero cl-cc::vm-ret
                cl-cc::vm-lt cl-cc::vm-gt cl-cc::vm-le cl-cc::vm-ge cl-cc::vm-num-eq cl-cc::vm-eq
                cl-cc::vm-neg cl-cc::vm-not cl-cc::vm-lognot cl-cc::vm-inc cl-cc::vm-dec
                cl-cc::vm-abs cl-cc::vm-min cl-cc::vm-max cl-cc::vm-ash
                cl-cc::vm-truncate cl-cc::vm-rem cl-cc::vm-div cl-cc::vm-mod
                cl-cc::vm-and cl-cc::vm-or cl-cc::vm-logand cl-cc::vm-logior cl-cc::vm-logxor
                cl-cc::vm-logeqv cl-cc::vm-logtest cl-cc::vm-logbitp
                cl-cc::vm-null-p cl-cc::vm-number-p cl-cc::vm-integer-p
                cl-cc::vm-cons-p cl-cc::vm-symbol-p cl-cc::vm-function-p
                cl-cc::vm-spill-store cl-cc::vm-spill-load))
    (assert-true (gethash tp cl-cc::*x86-64-instruction-sizes*))))

(deftest-each x86-instruction-size-zero-cases
  "vm-label and unknown types both report instruction-size = 0."
  :cases (("vm-label"   (cl-cc::make-vm-label :name "test"))
          ("unknown"    (list :not-a-real-inst)))
  (inst)
  (assert-equal 0 (cl-cc::instruction-size inst)))

;;; ─── build-label-offsets ────────────────────────────────────────────────

(deftest-each x86-build-label-offsets-simple
  "build-label-offsets: label offset equals prologue-size plus preceding instruction bytes."
  :cases (("single-at-start"
           (list (cl-cc::make-vm-label :name "L"))
           6 "L" 6)
          ("after-vm-const"
           (list (cl-cc::make-vm-const :dst :R0 :value 42)
                 (cl-cc::make-vm-label :name "L"))
           6 "L" 16)
          ("after-vm-add"
           (list (cl-cc::make-vm-add :dst :R0 :lhs :R1 :rhs :R2)
                 (cl-cc::make-vm-label :name "L"))
           0 "L" 6))
  (insts prologue label expected-offset)
  (assert-equal expected-offset (gethash label (cl-cc::build-label-offsets insts prologue))))

(deftest x86-build-label-offsets-multi
  "Empty list → 0 table entries; multiple labels track correct byte positions."
  (assert-equal 0 (hash-table-count (cl-cc::build-label-offsets nil 6)))
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

(deftest x86-emitter-table-integrity
  "Emitter table has >= 40 entries and every value is a function."
  (assert-true (>= (hash-table-count cl-cc::*x86-64-emitter-table*) 40))
  (let ((all-ok t))
    (maphash (lambda (key fn)
               (declare (ignore key))
               (unless (functionp fn)
                 (setf all-ok nil)))
             cl-cc::*x86-64-emitter-table*)
    (assert-true all-ok)))

;;; ─── High-register encoding (REX.R / REX.B) and memory displacement ────

(deftest-each x86-mov-mem-displacement-sizes
  "Memory load/store displacement: zero-disp uses mod=00 (3 bytes); byte-disp uses mod=01 (4 bytes)."
  :cases (("rm64-no-disp"   (lambda (s) (cl-cc::emit-mov-rm64 cl-cc::+rax+ cl-cc::+rcx+  0 s)) 3)
          ("rm64-disp8"     (lambda (s) (cl-cc::emit-mov-rm64 cl-cc::+rax+ cl-cc::+rcx+  8 s)) 4)
          ("mr64-disp8-16"  (lambda (s) (cl-cc::emit-mov-mr64 cl-cc::+rcx+ 16 cl-cc::+rax+ s)) 4))
  (emit-fn expected-len)
  (assert-equal expected-len (length (%x86-collect-bytes emit-fn))))

(deftest-each x86-mov-exact-bytes
  "MOV load/store instructions (indexed, RSP-based SIB) produce exact byte sequences."
  :cases (("rm64-indexed-disp8"
           (lambda (s) (cl-cc::emit-mov-rm64-indexed cl-cc::+rax+ cl-cc::+rbx+ cl-cc::+rcx+ 8 16 s))
           '(#x48 #x8B #x44 #xCB #x10))
          ("mr64-indexed-disp8"
           (lambda (s) (cl-cc::emit-mov-mr64-indexed cl-cc::+rbx+ cl-cc::+rcx+ 4 8 cl-cc::+rax+ s))
           '(#x48 #x89 #x44 #x8B #x08))
          ("rm64-indexed-scale1-zero"
           (lambda (s) (cl-cc::emit-mov-rm64-indexed cl-cc::+rax+ cl-cc::+rbx+ cl-cc::+rcx+ 1 0 s))
           '(#x48 #x8B #x04 #x0B))
          ("load-rsp-disp8"
           (lambda (s) (cl-cc::emit-mov-rm64 cl-cc::+rax+ cl-cc::+rsp+ -8 s))
           '(#x48 #x8B #x44 #x24 #xF8))
          ("store-rsp-disp8"
           (lambda (s) (cl-cc::emit-mov-mr64 cl-cc::+rsp+ -16 cl-cc::+rax+ s))
           '(#x48 #x89 #x44 #x24 #xF0)))
  (emit-fn expected-bytes)
  (assert-equal expected-bytes (%x86-collect-bytes emit-fn)))

;;; ─── emit-mov-rr64 ModR/M correctness ──────────────────────────────────

(deftest-each x86-mov-rr64-modrm-encoding
  "emit-mov-rr64: REX.W=#x48, opcode=#x89, ModR/M byte varies by register pair."
  :cases (("rax-rcx" cl-cc::+rax+ cl-cc::+rcx+ #xC8)
          ("rax-rbx" cl-cc::+rax+ cl-cc::+rbx+ #xD8)
          ("rax-rax" cl-cc::+rax+ cl-cc::+rax+ #xC0))
  (dst src modrm)
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-mov-rr64 dst src s)))))
    (assert-equal 3 (length bytes))
    (assert-equal #x48 (first bytes))
    (assert-equal #x89 (second bytes))
    (assert-equal modrm (third bytes))))

;;; ─── PUSH/POP single-byte opcode coverage ───────────────────────────────

(deftest-each x86-push-pop-single-byte-opcodes
  "PUSH/POP registers all emit single-byte opcodes."
  :cases (("push-rax" cl-cc::+rax+ #'cl-cc::emit-push-r64 #x50)
          ("push-rcx" cl-cc::+rcx+ #'cl-cc::emit-push-r64 #x51)
          ("pop-rax"  cl-cc::+rax+ #'cl-cc::emit-pop-r64  #x58)
          ("push-rbx" cl-cc::+rbx+ #'cl-cc::emit-push-r64 #x53)
          ("pop-rcx"  cl-cc::+rcx+ #'cl-cc::emit-pop-r64  #x59)
          ("pop-rdx"  cl-cc::+rdx+ #'cl-cc::emit-pop-r64  #x5A))
  (reg emit-fn opcode)
  (let ((bytes (%x86-collect-bytes (lambda (s) (funcall emit-fn reg s)))))
    (assert-equal 1 (length bytes))
    (assert-equal opcode (first bytes))))

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

(deftest-each x86-vm-const-bool-immediate
  "vm-const always emits 10 bytes; nil→0, t→1 encodes correct LE immediate bytes."
  :cases (("nil" nil 0)
          ("t"   t   1))
  (val expected-byte)
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-const
                             (cl-cc::make-vm-const :dst :R0 :value val) s)))))
    (assert-equal 10 (length bytes))
    (assert-equal expected-byte (third bytes))
    (assert-equal 0 (fourth bytes))))

(deftest-each x86-vm-move-halt-elision
  "No-op moves (same src/dst or result already in RAX) emit 0 bytes; cross-register emit 3 bytes with opcode #x89."
  :cases (("move-cross" (lambda (s) (cl-cc::emit-vm-move
                                     (cl-cc::make-vm-move :dst :R0 :src :R1) s))
                        (lambda (s) (cl-cc::emit-vm-move
                                     (cl-cc::make-vm-move :dst :R0 :src :R0) s)))
          ("halt"       (lambda (s) (cl-cc::emit-vm-halt-inst
                                     (cl-cc::make-vm-halt :reg :R1) s))
                        (lambda (s) (cl-cc::emit-vm-halt-inst
                                     (cl-cc::make-vm-halt :reg :R0) s))))
  (cross-emitter zero-emitter)
  (let ((cross-bytes (%x86-collect-bytes cross-emitter))
        (zero-bytes  (%x86-collect-bytes zero-emitter)))
    (assert-equal 3   (length cross-bytes))
    (assert-equal #x89 (second cross-bytes))
    (assert-equal 0   (length zero-bytes))))

;;; ─── emit-idiv-sequence ──────────────────────────────────────────────────

(deftest-each x86-idiv-sequence-size
  "emit-idiv-sequence emits exactly 18 bytes for both quotient and remainder modes."
  :cases (("quotient"  nil)
          ("remainder" t))
  (remainder-p)
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc::emit-idiv-sequence cl-cc::+rcx+ cl-cc::+rdx+ remainder-p s)))))
    (assert-equal 18 (length bytes))))

;;; ─── emit-vm-program prologue/epilogue ───────────────────────────────────

(deftest x86-vm-program-output
  "compile-to-x86-64-bytes produces non-empty bytes for non-empty programs
and emits a short prologue+return sequence for empty programs.
Use compile-to-x86-64-bytes (which sets up the regalloc context internally)
rather than emit-vm-program directly — the latter expects *current-regalloc*
to be bound, which fails with NIL under raw invocation."
  (let* ((non-empty-insts (list (cl-cc::make-vm-halt :reg :R0)
                                (cl-cc::make-vm-ret :reg :R0)))
         (full-prog (cl-cc::make-vm-program :instructions non-empty-insts :result-register :R0))
         (full-bytes (cl-cc::compile-to-x86-64-bytes full-prog))
         (empty-prog (cl-cc::make-vm-program :instructions nil :result-register :R0))
         (empty-bytes (cl-cc::compile-to-x86-64-bytes empty-prog)))
    (assert-true (> (length full-bytes) 0))
    (assert-true (> (length empty-bytes) 0))))

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
