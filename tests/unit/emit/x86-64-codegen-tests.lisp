;;;; tests/unit/emit/x86-64-codegen-tests.lisp — x86-64 Code Generation Tests
;;;;
;;;; Tests for src/emit/x86-64-codegen.lisp:
;;;; *vm-reg-map*, *phys-reg-to-x86-code*, vm-reg-to-x86,
;;;; vm-const-to-integer, instruction-size, build-label-offsets,
;;;; *x86-64-emitter-entries*, *x86-64-emitter-table*

(in-package :cl-cc/test)

(defsuite x86-64-codegen-suite :description "x86-64 machine code generation tests")

;;; ─── *vm-reg-map* ───────────────────────────────────────────────────────────

(deftest x86-64-vm-reg-map-length
  "*vm-reg-map* maps exactly 8 virtual registers."
  (assert-= 8 (length cl-cc::*vm-reg-map*)))

(deftest-each x86-64-vm-reg-map-entries
  "*vm-reg-map* maps each VM register to the correct x86-64 code."
  :cases (("R0→rax" :R0 cl-cc::+rax+)
          ("R1→rcx" :R1 cl-cc::+rcx+)
          ("R2→rdx" :R2 cl-cc::+rdx+)
          ("R3→rbx" :R3 cl-cc::+rbx+)
          ("R4→rsi" :R4 cl-cc::+rsi+)
          ("R5→rdi" :R5 cl-cc::+rdi+)
          ("R6→r8"  :R6 cl-cc::+r8+)
          ("R7→r9"  :R7 cl-cc::+r9+))
  (vm-reg expected)
  (assert-= expected (cdr (assoc vm-reg cl-cc::*vm-reg-map*))))

;;; ─── *phys-reg-to-x86-code* ─────────────────────────────────────────────────

(deftest x86-64-phys-reg-map-length
  "*phys-reg-to-x86-code* maps exactly 14 physical registers."
  (assert-= 14 (length cl-cc::*phys-reg-to-x86-code*)))

(deftest-each x86-64-phys-reg-map-entries
  "*phys-reg-to-x86-code* maps each physical register to the correct code."
  :cases (("rax"  :rax  cl-cc::+rax+)
          ("rcx"  :rcx  cl-cc::+rcx+)
          ("rdx"  :rdx  cl-cc::+rdx+)
          ("rbx"  :rbx  cl-cc::+rbx+)
          ("rsi"  :rsi  cl-cc::+rsi+)
          ("rdi"  :rdi  cl-cc::+rdi+)
          ("r8"   :r8   cl-cc::+r8+)
          ("r9"   :r9   cl-cc::+r9+)
          ("r10"  :r10  cl-cc::+r10+)
          ("r11"  :r11  cl-cc::+r11+)
          ("r12"  :r12  cl-cc::+r12+)
          ("r13"  :r13  cl-cc::+r13+)
          ("r14"  :r14  cl-cc::+r14+)
          ("r15"  :r15  cl-cc::+r15+))
  (phys-reg expected)
  (assert-= expected (cdr (assoc phys-reg cl-cc::*phys-reg-to-x86-code*))))

;;; ─── vm-reg-to-x86 ──────────────────────────────────────────────────────────

(deftest-each x86-64-vm-reg-to-x86-naive
  "vm-reg-to-x86 maps VM registers to x86 codes (naive mode, no regalloc)."
  :cases (("R0" :R0 cl-cc::+rax+)
          ("R1" :R1 cl-cc::+rcx+)
          ("R2" :R2 cl-cc::+rdx+)
          ("R3" :R3 cl-cc::+rbx+)
          ("R4" :R4 cl-cc::+rsi+)
          ("R5" :R5 cl-cc::+rdi+)
          ("R6" :R6 cl-cc::+r8+)
          ("R7" :R7 cl-cc::+r9+))
  (vm-reg expected)
  (let ((cl-cc::*current-regalloc* nil))
    (assert-= expected (cl-cc::vm-reg-to-x86 vm-reg))))

(deftest x86-64-vm-reg-to-x86-unknown-signals
  "vm-reg-to-x86 signals error for unknown register."
  (let ((cl-cc::*current-regalloc* nil))
    (assert-signals error (cl-cc::vm-reg-to-x86 :R99))))

;;; ─── vm-const-to-integer ────────────────────────────────────────────────────

(deftest-each x86-64-vm-const-to-integer
  "vm-const-to-integer coerces VM constant values to integers."
  :cases (("nil→0"     nil   0)
          ("t→1"       t     1)
          ("42→42"     42    42)
          ("-7→-7"     -7    -7)
          ("other→0"   :foo  0))
  (input expected)
  (assert-= expected (cl-cc::vm-const-to-integer input)))

;;; ─── *x86-64-instruction-sizes* ─────────────────────────────────────────────
;;; Note: hash keys are symbols in the cl-cc package; use cl-cc:: prefix.

(deftest-each x86-64-instruction-sizes-spot-checks
  "Known VM instruction types have correct byte sizes in the size table."
  :cases (("vm-const"     'cl-cc::vm-const    10)
           ("vm-move"      'cl-cc::vm-move      3)
           ("vm-add"       'cl-cc::vm-add       6)
           ("vm-integer-add" 'cl-cc::vm-integer-add 6)
           ("vm-sub"       'cl-cc::vm-sub       6)
           ("vm-integer-sub" 'cl-cc::vm-integer-sub 6)
           ("vm-mul"       'cl-cc::vm-mul       7)
           ("vm-integer-mul" 'cl-cc::vm-integer-mul 7)
           ("vm-bswap"     'cl-cc::vm-bswap     6)
          ("vm-halt"      'cl-cc::vm-halt      3)
          ("vm-call"      'cl-cc::vm-call      6)
          ("vm-tail-call" 'cl-cc::vm-tail-call 3)
          ("vm-label"     'cl-cc::vm-label     0)
          ("vm-jump"      'cl-cc::vm-jump      5)
          ("vm-jump-zero" 'cl-cc::vm-jump-zero 9)
          ("vm-ret"       'cl-cc::vm-ret       1)
          ("vm-ash"       'cl-cc::vm-ash      24)
          ("vm-div"       'cl-cc::vm-div      34)
          ("vm-mod"       'cl-cc::vm-mod      37)
          ("vm-abs"       'cl-cc::vm-abs      15)
          ("vm-min"       'cl-cc::vm-min      10)
          ("vm-max"       'cl-cc::vm-max      10))
  (sym expected)
  (assert-= expected (gethash sym cl-cc::*x86-64-instruction-sizes*)))

(deftest x86-64-instruction-sizes-comparison-ops
  "All comparison instructions have size 12."
  (dolist (tp '(cl-cc::vm-lt cl-cc::vm-gt cl-cc::vm-le
                cl-cc::vm-ge cl-cc::vm-num-eq cl-cc::vm-eq))
    (assert-= 12 (gethash tp cl-cc::*x86-64-instruction-sizes*))))

(deftest x86-64-instruction-sizes-type-predicates
  "Type predicate instructions have correct sizes."
  (assert-= 11 (gethash 'cl-cc::vm-null-p cl-cc::*x86-64-instruction-sizes*))
  (dolist (tp '(cl-cc::vm-number-p cl-cc::vm-integer-p cl-cc::vm-cons-p
                cl-cc::vm-symbol-p cl-cc::vm-function-p))
    (assert-= 10 (gethash tp cl-cc::*x86-64-instruction-sizes*))))

(deftest x86-64-instruction-size-vm-move-self-is-zero
  "instruction-size returns 0 for a vm-move that maps to the same physical register."
  (let ((cl-cc::*current-regalloc* nil))
    (assert-= 0 (cl-cc::instruction-size (cl-cc::make-vm-move :dst :R0 :src :R0)))))

;;; ─── *x86-64-emitter-entries* / *x86-64-emitter-table* ─────────────────────

(deftest x86-64-emitter-entries-count
  "*x86-64-emitter-entries* has 45 entries covering all supported instructions."
  (assert-= 56 (length cl-cc::*x86-64-emitter-entries*)))

(deftest x86-64-emitter-table-built-from-entries
  "*x86-64-emitter-table* contains an entry for each item in *x86-64-emitter-entries*."
  (dolist (entry cl-cc::*x86-64-emitter-entries*)
    (assert-true (gethash (car entry) cl-cc::*x86-64-emitter-table*))))

(deftest x86-64-empty-program-trims-unused-callee-saved-regs
  "compile-to-x86-64-bytes emits only the frame-pointer save/restore for an empty program."
  (let* ((prog (cl-cc::make-vm-program :instructions nil :result-register :R0))
         (bytes (cl-cc::compile-to-x86-64-bytes prog)))
    (assert-= 3 (length bytes))))

(deftest x86-64-leaf-program-trims-prologue-through-pipeline
  "A real compiled leaf program reaches native codegen and trims the prologue."
  (let* ((result (compile-string "(+ 1 2)" :target :vm))
         (prog (compilation-result-program result))
         (base (cl-cc::make-vm-program :instructions (cl-cc::vm-program-instructions prog)
                                       :result-register (cl-cc::vm-program-result-register prog)
                                       :leaf-p nil))
         (leaf-bytes (cl-cc::compile-to-x86-64-bytes prog))
         (nonleaf-bytes (cl-cc::compile-to-x86-64-bytes base)))
    (assert-true (cl-cc::vm-program-leaf-p prog))
    (assert-true (< (length leaf-bytes) (length nonleaf-bytes)))))

(deftest-each x86-64-emitter-table-spot-checks
  "Key instructions are present in *x86-64-emitter-table* and are functions."
  :cases (("vm-const"   'cl-cc::vm-const)
           ("vm-add"     'cl-cc::vm-add)
           ("vm-integer-add" 'cl-cc::vm-integer-add)
           ("vm-float-add" 'cl-cc::vm-float-add)
           ("vm-float-div" 'cl-cc::vm-float-div)
           ("vm-call"    'cl-cc::vm-call)
           ("vm-tail-call" 'cl-cc::vm-tail-call)
           ("vm-lt"      'cl-cc::vm-lt)
           ("vm-neg"     'cl-cc::vm-neg)
           ("vm-bswap"   'cl-cc::vm-bswap)
          ("vm-and"     'cl-cc::vm-and)
          ("vm-logand"  'cl-cc::vm-logand)
          ("vm-null-p"  'cl-cc::vm-null-p))
  (sym)
  (assert-true (functionp (gethash sym cl-cc::*x86-64-emitter-table*))))

(deftest x86-64-float-const-add-program-uses-xmm-path
  "Float const/add/halt emits MOVQ+scalar SSE opcodes instead of integer ALU bytes."
  (let* ((prog (cl-cc::make-vm-program
                :instructions (list (cl-cc::make-vm-const :dst :R0 :value 1.0d0)
                                    (cl-cc::make-vm-const :dst :R1 :value 2.0d0)
                                    (cl-cc::make-vm-float-add :dst :R2 :lhs :R0 :rhs :R1)
                                    (cl-cc::make-vm-halt :reg :R2))
                :result-register :R2))
         (bytes (coerce (cl-cc::compile-to-x86-64-bytes prog) 'list)))
    (assert-true (search '(#x66 #x49 #x0F #x6E) bytes :test #'eql))
    (assert-true (search '(#xF2 #x0F #x10) bytes :test #'eql))
    (assert-true (search '(#xF2 #x0F #x58) bytes :test #'eql))))

;;; Helper: collect bytes emitted by a function (local copy; also defined in encoding-tests)
(defun %x86-collect-bytes (emit-fn)
  "Call EMIT-FN with a stream that collects bytes. Returns byte list."
  (let ((bytes nil))
    (funcall emit-fn (lambda (b) (push b bytes)))
    (nreverse bytes)))

(deftest x86-64-bswap-emitter-encoding
  "emit-vm-bswap emits a MOV followed by BSWAP r32."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc::emit-vm-bswap (cl-cc::make-vm-bswap :dst :R0 :src :R1) s)))))
    (assert-= 5 (length bytes))
    (assert-= #x48 (first bytes))
    (assert-= #x89 (second bytes))
    (assert-= #xC8 (third bytes))
    (assert-= #x0F (fourth bytes))
    (assert-= #xC8 (fifth bytes))))

(deftest x86-64-add-emitter-two-address-lowering
  "emit-vm-add lowers to MOV dst,lhs before ADD dst,rhs to satisfy x86 two-address form."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc::emit-vm-add
                   (cl-cc::make-vm-add :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (assert-= 6 (length bytes))
    ;; MOV rax,rcx / ADD rax,rdx
    (assert-= #x48 (nth 0 bytes))
    (assert-= #x89 (nth 1 bytes))
    (assert-= #xC8 (nth 2 bytes))
    (assert-= #x48 (nth 3 bytes))
    (assert-= #x01 (nth 4 bytes))
    (assert-= #xD0 (nth 5 bytes))))

(deftest x86-64-select-emitter-encoding
  "emit-vm-select uses MOV + TEST + CMOVNE branchless lowering."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc::emit-vm-select
                   (cl-cc::make-vm-select :dst :R0 :cond-reg :R1 :then-reg :R2 :else-reg :R3)
                   s)))))
    (assert-= 10 (length bytes))
    (assert-= #x48 (nth 0 bytes))
    (assert-= #x89 (nth 1 bytes))
    (assert-= #x48 (nth 3 bytes))
    (assert-= #x85 (nth 4 bytes))
    (assert-= #x48 (nth 6 bytes))
    (assert-= #x0F (nth 7 bytes))
    (assert-= #x45 (nth 8 bytes))))

(deftest x86-64-jump-zero-test-je-adjacent
  "emit-vm-jump-zero-inst emits TEST immediately followed by JE rel32." 
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc::emit-vm-jump-zero-inst
                   (cl-cc::make-vm-jump-zero :reg :R1 :label "L1")
                   s 0 (let ((ht (make-hash-table :test #'equal)))
                         (setf (gethash "L1" ht) 9)
                         ht))))))
    (assert-= 9 (length bytes))
    (assert-= #x48 (nth 0 bytes))
    (assert-= #x85 (nth 1 bytes))
    (assert-= #x0F (nth 3 bytes))
    (assert-= #x84 (nth 4 bytes))))

(deftest x86-64-logcount-emitter-encoding
  "emit-vm-logcount emits POPCNT with the expected opcode sequence."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc::emit-vm-logcount
                   (cl-cc::make-vm-logcount :dst :R0 :src :R1) s)))))
    (assert-= 5 (length bytes))
    (assert-= #xF3 (nth 0 bytes))
    (assert-= #x48 (nth 1 bytes))
    (assert-= #x0F (nth 2 bytes))
    (assert-= #xB8 (nth 3 bytes))))

(deftest x86-64-integer-length-emitter-encoding
  "emit-vm-integer-length emits xor/test/je/bsr/add sequence."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc::emit-vm-integer-length
                   (cl-cc::make-vm-integer-length :dst :R0 :src :R1) s)))))
    (assert-= 16 (length bytes))
    ;; xor rax,rax / test rcx,rcx / je rel8 / bsr rax,rcx / add rax,1
    (assert-= #x48 (nth 0 bytes))
    (assert-= #x31 (nth 1 bytes))
    (assert-= #x48 (nth 3 bytes))
    (assert-= #x85 (nth 4 bytes))
    (assert-= #x74 (nth 6 bytes))
    (assert-= #x48 (nth 8 bytes))
    (assert-= #x0F (nth 9 bytes))
    (assert-= #xBD (nth 10 bytes))))

(deftest x86-64-call-emitter-encoding
  "emit-vm-call-like-inst emits CALL r64 followed by MOV dst, rax."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc::emit-vm-call-like-inst
                   (cl-cc::make-vm-call :dst :R0 :func :R1 :args nil) s)))))
    (assert-= 6 (length bytes))
    (assert-= #x48 (nth 0 bytes))
    (assert-= #xFF (nth 1 bytes))
    (assert-= #xD1 (nth 2 bytes))
    (assert-= #x48 (nth 3 bytes))
    (assert-= #x89 (nth 4 bytes))
    (assert-= #xC0 (nth 5 bytes))))

(deftest x86-64-tail-call-emitter-encoding
  "emit-vm-tail-call-inst emits JMP r64 with no destination move."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc::emit-vm-tail-call-inst
                   (cl-cc::make-vm-tail-call :dst :R0 :func :R1 :args nil) s)))))
    (assert-= 3 (length bytes))
    (assert-= #x48 (nth 0 bytes))
    (assert-= #xFF (nth 1 bytes))
    (assert-= #xE1 (nth 2 bytes))))

;;; ─── Comparison emitter byte content ────────────────────────────────────────
;;;
;;; Each comparison emitter emits: CMP(3) + SETcc(3) + MOVZX(4) = 10 bytes
;;; (when all three registers are low regs R0/R1/R2 = rax/rcx/rdx, no REX on SETcc).
;;; The SETcc sub-sequence is at offset [3]: 0F <opcode2> ModRM.
;;; So byte index 4 is the condition opcode distinguishing each comparison.

(deftest-each x86-64-comparison-emitter-setcc-opcode
  "Each comparison emitter embeds the correct SETcc condition opcode at byte index 4."
  :cases (("vm-lt"     (lambda (s) (cl-cc::emit-vm-lt
                          (cl-cc::make-vm-lt :dst :R0 :lhs :R1 :rhs :R2) s))    #x9C)
          ("vm-gt"     (lambda (s) (cl-cc::emit-vm-gt
                          (cl-cc::make-vm-gt :dst :R0 :lhs :R1 :rhs :R2) s))    #x9F)
          ("vm-le"     (lambda (s) (cl-cc::emit-vm-le
                          (cl-cc::make-vm-le :dst :R0 :lhs :R1 :rhs :R2) s))    #x9E)
          ("vm-ge"     (lambda (s) (cl-cc::emit-vm-ge
                          (cl-cc::make-vm-ge :dst :R0 :lhs :R1 :rhs :R2) s))    #x9D)
          ("vm-num-eq" (lambda (s) (cl-cc::emit-vm-num-eq
                          (cl-cc::make-vm-num-eq :dst :R0 :lhs :R1 :rhs :R2) s)) #x94)
          ("vm-eq"     (lambda (s) (cl-cc::emit-vm-eq
                          (cl-cc::make-vm-eq :dst :R0 :lhs :R1 :rhs :R2) s))    #x94))
  (emit-fn expected-opcode2)
  (let* ((bytes (%x86-collect-bytes emit-fn))
         ;; CMP rax,rcx = 3 bytes; SETcc sequence starts at offset 3.
         ;; SETcc on low reg (rax=0): 0F <opcode2> ModRM -- opcode2 is at index 4.
         (setcc-opcode2 (nth 4 bytes)))
    (assert-= 10 (length bytes))
    (assert-= expected-opcode2 setcc-opcode2)))

(deftest-each x86-64-comparison-emitter-cmp-opcode
  "All comparison emitters begin with a CMP rr64 whose opcode byte is #x39."
  :cases (("vm-lt"     (lambda (s) (cl-cc::emit-vm-lt
                          (cl-cc::make-vm-lt :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("vm-gt"     (lambda (s) (cl-cc::emit-vm-gt
                          (cl-cc::make-vm-gt :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("vm-le"     (lambda (s) (cl-cc::emit-vm-le
                          (cl-cc::make-vm-le :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("vm-ge"     (lambda (s) (cl-cc::emit-vm-ge
                          (cl-cc::make-vm-ge :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("vm-num-eq" (lambda (s) (cl-cc::emit-vm-num-eq
                          (cl-cc::make-vm-num-eq :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("vm-eq"     (lambda (s) (cl-cc::emit-vm-eq
                          (cl-cc::make-vm-eq :dst :R0 :lhs :R1 :rhs :R2) s))))
  (emit-fn)
  ;; REX.W prefix at byte 0; CMP opcode #x39 at byte 1
  (let ((bytes (%x86-collect-bytes emit-fn)))
    (assert-= #x39 (nth 1 bytes))))

(deftest x86-64-num-eq-and-eq-share-encoding
  "vm-num-eq and vm-eq use identical byte sequences (both use SETE/#x94)."
  (let ((num-eq-bytes (%x86-collect-bytes
                       (lambda (s) (cl-cc::emit-vm-num-eq
                                    (cl-cc::make-vm-num-eq :dst :R0 :lhs :R1 :rhs :R2) s))))
        (eq-bytes (%x86-collect-bytes
                   (lambda (s) (cl-cc::emit-vm-eq
                                (cl-cc::make-vm-eq :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (assert-equal num-eq-bytes eq-bytes)))

;;; ─── Unary emitter byte content ──────────────────────────────────────────────
;;;
;;; vm-neg:    MOV dst←src (3) + NEG dst (3) = 6 bytes
;;; vm-lognot: MOV dst←src (3) + NOT dst (3) = 6 bytes
;;; vm-not:    TEST src,src (3) + SETE dst (3) + MOVZX dst,dst8 (4) = 10 bytes
;;; vm-inc:    MOV dst←src (3) + ADD dst,1 imm8 (4) = 7 bytes
;;; vm-dec:    MOV dst←src (3) + SUB dst,1 imm8 (4) = 7 bytes

(deftest-each x86-64-unary-emitter-byte-count
  "Unary VM instruction emitters produce the correct total byte count."
  :cases (("vm-neg"    (lambda (s) (cl-cc::emit-vm-neg
                          (cl-cc::make-vm-neg :dst :R0 :src :R1) s))    6)
          ("vm-lognot" (lambda (s) (cl-cc::emit-vm-lognot
                          (cl-cc::make-vm-lognot :dst :R0 :src :R1) s)) 6)
          ("vm-not"    (lambda (s) (cl-cc::emit-vm-not
                          (cl-cc::make-vm-not :dst :R0 :src :R1) s))   10)
          ("vm-inc"    (lambda (s) (cl-cc::emit-vm-inc
                          (cl-cc::make-vm-inc :dst :R0 :src :R1) s))    7)
          ("vm-dec"    (lambda (s) (cl-cc::emit-vm-dec
                          (cl-cc::make-vm-dec :dst :R0 :src :R1) s))    7))
  (emit-fn expected-size)
  (assert-= expected-size (length (%x86-collect-bytes emit-fn))))

(deftest x86-64-unary-neg-starts-with-mov
  "vm-neg begins with MOV dst←src: REX.W(#x48) at byte 0, MOV opcode(#x89) at byte 1."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-neg
                             (cl-cc::make-vm-neg :dst :R0 :src :R1) s)))))
    (assert-= #x48 (nth 0 bytes))
    (assert-= #x89 (nth 1 bytes))))

(deftest x86-64-unary-not-uses-sete
  "vm-not uses TEST+SETE: byte 0 is REX.W (#x48) for TEST; byte 4 is #x0F (SETcc escape)."
  (let ((bytes (%x86-collect-bytes
                (lambda (s) (cl-cc::emit-vm-not
                             (cl-cc::make-vm-not :dst :R0 :src :R1) s)))))
    ;; TEST rcx,rcx: REX.W(#x48) at byte 0
    (assert-= #x48 (nth 0 bytes))
    ;; SETE starts at offset 3: 0F at index 3
    (assert-= #x0F (nth 3 bytes))
    ;; opcode2 = #x94 (SETE) at index 4
    (assert-= #x94 (nth 4 bytes))))

(deftest x86-64-unary-inc-dec-use-add-sub
  "vm-inc uses ADD imm8 and vm-dec uses SUB imm8 as their second instruction."
  (let ((inc-bytes (%x86-collect-bytes
                   (lambda (s) (cl-cc::emit-vm-inc
                                (cl-cc::make-vm-inc :dst :R0 :src :R1) s))))
        (dec-bytes (%x86-collect-bytes
                   (lambda (s) (cl-cc::emit-vm-dec
                                (cl-cc::make-vm-dec :dst :R0 :src :R1) s)))))
    ;; After MOV (3 bytes), ADD/SUB ri8 begins; opcode byte at index 4 (REX at 3, opcode at 4)
    ;; ADD r/m64, imm8 = #x83 /0; SUB r/m64, imm8 = #x83 /5 -- same opcode #x83, different ModRM
    (assert-= #x83 (nth 4 inc-bytes))
    (assert-= #x83 (nth 4 dec-bytes))
    ;; The immediate byte (last byte) is 1 in both cases
    (assert-= 1 (car (last inc-bytes)))
    (assert-= 1 (car (last dec-bytes)))))

;;; ─── build-label-offsets ────────────────────────────────────────────────────

(deftest x86-64-build-label-offsets-empty
  "build-label-offsets returns an empty table for empty instruction list."
  (let ((offsets (cl-cc::build-label-offsets '() 0)))
    (assert-= 0 (hash-table-count offsets))))

(deftest x86-64-build-label-offsets-simple
  "build-label-offsets assigns offset 0 to first label (no prologue)."
  (let* ((lbl (cl-cc::make-vm-label :name "entry"))
         (offsets (cl-cc::build-label-offsets (list lbl) 0)))
    (assert-= 0 (gethash "entry" offsets))))

(deftest x86-64-build-label-offsets-with-prologue
  "build-label-offsets offsets labels by prologue-size."
  (let* ((lbl (cl-cc::make-vm-label :name "start"))
         (offsets (cl-cc::build-label-offsets (list lbl) 6)))
    (assert-= 6 (gethash "start" offsets))))

(deftest x86-64-build-label-offsets-after-const
  "build-label-offsets accounts for instruction sizes before label."
  ;; vm-const = 10 bytes; label after it should be at offset 0+10 = 10
  (let* ((const-inst (cl-cc::make-vm-const :dst :R0 :value 42))
         (lbl (cl-cc::make-vm-label :name "after-const"))
         (offsets (cl-cc::build-label-offsets (list const-inst lbl) 0)))
    (assert-= 10 (gethash "after-const" offsets))))

(deftest x86-64-build-label-offsets-account-for-elided-self-move
  "build-label-offsets does not advance offsets for self-moves elided at emit time."
  (let* ((insts (list (cl-cc::make-vm-move :dst :R0 :src :R0)
                      (cl-cc::make-vm-label :name "after-self-move")
                      (cl-cc::make-vm-halt :reg :R0)))
         (offsets (let ((cl-cc::*current-regalloc* nil))
                    (cl-cc::build-label-offsets insts 0))))
    (assert-= 0 (gethash "after-self-move" offsets))))
