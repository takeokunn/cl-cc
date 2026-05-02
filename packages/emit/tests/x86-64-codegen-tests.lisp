;;;; tests/unit/emit/x86-64-codegen-tests.lisp — x86-64 Code Generation Tests
;;;;
;;;; Tests for src/emit/x86-64-codegen.lisp:
;;;; *vm-reg-map*, *phys-reg-to-x86-code*, vm-reg-to-x86,
;;;; vm-const-to-integer, instruction-size, build-label-offsets,
;;;; *x86-64-emitter-entries*, *x86-64-emitter-table*

(in-package :cl-cc/test)

(defsuite x86-64-codegen-suite :description "x86-64 machine code generation tests"
  :parent cl-cc-unit-suite)


(in-suite x86-64-codegen-suite)
;;; ─── *vm-reg-map* ───────────────────────────────────────────────────────────

(deftest x86-64-reg-map-lengths
  "*vm-reg-map* has 8 entries; *phys-reg-to-x86-code* has 14 entries."
  (assert-= 8  (length cl-cc/codegen::*vm-reg-map*))
  (assert-= 14 (length cl-cc/codegen::*phys-reg-to-x86-code*)))

(deftest-each x86-64-vm-reg-map-entries
  "*vm-reg-map* maps each VM register to the correct x86-64 code."
  :cases (("R0→rax" :R0 cl-cc/codegen::+rax+)
          ("R1→rcx" :R1 cl-cc/codegen::+rcx+)
          ("R2→rdx" :R2 cl-cc/codegen::+rdx+)
          ("R3→rbx" :R3 cl-cc/codegen::+rbx+)
          ("R4→rsi" :R4 cl-cc/codegen::+rsi+)
          ("R5→rdi" :R5 cl-cc/codegen::+rdi+)
          ("R6→r8"  :R6 cl-cc/codegen::+r8+)
          ("R7→r9"  :R7 cl-cc/codegen::+r9+))
  (vm-reg expected)
  (assert-= expected (cdr (assoc vm-reg cl-cc/codegen::*vm-reg-map*))))

;;; ─── *phys-reg-to-x86-code* ─────────────────────────────────────────────────


(deftest-each x86-64-phys-reg-map-entries
  "*phys-reg-to-x86-code* maps each physical register to the correct code."
  :cases (("rax"  :rax  cl-cc/codegen::+rax+)
          ("rcx"  :rcx  cl-cc/codegen::+rcx+)
          ("rdx"  :rdx  cl-cc/codegen::+rdx+)
          ("rbx"  :rbx  cl-cc/codegen::+rbx+)
          ("rsi"  :rsi  cl-cc/codegen::+rsi+)
          ("rdi"  :rdi  cl-cc/codegen::+rdi+)
          ("r8"   :r8   cl-cc/codegen::+r8+)
          ("r9"   :r9   cl-cc/codegen::+r9+)
          ("r10"  :r10  cl-cc/codegen::+r10+)
          ("r11"  :r11  cl-cc/codegen::+r11+)
          ("r12"  :r12  cl-cc/codegen::+r12+)
          ("r13"  :r13  cl-cc/codegen::+r13+)
          ("r14"  :r14  cl-cc/codegen::+r14+)
          ("r15"  :r15  cl-cc/codegen::+r15+))
  (phys-reg expected)
  (assert-= expected (cdr (assoc phys-reg cl-cc/codegen::*phys-reg-to-x86-code*))))

;;; ─── vm-reg-to-x86 ──────────────────────────────────────────────────────────

(deftest-each x86-64-vm-reg-to-x86-naive
  "vm-reg-to-x86 maps VM registers to x86 codes (naive mode, no regalloc)."
  :cases (("R0" :R0 cl-cc/codegen::+rax+)
          ("R1" :R1 cl-cc/codegen::+rcx+)
          ("R2" :R2 cl-cc/codegen::+rdx+)
          ("R3" :R3 cl-cc/codegen::+rbx+)
          ("R4" :R4 cl-cc/codegen::+rsi+)
          ("R5" :R5 cl-cc/codegen::+rdi+)
          ("R6" :R6 cl-cc/codegen::+r8+)
          ("R7" :R7 cl-cc/codegen::+r9+))
  (vm-reg expected)
  (let ((cl-cc/codegen::*current-regalloc* nil))
    (assert-= expected (cl-cc/codegen::vm-reg-to-x86 vm-reg))))

(deftest x86-64-vm-reg-to-x86-unknown-signals
  "vm-reg-to-x86 signals error for unknown register."
  (let ((cl-cc/codegen::*current-regalloc* nil))
    (assert-signals error (cl-cc/codegen::vm-reg-to-x86 :R99))))

;;; ─── vm-const-to-integer ────────────────────────────────────────────────────

(deftest-each x86-64-vm-const-to-integer
  "vm-const-to-integer coerces VM constant values to integers."
  :cases (("nil→0"     nil   0)
          ("t→1"       t     1)
          ("42→42"     42    42)
          ("-7→-7"     -7    -7)
          ("other→0"   :foo  0))
  (input expected)
  (assert-= expected (cl-cc/codegen::vm-const-to-integer input)))

;;; ─── *x86-64-instruction-sizes* ─────────────────────────────────────────────
;;; Note: hash keys are symbols in the cl-cc package; use cl-cc:: prefix.

(deftest-each x86-64-instruction-sizes-spot-checks
  "Known VM instruction types have correct byte sizes in the size table."
  :cases (("vm-const"     'cl-cc/vm::vm-const    10)
           ("vm-move"      'cl-cc/vm::vm-move      3)
           ("vm-add"       'cl-cc/vm::vm-add       6)
           ("vm-integer-add" 'cl-cc/vm::vm-integer-add 6)
           ("vm-sub"       'cl-cc/vm::vm-sub       6)
           ("vm-integer-sub" 'cl-cc/vm::vm-integer-sub 6)
           ("vm-mul"       'cl-cc/vm::vm-mul       7)
           ("vm-integer-mul" 'cl-cc/vm::vm-integer-mul 7)
           ("vm-bswap"     'cl-cc/vm::vm-bswap     6)
          ("vm-halt"      'cl-cc/vm::vm-halt      3)
          ("vm-call"      'cl-cc/vm::vm-call      6)
          ("vm-tail-call" 'cl-cc/vm::vm-tail-call 3)
          ("vm-label"     'cl-cc/vm::vm-label     0)
          ("vm-jump"      'cl-cc/vm::vm-jump      5)
          ("vm-jump-zero" 'cl-cc/vm::vm-jump-zero 9)
          ("vm-ret"       'cl-cc/vm::vm-ret       1)
          ("vm-ash"       'cl-cc/vm::vm-ash      24)
          ("vm-div"       'cl-cc/vm::vm-div      34)
          ("vm-mod"       'cl-cc/vm::vm-mod      37)
          ("vm-abs"       'cl-cc/vm::vm-abs      15)
          ("vm-min"       'cl-cc/vm::vm-min      10)
          ("vm-max"       'cl-cc/vm::vm-max      10))
  (sym expected)
  (assert-= expected (gethash sym cl-cc/codegen::*x86-64-instruction-sizes*)))

(deftest x86-64-instruction-size-checks
  "Comparison ops → 12; null-p → 11; other predicates → 10; self-move → 0."
  (dolist (tp '(cl-cc/vm::vm-lt cl-cc/vm::vm-gt cl-cc/vm::vm-le
                cl-cc/vm::vm-ge cl-cc/vm::vm-num-eq cl-cc/vm::vm-eq))
    (assert-= 12 (gethash tp cl-cc/codegen::*x86-64-instruction-sizes*)))
  (assert-= 11 (gethash 'cl-cc/vm::vm-null-p cl-cc/codegen::*x86-64-instruction-sizes*))
  (dolist (tp '(cl-cc/vm::vm-number-p cl-cc/vm::vm-integer-p cl-cc/vm::vm-cons-p
                cl-cc/vm::vm-symbol-p cl-cc/vm::vm-function-p))
    (assert-= 10 (gethash tp cl-cc/codegen::*x86-64-instruction-sizes*)))
  (let ((cl-cc/codegen::*current-regalloc* nil))
    (assert-= 0 (cl-cc/codegen::instruction-size (cl-cc::make-vm-move :dst :R0 :src :R0)))))

;;; ─── *x86-64-emitter-entries* / *x86-64-emitter-table* ─────────────────────

(deftest x86-64-emitter-table-integrity
  "*x86-64-emitter-entries* has 56 entries; each entry appears in *x86-64-emitter-table*."
  (assert-= 56 (length cl-cc/codegen::*x86-64-emitter-entries*))
  (dolist (entry cl-cc/codegen::*x86-64-emitter-entries*)
    (assert-true (gethash (car entry) cl-cc/codegen::*x86-64-emitter-table*))))

(deftest x86-64-empty-program-minimal-bytes
  "Empty program emits exactly 3 bytes (minimal frame)."
  (let* ((prog (cl-cc/vm::make-vm-program :instructions nil :result-register :R0))
         (bytes (cl-cc/codegen::compile-to-x86-64-bytes prog)))
    (assert-= 3 (length bytes))))

(deftest x86-64-leaf-program-smaller-than-nonleaf
  "Leaf program emits fewer bytes than the same program with leaf-p=nil."
  (let* ((result (compile-string "(+ 1 2)" :target :x86_64))
         (prog (compilation-result-program result))
         (base (cl-cc/vm::make-vm-program
                :instructions (cl-cc/vm::vm-program-instructions prog)
                :result-register (cl-cc/vm::vm-program-result-register prog)
                :leaf-p nil))
         (leaf-bytes    (cl-cc/codegen::compile-to-x86-64-bytes prog))
         (nonleaf-bytes (cl-cc/codegen::compile-to-x86-64-bytes base)))
    (assert-true (cl-cc/vm::vm-program-leaf-p prog))
    (assert-true (< (length leaf-bytes) (length nonleaf-bytes)))))

(deftest-each x86-64-emitter-table-spot-checks
  "Key instructions are present in *x86-64-emitter-table* and are functions."
  :cases (("vm-const"   'cl-cc/vm::vm-const)
           ("vm-add"     'cl-cc/vm::vm-add)
           ("vm-integer-add" 'cl-cc/vm::vm-integer-add)
           ("vm-float-add" 'cl-cc/vm::vm-float-add)
           ("vm-float-div" 'cl-cc/vm::vm-float-div)
           ("vm-call"    'cl-cc/vm::vm-call)
           ("vm-tail-call" 'cl-cc/vm::vm-tail-call)
           ("vm-lt"      'cl-cc/vm::vm-lt)
           ("vm-neg"     'cl-cc/vm::vm-neg)
           ("vm-bswap"   'cl-cc/vm::vm-bswap)
          ("vm-and"     'cl-cc/vm::vm-and)
          ("vm-logand"  'cl-cc/vm::vm-logand)
          ("vm-null-p"  'cl-cc/vm::vm-null-p))
  (sym)
  (assert-true (functionp (gethash sym cl-cc/codegen::*x86-64-emitter-table*))))

(deftest x86-64-float-const-add-program-uses-xmm-path
  "Float const/add/halt emits MOVQ+scalar SSE opcodes instead of integer ALU bytes."
  (let* ((prog (cl-cc/vm::make-vm-program
                :instructions (list (cl-cc::make-vm-const :dst :R0 :value 1.0d0)
                                    (cl-cc::make-vm-const :dst :R1 :value 2.0d0)
                                    (cl-cc::make-vm-float-add :dst :R2 :lhs :R0 :rhs :R1)
                                    (cl-cc::make-vm-halt :reg :R2))
                :result-register :R2))
         (bytes (coerce (cl-cc/codegen::compile-to-x86-64-bytes prog) 'list)))
    (assert-true (search '(#x66 #x49 #x0F #x6E) bytes :test #'eql))
    (assert-true (search '(#xF2 #x0F #x10) bytes :test #'eql))
    (assert-true (search '(#xF2 #x0F #x58) bytes :test #'eql))))

;;; ─── Byte-collection helper ─────────────────────────────────────────────────
;;; Used by x86-64-codegen-emitter-tests and x86-64-codegen-insn-tests,
;;; which are loaded after this file via :serial t ASDF.

(defun %x86-collect-bytes (emit-fn)
  "Call EMIT-FN with a stream that collects bytes. Returns byte list."
  (let ((bytes nil))
    (funcall emit-fn (lambda (b) (push b bytes)))
    (nreverse bytes)))
