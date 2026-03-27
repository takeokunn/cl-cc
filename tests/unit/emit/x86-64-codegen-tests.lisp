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
          ("vm-sub"       'cl-cc::vm-sub       6)
          ("vm-mul"       'cl-cc::vm-mul       7)
          ("vm-halt"      'cl-cc::vm-halt      3)
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

;;; ─── *x86-64-emitter-entries* / *x86-64-emitter-table* ─────────────────────

(deftest x86-64-emitter-entries-count
  "*x86-64-emitter-entries* has 42 entries covering all supported instructions."
  (assert-= 42 (length cl-cc::*x86-64-emitter-entries*)))

(deftest x86-64-emitter-table-built-from-entries
  "*x86-64-emitter-table* contains an entry for each item in *x86-64-emitter-entries*."
  (dolist (entry cl-cc::*x86-64-emitter-entries*)
    (assert-true (gethash (car entry) cl-cc::*x86-64-emitter-table*))))

(deftest-each x86-64-emitter-table-spot-checks
  "Key instructions are present in *x86-64-emitter-table* and are functions."
  :cases (("vm-const"   'cl-cc::vm-const)
          ("vm-add"     'cl-cc::vm-add)
          ("vm-lt"      'cl-cc::vm-lt)
          ("vm-neg"     'cl-cc::vm-neg)
          ("vm-and"     'cl-cc::vm-and)
          ("vm-logand"  'cl-cc::vm-logand)
          ("vm-null-p"  'cl-cc::vm-null-p))
  (sym)
  (assert-true (functionp (gethash sym cl-cc::*x86-64-emitter-table*))))

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
