;;;; tests/unit/emit/x86-64-codegen-emitter-tests.lisp — x86-64 Comparison/Unary Emitter Tests
;;;;
;;;; Tests for comparison emitter byte content, unary emitter byte content,
;;;; and build-label-offsets from src/emit/x86-64-codegen.lisp.

(in-package :cl-cc/test)

(in-suite x86-64-codegen-suite)

;;; ─── Comparison emitter byte content ────────────────────────────────────────
;;;
;;; Each comparison emitter emits: CMP(3) + SETcc(3) + MOVZX(4) = 10 bytes
;;; (when all three registers are low regs R0/R1/R2 = rax/rcx/rdx, no REX on SETcc).
;;; The SETcc sub-sequence is at offset [3]: 0F <opcode2> ModRM.
;;; So byte index 4 is the condition opcode distinguishing each comparison.

(deftest-each x86-64-comparison-emitter-setcc-opcode
  "Each comparison emitter embeds the correct SETcc condition opcode at byte index 4."
  :cases (("vm-lt"     (lambda (s) (cl-cc/emit::emit-vm-lt
                          (cl-cc::make-vm-lt :dst :R0 :lhs :R1 :rhs :R2) s))    #x9C)
          ("vm-gt"     (lambda (s) (cl-cc/emit::emit-vm-gt
                          (cl-cc::make-vm-gt :dst :R0 :lhs :R1 :rhs :R2) s))    #x9F)
          ("vm-le"     (lambda (s) (cl-cc/emit::emit-vm-le
                          (cl-cc::make-vm-le :dst :R0 :lhs :R1 :rhs :R2) s))    #x9E)
          ("vm-ge"     (lambda (s) (cl-cc/emit::emit-vm-ge
                          (cl-cc::make-vm-ge :dst :R0 :lhs :R1 :rhs :R2) s))    #x9D)
          ("vm-num-eq" (lambda (s) (cl-cc/emit::emit-vm-num-eq
                          (cl-cc::make-vm-num-eq :dst :R0 :lhs :R1 :rhs :R2) s)) #x94)
          ("vm-eq"     (lambda (s) (cl-cc/emit::emit-vm-eq
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
  :cases (("vm-lt"     (lambda (s) (cl-cc/emit::emit-vm-lt
                          (cl-cc::make-vm-lt :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("vm-gt"     (lambda (s) (cl-cc/emit::emit-vm-gt
                          (cl-cc::make-vm-gt :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("vm-le"     (lambda (s) (cl-cc/emit::emit-vm-le
                          (cl-cc::make-vm-le :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("vm-ge"     (lambda (s) (cl-cc/emit::emit-vm-ge
                          (cl-cc::make-vm-ge :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("vm-num-eq" (lambda (s) (cl-cc/emit::emit-vm-num-eq
                          (cl-cc::make-vm-num-eq :dst :R0 :lhs :R1 :rhs :R2) s)))
          ("vm-eq"     (lambda (s) (cl-cc/emit::emit-vm-eq
                          (cl-cc::make-vm-eq :dst :R0 :lhs :R1 :rhs :R2) s))))
  (emit-fn)
  ;; REX.W prefix at byte 0; CMP opcode #x39 at byte 1
  (let ((bytes (%x86-collect-bytes emit-fn)))
    (assert-= #x39 (nth 1 bytes))))

(deftest x86-64-num-eq-and-eq-share-encoding
  "vm-num-eq and vm-eq use identical byte sequences (both use SETE/#x94)."
  (let ((num-eq-bytes (%x86-collect-bytes
                       (lambda (s) (cl-cc/emit::emit-vm-num-eq
                                    (cl-cc::make-vm-num-eq :dst :R0 :lhs :R1 :rhs :R2) s))))
        (eq-bytes (%x86-collect-bytes
                   (lambda (s) (cl-cc/emit::emit-vm-eq
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
  :cases (("vm-neg"    (lambda (s) (cl-cc/emit::emit-vm-neg
                          (cl-cc::make-vm-neg :dst :R0 :src :R1) s))    6)
          ("vm-lognot" (lambda (s) (cl-cc/emit::emit-vm-lognot
                          (cl-cc::make-vm-lognot :dst :R0 :src :R1) s)) 6)
          ("vm-not"    (lambda (s) (cl-cc/emit::emit-vm-not
                          (cl-cc::make-vm-not :dst :R0 :src :R1) s))   10)
          ("vm-inc"    (lambda (s) (cl-cc/emit::emit-vm-inc
                          (cl-cc::make-vm-inc :dst :R0 :src :R1) s))    7)
          ("vm-dec"    (lambda (s) (cl-cc/emit::emit-vm-dec
                          (cl-cc::make-vm-dec :dst :R0 :src :R1) s))    7))
  (emit-fn expected-size)
  (assert-= expected-size (length (%x86-collect-bytes emit-fn))))

(deftest x86-64-unary-encoding-details
  "vm-neg starts with MOV (#x48,#x89); vm-not uses TEST+SETE (#x0F at byte 3); vm-inc/dec use #x83 ADD/SUB."
  (let ((neg-bytes (%x86-collect-bytes
                    (lambda (s) (cl-cc/emit::emit-vm-neg
                                 (cl-cc::make-vm-neg :dst :R0 :src :R1) s)))))
    (assert-= #x48 (nth 0 neg-bytes))
    (assert-= #x89 (nth 1 neg-bytes)))
  (let ((not-bytes (%x86-collect-bytes
                    (lambda (s) (cl-cc/emit::emit-vm-not
                                 (cl-cc::make-vm-not :dst :R0 :src :R1) s)))))
    (assert-= #x48 (nth 0 not-bytes))
    (assert-= #x0F (nth 3 not-bytes))
    (assert-= #x94 (nth 4 not-bytes)))
  (let ((inc-bytes (%x86-collect-bytes
                    (lambda (s) (cl-cc/emit::emit-vm-inc
                                 (cl-cc::make-vm-inc :dst :R0 :src :R1) s))))
        (dec-bytes (%x86-collect-bytes
                    (lambda (s) (cl-cc/emit::emit-vm-dec
                                 (cl-cc::make-vm-dec :dst :R0 :src :R1) s)))))
    (assert-= #x83 (nth 4 inc-bytes))
    (assert-= #x83 (nth 4 dec-bytes))
    (assert-= 1 (car (last inc-bytes)))
    (assert-= 1 (car (last dec-bytes)))))

;;; ─── build-label-offsets ────────────────────────────────────────────────────

(deftest x86-64-build-label-offsets-empty
  "build-label-offsets on empty instruction list returns a hash with zero entries."
  (assert-= 0 (hash-table-count (cl-cc/emit::build-label-offsets '() 0))))

(deftest x86-64-build-label-offsets-first-label-at-zero
  "A label as the first instruction maps to offset 0."
  (let* ((lbl (cl-cc::make-vm-label :name "entry"))
         (offsets (cl-cc/emit::build-label-offsets (list lbl) 0)))
    (assert-= 0 (gethash "entry" offsets))))

(deftest x86-64-build-label-offsets-prologue-offset
  "A non-zero prologue size shifts the label offset accordingly."
  (let* ((lbl (cl-cc::make-vm-label :name "start"))
         (offsets (cl-cc/emit::build-label-offsets (list lbl) 6)))
    (assert-= 6 (gethash "start" offsets))))

(deftest x86-64-build-label-offsets-after-const-is-10
  "A vm-const instruction contributes 10 bytes, so a label after it maps to offset 10."
  (let* ((const-inst (cl-cc::make-vm-const :dst :R0 :value 42))
         (lbl (cl-cc::make-vm-label :name "after-const"))
         (offsets (cl-cc/emit::build-label-offsets (list const-inst lbl) 0)))
    (assert-= 10 (gethash "after-const" offsets))))

(deftest x86-64-build-label-offsets-elided-self-move
  "A self-move (dst = src) is elided; label after it stays at offset 0."
  (let* ((insts (list (cl-cc::make-vm-move :dst :R0 :src :R0)
                      (cl-cc::make-vm-label :name "after-self-move")
                      (cl-cc::make-vm-halt :reg :R0)))
         (offsets (let ((cl-cc/emit::*current-regalloc* nil))
                    (cl-cc/emit::build-label-offsets insts 0))))
    (assert-= 0 (gethash "after-self-move" offsets))))
