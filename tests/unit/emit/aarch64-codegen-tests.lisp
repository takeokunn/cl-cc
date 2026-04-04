;;;; tests/unit/emit/aarch64-codegen-tests.lisp
;;;; Unit tests for compile-to-aarch64-bytes (src/emit/aarch64-codegen.lisp)
;;;;
;;;; Covers:
;;;;   - Return type: must be (array (unsigned-byte 8) (*))
;;;;   - Non-empty output for any valid program
;;;;   - AArch64 ISA alignment invariant (instruction width = 4 bytes)
;;;;   - Regression guard: arm64 and x86-64 backends produce different bytes

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Helper
;;; ─────────────────────────────────────────────────────────────────────────

(defun %a64-compile (source)
  "Compile SOURCE string to AArch64 machine-code bytes.
Returns the byte vector, or NIL on error."
  (ignore-errors
    (compile-to-aarch64-bytes
     (compilation-result-program
      (compile-string source :target :vm)))))

(defun %a64-collect-bytes (emit-fn)
  "Collect bytes emitted by a native AArch64 emitter."
  (let ((bytes nil))
    (funcall emit-fn (lambda (b) (push b bytes)))
    (nreverse bytes)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Return-type contract
;;; ─────────────────────────────────────────────────────────────────────────

(deftest aarch64-bytes-output-contract
  "compile-to-aarch64-bytes returns a non-empty (unsigned-byte 8) vector with length divisible by 4."
  (let ((bytes (%a64-compile "(+ 1 2)")))
    (assert-true bytes)
    (assert-true (typep bytes '(array (unsigned-byte 8) (*))))
    (assert-true (> (length bytes) 0))
    (assert-= 0 (mod (length bytes) 4))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; AArch64 ISA alignment: all instructions are 4 bytes
;;; ─────────────────────────────────────────────────────────────────────────

;;; ─────────────────────────────────────────────────────────────────────────
;;; Regression guard: arm64 != x86-64
;;; This test would have FAILED before the fix because compile-file-to-native
;;; called compile-to-x86-64-bytes for both architectures.
;;; ─────────────────────────────────────────────────────────────────────────

(deftest aarch64-bytes-distinct-from-x86-64
  "AArch64 and x86-64 backends produce different byte sequences for the same program."
  (let* ((program (compilation-result-program
                   (compile-string "(+ 1 2)" :target :vm)))
         (a64 (ignore-errors (compile-to-aarch64-bytes program)))
         (x64 (ignore-errors (compile-to-x86-64-bytes program))))
    (assert-true a64)
    (assert-true x64)
    (assert-false (equalp a64 x64))))

(deftest aarch64-empty-program-trims-unused-callee-saved-regs
  "compile-to-aarch64-bytes emits only FP/LR save/restore and RET for an empty program."
  (let* ((program (cl-cc::make-vm-program :instructions nil :result-register :R0))
         (bytes (compile-to-aarch64-bytes program)))
    (assert-= 12 (length bytes))))

(deftest aarch64-bswap-emitter-encoding
  "emit-a64-vm-bswap emits a single REV Wd, Wn instruction."
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc::emit-a64-vm-bswap (cl-cc::make-vm-bswap :dst :R0 :src :R1) s)))))
    (assert-= 4 (length bytes))
    (assert-= #x20 (nth 0 bytes))
    (assert-= #x08 (nth 1 bytes))
    (assert-= #xC0 (nth 2 bytes))
    (assert-= #x5A (nth 3 bytes))))

(deftest aarch64-bswap-instruction-size
  "vm-bswap is accounted for in the AArch64 instruction-size table."
  (assert-= 4 (gethash 'cl-cc::vm-bswap cl-cc::*a64-instruction-sizes*)))

(deftest aarch64-tail-call-instruction-size
  "vm-tail-call is accounted for in the AArch64 instruction-size table."
  (assert-= 4 (gethash 'cl-cc::vm-tail-call cl-cc::*a64-instruction-sizes*)))

(deftest aarch64-min-max-instruction-sizes
  "vm-min and vm-max are accounted for in the AArch64 instruction-size table."
  (assert-= 8 (gethash 'cl-cc::vm-min cl-cc::*a64-instruction-sizes*))
  (assert-= 8 (gethash 'cl-cc::vm-max cl-cc::*a64-instruction-sizes*)))

(deftest aarch64-integer-arith-instruction-sizes
  "Integer-specialized arithmetic aliases are accounted for in the AArch64 size table."
  (assert-= 4 (gethash 'cl-cc::vm-integer-add cl-cc::*a64-instruction-sizes*))
  (assert-= 4 (gethash 'cl-cc::vm-integer-sub cl-cc::*a64-instruction-sizes*))
  (assert-= 4 (gethash 'cl-cc::vm-integer-mul cl-cc::*a64-instruction-sizes*)))

(deftest aarch64-vm-move-self-is-elided
  "vm-move to the same physical register emits no bytes on AArch64."
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc::emit-a64-vm-move
                   (cl-cc::make-vm-move :dst :R0 :src :R0) s)))))
    (assert-= 0 (length bytes))))

(deftest aarch64-scs-single-register-encodings
  "Shadow call stack helper encodings for STR-post, LDR-pre, B.cond, and BRK are stable."
  (let ((store (%a64-collect-bytes (lambda (s) (cl-cc::emit-a64-instr (cl-cc::encode-str-post 30 18 8) s))))
        (load  (%a64-collect-bytes (lambda (s) (cl-cc::emit-a64-instr (cl-cc::encode-ldr-pre 17 18 -8) s))))
        (beq   (%a64-collect-bytes (lambda (s) (cl-cc::emit-a64-instr (cl-cc::encode-b-cond 2 0) s))))
        (brk   (%a64-collect-bytes (lambda (s) (cl-cc::emit-a64-instr (cl-cc::encode-brk 0) s)))))
    (assert-equal '(94 134 0 248) store)
    (assert-equal '(81 142 95 248) load)
    (assert-equal '(64 0 0 84) beq)
    (assert-equal '(0 0 32 212) brk)))

(deftest aarch64-instruction-size-vm-move-self-is-zero
  "a64-instruction-size returns 0 for self-moves elided at emit time."
  (assert-= 0 (cl-cc::a64-instruction-size (cl-cc::make-vm-move :dst :R0 :src :R0))))

(deftest aarch64-min-max-emitter-table-entries
  "vm-min and vm-max are present in the AArch64 emitter table."
  (assert-true (functionp (gethash 'cl-cc::vm-min cl-cc::*a64-emitter-table*)))
  (assert-true (functionp (gethash 'cl-cc::vm-max cl-cc::*a64-emitter-table*))))

(deftest aarch64-integer-arith-emitter-table-entries
  "Integer-specialized arithmetic aliases are present in the AArch64 emitter table."
  (assert-true (functionp (gethash 'cl-cc::vm-integer-add cl-cc::*a64-emitter-table*)))
  (assert-true (functionp (gethash 'cl-cc::vm-integer-sub cl-cc::*a64-emitter-table*)))
  (assert-true (functionp (gethash 'cl-cc::vm-integer-mul cl-cc::*a64-emitter-table*))))

(deftest aarch64-tail-call-emitter-encoding
  "vm-tail-call emits BR Xn on AArch64."
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc::emit-a64-instruction
                   (cl-cc::make-vm-tail-call :dst :R0 :func :R1 :args nil)
                   s 0 (make-hash-table :test #'eq))))))
    (assert-= 4 (length bytes))
    (assert-= #x20 (nth 0 bytes))
    (assert-= #x00 (nth 1 bytes))
    (assert-= #x1F (nth 2 bytes))
    (assert-= #xD6 (nth 3 bytes))))

(deftest aarch64-build-label-offsets-account-for-elided-self-move
  "build-a64-label-offsets does not advance offsets for self-moves elided at emit time."
  (let* ((insts (list (cl-cc::make-vm-move :dst :R0 :src :R0)
                      (cl-cc::make-vm-label :name "after-self-move")
                      (cl-cc::make-vm-halt :reg :R0)))
         (offsets (cl-cc::build-a64-label-offsets insts 0)))
    (assert-= 0 (gethash "after-self-move" offsets))))

(deftest aarch64-min-max-emitter-encoding
  "emit-a64-vm-min/max emit CMP followed by CSEL on AArch64."
  (let ((min-bytes (%a64-collect-bytes
                    (lambda (s)
                      (cl-cc::emit-a64-vm-min
                       (cl-cc::make-vm-min :dst :R0 :lhs :R1 :rhs :R2) s))))
        (max-bytes (%a64-collect-bytes
                    (lambda (s)
                      (cl-cc::emit-a64-vm-max
                       (cl-cc::make-vm-max :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (assert-= 8 (length min-bytes))
    (assert-= 8 (length max-bytes))
    ;; CMP X1, X2 => 3F 00 02 EB; CSEL X0, X1, X2, LT => 20 B0 82 9A
    (assert-= #x3F (nth 0 min-bytes))
    (assert-= #x00 (nth 1 min-bytes))
    (assert-= #x02 (nth 2 min-bytes))
    (assert-= #xEB (nth 3 min-bytes))
    (assert-= #x20 (nth 4 min-bytes))
    (assert-= #xB0 (nth 5 min-bytes))
    (assert-= #x82 (nth 6 min-bytes))
    (assert-= #x9A (nth 7 min-bytes))
    ;; CMP X1, X2 => 3F 00 02 EB; CSEL X0, X1, X2, GT => 20 C0 82 9A
    (assert-= #x3F (nth 0 max-bytes))
    (assert-= #x00 (nth 1 max-bytes))
    (assert-= #x02 (nth 2 max-bytes))
    (assert-= #xEB (nth 3 max-bytes))
    (assert-= #x20 (nth 4 max-bytes))
    (assert-= #xC0 (nth 5 max-bytes))
    (assert-= #x82 (nth 6 max-bytes))
    (assert-= #x9A (nth 7 max-bytes))))

(deftest aarch64-select-emitter-encoding
  "emit-a64-vm-select emits MOV + CMP + CSEL on AArch64."
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc::emit-a64-vm-select
                   (cl-cc::make-vm-select :dst :R0 :cond-reg :R1 :then-reg :R2 :else-reg :R3)
                   s)))))
    (assert-= 12 (length bytes))
    ;; First instruction is MOV X0, X3; second is CMP X1, XZR; third is CSEL
    (assert-= #xE0 (nth 0 bytes))
    (assert-= #x03 (nth 1 bytes))
    (assert-= #x1F (nth 6 bytes))
    (assert-= #x9A (nth 11 bytes))))

(deftest aarch64-jump-zero-uses-cbz
  "emit-a64-vm-jump-zero emits a single CBZ instruction." 
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc::emit-a64-vm-jump-zero
                   (cl-cc::make-vm-jump-zero :reg :R1 :label "L1")
                   s 0 (let ((ht (make-hash-table :test #'equal)))
                         (setf (gethash "L1" ht) 4)
                         ht))))))
    (assert-= 4 (length bytes))
    ;; low byte and high opcode nibble are enough to prove CBZ path, not CMP+B.EQ
    (assert-= #x01 (logand (nth 0 bytes) #x1F))
    (assert-= #xB4 (nth 3 bytes))))

(deftest aarch64-leaf-program-trims-prologue-through-pipeline
  "A real compiled leaf program reaches native codegen and trims the prologue."
  (let* ((result (compile-string "(+ 1 2)" :target :vm))
         (program (compilation-result-program result))
         (base (cl-cc::make-vm-program :instructions (cl-cc::vm-program-instructions program)
                                       :result-register (cl-cc::vm-program-result-register program)
                                       :leaf-p nil))
         (leaf-bytes (compile-to-aarch64-bytes program))
         (nonleaf-bytes (compile-to-aarch64-bytes base)))
    (assert-true (cl-cc::vm-program-leaf-p program))
    (assert-true (< (length leaf-bytes) (length nonleaf-bytes)))))

(deftest aarch64-empty-program-includes-shadow-call-stack
  "Empty AArch64 programs include the shadow call stack prologue/epilogue sequence."
  (let* ((program (cl-cc::make-vm-program :instructions nil :result-register :R0))
         (bytes (compile-to-aarch64-bytes program)))
    (assert-= 32 (length bytes))
    ;; Prologue begins with STR LR, [X18], #8
    (assert-= 94 (nth 0 bytes))
    (assert-= 134 (nth 1 bytes))
    (assert-= 0 (nth 2 bytes))
    (assert-= 248 (nth 3 bytes))
    ;; Normal FP/LR save and restore remain present.
    (assert-= 253 (nth 4 bytes))
    (assert-= 123 (nth 5 bytes))
    (assert-= 191 (nth 6 bytes))
    (assert-= 169 (nth 7 bytes))
    ;; Epilogue contains BRK #0 just before final RET.
    (assert-= 0 (nth 24 bytes))
    (assert-= 0 (nth 25 bytes))
    (assert-= 32 (nth 26 bytes))
    (assert-= 212 (nth 27 bytes))
    (assert-= #xC0 (nth 28 bytes))
    (assert-= #x03 (nth 29 bytes))
    (assert-= #x5F (nth 30 bytes))
    (assert-= #xD6 (nth 31 bytes))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Variety of programs
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each aarch64-bytes-various-programs
  "compile-to-aarch64-bytes handles a range of program shapes without error."
  :cases (("constant"    "42")
          ("arithmetic"  "(+ 3 4)")
          ("let"         "(let ((x 10)) (* x 2))")
          ("if"          "(if 1 100 200)")
          ("nested"      "(+ (* 2 3) (- 10 4))"))
  (source)
  (let ((bytes (%a64-compile source)))
    (assert-true bytes)
    (assert-true (> (length bytes) 0))
    (assert-= 0 (mod (length bytes) 4))))
