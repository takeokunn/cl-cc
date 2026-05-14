;;;; tests/unit/emit/aarch64-codegen-tests.lisp
;;;; Unit tests for compile-to-aarch64-bytes (src/emit/aarch64-codegen.lisp)
;;;;
;;;; Covers:
;;;;   - Return type: must be (array (unsigned-byte 8) (*))
;;;;   - Non-empty output for any valid program
;;;;   - AArch64 ISA alignment invariant (instruction width = 4 bytes)
;;;;   - Regression guard: arm64 and x86-64 backends produce different bytes

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

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

(deftest aarch64-fpe-codegen-target-frees-x29
  "Default AArch64 FPE exposes X29 to regalloc; debug opt-out reserves it again."
  (let ((fpe-target (let ((cl-cc/codegen::*a64-omit-frame-pointer* t))
                      (cl-cc/codegen::a64-codegen-target)))
        (debug-target (let ((cl-cc/codegen::*a64-omit-frame-pointer* nil))
                        (cl-cc/codegen::a64-codegen-target))))
    (assert-true (member :x29 (cl-cc/target:target-allocatable-regs fpe-target)))
    (assert-true (member :x29 (cl-cc/target:target-callee-saved fpe-target)))
    (assert-false (member :x29 (cl-cc/target:target-allocatable-regs debug-target)))))

(deftest aarch64-tls-base-register-uses-tpidr-el0-plan
  "AArch64 TLS base register is selected via optimizer TLS planning."
  (assert-eq :tpidr_el0 (cl-cc/codegen::aarch64-tls-base-register)))

(deftest aarch64-atomic-lowering-plan-adds-acq-rel-fences
  "AArch64 acq-rel atomic lowering adds DMB ISH fences around selected opcode."
  (let ((plan (cl-cc/codegen::aarch64-atomic-lowering-plan :cas :acq-rel)))
    (assert-eq :ldxr-stxr (getf plan :opcode))
    (assert-equal '(:dmb-ish) (getf plan :pre-fence))
    (assert-equal '(:dmb-ish) (getf plan :post-fence))))

(deftest aarch64-used-callee-saved-pairs-maps-keyword-registers
  "AArch64 callee-saved detection maps regalloc keyword registers to numeric STP/LDP pairs."
  (let ((assignment (make-hash-table :test #'eq)))
    (setf (gethash :R0 assignment) :x19
          (gethash :R1 assignment) :x29)
    (let ((ra (cl-cc/regalloc::make-regalloc-result :assignment assignment
                                                    :spill-count 0
                                                    :instructions nil)))
      (assert-equal '((29 30) (19 20))
                    (cl-cc/codegen::a64-used-callee-saved-pairs ra)))))

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
  (let* ((a64-program (compilation-result-program
                       (compile-string "(+ 1 2)" :target :aarch64)))
         (x64-program (compilation-result-program
                       (compile-string "(+ 1 2)" :target :x86_64)))
         (a64 (ignore-errors (compile-to-aarch64-bytes a64-program)))
         (x64 (ignore-errors (compile-to-x86-64-bytes x64-program))))
    (assert-true a64)
    (assert-true x64)
    (assert-false (equalp a64 x64))))


(deftest aarch64-bswap-emitter-encoding
  "emit-a64-vm-bswap emits a single REV Wd, Wn instruction."
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-a64-vm-bswap (cl-cc:make-vm-bswap :dst :R0 :src :R1) s)))))
    (assert-= 4 (length bytes))
    (assert-= #x20 (nth 0 bytes))
    (assert-= #x08 (nth 1 bytes))
    (assert-= #xC0 (nth 2 bytes))
    (assert-= #x5A (nth 3 bytes))))

(deftest aarch64-rotate-emitter-encoding
  "emit-a64-vm-rotate emits MOV Rd, Rn followed by RORV Rd, Rd, Rm (8 bytes total)."
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-a64-vm-rotate (cl-cc:make-vm-rotate :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (assert-= 8 (length bytes))
    ;; First instruction: MOV X0, X1 (encode-mov-rr 0 1) = #xAA0103E0
    (assert-= #xE0 (nth 0 bytes))
    (assert-= #x03 (nth 1 bytes))
    (assert-= #x01 (nth 2 bytes))
    (assert-= #xAA (nth 3 bytes))
    ;; Second instruction: RORV X0, X0, X2 (encode-rorv 0 0 2) = #x9AC22C00
    (assert-= #x00 (nth 4 bytes))
    (assert-= #x2C (nth 5 bytes))
     (assert-= #xC2 (nth 6 bytes))
     (assert-= #x9A (nth 7 bytes))))

(deftest aarch64-mul-high-emitter-encodings
  "emit-a64-vm-integer-mul-high-{u,s} emit single UMULH/SMULH instructions with stable encodings."
  (let ((umulh-bytes (%a64-collect-bytes
                      (lambda (s)
                        (cl-cc/codegen::emit-a64-vm-integer-mul-high-u
                         (cl-cc:make-vm-integer-mul-high-u :dst :R0 :lhs :R1 :rhs :R2) s))))
        (smulh-bytes (%a64-collect-bytes
                      (lambda (s)
                        (cl-cc/codegen::emit-a64-vm-integer-mul-high-s
                         (cl-cc:make-vm-integer-mul-high-s :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (assert-equal '(#x20 #x7C #xC2 #x9B) umulh-bytes)
    (assert-equal '(#x20 #x7C #x42 #x9B) smulh-bytes)))

(deftest aarch64-mul-high-size-and-dispatch-registered
  "vm-integer-mul-high-{u,s} are present in the AArch64 size table and emitter dispatch table."
  (dolist (tp '(cl-cc/vm::vm-integer-mul-high-u cl-cc/vm::vm-integer-mul-high-s))
    (assert-= 4 (gethash tp cl-cc/codegen::*a64-instruction-sizes*))
    (assert-true (functionp (gethash tp cl-cc/codegen::*a64-emitter-table*)))))

(deftest aarch64-sqrt-emitter-encoding
  "vm-sqrt emits one scalar double FSQRT instruction with stable encoding."
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-a64-vm-sqrt
                   (cl-cc:make-vm-sqrt :dst :R0 :src :R1) s)))))
    (assert-equal '(#x20 #xC0 #x61 #x1E) bytes)))

(deftest aarch64-sqrt-size-and-dispatch-registered
  "vm-sqrt is present in the AArch64 size table and emitter dispatch table."
  (assert-= 4 (gethash 'cl-cc/vm::vm-sqrt cl-cc/codegen::*a64-instruction-sizes*))
  (assert-true (functionp (gethash 'cl-cc/vm::vm-sqrt cl-cc/codegen::*a64-emitter-table*))))

;;; ─── Libm call emitters (sin/cos/exp/log/tan/asin/acos/atan — FR-286) ─────

(deftest-each aarch64-libm-unary-emitter-size
  "Each AArch64 libm unary emitter produces a byte sequence whose length is a multiple of 4 (AArch64 instr width)."
  :cases (("sin" #'cl-cc/codegen::emit-a64-vm-sin
           (cl-cc/vm::make-vm-sin-inst :dst :R0 :src :R1))
          ("cos" #'cl-cc/codegen::emit-a64-vm-cos
           (cl-cc/vm::make-vm-cos-inst :dst :R0 :src :R1))
          ("exp" #'cl-cc/codegen::emit-a64-vm-exp
           (cl-cc/vm::make-vm-exp-inst :dst :R0 :src :R1))
          ("log" #'cl-cc/codegen::emit-a64-vm-log
           (cl-cc/vm::make-vm-log-inst :dst :R0 :src :R1))
          ("tan" #'cl-cc/codegen::emit-a64-vm-tan
           (cl-cc/vm::make-vm-tan-inst :dst :R0 :src :R1))
          ("asin" #'cl-cc/codegen::emit-a64-vm-asin
           (cl-cc/vm::make-vm-asin-inst :dst :R0 :src :R1))
          ("acos" #'cl-cc/codegen::emit-a64-vm-acos
           (cl-cc/vm::make-vm-acos-inst :dst :R0 :src :R1))
          ("atan" #'cl-cc/codegen::emit-a64-vm-atan
           (cl-cc/vm::make-vm-atan-inst :dst :R0 :src :R1)))
  (emit-fn inst)
  (let ((bytes (%a64-collect-bytes (lambda (s) (funcall emit-fn inst s)))))
    (assert-true (>= (length bytes) 24))       ; at least FMOV+STP+1MOVZ+BLR+LDP+FMOV
    (assert-= 0 (mod (length bytes) 4))))      ; AArch64 = 4-byte aligned

(deftest aarch64-libm-sin-starts-with-fmov
  "vm-sin libm call begins with FMOV D0, D1 encoding (0x1E604020 little-endian)."
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-a64-vm-sin
                   (cl-cc/vm::make-vm-sin-inst :dst :R0 :src :R1) s)))))
    ;; FMOV D0, D1 = encode-fmov-dd(0, 1) = 0x1E604000 | (1<<5) | 0 = 0x1E604020
    (assert-equal '(#x20 #x40 #x60 #x1E) (subseq bytes 0 4))))

(deftest-each aarch64-instruction-size-table-entries
  "All instruction types are correctly registered in *a64-instruction-sizes*."
  :cases (("bswap"       4 'cl-cc/vm::vm-bswap)
          ("tail-call"   4 'cl-cc/vm::vm-tail-call)
          ("min"         8 'cl-cc/vm::vm-min)
          ("max"         8 'cl-cc/vm::vm-max)
          ("integer-add" 4 'cl-cc/vm::vm-integer-add)
          ("integer-sub" 4 'cl-cc/vm::vm-integer-sub)
          ("integer-mul" 4 'cl-cc/vm::vm-integer-mul)
          ("integer-mul-high-u" 4 'cl-cc/vm::vm-integer-mul-high-u)
          ("integer-mul-high-s" 4 'cl-cc/vm::vm-integer-mul-high-s)
           ("sqrt" 4 'cl-cc/vm::vm-sqrt)
            ("sin-inst" 36 'cl-cc/vm::vm-sin-inst)
            ("cos-inst" 36 'cl-cc/vm::vm-cos-inst)
            ("exp-inst" 36 'cl-cc/vm::vm-exp-inst)
            ("log-inst" 36 'cl-cc/vm::vm-log-inst)
            ("tan-inst" 36 'cl-cc/vm::vm-tan-inst)
            ("asin-inst" 36 'cl-cc/vm::vm-asin-inst)
            ("acos-inst" 36 'cl-cc/vm::vm-acos-inst)
            ("atan-inst" 36 'cl-cc/vm::vm-atan-inst))
  (expected instr-type)
  (assert-= expected (gethash instr-type cl-cc/codegen::*a64-instruction-sizes*)))

(deftest aarch64-vm-move-self-elision-emits-no-bytes
  "Self-move emits no bytes and a64-instruction-size returns 0."
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-a64-vm-move
                   (cl-cc:make-vm-move :dst :R0 :src :R0) s)))))
    (assert-= 0 (length bytes)))
  (assert-= 0 (cl-cc/codegen::a64-instruction-size (cl-cc:make-vm-move :dst :R0 :src :R0))))

(deftest aarch64-scs-single-register-encodings
  "Shadow call stack helper encodings for STR-post, LDR-pre, B.cond, and BRK are stable."
  (let ((store (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-instr (cl-cc/codegen::encode-str-post 30 18 8) s))))
        (load  (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-instr (cl-cc/codegen::encode-ldr-pre 17 18 -8) s))))
        (beq   (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-instr (cl-cc/codegen::encode-b-cond 2 0) s))))
        (brk   (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-instr (cl-cc/codegen::encode-brk 0) s)))))
    (assert-equal '(94 134 0 248) store)
    (assert-equal '(81 142 95 248) load)
    (assert-equal '(64 0 0 84) beq)
    (assert-equal '(0 0 32 212) brk)))

(deftest-each aarch64-emitter-table-entries
  "Instruction types are registered as functions in the AArch64 emitter table."
  :cases (("min"         'cl-cc/vm::vm-min)
           ("max"         'cl-cc/vm::vm-max)
           ("integer-add" 'cl-cc/vm::vm-integer-add)
            ("integer-sub" 'cl-cc/vm::vm-integer-sub)
            ("integer-mul" 'cl-cc/vm::vm-integer-mul)
            ("integer-mul-high-u" 'cl-cc/vm::vm-integer-mul-high-u)
            ("integer-mul-high-s" 'cl-cc/vm::vm-integer-mul-high-s)
             ("sqrt" 'cl-cc/vm::vm-sqrt)
              ("sin-inst" 'cl-cc/vm::vm-sin-inst)
              ("cos-inst" 'cl-cc/vm::vm-cos-inst)
              ("exp-inst" 'cl-cc/vm::vm-exp-inst)
              ("log-inst" 'cl-cc/vm::vm-log-inst)
              ("tan-inst" 'cl-cc/vm::vm-tan-inst)
              ("asin-inst" 'cl-cc/vm::vm-asin-inst)
              ("acos-inst" 'cl-cc/vm::vm-acos-inst)
              ("atan-inst" 'cl-cc/vm::vm-atan-inst))
  (instr-type)
  (assert-true (functionp (gethash instr-type cl-cc/codegen::*a64-emitter-table*))))

(deftest aarch64-tail-call-emitter-encoding
  "vm-tail-call emits BR Xn on AArch64."
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-a64-instruction
                   (cl-cc:make-vm-tail-call :dst :R0 :func :R1 :args nil)
                   s 0 (make-hash-table :test #'eq))))))
    (assert-= 4 (length bytes))
    (assert-= #x20 (nth 0 bytes))
    (assert-= #x00 (nth 1 bytes))
    (assert-= #x1F (nth 2 bytes))
    (assert-= #xD6 (nth 3 bytes))))

(deftest aarch64-build-label-offsets-account-for-elided-self-move
  "build-a64-label-offsets does not advance offsets for self-moves elided at emit time."
  (let* ((insts (list (cl-cc:make-vm-move :dst :R0 :src :R0)
                      (cl-cc:make-vm-label :name "after-self-move")
                      (cl-cc:make-vm-halt :reg :R0)))
         (offsets (cl-cc/codegen::build-a64-label-offsets insts 0)))
    (assert-= 0 (gethash "after-self-move" offsets))))

(deftest aarch64-min-max-emitter-encoding
  "emit-a64-vm-min/max emit CMP followed by CSEL on AArch64."
  (let ((min-bytes (%a64-collect-bytes
                    (lambda (s)
                      (cl-cc/codegen::emit-a64-vm-min
                       (cl-cc:make-vm-min :dst :R0 :lhs :R1 :rhs :R2) s))))
        (max-bytes (%a64-collect-bytes
                    (lambda (s)
                      (cl-cc/codegen::emit-a64-vm-max
                       (cl-cc:make-vm-max :dst :R0 :lhs :R1 :rhs :R2) s)))))
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
                  (cl-cc/codegen::emit-a64-vm-select
                   (cl-cc:make-vm-select :dst :R0 :cond-reg :R1 :then-reg :R2 :else-reg :R3)
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
                  (cl-cc/codegen::emit-a64-vm-jump-zero
                   (cl-cc:make-vm-jump-zero :reg :R1 :label "L1")
                   s 0 (let ((ht (make-hash-table :test #'equal)))
                         (setf (gethash "L1" ht) 4)
                         ht))))))
    (assert-= 4 (length bytes))
    ;; low byte and high opcode nibble are enough to prove CBZ path, not CMP+B.EQ
    (assert-= #x01 (logand (nth 0 bytes) #x1F))
    (assert-= #xB4 (nth 3 bytes))))

(deftest aarch64-leaf-and-nonleaf-without-spills-share-fpe-layout
  "Default AArch64 FPE keeps leaf and non-leaf programs without spills on the same shadow-call-stack-only frame layout."
  (let* ((result (compile-string "(+ 1 2)" :target :aarch64))
         (program (compilation-result-program result))
         (base (cl-cc/vm::make-vm-program :instructions (cl-cc/vm::vm-program-instructions program)
                                        :result-register (cl-cc/vm::vm-program-result-register program)
                                        :leaf-p nil))
         (leaf-bytes (compile-to-aarch64-bytes program))
         (nonleaf-bytes (compile-to-aarch64-bytes base)))
    (assert-true (cl-cc/vm::vm-program-leaf-p program))
    (assert-= (length leaf-bytes) (length nonleaf-bytes))
    (assert-true (equalp leaf-bytes nonleaf-bytes))))

(deftest aarch64-empty-program-default-fpe-emits-24-bytes-with-shadow-call-stack
  "Default AArch64 FPE omits the FP/LR pair for an empty program while keeping the shadow-call-stack epilogue."
  (let* ((program (cl-cc/vm::make-vm-program :instructions nil :result-register :R0))
         (bytes (compile-to-aarch64-bytes program)))
    (assert-= 24 (length bytes))
    ;; Prologue begins with STR LR, [X18], #8
    (assert-= 94 (elt bytes 0))
    (assert-= 134 (elt bytes 1))
    (assert-= 0 (elt bytes 2))
    (assert-= 248 (elt bytes 3))
    ;; With default FPE, the epilogue starts immediately with the SCS verification load.
    (assert-= 81 (elt bytes 4))
    (assert-= 142 (elt bytes 5))
    (assert-= 95 (elt bytes 6))
    (assert-= 248 (elt bytes 7))
    ;; Epilogue contains BRK #0 just before final RET.
    (assert-= 0 (elt bytes 16))
    (assert-= 0 (elt bytes 17))
    (assert-= 32 (elt bytes 18))
    (assert-= 212 (elt bytes 19))
    (assert-= #xC0 (elt bytes 20))
    (assert-= #x03 (elt bytes 21))
    (assert-= #x5F (elt bytes 22))
    (assert-= #xD6 (elt bytes 23))))

(deftest aarch64-default-fpe-uses-sp-relative-spill-frame
  "Default AArch64 native code omits X29 and uses an SP-relative spill frame for leaf spills."
  (let* ((store-inst (cl-cc:make-vm-spill-store :src-reg :x19 :slot 1))
         (load-inst (cl-cc:make-vm-spill-load :dst-reg :x20 :slot 1))
         (program (cl-cc/vm::make-vm-program
                   :instructions (list store-inst load-inst)
                   :result-register :R0
                   :leaf-p t))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment (make-hash-table :test #'eq)
                                                   :spill-count 1
                                                   :instructions (cl-cc/vm::vm-program-instructions program)))
         (alloc-bytes (%a64-collect-bytes
                       (lambda (s)
                         (cl-cc/codegen::emit-a64-instr
                          (cl-cc/codegen::encode-sub-imm cl-cc/codegen::+a64-sp+
                                                         cl-cc/codegen::+a64-sp+
                                                         8 0)
                          s))))
         (free-bytes (%a64-collect-bytes
                      (lambda (s)
                        (cl-cc/codegen::emit-a64-instr
                         (cl-cc/codegen::encode-add-imm cl-cc/codegen::+a64-sp+
                                                        cl-cc/codegen::+a64-sp+
                                                        8 0)
                         s))))
         (store-bytes (let ((cl-cc/codegen::*current-a64-spill-base-reg* cl-cc/codegen::+a64-sp+)
                            (cl-cc/codegen::*current-a64-spill-offset-bias* 8))
                        (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-vm-spill-store store-inst s)))))
         (load-bytes (let ((cl-cc/codegen::*current-a64-spill-base-reg* cl-cc/codegen::+a64-sp+)
                           (cl-cc/codegen::*current-a64-spill-offset-bias* 8))
                       (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-vm-spill-load load-inst s)))))
         (bytes (let ((cl-cc/codegen::*current-a64-regalloc* ra))
                  (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-program program s))))))
    (assert-equal '(#x5E #x86 #x00 #xF8) (subseq bytes 0 4))
    (assert-equal alloc-bytes (subseq bytes 4 8))
    (assert-equal store-bytes (subseq bytes 8 12))
    (assert-equal load-bytes (subseq bytes 12 16))
    (assert-equal free-bytes (subseq bytes 16 20))
    (assert-equal '(#xC0 #x03 #x5F #xD6)
                  (subseq bytes (- (length bytes) 4) (length bytes)))))

(deftest aarch64-debug-opt-out-keeps-fp-lr-pair-and-fp-spills
  "Disabling AArch64 FPE keeps the FP/LR save pair and X29-based spill addressing."
  (let* ((store-inst (cl-cc:make-vm-spill-store :src-reg :x19 :slot 1))
         (load-inst (cl-cc:make-vm-spill-load :dst-reg :x20 :slot 1))
         (program (cl-cc/vm::make-vm-program
                   :instructions (list store-inst load-inst)
                   :result-register :R0
                   :leaf-p t))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment (make-hash-table :test #'eq)
                                                   :spill-count 1
                                                   :instructions (cl-cc/vm::vm-program-instructions program)))
         (save-pair (%a64-collect-bytes
                     (lambda (s)
                       (cl-cc/codegen::emit-a64-instr
                        (cl-cc/codegen::encode-stp-pre cl-cc/codegen::+a64-fp+
                                                       cl-cc/codegen::+a64-lr+
                                                       cl-cc/codegen::+a64-sp+
                                                       -2)
                        s))))
         (restore-pair (%a64-collect-bytes
                        (lambda (s)
                          (cl-cc/codegen::emit-a64-instr
                           (cl-cc/codegen::encode-ldp-post cl-cc/codegen::+a64-fp+
                                                           cl-cc/codegen::+a64-lr+
                                                           cl-cc/codegen::+a64-sp+
                                                           2)
                           s))))
         (store-bytes (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-vm-spill-store store-inst s))))
         (load-bytes (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-vm-spill-load load-inst s))))
         (bytes (let ((cl-cc/codegen::*current-a64-regalloc* ra)
                      (cl-cc/codegen::*a64-omit-frame-pointer* nil))
                  (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-program program s))))))
    (assert-equal '(#x5E #x86 #x00 #xF8) (subseq bytes 0 4))
    (assert-equal save-pair (subseq bytes 4 8))
    (assert-equal store-bytes (subseq bytes 8 12))
    (assert-equal load-bytes (subseq bytes 12 16))
    (assert-equal restore-pair (subseq bytes 16 20))))

(deftest aarch64-stack-probe-emits-page-touch-sequence
  "emit-a64-stack-probes emits SUB x16, sp, #4096 followed by LDUR xzr, [x16]."
  (let ((bytes (%a64-collect-bytes
                (lambda (s)
                  (cl-cc/codegen::emit-a64-stack-probes s 1)))))
    (assert-equal '(#xF0 #x07 #x40 #xD1 #x1F #x02 #x40 #xF8) bytes)))

(deftest aarch64-default-fpe-large-spill-frame-signals-unsupported-adjust
  "Default AArch64 FPE rejects spill frames that exceed the simple ADD/SUB immediate range."
  (let* ((program (cl-cc/vm::make-vm-program :instructions nil :result-register :R0 :leaf-p t))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment (make-hash-table :test #'eq)
                                                   :spill-count 512
                                                   :instructions nil)))
    (assert-signals error
      (let ((cl-cc/codegen::*current-a64-regalloc* ra))
        (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-program program s)))))))

(deftest aarch64-large-spill-frame-inserts-stack-probe-before-prologue-when-fpe-disabled
  "Disabling AArch64 FPE preserves the conservative large-frame probe before the shadow-call-stack prologue."
  (let* ((program (cl-cc/vm::make-vm-program :instructions nil :result-register :R0 :leaf-p t))
         (ra (cl-cc/regalloc::make-regalloc-result :assignment (make-hash-table :test #'eq)
                                                    :spill-count 512
                                                    :instructions nil))
         (bytes (let ((cl-cc/codegen::*current-a64-regalloc* ra)
                      (cl-cc/codegen::*a64-omit-frame-pointer* nil))
                   (%a64-collect-bytes (lambda (s) (cl-cc/codegen::emit-a64-program program s))))))
    (assert-equal '(#xF0 #x07 #x40 #xD1 #x1F #x02 #x40 #xF8) (subseq bytes 0 8))
    (assert-equal '(#x5E #x86 #x00 #xF8) (subseq bytes 8 12))))

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
