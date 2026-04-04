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

(deftest aarch64-min-max-emitter-table-entries
  "vm-min and vm-max are present in the AArch64 emitter table."
  (assert-true (functionp (gethash 'cl-cc::vm-min cl-cc::*a64-emitter-table*)))
  (assert-true (functionp (gethash 'cl-cc::vm-max cl-cc::*a64-emitter-table*))))

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
