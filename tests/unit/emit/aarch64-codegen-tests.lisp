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

;;; ─────────────────────────────────────────────────────────────────────────
;;; Return-type contract
;;; ─────────────────────────────────────────────────────────────────────────

(deftest aarch64-bytes-returns-octet-vector
  "compile-to-aarch64-bytes returns an array of (unsigned-byte 8)."
  (let ((bytes (%a64-compile "(+ 1 2)")))
    (assert-true bytes)
    (assert-true (typep bytes '(array (unsigned-byte 8) (*))))))

(deftest aarch64-bytes-non-empty
  "compile-to-aarch64-bytes emits at least one byte."
  (assert-true (> (length (%a64-compile "(+ 1 2)")) 0)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; AArch64 ISA alignment: all instructions are 4 bytes
;;; ─────────────────────────────────────────────────────────────────────────

(deftest aarch64-bytes-length-multiple-of-4
  "Emitted byte vector length must be a multiple of 4 (AArch64 fixed-width ISA)."
  (let ((bytes (%a64-compile "(+ 1 2)")))
    (assert-true bytes)
    (assert-= 0 (mod (length bytes) 4))))

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
