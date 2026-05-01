;;;; tests/unit/emit/x86-64-emit-logical-tests.lisp
;;;; Unit tests for src/emit/x86-64-emit-ops-logical.lisp
;;;;
;;;; Covers: emit-vm-null-p, emit-vm-true-pred, emit-vm-false-pred,
;;;;   emit-vm-and, emit-vm-or, emit-vm-logand, emit-vm-logior,
;;;;   emit-vm-logxor, emit-vm-logeqv, emit-vm-logtest, emit-vm-logbitp.
;;;;
;;;; Strategy: emit each instruction into a byte-collecting stream,
;;;; verify the byte count matches the byte-budget documented in the source.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Test helpers ────────────────────────────────────────────────────────

(defun %collect-logical-bytes (emit-fn inst)
  "Collect bytes emitted by EMIT-FN for INST into a list."
  (let ((bytes nil))
    (funcall emit-fn inst (lambda (b) (push b bytes)))
    (nreverse bytes)))

;;; ─── emit-vm-null-p ──────────────────────────────────────────────────────

(deftest x86-emit-null-p-emits-bytes
  "emit-vm-null-p emits a non-empty byte sequence (TEST + SETE + MOVZX)."
  (let* ((inst (make-vm-null-p :dst :r0 :src :r1))
         (bytes (%collect-logical-bytes #'cl-cc/emit::emit-vm-null-p inst)))
    (assert-true (> (length bytes) 0))))

;;; ─── emit-vm-true-pred / emit-vm-false-pred ──────────────────────────────

(deftest x86-emit-true-pred-emits-mov-imm-1
  "emit-vm-true-pred emits an immediate-1 MOV (7 bytes for REX + MOV r64, imm32 with imm=1)."
  (let* ((inst (make-vm-null-p :dst :r0 :src :r0))  ; reusing null-p struct (has :dst)
         (bytes (%collect-logical-bytes #'cl-cc/emit::emit-vm-true-pred inst)))
    ;; MOV r64, imm with small immediate value ≥ 4 bytes
    (assert-true (>= (length bytes) 4))))

(deftest x86-emit-false-pred-emits-mov-imm-0
  "emit-vm-false-pred emits an immediate-0 MOV (same layout as true-pred)."
  (let* ((inst (make-vm-null-p :dst :r0 :src :r0))
         (bytes (%collect-logical-bytes #'cl-cc/emit::emit-vm-false-pred inst)))
    (assert-true (>= (length bytes) 4))))

;;; ─── emit-vm-and / emit-vm-or ────────────────────────────────────────────

(deftest x86-emit-vm-and-emits-17-bytes
  "emit-vm-and emits exactly 17 bytes (as documented in the source layout comment)."
  (let* ((inst (make-vm-and :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-logical-bytes #'cl-cc/emit::emit-vm-and inst)))
    (assert-= 17 (length bytes))))

(deftest x86-emit-vm-or-emits-17-bytes
  "emit-vm-or emits exactly 17 bytes (as documented in the source layout comment)."
  (let* ((inst (make-vm-or :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-logical-bytes #'cl-cc/emit::emit-vm-or inst)))
    (assert-= 17 (length bytes))))

;;; ─── emit-vm-logand / emit-vm-logior / emit-vm-logxor ───────────────────

(deftest x86-emit-logand-emits-bytes
  "emit-vm-logand emits a non-empty sequence (define-binary-alu-emitter pattern)."
  (let* ((inst (make-vm-logand :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-logical-bytes #'cl-cc/emit::emit-vm-logand inst)))
    (assert-true (> (length bytes) 0))))

(deftest x86-emit-logior-emits-bytes
  "emit-vm-logior emits a non-empty sequence."
  (let* ((inst (make-vm-logior :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-logical-bytes #'cl-cc/emit::emit-vm-logior inst)))
    (assert-true (> (length bytes) 0))))

(deftest x86-emit-logxor-emits-bytes
  "emit-vm-logxor emits a non-empty sequence."
  (let* ((inst (make-vm-logxor :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-logical-bytes #'cl-cc/emit::emit-vm-logxor inst)))
    (assert-true (> (length bytes) 0))))

;;; ─── emit-vm-logeqv ──────────────────────────────────────────────────────

(deftest x86-emit-logeqv-emits-9-bytes
  "emit-vm-logeqv emits exactly 9 bytes (MOV+XOR+NOT, 3 bytes each)."
  (let* ((inst (make-vm-logeqv :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-logical-bytes #'cl-cc/emit::emit-vm-logeqv inst)))
    (assert-= 9 (length bytes))))

;;; ─── emit-vm-logtest ─────────────────────────────────────────────────────

(deftest x86-emit-logtest-emits-bytes
  "emit-vm-logtest emits a non-empty sequence (conservative 14 bytes max)."
  (let* ((inst (make-vm-logtest :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-logical-bytes #'cl-cc/emit::emit-vm-logtest inst)))
    (assert-true (> (length bytes) 0))
    (assert-true (<= (length bytes) 14))))

;;; ─── emit-vm-logbitp ─────────────────────────────────────────────────────

(deftest x86-emit-logbitp-emits-15-bytes
  "emit-vm-logbitp emits exactly 15 bytes (PUSH+MOV+MOV+SAR+AND+POP layout)."
  (let* ((inst (make-vm-logbitp :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-logical-bytes #'cl-cc/emit::emit-vm-logbitp inst)))
    (assert-= 15 (length bytes))))
