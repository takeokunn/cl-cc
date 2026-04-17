;;;; tests/unit/emit/x86-64-emit-ops-tests.lisp
;;;; Unit tests for src/emit/x86-64-emit-ops.lisp
;;;;
;;;; Covers: emit-vm-const, emit-vm-move, float-binary emitters,
;;;;   integer alu (add/sub/mul), truncate/rem, div/mod,
;;;;   comparison emitters (lt/gt/le/ge/num-eq/eq),
;;;;   unary emitters (neg/not/lognot/logcount/integer-length/bswap/inc/dec/abs),
;;;;   emit-vm-ash, emit-vm-rotate, emit-vm-min/max, emit-vm-select.
;;;;
;;;; Strategy: emit each instruction into a byte-collecting lambda stream,
;;;;   verify byte count matches the documented layout or is non-empty.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helper ──────────────────────────────────────────────────────────────

(defun %collect-emit-ops-bytes (emit-fn inst)
  "Collect bytes emitted by EMIT-FN for INST into a list."
  (let ((bytes nil))
    (funcall emit-fn inst (lambda (b) (push b bytes)))
    (nreverse bytes)))

;;; ─── emit-vm-const ───────────────────────────────────────────────────────

(deftest x86-emit-const-integer-emits-bytes
  "emit-vm-const with an integer value emits a non-empty byte sequence."
  (let* ((inst (make-vm-const :dst :r0 :value 42))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-const inst)))
    (assert-true (> (length bytes) 0))))

(deftest x86-emit-const-float-emits-bytes
  "emit-vm-const with a float value emits non-empty bytes (uses XMM path)."
  (let* ((inst (make-vm-const :dst :r0 :value 3.14d0))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-const inst)))
    (assert-true (> (length bytes) 0))))

;;; ─── emit-vm-move ────────────────────────────────────────────────────────

(deftest x86-emit-move-between-gp-regs
  "emit-vm-move between two general-purpose registers emits bytes."
  (let* ((inst (make-vm-move :dst :r0 :src :r1))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-move inst)))
    ;; Distinct GP registers → MOV emitted
    (assert-true (> (length bytes) 0))))

(deftest x86-emit-move-same-gp-reg-emits-nothing
  "emit-vm-move from a register to itself emits no bytes (eliminated)."
  (let* ((inst (make-vm-move :dst :r0 :src :r0))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-move inst)))
    (assert-equal 0 (length bytes))))

;;; ─── Binary ALU emitters (define-binary-alu-emitter) ────────────────────

(deftest-each x86-emit-binary-alu-emits-bytes
  "All define-binary-alu-emitter functions emit at least 6 bytes (MOV+op)."
  :cases (("add" #'cl-cc/emit::emit-vm-add (make-vm-add :dst :r0 :lhs :r1 :rhs :r2))
          ("sub" #'cl-cc/emit::emit-vm-sub (make-vm-sub :dst :r0 :lhs :r1 :rhs :r2))
          ("mul" #'cl-cc/emit::emit-vm-mul (make-vm-mul :dst :r0 :lhs :r1 :rhs :r2)))
  (emit-fn inst)
  (let ((bytes (%collect-emit-ops-bytes emit-fn inst)))
    (assert-true (>= (length bytes) 6))))

;;; ─── Truncate / Rem ──────────────────────────────────────────────────────

(deftest x86-emit-truncate-emits-bytes
  "emit-vm-truncate emits a non-empty byte sequence."
  (let* ((inst (make-vm-truncate :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-truncate inst)))
    (assert-true (> (length bytes) 0))))

(deftest x86-emit-rem-emits-bytes
  "emit-vm-rem emits a non-empty byte sequence."
  (let* ((inst (make-vm-rem :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-rem inst)))
    (assert-true (> (length bytes) 0))))

;;; ─── Floor Division / Mod (documented byte counts) ──────────────────────

(deftest x86-emit-div-emits-34-bytes
  "emit-vm-div emits exactly 34 bytes as documented in the source layout comment."
  (let* ((inst (make-vm-div :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-div inst)))
    (assert-= 34 (length bytes))))

(deftest x86-emit-mod-emits-37-bytes
  "emit-vm-mod emits exactly 37 bytes as documented in the source layout comment."
  (let* ((inst (make-vm-mod :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-mod inst)))
    (assert-= 37 (length bytes))))

;;; ─── Comparison emitters (define-cmp-emitter) ───────────────────────────

(deftest-each x86-emit-comparison-emits-bytes
  "All comparison emitters produce a non-empty byte sequence (CMP+SETcc+MOVZX)."
  :cases (("lt"     #'cl-cc/emit::emit-vm-lt     (make-vm-lt     :dst :r0 :lhs :r1 :rhs :r2))
          ("gt"     #'cl-cc/emit::emit-vm-gt     (make-vm-gt     :dst :r0 :lhs :r1 :rhs :r2))
          ("le"     #'cl-cc/emit::emit-vm-le     (make-vm-le     :dst :r0 :lhs :r1 :rhs :r2))
          ("ge"     #'cl-cc/emit::emit-vm-ge     (make-vm-ge     :dst :r0 :lhs :r1 :rhs :r2))
          ("num-eq" #'cl-cc/emit::emit-vm-num-eq (make-vm-num-eq :dst :r0 :lhs :r1 :rhs :r2))
          ("eq"     #'cl-cc/emit::emit-vm-eq     (make-vm-eq     :dst :r0 :lhs :r1 :rhs :r2)))
  (emit-fn inst)
  (let ((bytes (%collect-emit-ops-bytes emit-fn inst)))
    (assert-true (> (length bytes) 0))))

;;; ─── Unary emitters ──────────────────────────────────────────────────────

(deftest x86-emit-neg-emits-bytes
  "emit-vm-neg emits non-empty bytes (MOV+NEG)."
  (let* ((inst (make-vm-neg :dst :r0 :src :r1))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-neg inst)))
    (assert-true (> (length bytes) 0))))

(deftest x86-emit-not-emits-bytes
  "emit-vm-not emits non-empty bytes (TEST+SETE+MOVZX)."
  (let* ((inst (make-vm-not :dst :r0 :src :r1))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-not inst)))
    (assert-true (> (length bytes) 0))))

(deftest x86-emit-lognot-emits-bytes
  "emit-vm-lognot emits non-empty bytes (MOV+NOT)."
  (let* ((inst (make-vm-lognot :dst :r0 :src :r1))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-lognot inst)))
    (assert-true (> (length bytes) 0))))

(deftest x86-emit-logcount-emits-bytes
  "emit-vm-logcount emits non-empty bytes (POPCNT)."
  (let* ((inst (make-vm-logcount :dst :r0 :src :r1))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-logcount inst)))
    (assert-true (> (length bytes) 0))))

(deftest x86-emit-integer-length-emits-bytes
  "emit-vm-integer-length emits non-empty bytes (XOR+TEST+JE+BSR+ADD)."
  (let* ((inst (make-vm-integer-length :dst :r0 :src :r1))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-integer-length inst)))
    (assert-true (> (length bytes) 0))))

(deftest x86-emit-bswap-emits-bytes
  "emit-vm-bswap emits non-empty bytes (MOV+BSWAP)."
  (let* ((inst (make-vm-bswap :dst :r0 :src :r1))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-bswap inst)))
    (assert-true (> (length bytes) 0))))

(deftest x86-emit-inc-emits-bytes
  "emit-vm-inc emits non-empty bytes (MOV+ADD imm8=1)."
  (let* ((inst (make-vm-inc :dst :r0 :src :r1))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-inc inst)))
    (assert-true (> (length bytes) 0))))

(deftest x86-emit-dec-emits-bytes
  "emit-vm-dec emits non-empty bytes (MOV+SUB imm8=1)."
  (let* ((inst (make-vm-dec :dst :r0 :src :r1))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-dec inst)))
    (assert-true (> (length bytes) 0))))

(deftest x86-emit-abs-emits-bytes
  "emit-vm-abs emits non-empty bytes (MOV+CMP+JGE+NEG, documented 15 bytes)."
  (let* ((inst (make-vm-abs :dst :r0 :src :r1))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-abs inst)))
    (assert-true (> (length bytes) 0))))

;;; ─── Arithmetic Shift (documented 24-byte layout) ────────────────────────

(deftest x86-emit-ash-emits-24-bytes
  "emit-vm-ash emits exactly 24 bytes as documented (PUSH+MOV+MOV+TEST+JGE+NEG+SAR+JMP+SAL+POP)."
  (let* ((inst (make-vm-ash :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-ash inst)))
    (assert-= 24 (length bytes))))

;;; ─── Rotate ──────────────────────────────────────────────────────────────

(deftest x86-emit-rotate-emits-bytes
  "emit-vm-rotate emits non-empty bytes (PUSH+MOV+MOV+ROR+POP)."
  (let* ((inst (make-vm-rotate :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-rotate inst)))
    (assert-true (> (length bytes) 0))))

;;; ─── Min / Max (define-cmov-emitter) ────────────────────────────────────

(deftest-each x86-emit-min-max-emits-bytes
  "emit-vm-min and emit-vm-max both emit non-empty byte sequences."
  :cases (("min" #'cl-cc/emit::emit-vm-min (make-vm-min :dst :r0 :lhs :r1 :rhs :r2))
          ("max" #'cl-cc/emit::emit-vm-max (make-vm-max :dst :r0 :lhs :r1 :rhs :r2)))
  (emit-fn inst)
  (let ((bytes (%collect-emit-ops-bytes emit-fn inst)))
    (assert-true (> (length bytes) 0))))

;;; ─── Select ──────────────────────────────────────────────────────────────

(deftest x86-emit-select-emits-bytes
  "emit-vm-select emits non-empty bytes (MOV+TEST+CMOVNE)."
  (let* ((inst (make-vm-select :dst :r0 :cond-reg :r1 :then-reg :r2 :else-reg :r3))
         (bytes (%collect-emit-ops-bytes #'cl-cc/emit::emit-vm-select inst)))
    (assert-true (> (length bytes) 0))))
