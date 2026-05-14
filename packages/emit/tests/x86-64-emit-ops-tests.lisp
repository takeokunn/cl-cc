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

(deftest-each x86-emit-const-cases
  "emit-vm-const emits non-empty bytes for both integer and float values."
  :cases (("integer" (make-vm-const :dst :r0 :value 42))
          ("float"   (make-vm-const :dst :r0 :value 3.14d0)))
  (inst)
  (assert-true (> (length (%collect-emit-ops-bytes #'cl-cc/codegen::emit-vm-const inst)) 0)))

;;; ─── emit-vm-move ────────────────────────────────────────────────────────

(deftest x86-emit-move-between-gp-regs
  "emit-vm-move between two general-purpose registers emits bytes."
  (let* ((inst (make-vm-move :dst :r0 :src :r1))
         (bytes (%collect-emit-ops-bytes #'cl-cc/codegen::emit-vm-move inst)))
    ;; Distinct GP registers → MOV emitted
    (assert-true (> (length bytes) 0))))

(deftest x86-emit-move-same-gp-reg-emits-nothing
  "emit-vm-move from a register to itself emits no bytes (eliminated)."
  (let* ((inst (make-vm-move :dst :r0 :src :r0))
         (bytes (%collect-emit-ops-bytes #'cl-cc/codegen::emit-vm-move inst)))
    (assert-equal 0 (length bytes))))

;;; ─── Binary ALU emitters (define-binary-alu-emitter) ────────────────────

(deftest-each x86-emit-binary-alu-emits-bytes
  "All define-binary-alu-emitter functions emit at least 6 bytes (MOV+op)."
  :cases (("add" #'cl-cc/codegen::emit-vm-add (make-vm-add :dst :r0 :lhs :r1 :rhs :r2))
           ("sub" #'cl-cc/codegen::emit-vm-sub (make-vm-sub :dst :r0 :lhs :r1 :rhs :r2))
           ("mul" #'cl-cc/codegen::emit-vm-mul (make-vm-mul :dst :r0 :lhs :r1 :rhs :r2)))
  (emit-fn inst)
  (let ((bytes (%collect-emit-ops-bytes emit-fn inst)))
    (assert-true (>= (length bytes) 6))))

;;; ─── Checked arithmetic emitters (FR-303) ─────────────────────────────────

(deftest x86-emit-add-checked-emits-14-bytes
  "emit-vm-add-checked emits exactly 14 bytes: MOV(3)+ADD(3)+JO(6)+UD2(2)."
  (let* ((inst (cl-cc:make-vm-add-checked :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-emit-ops-bytes #'cl-cc/codegen::emit-vm-add-checked inst)))
    (assert-= 14 (length bytes))))

(deftest x86-emit-sub-checked-emits-14-bytes
  "emit-vm-sub-checked emits exactly 14 bytes: MOV(3)+SUB(3)+JO(6)+UD2(2)."
  (let* ((inst (cl-cc:make-vm-sub-checked :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-emit-ops-bytes #'cl-cc/codegen::emit-vm-sub-checked inst)))
    (assert-= 14 (length bytes))))

(deftest x86-emit-mul-checked-emits-15-bytes
  "emit-vm-mul-checked emits exactly 15 bytes: MOV(3)+IMUL(4)+JO(6)+UD2(2)."
  (let* ((inst (cl-cc:make-vm-mul-checked :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-emit-ops-bytes #'cl-cc/codegen::emit-vm-mul-checked inst)))
    (assert-= 15 (length bytes))))

(deftest x86-emit-mul-high-emits-19-bytes
  "vm-integer-mul-high-{u,s} emit the documented 19-byte save/MUL-high/restore sequence."
  (assert-equal '(#x49 #x89 #xD3 #x50 #x52 #x48 #x89 #xC8 #x49 #xF7 #xE3 #x49 #x89 #xD3 #x5A #x58 #x4C #x89 #xD8)
                (%collect-emit-ops-bytes #'cl-cc/codegen::emit-vm-integer-mul-high-u
                                         (cl-cc:make-vm-integer-mul-high-u :dst :r0 :lhs :r1 :rhs :r2)))
  (assert-equal '(#x49 #x89 #xD3 #x50 #x52 #x48 #x89 #xC8 #x49 #xF7 #xEB #x49 #x89 #xD3 #x5A #x58 #x4C #x89 #xD8)
                (%collect-emit-ops-bytes #'cl-cc/codegen::emit-vm-integer-mul-high-s
                                          (cl-cc:make-vm-integer-mul-high-s :dst :r0 :lhs :r1 :rhs :r2))))

(deftest x86-emit-sqrt-emits-sqrtsd-sequence
  "vm-sqrt emits MOVSD dst,src followed by SQRTSD dst,dst."
  (assert-equal '(#xF2 #x0F #x10 #xC1 #xF2 #x0F #x51 #xC0)
                (%collect-emit-ops-bytes #'cl-cc/codegen::emit-vm-sqrt
                                         (cl-cc:make-vm-sqrt :dst :r0 :src :r1))))

;;; ─── Libm call emitters (sin/cos/exp/log/tan/asin/acos/atan) ───────────────

(deftest-each x86-emit-libm-unary-emits-21-bytes
  "Each libm unary emitter (sin/cos/exp/log/tan/asin/acos/atan) emits exactly 21 bytes."
  :cases (("sin" #'cl-cc/codegen::emit-vm-sin
           (cl-cc/vm::make-vm-sin-inst :dst :r0 :src :r1))
          ("cos" #'cl-cc/codegen::emit-vm-cos
           (cl-cc/vm::make-vm-cos-inst :dst :r0 :src :r1))
          ("exp" #'cl-cc/codegen::emit-vm-exp
           (cl-cc/vm::make-vm-exp-inst :dst :r0 :src :r1))
          ("log" #'cl-cc/codegen::emit-vm-log
           (cl-cc/vm::make-vm-log-inst :dst :r0 :src :r1))
          ("tan" #'cl-cc/codegen::emit-vm-tan
           (cl-cc/vm::make-vm-tan-inst :dst :r0 :src :r1))
          ("asin" #'cl-cc/codegen::emit-vm-asin
           (cl-cc/vm::make-vm-asin-inst :dst :r0 :src :r1))
          ("acos" #'cl-cc/codegen::emit-vm-acos
           (cl-cc/vm::make-vm-acos-inst :dst :r0 :src :r1))
          ("atan" #'cl-cc/codegen::emit-vm-atan
           (cl-cc/vm::make-vm-atan-inst :dst :r0 :src :r1)))
  (emit-fn inst)
  (assert-= 21 (length (%collect-emit-ops-bytes emit-fn inst))))

(deftest x86-emit-libm-sin-starts-with-movsd
  "vm-sin libm call begins with MOVSD XMM0,src (F2 0F 10 C1 for src=XMM1)."
  (let ((bytes (%collect-emit-ops-bytes
                #'cl-cc/codegen::emit-vm-sin
                (cl-cc/vm::make-vm-sin-inst :dst :r0 :src :r1))))
    (assert-equal '(#xF2 #x0F #x10 #xC1) (subseq bytes 0 4))))

;;; ─── Truncate / Rem ──────────────────────────────────────────────────────

(deftest-each x86-emit-truncate-rem-cases
  "emit-vm-truncate and emit-vm-rem each emit non-empty byte sequences."
  :cases (("truncate" #'cl-cc/codegen::emit-vm-truncate (make-vm-truncate :dst :r0 :lhs :r1 :rhs :r2))
          ("rem"      #'cl-cc/codegen::emit-vm-rem      (make-vm-rem      :dst :r0 :lhs :r1 :rhs :r2)))
  (emit-fn inst)
  (assert-true (> (length (%collect-emit-ops-bytes emit-fn inst)) 0)))

;;; ─── Floor Division / Mod (documented byte counts) ──────────────────────

(deftest x86-emit-div-emits-34-bytes
  "emit-vm-div emits exactly 34 bytes as documented in the source layout comment."
  (let* ((inst (make-vm-div :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-emit-ops-bytes #'cl-cc/codegen::emit-vm-div inst)))
    (assert-= 34 (length bytes))))

(deftest x86-emit-mod-emits-37-bytes
  "emit-vm-mod emits exactly 37 bytes as documented in the source layout comment."
  (let* ((inst (make-vm-mod :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-emit-ops-bytes #'cl-cc/codegen::emit-vm-mod inst)))
    (assert-= 37 (length bytes))))

;;; ─── Comparison emitters (define-cmp-emitter) ───────────────────────────

(deftest-each x86-emit-comparison-emits-bytes
  "All comparison emitters produce a non-empty byte sequence (CMP+SETcc+MOVZX)."
  :cases (("lt"     #'cl-cc/codegen::emit-vm-lt     (make-vm-lt     :dst :r0 :lhs :r1 :rhs :r2))
          ("gt"     #'cl-cc/codegen::emit-vm-gt     (make-vm-gt     :dst :r0 :lhs :r1 :rhs :r2))
          ("le"     #'cl-cc/codegen::emit-vm-le     (make-vm-le     :dst :r0 :lhs :r1 :rhs :r2))
          ("ge"     #'cl-cc/codegen::emit-vm-ge     (make-vm-ge     :dst :r0 :lhs :r1 :rhs :r2))
          ("num-eq" #'cl-cc/codegen::emit-vm-num-eq (make-vm-num-eq :dst :r0 :lhs :r1 :rhs :r2))
          ("eq"     #'cl-cc/codegen::emit-vm-eq     (make-vm-eq     :dst :r0 :lhs :r1 :rhs :r2)))
  (emit-fn inst)
  (let ((bytes (%collect-emit-ops-bytes emit-fn inst)))
    (assert-true (> (length bytes) 0))))

;;; ─── Unary emitters ──────────────────────────────────────────────────────

(deftest-each x86-emit-unary-non-empty
  "All unary vm emitters produce non-empty byte sequences."
  :cases (("neg"            #'cl-cc/codegen::emit-vm-neg            (make-vm-neg            :dst :r0 :src :r1))
          ("not"            #'cl-cc/codegen::emit-vm-not            (make-vm-not            :dst :r0 :src :r1))
          ("lognot"         #'cl-cc/codegen::emit-vm-lognot         (make-vm-lognot         :dst :r0 :src :r1))
          ("logcount"       #'cl-cc/codegen::emit-vm-logcount       (make-vm-logcount       :dst :r0 :src :r1))
          ("integer-length" #'cl-cc/codegen::emit-vm-integer-length (make-vm-integer-length :dst :r0 :src :r1))
          ("bswap"          #'cl-cc/codegen::emit-vm-bswap          (make-vm-bswap          :dst :r0 :src :r1))
          ("inc"            #'cl-cc/codegen::emit-vm-inc            (make-vm-inc            :dst :r0 :src :r1))
          ("dec"            #'cl-cc/codegen::emit-vm-dec            (make-vm-dec            :dst :r0 :src :r1))
          ("abs"            #'cl-cc/codegen::emit-vm-abs            (make-vm-abs            :dst :r0 :src :r1))
          ("rotate"         #'cl-cc/codegen::emit-vm-rotate         (make-vm-rotate         :dst :r0 :lhs :r1 :rhs :r2)))
  (emit-fn inst)
  (assert-true (> (length (%collect-emit-ops-bytes emit-fn inst)) 0)))

;;; ─── Arithmetic Shift (documented 24-byte layout) ────────────────────────

(deftest x86-emit-ash-emits-24-bytes
  "emit-vm-ash emits exactly 24 bytes as documented (PUSH+MOV+MOV+TEST+JGE+NEG+SAR+JMP+SAL+POP)."
  (let* ((inst (make-vm-ash :dst :r0 :lhs :r1 :rhs :r2))
         (bytes (%collect-emit-ops-bytes #'cl-cc/codegen::emit-vm-ash inst)))
    (assert-= 24 (length bytes))))

;;; ─── Min / Max (define-cmov-emitter) ────────────────────────────────────

(deftest-each x86-emit-min-max-emits-bytes
  "emit-vm-min and emit-vm-max both emit non-empty byte sequences."
  :cases (("min" #'cl-cc/codegen::emit-vm-min (make-vm-min :dst :r0 :lhs :r1 :rhs :r2))
          ("max" #'cl-cc/codegen::emit-vm-max (make-vm-max :dst :r0 :lhs :r1 :rhs :r2)))
  (emit-fn inst)
  (let ((bytes (%collect-emit-ops-bytes emit-fn inst)))
    (assert-true (> (length bytes) 0))))

;;; ─── Select ──────────────────────────────────────────────────────────────

(deftest x86-emit-select-emits-bytes
  "emit-vm-select emits non-empty bytes (MOV+TEST+CMOVNE)."
  (let* ((inst (make-vm-select :dst :r0 :cond-reg :r1 :then-reg :r2 :else-reg :r3))
         (bytes (%collect-emit-ops-bytes #'cl-cc/codegen::emit-vm-select inst)))
    (assert-true (> (length bytes) 0))))
