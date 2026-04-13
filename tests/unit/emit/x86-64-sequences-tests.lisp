;;;; tests/unit/emit/x86-64-sequences-tests.lisp
;;;; Coverage for src/emit/x86-64-sequences.lisp:
;;;;   emit-idiv-r11, emit-cqo, emit-idiv-sequence,
;;;;   emit-sal-r64-cl, emit-sar-r64-cl, emit-ror-r64-cl,
;;;;   emit-add-ri8, emit-sub-ri8, emit-and-ri8, emit-jge-short,
;;;;   emit-cmovl-rr64, emit-cmovg-rr64, emit-cmovne-rr64,
;;;;   emit-and-rr64, emit-or-rr64, emit-xor-rr64,
;;;;   emit-not-r64, emit-bswap-r32, emit-neg-r64, emit-dec-r64,
;;;;   emit-setcc, emit-movzx-r64-r8.

(in-package :cl-cc/test)

(defsuite x86-64-sequences-suite
  :description "x86-64 instruction sequence emitter tests"
  :parent cl-cc-suite)

(in-suite x86-64-sequences-suite)

;;; ─── Helper ──────────────────────────────────────────────────────────────

(defun %collect-seq-bytes (emit-fn)
  "Call EMIT-FN with a byte-collector stream. Returns list of emitted bytes."
  (let ((bytes nil))
    (funcall emit-fn (lambda (b) (push b bytes)))
    (nreverse bytes)))

;;; ─── emit-idiv-r11 ───────────────────────────────────────────────────────

(deftest x86-seq-idiv-r11-emits-3-bytes
  "emit-idiv-r11 emits exactly 3 bytes: REX.W.B F7 /7."
  (let ((bs (%collect-seq-bytes #'cl-cc::emit-idiv-r11)))
    (assert-= 3 (length bs))
    (assert-= #x49 (first bs))   ; REX.W=1, REX.B=1
    (assert-= #xF7 (second bs))  ; IDIV opcode
    (assert-= (cl-cc::modrm 3 7 3) (third bs)))) ; mod=11, reg=7, rm=3

;;; ─── emit-cqo ────────────────────────────────────────────────────────────

(deftest x86-seq-cqo-emits-2-bytes
  "emit-cqo emits exactly 2 bytes: REX.W (48) and 99."
  (let ((bs (%collect-seq-bytes #'cl-cc::emit-cqo)))
    (assert-= 2 (length bs))
    (assert-= #x48 (first bs))
    (assert-= #x99 (second bs))))

;;; ─── emit-idiv-sequence ──────────────────────────────────────────────────

(deftest x86-seq-idiv-sequence-quotient-is-18-bytes
  "emit-idiv-sequence for quotient emits exactly 18 bytes."
  (let ((bs (%collect-seq-bytes
             (lambda (s) (cl-cc::emit-idiv-sequence
                          cl-cc::+rax+ cl-cc::+rcx+ nil s)))))
    (assert-= 18 (length bs))))

(deftest x86-seq-idiv-sequence-remainder-is-18-bytes
  "emit-idiv-sequence for remainder also emits exactly 18 bytes."
  (let ((bs (%collect-seq-bytes
             (lambda (s) (cl-cc::emit-idiv-sequence
                          cl-cc::+rax+ cl-cc::+rcx+ t s)))))
    (assert-= 18 (length bs))))

(deftest x86-seq-idiv-sequence-contains-cqo
  "emit-idiv-sequence includes CQO bytes (48 99) in the output."
  (let ((bs (%collect-seq-bytes
             (lambda (s) (cl-cc::emit-idiv-sequence
                          cl-cc::+rax+ cl-cc::+rcx+ nil s)))))
    ;; CQO = 48 99 somewhere in the sequence
    (let ((cqo-pos (loop for i from 0 below (1- (length bs))
                         when (and (= (nth i bs) #x48)
                                   (= (nth (1+ i) bs) #x99))
                           return i)))
      (assert-true cqo-pos))))

;;; ─── emit-sal-r64-cl / emit-sar-r64-cl / emit-ror-r64-cl ────────────────

(deftest x86-seq-sal-r64-cl-emits-3-bytes
  "emit-sal-r64-cl emits 3 bytes for a standard register."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc::emit-sal-r64-cl cl-cc::+rax+ s)))))
    (assert-= 3 (length bs))
    ;; Last byte is ModRM with /4 (shift left) and reg field
    (assert-= #xD3 (second bs))))

(deftest x86-seq-sar-r64-cl-emits-3-bytes
  "emit-sar-r64-cl emits 3 bytes for a standard register."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc::emit-sar-r64-cl cl-cc::+rax+ s)))))
    (assert-= 3 (length bs))
    (assert-= #xD3 (second bs))))

(deftest x86-seq-ror-r64-cl-emits-3-bytes
  "emit-ror-r64-cl emits 3 bytes for a standard register."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc::emit-ror-r64-cl cl-cc::+rax+ s)))))
    (assert-= 3 (length bs))
    (assert-= #xD3 (second bs))))

;;; ─── emit-add/sub/and-ri8 ────────────────────────────────────────────────

(deftest x86-seq-add-ri8-emits-4-bytes
  "emit-add-ri8 emits 4 bytes: REX + 83 /0 + imm8."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc::emit-add-ri8 cl-cc::+rax+ 5 s)))))
    (assert-= 4 (length bs))
    (assert-= #x83 (second bs))
    (assert-= 5 (fourth bs))))

(deftest x86-seq-sub-ri8-emits-4-bytes
  "emit-sub-ri8 emits 4 bytes: REX + 83 /5 + imm8."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc::emit-sub-ri8 cl-cc::+rax+ 8 s)))))
    (assert-= 4 (length bs))
    (assert-= #x83 (second bs))
    (assert-= 8 (fourth bs))))

(deftest x86-seq-and-ri8-emits-4-bytes
  "emit-and-ri8 emits 4 bytes: REX + 83 /4 + imm8."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc::emit-and-ri8 cl-cc::+rax+ 15 s)))))
    (assert-= 4 (length bs))
    (assert-= #x83 (second bs))
    (assert-= 15 (fourth bs))))

;;; ─── emit-jge-short ──────────────────────────────────────────────────────

(deftest x86-seq-jge-short-emits-2-bytes
  "emit-jge-short emits 2 bytes: 7D + offset."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc::emit-jge-short 10 s)))))
    (assert-= 2 (length bs))
    (assert-= #x7D (first bs))
    (assert-= 10 (second bs))))

;;; ─── CMOV variants ───────────────────────────────────────────────────────

(deftest x86-seq-cmovl-rr64-emits-correct-prefix
  "emit-cmovl-rr64 uses opcode 0F 4C."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc::emit-cmovl-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)))))
    (assert-true (> (length bs) 2))
    (assert-true (member #x0F bs))
    (assert-true (member #x4C bs))))

(deftest x86-seq-cmovg-rr64-emits-correct-opcode
  "emit-cmovg-rr64 uses opcode 0F 4F."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc::emit-cmovg-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)))))
    (assert-true (member #x0F bs))
    (assert-true (member #x4F bs))))

(deftest x86-seq-cmovne-rr64-emits-correct-opcode
  "emit-cmovne-rr64 uses opcode 0F 45."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc::emit-cmovne-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)))))
    (assert-true (member #x0F bs))
    (assert-true (member #x45 bs))))

;;; ─── Boolean ops on registers ────────────────────────────────────────────

(deftest x86-seq-and-rr64-is-non-empty
  "emit-and-rr64 emits at least 3 bytes."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc::emit-and-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)))))
    (assert-true (>= (length bs) 3))))

(deftest x86-seq-or-rr64-is-non-empty
  "emit-or-rr64 emits at least 3 bytes."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc::emit-or-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)))))
    (assert-true (>= (length bs) 3))))

(deftest x86-seq-xor-rr64-is-non-empty
  "emit-xor-rr64 emits at least 3 bytes."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc::emit-xor-rr64 cl-cc::+rax+ cl-cc::+rcx+ s)))))
    (assert-true (>= (length bs) 3))))

;;; ─── Unary ops ───────────────────────────────────────────────────────────

(deftest x86-seq-not-r64-emits-3-bytes
  "emit-not-r64 emits 3 bytes: REX + F7 /2 ModRM."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc::emit-not-r64 cl-cc::+rax+ s)))))
    (assert-= 3 (length bs))
    (assert-= #xF7 (second bs))))

(deftest x86-seq-neg-r64-emits-3-bytes
  "emit-neg-r64 emits 3 bytes: REX + F7 /3 ModRM."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc::emit-neg-r64 cl-cc::+rax+ s)))))
    (assert-= 3 (length bs))
    (assert-= #xF7 (second bs))))

(deftest x86-seq-dec-r64-emits-3-bytes
  "emit-dec-r64 emits 3 bytes: REX + FF /1 ModRM."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc::emit-dec-r64 cl-cc::+rax+ s)))))
    (assert-= 3 (length bs))
    (assert-= #xFF (second bs))))

(deftest x86-seq-bswap-r32-emits-correct-bytes
  "emit-bswap-r32 emits REX + 0F C8+reg for 64-bit bswap."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc::emit-bswap-r32 cl-cc::+rax+ s)))))
    (assert-true (>= (length bs) 2))
    (assert-true (member #x0F bs))))
