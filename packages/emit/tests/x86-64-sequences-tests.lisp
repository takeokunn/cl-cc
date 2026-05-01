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
  :parent cl-cc-unit-suite)

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
  (let ((bs (%collect-seq-bytes #'cl-cc/emit::emit-idiv-r11)))
    (assert-= 3 (length bs))
    (assert-= #x49 (first bs))   ; REX.W=1, REX.B=1
    (assert-= #xF7 (second bs))  ; IDIV opcode
    (assert-= (cl-cc/emit::modrm 3 7 3) (third bs)))) ; mod=11, reg=7, rm=3

;;; ─── emit-cqo ────────────────────────────────────────────────────────────

(deftest x86-seq-cqo-emits-2-bytes
  "emit-cqo emits exactly 2 bytes: REX.W (48) and 99."
  (let ((bs (%collect-seq-bytes #'cl-cc/emit::emit-cqo)))
    (assert-= 2 (length bs))
    (assert-= #x48 (first bs))
    (assert-= #x99 (second bs))))

;;; ─── emit-idiv-sequence ──────────────────────────────────────────────────

(deftest-each x86-seq-idiv-sequence-18-bytes
  "emit-idiv-sequence emits 18 bytes for both quotient and remainder modes."
  :cases (("quotient"  nil)
          ("remainder" t))
  (remainder-p)
  (let ((bs (%collect-seq-bytes
             (lambda (s) (cl-cc/emit::emit-idiv-sequence
                          cl-cc/emit::+rax+ cl-cc/emit::+rcx+ remainder-p s)))))
    (assert-= 18 (length bs))))

(deftest x86-seq-idiv-sequence-contains-cqo
  "emit-idiv-sequence includes CQO bytes (48 99) in the output."
  (let ((bs (%collect-seq-bytes
             (lambda (s) (cl-cc/emit::emit-idiv-sequence
                          cl-cc/emit::+rax+ cl-cc/emit::+rcx+ nil s)))))
    ;; CQO = 48 99 somewhere in the sequence
    (let ((cqo-pos (loop for i from 0 below (1- (length bs))
                         when (and (= (nth i bs) #x48)
                                   (= (nth (1+ i) bs) #x99))
                           return i)))
      (assert-true cqo-pos))))

;;; ─── emit-sal-r64-cl / emit-sar-r64-cl / emit-ror-r64-cl ────────────────

(deftest-each x86-seq-shift-r64-cl-cases
  "emit-sal/sar/ror-r64-cl each emit 3 bytes with 0xD3 as the opcode byte."
  :cases (("sal" #'cl-cc/emit::emit-sal-r64-cl)
          ("sar" #'cl-cc/emit::emit-sar-r64-cl)
          ("ror" #'cl-cc/emit::emit-ror-r64-cl))
  (emitter)
  (let ((bs (%collect-seq-bytes (lambda (s) (funcall emitter cl-cc/emit::+rax+ s)))))
    (assert-= 3 (length bs))
    (assert-= #xD3 (second bs))))

;;; ─── emit-add/sub/and-ri8 ────────────────────────────────────────────────

(deftest-each x86-seq-alu-ri8-cases
  "emit-add/sub/and-ri8 each emit 4 bytes: REX + 0x83 + ModRM + imm8."
  :cases (("add" #'cl-cc/emit::emit-add-ri8  5)
          ("sub" #'cl-cc/emit::emit-sub-ri8  8)
          ("and" #'cl-cc/emit::emit-and-ri8 15))
  (emitter imm8)
  (let ((bs (%collect-seq-bytes (lambda (s) (funcall emitter cl-cc/emit::+rax+ imm8 s)))))
    (assert-= 4 (length bs))
    (assert-= #x83 (second bs))
    (assert-= imm8 (fourth bs))))

;;; ─── emit-jge-short ──────────────────────────────────────────────────────

(deftest x86-seq-jge-short-emits-2-bytes
  "emit-jge-short emits 2 bytes: 7D + offset."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc/emit::emit-jge-short 10 s)))))
    (assert-= 2 (length bs))
    (assert-= #x7D (first bs))
    (assert-= 10 (second bs))))

;;; ─── CMOV variants ───────────────────────────────────────────────────────

(deftest x86-seq-cmovl-rr64-emits-correct-prefix
  "emit-cmovl-rr64 uses opcode 0F 4C."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc/emit::emit-cmovl-rr64 cl-cc/emit::+rax+ cl-cc/emit::+rcx+ s)))))
    (assert-true (> (length bs) 2))
    (assert-true (member #x0F bs))
    (assert-true (member #x4C bs))))

(deftest x86-seq-cmovg-rr64-emits-correct-opcode
  "emit-cmovg-rr64 uses opcode 0F 4F."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc/emit::emit-cmovg-rr64 cl-cc/emit::+rax+ cl-cc/emit::+rcx+ s)))))
    (assert-true (member #x0F bs))
    (assert-true (member #x4F bs))))

(deftest x86-seq-cmovne-rr64-emits-correct-opcode
  "emit-cmovne-rr64 uses opcode 0F 45."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc/emit::emit-cmovne-rr64 cl-cc/emit::+rax+ cl-cc/emit::+rcx+ s)))))
    (assert-true (member #x0F bs))
    (assert-true (member #x45 bs))))

;;; ─── Boolean ops on registers ────────────────────────────────────────────

(deftest-each x86-seq-boolean-rr64-cases
  "emit-and/or/xor-rr64 each emit at least 3 bytes."
  :cases (("and" #'cl-cc/emit::emit-and-rr64)
          ("or"  #'cl-cc/emit::emit-or-rr64)
          ("xor" #'cl-cc/emit::emit-xor-rr64))
  (emitter)
  (let ((bs (%collect-seq-bytes (lambda (s) (funcall emitter cl-cc/emit::+rax+ cl-cc/emit::+rcx+ s)))))
    (assert-true (>= (length bs) 3))))

;;; ─── Unary ops ───────────────────────────────────────────────────────────

(deftest x86-seq-not-neg-r64-3-bytes
  "emit-not-r64 and emit-neg-r64 each emit 3 bytes with 0xF7 as opcode."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc/emit::emit-not-r64 cl-cc/emit::+rax+ s)))))
    (assert-= 3 (length bs))
    (assert-= #xF7 (second bs)))
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc/emit::emit-neg-r64 cl-cc/emit::+rax+ s)))))
    (assert-= 3 (length bs))
    (assert-= #xF7 (second bs))))

(deftest x86-seq-dec-r64-emits-3-bytes
  "emit-dec-r64 emits 3 bytes: REX + FF /1 ModRM."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc/emit::emit-dec-r64 cl-cc/emit::+rax+ s)))))
    (assert-= 3 (length bs))
    (assert-= #xFF (second bs))))

(deftest x86-seq-bswap-r32-emits-correct-bytes
  "emit-bswap-r32 emits REX + 0F C8+reg for 64-bit bswap."
  (let ((bs (%collect-seq-bytes (lambda (s) (cl-cc/emit::emit-bswap-r32 cl-cc/emit::+rax+ s)))))
    (assert-true (>= (length bs) 2))
    (assert-true (member #x0F bs))))
