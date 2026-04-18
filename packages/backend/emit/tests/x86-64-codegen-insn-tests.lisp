;;;; tests/unit/emit/x86-64-codegen-insn-tests.lisp — x86-64 Individual Instruction Emitter Tests
;;;;
;;;; Tests for individual x86-64 instruction emitters in src/emit/x86-64-codegen.lisp:
;;;; emit-vm-bswap, emit-vm-add, emit-vm-select, emit-vm-jump-zero-inst,
;;;; emit-vm-logcount, emit-vm-integer-length, emit-vm-call-like-inst,
;;;; emit-vm-tail-call-inst
;;;;
;;;; Depends on %x86-collect-bytes helper defined in x86-64-codegen-tests.lisp
;;;; (loaded before this file via :serial t ASDF).

(in-package :cl-cc/test)

(in-suite x86-64-codegen-suite)

(deftest x86-64-bswap-emitter-encoding
  "emit-vm-bswap emits a MOV followed by BSWAP r32."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/emit::emit-vm-bswap (cl-cc::make-vm-bswap :dst :R0 :src :R1) s)))))
    (assert-= 5 (length bytes))
    (assert-= #x48 (first bytes))
    (assert-= #x89 (second bytes))
    (assert-= #xC8 (third bytes))
    (assert-= #x0F (fourth bytes))
    (assert-= #xC8 (fifth bytes))))

(deftest x86-64-add-emitter-two-address-lowering
  "emit-vm-add lowers to MOV dst,lhs before ADD dst,rhs to satisfy x86 two-address form."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/emit::emit-vm-add
                   (cl-cc::make-vm-add :dst :R0 :lhs :R1 :rhs :R2) s)))))
    (assert-= 6 (length bytes))
    ;; MOV rax,rcx / ADD rax,rdx
    (assert-= #x48 (nth 0 bytes))
    (assert-= #x89 (nth 1 bytes))
    (assert-= #xC8 (nth 2 bytes))
    (assert-= #x48 (nth 3 bytes))
    (assert-= #x01 (nth 4 bytes))
    (assert-= #xD0 (nth 5 bytes))))

(deftest x86-64-select-emitter-encoding
  "emit-vm-select uses MOV + TEST + CMOVNE branchless lowering."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/emit::emit-vm-select
                   (cl-cc::make-vm-select :dst :R0 :cond-reg :R1 :then-reg :R2 :else-reg :R3)
                   s)))))
    (assert-= 10 (length bytes))
    (assert-= #x48 (nth 0 bytes))
    (assert-= #x89 (nth 1 bytes))
    (assert-= #x48 (nth 3 bytes))
    (assert-= #x85 (nth 4 bytes))
    (assert-= #x48 (nth 6 bytes))
    (assert-= #x0F (nth 7 bytes))
    (assert-= #x45 (nth 8 bytes))))

(deftest x86-64-jump-zero-test-je-adjacent
  "emit-vm-jump-zero-inst emits TEST immediately followed by JE rel32."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/emit::emit-vm-jump-zero-inst
                   (cl-cc::make-vm-jump-zero :reg :R1 :label "L1")
                   s 0 (let ((ht (make-hash-table :test #'equal)))
                         (setf (gethash "L1" ht) 9)
                         ht))))))
    (assert-= 9 (length bytes))
    (assert-= #x48 (nth 0 bytes))
    (assert-= #x85 (nth 1 bytes))
    (assert-= #x0F (nth 3 bytes))
    (assert-= #x84 (nth 4 bytes))))

(deftest x86-64-logcount-emitter-encoding
  "emit-vm-logcount emits POPCNT with the expected opcode sequence."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/emit::emit-vm-logcount
                   (cl-cc::make-vm-logcount :dst :R0 :src :R1) s)))))
    (assert-= 5 (length bytes))
    (assert-= #xF3 (nth 0 bytes))
    (assert-= #x48 (nth 1 bytes))
    (assert-= #x0F (nth 2 bytes))
    (assert-= #xB8 (nth 3 bytes))))

(deftest x86-64-integer-length-emitter-encoding
  "emit-vm-integer-length emits xor/test/je/bsr/add sequence."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/emit::emit-vm-integer-length
                   (cl-cc::make-vm-integer-length :dst :R0 :src :R1) s)))))
    (assert-= 16 (length bytes))
    ;; xor rax,rax / test rcx,rcx / je rel8 / bsr rax,rcx / add rax,1
    (assert-= #x48 (nth 0 bytes))
    (assert-= #x31 (nth 1 bytes))
    (assert-= #x48 (nth 3 bytes))
    (assert-= #x85 (nth 4 bytes))
    (assert-= #x74 (nth 6 bytes))
    (assert-= #x48 (nth 8 bytes))
    (assert-= #x0F (nth 9 bytes))
    (assert-= #xBD (nth 10 bytes))))

(deftest x86-64-call-encoding
  "vm-call emits CALL r64 (#xFF /2) followed by MOV dst←rax: 6 bytes total."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/emit::emit-vm-call-like-inst
                   (cl-cc::make-vm-call :dst :R0 :func :R1 :args nil) s)))))
    (assert-= 6 (length bytes))
    (assert-= #x48 (nth 0 bytes))
    (assert-= #xFF (nth 1 bytes))
    (assert-= #xD1 (nth 2 bytes))
    (assert-= #x48 (nth 3 bytes))
    (assert-= #x89 (nth 4 bytes))
    (assert-= #xC0 (nth 5 bytes))))

(deftest x86-64-tail-call-encoding
  "vm-tail-call emits JMP r64 (#xFF /4) only: 3 bytes, no move-to-result register."
  (let ((bytes (%x86-collect-bytes
                (lambda (s)
                  (cl-cc/emit::emit-vm-tail-call-inst
                   (cl-cc::make-vm-tail-call :dst :R0 :func :R1 :args nil) s)))))
    (assert-= 3 (length bytes))
    (assert-= #x48 (nth 0 bytes))
    (assert-= #xFF (nth 1 bytes))
    (assert-= #xE1 (nth 2 bytes))))
