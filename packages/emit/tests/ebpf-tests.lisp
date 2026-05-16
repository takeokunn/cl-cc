(in-package :cl-cc/test)

(defsuite ebpf-suite :description "eBPF emit planning/encoding tests"
  :parent cl-cc-unit-suite)

(in-suite ebpf-suite)

(deftest ebpf-plan-rejects-heap-ops
  "eBPF planner rejects obvious heap operations."
  (assert-signals error
    (cl-cc/emit:plan-ebpf-program 'bad '((alloc 16) (:exit)))))

(deftest ebpf-emit-bytecode-encodes-basic-program
  "Simple eBPF sequence encodes to 8-byte aligned instruction stream."
  (multiple-value-bind (plan bytes)
      (cl-cc/emit:compile-ebpf-program 'ok
                                       '((:mov64-imm 0 42)
                                         (:add64-imm 0 1)
                                         (:exit)))
    (assert-equal 'ok (cl-cc/emit::ebpf-program-plan-program-name plan))
    (assert-true (cl-cc/emit::ebpf-program-plan-verifier-safe-p plan))
    (assert-equal 24 (length bytes))
    ;; mov64 imm r0, 42 => opcode B7, dst/src byte 00, imm low byte 2A
    (assert-equal #xB7 (aref bytes 0))
    (assert-equal #x00 (aref bytes 1))
    (assert-equal #x2A (aref bytes 4))
    ;; add64 imm r0, 1 => opcode 07 at second insn
    (assert-equal #x07 (aref bytes 8))
    (assert-equal #x01 (aref bytes 12))
    ;; exit opcode at third insn
    (assert-equal #x95 (aref bytes 16))))
