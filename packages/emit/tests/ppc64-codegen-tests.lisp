(in-package :cl-cc/test)

(defsuite ppc64-codegen-suite :description "POWER10/PPC64 encoder tests"
  :parent cl-cc-unit-suite)

(in-suite ppc64-codegen-suite)

(deftest power10-basic-encoders-and-big-endian-emission
  "FR-695: POWER10 encoders produce stable 32-bit words and big-endian byte emission."
  (assert-equal #x60000000 (cl-cc/codegen:encode-ppc64-nop))
  (assert-equal #x38610010 (cl-cc/codegen:encode-power10-addi cl-cc/codegen:r3 cl-cc/codegen:r1 16))
  (assert-equal #x7C632214 (cl-cc/codegen:encode-power10-add cl-cc/codegen:r3 cl-cc/codegen:r3 cl-cc/codegen:r4))
  (assert-equal #xE8610010 (cl-cc/codegen:encode-power10-ld cl-cc/codegen:r3 cl-cc/codegen:r1 16))
  (let ((bytes nil))
    (cl-cc/codegen:emit-ppc64-instr #x38610010 (lambda (b) (push b bytes)))
    (assert-equal '(#x38 #x61 #x00 #x10) (nreverse bytes))))

(deftest power10-elfv2-frame-and-backend-gate
  "FR-695: ELFv2 prologue/epilogue builders validate frame sizes and backend availability gate."
  (let ((prologue (cl-cc/codegen:ppc64-elfv2-prologue :frame-size 32))
        (epilogue (cl-cc/codegen:ppc64-elfv2-epilogue :frame-size 32)))
    (assert-equal 3 (length prologue))
    (assert-equal 3 (length epilogue))
    (assert-equal (cl-cc/codegen:encode-power10-addi cl-cc/codegen:r1 cl-cc/codegen:r1 -32)
                  (third prologue))
    (assert-equal (cl-cc/codegen:encode-power10-addi cl-cc/codegen:r1 cl-cc/codegen:r1 32)
                  (first epilogue)))
  (assert-signals error (cl-cc/codegen:ppc64-elfv2-prologue :frame-size 24))
  (let ((cl-cc/codegen:*ppc64-enabled* nil))
    (assert-false (cl-cc/codegen:ppc64-backend-available-p)))
  (let ((cl-cc/codegen:*ppc64-enabled* t))
    (assert-true (cl-cc/codegen:ppc64-backend-available-p))))
