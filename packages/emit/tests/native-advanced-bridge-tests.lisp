;;;; packages/emit/tests/native-advanced-bridge-tests.lisp

(in-package :cl-cc/test)

(defsuite native-advanced-emit-suite
  :description "Native advanced emit/codegen FR evidence tests"
  :parent cl-cc-unit-suite)

(in-suite native-advanced-emit-suite)

(deftest fr-534-speculative-execution-mitigation-lfence
  "FR-534: speculative execution mitigation exposes LFENCE barrier emission."
  (let ((bytes (cl-cc/codegen::with-output-to-vector (out)
                 (cl-cc/codegen:emit-x86-64-speculation-barrier out))))
    (assert-equal '(#x0F #xAE #xE8) (coerce bytes 'list)))
  (let ((cl-cc/codegen:*x86-64-spectre-mitigations-enabled* t))
    (assert-true (cl-cc/codegen:x86-64-speculative-execution-mitigation-enabled-p))))

(deftest fr-690-llvm-ir-backend-lowering-emits-module
  "FR-690: LLVM IR backend emits textual LLVM IR module scaffolding without bridge markers."
  (let ((ir (cl-cc/emit:emit-llvm-ir nil :name "fr690" :target-triple "x86_64-apple-darwin")))
    (assert-true (search "ModuleID" ir))
    (assert-true (search "target datalayout" ir))
    (assert-false (search "TODO" ir))
    (assert-false (search "bridge-only" ir))
    (assert-true (member :fr-690 (cl-cc/emit:llvm-ir-bridge-capabilities)))))

(deftest fr-712-mlir-integration-bridge-emits-module
  "FR-712: MLIR lowering emits textual MLIR without bridge markers."
  (let ((mlir (cl-cc/emit:emit-mlir nil :name "fr712")))
    (assert-true (search "module @fr712" mlir))
    (assert-true (search "func.func @clcc_entry() -> i64" mlir))
    (assert-true (search "arith.constant 0 : i64" mlir))
    (assert-true (search "func.return %reg0 : i64" mlir))
    (assert-false (search "TODO" mlir))
    (assert-false (search "bridge-only" mlir))
    (assert-true (member :fr-712 (cl-cc/emit:mlir-bridge-capabilities)))))

(deftest fr-721-macro-fusion-awareness-preserves-cmp-jcc
  "FR-721: macro-fusion helper recognizes compare/test plus conditional branch."
  (let* ((cmp (cl-cc:make-vm-lt :dst :R0 :lhs :R1 :rhs :R2))
         (br  (cl-cc:make-vm-jump-zero :reg :R0 :label "cold")))
    (assert-true (cl-cc/codegen:x86-64-macro-fusion-candidate-p cmp br))
    (assert-equal (list cmp br)
                  (cl-cc/codegen:x86-64-preserve-macro-fusion (list cmp br)))))
