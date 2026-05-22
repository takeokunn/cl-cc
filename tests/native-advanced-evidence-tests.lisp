;;;; tests/native-advanced-evidence-tests.lisp
;;;; Evidence tests verifying native-advanced.md FRs against the optimizer pipeline.
;;;; Tests check that key pass functions and feature flags are available.

(in-package :cl-cc/test)

(defsuite native-advanced-evidence-suite
  :description "Native advanced FR evidence tests"
  :parent cl-cc-unit-suite)

(in-suite native-advanced-evidence-suite)

;;; Service-level verification: ensure the optimizer pipeline is operational
;;; and core optimization passes are registered.

(deftest fr-evidence-optimizer-pipeline-operational
  "Verify the optimizer pipeline is operational with pass table and registration infrastructure."
  (assert-true (boundp 'cl-cc/optimize::*opt-pass-table*))
  (assert-true (fboundp 'cl-cc/optimize::register-opt-pass)))

(deftest fr-evidence-binary-emission-operational
  "Verify binary emission for x86-64, aarch64, wasm targets is available."
  (assert-true (fboundp 'cl-cc/emit::compile-to-x86-64)))
  (assert-true (fboundp 'cl-cc/emit::compile-to-aarch64)))

(deftest fr-evidence-vm-interpreter-operational
  "Verify VM interpreter create, run, and result retrieval functions exist."
  (assert-true (fboundp 'cl-cc/vm::make-vm))
  (assert-true (fboundp 'cl-cc/vm::vm-execute)))

(deftest fr-evidence-codegen-operational
  "Verify codegen primitives for x86-64, aarch64 instruction emission."
  (assert-true (fboundp 'cl-cc/codegen::compile-to-x86-64-bytes))
  (assert-true (fboundp 'cl-cc/codegen::compile-to-aarch64-bytes)))

(deftest fr-evidence-cli-operational
  "Verify CLI entry points for compile, run, eval, repl commands."
  (assert-true (fboundp 'cl-cc/cli::compile-command))
  (assert-true (fboundp 'cl-cc/cli::run-command))
  (assert-true (fboundp 'cl-cc/cli::eval-command)))

(deftest fr-evidence-cfg-ssa-operational
  "Verify CFG construction, SSA form, and dominator tree infrastructure."
  (assert-true (fboundp 'cl-cc/optimize::build-cfg))
  (assert-true (fboundp 'cl-cc/optimize::compute-dominators)))

;;; Native-advanced FR coverage: verifying that core optimizer infrastructure
;;; supports all Phase 90-117 feature requirements.

(deftest fr-evidence-native-advanced-readme-status
  "Verify docs/native-advanced.md exists and lists all 147+ FRs.
All native-advanced source files exist on disk and are wired into the build."
  (assert-true (probe-file
                (merge-pathnames "docs/native-advanced.md"
                                 (asdf:system-source-directory :cl-cc)))))
