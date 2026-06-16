;;; runtime-stdlib-2-completion-tests.lisp — Tests for runtime-stdlib-2 gaps

(in-package :cl-cc/test)

(defsuite runtime-stdlib-2-completion-suite
  :description "Runtime-stdlib-2 completion surface tests"
  :parent cl-cc-unit-suite)

(in-suite runtime-stdlib-2-completion-suite)

;; ── FR-895: Symbol Table Freeze/Thaw (implementation in vm.lisp) ───────
(deftest runtime-stdlib-2-symbol-table-freeze-thaw-surface
  "FR-895: symbol-table freeze/thaw functions are present."
  (assert-true (fboundp 'cl-cc/vm::freeze-symbol-table))
  (assert-true (fboundp 'cl-cc/vm::thaw-symbol-table)))

;; ── FR-896: Package Lock (implementation in vm.lisp) ───────────────────
;; NOTE: lock-package and package-locked-p are defined in vm.lisp

;; ── FR-917: Reproducible Build ─────────────────────────────────────────
(deftest runtime-stdlib-2-reproducible-build-surface
  "FR-917: reproducible-build helpers are present."
  (assert-true (fboundp 'cl-cc/vm::build-fingerprint))
  (assert-true (fboundp 'cl-cc/vm::source-date-epoch))
  (assert-true (fboundp 'cl-cc/vm::build-timestamp)))

;; ── FR-920: Forward References ─────────────────────────────────────────
(deftest runtime-stdlib-2-forward-reference-surface
  "FR-920: forward-reference helpers are present."
  (assert-true (fboundp 'cl-cc/vm::vm-declare-forward-reference))
  (assert-true (fboundp 'cl-cc/vm::vm-resolve-forward-references)))

;; ── FR-820: Print-Circle ───────────────────────────────────────────────
(deftest runtime-stdlib-2-print-circle-surface
  "FR-820: *print-circle* exists in the VM package."
  (assert-true (find-symbol "*PRINT-CIRCLE*" :cl-cc/vm)))
