;;; runtime-stdlib-2-completion-tests.lisp — Tests for runtime-stdlib-2 gaps

(in-package :cl-cc/vm)

;; ── FR-895: Symbol Table Freeze/Thaw (implementation in vm.lisp) ───────
(assert-true (functionp 'freeze-symbol-table) "freeze-symbol-table")
(assert-true (functionp 'thaw-symbol-table) "thaw-symbol-table")

;; ── FR-896: Package Lock (implementation in vm.lisp) ───────────────────
;; NOTE: lock-package and package-locked-p are defined in vm.lisp

;; ── FR-917: Reproducible Build ─────────────────────────────────────────
(assert-true (functionp 'build-fingerprint) "build-fingerprint")
(assert-true (functionp 'source-date-epoch) "source-date-epoch")
(assert-true (functionp 'build-timestamp) "build-timestamp")

;; ── FR-920: Forward References ─────────────────────────────────────────
(assert-true (functionp 'declare-forward-reference) "declare-forward-reference")
(assert-true (functionp 'resolve-forward-reference) "resolve-forward-reference")
(assert-true (functionp 'resolve-all-forward-references) "resolve-all-forward-references")

;; ── FR-820: Print-Circle ───────────────────────────────────────────────
(assert-true (find-symbol "*PRINT-CIRCLE*" :cl-cc/vm) "*print-circle* exists")
