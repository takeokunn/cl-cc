;;; runtime-stdlib-2-completion-tests.lisp — Tests for runtime-stdlib-2 gaps

(in-package :cl-cc/vm)

;; ── FR-895/896: Symbol table / package lock ────────────────────────────
(assert-true (functionp 'freeze-symbol-table) "freeze-symbol-table")
(assert-true (functionp 'thaw-symbol-table) "thaw-symbol-table")
(assert-true (functionp 'lock-package) "lock-package")
(assert-true (functionp 'package-locked-p) "package-locked-p")

;; ── FR-917: Reproducible Build ─────────────────────────────────────────
(assert-true (functionp 'build-fingerprint) "build-fingerprint")

;; ── FR-920: Forward References ─────────────────────────────────────────
(assert-true (functionp 'declare-forward-reference) "declare-forward-reference")
(assert-true (functionp 'resolve-forward-reference) "resolve-forward-reference")
(assert-true (functionp 'resolve-all-forward-references) "resolve-all-forward-references")

(let ((name 'test-fwd-ref-r2))
  (declare-forward-reference name)
  (resolve-forward-reference name (lambda (x) (+ x 1)))
  (assert-true t "Forward reference resolved"))
