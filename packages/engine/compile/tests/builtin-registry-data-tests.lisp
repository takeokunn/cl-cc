;;;; tests/unit/compile/builtin-registry-data-tests.lisp — Builtin Registry Data tests
(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest-each builtin-registry-data-table-sizes
  "The raw registry tables retain representative sizes."
  :cases (("unary"      cl-cc/compile::*builtin-unary-entries*      100)
          ("binary"     cl-cc/compile::*builtin-binary-entries*     20)
          ("string-cmp" cl-cc/compile::*builtin-string-cmp-entries* 10))
  (table min-size)
  (assert-true (> (length table) min-size)))

;; NOTE: "car" and "mod" cases removed — their constructor mapping is already
;; asserted via builtin-registry-constructor-symbols in builtin-registry-tests.lisp,
;; which tests the same ctor through the registry built FROM these entries tables
;; (see builtin-registry.lisp line 93: dolist over *builtin-unary-entries* etc.).
;; Kept "set" and "string=" — no registry-test asserts their ctor directly.
(deftest-each builtin-registry-data-representative-mappings
  "Representative raw mappings stay wired to the expected constructors."
  :cases (("set"    'set    cl-cc/compile::*builtin-binary-entries*     'cl-cc::make-vm-set-symbol-value)
          ("string=" 'string= cl-cc/compile::*builtin-string-cmp-entries* 'cl-cc::make-vm-string=))
  (sym table expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym table))))
