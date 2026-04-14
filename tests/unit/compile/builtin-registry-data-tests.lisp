;;;; tests/unit/compile/builtin-registry-data-tests.lisp — Builtin Registry Data tests
(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest-each builtin-registry-data-table-sizes
  "The raw registry tables retain representative sizes."
  :cases (("unary"      cl-cc::*builtin-unary-entries*      100)
          ("binary"     cl-cc::*builtin-binary-entries*     20)
          ("string-cmp" cl-cc::*builtin-string-cmp-entries* 10))
  (table min-size)
  (assert-true (> (length table) min-size)))

(deftest-each builtin-registry-data-representative-mappings
  "Representative raw mappings stay wired to the expected constructors."
  :cases (("car"    'car    cl-cc::*builtin-unary-entries*      'cl-cc::make-vm-car)
          ("mod"    'mod    cl-cc::*builtin-binary-entries*     'cl-cc::make-vm-mod)
          ("string=" 'string= cl-cc::*builtin-string-cmp-entries* 'cl-cc::make-vm-string=))
  (sym table expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym table))))
