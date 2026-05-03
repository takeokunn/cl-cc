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

;; "car" and "mod" ctors are covered by builtin-registry-constructor-symbols
;; in builtin-registry-tests.lisp. "set" and "string=" are not covered there.
(deftest-each builtin-registry-data-representative-mappings
  "Representative raw mappings stay wired to the expected constructors."
  :cases (("set"    'set    cl-cc/compile::*builtin-binary-entries*     'cl-cc::make-vm-set-symbol-value)
          ("string=" 'string= cl-cc/compile::*builtin-string-cmp-entries* 'cl-cc::make-vm-string=)
          ("find-package" 'find-package cl-cc/compile::*builtin-unary-entries* 'cl-cc::make-vm-find-package))
  (sym table expected-ctor)
  (assert-equal expected-ctor (cdr (assoc sym table))))

(deftest-each builtin-registry-data-float-mappings
  "Float builtin raw mappings resolve to the VM constructors exported for compile-time lookup."
  :cases (("float" 'float 'cl-cc::make-vm-float-inst)
          ("float-precision" 'float-precision 'cl-cc::make-vm-float-precision)
          ("float-radix" 'float-radix 'cl-cc::make-vm-float-radix)
          ("float-sign" 'float-sign 'cl-cc::make-vm-float-sign)
          ("float-digits" 'float-digits 'cl-cc::make-vm-float-digits)
          ("decode-float" 'decode-float 'cl-cc::make-vm-decode-float)
          ("integer-decode-float" 'integer-decode-float 'cl-cc::make-vm-integer-decode-float))
  (sym expected-ctor)
  (assert-eq expected-ctor (cdr (assoc sym cl-cc/compile::*builtin-unary-entries*))))
