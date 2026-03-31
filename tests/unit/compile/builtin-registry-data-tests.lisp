;;;; tests/unit/compile/builtin-registry-data-tests.lisp — Builtin Registry Data tests
(in-package :cl-cc/test)
(in-suite cl-cc-suite)

(deftest builtin-registry-data-table-sizes
  "The raw registry tables retain representative sizes."
  (assert-true (> (length cl-cc::*builtin-unary-entries*) 100))
  (assert-true (> (length cl-cc::*builtin-binary-entries*) 20))
  (assert-true (> (length cl-cc::*builtin-string-cmp-entries*) 10)))

(deftest builtin-registry-data-representative-mappings
  "Representative raw mappings stay wired to the expected constructors."
  (assert-equal 'cl-cc::make-vm-car
                (cdr (assoc 'car cl-cc::*builtin-unary-entries*)))
  (assert-equal 'cl-cc::make-vm-mod
                (cdr (assoc 'mod cl-cc::*builtin-binary-entries*)))
  (assert-equal 'cl-cc::make-vm-string=
                (cdr (assoc 'string= cl-cc::*builtin-string-cmp-entries*))))
