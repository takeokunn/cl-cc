;;;; tests/unit/compile/codegen-slot-predicates-tests.lisp — Slot-predicate codegen tests

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest-each codegen-slot-predicate-emits-instruction
  "slot-boundp, slot-exists-p, and slot-makunbound emit their dedicated VM instructions."
  :cases (("boundp"     'slot-boundp     'cl-cc/vm::vm-slot-boundp)
          ("exists-p"   'slot-exists-p   'cl-cc/vm::vm-slot-exists-p)
          ("makunbound" 'slot-makunbound 'cl-cc/vm::vm-slot-makunbound))
  (form inst-type)
  (let ((ctx (make-ctx-with-vars 'obj)))
    (compile-ast (make-call form (make-var 'obj) (make-quoted 'field)) ctx)
    (let ((inst (codegen-find-inst ctx inst-type)))
      (assert-true inst)
      (assert-eq 'field (cl-cc/vm::vm-slot-name-sym inst)))))
