;;;; tests/unit/vm/vm-instructions-tests.lisp — VM instruction definition tests

(in-package :cl-cc/test)

(defsuite vm-instructions-suite
  :description "VM instruction definition unit tests"
  :parent cl-cc-unit-suite)

(in-suite vm-instructions-suite)

(deftest-each vm-instruction-sexp-tags
  "Representative instruction constructors round-trip to their sexp tags."
  :cases (("const"        (cl-cc::make-vm-const :dst :r0 :value 42) :const)
          ("move"         (cl-cc::make-vm-move :dst :r0 :src :r1) :move)
          ("add"          (cl-cc::make-vm-add :dst :r0 :lhs :r1 :rhs :r2) :add)
          ("slot-boundp"  (cl-cc::make-vm-slot-boundp :dst :r0 :obj-reg :r1 :slot-name-sym 'field)
                           :slot-boundp)
          ("closure"      (cl-cc::make-vm-closure :dst :r0 :label 'L :params '(x)
                                                  :optional-params nil :rest-param nil
                                                  :key-params nil :captured '(:r1))
                           :closure)
          ("make-closure" (cl-cc::make-vm-make-closure :dst :r0 :label 'L :params '(x)
                                                      :env-regs '(:r1))
                           :make-closure))
  (inst expected-tag)
  (assert-eq expected-tag (first (cl-cc::instruction->sexp inst))))

(deftest vm-instruction-readers
  "Key instruction reader accessors expose constructor arguments."
  (let ((inst (cl-cc::make-vm-closure :dst :r7 :label 'LBL :params '(a b)
                                     :optional-params '(c) :rest-param 'rest
                                     :key-params '((:k k)) :captured '(:r1 :r2))))
    (assert-eq :r7 (cl-cc/vm::vm-dst inst))
    (assert-eq 'LBL (cl-cc/vm::vm-label-name inst))
    (assert-equal '(a b) (cl-cc/vm::vm-closure-params inst))
    (assert-equal '(c) (cl-cc/vm::vm-closure-optional-params inst))
    (assert-eq 'rest (cl-cc/vm::vm-closure-rest-param inst))
    (assert-equal '((:k k)) (cl-cc/vm::vm-closure-key-params inst))
    (assert-equal '(:r1 :r2) (cl-cc/vm::vm-captured-vars inst))))
