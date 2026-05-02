;;;; tests/unit/emit/x86-64-vm-emitter-tests.lisp — VM Instruction Emitter Byte-Size Tests
;;;;
;;;; Continuation of x86-64-encoding-tests.lisp.
;;;; Tests for VM instruction emitters: exact byte counts and instruction-size
;;;; dispatch table completeness.

(in-package :cl-cc/test)

(in-suite x86-64-encoding-suite)

;;; ─── VM instruction emitter byte sizes ──────────────────────────────────────

(deftest-each x86-vm-emitter-byte-size
  "VM instruction emitters produce the documented exact byte count."
  :cases (("vm-neg"      (lambda (s) (cl-cc/codegen::emit-vm-neg
                           (cl-cc::make-vm-neg :dst :R0 :src :R1) s))      6)
          ("vm-not"      (lambda (s) (cl-cc/codegen::emit-vm-not
                           (cl-cc::make-vm-not :dst :R0 :src :R1) s))     10)
          ("vm-lognot"   (lambda (s) (cl-cc/codegen::emit-vm-lognot
                           (cl-cc::make-vm-lognot :dst :R0 :src :R1) s))   6)
          ("vm-inc"      (lambda (s) (cl-cc/codegen::emit-vm-inc
                           (cl-cc::make-vm-inc :dst :R0 :src :R1) s))      7)
          ("vm-dec"      (lambda (s) (cl-cc/codegen::emit-vm-dec
                           (cl-cc::make-vm-dec :dst :R0 :src :R1) s))      7)
          ("vm-abs"      (lambda (s) (cl-cc/codegen::emit-vm-abs
                           (make-vm-abs :dst :R0 :src :R1) s))            15)
          ("vm-min"      (lambda (s) (cl-cc/codegen::emit-vm-min
                           (make-vm-min :dst :R0 :lhs :R1 :rhs :R2) s))  10)
          ("vm-max"      (lambda (s) (cl-cc/codegen::emit-vm-max
                           (make-vm-max :dst :R0 :lhs :R1 :rhs :R2) s))  10)
          ("vm-logand"   (lambda (s) (cl-cc/codegen::emit-vm-logand
                           (make-vm-logand :dst :R0 :lhs :R1 :rhs :R2) s)) 6)
          ("vm-logior"   (lambda (s) (cl-cc/codegen::emit-vm-logior
                           (make-vm-logior :dst :R0 :lhs :R1 :rhs :R2) s)) 6)
          ("vm-logxor"   (lambda (s) (cl-cc/codegen::emit-vm-logxor
                           (make-vm-logxor :dst :R0 :lhs :R1 :rhs :R2) s)) 6)
          ("vm-logeqv"   (lambda (s) (cl-cc/codegen::emit-vm-logeqv
                           (cl-cc::make-vm-logeqv :dst :R0 :lhs :R1 :rhs :R2) s))  9)
          ("vm-and"      (lambda (s) (cl-cc/codegen::emit-vm-and
                           (cl-cc::make-vm-and :dst :R0 :lhs :R1 :rhs :R2) s))    17)
          ("vm-or"       (lambda (s) (cl-cc/codegen::emit-vm-or
                           (cl-cc::make-vm-or  :dst :R0 :lhs :R1 :rhs :R2) s))    17)
          ("vm-true-pred"  (lambda (s) (cl-cc/codegen::emit-vm-true-pred
                             (cl-cc::make-vm-number-p :dst :R0 :src :R1) s))       10)
          ("vm-false-pred" (lambda (s) (cl-cc/codegen::emit-vm-false-pred
                             (cl-cc::make-vm-cons-p :dst :R0 :src :R1) s))         10)
          ("vm-truncate" (lambda (s) (cl-cc/codegen::emit-vm-truncate
                           (make-vm-truncate :dst :R0 :lhs :R1 :rhs :R2) s))       21)
          ("vm-rem"      (lambda (s) (cl-cc/codegen::emit-vm-rem
                           (cl-cc::make-vm-rem :dst :R0 :lhs :R1 :rhs :R2) s))     21)
          ("vm-ash"      (lambda (s) (cl-cc/codegen::emit-vm-ash
                           (make-vm-ash :dst :R0 :lhs :R1 :rhs :R2) s))            24)
          ("vm-mul"      (lambda (s) (cl-cc/codegen::emit-vm-mul
                           (cl-cc::make-vm-mul :dst :R0 :lhs :R1 :rhs :R2) s))      7)
          ("vm-div"      (lambda (s) (cl-cc/codegen::emit-vm-div
                           (cl-cc::make-vm-div :dst :R0 :lhs :R1 :rhs :R2) s))     34)
          ("vm-mod"      (lambda (s) (cl-cc/codegen::emit-vm-mod
                           (cl-cc::make-vm-mod :dst :R0 :lhs :R1 :rhs :R2) s))     37)
          ("vm-lt"       (lambda (s) (cl-cc/codegen::emit-vm-lt
                           (cl-cc::make-vm-lt :dst :R0 :lhs :R1 :rhs :R2) s))      10)
          ("vm-gt"       (lambda (s) (cl-cc/codegen::emit-vm-gt
                           (cl-cc::make-vm-gt :dst :R0 :lhs :R1 :rhs :R2) s))      10)
          ("vm-le"       (lambda (s) (cl-cc/codegen::emit-vm-le
                           (cl-cc::make-vm-le :dst :R0 :lhs :R1 :rhs :R2) s))      10)
          ("vm-ge"       (lambda (s) (cl-cc/codegen::emit-vm-ge
                           (cl-cc::make-vm-ge :dst :R0 :lhs :R1 :rhs :R2) s))      10)
          ("vm-num-eq"   (lambda (s) (cl-cc/codegen::emit-vm-num-eq
                           (make-vm-num-eq :dst :R0 :lhs :R1 :rhs :R2) s))         10)
          ("vm-eq"       (lambda (s) (cl-cc/codegen::emit-vm-eq
                           (make-vm-eq :dst :R0 :lhs :R1 :rhs :R2) s))             10)
          ("vm-null-p"   (lambda (s) (cl-cc/codegen::emit-vm-null-p
                           (cl-cc::make-vm-null-p :dst :R0 :src :R1) s))           10)
          ("vm-logtest"  (lambda (s) (cl-cc/codegen::emit-vm-logtest
                           (cl-cc::make-vm-logtest :dst :R0 :lhs :R1 :rhs :R2) s)) 13)
          ("vm-logbitp"  (lambda (s) (cl-cc/codegen::emit-vm-logbitp
                           (cl-cc::make-vm-logbitp :dst :R0 :lhs :R1 :rhs :R2) s)) 15))
  (emit-fn expected-size)
  (assert-equal expected-size (length (%x86-encoding-collect-bytes emit-fn))))

;;; ─── instruction-size table ──────────────────────────────────────────────────

(deftest x86-instruction-size-table-coverage
  "Instruction size table has entries for all expected VM types."
  (dolist (tp '(cl-cc/vm::vm-const cl-cc/vm::vm-move cl-cc/vm::vm-add cl-cc/vm::vm-sub cl-cc/vm::vm-mul
                cl-cc/vm::vm-halt cl-cc/vm::vm-label cl-cc/vm::vm-jump cl-cc/vm::vm-jump-zero cl-cc/vm::vm-ret
                cl-cc/vm::vm-lt cl-cc/vm::vm-gt cl-cc/vm::vm-le cl-cc/vm::vm-ge cl-cc/vm::vm-num-eq cl-cc/vm::vm-eq
                cl-cc/vm::vm-neg cl-cc/vm::vm-not cl-cc/vm::vm-lognot cl-cc/vm::vm-inc cl-cc/vm::vm-dec
                cl-cc/vm::vm-abs cl-cc/vm::vm-min cl-cc/vm::vm-max cl-cc/vm::vm-ash
                cl-cc/vm::vm-truncate cl-cc/vm::vm-rem cl-cc/vm::vm-div cl-cc/vm::vm-mod
                cl-cc/vm::vm-and cl-cc/vm::vm-or cl-cc/vm::vm-logand cl-cc/vm::vm-logior cl-cc/vm::vm-logxor
                cl-cc/vm::vm-logeqv cl-cc/vm::vm-logtest cl-cc/vm::vm-logbitp
                cl-cc/vm::vm-null-p cl-cc/vm::vm-number-p cl-cc/vm::vm-integer-p
                cl-cc/vm::vm-cons-p cl-cc/vm::vm-symbol-p cl-cc/vm::vm-function-p
                cl-cc/codegen::vm-spill-store cl-cc/codegen::vm-spill-load))
    (assert-true (gethash tp cl-cc/codegen::*x86-64-instruction-sizes*))))
