;;;; tests/unit/vm/vm-run-vm2-tests.lisp — VM2 run-vm execution tests

(in-package :cl-cc/test)

(in-suite vm-run-suite)

(deftest-each run-vm-basic-ops
  "run-vm executes a basic VM2 program and returns the correct halted value."
  :cases (("nop"
           (make-bytecode cl-cc::+op2-nop+ 0 nil nil
                          cl-cc::+op2-const+ 0 42 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           42)
          ("load-const"
           (make-bytecode cl-cc::+op2-load-const+ 0 '(1 2 3) nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           '(1 2 3))
          ("const-load"
           (make-bytecode cl-cc::+op2-const+ 0 42 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           42)
          ("load-nil"
           (make-bytecode cl-cc::+op2-load-nil+ 0 nil nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           nil)
          ("load-true"
           (make-bytecode cl-cc::+op2-load-true+ 0 nil nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           t)
          ("load-fixnum"
           (make-bytecode cl-cc::+op2-load-fixnum+ 0 123 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           123)
          ("move"
           (make-bytecode cl-cc::+op2-const+ 1 7   nil
                          cl-cc::+op2-move+  0 1   nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           7)
          ("neg"
           (make-bytecode cl-cc::+op2-const+ 1 7   nil
                          cl-cc::+op2-neg+   0 1   nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           -7)
          ("inc"
           (make-bytecode cl-cc::+op2-const+ 1 7   nil
                          cl-cc::+op2-inc+   0 1   nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           8)
          ("dec"
           (make-bytecode cl-cc::+op2-const+ 1 7   nil
                          cl-cc::+op2-dec+   0 1   nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           6)
          ("add"
           (make-bytecode cl-cc::+op2-const+ 1 3   nil
                          cl-cc::+op2-const+ 2 4   nil
                          cl-cc::+op2-add2+  0 1   2
                          cl-cc::+op2-halt2+ 0 nil nil)
           7)
          ("sub"
           (make-bytecode cl-cc::+op2-const+ 1 10  nil
                          cl-cc::+op2-const+ 2 3   nil
                          cl-cc::+op2-sub2+  0 1   2
                          cl-cc::+op2-halt2+ 0 nil nil)
           7)
          ("mul"
           (make-bytecode cl-cc::+op2-const+ 1 6   nil
                          cl-cc::+op2-const+ 2 7   nil
                          cl-cc::+op2-mul2+  0 1   2
                          cl-cc::+op2-halt2+ 0 nil nil)
           42)
          ("div"
           (make-bytecode cl-cc::+op2-const+ 1 21  nil
                          cl-cc::+op2-const+ 2 3   nil
                          cl-cc::+op2-div+   0 1   2
                          cl-cc::+op2-halt2+ 0 nil nil)
           7)
          ("mod"
           (make-bytecode cl-cc::+op2-const+ 1 22  nil
                          cl-cc::+op2-const+ 2 5   nil
                          cl-cc::+op2-mod+   0 1   2
                          cl-cc::+op2-halt2+ 0 nil nil)
           2)
          ("jump"
           (make-bytecode cl-cc::+op2-jump+ 8 nil nil
                          cl-cc::+op2-const+ 0 1 nil
                          cl-cc::+op2-const+ 0 2 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           2)
          ("jump-if-nil"
           (make-bytecode cl-cc::+op2-load-nil+ 1 nil nil
                          cl-cc::+op2-jump-if-nil+ 1 8 nil
                          cl-cc::+op2-const+ 0 1 nil
                          cl-cc::+op2-const+ 0 2 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           2)
          ("jump-if-true"
           (make-bytecode cl-cc::+op2-load-true+ 1 nil nil
                          cl-cc::+op2-jump-if-true+ 1 8 nil
                          cl-cc::+op2-const+ 0 1 nil
                          cl-cc::+op2-const+ 0 2 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           2)
          ("values/recv-values"
           (make-bytecode cl-cc::+op2-load-fixnum+ 0 10 nil
                          cl-cc::+op2-load-fixnum+ 1 20 nil
                          cl-cc::+op2-values+ 2 nil nil
                          cl-cc::+op2-load-nil+ 0 nil nil
                          cl-cc::+op2-load-nil+ 1 nil nil
                          cl-cc::+op2-recv-values+ 2 nil nil
                          cl-cc::+op2-halt2+ 1 nil nil)
           20)
          ("return"
           (make-bytecode cl-cc::+op2-load-fixnum+ 1 33 nil
                          cl-cc::+op2-return+ 1 nil nil)
           33)
          ("return-nil"
           (make-bytecode cl-cc::+op2-return-nil+ 0 nil nil)
           nil)
          ("fixnump"
           (make-bytecode cl-cc::+op2-load-fixnum+ 1 123 nil
                          cl-cc::+op2-fixnump+    0 1   nil
                          cl-cc::+op2-halt2+      0 nil nil)
           1)
          ("consp"
           (make-bytecode cl-cc::+op2-const+ 1 '(a . b) nil
                          cl-cc::+op2-consp+ 0 1 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("symbolp"
           (make-bytecode cl-cc::+op2-const+ 1 'foo nil
                          cl-cc::+op2-symbolp+ 0 1 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("functionp"
           (make-bytecode cl-cc::+op2-const+ 1 #'car nil
                          cl-cc::+op2-functionp+ 0 1 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("stringp"
           (make-bytecode cl-cc::+op2-const+ 1 "hi" nil
                          cl-cc::+op2-stringp+ 0 1 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("cons/car"
           (make-bytecode cl-cc::+op2-const+ 1 'a nil
                          cl-cc::+op2-const+ 2 'b nil
                          cl-cc::+op2-cons+  3 1 2
                          cl-cc::+op2-car+   0 3 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           'a)
          ("cdr"
           (make-bytecode cl-cc::+op2-const+ 1 'a nil
                          cl-cc::+op2-const+ 2 'b nil
                          cl-cc::+op2-cons+  3 1 2
                          cl-cc::+op2-cdr+   0 3 nil
                          cl-cc::+op2-halt2+ 0 nil nil)
           'b)
          ("vector-ref/set"
           (make-bytecode cl-cc::+op2-load-fixnum+ 1 3 nil
                          cl-cc::+op2-load-fixnum+ 2 0 nil
                          cl-cc::+op2-load-fixnum+ 3 7 nil
                          cl-cc::+op2-make-vector+ 4 1 2
                          cl-cc::+op2-vector-set+ 4 2 3
                          cl-cc::+op2-vector-ref+ 0 4 2
                          cl-cc::+op2-halt2+ 0 nil nil)
           7)
          ("hash-ref/set"
           (make-bytecode cl-cc::+op2-load-fixnum+ 1 8 nil
                          cl-cc::+op2-make-hash+  4 1 nil
                          cl-cc::+op2-const+      2 'foo nil
                          cl-cc::+op2-load-fixnum+ 3 77 nil
                          cl-cc::+op2-hash-set+   4 2 3
                          cl-cc::+op2-hash-ref+   0 4 2
                          cl-cc::+op2-halt2+      0 nil nil)
           77)
          ("global-ref/set"
           (make-bytecode cl-cc::+op2-load-fixnum+ 1 55 nil
                          cl-cc::+op2-set-global+ 'foo 1 nil
                          cl-cc::+op2-get-global+ 0 'foo nil
                          cl-cc::+op2-halt2+      0 nil nil)
           55))
  (bytecode expected)
  (let ((s (cl-cc::make-vm2-state)))
    (assert-equal expected (cl-cc/vm::run-vm bytecode s))))

(deftest-each run-vm-immediate-ops
  "run-vm executes immediate arithmetic/comparison ops."
  :cases (("add-imm"
           (make-bytecode cl-cc::+op2-const+ 1 3   nil
                          cl-cc::+op2-add-imm2+ 0 1   4
                          cl-cc::+op2-halt2+ 0 nil nil)
           7)
          ("cmp-imm"
           (make-bytecode cl-cc::+op2-const+ 1 7   nil
                          cl-cc::+op2-num-gt-imm2+ 0 1   5
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("cmp-imm-false"
           (make-bytecode cl-cc::+op2-const+ 1 2   nil
                          cl-cc::+op2-num-eq-imm2+ 0 1   5
                          cl-cc::+op2-halt2+ 0 nil nil)
           0))
  (bytecode expected)
  (let ((s (cl-cc::make-vm2-state)))
    (assert-= expected (cl-cc/vm::run-vm bytecode s))))

(deftest-each run-vm-generic-compare-ops
  "run-vm executes generic compare opcodes over register operands."
  :cases (("num-eq-true"
           (make-bytecode cl-cc::+op2-const+ 1 5 nil
                          cl-cc::+op2-const+ 2 5 nil
                          cl-cc::+op2-num-eq2+ 0 1 2
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("num-lt-true"
           (make-bytecode cl-cc::+op2-const+ 1 3 nil
                          cl-cc::+op2-const+ 2 9 nil
                          cl-cc::+op2-num-lt2+ 0 1 2
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("num-gt-false"
           (make-bytecode cl-cc::+op2-const+ 1 3 nil
                          cl-cc::+op2-const+ 2 9 nil
                          cl-cc::+op2-num-gt2+ 0 1 2
                          cl-cc::+op2-halt2+ 0 nil nil)
           0)
          ("num-le-true"
           (make-bytecode cl-cc::+op2-const+ 1 9 nil
                          cl-cc::+op2-const+ 2 9 nil
                          cl-cc::+op2-num-le2+ 0 1 2
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("num-ge-false"
           (make-bytecode cl-cc::+op2-const+ 1 3 nil
                          cl-cc::+op2-const+ 2 9 nil
                          cl-cc::+op2-num-ge2+ 0 1 2
                          cl-cc::+op2-halt2+ 0 nil nil)
           0)
          ("eq-true"
           (make-bytecode cl-cc::+op2-const+ 1 7 nil
                          cl-cc::+op2-move+  2 1 nil
                          cl-cc::+op2-eq+    0 1 2
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("eql-true"
           (make-bytecode cl-cc::+op2-const+ 1 7 nil
                          cl-cc::+op2-const+ 2 7 nil
                          cl-cc::+op2-eql+   0 1 2
                          cl-cc::+op2-halt2+ 0 nil nil)
           1)
          ("equal-true"
           (make-bytecode cl-cc::+op2-const+ 1 '(1 2) nil
                          cl-cc::+op2-const+ 2 '(1 2) nil
                          cl-cc::+op2-equal+ 0 1 2
                          cl-cc::+op2-halt2+ 0 nil nil)
           1))
  (bytecode expected)
  (let ((s (cl-cc::make-vm2-state)))
    (assert-= expected (cl-cc/vm::run-vm bytecode s))))
