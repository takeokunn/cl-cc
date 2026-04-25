;;;; tests/unit/vm/vm-bitwise-tests.lisp — VM bitwise integer instruction tests

(in-package :cl-cc/test)

(defsuite vm-bitwise-suite
  :description "Unit tests for src/vm/vm-bitwise.lisp"
  :parent cl-cc-unit-suite)

(in-suite vm-bitwise-suite)

(defun %make-unary (ctor src)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 src)
    (exec1 (funcall ctor :dst 0 :src 1) s)
    (cl-cc:vm-reg-get s 0)))

(defun %make-binary (ctor lhs rhs)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 lhs)
    (cl-cc:vm-reg-set s 2 rhs)
    (exec1 (funcall ctor :dst 0 :lhs 1 :rhs 2) s)
    (cl-cc:vm-reg-get s 0)))

(defun %make-pred2 (ctor lhs rhs)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 lhs)
    (cl-cc:vm-reg-set s 2 rhs)
    (exec1 (funcall ctor :dst 0 :lhs 1 :rhs 2) s)
    (cl-cc:vm-reg-get s 0)))

(deftest-each vm-bitwise-binary
  "Bitwise binary operations compute correct results."
  :cases (("logand"    #'cl-cc::make-vm-logand  #xFF #x0F  #x0F)
          ("logior"    #'cl-cc::make-vm-logior  #xF0 #x0F  #xFF)
          ("logxor"    #'cl-cc::make-vm-logxor  #xFF #x0F  #xF0)
          ("logeqv"    #'cl-cc::make-vm-logeqv  #xFF #xFF  -1)
          ("ash-left"  #'cl-cc::make-vm-ash     1    4     16)
          ("ash-right" #'cl-cc::make-vm-ash     16   -2    4)
          ("expt-2^10" #'cl-cc::make-vm-expt    2    10    1024))
  (ctor lhs rhs expected)
  (assert-= expected (%make-binary ctor lhs rhs)))

(deftest-each vm-bitwise-unary
  "Bitwise unary operations compute correct results."
  :cases (("lognot-0"    #'cl-cc::make-vm-lognot         0          -1)
          ("bswap-1234"  #'cl-cc::make-vm-bswap          #x12345678 #x78563412)
          ("lognot-ff"   #'cl-cc::make-vm-lognot         #xFF       (lognot #xFF))
          ("logcount-7"  #'cl-cc::make-vm-logcount        7          3)
          ("logcount-0"  #'cl-cc::make-vm-logcount        0          0)
          ("intlen-0"    #'cl-cc::make-vm-integer-length  0          0)
          ("intlen-255"  #'cl-cc::make-vm-integer-length  255        8))
  (ctor src expected)
  (assert-= expected (%make-unary ctor src)))

(deftest-each vm-bitwise-pred
  "Bitwise predicate instructions return 1/0."
  :cases (("logtest-true"    #'cl-cc::make-vm-logtest   #xFF #x01  1)
          ("logtest-false"   #'cl-cc::make-vm-logtest   #xF0 #x0F  0)
          ("logbitp-set"     #'cl-cc::make-vm-logbitp   0    1      1)
          ("logbitp-clear"   #'cl-cc::make-vm-logbitp   1    1      0))
  (ctor lhs rhs expected)
  (assert-= expected (%make-pred2 ctor lhs rhs)))
