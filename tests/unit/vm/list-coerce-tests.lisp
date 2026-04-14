;;;; tests/unit/vm/list-coerce-tests.lisp — VM List Coercion Instruction Tests
;;;;
;;;; Tests for coercion instructions extracted from src/vm/list-coerce.lisp.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Coercion instructions ─────────────────────────────────────────────────

(deftest-each vm-list-coerce-simple
  "Simple coercion instructions round-trip a single value correctly."
  :cases (("chars-to-string" #'cl-cc::make-vm-coerce-to-string  '(#\h #\i)  "hi")
          ("vector-to-list"  #'cl-cc::make-vm-coerce-to-list    #(1 2 3)    '(1 2 3))
          ("symbol-to-name"  #'cl-cc::make-vm-string-coerce     'hello      "HELLO"))
  (ctor input expected)
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 input)
    (exec1 (funcall ctor :dst 0 :src 1) s)
    (assert-equal expected (cl-cc:vm-reg-get s 0))))

(deftest vm-list-coerce-to-vector
  "vm-coerce-to-vector coerces a list to a proper vector."
  (let ((s (make-test-vm)))
    (cl-cc:vm-reg-set s 1 '(a b c))
    (exec1 (cl-cc::make-vm-coerce-to-vector :dst 0 :src 1) s)
    (let ((v (cl-cc:vm-reg-get s 0)))
      (assert-true (vectorp v))
      (assert-= 3 (length v))
      (assert-eq 'a (aref v 0)))))
