;;;; packages/vm/tests/fr-668-669-754-757-tests.lisp

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(defun %fr-vm-unary (ctor value)
  (let ((state (make-test-vm)))
    (cl-cc:vm-reg-set state 1 value)
    (exec1 (funcall ctor :dst 0 :src 1) state)
    (values (cl-cc:vm-reg-get state 0)
            (cl-cc:vm-values-list state)
            state)))

(defun %fr-vm-binary (ctor lhs rhs)
  (let ((state (make-test-vm)))
    (cl-cc:vm-reg-set state 1 lhs)
    (cl-cc:vm-reg-set state 2 rhs)
    (exec1 (funcall ctor :dst 0 :lhs 1 :rhs 2) state)
    (values (cl-cc:vm-reg-get state 0)
            (cl-cc:vm-values-list state)
            state)))

(deftest fr-668-parse-float-returns-float-and-position
  "vm-parse-float parses decimal and exponent syntax and records the end position."
  (multiple-value-bind (value values-list)
      (%fr-vm-unary #'cl-cc:make-vm-parse-float " -12.5e1 rest")
    (assert-= -125.0d0 value)
    (assert-= -125.0d0 (first values-list))
    (assert-= 8 (second values-list))))

(deftest fr-668-parse-integer-records-position
  "vm-parse-integer keeps the existing primary value and stores the parse end position."
  (multiple-value-bind (value values-list)
      (%fr-vm-unary #'cl-cc:make-vm-parse-integer "12345")
    (assert-= 12345 value)
    (assert-equal '(12345 5) values-list)))

(deftest fr-669-float-decode-functions-match-cl-values
  "VM float decode instructions produce CL-compatible multiple values."
  (let ((float 6.5d0))
    (multiple-value-bind (expected-sig expected-exp expected-sign) (decode-float float)
      (multiple-value-bind (actual values-list)
          (%fr-vm-unary #'cl-cc:make-vm-decode-float float)
        (assert-= expected-sig actual)
        (assert-equal (list expected-sig expected-exp expected-sign) values-list)))
    (multiple-value-bind (expected-sig expected-exp expected-sign) (integer-decode-float float)
      (multiple-value-bind (actual values-list)
          (%fr-vm-unary #'cl-cc:make-vm-integer-decode-float float)
        (assert-= expected-sig actual)
        (assert-equal (list expected-sig expected-exp expected-sign) values-list)))))

(deftest-each fr-669-float-inspection-and-scale
  "VM float inspection helpers and scale-float execute without runtime wrappers."
  :cases (("radix" #'cl-cc:make-vm-float-radix 1.0d0 2)
          ("digits" #'cl-cc:make-vm-float-digits 1.0d0 53)
          ("sign" #'cl-cc:make-vm-float-sign -2.0d0 -1.0d0))
  (ctor input expected)
  (multiple-value-bind (actual) (%fr-vm-unary ctor input)
    (assert-= expected actual)))

(deftest fr-669-scale-float-scales-by-binary-exponent
  "vm-scale-float scales a float by powers of the IEEE binary radix."
  (multiple-value-bind (actual) (%fr-vm-binary #'cl-cc:make-vm-scale-float 1.5d0 3)
    (assert-= 12.0d0 actual)))

(deftest-each fr-754-coerce-runtime-cases
  "vm-coerce supports numeric, sequence, string, and character conversions."
  :cases (("integer-to-double" 3 'double-float 3.0d0)
          ("ratio-to-single" 1/2 'single-float 0.5f0)
          ("float-to-integer" 3.9d0 'integer 3)
          ("list-to-vector" '(a b) 'vector #(a b))
          ("vector-to-list" #(1 2 3) 'list '(1 2 3))
          ("char-to-string" #\x 'string "x")
          ("string-to-character" "Z" 'character #\Z))
  (value type expected)
  (multiple-value-bind (actual) (%fr-vm-binary #'cl-cc:make-vm-coerce value type)
    (if (vectorp expected)
        (assert-equal (coerce expected 'list) (coerce actual 'list))
        (assert-equal expected actual))))

(deftest fr-757-function-cell-management-round-trip
  "set-fdefinition, fboundp, fdefinition, and fmakunbound manage the VM function registry."
  (let ((state (make-test-vm))
        (fn #'identity))
    (cl-cc:vm-reg-set state 1 'fr-757-fn)
    (cl-cc:vm-reg-set state 2 fn)
    (exec1 (cl-cc:make-vm-set-fdefinition :dst 0 :lhs 1 :rhs 2) state)
    (assert-eq fn (cl-cc:vm-reg-get state 0))
    (exec1 (cl-cc:make-vm-fboundp :dst 3 :src 1) state)
    (assert-true (cl-cc:vm-reg-get state 3))
    (exec1 (cl-cc:make-vm-fdefinition :dst 4 :src 1) state)
    (assert-eq fn (cl-cc:vm-reg-get state 4))
    (exec1 (cl-cc:make-vm-fmakunbound :dst 5 :src 1) state)
    (assert-eq 'fr-757-fn (cl-cc:vm-reg-get state 5))
    (exec1 (cl-cc:make-vm-fboundp :dst 6 :src 1) state)
    (assert-false (cl-cc:vm-reg-get state 6))))
