;;;; tests/unit/vm/primitives-typep-tests.lisp — VM vm-typep Tests
;;;
;;; Tests for vm-typep, vm-typep-check, and *vm-compound-type-handlers*.
;;; Depends on %run-unary-inst-with defined in primitives-tests.lisp.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(defun %run-vm-typep (value type-name)
  "Run vm-typep against VALUE and TYPE-NAME, returning the destination register."
  (%run-unary-inst-with
   (lambda (src)
     (cl-cc::make-vm-typep :dst 0 :src src :type-name type-name))
   value))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 1: vm-typep General Predicate (primitive and compound)
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each prim-typep
  "vm-typep checks various type names including compound type specifiers."
  :cases (;; Primitive types
          ("integer"         42         'integer              1)
          ("string"          "hello"    'string               1)
          ("symbol"          'foo       'symbol               1)
          ("cons"            '(a)       'cons                 1)
          ("null"            nil        'null                 1)
          ("list-cons"       '(a)       'list                 1)
          ("list-nil"        nil        'list                 1)
          ("char"            #\a        'character            1)
          ("atom-num"        42         'atom                 1)
          ("int-wrong"       "hello"    'integer              0)
          ;; Compound: refine (base + predicate). 'refine' is cl-cc-specific; other
          ;; compound heads (or/and/not/member/eql/values/function/satisfies) are
          ;; CL symbols imported into :cl-cc/test, so they just work unqualified.
          ("refine-true"     42         '(cl-cc::refine fixnum plusp) 1)
          ("refine-false"    -1         '(cl-cc::refine fixnum plusp) 0)
          ;; Compound: or
          ("or-first"        42         '(or integer string)  1)
          ("or-second"       "hi"       '(or integer string)  1)
          ("or-none"         'x         '(or integer string)  0)
          ;; Compound: and
          ("and-both"        42         '(and integer number) 1)
          ("and-fail"        42         '(and integer string) 0)
          ;; Compound: not
          ("not-true"        42         '(not string)         1)
          ("not-false"       "hi"       '(not string)         0)
          ;; Compound: member
          ("member-hit"      2          '(member 1 2 3)       1)
          ("member-miss"     5          '(member 1 2 3)       0)
          ;; Compound: eql
          ("eql-hit"         42         '(eql 42)             1)
          ("eql-miss"        99         '(eql 42)             0)
          ;; Compound: values (always t)
          ("values-any"      42         '(values)             1)
          ;; Compound: function
          ("function-fn"     #'car      '(function)           1)
          ("function-int"    42         '(function)           0)
          ;; Compound: satisfies
          ("satisfies-true"  4          '(satisfies evenp)    1)
          ("satisfies-false" 3          '(satisfies evenp)    0))
  (src type-sym expected)
  (assert-= expected (%run-vm-typep src type-sym)))

(deftest prim-typep-structural-refinement-object
  "vm-typep accepts structural cl-cc/type refinement objects as TYPE-NAME inputs."
  (let ((refined-int (cl-cc/type:make-type-refinement :base type-int :predicate #'plusp)))
    (assert-= 1 (%run-vm-typep 42 refined-int))
    (assert-= 0 (%run-vm-typep -1 refined-int))))

;;; ═══════════════════════════════════════════════════════════════════════════
;;; Section 2: vm-typep-check and *vm-primitive-type-predicates* table
;;; ═══════════════════════════════════════════════════════════════════════════

(deftest-each prim-typep-check-primitive-table
  "vm-typep-check correctly dispatches every entry in *vm-primitive-type-predicates*."
  :cases (("integer"        42         'integer        t)
          ("string"         "hi"       'string         t)
          ("symbol"         'x         'symbol         t)
          ("keyword"        :k         'keyword        t)
          ("cons"           '(a)       'cons           t)
          ("null"           nil        'null           t)
          ("list"           '(1 2)     'list           t)
          ("number"         3.14       'number         t)
          ("character"      #\a        'character      t)
          ("function"       #'car      'function       t)
          ("vector"         #(1 2)     'vector         t)
          ("array"          #(1 2)     'array          t)
          ("integer-wrong"  "hello"    'integer        nil)
          ("string-wrong"   42         'string         nil))
  (value type-sym expected)
  (assert-equal expected (cl-cc/vm::vm-typep-check value type-sym)))

(deftest prim-typep-normalize-sym-roundtrips-cl-symbols
  "vm-typep-normalize-sym maps CL symbols through themselves."
  (assert-eq 'cl:integer (cl-cc/vm::%vm-typep-normalize-sym 'integer))
  (assert-eq 'cl:string  (cl-cc/vm::%vm-typep-normalize-sym 'string))
  (assert-eq 'cl:symbol  (cl-cc/vm::%vm-typep-normalize-sym 'symbol)))

(deftest prim-compound-type-handlers-table-has-expected-keys
  "*vm-compound-type-handlers* contains all standard compound head symbols."
  (let ((ht cl-cc/vm::*vm-compound-type-handlers*))
    (assert-true (gethash 'or        ht))
    (assert-true (gethash 'and       ht))
    (assert-true (gethash 'not       ht))
    (assert-true (gethash 'member    ht))
    (assert-true (gethash 'eql       ht))
    (assert-true (gethash 'satisfies ht))
    (assert-true (gethash 'values    ht))
    (assert-true (gethash 'function  ht))))
