;;;; tests/unit/vm/vm-dispatch-gf-tests.lisp
;;;; Unit tests for EQL specializer helpers in src/vm/vm-dispatch-gf.lisp.
;;;;
;;;; Covers: %eql-specializer-p, %eql-specializer-matches-p,
;;;;   %vm-extract-eql-specializer-keys.
;;;;
;;;; vm-classify-arg and vm-generic-function-p are already covered in
;;;; vm-dispatch-tests.lisp.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── %eql-specializer-p ──────────────────────────────────────────────────

(deftest-each eql-specializer-p-true-for-eql-forms
  "%eql-specializer-p is true for any (eql <value>) pair."
  :cases (("eql-integer"  '(eql 42))
          ("eql-symbol"   '(eql foo))
          ("eql-nil"      '(eql nil)))
  (form)
  (assert-true (cl-cc/vm::%eql-specializer-p form)))

(deftest-each eql-specializer-p-false-for-non-eql
  "%eql-specializer-p is false for plain symbols, integers, and non-eql lists."
  :cases (("symbol"       'integer)
          ("integer"      42)
          ("other-list"   '(and integer))
          ("nil"          nil))
  (form)
  (assert-false (cl-cc/vm::%eql-specializer-p form)))

;;; ─── %eql-specializer-matches-p ──────────────────────────────────────────

(deftest eql-specializer-matches-p-cases
  "%eql-specializer-matches-p: true for matching value/symbol; false for different value or non-eql form."
  (assert-true  (cl-cc/vm::%eql-specializer-matches-p '(eql 42) 42))
  (assert-true  (cl-cc/vm::%eql-specializer-matches-p '(eql foo) 'foo))
  (assert-false (cl-cc/vm::%eql-specializer-matches-p '(eql 42) 99))
  (assert-false (cl-cc/vm::%eql-specializer-matches-p 'integer 42)))

;;; ─── %vm-extract-eql-specializer-keys ───────────────────────────────────

(deftest vm-extract-eql-specializer-keys-cases
  "%vm-extract-eql-specializer-keys: (eql v)→(v); ((eql v))→(v); symbol→nil; multi-list→nil."
  (assert-equal '(42)  (cl-cc/vm::%vm-extract-eql-specializer-keys '(eql 42)))
  (assert-equal '(foo) (cl-cc/vm::%vm-extract-eql-specializer-keys '((eql foo))))
  (assert-null         (cl-cc/vm::%vm-extract-eql-specializer-keys 'integer))
  (assert-null         (cl-cc/vm::%vm-extract-eql-specializer-keys '(integer string))))

;;; ─── *method-combination-operators* data table ───────────────────────────

(deftest-each method-combination-operators-all-known-combinations
  "*method-combination-operators* contains all standard combination symbols."
  :cases (("plus"   '+)
          ("times"  '*)
          ("list"   'list)
          ("append" 'append)
          ("nconc"  'nconc)
          ("max"    'max)
          ("min"    'min)
          ("and"    'and)
          ("or"     'or)
          ("progn"  'progn))
  (combo)
  (assert-true (assoc combo cl-cc/vm::*method-combination-operators*)))

;;; ─── %resolve-combination-operator ──────────────────────────────────────

(deftest-each resolve-combination-operator-numeric-folds
  "%resolve-combination-operator returns correct operator for numeric combinations."
  :cases (("plus-folds-to-sum"    '+     '(1 2 3)  6)
          ("times-folds-to-product" '*   '(2 3 4)  24)
          ("max-picks-largest"    'max   '(1 7 3)  7)
          ("min-picks-smallest"   'min   '(5 2 9)  2))
  (combo args expected)
  (let ((op (cl-cc/vm::%resolve-combination-operator combo)))
    (assert-equal expected (apply op args))))

(deftest-each resolve-combination-operator-collection-folds
  "%resolve-combination-operator returns correct operator for collection combinations."
  :cases (("list-collects"   'list   '(1 2 3)     '(1 2 3))
          ("append-merges"   'append '((a b) (c))  '(a b c)))
  (combo args expected)
  (let ((op (cl-cc/vm::%resolve-combination-operator combo)))
    (assert-equal expected (apply op args))))

(deftest resolve-combination-operator-logic-cases
  "AND: truthy-all→t, nil-any→nil; OR: any-truthy→t, all-nil→nil; PROGN: returns last arg."
  (let ((and-op (cl-cc/vm::%resolve-combination-operator 'and)))
    (assert-true  (funcall and-op 1 2 3))
    (assert-false (funcall and-op 1 nil 3)))
  (let ((or-op (cl-cc/vm::%resolve-combination-operator 'or)))
    (assert-true  (funcall or-op nil 2 nil))
    (assert-false (funcall or-op nil nil nil)))
  (let ((progn-op (cl-cc/vm::%resolve-combination-operator 'progn)))
    (assert-equal 99 (funcall progn-op 1 2 99))))

(deftest resolve-combination-operator-unknown-signals-error
  "%resolve-combination-operator signals an error for unknown combinations."
  (assert-signals error
    (cl-cc/vm::%resolve-combination-operator 'unknown-combo)))
