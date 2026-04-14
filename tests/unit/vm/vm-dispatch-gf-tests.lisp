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
  (assert-true (cl-cc::%eql-specializer-p form)))

(deftest-each eql-specializer-p-false-for-non-eql
  "%eql-specializer-p is false for plain symbols, integers, and non-eql lists."
  :cases (("symbol"       'integer)
          ("integer"      42)
          ("other-list"   '(and integer))
          ("nil"          nil))
  (form)
  (assert-false (cl-cc::%eql-specializer-p form)))

;;; ─── %eql-specializer-matches-p ──────────────────────────────────────────

(deftest eql-specializer-matches-p-true-for-eql-value
  "%eql-specializer-matches-p is true when the arg is eql to the specializer value."
  (assert-true (cl-cc::%eql-specializer-matches-p '(eql 42) 42)))

(deftest eql-specializer-matches-p-true-for-symbol
  "%eql-specializer-matches-p is true for a matching symbol."
  (assert-true (cl-cc::%eql-specializer-matches-p '(eql foo) 'foo)))

(deftest eql-specializer-matches-p-false-for-different-value
  "%eql-specializer-matches-p is false when arg differs from the specializer value."
  (assert-false (cl-cc::%eql-specializer-matches-p '(eql 42) 99)))

(deftest eql-specializer-matches-p-false-for-non-eql-form
  "%eql-specializer-matches-p is false when given a non-eql specializer."
  (assert-false (cl-cc::%eql-specializer-matches-p 'integer 42)))

;;; ─── %vm-extract-eql-specializer-keys ───────────────────────────────────

(deftest vm-extract-eql-specializer-keys-direct-eql
  "%vm-extract-eql-specializer-keys returns (value) for a direct (eql value) form."
  (assert-equal '(42) (cl-cc::%vm-extract-eql-specializer-keys '(eql 42))))

(deftest vm-extract-eql-specializer-keys-wrapped-single
  "%vm-extract-eql-specializer-keys returns (value) for a ((eql value)) list."
  (assert-equal '(foo) (cl-cc::%vm-extract-eql-specializer-keys '((eql foo)))))

(deftest vm-extract-eql-specializer-keys-nil-for-plain-symbol
  "%vm-extract-eql-specializer-keys returns nil for a plain class symbol."
  (assert-null (cl-cc::%vm-extract-eql-specializer-keys 'integer)))

(deftest vm-extract-eql-specializer-keys-nil-for-multi-element-list
  "%vm-extract-eql-specializer-keys returns nil for a multi-element specializer list."
  (assert-null (cl-cc::%vm-extract-eql-specializer-keys '(integer string))))

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
  (assert-true (assoc combo cl-cc::*method-combination-operators*)))

;;; ─── %resolve-combination-operator ──────────────────────────────────────

(deftest-each resolve-combination-operator-numeric-folds
  "%resolve-combination-operator returns correct operator for numeric combinations."
  :cases (("plus-folds-to-sum"    '+     '(1 2 3)  6)
          ("times-folds-to-product" '*   '(2 3 4)  24)
          ("max-picks-largest"    'max   '(1 7 3)  7)
          ("min-picks-smallest"   'min   '(5 2 9)  2))
  (combo args expected)
  (let ((op (cl-cc::%resolve-combination-operator combo)))
    (assert-equal expected (apply op args))))

(deftest-each resolve-combination-operator-collection-folds
  "%resolve-combination-operator returns correct operator for collection combinations."
  :cases (("list-collects"   'list   '(1 2 3)     '(1 2 3))
          ("append-merges"   'append '((a b) (c))  '(a b c)))
  (combo args expected)
  (let ((op (cl-cc::%resolve-combination-operator combo)))
    (assert-equal expected (apply op args))))

(deftest resolve-combination-operator-and-returns-t-for-truthy
  "%resolve-combination-operator for AND returns true when all args are truthy."
  (let ((op (cl-cc::%resolve-combination-operator 'and)))
    (assert-true (funcall op 1 2 3))))

(deftest resolve-combination-operator-and-returns-nil-for-falsy
  "%resolve-combination-operator for AND returns nil when any arg is nil."
  (let ((op (cl-cc::%resolve-combination-operator 'and)))
    (assert-false (funcall op 1 nil 3))))

(deftest resolve-combination-operator-or-returns-t-for-any-truthy
  "%resolve-combination-operator for OR returns true when any arg is truthy."
  (let ((op (cl-cc::%resolve-combination-operator 'or)))
    (assert-true (funcall op nil 2 nil))))

(deftest resolve-combination-operator-or-returns-nil-for-all-falsy
  "%resolve-combination-operator for OR returns nil when all args are nil."
  (let ((op (cl-cc::%resolve-combination-operator 'or)))
    (assert-false (funcall op nil nil nil))))

(deftest resolve-combination-operator-progn-returns-last
  "%resolve-combination-operator for PROGN returns the last argument."
  (let ((op (cl-cc::%resolve-combination-operator 'progn)))
    (assert-equal 99 (funcall op 1 2 99))))

(deftest resolve-combination-operator-unknown-signals-error
  "%resolve-combination-operator signals an error for unknown combinations."
  (assert-signals error
    (cl-cc::%resolve-combination-operator 'unknown-combo)))
