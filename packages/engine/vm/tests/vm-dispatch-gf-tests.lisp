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

(deftest eql-specializer-matches-p-returns-true-for-exact-match
  "%eql-specializer-matches-p returns T when the argument equals the specializer value."
  (assert-true (cl-cc/vm::%eql-specializer-matches-p '(eql 42) 42))
  (assert-true (cl-cc/vm::%eql-specializer-matches-p '(eql foo) 'foo)))

(deftest eql-specializer-matches-p-returns-false-for-different-value
  "%eql-specializer-matches-p returns NIL when the argument differs from the specializer value."
  (assert-false (cl-cc/vm::%eql-specializer-matches-p '(eql 42) 99)))

(deftest eql-specializer-matches-p-returns-false-for-non-eql-form
  "%eql-specializer-matches-p returns NIL when given a plain symbol (not an eql form)."
  (assert-false (cl-cc/vm::%eql-specializer-matches-p 'integer 42)))

;;; ─── %vm-extract-eql-specializer-keys ───────────────────────────────────

(deftest vm-extract-eql-specializer-keys-bare-eql-form-returns-list
  "%vm-extract-eql-specializer-keys extracts the value from a bare (eql v) form."
  (assert-equal '(42) (cl-cc/vm::%vm-extract-eql-specializer-keys '(eql 42))))

(deftest vm-extract-eql-specializer-keys-nested-eql-form-returns-list
  "%vm-extract-eql-specializer-keys extracts the value from a nested ((eql v)) form."
  (assert-equal '(foo) (cl-cc/vm::%vm-extract-eql-specializer-keys '((eql foo)))))

(deftest vm-extract-eql-specializer-keys-symbol-returns-nil
  "%vm-extract-eql-specializer-keys returns nil for a plain class-name symbol."
  (assert-null (cl-cc/vm::%vm-extract-eql-specializer-keys 'integer)))

(deftest vm-extract-eql-specializer-keys-multi-specializer-list-returns-nil
  "%vm-extract-eql-specializer-keys returns nil for a multi-element specializer list."
  (assert-null (cl-cc/vm::%vm-extract-eql-specializer-keys '(integer string))))

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

(deftest resolve-combination-operator-and-short-circuits-on-nil
  "%resolve-combination-operator 'and returns T for all-truthy and NIL when any arg is nil."
  (let ((and-op (cl-cc/vm::%resolve-combination-operator 'and)))
    (assert-true  (funcall and-op 1 2 3))
    (assert-false (funcall and-op 1 nil 3))))

(deftest resolve-combination-operator-or-stops-at-first-truthy
  "%resolve-combination-operator 'or returns T for any-truthy and NIL for all-nil."
  (let ((or-op (cl-cc/vm::%resolve-combination-operator 'or)))
    (assert-true  (funcall or-op nil 2 nil))
    (assert-false (funcall or-op nil nil nil))))

(deftest resolve-combination-operator-progn-returns-last-value
  "%resolve-combination-operator 'progn returns the last argument."
  (let ((progn-op (cl-cc/vm::%resolve-combination-operator 'progn)))
    (assert-equal 99 (funcall progn-op 1 2 99))))

(deftest resolve-combination-operator-unknown-signals-error
  "%resolve-combination-operator signals an error for unknown combinations."
  (assert-signals error
    (cl-cc/vm::%resolve-combination-operator 'unknown-combo)))

;;; ─── %vm-dispatch-key-collect (extracted combination generator) ──────────

(deftest vm-dispatch-key-collect-single-cpl
  "%vm-dispatch-key-collect with one CPL returns one-element lists (reversed from prefix)."
  (let ((result (cl-cc/vm::%vm-dispatch-key-collect '((a b c)) nil)))
    (assert-equal 3 (length result))
    (assert-true (member '(a) result :test #'equal))
    (assert-true (member '(b) result :test #'equal))
    (assert-true (member '(c) result :test #'equal))))

(deftest vm-dispatch-key-collect-two-cpls
  "%vm-dispatch-key-collect with two CPLs returns all four combinations."
  (let ((result (cl-cc/vm::%vm-dispatch-key-collect '((x y) (1 2)) nil)))
    (assert-= 4 (length result))
    (assert-true (member '(x 1) result :test #'equal))
    (assert-true (member '(x 2) result :test #'equal))
    (assert-true (member '(y 1) result :test #'equal))
    (assert-true (member '(y 2) result :test #'equal))))

(deftest vm-dispatch-key-collect-empty-remaining
  "%vm-dispatch-key-collect with empty REMAINING returns a list containing the reversed prefix."
  (let ((result (cl-cc/vm::%vm-dispatch-key-collect nil '(b a))))
    (assert-equal 1 (length result))
    (assert-equal '(a b) (first result))))
