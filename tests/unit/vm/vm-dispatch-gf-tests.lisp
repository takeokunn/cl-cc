;;;; tests/unit/vm/vm-dispatch-gf-tests.lisp
;;;; Unit tests for EQL specializer helpers in src/vm/vm-dispatch-gf.lisp.
;;;;
;;;; Covers: %eql-specializer-p, %eql-specializer-matches-p,
;;;;   %vm-extract-eql-specializer-keys.
;;;;
;;;; vm-classify-arg and vm-generic-function-p are already covered in
;;;; vm-dispatch-tests.lisp.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

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
