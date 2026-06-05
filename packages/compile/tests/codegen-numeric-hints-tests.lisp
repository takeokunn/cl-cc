;;;; packages/compile/tests/codegen-numeric-hints-tests.lisp
;;;; Unit tests for FR-860/FR-861 numeric compile-time helpers
;;;; (packages/compile/src/codegen-numeric-hints.lisp).

(in-package :cl-cc/test)

(in-suite compile-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; %codegen-normalize-contagion-type — alist lookup + complex special case
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each normalize-contagion-type-cases
  "%codegen-normalize-contagion-type maps all 9 *codegen-contagion-type-map* entries and nil for unknown."
  :cases (("fixnum"        'fixnum        'integer)
          ("bignum"        'bignum        'integer)
          ("integer"       'integer       'integer)
          ("ratio"         'ratio         'rational)
          ("rational"      'rational      'rational)
          ("single-float"  'single-float  'single-float)
          ("double-float"  'double-float  'double-float)
          ("float"         'float         'double-float)
          ("complex"       'complex       'complex)
          ("complex-cons"  '(complex integer) 'complex)
          ("unknown"       'unknown       nil))
  (input expected)
  (assert-equal expected
                (cl-cc/compile::%codegen-normalize-contagion-type input)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; codegen-infer-numeric-contagion-type — two-argument contagion
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each infer-numeric-contagion-type-cases
  "codegen-infer-numeric-contagion-type returns the result type for known operand pairs."
  :cases (("fixnum*fixnum"        'fixnum       'fixnum       'integer)
          ("fixnum*single-float"  'fixnum       'single-float 'single-float)
          ("double-float*complex" 'double-float 'complex      'complex))
  (left right expected)
  (assert-equal expected
                (cl-cc/compile::codegen-infer-numeric-contagion-type left right)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; codegen-inline-arith-dispatch-index — returns non-nil for valid types
;;; ─────────────────────────────────────────────────────────────────────────

(deftest codegen-inline-arith-dispatch-index-returns-non-nil
  "codegen-inline-arith-dispatch-index returns a non-nil integer index for fixnum+fixnum with op '+"
  (let ((index (cl-cc/compile::codegen-inline-arith-dispatch-index '+ 'fixnum 'fixnum)))
    (assert-true (integerp index))))
