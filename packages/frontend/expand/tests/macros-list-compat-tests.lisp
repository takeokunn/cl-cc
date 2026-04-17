;;;; tests/unit/expand/macros-list-compat-tests.lisp
;;;; Coverage tests for src/expand/macros-list-compat.lisp

(in-package :cl-cc/test)

(defsuite macros-list-compat-suite
  :description "Tests for macros-list-compat.lisp"
  :parent cl-cc-unit-suite)

(in-suite macros-list-compat-suite)

;;; ── SUBST-IF / SUBST-IF-NOT ────────────────────────────────────────────────

(deftest-each macros-list-compat-let-body-structure
  "subst-if, member-if, and maphash each expand to a LET whose body begins with a specific inner operator."
  :cases (("subst-if"  '(subst-if new pred tree)  'labels)
          ("member-if" '(member-if pred lst)       'do)
          ("maphash"   '(maphash fn table)         'dolist))
  (form inner-op)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq 'let (car result))
    (assert-eq inner-op (car (caddr result)))))

(deftest-each macros-list-compat-complement-delegation
  "subst-if-not and member-if-not delegate to their non-not counterparts via COMPLEMENT.
For member-if-not the (complement pred) cons sits at position 2 (cadr), not
cadar — caadr would extract the CAR of that cons (the symbol 'complement) and
then error when (car 'complement) is applied."
  :cases (("subst-if-not"  '(subst-if-not new pred tree) 'subst-if  #'caddr)
          ("member-if-not" '(member-if-not pred lst)     'member-if #'cadr))
  (form outer-op complement-extractor)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq outer-op (car result))
    (assert-eq 'complement (car (funcall complement-extractor result)))))

;;; ── VECTOR ──────────────────────────────────────────────────────────────────

(deftest vector-expansion
  "VECTOR expands to MAKE-ARRAY or a LET around MAKE-ARRAY."
  (let ((empty-result (our-macroexpand-1 '(vector))))
    (assert-equal empty-result '(make-array 0)))
  (let ((result (our-macroexpand-1 '(vector a b c))))
    (assert-eq 'let (car result))
    (assert-eq 'make-array (car (cadr (car (second result)))))))

;;; ── MEMBER-IF / MEMBER-IF-NOT ───────────────────────────────────────────────

;;; ── MAPHASH ─────────────────────────────────────────────────────────────────
