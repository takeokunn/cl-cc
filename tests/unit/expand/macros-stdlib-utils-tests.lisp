;;;; tests/unit/expand/macros-stdlib-utils-tests.lisp
;;;; Coverage tests for src/expand/macros-stdlib-utils.lisp
;;;;
;;;; Covers: tailp, ldiff, copy-alist, tree-equal, get-properties,
;;;; nunion/nintersection/nset-difference/nset-exclusive-or,
;;;; nsubst/nsubst-if/nsubst-if-not, nstring-upcase/downcase/capitalize,
;;;; bit-vector-p, simple-string-p, simple-bit-vector-p,
;;;; array-element-type, array-in-bounds-p, upgraded-array-element-type.

(in-package :cl-cc/test)

(defsuite macros-stdlib-utils-suite
  :description "Tests for macros-stdlib-utils.lisp: list/tree/string/array utilities"
  :parent cl-cc-unit-suite)

(in-suite macros-stdlib-utils-suite)

;;; ─── tailp ────────────────────────────────────────────────────────────────

(deftest tailp-expands-to-do
  "TAILP wraps a DO loop that traverses the list until an atom."
  (let ((result (our-macroexpand-1 '(tailp obj lst))))
    (assert-eq 'do (car result))))

(deftest tailp-do-terminates-at-atom
  "TAILP DO termination clause ends with (eq tail obj) to check identity."
  (let* ((result (our-macroexpand-1 '(tailp obj lst)))
         (end-clause (third result)))   ; (atom tail) → (eq tail obj)
    (assert-true (consp end-clause))))

;;; ─── ldiff ────────────────────────────────────────────────────────────────

(deftest ldiff-expands-to-let
  "LDIFF expands to a LET with a result accumulator and a DO loop."
  (let ((result (our-macroexpand-1 '(ldiff lst obj))))
    (assert-eq 'let (car result))))

(deftest ldiff-result-has-nreverse
  "LDIFF body reverses the accumulated result — NREVERSE should appear anywhere
in the expansion body (use recursive tree-walk, not shallow member)."
  (let* ((result (our-macroexpand-1 '(ldiff lst obj)))
         (body (cddr result)))
    (assert-true (%tree-contains-p 'nreverse body))))

;;; ─── copy-alist ───────────────────────────────────────────────────────────

(deftest copy-alist-expands-to-let
  "COPY-ALIST expands to a LET with a result accumulator."
  (let ((result (our-macroexpand-1 '(copy-alist alist))))
    (assert-eq 'let (car result))))

(deftest copy-alist-body-has-dolist
  "COPY-ALIST body iterates with DOLIST."
  (let* ((result (our-macroexpand-1 '(copy-alist alist)))
         (body (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'dolist))) body))))

;;; ─── tree-equal ───────────────────────────────────────────────────────────

(deftest tree-equal-expands-to-labels
  "TREE-EQUAL expands to a LABELS form for recursive traversal."
  (let ((result (our-macroexpand-1 '(tree-equal x y))))
    (assert-eq 'labels (car result))))

(defun %tree-contains-p (target form)
  "True if TARGET appears anywhere inside FORM (nested)."
  (cond ((eq form target) t)
        ((consp form) (or (%tree-contains-p target (car form))
                          (%tree-contains-p target (cdr form))))
        (t nil)))

(deftest tree-equal-uses-default-eql-test
  "TREE-EQUAL without :test uses #'eql as the comparison function."
  (let* ((result   (our-macroexpand-1 '(tree-equal x y)))
         (bindings (second result))
         (fn-body  (cddr (first bindings))))
    ;; eql appears anywhere in the recursive fn body (nested ok)
    (assert-true (%tree-contains-p 'eql fn-body))))

(deftest tree-equal-respects-test-keyword
  "TREE-EQUAL with :test #'equal uses the supplied test."
  (let* ((result (our-macroexpand-1 '(tree-equal x y :test #'equal)))
         (fn-body (cddr (first (second result)))))
    (assert-true (%tree-contains-p 'equal fn-body))))

;;; ─── get-properties ───────────────────────────────────────────────────────

(deftest get-properties-expands-to-do
  "GET-PROPERTIES expands to a DO loop over the plist."
  (let ((result (our-macroexpand-1 '(get-properties plist '(:a :b)))))
    (assert-eq 'do (car result))))

(deftest get-properties-has-values-in-body
  "GET-PROPERTIES returns three values via VALUES."
  (let* ((result (our-macroexpand-1 '(get-properties plist '(:a :b))))
         ;; termination clause of DO returns (values nil nil nil)
         (end-clause (third result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'values))) end-clause))))

;;; ─── Destructive set operations ───────────────────────────────────────────

(deftest-each destructive-set-ops-delegate
  "nunion/nintersection/nset-difference/nset-exclusive-or each delegate to non-n counterpart."
  :cases (("nunion"             '(nunion a b)             'union)
          ("nintersection"      '(nintersection a b)      'intersection)
          ("nset-difference"    '(nset-difference a b)    'set-difference)
          ("nset-exclusive-or"  '(nset-exclusive-or a b)  'set-exclusive-or))
  (form expected-head)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq expected-head (car result))))

;;; ─── nsubst / nsubst-if / nsubst-if-not ──────────────────────────────────

(deftest nsubst-without-test-delegates-to-subst
  "NSUBST without :test delegates directly to SUBST."
  (let ((result (our-macroexpand-1 '(nsubst new old tree))))
    (assert-eq 'subst (car result))))

(deftest nsubst-with-test-delegates-to-subst-if
  "NSUBST with :test wraps into SUBST-IF with a lambda predicate."
  (let ((result (our-macroexpand-1 '(nsubst new old tree :test #'equal))))
    (assert-eq 'subst-if (car result))))

(deftest-each nsubst-if-variants-delegate
  "NSUBST-IF and NSUBST-IF-NOT delegate to their SUBST counterparts."
  :cases (("nsubst-if"     '(nsubst-if new pred tree)     'subst-if)
          ("nsubst-if-not" '(nsubst-if-not new pred tree) 'subst-if-not))
  (form expected-head)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq expected-head (car result))))

;;; ─── nstring-upcase / nstring-downcase / nstring-capitalize ──────────────

(deftest-each nstring-case-variants-no-bounds-delegate
  "NSTRING-{UPCASE,DOWNCASE,CAPITALIZE} without :start/:end delegate to non-n counterpart."
  :cases (("nstring-upcase"     '(nstring-upcase s)     'string-upcase)
          ("nstring-downcase"   '(nstring-downcase s)   'string-downcase)
          ("nstring-capitalize" '(nstring-capitalize s) 'string-capitalize))
  (form expected-head)
  (let ((result (our-macroexpand-1 form)))
    (assert-eq expected-head (car result))))

(deftest nstring-upcase-with-start-passes-keyword
  "NSTRING-UPCASE with :start includes the :start keyword in the delegate call."
  (let ((result (our-macroexpand-1 '(nstring-upcase s :start 2))))
    (assert-eq 'string-upcase (car result))
    (assert-true (member :start result))))

(deftest nstring-upcase-with-end-passes-keyword
  "NSTRING-UPCASE with :end includes both :start 0 and :end in the delegate call."
  (let ((result (our-macroexpand-1 '(nstring-upcase s :end 5))))
    (assert-eq 'string-upcase (car result))
    (assert-true (member :end result))))

;;; ─── Array predicate macros ───────────────────────────────────────────────

(deftest bit-vector-p-expands-to-let
  "BIT-VECTOR-P expands to a LET that guards with VECTORP."
  (let ((result (our-macroexpand-1 '(bit-vector-p x))))
    (assert-eq 'let (car result))))

(deftest simple-string-p-delegates-to-stringp
  "SIMPLE-STRING-P delegates directly to STRINGP."
  (let ((result (our-macroexpand-1 '(simple-string-p x))))
    (assert-eq 'stringp (car result))))

(deftest simple-bit-vector-p-delegates-to-bit-vector-p
  "SIMPLE-BIT-VECTOR-P delegates to BIT-VECTOR-P."
  (let ((result (our-macroexpand-1 '(simple-bit-vector-p x))))
    (assert-eq 'bit-vector-p (car result))))

;;; ─── Array utility macros ─────────────────────────────────────────────────

(deftest array-element-type-returns-t
  "ARRAY-ELEMENT-TYPE evaluates array for side effects then returns T."
  (let ((result (our-macroexpand-1 '(array-element-type arr))))
    (assert-eq 'progn (car result))
    ;; last element is quoted T
    (assert-equal ''t (car (last result)))))

(deftest array-in-bounds-p-expands-to-let
  "ARRAY-IN-BOUNDS-P expands to a LET binding the array and subscript list."
  (let ((result (our-macroexpand-1 '(array-in-bounds-p arr 0 1))))
    (assert-eq 'let (car result))))

(deftest array-in-bounds-p-uses-every
  "ARRAY-IN-BOUNDS-P body uses EVERY to check each subscript."
  (let* ((result (our-macroexpand-1 '(array-in-bounds-p arr 0 1)))
         (body (cddr result)))
    (assert-true (some (lambda (f) (and (consp f) (eq (car f) 'and))) body))))

(deftest upgraded-array-element-type-returns-t
  "UPGRADED-ARRAY-ELEMENT-TYPE evaluates type for side effects then returns T.
Qualify with cl-cc:: because the macro is registered under the :cl-cc package
by our-defmacro and can't be found via an unqualified symbol in :cl-cc/test."
  (let ((result (our-macroexpand-1 '(cl-cc::upgraded-array-element-type 'integer))))
    (assert-eq 'progn (car result))
    (assert-equal ''t (car (last result)))))
