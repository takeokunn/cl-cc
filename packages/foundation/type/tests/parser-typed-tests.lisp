;;;; tests/unit/type/parser-typed-tests.lisp — Type Parser Tests (Row / Typed Parameters)
;;;;
;;;; Continuation of parser-tests.lisp:
;;;; Row types (record/variant), type-app fallback, constraint spec parsing,
;;;; lambda-list parsing, typed parameters, typed AST nodes,
;;;; looks-like-type-specifier-p, make-type-function-from-spec.

(in-package :cl-cc/test)

(in-suite parser-suite)

;;; ─── Row types: Record / Variant ─────────────────────────────────────────

(deftest parse-record-closed
  "A closed record form produces a type-record with 2 fields and no row-var."
  (let ((ty (cl-cc/type::parse-type-specifier '(record (x fixnum) (y string)))))
    (assert-true (type-record-p ty))
    (assert-equal 2 (length (type-record-fields ty)))
    (assert-null (cl-cc/type::type-record-row-var ty))))

(deftest parse-record-open
  "An open record form produces a type-record with 1 field and a non-nil row-var."
  (let ((ty (cl-cc/type::parse-type-specifier
             `(record (x fixnum) ,(intern "|" :cl-cc/type) rho))))
    (assert-true (type-record-p ty))
    (assert-equal 1 (length (type-record-fields ty)))
    (assert-true (not (null (cl-cc/type::type-record-row-var ty))))))

(deftest parse-variant-form
  "A variant form produces a type-variant with 2 cases."
  (let ((ty (cl-cc/type::parse-type-specifier '(variant (some fixnum) (none null)))))
    (assert-true (type-variant-p ty))
    (assert-equal 2 (length (cl-cc/type::type-variant-cases ty)))))

;;; ─── Type application fallback ───────────────────────────────────────────

(deftest-each parse-type-app-cases
  "type-app fallback: single-arg resolves to app; multi-arg chains nested apps."
  :cases (("single-arg" '(maybe fixnum) nil)
          ("multi-arg"  '(f a b)        t))
  (form expect-nested)
  (let ((ty (cl-cc/type::parse-type-specifier form)))
    (assert-true (type-app-p ty))
    (if expect-nested
        (assert-true (type-app-p (cl-cc/type::type-app-fun ty)))
        (assert-true (type-primitive-p (cl-cc/type::type-app-fun ty))))))

;;; ─── Constraint spec parsing ─────────────────────────────────────────────

(deftest-each parse-constraint-spec-cases
  "parse-constraint-spec: list → canonical type-constraint; atom → type-parse-error."
  :cases (("basic" '(num fixnum) nil)
          ("error" 'num          t))
  (input expect-error)
  (if expect-error
      (assert-signals cl-cc/type::type-parse-error
        (cl-cc/type::parse-constraint-spec input))
      (let ((c (cl-cc/type::parse-constraint-spec input)))
        (assert-true (cl-cc/type::type-constraint-p c))
        (assert-eq 'num (cl-cc/type::type-constraint-class-name c)))))

;;; ─── Lambda list parsing ─────────────────────────────────────────────────

(deftest parse-lambda-list-typed
  "((x fixnum) (y string)) returns correct names and types."
  (multiple-value-bind (names types)
      (cl-cc/type::parse-lambda-list-with-types '((x fixnum) (y string)))
    (assert-equal '(x y) names)
    (assert-equal 2 (length types))
    (assert-true (type-equal-p type-int (first types)))
    (assert-true (type-equal-p type-string (second types)))))

(deftest parse-lambda-list-untyped
  "(x y) returns type-any for each parameter."
  (multiple-value-bind (names types)
      (cl-cc/type::parse-lambda-list-with-types '(x y))
    (assert-equal '(x y) names)
    (assert-true (type-equal-p type-any (first types)))
    (assert-true (type-equal-p type-any (second types)))))

(deftest parse-lambda-list-mixed
  "((x fixnum) y) mixes typed and untyped."
  (multiple-value-bind (names types)
      (cl-cc/type::parse-lambda-list-with-types '((x fixnum) y))
    (assert-equal '(x y) names)
    (assert-true (type-equal-p type-int (first types)))
    (assert-true (type-equal-p type-any (second types)))))

(deftest parse-lambda-list-empty
  "() returns empty lists."
  (multiple-value-bind (names types)
      (cl-cc/type::parse-lambda-list-with-types nil)
    (assert-null names)
    (assert-null types)))

(deftest parse-lambda-list-error
  "Invalid item signals error."
  (assert-signals cl-cc/type::type-parse-error
    (cl-cc/type::parse-lambda-list-with-types '(42))))

;;; ─── parse-typed-parameter ───────────────────────────────────────────────

(deftest-each parse-typed-parameter-cases
  "parse-typed-parameter: (x fixnum)→int; bare x→any."
  :cases (("typed" '(x fixnum) type-int)
          ("bare"  'x          type-any))
  (param expected-type)
  (let ((result (cl-cc/type::parse-typed-parameter param)))
    (assert-eq 'x (car result))
    (assert-true (type-equal-p expected-type (cdr result)))))

;;; ─── parse-typed-optional-parameter ──────────────────────────────────────

(deftest-each parse-optional-parameter-cases
  "parse-typed-optional-parameter: (x fixnum nil)→int; bare x→any."
  :cases (("typed" '(x fixnum nil) type-int)
          ("bare"  'x              type-any))
  (param expected-type)
  (let ((result (cl-cc/type::parse-typed-optional-parameter param)))
    (assert-eq 'x (car result))
    (assert-true (type-equal-p expected-type (cdr result)))))

;;; ─── extract-return-type ─────────────────────────────────────────────────

(deftest-each extract-return-type-cases
  "extract-return-type: body with return-type declare → type-int; without or nil body → nil."
  :cases (("with-declare" t   '((declare (return-type fixnum)) (+ x 1)))
          ("no-declare"   nil '((+ x 1)))
          ("nil-body"     nil nil))
  (expected body)
  (if expected
      (assert-true (type-equal-p type-int (cl-cc/type::extract-return-type body)))
      (assert-null (cl-cc/type::extract-return-type body))))

;;; ─── Typed AST nodes ─────────────────────────────────────────────────────

(deftest parse-typed-defun-basic
  "parse-typed-defun creates ast-defun-typed."
  (let ((node (cl-cc/type::parse-typed-defun '(defun foo ((x fixnum)) (+ x 1)))))
    (assert-true (cl-cc/type::ast-defun-typed-p node))
    (assert-eq 'foo (cl-cc/type::ast-defun-typed-name node))
    (assert-equal '(x) (cl-cc/type::ast-defun-typed-params node))
    (assert-equal 1 (length (cl-cc/type::ast-defun-typed-param-types node)))
    (assert-true (type-equal-p type-int (first (cl-cc/type::ast-defun-typed-param-types node))))))

(deftest parse-typed-lambda-basic
  "parse-typed-lambda creates ast-lambda-typed."
  (let ((node (cl-cc/type::parse-typed-lambda '(lambda ((x fixnum)) (+ x 1)))))
    (assert-true (cl-cc/type::ast-lambda-typed-p node))
    (assert-equal '(x) (cl-cc/type::ast-lambda-typed-params node))
    (assert-equal 1 (length (cl-cc/type::ast-lambda-typed-param-types node)))))

;;; ─── looks-like-type-specifier-p ─────────────────────────────────────────

(deftest-each looks-like-type-specifier-p-cases
  "looks-like-type-specifier-p: primitives, ?, compound forms → true; unknown → false."
  :cases (("fixnum"         t   'fixnum)
          ("string"         t   'string)
          ("boolean"        t   'boolean)
          ("question-mark"  t   '?)
          ("or-composite"   t   '(or fixnum string))
          ("function-type"  t   '(function (fixnum) string))
          ("values-type"    t   '(values fixnum string))
          ("unknown-symbol" nil 'my-random-thing))
  (expected form)
  (if expected
      (assert-true  (cl-cc/type::looks-like-type-specifier-p form))
      (assert-false (cl-cc/type::looks-like-type-specifier-p form))))

;;; ─── parse-type-specifier-maybe ──────────────────────────────────────────

(deftest-each parse-type-specifier-maybe-cases
  "parse-type-specifier-maybe: recognized type → node; unknown → nil."
  :cases (("known"   t   'fixnum)
          ("unknown" nil 'my-random-thing))
  (expected form)
  (if expected
      (assert-true (type-equal-p type-int (cl-cc/type::parse-type-specifier-maybe form)))
      (assert-null (cl-cc/type::parse-type-specifier-maybe form))))

;;; ─── make-type-function-from-spec ────────────────────────────────────────

(deftest make-type-function-from-spec-basic
  "Creates arrow type from param-types and return-type."
  (let ((ty (cl-cc/type::make-type-function-from-spec (list type-int) type-string)))
    (assert-true (type-arrow-p ty))
    (assert-equal 1 (length (type-arrow-params ty)))
    (assert-true (type-equal-p type-string (type-arrow-return ty)))))

;;; ─── Error on non-s-expression ───────────────────────────────────────────

(deftest parse-invalid-atom
  "Non-symbol non-nil atom signals error."
  (assert-signals cl-cc/type::type-parse-error
    (cl-cc/type::parse-type-specifier 42)))
