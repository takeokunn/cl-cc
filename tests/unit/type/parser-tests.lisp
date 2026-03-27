;;;; tests/unit/type/parser-tests.lisp — Type Parser Tests
;;;;
;;;; Tests for src/type/parser.lisp:
;;;; parse-type-specifier, parse-primitive-type, parse-compound-type,
;;;; arrow/row/constraint parsing, lambda-list parsing, return-type extraction,
;;;; typed AST nodes, looks-like-type-specifier-p.

(in-package :cl-cc/test)

(defsuite parser-suite :description "Type annotation parser tests")

;;; ─── parse-type-specifier: atoms ─────────────────────────────────────────

(deftest parse-nil-to-null
  "nil parses to type-null."
  (assert-true (type-equal-p type-null (cl-cc/type::parse-type-specifier nil))))

(deftest parse-question-mark
  "? parses to a type-error node (gradual hole)."
  (let ((ty (cl-cc/type::parse-type-specifier '?)))
    (assert-true (cl-cc/type::type-error-p ty))))

(deftest-each parse-primitive-symbols
  "Primitive type symbols parse to their expected type nodes."
  :cases (("fixnum"    'fixnum    type-int)
          ("integer"   'integer   type-int)
          ("string"    'string    type-string)
          ("boolean"   'boolean   type-bool)
          ("bool"      'bool      type-bool)
          ("symbol"    'symbol    type-symbol)
          ("character" 'character type-char)
          ("char"      'char      type-char)
          ("t"         't         type-any)
          ("top"       'top       type-any)
          ("cons"      'cons      type-cons))
  (sym expected)
  (assert-true (type-equal-p expected (cl-cc/type::parse-type-specifier sym))))

(deftest parse-unknown-symbol
  "Unknown symbol becomes type-primitive with that name."
  (let ((ty (cl-cc/type::parse-type-specifier 'my-custom-type)))
    (assert-true (type-primitive-p ty))
    (assert-eq 'my-custom-type (type-primitive-name ty))))

;;; ─── parse-type-specifier: union / intersection ──────────────────────────

(deftest parse-or-union
  "(or fixnum string) produces a type-union."
  (let ((ty (cl-cc/type::parse-type-specifier '(or fixnum string))))
    (assert-true (type-union-p ty))
    (assert-equal 2 (length (type-union-types ty)))))

(deftest-each parse-boolean-ops-empty-error
  "(or) and (and) with no arguments each signal type-parse-error."
  :cases (("or"  '(or))
          ("and" '(and)))
  (form)
  (assert-signals cl-cc/type::type-parse-error
    (cl-cc/type::parse-type-specifier form)))

(deftest parse-and-intersection
  "(and fixnum string) produces a type-intersection."
  (let ((ty (cl-cc/type::parse-type-specifier '(and fixnum string))))
    (assert-true (type-intersection-p ty))
    (assert-equal 2 (length (type-intersection-types ty)))))

;;; ─── parse-type-specifier: function / values / cons ──────────────────────

(deftest parse-function-type
  "(function (fixnum) string) produces arrow."
  (let ((ty (cl-cc/type::parse-type-specifier '(function (fixnum) string))))
    (assert-true (type-arrow-p ty))
    (assert-equal 1 (length (type-arrow-params ty)))
    (assert-true (type-equal-p type-string (type-arrow-return ty)))))

(deftest parse-function-type-multi-params
  "(function (fixnum string) boolean) has 2 params."
  (let ((ty (cl-cc/type::parse-type-specifier '(function (fixnum string) boolean))))
    (assert-true (type-arrow-p ty))
    (assert-equal 2 (length (type-arrow-params ty)))))

(deftest parse-function-type-error
  "(function fixnum) signals error (missing param list)."
  (assert-signals cl-cc/type::type-parse-error
    (cl-cc/type::parse-type-specifier '(function fixnum))))

(deftest parse-values-product
  "(values fixnum string) produces product."
  (let ((ty (cl-cc/type::parse-type-specifier '(values fixnum string))))
    (assert-true (type-product-p ty))
    (assert-equal 2 (length (type-product-elems ty)))))

(deftest parse-cons-compound
  "(cons fixnum string) produces 2-element product."
  (let ((ty (cl-cc/type::parse-type-specifier '(cons fixnum string))))
    (assert-true (type-product-p ty))
    (assert-equal 2 (length (type-product-elems ty)))))

(deftest parse-cons-error-wrong-arity
  "(cons fixnum) signals error."
  (assert-signals cl-cc/type::type-parse-error
    (cl-cc/type::parse-type-specifier '(cons fixnum))))

;;; ─── parse-type-specifier: list / vector / array ─────────────────────────

(deftest parse-list-type-app
  "(list fixnum) produces type-app."
  (let ((ty (cl-cc/type::parse-type-specifier '(list fixnum))))
    (assert-true (type-app-p ty))
    (assert-true (type-equal-p type-int (type-app-arg ty)))))

(deftest parse-list-error-wrong-arity
  "(list fixnum string) signals error."
  (assert-signals cl-cc/type::type-parse-error
    (cl-cc/type::parse-type-specifier '(list fixnum string))))

(deftest-each parse-collection-type-apps
  "(vector T) and (array T) both produce type-app nodes."
  :cases (("vector" '(vector fixnum))
          ("array"  '(array string)))
  (form)
  (assert-true (type-app-p (cl-cc/type::parse-type-specifier form))))

;;; ─── Arrow types: ->, ->1, ->0 ──────────────────────────────────────────

(deftest parse-arrow-pure
  "(-> fixnum string) produces pure arrow."
  (let ((ty (cl-cc/type::parse-type-specifier `(,(intern "->" :cl-cc/type) fixnum string))))
    (assert-true (type-arrow-p ty))
    (assert-true (type-equal-p type-int (first (type-arrow-params ty))))
    (assert-true (type-equal-p type-string (type-arrow-return ty)))
    (assert-true (type-effect-row-p (type-arrow-effects ty)))))

(deftest parse-arrow-multi-param
  "(-> fixnum string boolean) has 2 params, returns boolean."
  (let ((ty (cl-cc/type::parse-type-specifier `(,(intern "->" :cl-cc/type) fixnum string boolean))))
    (assert-equal 2 (length (type-arrow-params ty)))
    (assert-true (type-equal-p type-bool (type-arrow-return ty)))))

(deftest-each parse-arrow-multiplicity
  "->1 and ->0 produce arrow types with :one and :zero multiplicity respectively."
  :cases (("linear" (intern "->1" :cl-cc/type) :one)
          ("erased" (intern "->0" :cl-cc/type) :zero))
  (arrow-sym expected-mult)
  (let ((ty (cl-cc/type::parse-type-specifier `(,arrow-sym fixnum string))))
    (assert-true (type-arrow-p ty))
    (assert-eq expected-mult (cl-cc/type::type-arrow-mult ty))))

(deftest parse-arrow-error-too-few
  "(-> fixnum) with only one arg signals error."
  (assert-signals cl-cc/type::type-parse-error
    (cl-cc/type::parse-type-specifier `(,(intern "->" :cl-cc/type) fixnum))))

(deftest parse-arrow-with-bang-effects
  "(-> fixnum string ! IO) has IO effect."
  (let ((ty (cl-cc/type::parse-type-specifier
             `(,(intern "->" :cl-cc/type) fixnum string ,(intern "!" :cl-cc/type) io))))
    (assert-true (type-arrow-p ty))
    (let ((eff (type-arrow-effects ty)))
      (assert-true (type-effect-row-p eff))
      (assert-true (> (length (type-effect-row-effects eff)) 0)))))

;;; ─── Quantifiers: forall, exists, mu ─────────────────────────────────────

(deftest parse-forall
  "(forall a fixnum) produces type-forall."
  (let ((ty (cl-cc/type::parse-type-specifier '(forall a fixnum))))
    (assert-true (type-forall-p ty))
    (assert-true (type-var-p (type-forall-var ty)))
    (assert-true (type-equal-p type-int (cl-cc/type::type-forall-body ty)))))

(deftest-each parse-quantifier-arity-errors
  "forall, exists, and mu with only one argument each signal type-parse-error."
  :cases (("forall" '(forall a))
          ("exists" '(exists a))
          ("mu"     '(mu a)))
  (form)
  (assert-signals cl-cc/type::type-parse-error
    (cl-cc/type::parse-type-specifier form)))

(deftest parse-exists
  "(exists a fixnum) produces type-exists."
  (let ((ty (cl-cc/type::parse-type-specifier '(exists a fixnum))))
    (assert-true (type-exists-p ty))
    (assert-true (type-var-p (cl-cc/type::type-exists-var ty)))))

(deftest parse-mu-recursive
  "(mu a fixnum) produces type-mu."
  (let ((ty (cl-cc/type::parse-type-specifier '(mu a fixnum))))
    (assert-true (type-mu-p ty))
    (assert-true (type-var-p (cl-cc/type::type-mu-var ty)))))

;;; ─── Qualified types: => ─────────────────────────────────────────────────

(deftest parse-qualified-type
  "(=> (Num fixnum) string) produces type-qualified."
  (let ((ty (cl-cc/type::parse-type-specifier
             `(,(intern "=>" :cl-cc/type) (num fixnum) string))))
    (assert-true (type-qualified-p ty))
    (assert-equal 1 (length (type-qualified-constraints ty)))
    (assert-true (type-equal-p type-string (cl-cc/type::type-qualified-body ty)))))

(deftest parse-qualified-error-no-body
  "(=>) signals error."
  (assert-signals cl-cc/type::type-parse-error
    (cl-cc/type::parse-type-specifier `(,(intern "=>" :cl-cc/type)))))

;;; ─── Refinement types ───────────────────────────────────────────────────

(deftest parse-refinement
  "(Refine fixnum positive-p) produces type-refinement."
  (let ((ty (cl-cc/type::parse-type-specifier '(refine fixnum positive-p))))
    (assert-true (cl-cc/type::type-refinement-p ty))
    (assert-true (type-equal-p type-int (cl-cc/type::type-refinement-base ty)))
    (assert-eq 'positive-p (cl-cc/type::type-refinement-predicate ty))))

(deftest parse-refinement-error
  "(Refine fixnum) signals error."
  (assert-signals cl-cc/type::type-parse-error
    (cl-cc/type::parse-type-specifier '(refine fixnum))))

;;; ─── Graded modal types ─────────────────────────────────────────────────

(deftest-each parse-graded-modal-types
  "!1, !0, and !W produce linear types with :one, :zero, :omega grades."
  :cases (("one"   (intern "!1" :cl-cc/type) :one)
          ("zero"  (intern "!0" :cl-cc/type) :zero)
          ("omega" (intern "!W" :cl-cc/type) :omega))
  (bang-sym expected-grade)
  (let ((ty (cl-cc/type::parse-type-specifier `(,bang-sym fixnum))))
    (assert-true (type-linear-p ty))
    (assert-eq expected-grade (cl-cc/type::type-linear-grade ty))))

(deftest parse-graded-bang-explicit
  "(! 1 fixnum) produces linear type with grade :one."
  (let ((ty (cl-cc/type::parse-type-specifier `(,(intern "!" :cl-cc/type) 1 fixnum))))
    (assert-true (type-linear-p ty))
    (assert-eq :one (cl-cc/type::type-linear-grade ty))))

;;; ─── Row types: Record / Variant ─────────────────────────────────────────

(deftest parse-record-closed
  "(Record (x fixnum) (y string)) produces closed record."
  (let ((ty (cl-cc/type::parse-type-specifier '(record (x fixnum) (y string)))))
    (assert-true (type-record-p ty))
    (assert-equal 2 (length (type-record-fields ty)))
    (assert-null (cl-cc/type::type-record-row-var ty))))

(deftest parse-record-open
  "(Record (x fixnum) | rho) produces open record with row var."
  (let ((ty (cl-cc/type::parse-type-specifier
             `(record (x fixnum) ,(intern "|" :cl-cc/type) rho))))
    (assert-true (type-record-p ty))
    (assert-equal 1 (length (type-record-fields ty)))
    (assert-true (not (null (cl-cc/type::type-record-row-var ty))))))

(deftest parse-variant-closed
  "(Variant (some fixnum) (none null)) produces variant."
  (let ((ty (cl-cc/type::parse-type-specifier '(variant (some fixnum) (none null)))))
    (assert-true (type-variant-p ty))
    (assert-equal 2 (length (cl-cc/type::type-variant-cases ty)))))

(deftest parse-row-field-error
  "Malformed field (x) signals error."
  (assert-signals cl-cc/type::type-parse-error
    (cl-cc/type::parse-type-specifier '(record (x)))))

;;; ─── Type application fallback ───────────────────────────────────────────

(deftest parse-type-app-fallback
  "(Maybe fixnum) falls through to type-app."
  (let ((ty (cl-cc/type::parse-type-specifier '(maybe fixnum))))
    (assert-true (type-app-p ty))
    (assert-true (type-primitive-p (cl-cc/type::type-app-fun ty)))
    (assert-true (type-equal-p type-int (type-app-arg ty)))))

(deftest parse-type-app-multi-arg
  "(F a b) chains into nested type-app."
  (let ((ty (cl-cc/type::parse-type-specifier '(f a b))))
    (assert-true (type-app-p ty))
    ;; Outer is (F a) applied to b
    (assert-true (type-app-p (cl-cc/type::type-app-fun ty)))))

;;; ─── Constraint spec parsing ─────────────────────────────────────────────

(deftest parse-constraint-spec-basic
  "(Num fixnum) produces type-class-constraint."
  (let ((c (cl-cc/type::parse-constraint-spec '(num fixnum))))
    (assert-true (cl-cc/type::type-class-constraint-p c))
    (assert-eq 'num (cl-cc/type::type-class-constraint-class-name c))))

(deftest parse-constraint-spec-error
  "Atom signals error."
  (assert-signals cl-cc/type::type-parse-error
    (cl-cc/type::parse-constraint-spec 'num)))

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

(deftest parse-typed-parameter-cases
  "parse-typed-parameter: (x fixnum)→int; bare x→any."
  (let ((result (cl-cc/type::parse-typed-parameter '(x fixnum))))
    (assert-eq 'x (car result))
    (assert-true (type-equal-p type-int (cdr result))))
  (let ((result (cl-cc/type::parse-typed-parameter 'x)))
    (assert-eq 'x (car result))
    (assert-true (type-equal-p type-any (cdr result)))))

;;; ─── parse-typed-optional-parameter ──────────────────────────────────────

(deftest parse-optional-parameter-cases
  "parse-typed-optional-parameter: (x fixnum nil)→int; bare x→any."
  (let ((result (cl-cc/type::parse-typed-optional-parameter '(x fixnum nil))))
    (assert-eq 'x (car result))
    (assert-true (type-equal-p type-int (cdr result))))
  (let ((result (cl-cc/type::parse-typed-optional-parameter 'x)))
    (assert-eq 'x (car result))
    (assert-true (type-equal-p type-any (cdr result)))))

;;; ─── extract-return-type ─────────────────────────────────────────────────

(deftest extract-return-type-present
  "Body with (declare (return-type fixnum)) extracts type-int."
  (let ((body '((declare (return-type fixnum)) (+ x 1))))
    (let ((ty (cl-cc/type::extract-return-type body)))
      (assert-true (type-equal-p type-int ty)))))

(deftest extract-return-type-nil-cases
  "Body without return-type declare, and nil body, both return nil."
  (assert-null (cl-cc/type::extract-return-type '((+ x 1))))
  (assert-null (cl-cc/type::extract-return-type nil)))

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

(deftest looks-like-type-specifier-p-behavior
  "looks-like-type-specifier-p: recognized primitives/?, compound forms → true; unknown symbol → false."
  (assert-true  (cl-cc/type::looks-like-type-specifier-p 'fixnum))
  (assert-true  (cl-cc/type::looks-like-type-specifier-p 'string))
  (assert-true  (cl-cc/type::looks-like-type-specifier-p 'boolean))
  (assert-true  (cl-cc/type::looks-like-type-specifier-p '?))
  (assert-true  (cl-cc/type::looks-like-type-specifier-p '(or fixnum string)))
  (assert-true  (cl-cc/type::looks-like-type-specifier-p '(function (fixnum) string)))
  (assert-true  (cl-cc/type::looks-like-type-specifier-p '(values fixnum string)))
  (assert-false (cl-cc/type::looks-like-type-specifier-p 'my-random-thing)))

;;; ─── parse-type-specifier-maybe ──────────────────────────────────────────

(deftest parse-type-specifier-maybe-behavior
  "parse-type-specifier-maybe: recognized type→node; unknown symbol→nil."
  (assert-true (type-equal-p type-int (cl-cc/type::parse-type-specifier-maybe 'fixnum)))
  (assert-null (cl-cc/type::parse-type-specifier-maybe 'my-random-thing)))

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
