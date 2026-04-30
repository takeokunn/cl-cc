;;;; tests/unit/type/inference-tests.lisp — Type Inference Registry + Predicate Tests
;;;
;;; Covers functions in src/type/inference.lisp:
;;; - Class/alias registries, type predicate mapping, type guard extraction
;;; - Union narrowing, constant effect table
;;; Inference forms (infer() on AST nodes) → inference-forms-tests.lisp

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Class Type Registry ──────────────────────────────────────────────────

(deftest infer-class-type-register-returns-slot-list
  "register-class-type + lookup-class-type returns the registered slot list."
  (let ((slots (list (cons 'x cl-cc/type:type-int)
                     (cons 'y cl-cc/type:type-string))))
    (cl-cc/type:register-class-type 'test-class-7891 slots)
    (let ((result (cl-cc/type:lookup-class-type 'test-class-7891)))
      (assert-true result)
      (assert-= 2 (length result)))))

(deftest infer-class-type-lookup-slot-finds-slot-type
  "lookup-slot-type finds the registered type for a known slot."
  (cl-cc/type:register-class-type 'test-class-7892
    (list (cons 'name cl-cc/type:type-string)
          (cons 'age cl-cc/type:type-int)))
  (let ((ty (cl-cc/type:lookup-slot-type 'test-class-7892 'age)))
    (assert-true (cl-cc/type:type-primitive-p ty))
    (assert-eq 'fixnum (cl-cc/type:type-primitive-name ty))))

(deftest infer-class-type-lookup-nonexistent-slot-returns-nil
  "lookup-slot-type returns nil for a slot not in the registered class."
  (cl-cc/type:register-class-type 'test-class-7893
    (list (cons 'x cl-cc/type:type-int)))
  (assert-null (cl-cc/type:lookup-slot-type 'test-class-7893 'nonexistent)))

(deftest infer-class-type-lookup-unknown-class-returns-nil
  "lookup-class-type returns nil for a class that was never registered."
  (assert-null (cl-cc/type:lookup-class-type 'completely-unknown-class-xyz)))

;;; ─── Type Alias Registry / Predicate Registry ────────────────────────────

(deftest infer-registry-alias-roundtrip
  "register-type-alias + lookup returns the registered form; unknown alias returns nil."
  (cl-cc/type:register-type-alias 'test-alias-7891 '(or fixnum string))
  (assert-equal '(or fixnum string) (cl-cc/type:lookup-type-alias 'test-alias-7891))
  (assert-null (cl-cc/type:lookup-type-alias 'no-such-alias-xyz)))

(deftest infer-registry-predicate-roundtrip
  "register-type-predicate + type-predicate-to-type returns the registered type; unknown returns nil."
  (cl-cc/type:register-type-predicate 'custom-pred-xyz-7891 cl-cc/type:type-int)
  (let ((ty (cl-cc/type::type-predicate-to-type 'custom-pred-xyz-7891)))
    (assert-true ty)
    (assert-eq 'fixnum (cl-cc/type:type-primitive-name ty)))
  (assert-null (cl-cc/type::type-predicate-to-type 'foobar-p)))

(deftest-each infer-global-tables-are-hash-tables
  "*type-predicate-table* and *constant-effect-table* are both hash tables."
  :cases (("predicate-table" cl-cc/type:*type-predicate-table*)
          ("effect-table"    cl-cc/type:*constant-effect-table*))
  (table)
  (assert-true (hash-table-p table)))

;;; ─── Constant Effect Table ────────────────────────────────────────────────

(deftest-each infer-constant-effect-table-entries
  "Constant effect table: ast-int→pure, ast-print→IO, ast-setq→STATE."
  :cases (("int-pure"   'cl-cc:ast-int   nil)
          ("print-io"   'cl-cc:ast-print "IO")
          ("setq-state" 'cl-cc:ast-setq  "STATE"))
  (ast-type expected-effect-name)
  (let ((row (gethash ast-type cl-cc/type:*constant-effect-table*)))
    (assert-true (cl-cc/type:type-effect-row-p row))
    (if expected-effect-name
        (let ((names (mapcar #'cl-cc/type:type-effect-op-name
                             (cl-cc/type:type-effect-row-effects row))))
          (assert-true (member expected-effect-name names :key #'symbol-name :test #'string=)))
        (assert-null (cl-cc/type:type-effect-row-effects row)))))

;;; ─── type-predicate-to-type ───────────────────────────────────────────────

(deftest-each infer-predicate-to-type-known
  "type-predicate-to-type maps known predicates to types."
  :cases (("numberp"    'numberp    'fixnum)
          ("integerp"   'integerp   'fixnum)
          ("stringp"    'stringp    'string)
          ("symbolp"    'symbolp    'symbol)
          ("consp"      'consp      'cons)
          ("null"       'null       'null)
          ("characterp" 'characterp 'character)
          ("functionp"  'functionp  'function))
  (pred expected-name)
  (let ((ty (cl-cc/type::type-predicate-to-type pred)))
    (assert-true ty)
    (assert-eq expected-name (cl-cc/type:type-primitive-name ty))))

;;; ─── extract-type-guard ───────────────────────────────────────────────────

(deftest-each infer-extract-type-guard-known-predicates
  "extract-type-guard extracts the variable and type from known predicate calls."
  :cases (("numberp" '(numberp x)        'x 'fixnum)
          ("typep"   '(typep x 'my-class) 'x 'my-class))
  (form expected-var expected-type-name)
  (let ((ast (cl-cc:lower-sexp-to-ast form)))
    (multiple-value-bind (var-name guard-type)
        (cl-cc/type::extract-type-guard ast)
      (assert-eq expected-var var-name)
      (assert-eq expected-type-name (cl-cc/type:type-primitive-name guard-type)))))

(deftest-each infer-extract-type-guard-returns-nil
  "extract-type-guard returns nil for non-predicate calls and non-call AST."
  :cases (("non-predicate-call" '(foo x))
          ("non-call-integer"   '42))
  (form)
  (let ((ast (cl-cc:lower-sexp-to-ast form)))
    (multiple-value-bind (v g) (cl-cc/type::extract-type-guard ast)
      (assert-null v)
      (assert-null g))))

;;; ─── narrow-union-type ────────────────────────────────────────────────────

(deftest-each infer-narrow-union-cases
  "narrow-union-type: removes member from union, collapses to single, passes through non-union."
  :cases (("removes-member"  (cl-cc/type:make-type-union
                               (list cl-cc/type:type-int cl-cc/type:type-string cl-cc/type:type-symbol))
                              cl-cc/type:type-int
                              (lambda (r) (and (cl-cc/type:type-union-p r)
                                               (= 2 (length (cl-cc/type:type-union-types r))))))
          ("collapses-to-single" (cl-cc/type:make-type-union
                                  (list cl-cc/type:type-int cl-cc/type:type-string))
                                 cl-cc/type:type-int
                                 (lambda (r) (and (cl-cc/type:type-primitive-p r)
                                                  (eq 'string (cl-cc/type:type-primitive-name r)))))
          ("non-union-passthrough" cl-cc/type:type-int
                                   cl-cc/type:type-string
                                   (lambda (r) (eq 'fixnum (cl-cc/type:type-primitive-name r)))))
  (union-type keep-type pred)
  (let ((result (cl-cc/type::narrow-union-type union-type keep-type)))
    (assert-true (funcall pred result))))
