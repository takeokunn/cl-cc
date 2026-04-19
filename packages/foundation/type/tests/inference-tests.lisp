;;;; tests/unit/type/inference-tests.lisp — Type Inference Unit Tests
;;;
;;; Covers functions in src/type/inference.lisp not already tested in type-tests.lisp:
;;; - Class/alias registries, type predicate mapping, type guard extraction
;;; - Union narrowing, effect inference, effect signatures
;;; - Bidirectional check with forall/skolem, skolem escape detection
;;; - infer for: ast-the, ast-setq, ast-defun, ast-defvar, ast-function,
;;;   ast-flet, ast-labels, ast-block, ast-return-from, ast-defclass,
;;;   ast-make-instance, ast-slot-value, ast-quote (cons/string)

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Class Type Registry ──────────────────────────────────────────────────

(deftest infer-class-type-registry-cases
  "Class type registry: register+lookup returns slot list; lookup-slot finds slot type; unknowns return nil."
  (let ((slots (list (cons 'x cl-cc/type:type-int)
                     (cons 'y cl-cc/type:type-string))))
    (cl-cc/type:register-class-type 'test-class-7891 slots)
    (let ((result (cl-cc/type:lookup-class-type 'test-class-7891)))
      (assert-true result)
      (assert-= 2 (length result))))
  (cl-cc/type:register-class-type 'test-class-7892
    (list (cons 'name cl-cc/type:type-string)
          (cons 'age cl-cc/type:type-int)))
  (let ((ty (cl-cc/type:lookup-slot-type 'test-class-7892 'age)))
    (assert-true (cl-cc/type:type-primitive-p ty))
    (assert-eq 'fixnum (cl-cc/type:type-primitive-name ty)))
  (cl-cc/type:register-class-type 'test-class-7893
    (list (cons 'x cl-cc/type:type-int)))
  (assert-null (cl-cc/type:lookup-slot-type 'test-class-7893 'nonexistent))
  (assert-null (cl-cc/type:lookup-class-type 'completely-unknown-class-xyz)))

;;; ─── Type Alias Registry / Predicate Registry ────────────────────────────

(deftest infer-registry-alias-and-pred-cases
  "Alias registry roundtrip; custom predicate lookup; unknown predicate returns nil."
  (cl-cc/type:register-type-alias 'test-alias-7891 '(or fixnum string))
  (assert-equal '(or fixnum string) (cl-cc/type:lookup-type-alias 'test-alias-7891))
  (assert-null (cl-cc/type:lookup-type-alias 'no-such-alias-xyz))
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
        (let ((names (mapcar #'cl-cc/type:type-effect-name
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

;;; ─── infer: ast-quote edge cases ──────────────────────────────────────────

(deftest-each infer-quote-literal-types
  "Quoting a string infers type-string; quoting a cons infers type-cons."
  :cases (("string" '(quote "hello") cl-cc/type:type-string)
          ("cons"   '(quote (1 2 3)) cl-cc/type:type-cons))
  (sexp expected-type)
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty expected-type))))

;;; ─── infer: ast-the ───────────────────────────────────────────────────────

(deftest infer-the-cases
  "ast-the: matching type→fixnum; refinement base→fixnum; type mismatch signals error."
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(the fixnum 42))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name ty))))
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(the (refine fixnum plusp) 42))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (let ((prim (if (cl-cc/type::type-refinement-p ty)
                      (cl-cc/type::type-refinement-base ty)
                      ty)))
        (assert-eq 'fixnum (cl-cc/type:type-primitive-name prim)))))
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(the string 42))))
    (assert-signals cl-cc/type:type-mismatch-error
      (infer-with-env ast))))

(deftest infer-typed-hole-cases
  "Typed hole signals error; error message names in-scope variables."
  (reset-type-vars!)
  (let* ((env (type-env-extend 'x (type-to-scheme cl-cc/type:type-int) (type-env-empty)))
         (ast (cl-cc:lower-sexp-to-ast '(+ x _))))
    (assert-signals cl-cc/type::typed-hole-error
      (cl-cc/type:infer ast env)))
  (reset-type-vars!)
  (let* ((env (type-env-extend 'x (type-to-scheme cl-cc/type:type-int) (type-env-empty)))
         (ast (cl-cc:lower-sexp-to-ast '(+ x _))))
    (handler-case
        (cl-cc/type:infer ast env)
      (cl-cc/type::typed-hole-error (e)
        (let ((message (cl-cc/type:type-inference-error-message e)))
          (assert-true (search "Available: X ::" message)))))))

;;; ─── infer: ast-setq / ast-block / infer-with-constraints ───────────────

(deftest infer-setq-returns-fixnum
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(setq x 42))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name ty)))))

(deftest infer-block-returns-last-form
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(block b 1 2 3))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty cl-cc/type:type-int))))

(deftest infer-application-resolves-constraint
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '((lambda (x) x) 42))))
    (multiple-value-bind (ty subst residual)
        (cl-cc/type::infer-with-constraints ast (type-env-empty))
      (declare (ignore subst))
      (assert-null residual)
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name ty)))))

;;; ─── infer: ast-defun / ast-defvar ────────────────────────────────────────

(deftest-each infer-top-level-form-types
  "Top-level defun returns a function type; defvar returns type-symbol."
  :cases (("defun"         '(defun f (x) (+ x 1))        :function)
          ("defvar-init"   '(defvar *x* 42)             :symbol)
          ("defvar-no-val" '(defvar *x*)               :symbol))
  (sexp expected-kind)
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (if (eq expected-kind :function)
          (assert-true (cl-cc/type:type-function-p ty))
          (assert-type-equal ty cl-cc/type:type-symbol)))))

;;; ─── infer: ast-function ──────────────────────────────────────────────────

(deftest-each infer-function-ref-cases
  "infer (function f): f in env → function type; f not in env → type-unknown."
  :cases (("known"   t)
          ("unknown" nil))
  (bound-p)
  (reset-type-vars!)
  (if bound-p
      (let* ((fn-ty (cl-cc/type:make-type-function-raw
                     :params (list cl-cc/type:type-int) :return cl-cc/type:type-int))
             (env (type-env-extend 'f (type-to-scheme fn-ty) (type-env-empty)))
             (ast (cl-cc:lower-sexp-to-ast '(function f))))
        (multiple-value-bind (ty subst) (cl-cc/type:infer ast env)
          (declare (ignore subst))
          (assert-true (cl-cc/type:type-function-p ty))))
      (let ((ast (cl-cc:lower-sexp-to-ast '(function unknown-fn-xyz))))
        (multiple-value-bind (ty subst) (infer-with-env ast)
          (declare (ignore subst))
          (assert-true (cl-cc/type:type-unknown-p ty))))))

;;; ─── infer: ast-flet / ast-labels ─────────────────────────────────────────

(deftest-each infer-local-function-forms
  "flet and labels local function bindings are usable in their body scope."
  :cases (("flet"   '(flet   ((f (x) (+ x 1))) (f 5)))
          ("labels" '(labels ((f (x) (+ x 1))) (f 5))))
  (sexp)
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty cl-cc/type:type-int))))

