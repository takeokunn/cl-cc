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
(in-suite cl-cc-suite)

;;; ─── Class Type Registry ──────────────────────────────────────────────────

(deftest infer-register-class-type
  "register-class-type stores slot types, lookup-class-type retrieves them."
  (let ((slots (list (cons 'x cl-cc/type:type-int)
                     (cons 'y cl-cc/type:type-string))))
    (cl-cc/type:register-class-type 'test-class-7891 slots)
    (let ((result (cl-cc/type:lookup-class-type 'test-class-7891)))
      (assert-true result)
      (assert-= 2 (length result)))))

(deftest infer-lookup-slot-type
  "lookup-slot-type returns the type of a specific slot."
  (cl-cc/type:register-class-type 'test-class-7892
    (list (cons 'name cl-cc/type:type-string)
          (cons 'age cl-cc/type:type-int)))
  (let ((ty (cl-cc/type:lookup-slot-type 'test-class-7892 'age)))
    (assert-true (cl-cc/type:type-primitive-p ty))
    (assert-eq 'fixnum (cl-cc/type:type-primitive-name ty))))

(deftest infer-lookup-unknown-types-return-nil
  "lookup-slot-type and lookup-class-type return nil for unknown slot/class."
  (cl-cc/type:register-class-type 'test-class-7893
    (list (cons 'x cl-cc/type:type-int)))
  (assert-null (cl-cc/type:lookup-slot-type 'test-class-7893 'nonexistent))
  (assert-null (cl-cc/type:lookup-class-type 'completely-unknown-class-xyz)))

;;; ─── Type Alias Registry ──────────────────────────────────────────────────

(deftest infer-type-alias-registry
  "register-type-alias/lookup-type-alias roundtrip; unknown alias returns nil."
  (cl-cc/type:register-type-alias 'test-alias-7891 '(or fixnum string))
  (assert-equal '(or fixnum string) (cl-cc/type:lookup-type-alias 'test-alias-7891))
  (assert-null (cl-cc/type:lookup-type-alias 'no-such-alias-xyz)))

;;; ─── Type Predicate Table ─────────────────────────────────────────────────

(deftest infer-global-tables-are-hash-tables
  "*type-predicate-table* and *constant-effect-table* are both hash tables."
  (assert-true (hash-table-p cl-cc/type:*type-predicate-table*))
  (assert-true (hash-table-p cl-cc/type:*constant-effect-table*)))

(deftest infer-register-type-predicate-custom
  "register-type-predicate adds a custom predicate."
  (cl-cc/type:register-type-predicate 'custom-pred-xyz-7891 cl-cc/type:type-int)
  (let ((ty (cl-cc/type::type-predicate-to-type 'custom-pred-xyz-7891)))
    (assert-true ty)
    (assert-eq 'fixnum (cl-cc/type:type-primitive-name ty))))

;;; ─── Constant Effect Table ────────────────────────────────────────────────

(deftest infer-constant-effect-table-int-is-pure
  "ast-int maps to +pure-effect-row+ in the constant table."
  (let ((row (gethash 'cl-cc:ast-int cl-cc/type:*constant-effect-table*)))
    (assert-true (cl-cc/type:type-effect-row-p row))
    (assert-null (cl-cc/type:type-effect-row-effects row))))

(deftest infer-constant-effect-table-print-is-io
  "ast-print maps to +io-effect-row+ in the constant table."
  (let ((row (gethash 'cl-cc:ast-print cl-cc/type:*constant-effect-table*)))
    (assert-true (cl-cc/type:type-effect-row-p row))
    (let ((names (mapcar #'cl-cc/type:type-effect-name
                         (cl-cc/type:type-effect-row-effects row))))
      (assert-true (member "IO" names :key #'symbol-name :test #'string=)))))

(deftest infer-constant-effect-table-setq-has-state
  "ast-setq maps to a state effect row in the constant table."
  (let ((row (gethash 'cl-cc:ast-setq cl-cc/type:*constant-effect-table*)))
    (assert-true (cl-cc/type:type-effect-row-p row))
    (let ((names (mapcar #'cl-cc/type:type-effect-name
                         (cl-cc/type:type-effect-row-effects row))))
      (assert-true (member "STATE" names :key #'symbol-name :test #'string=)))))

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

(deftest infer-predicate-to-type-unknown
  "type-predicate-to-type returns nil for unknown predicate."
  (assert-null (cl-cc/type::type-predicate-to-type 'foobar-p)))

;;; ─── extract-type-guard ───────────────────────────────────────────────────

(deftest infer-extract-type-guard-numberp
  "extract-type-guard extracts (numberp x) → x, int."
  (let ((ast (cl-cc:lower-sexp-to-ast '(numberp x))))
    (multiple-value-bind (var-name guard-type)
        (cl-cc/type::extract-type-guard ast)
      (assert-eq 'x var-name)
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name guard-type)))))

(deftest infer-extract-type-guard-typep
  "extract-type-guard extracts (typep x 'my-class) → x, my-class."
  (let ((ast (cl-cc:lower-sexp-to-ast '(typep x 'my-class))))
    (multiple-value-bind (var-name guard-type)
        (cl-cc/type::extract-type-guard ast)
      (assert-eq 'x var-name)
      (assert-eq 'my-class (cl-cc/type:type-primitive-name guard-type)))))

(deftest infer-extract-type-guard-returns-nil
  "extract-type-guard returns nil for non-predicate calls and non-call AST."
  (let ((ast-call (cl-cc:lower-sexp-to-ast '(foo x)))
        (ast-int  (cl-cc:lower-sexp-to-ast '42)))
    (multiple-value-bind (v1 t1) (cl-cc/type::extract-type-guard ast-call)
      (assert-null v1)
      (assert-null t1))
    (multiple-value-bind (v2 t2) (cl-cc/type::extract-type-guard ast-int)
      (assert-null v2)
      (assert-null t2))))

;;; ─── narrow-union-type ────────────────────────────────────────────────────

(deftest infer-narrow-union-removes-member
  "narrow-union-type removes the matching member from a union."
  (let* ((union (cl-cc/type:make-type-union
                 (list cl-cc/type:type-int cl-cc/type:type-string cl-cc/type:type-symbol)))
         (result (cl-cc/type::narrow-union-type union cl-cc/type:type-int)))
    (assert-true (cl-cc/type:type-union-p result))
    (assert-= 2 (length (cl-cc/type:type-union-types result)))))

(deftest infer-narrow-union-to-single
  "narrow-union-type reduces to single type when two-member union."
  (let* ((union (cl-cc/type:make-type-union
                 (list cl-cc/type:type-int cl-cc/type:type-string)))
         (result (cl-cc/type::narrow-union-type union cl-cc/type:type-int)))
    (assert-true (cl-cc/type:type-primitive-p result))
    (assert-eq 'string (cl-cc/type:type-primitive-name result))))

(deftest infer-narrow-union-non-union-passthrough
  "narrow-union-type returns non-union type unchanged."
  (let ((result (cl-cc/type::narrow-union-type cl-cc/type:type-int cl-cc/type:type-string)))
    (assert-eq 'fixnum (cl-cc/type:type-primitive-name result))))

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

(deftest infer-the-matching-type
  "infer (the fixnum 42) succeeds and returns fixnum."
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(the fixnum 42))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name ty)))))

(deftest infer-the-refinement-type
  "infer (the (refine fixnum plusp) 42) succeeds through the refinement base type."
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(the (refine fixnum plusp) 42))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name ty)))))

(deftest infer-the-mismatch-error
  "infer (the string 42) signals type-mismatch-error."
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(the string 42))))
    (assert-signals cl-cc/type:type-mismatch-error
      (infer-with-env ast))))

;;; ─── infer: ast-setq ─────────────────────────────────────────────────────

(deftest infer-setq-returns-value-type
  "infer (setq x 42) returns fixnum regardless of whether x is bound in env."
  (reset-type-vars!)
  (let* ((ast (cl-cc:lower-sexp-to-ast '(setq x 42)))
         (env (type-env-extend 'x (type-to-scheme cl-cc/type:type-int)
                               (type-env-empty))))
    (multiple-value-bind (ty subst) (cl-cc/type:infer ast env)
      (declare (ignore subst))
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name ty))))
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(setq x 42))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
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

(deftest infer-function-ref-known
  "infer (function f) where f is in env returns its type."
  (reset-type-vars!)
  (let* ((fn-ty (cl-cc/type:make-type-function-raw
                 :params (list cl-cc/type:type-int) :return cl-cc/type:type-int))
         (env (type-env-extend 'f (type-to-scheme fn-ty) (type-env-empty)))
         (ast (cl-cc:lower-sexp-to-ast '(function f))))
    (multiple-value-bind (ty subst) (cl-cc/type:infer ast env)
      (declare (ignore subst))
      (assert-true (cl-cc/type:type-function-p ty)))))

(deftest infer-function-ref-unknown
  "infer (function f) where f is NOT in env returns unknown."
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(function unknown-fn-xyz))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-true (cl-cc/type:type-unknown-p ty)))))

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

;;; ─── infer-with-constraints ───────────────────────────────────────────────

(deftest infer-with-constraints-simple-application
  "infer-with-constraints resolves a simple application via constraints."
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '((lambda (x) x) 42))))
    (multiple-value-bind (ty subst residual)
        (cl-cc/type::infer-with-constraints ast (type-env-empty))
      (declare (ignore subst))
      (assert-null residual)
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name ty)))))

;;; ─── infer: ast-block / ast-return-from ───────────────────────────────────

(deftest infer-block-body-type
  "infer (block b 1 2 3) returns int (last form)."
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(block b 1 2 3))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty cl-cc/type:type-int))))

;;; ─── Effect Inference ─────────────────────────────────────────────────────

(deftest infer-effects-setq-has-state
  "infer-effects for setq returns {state}."
  (let* ((ast (cl-cc:lower-sexp-to-ast '(setq x 42)))
         (row (cl-cc/type:infer-effects ast (type-env-empty))))
    (assert-true (cl-cc/type:type-effect-row-p row))
    (let* ((effects (cl-cc/type:type-effect-row-effects row))
           (names (mapcar #'cl-cc/type:type-effect-name effects)))
      (assert-true (member "STATE" names :key #'symbol-name :test #'string=)))))

(deftest infer-effects-if-unions-branches
  "infer-effects for if unions condition+then+else effects."
  (let* ((ast (cl-cc:lower-sexp-to-ast '(if (print 1) (setq x 2) 3)))
         (row (cl-cc/type:infer-effects ast (type-env-empty))))
    (assert-true (cl-cc/type:type-effect-row-p row))
    (let* ((effects (cl-cc/type:type-effect-row-effects row))
           (names (mapcar #'cl-cc/type:type-effect-name effects)))
      ;; Should contain both io (from print) and state (from setq)
      (assert-true (member "IO" names :key #'symbol-name :test #'string=))
      (assert-true (member "STATE" names :key #'symbol-name :test #'string=)))))

(deftest infer-effects-progn-unions-all
  "infer-effects for progn unions all sub-form effects."
  (let* ((ast (cl-cc:lower-sexp-to-ast '(progn (print 1) (setq x 2))))
         (row (cl-cc/type:infer-effects ast (type-env-empty))))
    (let* ((effects (cl-cc/type:type-effect-row-effects row))
           (names (mapcar #'cl-cc/type:type-effect-name effects)))
      (assert-true (member "IO" names :key #'symbol-name :test #'string=))
      (assert-true (member "STATE" names :key #'symbol-name :test #'string=)))))

(deftest infer-effects-lambda-is-pure
  "infer-effects for lambda returns pure (no effects for the lambda itself)."
  (let* ((ast (cl-cc:lower-sexp-to-ast '(lambda (x) x)))
         (row (cl-cc/type:infer-effects ast (type-env-empty))))
    (assert-true (cl-cc/type:type-effect-row-p row))
    (assert-true (null (cl-cc/type:type-effect-row-effects row)))))

;;; ─── Effect Signature Registry ────────────────────────────────────────────

(deftest infer-effect-signature-io-ops
  "IO operations (print, format, etc.) have io effect."
  (let ((row (cl-cc/type:lookup-effect-signature 'print)))
    (assert-true (cl-cc/type:type-effect-row-p row))
    (let ((names (mapcar #'cl-cc/type:type-effect-name
                         (cl-cc/type:type-effect-row-effects row))))
      (assert-true (member "IO" names :key #'symbol-name :test #'string=)))))

(deftest infer-effect-signature-error-ops
  "Error operations have error effect."
  (let ((row (cl-cc/type:lookup-effect-signature 'error)))
    (let ((names (mapcar #'cl-cc/type:type-effect-name
                         (cl-cc/type:type-effect-row-effects row))))
      (assert-true (member "ERROR" names :key #'symbol-name :test #'string=)))))

(deftest infer-effect-signature-unknown-is-pure
  "Unknown operation has pure (empty) effect."
  (let ((row (cl-cc/type:lookup-effect-signature 'completely-unknown-fn-xyz)))
    (assert-true (null (cl-cc/type:type-effect-row-effects row)))))

(deftest infer-register-custom-effect-signature
  "register-effect-signature allows custom effect registration."
  (let ((custom-row (cl-cc/type:make-type-effect-row
                     :effects (list (cl-cc/type:make-type-effect :name 'network))
                     :row-var nil)))
    (cl-cc/type:register-effect-signature 'http-get-xyz custom-row)
    (let ((result (cl-cc/type:lookup-effect-signature 'http-get-xyz)))
      (assert-eq custom-row result))))

;;; ─── infer-with-effects ───────────────────────────────────────────────────

(deftest infer-with-effects-returns-triple
  "infer-with-effects returns type, substitution, and effect-row."
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(print 42))))
    (multiple-value-bind (ty subst effects)
        (cl-cc/type:infer-with-effects ast (type-env-empty))
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name ty))
      (assert-true (cl-cc/type:type-effect-row-p effects))
      (let ((names (mapcar #'cl-cc/type:type-effect-name
                           (cl-cc/type:type-effect-row-effects effects))))
        (assert-true (member "IO" names :key #'symbol-name :test #'string=))))))

;;; ─── check-body-effects ───────────────────────────────────────────────────

(deftest infer-check-body-effects-behavior
  "check-body-effects passes when declared effects cover actual; signals error when undeclared effects present."
  (let ((asts (list (cl-cc:lower-sexp-to-ast '(print 42)))))
    (cl-cc/type:check-body-effects asts cl-cc/type:+io-effect-row+ (type-env-empty))
    (assert-true t)
    (assert-signals cl-cc/type:type-inference-error
      (cl-cc/type:check-body-effects asts cl-cc/type:+pure-effect-row+ (type-env-empty)))))

;;; ─── Skolem Helpers ───────────────────────────────────────────────────────

(deftest infer-skolem-appears-in-type-p
  "skolem-appears-in-type-p detects skolem in type when present; nil when absent."
  (let* ((sk (cl-cc/type:make-type-skolem "a"))
         (fn-with (cl-cc/type:make-type-function-raw
                   :params (list sk) :return cl-cc/type:type-int))
         (fn-without (cl-cc/type:make-type-function-raw
                      :params (list cl-cc/type:type-int) :return cl-cc/type:type-int)))
    (assert-true  (cl-cc/type::skolem-appears-in-type-p sk fn-with))
    (assert-false (cl-cc/type::skolem-appears-in-type-p sk fn-without))))

(deftest infer-skolem-appears-in-subst-p
  "skolem-appears-in-subst-p detects skolem in substitution range when present; nil when absent."
  (let* ((sk (cl-cc/type:make-type-skolem "a"))
         (v  (cl-cc/type:make-type-variable 'x)))
    (let ((s-with (subst-extend v sk (make-substitution))))
      (assert-true (cl-cc/type::skolem-appears-in-subst-p sk s-with)))
    (let ((s-without (subst-extend v cl-cc/type:type-int (make-substitution))))
      (assert-false (cl-cc/type::skolem-appears-in-subst-p sk s-without)))))

(deftest infer-check-skolem-escape-signals
  "check-skolem-escape signals error when skolem leaks."
  (let* ((sk (cl-cc/type:make-type-skolem "a"))
         (v (cl-cc/type:make-type-variable 'x))
         (s (subst-extend v sk (make-substitution))))
    (assert-signals cl-cc/type:type-inference-error
      (cl-cc/type:check-skolem-escape sk s))))

;;; ─── Bidirectional check: forall ──────────────────────────────────────────

(deftest infer-check-forall-introduces-skolem
  "check against (forall a (a -> a)) skolemizes and checks body."
  (reset-type-vars!)
  (let* ((a (cl-cc/type:make-type-variable 'a))
         (fn-body (cl-cc/type:make-type-function-raw :params (list a) :return a))
         (forall-ty (cl-cc/type:make-type-forall :var a :body fn-body))
         (ast (cl-cc:lower-sexp-to-ast '(lambda (x) x)))
         (env (type-env-empty)))
    ;; Should succeed (identity matches forall a. a -> a)
    (let ((subst (cl-cc/type:check ast forall-ty env)))
      (declare (ignore subst))
      (assert-true t))))

(deftest infer-check-unknown-always-succeeds
  "check against type-unknown always succeeds (gradual typing)."
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '42))
        (env (type-env-empty)))
    (let ((subst (cl-cc/type:check ast cl-cc/type:+type-unknown+ env)))
      (assert-null subst))))

;;; ─── annotate-type ────────────────────────────────────────────────────────

(deftest infer-annotate-type-returns-type-and-ast
  "annotate-type returns inferred type and the original AST."
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '42)))
    (multiple-value-bind (ty result-ast)
        (cl-cc/type:annotate-type ast (type-env-empty))
      (assert-type-equal ty cl-cc/type:type-int)
      (assert-eq ast result-ast))))

;;; ─── Condition Classes ────────────────────────────────────────────────────

(deftest infer-type-inference-error-is-error
  "type-inference-error is a subtype of error."
  (let ((c (make-condition 'cl-cc/type:type-inference-error :message "test")))
    (assert-true (typep c 'error))
    (assert-equal "test" (cl-cc/type:type-inference-error-message c))))

(deftest infer-unbound-variable-error-has-name
  "unbound-variable-error carries the variable name."
  (let ((c (make-condition 'cl-cc/type:unbound-variable-error :name 'x)))
    (assert-true (typep c 'cl-cc/type:type-inference-error))
    (assert-eq 'x (cl-cc/type:unbound-variable-error-name c))))

(deftest infer-type-mismatch-error-has-fields
  "type-mismatch-error carries expected and actual."
  (let ((c (make-condition 'cl-cc/type:type-mismatch-error
                           :expected cl-cc/type:type-int
                           :actual cl-cc/type:type-string)))
    (assert-true (typep c 'cl-cc/type:type-inference-error))
    (assert-eq 'fixnum (cl-cc/type:type-primitive-name
                        (cl-cc/type:type-mismatch-error-expected c)))
    (assert-eq 'string (cl-cc/type:type-primitive-name
                        (cl-cc/type:type-mismatch-error-actual c)))))
