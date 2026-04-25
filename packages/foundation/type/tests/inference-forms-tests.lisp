;;;; tests/unit/type/inference-forms-tests.lisp — Type Inference AST Form Tests
;;;
;;; Tests for infer() on concrete AST node types: ast-quote, ast-the, typed holes,
;;; ast-setq, ast-block, ast-defun/defvar, ast-function, lambda-param-env,
;;; ast-flet/ast-labels.
;;; Depends on helpers defined in inference-tests.lisp (same package, loads first).

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

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
          (assert-true (cl-cc/type:type-arrow-p ty))
          (assert-type-equal ty cl-cc/type:type-symbol)))))

;;; ─── infer: ast-function ──────────────────────────────────────────────────

(deftest-each infer-function-ref-cases
  "infer (function f): f in env → function type; f not in env → type-unknown."
  :cases (("known"   t)
          ("unknown" nil))
  (bound-p)
  (reset-type-vars!)
  (if bound-p
      (let* ((fn-ty (cl-cc/type:make-type-arrow-raw
                     :params (list cl-cc/type:type-int) :return cl-cc/type:type-int))
             (env (type-env-extend 'f (type-to-scheme fn-ty) (type-env-empty)))
             (ast (cl-cc:lower-sexp-to-ast '(function f))))
        (multiple-value-bind (ty subst) (cl-cc/type:infer ast env)
          (declare (ignore subst))
          (assert-true (cl-cc/type:type-arrow-p ty))))
      (let ((ast (cl-cc:lower-sexp-to-ast '(function unknown-fn-xyz))))
        (multiple-value-bind (ty subst) (infer-with-env ast)
          (declare (ignore subst))
          (assert-true (cl-cc/type::type-unknown-p ty))))))

;;; ─── %make-lambda-param-env ────────────────────────────────────────────────

(deftest infer-make-lambda-param-env-cases
  "%make-lambda-param-env: empty params → empty types, unextended env; two params → two type vars and extended env."
  (reset-type-vars!)
  (let ((empty-env (cl-cc/type:type-env-empty)))
    (multiple-value-bind (types body-env)
        (cl-cc/type::%make-lambda-param-env nil empty-env)
      (assert-null types)
      (assert-equal empty-env body-env)))
  (reset-type-vars!)
  (multiple-value-bind (types body-env)
      (cl-cc/type::%make-lambda-param-env '(x y) (cl-cc/type:type-env-empty))
    (assert-= 2 (length types))
    (assert-true (cl-cc/type::type-var-p (first types)))
    (assert-true (cl-cc/type::type-var-p (second types)))
    (assert-false (eq (first types) (second types)))
    (assert-true (cl-cc/type:type-env-p body-env))
    (multiple-value-bind (scheme-x found-x) (cl-cc/type:type-env-lookup 'x body-env)
      (assert-true found-x)
      (assert-null (cl-cc/type:type-scheme-quantified-vars scheme-x)))))

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
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name ty)))))
