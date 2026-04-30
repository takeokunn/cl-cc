;;;; tests/unit/type/solver-collect-tests.lisp — collect-constraints Tests
;;;;
;;;; Full coverage for packages/foundation/type/src/solver-collect.lisp.
;;;; Each typecase arm is exercised at least once, plus constraint emission
;;;; and the gradual-typing fallback.

(in-package :cl-cc/test)

(in-suite solver-suite)

;;; helpers
(defun empty-env ()
  (cl-cc/type::type-env-empty))

(defun collect (ast)
  (cl-cc/type::collect-constraints ast (empty-env)))

(defun collect-in (ast env)
  (cl-cc/type::collect-constraints ast env))

;;; ─── ast-int → type-int, no constraints ─────────────────────────────────────

(deftest collect-int-literal
  "ast-int node returns type-int with no constraints."
  (multiple-value-bind (ty cs)
      (collect (cl-cc/ast:make-ast-int :value 42))
    (assert-true (type-equal-p type-int ty))
    (assert-null cs)))

;;; ─── ast-var → instantiated scheme ──────────────────────────────────────────

(deftest collect-var-bound
  "ast-var bound in env returns the instantiated scheme type."
  (let* ((env (cl-cc/type::type-env-extend
               'x (cl-cc/type::type-to-scheme type-int) (empty-env))))
    (multiple-value-bind (ty cs)
        (collect-in (cl-cc/ast:make-ast-var :name 'x) env)
      (assert-true (type-equal-p type-int ty))
      (assert-null cs))))

(deftest collect-var-unbound-signals
  "ast-var not in env signals unbound-variable-error."
  (assert-signals cl-cc/type::unbound-variable-error
    (collect (cl-cc/ast:make-ast-var :name 'missing-var))))

;;; ─── ast-quote → literal type dispatch ──────────────────────────────────────

(deftest-each collect-quote-literal-types
  "ast-quote returns the correct type based on the quoted value's CL type."
  :cases (("integer" 42     'type-int)
          ("string"  "hi"   'type-string)
          ("symbol"  'foo   'type-symbol)
          ("cons"    '(1 2) 'type-cons))
  (val expected-var)
  (multiple-value-bind (ty cs)
      (collect (cl-cc/ast:make-ast-quote :value val))
    (assert-true (type-equal-p (symbol-value expected-var) ty))
    (assert-null cs)))

(deftest collect-quote-unknown
  "ast-quote with unrecognized value type returns cl-cc/type::+type-unknown+."
  (multiple-value-bind (ty cs)
      (collect (cl-cc/ast:make-ast-quote :value #\a))
    (assert-true (type-equal-p cl-cc/type::+type-unknown+ ty))
    (assert-null cs)))

;;; ─── ast-if → fresh result var + 2 equality constraints ─────────────────────

(deftest collect-if-emits-two-constraints
  "ast-if emits exactly 2 equality constraints binding result to then/else branches."
  (let* ((env (cl-cc/type::type-env-extend
               'b (cl-cc/type::type-to-scheme type-int) (empty-env))))
    (multiple-value-bind (ty cs)
        (collect-in
         (cl-cc/ast:make-ast-if
          :cond (cl-cc/ast:make-ast-var :name 'b)
          :then (cl-cc/ast:make-ast-int :value 1)
          :else (cl-cc/ast:make-ast-int :value 2))
         env)
      (assert-true (cl-cc/type::type-var-p ty))
      (assert-= 2 (length cs))
      (assert-true (every (lambda (c) (eq :equal (cl-cc/type::constraint-kind c))) cs)))))

;;; ─── ast-let → extends env, body type is result ─────────────────────────────

(deftest collect-let-behavior
  "ast-let returns body type; binding is available to body forms."
  (multiple-value-bind (ty _cs)
      (collect (cl-cc/ast:make-ast-let
                :bindings (list (cons 'n (cl-cc/ast:make-ast-int :value 5)))
                :body (list (cl-cc/ast:make-ast-int :value 99))))
    (declare (ignore _cs))
    (assert-true (type-equal-p type-int ty)))
  (multiple-value-bind (ty _cs)
      (collect (cl-cc/ast:make-ast-let
                :bindings (list (cons 'n (cl-cc/ast:make-ast-int :value 5)))
                :body (list (cl-cc/ast:make-ast-var :name 'n))))
    (declare (ignore _cs))
    (assert-true (type-equal-p type-int ty))))

;;; ─── ast-lambda → type-arrow ─────────────────────────────────────────────────

(deftest collect-lambda-no-params-yields-arrow-with-int-return
  "ast-lambda with no params yields arrow with nil params and int return."
  (multiple-value-bind (ty _cs)
      (collect
       (cl-cc/ast:make-ast-lambda
        :params '()
        :body   (list (cl-cc/ast:make-ast-int :value 1))))
    (declare (ignore _cs))
    (assert-true (cl-cc/type::type-arrow-p ty))
    (assert-null (cl-cc/type::type-arrow-params ty))
    (assert-true (type-equal-p type-int (cl-cc/type::type-arrow-return ty)))))

(deftest collect-lambda-one-param-yields-arrow-with-one-param
  "ast-lambda with 1 param yields arrow type with 1 param."
  (multiple-value-bind (ty _cs)
      (collect
       (cl-cc/ast:make-ast-lambda
        :params '(x)
        :body   (list (cl-cc/ast:make-ast-int :value 0))))
    (declare (ignore _cs))
    (assert-true (cl-cc/type::type-arrow-p ty))
    (assert-= 1 (length (cl-cc/type::type-arrow-params ty)))))

(deftest collect-lambda-empty-body-yields-null-return-type
  "ast-lambda with empty body yields arrow with null return type."
  (multiple-value-bind (ty _cs)
      (collect
       (cl-cc/ast:make-ast-lambda :params '() :body '()))
    (declare (ignore _cs))
    (assert-true (type-equal-p type-null (cl-cc/type::type-arrow-return ty)))))

;;; ─── ast-call → ret type-var + arrow constraint ──────────────────────────────

(deftest collect-call-emits-arrow-constraint
  "ast-call emits one equality constraint (fn-ty ~ (arg-tys → ret-ty))."
  (let* ((env (cl-cc/type::type-env-extend
               'f (cl-cc/type::type-to-scheme
                   (cl-cc/type::make-type-arrow (list type-int) type-string))
               (empty-env))))
    (multiple-value-bind (ty cs)
        (collect-in
         (cl-cc/ast:make-ast-call
          :func (cl-cc/ast:make-ast-var :name 'f)
          :args (list (cl-cc/ast:make-ast-int :value 1)))
         env)
      (assert-true (cl-cc/type::type-var-p ty))
      (assert-true (>= (length cs) 1))
      (assert-true (some (lambda (c) (eq :equal (cl-cc/type::constraint-kind c))) cs)))))

;;; ─── ast-progn → type of last form ──────────────────────────────────────────

(deftest collect-progn-behavior
  "ast-progn: empty body returns type-null; non-empty body returns type of last form."
  (multiple-value-bind (ty _cs)
      (collect (cl-cc/ast:make-ast-progn :forms '()))
    (declare (ignore _cs))
    (assert-true (type-equal-p type-null ty)))
  (multiple-value-bind (ty _cs)
      (collect (cl-cc/ast:make-ast-progn
                :forms (list (cl-cc/ast:make-ast-int :value 1)
                             (cl-cc/ast:make-ast-int :value 2))))
    (declare (ignore _cs))
    (assert-true (type-equal-p type-int ty))))

;;; ─── ast-defun → type-symbol ─────────────────────────────────────────────────

(deftest collect-defun-returns-symbol-type
  "ast-defun always returns type-symbol (its definition value)."
  (multiple-value-bind (ty _cs)
      (collect
       (cl-cc/ast:make-ast-defun
        :name 'my-fn
        :params '(x)
        :body (list (cl-cc/ast:make-ast-int :value 0))))
    (declare (ignore _cs))
    (assert-true (type-equal-p type-symbol ty))))

;;; ─── ast-defvar ──────────────────────────────────────────────────────────────

(deftest collect-defvar-returns-symbol-type
  "ast-defvar returns type-symbol with or without an initial value."
  (multiple-value-bind (ty _cs)
      (collect (cl-cc/ast:make-ast-defvar :name '*x* :value (cl-cc/ast:make-ast-int :value 0)))
    (declare (ignore _cs))
    (assert-true (type-equal-p type-symbol ty)))
  (multiple-value-bind (ty _cs)
      (collect (cl-cc/ast:make-ast-defvar :name '*x* :value nil))
    (declare (ignore _cs))
    (assert-true (type-equal-p type-symbol ty))))

;;; ─── ast-setq ────────────────────────────────────────────────────────────────

(deftest collect-setq-behavior
  "ast-setq: bound var emits equality constraint; unbound var emits no constraint."
  (let* ((env (cl-cc/type::type-env-extend 'n (cl-cc/type::type-to-scheme type-int) (empty-env))))
    (multiple-value-bind (ty cs)
        (collect-in (cl-cc/ast:make-ast-setq :var 'n :value (cl-cc/ast:make-ast-int :value 99)) env)
      (assert-true (type-equal-p type-int ty))
      (assert-= 1 (length cs))
      (assert-eq :equal (cl-cc/type::constraint-kind (first cs)))))
  (multiple-value-bind (ty cs)
      (collect (cl-cc/ast:make-ast-setq :var 'unbound :value (cl-cc/ast:make-ast-int :value 0)))
    (assert-true (type-equal-p type-int ty))
    (assert-null cs)))

;;; ─── gradual-typing fallback (t arm) ─────────────────────────────────────────

(deftest collect-unknown-node-returns-fresh-var
  "An unrecognized AST node returns a fresh type variable (gradual typing)."
  (multiple-value-bind (ty cs)
      (collect (cl-cc/ast:make-ast-apply
                :func (cl-cc/ast:make-ast-int :value 0)
                :args '()))
    (assert-true (cl-cc/type::type-var-p ty))
    (assert-null cs)))
