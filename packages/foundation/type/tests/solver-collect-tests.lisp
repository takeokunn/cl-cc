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
  "ast-quote with unrecognized value type returns +type-unknown+."
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

(deftest collect-let-body-type
  "ast-let returns the type of the last body form."
  (multiple-value-bind (ty _cs)
      (collect
       (cl-cc/ast:make-ast-let
        :bindings (list (cons 'n (cl-cc/ast:make-ast-int :value 5)))
        :body (list (cl-cc/ast:make-ast-int :value 99))))
    (declare (ignore _cs))
    (assert-true (type-equal-p type-int ty))))

(deftest collect-let-binding-used-in-body
  "ast-let makes the binding available to body forms."
  (multiple-value-bind (ty _cs)
      (collect
       (cl-cc/ast:make-ast-let
        :bindings (list (cons 'n (cl-cc/ast:make-ast-int :value 5)))
        :body (list (cl-cc/ast:make-ast-var :name 'n))))
    (declare (ignore _cs))
    (assert-true (type-equal-p type-int ty))))

;;; ─── ast-lambda → type-arrow ─────────────────────────────────────────────────

(deftest collect-lambda-zero-params
  "ast-lambda with no params and an int body returns (→ () int)."
  (multiple-value-bind (ty _cs)
      (collect
       (cl-cc/ast:make-ast-lambda
        :params '()
        :body   (list (cl-cc/ast:make-ast-int :value 1))))
    (declare (ignore _cs))
    (assert-true (cl-cc/type::type-arrow-p ty))
    (assert-null (cl-cc/type::type-arrow-params ty))
    (assert-true (type-equal-p type-int (cl-cc/type::type-arrow-return ty)))))

(deftest collect-lambda-one-param
  "ast-lambda with one param returns arrow with 1 fresh param type."
  (multiple-value-bind (ty _cs)
      (collect
       (cl-cc/ast:make-ast-lambda
        :params '(x)
        :body   (list (cl-cc/ast:make-ast-int :value 0))))
    (declare (ignore _cs))
    (assert-true (cl-cc/type::type-arrow-p ty))
    (assert-= 1 (length (cl-cc/type::type-arrow-params ty)))))

(deftest collect-lambda-empty-body
  "ast-lambda with empty body has return type null."
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

(deftest collect-progn-empty
  "ast-progn with no forms returns type-null."
  (multiple-value-bind (ty _cs)
      (collect (cl-cc/ast:make-ast-progn :forms '()))
    (declare (ignore _cs))
    (assert-true (type-equal-p type-null ty))))

(deftest collect-progn-last-form-type
  "ast-progn returns the type of the last form."
  (multiple-value-bind (ty _cs)
      (collect
       (cl-cc/ast:make-ast-progn
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

(deftest collect-defvar-with-value
  "ast-defvar with value walks the value and returns type-symbol."
  (multiple-value-bind (ty _cs)
      (collect
       (cl-cc/ast:make-ast-defvar
        :name '*x*
        :value (cl-cc/ast:make-ast-int :value 0)))
    (declare (ignore _cs))
    (assert-true (type-equal-p type-symbol ty))))

(deftest collect-defvar-without-value
  "ast-defvar with no value returns type-symbol without signaling."
  (multiple-value-bind (ty _cs)
      (collect
       (cl-cc/ast:make-ast-defvar :name '*x* :value nil))
    (declare (ignore _cs))
    (assert-true (type-equal-p type-symbol ty))))

;;; ─── ast-setq ────────────────────────────────────────────────────────────────

(deftest collect-setq-known-var-emits-constraint
  "ast-setq for a bound var emits an equality constraint and returns the value type."
  (let* ((env (cl-cc/type::type-env-extend
               'n (cl-cc/type::type-to-scheme type-int) (empty-env))))
    (multiple-value-bind (ty cs)
        (collect-in
         (cl-cc/ast:make-ast-setq
          :var 'n
          :value (cl-cc/ast:make-ast-int :value 99))
         env)
      (assert-true (type-equal-p type-int ty))
      (assert-= 1 (length cs))
      (assert-eq :equal (cl-cc/type::constraint-kind (first cs))))))

(deftest collect-setq-unknown-var-no-constraint
  "ast-setq for an unbound var emits no constraint but still returns the value type."
  (multiple-value-bind (ty cs)
      (collect
       (cl-cc/ast:make-ast-setq
        :var 'unbound
        :value (cl-cc/ast:make-ast-int :value 0)))
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
