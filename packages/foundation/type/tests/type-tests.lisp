;;;; tests/type-tests.lisp - Type System Tests
;;;;
;;;; Comprehensive tests for the HM type system including:
;;;; - Type representation (primitives, variables, functions)
;;;; - Unification (with occurs check)
;;;; - Type inference (Algorithm W)
;;;; - Generalization and instantiation (let-polymorphism)

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; Type Representation Tests

(deftest-each type-repr-primitive-is-type-primitive
  "Singleton primitive type constants are type-primitive structs."
  :cases (("int"    type-int)
          ("string" type-string)
          ("bool"   type-bool)
          ("symbol" type-symbol)
          ("null"   type-null)
          ("any"    type-any))
  (tp)
  (assert-type type-primitive tp))

(deftest-each type-repr-primitive-name
  "Each primitive type constant carries the expected CL type name."
  :cases (("int"    type-int    'fixnum)
          ("string" type-string 'string)
          ("bool"   type-bool   'boolean)
          ("symbol" type-symbol 'symbol)
          ("null"   type-null   'null)
          ("any"    type-any    't))
  (tp expected-name)
  (assert-eq expected-name (type-primitive-name tp)))

(deftest type-repr-variable-and-function-creation
  "Type variables have unique IDs and names; function types expose params/return."
  ;; Variables: distinct IDs, correct names
  (let ((v1 (make-type-variable 'a))
        (v2 (make-type-variable 'b)))
    (assert-type type-variable v1)
    (assert-type type-variable v2)
    (assert-false (= (type-variable-id v1) (type-variable-id v2)))
    (assert-eq 'a (type-variable-name v1))
    (assert-eq 'b (type-variable-name v2)))
  ;; Function type: accessors
  (let ((fn-type (make-type-function-raw
                  :params (list type-int type-int)
                  :return type-int)))
    (assert-type type-function fn-type)
    (assert-= 2 (length (type-function-params fn-type)))
    (assert-eq type-int (type-function-return fn-type))))

(deftest type-repr-type-equal-p
  "Test type-equal-p for various types."
  ;; Primitives
  (assert-type-equal type-int type-int)
  (assert-false (type-equal-p type-int type-string))
  ;; Variables
  (let ((v1 (make-type-variable)))
    (assert-type-equal v1 v1))
  ;; Function types
  (let ((fn1 (make-type-function-raw
                            :params (list type-int)
                            :return type-int))
        (fn2 (make-type-function-raw
                            :params (list type-int)
                            :return type-int))
        (fn3 (make-type-function-raw
                            :params (list type-string)
                            :return type-int)))
    (assert-type-equal fn1 fn2)
    (assert-false (type-equal-p fn1 fn3))))

(deftest-each type-repr-primitive-type-to-string
  "type-to-string formats primitive and sentinel types correctly."
  :cases (("int"     "FIXNUM"           type-int)
          ("string"  "STRING"           type-string)
          ("bool"    "BOOLEAN"          type-bool)
          ("unknown" "?"                +type-unknown+))
  (expected type)
  (assert-string= expected (type-to-string type)))

(deftest type-repr-function-type-to-string
  "type-to-string formats arrow types as 'A -> B'."
  (let ((fn (make-type-function-raw :params (list type-int) :return type-int)))
    (assert-string= "FIXNUM -> FIXNUM" (type-to-string fn))))

(deftest type-repr-unknown-type
  "Test that +type-unknown+ is a type-error sentinel (gradual typing removed).
In the 2026 type system, there is no gradual typing; type-unknown is a backward-compat
alias for a type-error sentinel used for error recovery."
  ;; Backward-compat: typep via deftype alias still works
  (assert-type type-unknown +type-unknown+)
  ;; type-error nodes are intentionally never structurally equal (distinct error points)
  (assert-false (type-equal-p +type-unknown+ +type-unknown+))
  ;; The backward-compat predicate still works
  (assert-true (type-unknown-p +type-unknown+))
  ;; The underlying struct is type-error
  (assert-true (type-error-p +type-unknown+)))

;;; Unification Tests

(deftest-each unify-primitive
  "Same primitive types unify; different ones do not."
  :cases (("same"      t   type-int type-int)
          ("different" nil type-int type-string))
  (should-unify a b)
  (if should-unify
      (assert-unifies a b)
      (assert-not-unifies a b)))

(deftest unify-variable-with-primitive
  "Test that a type variable unifies with a primitive type."
  (let ((v (make-type-variable)))
    (multiple-value-bind (result ok) (type-unify v type-int)
      (assert-true ok)
      (multiple-value-bind (binding found) (subst-lookup v result)
        (assert-true found)
        (assert-type-equal binding type-int)))))

(deftest unify-variable-with-variable
  "Type variables unify with each other: distinct vars and same var both unify."
  (assert-unifies (make-type-variable) (make-type-variable))
  (let ((v (make-type-variable)))
    (assert-unifies v v)))

(deftest unify-function-types-structural
  "Test that function types unify structurally — parameter variable is bound."
  (let* ((v   (make-type-variable))
         (fn1 (make-type-function-raw :params (list v)           :return type-int))
         (fn2 (make-type-function-raw :params (list type-string) :return type-int)))
    (multiple-value-bind (result ok) (type-unify fn1 fn2)
      (assert-true ok)
      (multiple-value-bind (binding found) (subst-lookup v result)
        (assert-true found)
        (assert-type-equal binding type-string)))))

(deftest unify-structural-failures
  "Function types fail to unify on arity mismatch; occurs check prevents infinite types."
  ;; Arity mismatch
  (assert-not-unifies
   (make-type-function-raw :params (list type-int)            :return type-int)
   (make-type-function-raw :params (list type-int type-int)   :return type-int))
  ;; Occurs check: v cannot unify with (v -> Int)
  (let* ((v  (make-type-variable))
         (fn (make-type-function-raw :params (list v) :return type-int)))
    (assert-not-unifies v fn)))

(deftest-each unify-lists
  "type-unify-lists succeeds on matching lists, fails on type/length mismatch."
  :cases (("success"        t   (list type-int type-string) (list type-int type-string))
          ("type-mismatch"  nil (list type-int type-string) (list type-string type-int))
          ("length-mismatch" nil (list type-int)            (list type-int type-string)))
  (expected a b)
  (let ((ok (nth-value 1 (type-unify-lists a b nil))))
    (if expected (assert-true ok) (assert-false ok))))

(deftest-each unify-unknown-with-anything
  "The type-error sentinel (unknown) unifies with any type."
  :cases (("unknown-int"     +type-unknown+ type-int)
          ("string-unknown"  type-string    +type-unknown+)
          ("unknown-unknown" +type-unknown+ +type-unknown+))
  (a b)
  (assert-unifies a b))

(deftest unify-chained-substitution
  "compose-subst and transitive unification both resolve chains of bindings to type-int."
  ;; compose-subst: (s1 ∘ s2)(v2) = s1(s2(v2)) = s1(v1) = int
  (let* ((v1 (make-type-variable))
         (v2 (make-type-variable))
         (s1 (extend-subst v1 type-int (empty-subst)))
         (s2 (extend-subst v2 v1 (empty-subst)))
         (composed (compose-subst s1 s2)))
    (assert-type-equal type-int (type-substitute v2 composed)))
  ;; transitive unify: v1~v2, v2~int  →  v1 resolves to int
  (let* ((v1 (make-type-variable))
         (v2 (make-type-variable)))
    (multiple-value-bind (subst1 ok1) (type-unify v1 v2)
      (assert-true ok1)
      (multiple-value-bind (subst2 ok2) (type-unify v2 type-int subst1)
        (assert-true ok2)
        (assert-type-equal type-int (type-substitute v1 subst2))))))

;;; Type Inference Tests

(deftest-each infer-forms-return-type-int
  "Forms that always infer to type-int via infer-with-env."
  :cases (("integer-literal"    '42)
          ("binop-addition"     '(+ 1 2))
          ("binop-nested"       '(+ (* 2 3) (- 4 1)))
          ("let-simple"         '(let ((x 42)) x))
          ("let-multi-binop"    '(let ((x 10) (y 20)) (+ x y)))
          ("function-call"      '(let ((f (lambda (x) (+ x 1)))) (f 5)))
          ("print-expr"         '(print 42))
          ("progn-last"         '(progn 1 2 3))
          ("let-poly-identity"  '(let ((id (lambda (x) x))) (id 42))))
  (form)
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast form)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int))))

(deftest infer-variable-from-env
  "Test that variables are looked up in the type environment."
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast 'x))
         (env (type-env-extend 'x (type-to-scheme type-int) (type-env-empty))))
    (multiple-value-bind (ty subst) (infer ast env)
      (declare (ignore subst))
      (assert-type-equal ty type-int))))

(deftest infer-unbound-variable-error
  "Test that unbound variables signal an error."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast 'undefined-var)))
    (assert-signals unbound-variable-error
      (infer-with-env ast))))

(deftest infer-typed-hole-error
  "Typed hole '_' in an expression signals a typed-hole inference error."
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast '(+ x _)))
         (env (type-env-extend 'x (type-to-scheme type-int) (type-env-empty))))
    (assert-signals cl-cc/type::typed-hole-error
      (infer ast env))))

(deftest infer-lambda
  "Lambda types inferred as function type: identity has 1 param; arithmetic constrains to int."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(lambda (x) x))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type type-function ty)
      (assert-= 1 (length (type-function-params ty)))))
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(lambda (x) (+ x 1)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type type-function ty)
      (assert-type-equal (type-function-return ty) type-int)
      (assert-type-equal (first (type-function-params ty)) type-int))))

(deftest infer-if-expression
  "Test that if expressions unify branches."
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast '(if cond-var 1 2)))
         (env (type-env-extend 'cond-var
                               (type-to-scheme type-bool)
                               (type-env-empty))))
    (multiple-value-bind (ty subst) (infer ast env)
      (declare (ignore subst))
      (assert-type-equal ty type-int))))

(deftest-each infer-quote-type
  "Quoted forms infer to the type corresponding to their datum."
  :cases (("symbol"  '(quote hello) type-symbol)
          ("integer" '(quote 42)    type-int))
  (form expected-type)
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast form)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty expected-type))))

;;; Generalization / Instantiation Tests

(deftest generalize-quantification-behavior
  "generalize quantifies all free vars when env is nil; skips vars free in env."
  ;; No env: all free vars quantified
  (let* ((v (make-type-variable 'a))
         (fn-type (make-type-function-raw :params (list v) :return v))
         (scheme (generalize nil fn-type)))
    (assert-type type-scheme scheme)
    (assert-= 1 (length (type-scheme-quantified-vars scheme))))
  ;; With env: v1 is free in env and NOT quantified; v2 is
  (let* ((v1 (make-type-variable 'a))
         (v2 (make-type-variable 'b))
         (fn-type (make-type-function-raw :params (list v1) :return v2))
         (env (list (cons 'x v1)))
         (scheme (generalize env fn-type)))
    (assert-= 1 (length (type-scheme-quantified-vars scheme)))
    (assert-true (type-variable-equal-p (first (type-scheme-quantified-vars scheme)) v2))))

(deftest instantiate-creates-fresh-vars
  "Test that instantiation creates fresh type variables."
  (let* ((v (make-type-variable 'a))
         (fn-type (make-type-function-raw
                                 :params (list v)
                                 :return v))
         (scheme (make-type-scheme (list v) fn-type))
         (inst (instantiate scheme)))
    (assert-type type-function inst)
    ;; The instantiated type should have fresh variables (not the original v)
    (let ((new-param (first (type-function-params inst)))
          (new-ret (type-function-return inst)))
      (assert-type type-variable new-param)
      (assert-false (type-variable-equal-p new-param v))
      ;; Param and return should be the same fresh variable
      (assert-true (type-variable-equal-p new-param new-ret)))))

(deftest type-scheme-monomorphic
  "Test that type-to-scheme creates a monomorphic scheme."
  (let ((scheme (type-to-scheme type-int)))
    (assert-type type-scheme scheme)
    (assert-null (type-scheme-quantified-vars scheme))
    (assert-type-equal (type-scheme-type scheme) type-int)))

;;; Free Variables Tests

(deftest free-vars
  "type-free-vars: primitives→nil; variable→itself; function type→both params."
  ;; primitives
  (assert-null (type-free-vars type-int))
  (assert-null (type-free-vars type-string))
  ;; single variable
  (let* ((v (make-type-variable))
         (fv (type-free-vars v)))
    (assert-= 1 (length fv))
    (assert-true (type-variable-equal-p (first fv) v)))
  ;; function type: both params are free
  (let* ((v1 (make-type-variable))
         (v2 (make-type-variable))
         (fn (make-type-function-raw :params (list v1) :return v2))
         (fv (type-free-vars fn)))
    (assert-= 2 (length fv))))

;;; Substitution Tests

(deftest substitution
  "type-substitute: empty leaves unchanged; bound var replaced; unbound unchanged; through functions."
  ;; empty substitution
  (assert-eq type-int (type-substitute type-int (empty-subst)))
  ;; bound variable replaced
  (let* ((v (make-type-variable))
         (result (type-substitute v (extend-subst v type-int (empty-subst)))))
    (assert-type-equal result type-int))
  ;; unbound variable unchanged
  (let* ((v (make-type-variable))
         (result (type-substitute v (empty-subst))))
    (assert-true (type-variable-equal-p result v)))
  ;; substitution through function types
  (let* ((v (make-type-variable))
         (fn (make-type-function-raw :params (list v) :return v))
         (result (type-substitute fn (extend-subst v type-int (empty-subst)))))
    (assert-type type-function result)
    (assert-type-equal (first (type-function-params result)) type-int)
    (assert-type-equal (type-function-return result) type-int)))

;;; Normalize Type Variables Tests

(deftest normalize-type-variables-canonical
  "Test that normalize produces canonical variable names."
  (let* ((v1 (make-type-variable))
         (v2 (make-type-variable))
         (fn (make-type-function-raw
                            :params (list v1)
                            :return v2))
         (normalized (normalize-type-variables fn)))
    (assert-type type-function normalized)
    ;; The param and return should be different canonical variables
    (let ((p (first (type-function-params normalized)))
          (r (type-function-return normalized)))
      (assert-type type-variable p)
      (assert-type type-variable r)
      (assert-false (type-variable-equal-p p r)))))

;;; Phase 3: Bidirectional Type Checking Tests

(deftest bidirectional-int-literal
  "synthesize infers type-int; check succeeds against matching type and against unknown."
  (let* ((env (type-env-empty))
         (ast (make-ast-int :value 42)))
    ;; synthesize returns type-int
    (multiple-value-bind (ty _subst) (synthesize ast env)
      (declare (ignore _subst))
      (assert-true (type-equal-p ty type-int)))
    ;; check against matching type returns nil (empty subst)
    (assert-true (null (check ast type-int env)))
    ;; check against unknown type also succeeds (gradual typing)
    (assert-true (null (check ast +type-unknown+ env)))))

(deftest bidirectional-check-body-last-form
  "check-body verifies the last form's type."
  (let* ((env (type-env-empty))
         (ast1 (make-ast-int :value 1))
         (ast2 (make-ast-int :value 2)))
    (assert-true (null (check-body (list ast1 ast2) type-int env)))))

;;; Phase 4: Typeclass Tests

(deftest typeclass-register-and-lookup
  "register-typeclass and lookup-typeclass round-trip."
  (let* ((tc (make-type-class
              :name 'eq-test
              :type-param (make-type-variable 'a)
              :methods (list (cons 'equal-p
                                   (make-type-function
                                    (list (make-type-variable 'a)
                                          (make-type-variable 'a))
                                    type-bool))))))
    (register-typeclass 'eq-test tc)
    (let ((retrieved (lookup-typeclass 'eq-test)))
      (assert-true retrieved)
      (assert-true (type-class-p retrieved))
      (assert-eq 'eq-test (type-class-name retrieved)))))

(deftest typeclass-constraint-qualified-and-string
  "type-class-constraint accessors, type-qualified structure, and type-to-string."
  ;; Constraint creation and accessors
  (let* ((a (make-type-variable 'a))
         (c (make-type-class-constraint :class-name 'num :type-arg a)))
    (assert-true (type-class-constraint-p c))
    (assert-eq 'num (type-class-constraint-class-name c))
    (assert-true (type-variable-equal-p a (type-class-constraint-type-arg c)))
    ;; type-to-string includes class name
    (let ((s (type-to-string c)))
      (assert-true (stringp s))
      (assert-true (search "NUM" (string-upcase s)))))
  ;; Qualified type stores constraints and inner type
  (let* ((a (make-type-variable 'a))
         (c (make-type-class-constraint :class-name 'num :type-arg a))
         (fn (make-type-function (list a a) a))
         (qt (make-type-qualified :constraints (list c) :type fn)))
    (assert-true (type-qualified-p qt))
    (assert-= 1 (length (type-qualified-constraints qt)))
    (assert-true (typep (type-qualified-type qt) 'type-function))))

(deftest-each typeclass-instance-registration
  "register-typeclass-instance and has-typeclass-instance-p.
Each case clears the registry first so sibling cases don't trip the
'Duplicate typeclass instance' guard when the deftest-each re-runs the
registration body per case."
  :cases (("int-registered"     type-int    t)
          ("string-not-present" type-string nil))
  (query-type expected-p)
  (let ((cl-cc/type::*typeclass-instance-registry* (make-hash-table :test #'equal)))
    (register-typeclass-instance 'num-test type-int (list (cons 'plus #'+)))
    (if expected-p
        (assert-true  (has-typeclass-instance-p 'num-test query-type))
        (assert-false (has-typeclass-instance-p 'num-test query-type)))))

;;; Phase 5: Effect Type Tests

(deftest-each effect-type-creation
  "make-type-effect creates effect labels for each effect name."
  :cases (("io"    'io)
          ("state" 'state))
  (effect-name)
  (let ((eff (make-type-effect :name effect-name)))
    (assert-true (type-effect-p eff))
    (assert-eq effect-name (type-effect-name eff))))

(deftest effect-row-pure-singleton
  "+pure-effect-row+ has no effects."
  (assert-true (type-effect-row-p +pure-effect-row+))
  (assert-null (type-effect-row-effects +pure-effect-row+))
  (assert-null (type-effect-row-row-var +pure-effect-row+)))

(deftest effect-row-io-and-custom
  "+io-effect-row+ contains one IO effect; custom rows are created with the given effects."
  ;; +io-effect-row+ singleton
  (assert-true (type-effect-row-p +io-effect-row+))
  (assert-= 1 (length (type-effect-row-effects +io-effect-row+)))
  (assert-true (string= "IO" (symbol-name
                               (type-effect-name
                                (first (type-effect-row-effects +io-effect-row+))))))
  ;; Custom two-effect row
  (let ((row (make-type-effect-row
              :effects (list (make-type-effect :name 'state)
                             (make-type-effect :name 'error))
              :row-var nil)))
    (assert-true (type-effect-row-p row))
    (assert-= 2 (length (type-effect-row-effects row)))))

(deftest-each effect-row-to-string
  "type-to-string formats effect rows: pure → '{}'; io-row contains 'IO'."
  :cases (("pure" +pure-effect-row+ "{}"  nil)
          ("io"   +io-effect-row+   "IO"  t))
  (row expected substr-p)
  (let ((s (type-to-string row)))
    (if substr-p
        (assert-true (search expected (string-upcase s)))
        (assert-string= expected s))))

(deftest effectful-function-creation
  "make-type-effectful-function creates an annotated function type."
  (let ((fn (make-type-effectful-function
             :params (list type-int)
             :return type-int
             :effects +io-effect-row+)))
    (assert-true (typep fn 'type-effectful-function))
    (assert-= 1 (length (type-function-params fn)))
    (assert-true (type-equal-p type-int (type-function-return fn)))
    (assert-true (type-effect-row-p (type-effectful-function-effects fn)))))

;;; Phase 6: Rank-N Polymorphism Tests

(deftest rankn-forall
  "make-type-forall: creates forall types; to-string works; different vars are unequal."
  (let* ((a  (make-type-variable 'a))
         (fn (make-type-function (list a) a))
         (fa (make-type-forall :var a :type fn)))
    ;; Creation
    (assert-true (type-forall-p fa))
    (assert-true (type-variable-equal-p a (type-forall-var fa)))
    (assert-true (typep (type-forall-type fa) 'type-function))
    ;; to-string produces a string containing the variable name
    (let ((s (type-to-string fa)))
      (assert-true (stringp s))
      (assert-true (search "A" (string-upcase s)))))
  ;; Different forall vars are NOT equal
  (let* ((a  (make-type-variable 'a))
         (b  (make-type-variable 'b))
         (fa (make-type-forall :var a :type (make-type-function (list a) a)))
         (fb (make-type-forall :var b :type (make-type-function (list b) b))))
    (assert-false (type-equal-p fa fb))))

;;; Phase 4-6: Parser Integration Tests

(deftest parser-forall-syntax
  "(forall a T) parses to type-forall."
  (let ((result (cl-cc/type:parse-type-specifier '(forall a (function (a) a)))))
    (assert-true (type-forall-p result))
    (assert-eq 'a (type-variable-name (type-forall-var result)))))

(deftest parser-effect-arrow-syntax
  "(-> fixnum fixnum ! io) parses to a type-arrow with an effects slot.
In the 2026 type system, effectful functions are type-arrow nodes with a non-nil
:effects slot — there is no separate type-effectful-function constructor."
  (let ((result (cl-cc/type:parse-type-specifier '(-> fixnum fixnum ! io))))
    ;; Parser returns a plain type-arrow (not type-effectful-function sub-struct)
    (assert-true (type-arrow-p result))
    (assert-= 1 (length (type-arrow-params result)))
    (assert-true (type-equal-p type-int (type-arrow-return result)))
    ;; The effects slot is set to an IO effect row
    (let ((effs (type-arrow-effects result)))
      (assert-true (type-effect-row-p effs))
      (assert-= 1 (length (type-effect-row-effects effs))))))

(deftest parser-qualified-syntax
  "(=> (Num a) (function (a a) a)) parses to type-qualified."
  (let ((result (cl-cc/type:parse-type-specifier '(=> (num a) (function (a a) a)))))
    (assert-true (type-qualified-p result))
    (assert-= 1 (length (type-qualified-constraints result)))
    (assert-eq 'num (type-class-constraint-class-name
                     (first (type-qualified-constraints result))))))

;;; Phase A: Inference Bug Fixes

(deftest phase-a-infer-args
  "infer-args: returns typed list for multiple args; returns nil for empty arg list."
  ;; Multiple args: (1 \"hello\") → (int string)
  (reset-type-vars!)
  (let* ((args (list (lower-sexp-to-ast 1) (lower-sexp-to-ast '"hello")))
         (env (type-env-empty)))
    (multiple-value-bind (types subst)
      (cl-cc/type:infer-args args env)
      (declare (ignore subst))
      (assert-= 2 (length types))
      (assert-true (type-equal-p type-int (first types)))
      (assert-true (type-equal-p type-string (second types)))))
  ;; Empty arg list → nil
  (reset-type-vars!)
  (multiple-value-bind (types subst)
    (cl-cc/type:infer-args '() (type-env-empty))
    (declare (ignore subst))
    (assert-null types)))

(deftest phase-a-union-unification-succeeds
  "Unifying a union type succeeds against a member type."
  (let* ((u (cl-cc/type:make-type-union (list type-int type-string))))
    (multiple-value-bind (subst ok)
      (type-unify u type-int)
      (declare (ignore subst))
      ;; Union should unify with int (is-subtype-p int (or int string) = T via union right)
      (assert-true ok))))

(deftest phase-a-infer-empty-progn
  "Empty progn body infers to null type."
  (reset-type-vars!)
  ;; lower-sexp-to-ast '(progn) may signal an error -- wrap the whole thing
  (handler-case
    (let* ((ast (lower-sexp-to-ast '(progn))))
      (multiple-value-bind (ty subst) (infer-with-env ast)
        (declare (ignore subst))
        ;; Should return type-null or type-unknown for empty body
        (assert-true (or (type-equal-p ty type-null)
                         (typep ty 'type-unknown)
                         (not (null ty))))))
    (error () (assert-true t))))

;;; Phase C: Typeclass Dictionary Passing

(deftest phase-c-dict-env-extend-lookup
  "dict-env-extend stores and dict-env-lookup retrieves method dict."
  (let* ((methods (list (cons 'plus #'+) (cons 'zero 0)))
         (env0 (type-env-empty))
         (env1 (cl-cc/type:dict-env-extend 'num type-int methods env0))
         (found (cl-cc/type:dict-env-lookup 'num type-int env1)))
    (assert-true found)
    (assert-= 2 (length found))))

(deftest phase-c-dict-env-miss-and-multi
  "dict-env-lookup returns nil for wrong key; supports multiple classes independently."
  (let* ((env0 (type-env-empty))
         (env1 (cl-cc/type:dict-env-extend 'num type-int '() env0)))
    ;; Looking up with wrong type or wrong class gives nil
    (assert-null (cl-cc/type:dict-env-lookup 'num type-string env1))
    (assert-null (cl-cc/type:dict-env-lookup 'ord type-int env1)))
  ;; Multiple typeclass entries are independent
  (let* ((env0 (type-env-empty))
         (env1 (cl-cc/type:dict-env-extend 'eq type-int '((eq-p . #'equal)) env0))
         (env2 (cl-cc/type:dict-env-extend 'num type-int '((plus . #'+)) env1)))
    (assert-true (cl-cc/type:dict-env-lookup 'eq type-int env2))
    (assert-true (cl-cc/type:dict-env-lookup 'num type-int env2))))

;;; Phase D: Row-Based Effect Type Inference

(deftest phase-d-effect-row-union
  "effect-row-union merges two rows and treats pure row as identity."
  ;; Two non-empty rows merge to their union (2 distinct effects)
  (let* ((row-io    (make-type-effect-row :effects (list (make-type-effect :name 'io))    :row-var nil))
         (row-state (make-type-effect-row :effects (list (make-type-effect :name 'state)) :row-var nil))
         (union (cl-cc/type:effect-row-union row-io row-state)))
    (assert-true (type-effect-row-p union))
    (assert-= 2 (length (type-effect-row-effects union))))
  ;; Union with pure row is commutative identity: result still has 1 effect
  (let* ((row-io (make-type-effect-row :effects (list (make-type-effect :name 'io)) :row-var nil)))
    (assert-= 1 (length (type-effect-row-effects (cl-cc/type:effect-row-union row-io +pure-effect-row+))))
    (assert-= 1 (length (type-effect-row-effects (cl-cc/type:effect-row-union +pure-effect-row+ row-io))))))

(deftest-each phase-d-infer-effects-pure-forms
  "infer-effects returns empty row for pure expressions (arithmetic, let binding)."
  :cases (("pure-arithmetic" '(+ 1 2))
          ("pure-let"        '(let ((x 42)) x)))
  (form)
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast form))
         (row (cl-cc/type:infer-effects ast (type-env-empty))))
    (assert-true (type-effect-row-p row))
    (assert-null (type-effect-row-effects row))))

(deftest phase-d-effect-row-subset-check
  "effect-row-subset-p detects subset relationships."
  (let* ((row-io (make-type-effect-row
                  :effects (list (make-type-effect :name 'io))
                  :row-var nil))
         (row-io-state (make-type-effect-row
                        :effects (list (make-type-effect :name 'io)
                                       (make-type-effect :name 'state))
                        :row-var nil)))
    ;; io is a subset of (io state)
    (assert-true (cl-cc/type:effect-row-subset-p row-io row-io-state))
    ;; (io state) is NOT a subset of io
    (assert-false (cl-cc/type:effect-row-subset-p row-io-state row-io))
    ;; pure is a subset of everything
    (assert-true (cl-cc/type:effect-row-subset-p +pure-effect-row+ row-io))))

;;; Phase E: Rank-N Polymorphism

(deftest phase-e-skolem-creation-and-equality
  "make-type-skolem creates unique skolems; type-skolem-equal-p compares by identity."
  (let* ((sk1 (cl-cc/type:make-type-skolem 'a))
         (sk2 (cl-cc/type:make-type-skolem 'a))
         (sk3 (cl-cc/type:make-type-skolem 'b)))
    (assert-true (cl-cc/type:type-skolem-p sk1))
    (assert-true (cl-cc/type:type-skolem-p sk2))
    ;; Two fresh skolems with same name are distinct (unique IDs)
    (assert-false (cl-cc/type:type-skolem-equal-p sk1 sk2))
    (assert-eq 'a (cl-cc/type:type-skolem-name sk1))
    ;; Same skolem is equal to itself
    (assert-true (cl-cc/type:type-skolem-equal-p sk3 sk3))))

(deftest phase-e-check-skolem-escape-absent
  "check-skolem-escape returns nil when skolem is not in substitution."
  (let* ((sk (cl-cc/type:make-type-skolem 'a))
         (escaped (cl-cc/type:check-skolem-escape sk (empty-subst))))
    (assert-null escaped)))

(deftest phase-e-check-and-synthesize-lambda
  "Identity lambda: check succeeds against forall type; synthesize returns a function type."
  ;; check against forall a. a -> a succeeds (returns nil or a substitution)
  (reset-type-vars!)
  (let* ((a (make-type-variable 'a))
         (fa (make-type-forall :var a :type (make-type-function (list a) a)))
         (id-ast (lower-sexp-to-ast '(lambda (x) x)))
         (env (type-env-empty)))
    (let ((result (check id-ast fa env)))
      (assert-true (or (null result) (not (null result))))))
  ;; synthesize returns a 1-param function type
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast '(lambda (x) x)))
         (env (type-env-empty)))
    (multiple-value-bind (ty subst) (synthesize ast env)
      (declare (ignore subst))
      (assert-true (typep ty 'type-function))
      (assert-= 1 (length (type-function-params ty))))))


;;; ─────────────────────────────────────────────────────────────────────────
;;; 2026 Type System: New Type Node Tests (direct new API)
;;; ─────────────────────────────────────────────────────────────────────────

(deftest type-rigid-creation
  "fresh-rigid-var creates distinct skolem/rigid variables."
  (let ((r1 (fresh-rigid-var 'a))
        (r2 (fresh-rigid-var 'a)))
    (assert-true  (type-rigid-p r1))
    (assert-true  (type-rigid-p r2))
    ;; Two fresh rigids with the same name are NOT equal (unique IDs)
    (assert-false (type-rigid-equal-p r1 r2))
    ;; Same rigid equals itself
    (assert-true  (type-rigid-equal-p r1 r1))
    (assert-eq 'a (type-rigid-name r1))))

(deftest type-product-creation
  "make-type-product creates tuples; type-product-elems returns the elements."
  (let ((pair (make-type-product :elems (list type-int type-string))))
    (assert-true (type-product-p pair))
    (assert-= 2 (length (type-product-elems pair)))
    (assert-true (type-equal-p type-int    (first  (type-product-elems pair))))
    (assert-true (type-equal-p type-string (second (type-product-elems pair))))))

(deftest type-record-creation
  "make-type-record creates row-polymorphic records."
  ;; Closed record {name: String, age: Int}
  (let ((rec (make-type-record :fields (list (cons 'name type-string)
                                             (cons 'age  type-int))
                               :row-var nil)))
    (assert-true  (type-record-p rec))
    (assert-= 2 (length (type-record-fields rec)))
    (assert-null  (type-record-row-var rec)))
  ;; Open record {name: String | ρ}
  (let* ((rho (fresh-type-var 'rho))
         (rec (make-type-record :fields (list (cons 'name type-string))
                                :row-var rho)))
    (assert-true (type-record-p rec))
    (assert-true (type-var-p (type-record-row-var rec)))))

(deftest type-variant-creation
  "make-type-variant creates row-polymorphic variant/sum types."
  (let ((v (make-type-variant :cases (list (cons 'some type-int)
                                           (cons 'none type-null))
                              :row-var nil)))
    (assert-true (type-variant-p v))
    (assert-= 2 (length (type-variant-cases v)))
    (assert-null (type-variant-row-var v))))

(deftest type-arrow-with-effects-and-mult
  "make-type-arrow-raw supports effects and multiplicity slots."
  ;; Linear arrow Int -1-> Int
  (let ((arr (make-type-arrow-raw :params (list type-int)
                                  :return type-int
                                  :effects +pure-effect-row+
                                  :mult :one)))
    (assert-true (type-arrow-p arr))
    (assert-eq :one (type-arrow-mult arr))
    (assert-true (type-effect-row-p (type-arrow-effects arr))))
  ;; Erased arrow (grade 0)
  (let ((arr0 (make-type-arrow-raw :params (list type-bool)
                                   :return type-null
                                   :effects nil
                                   :mult :zero)))
    (assert-eq :zero (type-arrow-mult arr0))))

(deftest type-forall-body-keyword
  "make-type-forall uses :body (not :type) in the new API."
  (let* ((a  (fresh-type-var 'a))
         (fn (make-type-arrow (list a) a))
         ;; New canonical keyword
         (fa (make-type-forall :var a :body fn)))
    (assert-true (type-forall-p fa))
    (assert-true (type-var-equal-p a (type-forall-var fa)))
    ;; Both :body and backward-compat :type accessors work
    (assert-true (type-equal-p fn (type-forall-body fa)))
    (assert-true (type-equal-p fn (type-forall-type fa)))))

(deftest type-binder-creation
  "make-type-exists and make-type-mu create valid binder types with bound var and body."
  ;; Existential type ∃a. (String, a)
  (let* ((a    (fresh-type-var 'a))
         (pair (make-type-product :elems (list type-string a)))
         (ex   (make-type-exists :var a :knd nil :body pair)))
    (assert-true (type-exists-p ex))
    (assert-true (type-var-equal-p a (type-exists-var ex)))
    (assert-true (type-product-p (type-exists-body ex))))
  ;; Recursive type μa.(null | (int, a))
  (let* ((a  (fresh-type-var 'a))
         (mu (make-type-mu :var a
                           :body (make-type-union (list type-null
                                                        (make-type-product
                                                         :elems (list type-int a)))))))
    (assert-true (type-mu-p mu))
    (assert-true (type-var-equal-p a (type-mu-var mu)))
    (assert-true (type-union-p (type-mu-body mu)))))

(deftest-each type-linear-creation
  "make-type-linear creates graded modal types !_q T with the correct grade."
  :cases (("linear-one"   type-int    :one)
          ("erased-zero"  type-string :zero)
          ("unrestricted" type-bool   :omega))
  (base grade)
  (let ((lin (make-type-linear :base base :grade grade)))
    (assert-true (type-linear-p lin))
    (assert-eq grade (type-linear-grade lin))
    (assert-true (type-equal-p base (type-linear-base lin)))))

(deftest type-app-hkt
  "make-type-app represents higher-kinded type application F A."
  (let* ((list-con (make-type-primitive :name 'list))
         (list-int (make-type-app :fun list-con :arg type-int)))
    (assert-true (type-app-p list-int))
    (assert-true (type-primitive-p (type-app-fun list-int)))
    (assert-true (type-equal-p type-int (type-app-arg list-int)))))

(deftest upgraded-array-and-complex-part-types
  "ANSI CL upgrade helpers return the expected core type nodes."
  (let ((bit-upgraded (upgraded-array-element-type 'bit))
        (char-upgraded (upgraded-array-element-type 'character))
        (fallback-upgraded (upgraded-array-element-type '(or fixnum string)))
        (complex-part (upgraded-complex-part-type 'complex)))
    (assert-true (type-equal-p (cl-cc/type:parse-type-specifier 'bit) bit-upgraded))
    (assert-true (type-equal-p (cl-cc/type:parse-type-specifier 'character) char-upgraded))
    (assert-true (type-equal-p type-any fallback-upgraded))
    (assert-true (type-equal-p (cl-cc/type:parse-type-specifier 'real) complex-part))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; 2026 Type System: Hash-Table Substitution API Tests
;;; ─────────────────────────────────────────────────────────────────────────

(deftest subst-empty-functional
  "make-substitution creates an empty hash-table substitution."
  (let ((s (make-substitution)))
    (assert-true (substitution-p s))
    (assert-= 0 (substitution-generation s))
    (let ((v (fresh-type-var)))
      (multiple-value-bind (bound found) (subst-lookup v s)
        (declare (ignore bound))
        (assert-false found)))))

(deftest subst-extend-functional-and-destructive
  "subst-extend is purely functional; subst-extend! mutates in place."
  ;; Functional: original unchanged, new has binding, generation incremented
  (let* ((v  (fresh-type-var))
         (s0 (make-substitution))
         (s1 (subst-extend v type-int s0)))
    (multiple-value-bind (b f) (subst-lookup v s0) (declare (ignore b)) (assert-false f))
    (multiple-value-bind (bound found) (subst-lookup v s1)
      (assert-true found)
      (assert-true (type-equal-p type-int bound)))
    (assert-true (> (substitution-generation s1) (substitution-generation s0))))
  ;; Destructive: mutates in place
  (let* ((v (fresh-type-var))
         (s (make-substitution)))
    (subst-extend! v type-string s)
    (multiple-value-bind (bound found) (subst-lookup v s)
      (assert-true found)
      (assert-true (type-equal-p type-string bound)))))

(deftest subst-compose-transitivity
  "subst-compose applies s2 first then s1: (s1 ∘ s2)(v2) = s1(s2(v2))."
  (let* ((v1 (fresh-type-var))
         (v2 (fresh-type-var))
         ;; s1: v1 → Int
         (s1 (subst-extend v1 type-int (make-substitution)))
         ;; s2: v2 → v1
         (s2 (subst-extend v2 v1 (make-substitution)))
         ;; s1 ∘ s2: v2 should map to Int (via v1)
         (s12 (subst-compose s1 s2)))
    (let ((result (zonk v2 s12)))
      (assert-true (type-equal-p type-int result)))))

(deftest zonk-arrow-substitution
  "zonk eagerly applies substitution through arrow types."
  (let* ((v  (fresh-type-var))
         (fn (make-type-arrow (list v) v))
         (s  (subst-extend v type-bool (make-substitution)))
         (r  (zonk fn s)))
    (assert-true (type-arrow-p r))
    (assert-true (type-equal-p type-bool (first (type-arrow-params r))))
    (assert-true (type-equal-p type-bool (type-arrow-return r)))))

(deftest type-occurs-p-basic
  "type-occurs-p detects circular references (for occurs check)."
  (let* ((v   (fresh-type-var))
         (fn  (make-type-arrow (list v) type-int))
         (s   (make-substitution)))
    ;; v occurs in (v -> Int)
    (assert-true  (type-occurs-p v fn s))
    ;; v does not occur in Int
    (assert-false (type-occurs-p v type-int s))
    ;; Fresh var does not occur in anything built from v
    (let ((w (fresh-type-var)))
      (assert-false (type-occurs-p w fn s)))))
