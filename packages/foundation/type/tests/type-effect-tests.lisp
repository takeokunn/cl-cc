;;;; tests/type-effect-tests.lisp - Type Effect, Free Variables, Substitution, and Rank-N Tests
;;;;
;;;; Covers: free variables, substitution, bidirectional checking, typeclass,
;;;; effect types, effect rows, and rank-N polymorphism (forall).

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; Free Variables Tests

(deftest-each free-vars-primitives
  "type-free-vars returns nil for primitive types."
  :cases (("int"    type-int)
          ("string" type-string))
  (ty)
  (assert-null (type-free-vars ty)))

(deftest free-vars-cases
  "type-free-vars: single variable returns itself; function type returns both param and return vars."
  (let* ((v  (make-type-variable))
         (fv (type-free-vars v)))
    (assert-= 1 (length fv))
    (assert-true (type-variable-equal-p (first fv) v)))
  (let* ((v1 (make-type-variable))
         (v2 (make-type-variable))
         (fn (make-type-function-raw :params (list v1) :return v2))
         (fv (type-free-vars fn)))
    (assert-= 2 (length fv))))

;;; Substitution Tests

(deftest substitution-cases
  "type-substitute: primitive unchanged; bound var replaced; unbound var identity."
  (assert-eq type-int (type-substitute type-int (empty-subst)))
  (let* ((v (make-type-variable))
         (result (type-substitute v (extend-subst v type-int (empty-subst)))))
    (assert-type-equal result type-int))
  (let* ((v (make-type-variable))
         (result (type-substitute v (empty-subst))))
    (assert-true (type-variable-equal-p result v))))

(deftest substitution-through-function-type
  "Substitution distributes into function type params and return."
  (let* ((v      (make-type-variable))
         (fn     (make-type-function-raw :params (list v) :return v))
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

(deftest bidirectional-checking-cases
  "Bidirectional checking: synthesize infers type-int; check succeeds; check-body verifies last form."
  (let* ((env (type-env-empty))
         (ast (make-ast-int :value 42)))
    (multiple-value-bind (ty _subst) (synthesize ast env)
      (declare (ignore _subst))
      (assert-true (type-equal-p ty type-int)))
    (assert-true (null (check ast type-int env)))
    (assert-true (null (check ast +type-unknown+ env))))
  (let* ((env (type-env-empty))
         (ast1 (make-ast-int :value 1))
         (ast2 (make-ast-int :value 2)))
    (assert-true (null (check-body (list ast1 ast2) type-int env)))))

;;; Phase 4: Typeclass Tests

(deftest typeclass-register-and-lookup
  "register-typeclass accepts old type-class values but stores canonical typeclass-defs."
  (let* ((tc (cl-cc/type::make-type-class
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
      (assert-true (typeclass-def-p retrieved))
      (assert-eq 'eq-test (typeclass-def-name retrieved))
      (assert-= 1 (length (typeclass-def-type-params retrieved))))))

(deftest typeclass-constraint-types-cases
  "Constraint accessors return correct fields; qualified type stores constraints + function body."
  (let* ((a (make-type-variable 'a))
         (c (cl-cc/type::make-type-class-constraint :class-name 'num :type-arg a)))
    (assert-true (cl-cc/type::type-class-constraint-p c))
    (assert-eq 'num (cl-cc/type::type-class-constraint-class-name c))
    (assert-true (type-variable-equal-p a (cl-cc/type::type-class-constraint-type-arg c)))
    (let ((s (type-to-string c)))
      (assert-true (stringp s))
      (assert-true (search "NUM" (string-upcase s)))))
  (let* ((a (make-type-variable 'a))
         (c (cl-cc/type::make-type-class-constraint :class-name 'num :type-arg a))
         (fn (make-type-function (list a a) a))
         (qt (make-type-qualified :constraints (list c) :type fn)))
    (assert-true (type-qualified-p qt))
    (assert-= 1 (length (type-qualified-constraints qt)))
    (assert-true (typep (cl-cc/type::type-qualified-type qt) 'type-function))))

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
  (let ((eff (cl-cc/type::make-type-effect :name effect-name)))
    (assert-true (cl-cc/type::type-effect-p eff))
    (assert-eq effect-name (cl-cc/type::type-effect-name eff))))

(deftest-each effect-row-singleton-cases
  "Effect row singletons: pure has 0 effects; io has 1 IO effect; custom multi has N effects."
  :cases (("pure"   +pure-effect-row+  0 nil)
          ("io"     +io-effect-row+    1 "IO")
          ("custom" (make-type-effect-row :effects (list (cl-cc/type::make-type-effect :name 'state)
                                                         (cl-cc/type::make-type-effect :name 'error))
                                          :row-var nil) 2 nil))
  (row expected-count expected-name)
  (assert-true (type-effect-row-p row))
    (assert-= expected-count (length (type-effect-row-effects row)))
    (when expected-name
      (assert-true (string= expected-name
                           (symbol-name (cl-cc/type::type-effect-name (first (type-effect-row-effects row))))))))

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
  (let ((fn (cl-cc/type::make-type-effectful-function
             :params (list type-int)
             :return type-int
             :effects +io-effect-row+)))
    (assert-true (typep fn 'type-effectful-function))
    (assert-= 1 (length (type-function-params fn)))
    (assert-true (type-equal-p type-int (type-function-return fn)))
    (assert-true (type-effect-row-p (type-effectful-function-effects fn)))))

;;; Phase 6: Rank-N Polymorphism Tests

(deftest rankn-forall-creation-and-equality
  "make-type-forall creates well-formed forall types; foralls with distinct vars are not equal."
  (let* ((a  (make-type-variable 'a))
         (fn (make-type-function (list a) a))
         (fa (make-type-forall :var a :type fn)))
    (assert-true (type-forall-p fa))
    (assert-true (type-variable-equal-p a (type-forall-var fa)))
    (assert-true (typep (cl-cc/type::type-forall-type fa) 'type-function))
    (let ((s (type-to-string fa)))
      (assert-true (stringp s))
      (assert-true (search "A" (string-upcase s)))))
  (let* ((a  (make-type-variable 'a))
         (b  (make-type-variable 'b))
         (fa (make-type-forall :var a :type (make-type-function (list a) a)))
         (fb (make-type-forall :var b :type (make-type-function (list b) b))))
    (assert-false (type-equal-p fa fb))))
