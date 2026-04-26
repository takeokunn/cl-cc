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
  (let* ((v  (fresh-type-var))
         (fv (type-free-vars v)))
    (assert-= 1 (length fv))
    (assert-true (type-var-equal-p (first fv) v)))
  (let* ((v1 (fresh-type-var))
         (v2 (fresh-type-var))
         (fn (make-type-arrow-raw :params (list v1) :return v2))
         (fv (type-free-vars fn)))
    (assert-= 2 (length fv))))

;;; Substitution Tests

(deftest substitution-cases
  "zonk: primitive unchanged; bound var replaced; unbound var identity."
  (assert-eq type-int (zonk type-int (make-substitution)))
  (let* ((v (fresh-type-var))
         (result (zonk v (subst-extend v type-int (make-substitution)))))
    (assert-type-equal result type-int))
  (let* ((v (fresh-type-var))
         (result (zonk v (make-substitution))))
    (assert-true (type-var-equal-p result v))))

(deftest substitution-through-function-type
  "Substitution distributes into function type params and return."
  (let* ((v      (fresh-type-var))
         (fn     (make-type-arrow-raw :params (list v) :return v))
         (result (zonk fn (subst-extend v type-int (make-substitution)))))
    (assert-type type-arrow result)
    (assert-type-equal (first (type-arrow-params result)) type-int)
    (assert-type-equal (type-arrow-return result) type-int)))

;;; Normalize Type Variables Tests

(deftest normalize-type-variables-canonical
  "Test that normalize produces canonical variable names."
  (let* ((v1 (fresh-type-var))
         (v2 (fresh-type-var))
         (fn (make-type-arrow-raw
                            :params (list v1)
                            :return v2))
         (normalized (normalize-type-variables fn)))
    (assert-type type-arrow normalized)
    ;; The param and return should be different canonical variables
    (let ((p (first (type-arrow-params normalized)))
          (r (type-arrow-return normalized)))
      (assert-type type-var p)
      (assert-type type-var r)
      (assert-false (type-var-equal-p p r)))))

;;; Phase 3: Bidirectional Type Checking Tests

(deftest bidirectional-checking-cases
  "Bidirectional checking: synthesize infers type-int; check succeeds; check-body verifies last form."
  (let* ((env (type-env-empty))
         (ast (make-ast-int :value 42)))
    (multiple-value-bind (ty _subst) (synthesize ast env)
      (declare (ignore _subst))
      (assert-true (type-equal-p ty type-int)))
    (assert-true (null (check ast type-int env)))
    (assert-true (null (check ast cl-cc/type::+type-unknown+ env))))
  (let* ((env (type-env-empty))
         (ast1 (make-ast-int :value 1))
         (ast2 (make-ast-int :value 2)))
    (assert-true (null (check-body (list ast1 ast2) type-int env)))))

;;; Phase 4: Typeclass Tests

;;; Legacy compatibility tests removed as part of public surface reduction.

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

;;; Legacy effect compatibility tests removed as part of public surface reduction.

(deftest-each effect-row-singleton-cases
  "Effect row singletons: pure has 0 effects; io has 1 IO effect; custom multi has N effects."
  :cases (("pure"   +pure-effect-row+  0 nil)
           ("io"     +io-effect-row+    1 "IO")
           ("custom" (make-type-effect-row :effects (list (cl-cc/type::make-type-effect-op :name 'state :args nil)
                                                          (cl-cc/type::make-type-effect-op :name 'error :args nil))
                                           :row-var nil) 2 nil))
  (row expected-count expected-name)
  (assert-true (type-effect-row-p row))
    (assert-= expected-count (length (type-effect-row-effects row)))
    (when expected-name
      (assert-true (string= expected-name
                            (symbol-name (cl-cc/type::type-effect-op-name (first (type-effect-row-effects row))))))))

(deftest-each effect-row-to-string
  "type-to-string formats effect rows: pure → '{}'; io-row contains 'IO'."
  :cases (("pure" +pure-effect-row+ "{}"  nil)
          ("io"   +io-effect-row+   "IO"  t))
  (row expected substr-p)
  (let ((s (type-to-string row)))
    (if substr-p
        (assert-true (search expected (string-upcase s)))
        (assert-string= expected s))))

;;; Legacy effect compatibility tests removed as part of public surface reduction.

;;; Phase 6: Rank-N Polymorphism Tests

(deftest rankn-forall-creation-and-equality
  "make-type-forall creates well-formed forall types; foralls with distinct vars are not equal."
  (let* ((a  (fresh-type-var 'a))
         (fn (make-type-arrow (list a) a))
         (fa (make-type-forall :var a :body fn)))
    (assert-true (type-forall-p fa))
    (assert-true (type-var-equal-p a (type-forall-var fa)))
    (assert-true (typep (type-forall-body fa) 'type-arrow))
    (let ((s (type-to-string fa)))
      (assert-true (stringp s))
      (assert-true (search "A" (string-upcase s)))))
  (let* ((a  (fresh-type-var 'a))
         (b  (fresh-type-var 'b))
         (fa (make-type-forall :var a :body (make-type-arrow (list a) a)))
         (fb (make-type-forall :var b :body (make-type-arrow (list b) b))))
    (assert-false (type-equal-p fa fb))))
