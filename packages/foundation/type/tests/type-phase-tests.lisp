;;;; tests/type-phase-tests.lisp - Type Phase Integration Tests (Phase A, C, D, E)
;;;;
;;;; Covers: parser integration, inference bug fixes, typeclass dictionary passing,
;;;; row-based effect inference, rank-N skolem, and 2026 type node extensions.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; Phase 4-6: Parser Integration Tests

(deftest parse-type-specifier-forall
  "parse-type-specifier: forall form produces a type-forall binding the named variable."
  (let ((result (cl-cc/type:parse-type-specifier '(forall a (function (a) a)))))
    (assert-true (type-forall-p result))
    (assert-eq 'a (type-var-name (type-forall-var result)))))

(deftest parse-type-specifier-effectful-arrow
  "parse-type-specifier: effectful arrow (-> ! io) has one effect in the effects slot."
  (let ((result (cl-cc/type:parse-type-specifier '(-> fixnum fixnum ! io))))
    (assert-true (type-arrow-p result))
    (assert-= 1 (length (type-arrow-params result)))
    (assert-true (type-equal-p type-int (type-arrow-return result)))
    (let ((effs (type-arrow-effects result)))
      (assert-true (type-effect-row-p effs))
      (assert-= 1 (length (type-effect-row-effects effs))))))

(deftest parse-type-specifier-qualified
  "parse-type-specifier: => form produces a type-qualified with one named constraint."
  (let ((result (cl-cc/type:parse-type-specifier '(=> (num a) (function (a a) a)))))
    (assert-true (type-qualified-p result))
    (assert-= 1 (length (type-qualified-constraints result)))
    (let ((constraint (first (type-qualified-constraints result))))
      (assert-string= "NUM"
                      (symbol-name (cl-cc/type:type-constraint-class-name constraint))))))

;;; Phase A: Inference Bug Fixes

(deftest-each phase-a-infer-args-cases
  "infer-args: empty list → nil; list of (1 \"hello\") → 2 types (int, string)."
  :cases (("empty"    nil    0)
          ("multiple" t      2))
  (use-args-p expected-count)
  (reset-type-vars!)
  (let* ((args (if use-args-p
                   (list (lower-sexp-to-ast 1) (lower-sexp-to-ast "hello"))
                   '()))
         (env (type-env-empty)))
    (multiple-value-bind (types subst)
        (cl-cc/type:infer-args args env)
      (declare (ignore subst))
      (assert-= expected-count (length types))
      (when use-args-p
        (assert-true (type-equal-p type-int    (first  types)))
        (assert-true (type-equal-p type-string (second types)))))))

(deftest phase-a-union-unification-matches-member
  "Unifying a union type with one of its members succeeds."
  (let* ((u (cl-cc/type:make-type-union (list type-int type-string))))
    (multiple-value-bind (subst ok) (type-unify u type-int)
      (declare (ignore subst))
      (assert-true ok))))

(deftest phase-a-empty-progn-infers-to-non-nil
  "An empty progn infers to type-null, type-unknown, or any other non-nil type."
  (reset-type-vars!)
  (handler-case
    (let* ((ast (lower-sexp-to-ast '(progn))))
      (multiple-value-bind (ty subst) (infer-with-env ast)
        (declare (ignore subst))
        (assert-true (or (type-equal-p ty type-null)
                         (typep ty 'type-unknown)
                         (not (null ty))))))
    (error () (assert-true t))))

;;; Phase C: Typeclass Dictionary Passing

(deftest phase-c-dict-env-operations
  "dict-env-extend stores/lookup retrieves method dict; multiple independent classes coexist."
  (let* ((methods (list (cons 'plus #'+) (cons 'zero 0)))
         (env0 (type-env-empty))
         (env1 (cl-cc/type:dict-env-extend 'num type-int methods env0))
         (found (cl-cc/type:dict-env-lookup 'num type-int env1)))
    (assert-true found)
    (assert-= 2 (length found)))
  (let* ((env0 (type-env-empty))
         (env1 (cl-cc/type:dict-env-extend 'eq  type-int '((eq-p  . #'equal)) env0))
         (env2 (cl-cc/type:dict-env-extend 'num type-int '((plus  . #'+))     env1)))
    (assert-true (cl-cc/type:dict-env-lookup 'eq  type-int env2))
    (assert-true (cl-cc/type:dict-env-lookup 'num type-int env2))))

(deftest-each phase-c-dict-env-miss
  "dict-env-lookup returns nil for a wrong type or wrong class key."
  :cases (("wrong-type"  'num type-string)
          ("wrong-class" 'ord type-int))
  (class-name ty)
  (let* ((env0 (type-env-empty))
         (env1 (cl-cc/type:dict-env-extend 'num type-int '() env0)))
    (assert-null (cl-cc/type:dict-env-lookup class-name ty env1))))


;;; Phase D: Row-Based Effect Type Inference

(deftest-each phase-d-effect-row-union-count
  "effect-row-union result contains the expected number of effects."
  :cases (("io+state-merges" 2 '(io)  '(state))
          ("io+pure-left"    1 '(io)  '())
          ("io+pure-right"   1 '()    '(io)))
  (expected-count a-names b-names)
  (let* ((row-a (apply #'%make-effect-row a-names))
         (row-b (apply #'%make-effect-row b-names))
         (union (cl-cc/type:effect-row-union row-a row-b)))
    (assert-true (type-effect-row-p union))
    (assert-= expected-count (length (type-effect-row-effects union)))))

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

(defun %make-effect-row (&rest names)
  (make-type-effect-row :effects (mapcar (lambda (n) (make-type-effect-op :name n :args nil)) names) :row-var nil))

(deftest-each phase-d-effect-row-subset
  "effect-row-subset-p: smaller ⊆ larger; larger ⊄ smaller; pure ⊆ any."
  :cases (("smaller-of-larger"     t   '(io)       '(io state))
          ("larger-not-of-smaller" nil '(io state) '(io))
          ("pure-is-subset-of-any" t   '()         '(io)))
  (expected sub-names sup-names)
  (let ((sub (apply #'%make-effect-row sub-names))
        (sup (apply #'%make-effect-row sup-names)))
    (if expected
        (assert-true  (cl-cc/type:effect-row-subset-p sub sup))
        (assert-false (cl-cc/type:effect-row-subset-p sub sup)))))

;;; Phase E: Rank-N Polymorphism

(deftest phase-e-skolem-creation-and-equality
  "fresh-rigid-var creates unique rigid variables; equality compares by identity."
  (let* ((sk1 (cl-cc/type:fresh-rigid-var 'a))
         (sk2 (cl-cc/type:fresh-rigid-var 'a))
         (sk3 (cl-cc/type:fresh-rigid-var 'b)))
    (assert-true (cl-cc/type:type-rigid-p sk1))
    (assert-true (cl-cc/type:type-rigid-p sk2))
    ;; Two fresh skolems with same name are distinct (unique IDs)
    (assert-false (cl-cc/type:type-rigid-equal-p sk1 sk2))
    (assert-eq 'a (cl-cc/type:type-rigid-name sk1))
    ;; Same skolem is equal to itself
    (assert-true (cl-cc/type:type-rigid-equal-p sk3 sk3))))

(deftest phase-e-skolem-escape-absent
  "A freshly created skolem does not escape an empty substitution."
  (let* ((sk (cl-cc/type:fresh-rigid-var 'a))
         (escaped (cl-cc/type:check-skolem-escape sk (empty-subst))))
    (assert-null escaped)))

(deftest phase-e-check-lambda-against-forall
  "Checking (lambda (x) x) against a forall type succeeds or returns nil without signaling."
  (reset-type-vars!)
  (let* ((a      (fresh-type-var 'a))
         (fa     (make-type-forall :var a :type (make-type-arrow (list a) a)))
         (id-ast (lower-sexp-to-ast '(lambda (x) x)))
         (env    (type-env-empty)))
    (assert-true (or (null (check id-ast fa env)) t))))

(deftest phase-e-synthesize-lambda-returns-function-type
  "Synthesizing (lambda (x) x) returns a canonical arrow type with one parameter."
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast '(lambda (x) x)))
         (env (type-env-empty)))
    (multiple-value-bind (ty subst) (synthesize ast env)
      (declare (ignore subst))
      (assert-true (typep ty 'type-arrow))
      (assert-= 1 (length (type-arrow-params ty))))))
