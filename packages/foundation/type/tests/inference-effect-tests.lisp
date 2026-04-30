;;;; packages/foundation/type/tests/inference-effect-tests.lisp — Effect Inference Tests
;;;
;;; Covers effect inference, effect signatures, skolem helpers, bidirectional check,
;;; annotation, condition classes, value restriction, and polymorphic recursion helpers.

(in-package :cl-cc/test)
(in-suite cl-cc-type-serial-suite)

;;; ─── Effect Inference ─────────────────────────────────────────────────────

(deftest-each infer-effects-single-forms
  "infer-effects: setq produces STATE; lambda is pure (no effects)."
  :cases (("setq-state"  '(setq x 42)    "STATE")
          ("lambda-pure" '(lambda (x) x) nil))
  (sexp expected-effect)
  (let* ((ast (cl-cc:lower-sexp-to-ast sexp))
         (row (cl-cc/type:infer-effects ast (type-env-empty))))
    (assert-true (cl-cc/type:type-effect-row-p row))
    (if expected-effect
        (let* ((effects (cl-cc/type:type-effect-row-effects row))
               (names (mapcar #'cl-cc/type:type-effect-op-name effects)))
          (assert-true (member expected-effect names :key #'symbol-name :test #'string=)))
        (assert-null (cl-cc/type:type-effect-row-effects row)))))

(deftest-each infer-effects-multi-form-unions
  "infer-effects unions IO and STATE effects across all sub-forms for if and progn."
  :cases (("if"    '(if (print 1) (setq x 2) 3))
          ("progn" '(progn (print 1) (setq x 2))))
  (sexp)
  (let* ((ast (cl-cc:lower-sexp-to-ast sexp))
         (row (cl-cc/type:infer-effects ast (type-env-empty)))
         (names (mapcar #'cl-cc/type:type-effect-op-name (cl-cc/type:type-effect-row-effects row))))
    (assert-true (member "IO"    names :key #'symbol-name :test #'string=))
    (assert-true (member "STATE" names :key #'symbol-name :test #'string=))))

;;; ─── Effect Signature Registry ────────────────────────────────────────────

(deftest-each infer-effect-signature-dispatch
  "Effect signatures: print→IO, error→ERROR, unknown→pure (nil effects)."
  :cases (("print"   'print "IO")
          ("error"   'error "ERROR")
          ("unknown" 'completely-unknown-fn-xyz nil))
  (fn-name expected-effect-name)
  (let ((row (cl-cc/type:lookup-effect-signature fn-name)))
    (assert-true (cl-cc/type:type-effect-row-p row))
    (if expected-effect-name
        (let ((names (mapcar #'cl-cc/type:type-effect-op-name
                             (cl-cc/type:type-effect-row-effects row))))
          (assert-true (member expected-effect-name names :key #'symbol-name :test #'string=)))
        (assert-null (cl-cc/type:type-effect-row-effects row)))))

(deftest infer-custom-effect-signature-register-and-lookup
  "register-effect-signature stores a row and lookup-effect-signature returns it."
  (let ((custom-row (cl-cc/type:make-type-effect-row
                     :effects (list (cl-cc/type:make-type-effect-op :name 'network :args nil))
                     :row-var nil)))
    (cl-cc/type:register-effect-signature 'http-get-xyz custom-row)
    (let ((result (cl-cc/type:lookup-effect-signature 'http-get-xyz)))
      (assert-eq custom-row result))))

(deftest infer-dict-env-constraint-satisfaction
  "dict-env-extend provides a constraint; missing env signals type-inference-error."
  (let* ((env0 (type-env-empty))
         (env1 (cl-cc/type:dict-env-extend 'local-num type-int '((plus . #'+)) env0))
         (constraint (cl-cc/type:make-type-constraint
                      :class-name 'local-num
                      :type-arg type-int))
         (q (cl-cc/type:make-type-qualified :constraints (list constraint)
                                            :body type-int)))
    (cl-cc/type:check-qualified-constraints q nil env1)
    (assert-signals cl-cc/type:type-inference-error
      (cl-cc/type:check-qualified-constraints q nil env0))))

;;; ─── infer-with-effects ───────────────────────────────────────────────────

;;; ─── check-body-effects ───────────────────────────────────────────────────

(deftest infer-with-effects-returns-triple
  "infer-with-effects returns type, subst, and effect row; (print 42) yields IO effect."
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(print 42))))
    (multiple-value-bind (ty subst effects)
        (cl-cc/type:infer-with-effects ast (type-env-empty))
      (declare (ignore subst))
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name ty))
      (assert-true (cl-cc/type:type-effect-row-p effects))
      (let ((names (mapcar #'cl-cc/type:type-effect-op-name
                           (cl-cc/type:type-effect-row-effects effects))))
        (assert-true (member "IO" names :key #'symbol-name :test #'string=))))))

(deftest infer-check-body-effects-io-row-succeeds
  "check-body-effects: IO row accepts (print 42) without signaling."
  (let ((asts (list (cl-cc:lower-sexp-to-ast '(print 42)))))
    (cl-cc/type:check-body-effects asts cl-cc/type:+io-effect-row+ (type-env-empty))
    (assert-true t)))

(deftest infer-check-body-effects-pure-row-signals-error
  "check-body-effects: pure row rejects (print 42) with type-inference-error."
  (let ((asts (list (cl-cc:lower-sexp-to-ast '(print 42)))))
    (assert-signals cl-cc/type:type-inference-error
      (cl-cc/type:check-body-effects asts cl-cc/type:+pure-effect-row+ (type-env-empty)))))

;;; ─── Skolem Helpers ───────────────────────────────────────────────────────

(deftest infer-skolem-appears-in-type-p
  "skolem-appears-in-type-p returns T when skolem is in the type, NIL when absent."
  (let* ((sk (cl-cc/type:fresh-rigid-var "a"))
         (fn-with (cl-cc/type:make-type-arrow-raw
                    :params (list sk) :return cl-cc/type:type-int))
         (fn-without (cl-cc/type:make-type-arrow-raw
                      :params (list cl-cc/type:type-int) :return cl-cc/type:type-int)))
    (assert-true  (cl-cc/type::skolem-appears-in-type-p sk fn-with))
    (assert-false (cl-cc/type::skolem-appears-in-type-p sk fn-without))))

(deftest infer-skolem-appears-in-subst-p
  "skolem-appears-in-subst-p returns T when skolem is a value in the substitution, NIL otherwise."
  (let* ((sk (cl-cc/type:fresh-rigid-var "a"))
         (v  (cl-cc/type:fresh-type-var 'x)))
    (assert-true  (cl-cc/type::skolem-appears-in-subst-p sk (subst-extend v sk (make-substitution))))
    (assert-false (cl-cc/type::skolem-appears-in-subst-p sk (subst-extend v cl-cc/type:type-int (make-substitution))))))

(deftest infer-check-skolem-escape-signals-error
  "check-skolem-escape signals type-inference-error when a skolem escapes its scope."
  (let* ((sk (cl-cc/type:fresh-rigid-var "a"))
         (v (cl-cc/type:fresh-type-var 'x))
         (s (subst-extend v sk (make-substitution))))
    (assert-signals cl-cc/type:type-inference-error
      (cl-cc/type:check-skolem-escape sk s))))

;;; ─── Bidirectional check: forall ──────────────────────────────────────────

(deftest infer-check-forall-type-skolemizes-and-succeeds
  "check: lambda against forall type skolemizes the type variable and succeeds."
  (reset-type-vars!)
  (let* ((a (cl-cc/type:fresh-type-var 'a))
         (fn-body (cl-cc/type:make-type-arrow-raw :params (list a) :return a))
         (forall-ty (cl-cc/type:make-type-forall :var a :body fn-body))
         (ast (cl-cc:lower-sexp-to-ast '(lambda (x) x)))
         (env (type-env-empty)))
    (let ((subst (cl-cc/type:check ast forall-ty env)))
      (declare (ignore subst))
      (assert-true t))))

(deftest infer-check-type-unknown-returns-nil-subst
  "check: any expression against +type-unknown+ returns nil substitution."
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '42))
        (env (type-env-empty)))
    (let ((subst (cl-cc/type:check ast cl-cc/type::+type-unknown+ env)))
      (assert-null subst))))

;;; ─── annotate-type / defun-without-annotation ───────────────────────────

(deftest infer-annotate-type-returns-int-and-original-ast
  "annotate-type on 42 returns type-int and the original AST node."
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '42)))
    (multiple-value-bind (ty result-ast)
        (cl-cc/type:annotate-type ast (type-env-empty))
      (assert-type-equal ty cl-cc/type:type-int)
      (assert-eq ast result-ast))))

(deftest infer-defun-return-type-is-fixnum
  "defun f (x) (+ x 1) infers a function type with fixnum return."
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(defun f (x) (+ x 1)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-true (cl-cc/type:type-arrow-p ty))
      (assert-type-equal (cl-cc/type:type-arrow-return ty) cl-cc/type:type-int))))

;;; ─── Condition Classes ────────────────────────────────────────────────────

(deftest infer-condition-base-error-has-message
  "type-inference-error is an error with a readable message slot."
  (let ((c (make-condition 'cl-cc/type:type-inference-error :message "test")))
    (assert-true (typep c 'error))
    (assert-equal "test" (cl-cc/type:type-inference-error-message c))))

(deftest infer-condition-unbound-variable-carries-name
  "unbound-variable-error is a type-inference-error with the variable's name."
  (let ((c (make-condition 'cl-cc/type:unbound-variable-error :name 'x)))
    (assert-true (typep c 'cl-cc/type:type-inference-error))
    (assert-eq 'x (cl-cc/type:unbound-variable-error-name c))))

(deftest infer-condition-type-mismatch-carries-expected-and-actual
  "type-mismatch-error carries expected and actual types accessible via slot readers."
  (let ((c (make-condition 'cl-cc/type:type-mismatch-error
                           :expected cl-cc/type:type-int
                           :actual cl-cc/type:type-string)))
    (assert-true (typep c 'cl-cc/type:type-inference-error))
    (assert-eq 'fixnum (cl-cc/type:type-primitive-name
                        (cl-cc/type:type-mismatch-error-expected c)))
    (assert-eq 'string (cl-cc/type:type-primitive-name
                        (cl-cc/type:type-mismatch-error-actual c)))))

;;; ─── FR-1604: Value Restriction ──────────────────────────────────────────

(deftest-each infer-syntactic-value-p
  "syntactic-value-p returns T for value expressions and NIL for non-value expressions."
  :cases (("integer"  '42               t)
          ("quote"    ''hello            t)
          ("lambda"   '(lambda (x) x)   t)
          ("function" '#'car             t)
          ("call"     '(foo 1)           nil)
          ("binop"    '(+ x y)           nil)
          ("if"       '(if t 1 2)        nil)
          ("progn"    '(progn 1 2)       nil))
  (sexp expected)
  (let ((ast (cl-cc:lower-sexp-to-ast sexp)))
    (if expected
        (assert-true  (cl-cc/type:syntactic-value-p ast))
        (assert-false (cl-cc/type:syntactic-value-p ast)))))

(deftest infer-value-restriction-lambda-binding-generalizes
  "Value restriction: let binding a lambda generalizes to polymorphic type; applying at int yields int."
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(let ((id (lambda (x) x))) (id 42)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty cl-cc/type:type-int))))

(deftest infer-value-restriction-call-binding-stays-monomorphic
  "Value restriction: let binding a call result stays monomorphic; result type is int."
  (reset-type-vars!)
  (let* ((fn-ty (cl-cc/type:make-type-arrow-raw
                 :params (list cl-cc/type:type-int)
                 :return cl-cc/type:type-int))
         (env (type-env-extend 'identity (type-to-scheme fn-ty) (type-env-empty)))
         (ast (cl-cc:lower-sexp-to-ast '(let ((x (identity 42))) x))))
    (multiple-value-bind (ty subst) (cl-cc/type:infer ast env)
      (declare (ignore subst))
      (assert-type-equal ty cl-cc/type:type-int))))

;;; ─── FR-004: Polymorphic Recursion ──────────────────────────────────────

(deftest-each infer-poly-recursion-helper-cases
  "%find-fn-type-declaration: finds matching declaration, returns nil on miss or nil name."
  :cases (("found"      '((type (-> fixnum fixnum) length) (optimize (speed 3)))
                         'length  '(-> fixnum fixnum))
          ("miss-name"  '((type fixnum x) (type string y))
                         'length  nil)
          ("nil-name"   '((type fixnum x))
                         nil      nil))
  (decls name expected)
  (let ((spec (cl-cc/type::%find-fn-type-declaration name decls)))
    (if expected
        (assert-equal expected spec)
        (assert-null spec))))

;;; ─── %infer-effects-union ────────────────────────────────────────────────

(deftest infer-effects-union-empty-is-pure
  "%infer-effects-union on empty list returns the pure effect row."
  (reset-type-vars!)
  (let ((result (cl-cc/type::%infer-effects-union nil nil)))
    (assert-true (cl-cc/type::effect-row-subset-p result cl-cc/type::+pure-effect-row+))
    (assert-true (cl-cc/type::effect-row-subset-p cl-cc/type::+pure-effect-row+ result))))

(deftest infer-effects-union-pure-forms-stay-pure
  "%infer-effects-union on a list of pure integer forms returns the pure effect row."
  (reset-type-vars!)
  (let* ((asts   (list (cl-cc:lower-sexp-to-ast '1) (cl-cc:lower-sexp-to-ast '2)))
         (result (cl-cc/type::%infer-effects-union asts nil)))
    (assert-true (cl-cc/type::effect-row-subset-p result cl-cc/type::+pure-effect-row+))))
