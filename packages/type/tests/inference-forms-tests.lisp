;;;; tests/unit/type/inference-forms-tests.lisp — Type Inference AST Form Tests
;;;
;;; Tests for infer() on concrete AST node types: ast-quote, ast-the, typed holes,
;;; ast-setq, ast-block, ast-defun/defvar, ast-function, lambda-param-env,
;;; ast-flet/ast-labels.
;;; Depends on helpers defined in inference-tests.lisp (same package, loads first).

(in-package :cl-cc/test)
(in-suite cl-cc-type-serial-suite)

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

(deftest infer-the-matching-type-is-fixnum
  "(the fixnum 42): annotated type matches inferred type; result is fixnum."
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(the fixnum 42))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-eq 'fixnum (cl-cc/type:type-primitive-name ty)))))

(deftest infer-the-refinement-base-is-fixnum
  "(the (refine fixnum plusp) 42): refinement base resolves to fixnum."
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(the (refine fixnum plusp) 42))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (let ((prim (if (cl-cc/type:type-refinement-p ty)
                      (cl-cc/type:type-refinement-base ty)
                      ty)))
        (assert-eq 'fixnum (cl-cc/type:type-primitive-name prim))))))

(deftest infer-the-type-mismatch-signals-error
  "(the string 42): annotation conflicts with inferred fixnum; signals type-mismatch-error."
  (reset-type-vars!)
  (let ((ast (cl-cc:lower-sexp-to-ast '(the string 42))))
    (assert-signals cl-cc/type:type-mismatch-error
      (infer-with-env ast))))

(deftest infer-typed-hole-signals-error
  "Typed hole _ in expression signals typed-hole-error."
  (reset-type-vars!)
  (let* ((env (type-env-extend 'x (type-to-scheme cl-cc/type:type-int) (type-env-empty)))
         (ast (cl-cc:lower-sexp-to-ast '(+ x _))))
    (assert-signals cl-cc/type::typed-hole-error
      (cl-cc/type:infer ast env))))

(deftest infer-typed-hole-message-names-in-scope-vars
  "Typed hole error message includes 'Available: X ::' listing in-scope bindings."
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
          (assert-true (cl-cc/type:type-unknown-p ty))))))

;;; ─── %make-lambda-param-env ────────────────────────────────────────────────

(deftest infer-make-lambda-param-env-empty-params
  "%make-lambda-param-env with nil params returns empty type list and the same unextended env."
  (reset-type-vars!)
  (let ((empty-env (cl-cc/type:type-env-empty)))
    (multiple-value-bind (types body-env)
        (cl-cc/type::%make-lambda-param-env nil empty-env)
      (assert-null types)
      (assert-equal empty-env body-env))))

(deftest infer-make-lambda-param-env-two-params
  "%make-lambda-param-env with (x y) returns two distinct type vars and extended env with x bound."
  (reset-type-vars!)
  (multiple-value-bind (types body-env)
      (cl-cc/type::%make-lambda-param-env '(x y) (cl-cc/type:type-env-empty))
    (assert-= 2 (length types))
    (assert-true (cl-cc/type:type-var-p (first types)))
    (assert-true (cl-cc/type:type-var-p (second types)))
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

;;; ─── Advanced FR inference integration ────────────────────────────────────

(deftest advanced-infer-spawn-enforces-send-and-returns-future
  "FR-1502/2201: infer-call rejects non-Send spawn payloads and returns Future for valid calls."
  (reset-type-vars!)
  (let ((valid (cl-cc:lower-sexp-to-ast '(spawn 1)))
        (invalid (cl-cc:lower-sexp-to-ast '(spawn '(1 2)))))
    (multiple-value-bind (ty subst) (infer-with-env valid)
      (declare (ignore subst))
      (assert-true (cl-cc/type:type-advanced-p ty))
      (assert-string= "FR-2201" (cl-cc/type:type-advanced-feature-id ty)))
    (assert-signals cl-cc/type:type-inference-error
      (infer-with-env invalid))))

(deftest advanced-infer-shared-ref-enforces-sync
  "FR-1502: shared references reject function closures because function values are not Sync."
  (reset-type-vars!)
  (assert-signals cl-cc/type:type-inference-error
    (infer-with-env (cl-cc:lower-sexp-to-ast '(shared-ref (lambda (x) x))))))

(deftest advanced-infer-ffi-interface-smt-plugin-and-synthesis-validate-descriptors
  "FR-2103/2405/2406/3002/3003 validators run from infer-call, not only parse-time metadata."
  (flet ((infer-ok (form)
            (multiple-value-bind (ty subst) (infer-with-env (cl-cc:lower-sexp-to-ast form))
              (declare (ignore subst))
              (assert-true ty))))
    (reset-type-vars!)
    (multiple-value-bind (ty subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(foreign-call '(foreign strlen (c-string) c-int) "abc")))
      (declare (ignore subst))
      (assert-type-equal ty cl-cc/type:type-int))
    (assert-signals cl-cc/type:type-inference-error
      (infer-with-env (cl-cc:lower-sexp-to-ast '(foreign-call '(foreign strlen (c-string) c-int) 1))))

    (infer-ok '(load-type-interface 'user-module '((lookup (function (fixnum) fixnum)) save) '"sha256:abc"))
    (multiple-value-bind (imported-ty imported-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast 'lookup))
      (declare (ignore imported-subst))
      (assert-true (cl-cc/type:type-arrow-p imported-ty)))
    (assert-signals cl-cc/type:type-inference-error
      (infer-with-env (cl-cc:lower-sexp-to-ast '(load-type-interface 'user-module '(lookup lookup) '"sha256:abc"))))

    (infer-ok '(smt-assert '(< x 3) 'z3 'lia))
    (assert-signals cl-cc/type:type-inference-error
      (infer-with-env (cl-cc:lower-sexp-to-ast '(smt-assert '(< x 3) 'unknown 'lia))))

    (infer-ok '(run-type-plugin 'nat-normalise 'solve))
    (assert-signals cl-cc/type:type-inference-error
      (infer-with-env (cl-cc:lower-sexp-to-ast '(run-type-plugin 'nat-normalise 'emit))))

    (multiple-value-bind (synth-ty synth-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(synthesize-program '(-> integer integer) 'enumerative 8)))
      (declare (ignore synth-subst))
      (assert-true (cl-cc/type:type-arrow-p synth-ty)))
    (assert-signals cl-cc/type:type-inference-error
      (infer-with-env (cl-cc:lower-sexp-to-ast '(synthesize-program '(-> integer integer) 'enumerative 0))))))

(deftest advanced-infer-mapped-and-conditional-type-calls-run-contracts
  "FR-3301/3302 mapped and conditional type evaluation calls reuse advanced contracts during inference."
  (flet ((infer-ok (form)
            (multiple-value-bind (ty subst) (infer-with-env (cl-cc:lower-sexp-to-ast form))
              (declare (ignore subst))
              (assert-true ty))))
    (reset-type-vars!)
    (multiple-value-bind (mapped-ty mapped-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(apply-mapped-type 'fixnum 'optional)))
      (declare (ignore mapped-subst))
      (assert-true (cl-cc/type:type-union-p mapped-ty))
      (assert-true (some (lambda (member) (cl-cc/type:type-equal-p member cl-cc/type:type-null))
                         (cl-cc/type:type-union-types mapped-ty))))
    (assert-signals cl-cc/type:type-inference-error
      (infer-with-env (cl-cc:lower-sexp-to-ast '(apply-mapped-type '(list fixnum) 'mysterious))))

    (multiple-value-bind (conditional-ty conditional-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(apply-conditional-type '(list fixnum) 'list 'item 'item 'null)))
      (declare (ignore conditional-subst))
      (assert-type-equal conditional-ty cl-cc/type:type-int))
    (multiple-value-bind (else-ty else-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(apply-conditional-type 'fixnum 'list 'item 'item 'null)))
      (declare (ignore else-subst))
      (assert-type-equal else-ty cl-cc/type:type-null))
    (assert-signals cl-cc/type:type-inference-error
      (infer-with-env (cl-cc:lower-sexp-to-ast '(apply-conditional-type '(list fixnum) 'list 'item 'item 'item))))))

(deftest advanced-infer-registries-dispatch-custom-hooks
  "FR-2406/3002/3003 registry hooks are actually invoked from advanced inference calls."
  (let ((smt-called nil)
        (plugin-called nil)
        (synthesis-called nil))
    (cl-cc/type:register-smt-solver 'unit-solver
                                    (lambda (constraint theory)
                                      (setf smt-called (list constraint theory))
                                      (list :status :sat :counterexample :none)))
    (cl-cc/type:register-type-checker-plugin 'unit-plugin 'solve
                                             (lambda (ast arg-types env)
                                               (declare (ignore ast arg-types env))
                                               (setf plugin-called t)
                                               (list :status :ok :type cl-cc/type:type-string)))
    (cl-cc/type:register-type-synthesis-strategy 'unit-search
                                                 (lambda (signature fuel)
                                                   (setf synthesis-called (list signature fuel))
                                                   (list :status :candidate :signature signature)))
    (multiple-value-bind (smt-ty smt-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(smt-assert '(< x 3) 'unit-solver 'lia)))
      (declare (ignore smt-subst))
      (assert-type-equal smt-ty cl-cc/type:type-bool))
    (multiple-value-bind (plugin-ty plugin-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(run-type-plugin 'unit-plugin 'solve)))
      (declare (ignore plugin-subst))
      (assert-type-equal plugin-ty cl-cc/type:type-string))
    (multiple-value-bind (synth-ty synth-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(synthesize-program 'fixnum 'unit-search 3)))
      (declare (ignore synth-subst))
      (assert-type-equal synth-ty cl-cc/type:type-int))
    (assert-true smt-called)
    (assert-true plugin-called)
    (assert-true synthesis-called)))

(deftest advanced-infer-constructor-policies-cover-channels-actors-stm-coroutines-simd-and-routing
  "FR-2202/2203/2204/2205/2206/3305 constructor helpers are intercepted by advanced call policies."
  (reset-type-vars!)
  (flet ((infer-type (form)
           (multiple-value-bind (ty subst) (infer-with-env (cl-cc:lower-sexp-to-ast form))
             (declare (ignore subst))
             ty)))
    (let ((channel-ty (infer-type '(make-typed-channel 'fixnum)))
          (buffered-ty (infer-type '(make-buffered-channel 'fixnum 4)))
          (actor-ty (infer-type '(make-actor-ref 'fixnum)))
          (stm-ty (infer-type '(make-tvar 'fixnum 1)))
          (generator-ty (infer-type '(make-generator-type 'fixnum 'string)))
          (coroutine-ty (infer-type '(make-coroutine-type 'fixnum 'string 'integer)))
          (simd-ty (infer-type '(make-simd-type 'fixnum 4))))
      (assert-true (cl-cc/type:type-advanced-p channel-ty))
      (assert-true (cl-cc/type:type-advanced-p buffered-ty))
      (assert-string= "FR-2202" (cl-cc/type:type-advanced-feature-id channel-ty))
      (assert-string= "FR-2202" (cl-cc/type:type-advanced-feature-id buffered-ty))
      (assert-string= "FR-2203" (cl-cc/type:type-advanced-feature-id actor-ty))
      (assert-string= "FR-2204" (cl-cc/type:type-advanced-feature-id stm-ty))
      (assert-string= "FR-2205" (cl-cc/type:type-advanced-feature-id generator-ty))
      (assert-string= "FR-2205" (cl-cc/type:type-advanced-feature-id coroutine-ty))
      (assert-string= "FR-2206" (cl-cc/type:type-advanced-feature-id simd-ty)))
    (multiple-value-bind (api-ty api-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(make-api-type 'get '"/users/{id}" '((id integer)) 'user)))
      (declare (ignore api-subst))
      (assert-true (cl-cc/type:type-advanced-p api-ty))
      (assert-string= "FR-3305" (cl-cc/type:type-advanced-feature-id api-ty)))
    (assert-signals cl-cc/type:type-inference-error
      (infer-with-env (cl-cc:lower-sexp-to-ast '(make-buffered-channel 'fixnum))))
    (assert-signals cl-cc/type:type-inference-error
      (infer-with-env (cl-cc:lower-sexp-to-ast '(make-typed-channel))))
    (assert-signals cl-cc/type:type-inference-error
      (infer-with-env (cl-cc:lower-sexp-to-ast '(make-actor-ref))))
    (assert-signals cl-cc/type:type-inference-error
      (infer-with-env (cl-cc:lower-sexp-to-ast '(make-tvar 'fixnum))))
    (assert-signals cl-cc/type:type-inference-error
      (infer-with-env (cl-cc:lower-sexp-to-ast '(make-generator-type 'fixnum))))
    (assert-signals cl-cc/type:type-inference-error
      (infer-with-env (cl-cc:lower-sexp-to-ast '(make-coroutine-type 'fixnum 'string))))
    (assert-signals cl-cc/type:type-inference-error
      (infer-with-env (cl-cc:lower-sexp-to-ast '(make-simd-type 'fixnum))))
    (multiple-value-bind (api2-ty api2-subst)
        (infer-with-env (cl-cc:lower-sexp-to-ast '(make-api-type 'get '"/users/{id}" '((id fixnum)) 'fixnum)))
      (declare (ignore api2-subst))
      (assert-true (cl-cc/type:type-advanced-p api2-ty))
      (assert-string= "FR-3305" (cl-cc/type:type-advanced-feature-id api2-ty)))
    (assert-signals cl-cc/type:type-inference-error
      (infer-with-env (cl-cc:lower-sexp-to-ast '(make-api-type 'get '"/users" '()))))))
