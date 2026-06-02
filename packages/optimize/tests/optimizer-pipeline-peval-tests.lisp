;;;; tests/unit/optimize/optimizer-pipeline-peval-tests.lisp
;;;; Unit tests for optimizer-pipeline.lisp — partial evaluation helpers
;;;;
;;;; Covers: FR-209 opt-specialize-constant-args, FR-210 SCCP binding-time
;;;;   analysis, FR-211 specialization plan caching, program-level partial
;;;;   evaluation, and opt-pass-specialize-known-args.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── FR-209 opt-specialize-constant-args ────────────────────────────────────

(deftest-each optimize-specialize-constant-args-cases
  "FR-209 helper substitutes known constants, respects lexical binders/quoted data, and kills stale signatures after setq."
  :cases
  (("basic-substitution"
    'f '(x y)
    '((if (= x 0) 0 (* x y)))
    '((x . 0))
    :check-residual '((if (= 0 0) 0 (* 0 y)))
    :check-signature '((x . 0))
    :check-dynamic '(y))

   ("lexical-binders-and-quoted"
    'f '(x y)
    '((quote x)
      (let ((x y) (z x)) (+ x z y))
      (let* ((z x) (x y)) (+ x z y))
      (lambda (x) (+ x y))
      (setq x y)
      (x y))
    '((x . 0) (y . 7))
    :check-residual '((quote x)
                      (let ((x 7) (z 0)) (+ x z 7))
                      (let* ((z 0) (x 7)) (+ x z 7))
                      (lambda (x) (+ x 7))
                      (setq x 7)
                      (x 7))
    :check-signature nil
    :check-dynamic nil)

   ("kills-signature-after-setq"
    'f '(x y)
    '((setq x y)
      (+ x y)
      (setq x (+ x 1))
      (+ x y)
      (setq x y y x)
      (+ x y))
    '((x . 0) (y . 7))
    :check-residual '((setq x 7)
                      (+ x 7)
                      (setq x (+ x 1))
                      (+ x 7)
                      (setq x 7 y x)
                      (+ x y))
    :check-signature nil
    :check-dynamic nil))
  (label params body static-bindings &key check-residual check-signature check-dynamic)
  (let ((specialization
          (cl-cc/optimize:opt-specialize-constant-args
           label params body static-bindings)))
    (assert-equal label
                  (cl-cc/optimize:opt-partial-spec-original-name specialization))
    (assert-equal check-residual
                  (cl-cc/optimize:opt-partial-spec-residual-body specialization))
    (when check-signature
      (assert-equal check-signature
                    (cl-cc/optimize:opt-partial-spec-static-args specialization)))
    (when check-dynamic
      (assert-equal check-dynamic
                    (cl-cc/optimize:opt-partial-spec-dynamic-args specialization)))))

;;; ─── FR-210 SCCP binding-time analysis ──────────────────────────────────────

(deftest optimize-sccp-analyze-binding-times-classifies-lattice-values
  "FR-210 helper maps SCCP lattice constants to static binding-time entries."
  (let* ((analysis
           (cl-cc/optimize:opt-sccp-analyze-binding-times
            '(x y z)
            `((x . ,(cl-cc/optimize:opt-lattice-constant 42))
              (y . ,(cl-cc/optimize:opt-lattice-overdefined))))))
    (assert-eq :static
               (cl-cc/optimize:opt-binding-time-kind (first analysis)))
    (assert-equal 42
                  (cl-cc/optimize:opt-binding-time-value (first analysis)))
    (assert-eq :dynamic
               (cl-cc/optimize:opt-binding-time-kind (second analysis)))
    (assert-eq :dynamic
               (cl-cc/optimize:opt-binding-time-kind (third analysis)))
    (assert-null (cl-cc/optimize:opt-binding-time-lattice (third analysis)))))

;;; ─── FR-211 specialization plan caching ──────────────────────────────────────

(deftest optimize-build-specialization-plan-reuses-cache-for-constant-signature
  "FR-211 helper requests one clone per callee/signature and reuses cached names."
  (let* ((cache (make-hash-table :test #'equal))
         (first-plan
           (cl-cc/optimize:opt-build-specialization-plan
            'f '(x y) '((x . 3)) :cache cache))
         (second-plan
           (cl-cc/optimize:opt-build-specialization-plan
            'f '(x y) '((x . 3)) :cache cache)))
    (assert-true first-plan)
    (assert-true second-plan)
    (assert-true (cl-cc/optimize:opt-specialization-plan-clone-needed-p first-plan))
    (assert-false (cl-cc/optimize:opt-specialization-plan-cache-hit-p first-plan))
    (assert-false (cl-cc/optimize:opt-specialization-plan-clone-needed-p second-plan))
    (assert-true (cl-cc/optimize:opt-specialization-plan-cache-hit-p second-plan))
    (assert-equal (cl-cc/optimize:opt-specialization-plan-specialized-name first-plan)
                  (cl-cc/optimize:opt-specialization-plan-specialized-name second-plan))
    (assert-equal '((x . 3))
                  (cl-cc/optimize:opt-specialization-plan-signature first-plan))
    (assert-equal '(y)
                  (cl-cc/optimize:opt-specialization-plan-dynamic-args first-plan))))

;;; ─── Program-level partial evaluation ───────────────────────────────────────

(deftest optimize-partial-evaluate-program-propagates-constants-through-call-graph
  "Program-level partial evaluator propagates constants across direct calls."
  (let* ((defs
           '((callee :params (:x :y)
              :body ((+ x y)))
             (caller :params (:a)
              :body ((callee a 7)))))
         (result (cl-cc/optimize:opt-partial-evaluate-program
                  defs
                  :constant-bindings-by-function '((caller . ((:a . 3))))))
         (reports (cl-cc/optimize::opt-partial-program-function-results result))
         (callee-report (cdr (assoc 'callee reports :test #'equal))))
    (assert-true callee-report)
    (assert-true (assoc :y (cl-cc/optimize:opt-partial-eval-signature callee-report) :test #'equal))
    (assert-equal 7 (cdr (assoc :y (cl-cc/optimize:opt-partial-eval-signature callee-report) :test #'equal)))))

(deftest optimize-partial-evaluate-program-uses-offline-bta-to-prune-static-forms
  "Offline BTA marks static forms and exposes dynamic residual body."
  (let* ((report (cl-cc/optimize:opt-partial-evaluate-function
                  'f
                  '(x)
                  '((+ x 1) x)
                  :constant-bindings '((x . 9))))
         (kinds (cl-cc/optimize:opt-partial-eval-form-kinds report))
         (dynamic-body (cl-cc/optimize:opt-partial-eval-dynamic-body report)))
    (assert-true (listp kinds))
    (assert-true (member :static kinds))
    (assert-true (listp dynamic-body))))

;;; ─── opt-pass-specialize-known-args ─────────────────────────────────────────

(deftest opt-pass-specialize-known-args-emits-specialized-clone-and-redirects-call
  "Specialization pass emits clone label/body and rewrites call args to dynamic subset."
  (let* ((func-label "add2")
         (closure (make-vm-closure :dst :r1 :label func-label :params '(:x :y) :captured nil))
         (body (list (make-vm-label :name func-label)
                     (make-vm-add :dst :r9 :lhs :x :rhs :y)
                     (make-vm-ret :reg :r9)))
         (caller (list (make-vm-const :dst :r2 :value 5)
                       (make-vm-call :dst :r3 :func :r1 :args '(:r2 :r4))))
         (optimized (cl-cc/optimize:opt-pass-specialize-known-args
                     (append (list closure) body caller)))
         (has-specialized-label
           (some (lambda (inst)
                   (and (typep inst 'cl-cc/vm::vm-label)
                        (search "__spec__" (cl-cc/vm::vm-name inst))))
                 optimized))
         (rewritten-call
           (find-if (lambda (inst)
                      (and (typep inst 'cl-cc/vm::vm-call)
                           (equal (cl-cc/vm::vm-args inst) '(:r4))))
                    optimized)))
    (assert-true has-specialized-label)
    (assert-true rewritten-call)))
