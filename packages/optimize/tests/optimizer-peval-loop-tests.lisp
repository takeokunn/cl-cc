;;;; tests/unit/optimize/optimizer-peval-loop-tests.lisp
;;;; Unit tests for partial evaluation and loop-level optimization helpers.
;;;;
;;;; Covers: FR-209 constant specialization, FR-210 SCCP binding-times,
;;;;   FR-211 specialization plan cache, FR-295 PGO counter plans,
;;;;   program-level partial evaluation, offline BTA, specialize-known-args pass,
;;;;   FR-523 affine loop analysis, FR-524 loop interchange, FR-525 polyhedral
;;;;   scheduling, FR-526 loop fusion/fission, FR-527 MLGO scoring,
;;;;   FR-528 learned codegen cost.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── FR-209/210/211 Partial Evaluation Helper Layer ────────────────────────

(deftest optimize-specialize-constant-args-builds-residual-body
  "FR-209 helper substitutes known constant parameters into a residual body."
  (let ((specialization
          (cl-cc/optimize:opt-specialize-constant-args
           'f
           '(x y)
           '((if (= x 0) 0 (* x y)))
           '((x . 0)))))
    (assert-equal 'f
                  (cl-cc/optimize:opt-partial-spec-original-name specialization))
    (assert-equal '((x . 0))
                  (cl-cc/optimize:opt-partial-spec-signature specialization))
    (assert-equal '((x . 0))
                  (cl-cc/optimize:opt-partial-spec-static-args specialization))
    (assert-equal '(y)
                  (cl-cc/optimize:opt-partial-spec-dynamic-args specialization))
    (assert-equal '((if (= 0 0) 0 (* 0 y)))
                  (cl-cc/optimize:opt-partial-spec-residual-body specialization))))

(deftest optimize-specialize-constant-args-respects-lexical-binders-and-quoted-data
  "FR-209 helper avoids substituting quoted data or lexically shadowed names."
  (let ((specialization
          (cl-cc/optimize:opt-specialize-constant-args
           'f
           '(x y)
           '((quote x)
             (let ((x y) (z x)) (+ x z y))
             (let* ((z x) (x y)) (+ x z y))
             (lambda (x) (+ x y))
             (setq x y)
             (x y))
           '((x . 0) (y . 7)))))
    (assert-equal '((quote x)
                    (let ((x 7) (z 0)) (+ x z 7))
                    (let* ((z 0) (x 7)) (+ x z 7))
                    (lambda (x) (+ x 7))
                    (setq x 7)
                    (x 7))
                  (cl-cc/optimize:opt-partial-spec-residual-body specialization))))

(deftest optimize-specialize-constant-args-kills-signature-after-setq
  "FR-209 helper invalidates stale constant signatures after setq assignment."
  (let ((specialization
          (cl-cc/optimize:opt-specialize-constant-args
           'f
           '(x y)
           '((setq x y)
             (+ x y)
             (setq x (+ x 1))
             (+ x y)
             (setq x y y x)
             (+ x y))
           '((x . 0) (y . 7)))))
    (assert-equal '((setq x 7)
                    (+ x 7)
                    (setq x (+ x 1))
                    (+ x 7)
                    (setq x 7 y x)
                    (+ x y))
                  (cl-cc/optimize:opt-partial-spec-residual-body specialization))))

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

(deftest optimize-pgo-build-counter-plan-emits-deterministic-bb-and-edge-ids
  "FR-295 helper emits deterministic BB/edge IDs for stable CFG input."
  (let* ((plan (cl-cc/optimize:opt-pgo-build-counter-plan
                :entry
                '((:entry :left :right)
                  (:left :exit)
                  (:right :exit)
                  (:exit))))
         (bb (getf plan :bb-counters))
         (edge (getf plan :edge-counters)))
    (assert-equal '((:entry . 0) (:left . 1) (:exit . 2) (:right . 3)) bb)
    (assert-equal '(((:entry . :left) . 0)
                    ((:entry . :right) . 1)
                    ((:left . :exit) . 2)
                    ((:right . :exit) . 3))
                  edge)
    (assert-= 4 (getf plan :total-bb))
    (assert-= 4 (getf plan :total-edge))))

(deftest optimize-pgo-make-profile-template-zero-initializes-counts
  "FR-295 helper builds zeroed BB/edge counters from a counter plan."
  (let* ((plan (cl-cc/optimize:opt-pgo-build-counter-plan
                :entry
                '((:entry :left :right)
                  (:left :exit)
                  (:right :exit)
                  (:exit))))
         (profile (cl-cc/optimize:opt-pgo-make-profile-template plan)))
    (assert-eq :cl-cc-pgo-v1 (getf profile :magic))
    (assert-equal '((:entry . 0) (:left . 0) (:exit . 0) (:right . 0))
                  (getf profile :bb-counts))
    (assert-equal '(((:entry . :left) . 0)
                    ((:entry . :right) . 0)
                    ((:left . :exit) . 0)
                    ((:right . :exit) . 0))
                  (getf profile :branch-counts))
    (assert-= (getf plan :total-bb) (getf profile :total-bb))
    (assert-= (getf plan :total-edge) (getf profile :total-edge))))

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

;;; ─── FR-523/524/525/526/527/528 Affine/Polyhedral/Loop/ML passes ──────────

(deftest optimize-affine-loop-summary-builds-descriptor
  "FR-523: affine-loop analysis helper returns structured summary metadata."
  (let ((summary (cl-cc/optimize::opt-build-affine-loop-summary
                  :induction-vars '(:i)
                  :bounds '((:i 0 100))
                  :accesses '((:a :i)))))
    (assert-eq :affine-loop-summary (getf summary :kind))
    (assert-equal '(:i) (getf summary :induction-vars))
    (assert-equal '((:i 0 100)) (getf summary :bounds))))

(deftest-each optimize-loop-interchange-plan-safety-gate
  "FR-524: loop interchange plan applies when dependence-safe, is blocked otherwise."
  :cases (("safe-applies"   t   t)
          ("unsafe-blocked" nil nil))
  (dependence-safe-p expected-applied)
  (let ((plan (cl-cc/optimize::opt-loop-interchange-plan
               :loops '(:i :j)
               :cache-locality-score 3
               :dependence-safe-p dependence-safe-p)))
    (assert-equal expected-applied (not (null (getf plan :applied-p))))))

(deftest optimize-pass-loop-interchange-handles-nested-canonical-loop
  "FR-524: safe canonical-loop core ops are interchanged (independent swap)."
  (let* ((program (list (make-vm-const :dst :ri :value 0)
                        (make-vm-const :dst :rlim :value 8)
                        (make-vm-const :dst :rstep :value 1)
                        (make-vm-label :name :l0)
                        (make-vm-lt :dst :rc :lhs :ri :rhs :rlim)
                        (make-vm-jump-zero :reg :rc :label :l1)
                        (make-vm-mul :dst :r7 :lhs :r3 :rhs :r4)
                        (make-vm-move :dst :r8 :src :r5)
                        (make-vm-add :dst :ri :lhs :ri :rhs :rstep)
                        (make-vm-jump :label :l0)
                        (make-vm-label :name :l1)))
         (optimized (cl-cc/optimize::opt-pass-loop-interchange program)))
    (assert-false (equal (mapcar #'instruction->sexp optimized)
                         (mapcar #'instruction->sexp program)))
    (assert-true (typep (nth 6 optimized) 'vm-move))
    (assert-true (typep (nth 7 optimized) 'vm-mul))))

(deftest optimize-pass-loop-interchange-skips-side-effecting-loop
  "FR-524 safety: side-effecting loop bodies are not interchanged."
  (let* ((program (list (make-vm-const :dst :ri :value 0)
                        (make-vm-const :dst :rlim :value 8)
                        (make-vm-const :dst :rstep :value 1)
                        (make-vm-label :name :l0)
                        (make-vm-lt :dst :rc :lhs :ri :rhs :rlim)
                        (make-vm-jump-zero :reg :rc :label :l1)
                        (make-vm-set-global :src :r2 :name 'g)
                        (make-vm-add :dst :ri :lhs :ri :rhs :rstep)
                        (make-vm-jump :label :l0)
                        (make-vm-label :name :l1)))
         (optimized (cl-cc/optimize::opt-pass-loop-interchange program)))
    (assert-equal (mapcar #'instruction->sexp optimized)
                  (mapcar #'instruction->sexp program))))

(deftest optimize-polyhedral-schedule-plan-preserves-objective
  "FR-525: polyhedral schedule helper keeps statement/constraint/objective payloads."
  (let ((plan (cl-cc/optimize::opt-polyhedral-schedule-plan
               :statements '(:s0 :s1)
               :constraints '((:s0-before :s1))
               :objective :throughput-max)))
    (assert-eq :polyhedral-schedule (getf plan :kind))
    (assert-eq :throughput-max (getf plan :objective))
    (assert-equal '(:s0 :s1) (getf plan :statements))))

(deftest-each optimize-loop-fusion-fission-strategy-selection
  "FR-526: fusion/fission selects :fusion for low register pressure, :fission for high."
  :cases (("fusion-low-pressure"   16 :fusion)
          ("fission-high-pressure" 64 :fission))
  (register-pressure expected-strategy)
  (let ((plan (cl-cc/optimize::opt-loop-fusion-fission-plan
               :loops '(:l0 :l1)
               :register-pressure register-pressure
               :instruction-budget 20)))
    (assert-eq expected-strategy (getf plan :strategy))))

(deftest optimize-ml-inline-score-plan-is-deterministic
  "FR-527: MLGO-style scoring helper is deterministic for identical features."
  (let ((a (cl-cc/optimize::opt-ml-inline-score-plan
            :features '(:hot-loop :small-body)
            :model-version "mlgo-v2"))
        (b (cl-cc/optimize::opt-ml-inline-score-plan
            :features '(:hot-loop :small-body)
            :model-version "mlgo-v2")))
    (assert-eq :ml-inline-score (getf a :kind))
    (assert-= (getf a :score) (getf b :score))
    (assert-= 2 (getf a :feature-count))))

(deftest optimize-learned-codegen-cost-plan-is-target-aware
  "FR-528: learned codegen cost helper reflects target-specific base costs."
  (let ((x86 (cl-cc/optimize::opt-learned-codegen-cost-plan
              :opcode-features '(:mul :add)
              :target :x86-64))
        (arm (cl-cc/optimize::opt-learned-codegen-cost-plan
              :opcode-features '(:mul :add)
              :target :aarch64)))
    (assert-eq :learned-codegen-cost (getf x86 :kind))
    (assert-eq :x86-64 (getf x86 :target))
    (assert-eq :aarch64 (getf arm :target))
    (assert-true (/= (getf x86 :predicted-cost)
                     (getf arm :predicted-cost)))))

(deftest optimize-pass-affine-loop-analysis-captures-real-loop-summary
  "FR-523: affine analysis pass extracts summaries from canonical loop instructions."
  (let* ((program (list (make-vm-const :dst :ri :value 0)
                        (make-vm-const :dst :rlim :value 8)
                        (make-vm-const :dst :rstep :value 1)
                        (make-vm-label :name :l0)
                        (make-vm-lt :dst :rc :lhs :ri :rhs :rlim)
                        (make-vm-jump-zero :reg :rc :label :l1)
                        (make-vm-get-global :dst :r2 :name 'g)
                        (make-vm-add :dst :ri :lhs :ri :rhs :rstep)
                        (make-vm-jump :label :l0)
                        (make-vm-label :name :l1)))
         (_ (cl-cc/optimize::opt-pass-affine-loop-analysis program))
         (summaries cl-cc/optimize::*opt-last-affine-loop-summaries*)
         (summary (first summaries)))
    (assert-true (listp summaries))
    (assert-true summary)
    (assert-eq :affine-loop-summary (getf summary :kind))
    (assert-equal '(:ri) (getf summary :induction-vars))
    (assert-true (some (lambda (access)
                         (eq (getf access :kind) :read-global))
                       (getf summary :accesses)))))

(deftest optimize-pass-polyhedral-schedule-reorders-loop-body
  "FR-525: schedule pass reorders sortable body ops inside canonical loop."
  (let* ((program (list (make-vm-const :dst :ri :value 0)
                        (make-vm-const :dst :rlim :value 8)
                        (make-vm-const :dst :rstep :value 1)
                        (make-vm-label :name :l0)
                        (make-vm-lt :dst :rc :lhs :ri :rhs :rlim)
                        (make-vm-jump-zero :reg :rc :label :l1)
                        (make-vm-mul :dst :r7 :lhs :r3 :rhs :r4)
                        (make-vm-move :dst :r8 :src :r5)
                        (make-vm-add :dst :ri :lhs :ri :rhs :rstep)
                        (make-vm-jump :label :l0)
                        (make-vm-label :name :l1)))
         (optimized (cl-cc/optimize::opt-pass-polyhedral-schedule program)))
    (assert-false (equal (mapcar #'instruction->sexp optimized)
                         (mapcar #'instruction->sexp program)))
    (assert-true (typep (nth 6 optimized) 'vm-move))
    (assert-true (typep (nth 7 optimized) 'vm-mul))))

(deftest optimize-pass-loop-fusion-fission-fuses-adjacent-loops
  "FR-526: adjacent compatible pure loops are fused into one canonical loop."
  (let* ((program
           (list (make-vm-const :dst :ri :value 0)
                 (make-vm-const :dst :rj :value 0)
                 (make-vm-const :dst :rlim :value 4)
                 (make-vm-const :dst :rstep :value 1)
                  ;; loop A
                  (make-vm-label :name :la)
                  (make-vm-lt :dst :rc :lhs :ri :rhs :rlim)
                  (make-vm-jump-zero :reg :rc :label :lax)
                  (make-vm-move :dst :r10 :src :r11)
                  (make-vm-add :dst :ri :lhs :ri :rhs :rstep)
                  (make-vm-jump :label :la)
                  (make-vm-label :name :lax)
                  ;; loop B
                  (make-vm-label :name :lb)
                  (make-vm-lt :dst :rc :lhs :rj :rhs :rlim)
                  (make-vm-jump-zero :reg :rc :label :lbx)
                  (make-vm-move :dst :r12 :src :r13)
                  (make-vm-add :dst :rj :lhs :rj :rhs :rstep)
                  (make-vm-jump :label :lb)
                  (make-vm-label :name :lbx)))
         (optimized (cl-cc/optimize::opt-pass-loop-fusion-fission program)))
    (assert-false (equal (mapcar #'instruction->sexp optimized)
                         (mapcar #'instruction->sexp program)))
    (assert-= 1 (count-if (lambda (inst)
                            (and (typep inst 'vm-label)
                                 (eq (cl-cc/vm::vm-name inst) :la)))
                          optimized))
    (assert-= 0 (count-if (lambda (inst)
                            (and (typep inst 'vm-label)
                                 (eq (cl-cc/vm::vm-name inst) :lb)))
                          optimized))))

(deftest optimize-pass-loop-fusion-fission-skips-unsafe-fusion
  "FR-526 safety: fusion is skipped when iteration spaces are not equivalent."
  (let* ((program (list (make-vm-const :dst :ri :value 0)
                        (make-vm-const :dst :rj :value 1) ;; different init
                        (make-vm-const :dst :rlim :value 4)
                        (make-vm-const :dst :rstep :value 1)
                        (make-vm-label :name :la)
                        (make-vm-lt :dst :rc :lhs :ri :rhs :rlim)
                        (make-vm-jump-zero :reg :rc :label :lax)
                        (make-vm-move :dst :r10 :src :r11)
                        (make-vm-add :dst :ri :lhs :ri :rhs :rstep)
                        (make-vm-jump :label :la)
                        (make-vm-label :name :lax)
                        (make-vm-label :name :lb)
                        (make-vm-lt :dst :rc2 :lhs :rj :rhs :rlim)
                        (make-vm-jump-zero :reg :rc2 :label :lbx)
                        (make-vm-move :dst :r12 :src :r13)
                        (make-vm-add :dst :rj :lhs :rj :rhs :rstep)
                        (make-vm-jump :label :lb)
                        (make-vm-label :name :lbx)))
         (optimized (cl-cc/optimize::opt-pass-loop-fusion-fission program)))
    (assert-equal (mapcar #'instruction->sexp optimized)
                  (mapcar #'instruction->sexp program))))

(deftest optimize-pass-loop-fusion-fission-splits-oversized-loop
  "FR-526: oversized pure loops are split into two core regions with a split marker."
  (let* ((core (loop for idx from 0 below 36
                     collect (make-vm-move :dst :r8 :src :r5)))
         (program (append (list (make-vm-const :dst :ri :value 0)
                                (make-vm-const :dst :rlim :value 8)
                                (make-vm-const :dst :rstep :value 1)
                                (make-vm-label :name :l0)
                                (make-vm-lt :dst :rc :lhs :ri :rhs :rlim)
                                (make-vm-jump-zero :reg :rc :label :l1))
                          core
                          (list (make-vm-add :dst :ri :lhs :ri :rhs :rstep)
                                (make-vm-jump :label :l0)
                                (make-vm-label :name :l1))))
         (optimized (cl-cc/optimize::opt-pass-loop-fusion-fission program)))
    (assert-false (equal (mapcar #'instruction->sexp optimized)
                         (mapcar #'instruction->sexp program)))
    (assert-true (some (lambda (inst)
                         (and (typep inst 'vm-label)
                              (search "__SPLIT" (string (cl-cc/vm::vm-name inst)))))
                       optimized))))
