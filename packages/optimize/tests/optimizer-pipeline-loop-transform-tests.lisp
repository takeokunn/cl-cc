;;;; tests/unit/optimize/optimizer-pipeline-loop-transform-tests.lisp
;;;; Unit tests for optimizer-pipeline.lisp — polyhedral and loop transform passes
;;;;
;;;; Covers: FR-523 affine loop analysis, FR-524 loop interchange,
;;;;   FR-525 polyhedral scheduling, FR-526 loop fusion/fission,
;;;;   FR-527 ML inline scoring, FR-528 learned codegen costs.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── FR-523 Affine loop analysis ────────────────────────────────────────────

(deftest optimize-affine-loop-summary-builds-descriptor
  "FR-523: affine-loop analysis helper returns structured summary metadata."
  (let ((summary (cl-cc/optimize::opt-build-affine-loop-summary
                  :induction-vars '(:i)
                  :bounds '((:i 0 100))
                  :accesses '((:a :i)))))
    (assert-eq :affine-loop-summary (getf summary :kind))
    (assert-equal '(:i) (getf summary :induction-vars))
    (assert-equal '((:i 0 100)) (getf summary :bounds))))

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

;;; ─── FR-524 Loop interchange ─────────────────────────────────────────────────

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

;;; ─── FR-525 Polyhedral scheduling ───────────────────────────────────────────

(deftest optimize-polyhedral-schedule-plan-preserves-objective
  "FR-525: polyhedral schedule helper keeps statement/constraint/objective payloads."
  (let ((plan (cl-cc/optimize::opt-polyhedral-schedule-plan
               :statements '(:s0 :s1)
               :constraints '((:s0-before :s1))
               :objective :throughput-max)))
    (assert-eq :polyhedral-schedule (getf plan :kind))
    (assert-eq :throughput-max (getf plan :objective))
    (assert-equal '(:s0 :s1) (getf plan :statements))))

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

;;; ─── FR-526 Loop fusion / fission ───────────────────────────────────────────

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

;;; ─── FR-527 ML inline scoring ────────────────────────────────────────────────

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

;;; ─── FR-528 Learned codegen cost ─────────────────────────────────────────────

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
