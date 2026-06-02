;;;; tests/unit/optimize/optimizer-pipeline-pgo-tests.lisp
;;;; Unit tests for optimizer-pipeline.lisp — PGO, ThinLTO, tiered JIT, deopt/OSR
;;;;
;;;; Covers: PGO counter plans, profile templates, ThinLTO import, tiered JIT
;;;;   compilation thresholds, deoptimization materialization, and OSR triggers.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── PGO: hot chain / loop rotation ─────────────────────────────────────────

(deftest optimize-pgo-build-hot-chain-prefers-hottest-successors
  "PGO layout helper builds a greedy hot successor chain."
  (let ((edges (make-hash-table :test #'equal)))
    (setf (gethash (cons :entry :cold) edges) 1
          (gethash (cons :entry :hot) edges) 10
          (gethash (cons :hot :exit) edges) 5
          (gethash (cons :cold :exit) edges) 1)
    (assert-equal '(:entry :hot :exit)
                  (cl-cc/optimize:opt-pgo-build-hot-chain
                   :entry
                   '((:entry . (:cold :hot))
                     (:hot . (:exit))
                     (:cold . (:exit)))
                   edges))))

(deftest optimize-pgo-rotate-loop-places-preferred-exit-at-bottom
  "Loop rotation helper makes the preferred exit the loop-chain bottom."
  (assert-equal '(:latch :header :exit)
                (cl-cc/optimize:opt-pgo-rotate-loop
                 '(:header :exit :latch)
                 :exit)))

;;; ─── ThinLTO module summaries ────────────────────────────────────────────────

(deftest optimize-merge-module-summaries-aggregates-exports-and-counts
  "opt-merge-module-summaries merges module names, deduped exports, and function counts."
  (let* ((a (cl-cc/optimize::make-opt-module-summary
             :module :a :exports '(fa fb) :function-count 2 :type-summaries nil))
         (b (cl-cc/optimize::make-opt-module-summary
             :module :b :exports '(fb fc) :function-count 3 :type-summaries nil))
         (merged (cl-cc/optimize::opt-merge-module-summaries (list a b))))
    (assert-equal '(:a :b) (getf merged :modules))
    (assert-true (member 'fa (getf merged :exports)))
    (assert-true (member 'fb (getf merged :exports)))
    (assert-true (member 'fc (getf merged :exports)))
    (assert-= 5 (getf merged :function-count))))

(deftest optimize-thinlto-import-decision-respects-budget-linkage-and-cycles
  "ThinLTO import helper accepts only small local non-recursive summaries."
  (let ((candidate (cl-cc/optimize:make-opt-function-summary
                    :name 'callee :inst-count 12 :exported-p nil :importable-p t))
        (exported (cl-cc/optimize:make-opt-function-summary
                   :name 'public :inst-count 12 :exported-p t :importable-p t))
        (too-large (cl-cc/optimize:make-opt-function-summary
                    :name 'big :inst-count 200 :exported-p nil :importable-p t)))
    (assert-true (cl-cc/optimize:opt-thinlto-should-import-p
                  candidate '(other) :budget 20))
    (assert-false (cl-cc/optimize:opt-thinlto-should-import-p
                   exported '(other) :budget 20))
    (assert-false (cl-cc/optimize:opt-thinlto-should-import-p
                   too-large '(other) :budget 20))
    (assert-false (cl-cc/optimize:opt-thinlto-should-import-p
                   candidate '(callee) :budget 20))))

;;; ─── Tiered JIT compilation thresholds ──────────────────────────────────────

(deftest optimize-adaptive-compilation-threshold-reacts-to-warmup-pressure-and-failures
  "opt-adaptive-compilation-threshold lowers for warmup and increases for pressure/failures."
  (assert-= 300 (cl-cc/optimize::opt-adaptive-compilation-threshold :base 900 :warmup-p t))
  (assert-= 1800 (cl-cc/optimize::opt-adaptive-compilation-threshold :base 900 :cache-pressure 0.8))
  (assert-= 2700 (cl-cc/optimize::opt-adaptive-compilation-threshold :base 900 :failures 2))
  (assert-= 1800 (cl-cc/optimize::opt-adaptive-compilation-threshold
                  :base 900 :warmup-p t :cache-pressure 0.8 :failures 2)))

(deftest-each optimize-tier-transition-cases
  "opt-tier-transition promotes interpreter→baseline at threshold, baseline→optimized at threshold."
  :cases (("interpreter-below-threshold" (cl-cc/optimize:opt-tier-transition :interpreter 99  :baseline-threshold 100)  :interpreter)
          ("interpreter-at-threshold"    (cl-cc/optimize:opt-tier-transition :interpreter 100 :baseline-threshold 100)  :baseline)
          ("baseline-below-threshold"    (cl-cc/optimize:opt-tier-transition :baseline 999   :optimized-threshold 1000) :baseline)
          ("baseline-at-threshold"       (cl-cc/optimize:opt-tier-transition :baseline 1000  :optimized-threshold 1000) :optimized))
  (result expected)
  (assert-eq expected result))

;;; ─── Deoptimization ──────────────────────────────────────────────────────────

(deftest optimize-materialize-deopt-state-maps-machine-registers-to-vm-registers
  "opt-materialize-deopt-state reconstructs VM register values from machine register snapshot."
  (let* ((frame (cl-cc/optimize::make-opt-deopt-frame
                 :vm-pc 42
                 :register-map '((:rax . :r0) (:rbx . :r1))
                 :inlined-frames nil))
         (machine '((:rax . 11) (:rbx . 22) (:rcx . 33)))
         (state (cl-cc/optimize::opt-materialize-deopt-state frame machine)))
    (assert-equal '((:r0 . 11) (:r1 . 22)) state)))

;;; ─── On-Stack Replacement (OSR) ──────────────────────────────────────────────

(deftest-each optimize-osr-trigger-p-threshold-cases
  "opt-osr-trigger-p fires when hotness meets threshold, stays false when below."
  :cases (("fires-at-lower-threshold" 1000 t)
          ("silent-at-higher-threshold" 2000 nil))
  (threshold expected)
  (let ((osr (cl-cc/optimize::make-opt-osr-point
              :loop-id :L1 :vm-pc 77 :live-registers nil :hotness 1200)))
    (assert-equal expected (not (null (cl-cc/optimize::opt-osr-trigger-p osr :threshold threshold))))))

(deftest optimize-osr-materialize-entry-maps-machine-to-vm-registers
  "opt-osr-materialize-entry reconstructs VM register state at OSR entry."
  (let* ((osr (cl-cc/optimize::make-opt-osr-point
               :loop-id :L2
               :vm-pc 88
               :live-registers '((:rax . :r0) (:r10 . :r7))
               :hotness 1500))
         (machine '((:rax . 3) (:r10 . 9) (:rbx . 99)))
         (state (cl-cc/optimize::opt-osr-materialize-entry osr machine)))
    (assert-equal '((:r0 . 3) (:r7 . 9)) state)))

;;; ─── PGO counter plan and profile template (FR-295) ─────────────────────────

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
