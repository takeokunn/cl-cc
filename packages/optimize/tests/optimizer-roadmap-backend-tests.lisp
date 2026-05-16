;;;; tests/unit/optimize/optimizer-roadmap-backend-tests.lisp
;;;; Unit tests for optimizer-pipeline-roadmap.lisp — runtime/support helpers
;;;; and docs/optimize-backend.md evidence
;;;;
;;;; Covers: IC, lattice, profile, deopt, shape, and tiering helpers;
;;;;   optimize-backend-roadmap doc parsing and evidence coverage.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest optimize-roadmap-runtime-helpers-have-concrete-behavior
  "Roadmap helper APIs maintain concrete PIC, profile, deopt, shape, and tiering state."
  (let ((site (cl-cc/optimize::make-opt-ic-site :max-polymorphic-entries 2)))
    (assert-eq :uninitialized (cl-cc/optimize::opt-ic-site-state site))
    (cl-cc/optimize::opt-ic-transition site :shape-a :target-a)
    (assert-eq :monomorphic (cl-cc/optimize::opt-ic-site-state site))
    (cl-cc/optimize::opt-ic-transition site :shape-b :target-b)
    (assert-eq :polymorphic (cl-cc/optimize::opt-ic-site-state site))
    (cl-cc/optimize::opt-ic-transition site :shape-c :target-c)
    (assert-eq :megamorphic (cl-cc/optimize::opt-ic-site-state site))
    (assert-= 3 (cl-cc/optimize::opt-ic-site-misses site))
    (assert-= 3 (length (cl-cc/optimize::opt-ic-site-megamorphic-fallback site))))
  (let ((spec-log (cl-cc/optimize::make-opt-speculation-log :threshold 2)))
    (assert-false (cl-cc/optimize::opt-speculation-failed-p spec-log :site :guard))
    (assert-= 1 (cl-cc/optimize::opt-record-speculation-failure spec-log :site :guard))
    (assert-false (cl-cc/optimize::opt-speculation-failed-p spec-log :site :guard))
    (assert-= 2 (cl-cc/optimize::opt-record-speculation-failure spec-log :site :guard))
    (assert-true (cl-cc/optimize::opt-speculation-failed-p spec-log :site :guard)))
  (let ((profile (cl-cc/optimize::make-opt-profile-data)))
    (assert-= 3 (cl-cc/optimize::opt-profile-record-edge profile :entry :exit 3))
    (assert-= 2 (cl-cc/optimize::opt-profile-record-value profile :ic-site 42 2))
    (assert-= 4 (cl-cc/optimize::opt-profile-record-call-chain profile '(:main :callee) 4))
    (assert-equal (cons 2 64)
                  (cl-cc/optimize::opt-profile-record-allocation profile :alloc-site 64 2)))
  (let* ((frame (cl-cc/optimize::make-opt-deopt-frame
                 :vm-pc 7
                 :register-map '((:rax . :r0) (:rbx . :r1))))
         (state (cl-cc/optimize::opt-materialize-deopt-state
                 frame
                 '((:rax . 10) (:rbx . 20)))))
    (assert-equal '((:r0 . 10) (:r1 . 20)) state))
  (let ((shape (cl-cc/optimize::make-opt-shape-descriptor-for-slots
                9
                '(:class :slots))))
    (assert-= 0 (cl-cc/optimize::opt-shape-slot-offset shape :class))
    (assert-= 1 (cl-cc/optimize::opt-shape-slot-offset shape :slots))
    (assert-null (cl-cc/optimize::opt-shape-slot-offset shape :missing)))
  (assert-= 120
            (cl-cc/optimize::opt-adaptive-compilation-threshold
             :base 90
             :warmup-p t
             :cache-pressure 0.7
             :failures 1)))

(deftest optimize-roadmap-support-helpers-have-conservative-behavior
  "Roadmap support helpers expose conservative lattice, summary, allocation, stack-map, guard, cache, module, and Sea-of-Nodes behavior."
  (let* ((bottom (cl-cc/optimize::opt-lattice-bottom))
         (seven-a (cl-cc/optimize::opt-lattice-constant 7))
         (seven-b (cl-cc/optimize::opt-lattice-constant 7))
         (eight (cl-cc/optimize::opt-lattice-constant 8))
         (overdefined (cl-cc/optimize::opt-lattice-overdefined)))
    (assert-eq :constant
               (cl-cc/optimize::opt-lattice-value-kind
                (cl-cc/optimize::opt-lattice-meet bottom seven-a)))
    (assert-= 7
              (cl-cc/optimize::opt-lattice-value-value
               (cl-cc/optimize::opt-lattice-meet seven-a seven-b)))
    (assert-eq :overdefined
               (cl-cc/optimize::opt-lattice-value-kind
                (cl-cc/optimize::opt-lattice-meet seven-a eight)))
    (assert-eq :overdefined
               (cl-cc/optimize::opt-lattice-value-kind
                (cl-cc/optimize::opt-lattice-meet seven-a overdefined))))
  (let ((pure-summary (cl-cc/optimize::make-opt-function-summary
                       :name 'pure-helper
                       :pure-p t
                       :effects nil))
        (effectful-summary (cl-cc/optimize::make-opt-function-summary
                            :name 'effectful-helper
                            :pure-p t
                            :effects '(:heap-write))))
    (assert-true (cl-cc/optimize::opt-function-summary-safe-to-inline-p pure-summary))
    (assert-false (cl-cc/optimize::opt-function-summary-safe-to-inline-p effectful-summary)))
  (let* ((pool (cl-cc/optimize::make-opt-slab-pool :object-size 2))
         (first-object (cl-cc/optimize::opt-slab-allocate pool)))
    (assert-equal '(:slab-object 2 1) first-object)
    (cl-cc/optimize::opt-slab-free pool first-object)
    (assert-eq first-object (cl-cc/optimize::opt-slab-allocate pool)))
  (let ((region (cl-cc/optimize::make-opt-bump-region :limit 8)))
    (assert-= 0 (cl-cc/optimize::opt-bump-allocate region 3))
    (assert-= 3 (cl-cc/optimize::opt-bump-mark region))
    (assert-= 4 (cl-cc/optimize::opt-bump-allocate region 2 :alignment 4))
    (assert-null (cl-cc/optimize::opt-bump-allocate region 4))
    (cl-cc/optimize::opt-bump-reset region)
    (assert-= 3 (cl-cc/optimize::opt-bump-region-cursor region)))
  (let ((stack-map (cl-cc/optimize::make-opt-stack-map :pc 42 :roots '(:r0 :r2))))
    (assert-true (cl-cc/optimize::opt-stack-map-live-root-p stack-map :r0))
    (assert-false (cl-cc/optimize::opt-stack-map-live-root-p stack-map :r1)))
  (let ((guard (cl-cc/optimize::make-opt-guard-state :executions 9)))
    (assert-eq :tag-bit-test
               (cl-cc/optimize::opt-guard-record guard t))
    (assert-eq :full-type-check
               (cl-cc/optimize::opt-guard-record guard nil)))
  (let* ((cold (cl-cc/optimize::make-opt-jit-cache-entry :id :cold :size 8 :warmth 1))
         (warm (cl-cc/optimize::make-opt-jit-cache-entry :id :warm :size 8 :warmth 9))
         (evicted (cl-cc/optimize::opt-jit-cache-select-eviction
                   (list warm cold)
                   :current-size 90
                   :max-size 100)))
    (assert-eq cold evicted))
  (let ((summary (cl-cc/optimize::opt-merge-module-summaries
                  (list (cl-cc/optimize::make-opt-module-summary
                         :module :a
                         :exports '(foo)
                         :function-count 2)
                        (cl-cc/optimize::make-opt-module-summary
                         :module :b
                         :exports '(foo bar)
                         :function-count 3)))))
    (assert-equal '(:a :b) (getf summary :modules))
    (assert-= 5 (getf summary :function-count))
    (assert-true (member 'foo (getf summary :exports)))
    (assert-true (member 'bar (getf summary :exports))))
  (assert-true
   (cl-cc/optimize::opt-sea-node-schedulable-p
    (cl-cc/optimize::make-opt-sea-node :id :n1 :op :add :controls '(:entry))))
  (assert-false
   (cl-cc/optimize::opt-sea-node-schedulable-p
      (cl-cc/optimize::make-opt-sea-node :id :n2 :controls '(:entry)))))

(deftest optimizer-roadmap-value-profiling-top-k-and-range-behavior
  "FR-261 keeps a bounded per-site Top-K histogram while preserving numeric range feedback."
  (let ((profile (cl-cc/optimize:make-opt-profile-data :value-limit 2)))
    (assert-= 2 (cl-cc/optimize:opt-profile-record-value profile :site 10 2))
    (assert-= 3 (cl-cc/optimize:opt-profile-record-value profile :site 20 3))
    (assert-= 1 (cl-cc/optimize:opt-profile-record-value profile :site 30 1))
    (assert-equal '((20 . 3) (10 . 2))
                  (cl-cc/optimize:opt-profile-top-values profile :site))
    (assert-equal '((20 . 3))
                  (cl-cc/optimize:opt-profile-top-values profile :site 1))
    (assert-equal '(10 . 30)
                  (cl-cc/optimize:opt-profile-value-range profile :site))))

(deftest optimizer-roadmap-speculation-log-gating-and-persistence-behavior
  "FR-283 exposes a process-global speculation log with gating and .prof persistence helpers."
  (let* ((cl-cc/optimize:*opt-speculation-log*
           (cl-cc/optimize:make-opt-speculation-log :threshold 2))
         (temp-path (merge-pathnames
                     (make-pathname :name (format nil "opt-spec-log-~D-~D"
                                                  (get-universal-time)
                                                  (random 1000000))
                                    :type "prof")
                     (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (assert-true (cl-cc/optimize:opt-speculation-allowed-p :site :guard))
           (assert-= 1
                     (cl-cc/optimize:opt-record-speculation-failure
                      cl-cc/optimize:*opt-speculation-log*
                      :site
                      :guard))
           (assert-true (cl-cc/optimize:opt-speculation-allowed-p :site :guard))
           (assert-= 2
                     (cl-cc/optimize:opt-record-speculation-failure
                      cl-cc/optimize:*opt-speculation-log*
                      :site
                      :guard))
           (assert-false (cl-cc/optimize:opt-speculation-allowed-p :site :guard))
           (cl-cc/optimize:opt-save-speculation-log temp-path)
           (assert-eq cl-cc/optimize:*opt-speculation-log*
                      (cl-cc/optimize:opt-clear-speculation-log))
           (assert-true (cl-cc/optimize:opt-speculation-allowed-p :site :guard))
           (setf (cl-cc/optimize::opt-spec-log-threshold cl-cc/optimize:*opt-speculation-log*) 99)
           (cl-cc/optimize:opt-load-speculation-log temp-path)
           (assert-= 2
                     (cl-cc/optimize::opt-spec-log-threshold
                      cl-cc/optimize:*opt-speculation-log*))
           (assert-true
            (cl-cc/optimize:opt-speculation-failed-p
             cl-cc/optimize:*opt-speculation-log*
             :site
             :guard))
           (assert-false (cl-cc/optimize:opt-speculation-allowed-p :site :guard)))
      (when (probe-file temp-path)
        (delete-file temp-path)))))


(defun %optimize-backend-doc-content ()
  "Return the current optimize-backend roadmap document text."
  (%doc-content #P"docs/optimize-backend.md"))

(defun %optimize-backend-doc-completed-heading-contradictions ()
  "Return ✅ optimize-backend FR headings whose own section says implementation is absent."
  (%doc-completed-heading-contradictions
   #P"docs/optimize-backend.md"
   (list "未実装" "未着手" "未対応" "未完"
         "未統合" "未接続" "欠落" "未定義" "不可能"
         "回転命令なし" "検出なし" "エミッションなし" "分岐なし"
         "分解なし" "ガードなし" "ABI 固定" "全 caller-saved")
   :reset-on-section-boundary t))

(defun %optimize-backend-evidence-status-for-feature (feature)
  "Return expected evidence status for optimize-backend FEATURE."
  (let ((status (cl-cc/optimize::opt-roadmap-feature-status feature)))
    (case status
      (:unknown :planned)
      (otherwise status))))

(defun %optimize-backend-test-anchor-name= (expected actual)
  "Return T when EXPECTED and ACTUAL name the same test anchor."
  (and (symbolp expected)
       (symbolp actual)
       (string= (symbol-name expected)
                (symbol-name actual))))

(deftest optimize-backend-roadmap-completed-headings-avoid-incomplete-language
  "FRs marked ✅ in optimize-backend.md must not explicitly say they are unimplemented."
  (assert-null (%optimize-backend-doc-completed-heading-contradictions)))

(deftest optimize-backend-roadmap-evidence-covers-doc-fr-list
  "Every docs/optimize-backend.md FR id has status-aware audit evidence."
  (let* ((features (cl-cc/optimize:optimize-backend-roadmap-doc-features))
         (ids (mapcar #'cl-cc/optimize::opt-roadmap-feature-id features))
         (table (cl-cc/optimize:optimize-backend-roadmap-register-doc-evidence))
         (implemented 0)
         (not-implemented 0)
         (profiles nil))
    (assert-= 232 (length ids))
    (assert-= (length ids) (hash-table-count table))
    (dolist (feature features)
      (let* ((feature-id (cl-cc/optimize::opt-roadmap-feature-id feature))
             (doc-status (cl-cc/optimize::opt-roadmap-feature-status feature))
             (evidence-status (%optimize-backend-evidence-status-for-feature feature))
             (evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence feature-id)))
        (assert-false (search ":" feature-id))
        (assert-true (member doc-status '(:implemented :partial :planned :unknown)))
        (assert-true evidence)
        (assert-equal feature-id
                      (cl-cc/optimize::opt-roadmap-evidence-feature-id evidence))
        (assert-eq evidence-status
                   (cl-cc/optimize::opt-roadmap-evidence-status evidence))
        (assert-true
         (member "docs/optimize-backend.md"
                 (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                 :test #'string=))
        (assert-true
         (cl-cc/optimize::optimize-roadmap-evidence-well-formed-p evidence))
        (push (cl-cc/optimize::opt-roadmap-evidence-modules evidence) profiles)
        (if (eq evidence-status :implemented)
            (progn
              (incf implemented)
              (assert-true
               (cl-cc/optimize:optimize-backend-roadmap-implementation-evidence-complete-p
                evidence)))
            (progn
              (incf not-implemented)
              (assert-false
               (cl-cc/optimize:optimize-backend-roadmap-implementation-evidence-complete-p
                evidence))))))
    (assert-true (> implemented 0))
    (assert-true (>= not-implemented 0))
    (assert-true (<= implemented (length ids)))
    (assert-true (> (length (remove-duplicates profiles :test #'equal)) 5))))

(deftest optimize-backend-roadmap-status-summary-counts-headings
  "Backend roadmap status summary reports a consistent heading partition."
  (let* ((summary (cl-cc/optimize:optimize-backend-roadmap-status-summary))
         (total (getf summary :total 0))
         (implemented (getf summary :implemented 0))
         (partial (getf summary :partial 0))
         (planned (getf summary :planned 0))
         (unknown (getf summary :unknown 0)))
    (assert-= 232 total)
    (assert-= total (+ implemented partial planned unknown))
    (assert-true (>= partial 0))
    (assert-true (>= unknown 0))))

(deftest optimize-backend-roadmap-all-fr-complete-gate-is-strict
  "Completion gate reflects all-✅ state exactly."
  (assert-true (cl-cc/optimize:optimize-backend-roadmap-all-fr-complete-p)))

(deftest optimize-backend-roadmap-fr-ids-by-status-partitions-document
  "Status-filtered FR ID lists partition the optimize-backend roadmap exactly once."
  (let* ((implemented (cl-cc/optimize:optimize-backend-roadmap-fr-ids-by-status :implemented))
         (partial (cl-cc/optimize:optimize-backend-roadmap-fr-ids-by-status :partial))
         (planned (cl-cc/optimize:optimize-backend-roadmap-fr-ids-by-status :planned))
         (unknown (cl-cc/optimize:optimize-backend-roadmap-fr-ids-by-status :unknown))
         (all (append implemented partial planned unknown))
         (all-doc (cl-cc/optimize:optimize-backend-roadmap-doc-fr-ids)))
    (assert-= (length all-doc) (length all))
    (assert-= (length all-doc) (length (remove-duplicates all :test #'string=)))
    (assert-true (>= (length partial) 0))
    (assert-true (>= (length unknown) 0))
    (assert-true (every (lambda (id) (member id all-doc :test #'string=)) all))))

(deftest optimize-backend-roadmap-analysis-evidence-is-loaded
  "Representative backend FR clusters expose status-aware audit anchors."
  (let ((complete 0)
        (open 0))
    (dolist (feature-id '("FR-007" "FR-116" "FR-209" "FR-282" "FR-370" "FR-502"))
      (let* ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence feature-id))
             (status (and evidence
                          (cl-cc/optimize::opt-roadmap-evidence-status evidence))))
        (assert-true evidence)
        (assert-true (member status '(:implemented :partial :planned)))
        (assert-true
         (cl-cc/optimize::optimize-roadmap-evidence-well-formed-p evidence))
        (if (eq status :implemented)
            (incf complete)
            (progn
              (incf open)
              (assert-false
               (cl-cc/optimize:optimize-backend-roadmap-implementation-evidence-complete-p
                evidence))))))
     (assert-= 6 (+ complete open))))

(deftest optimize-backend-roadmap-promoted-existing-frs-have-specific-evidence
  "Backend FRs promoted from existing implementation use specific evidence anchors."
  (dolist (case '(("FR-014" :implemented
                    "packages/optimize/src/optimizer-memory-passes.lisp"
                   cl-cc/optimize::opt-pass-cons-slot-forward
                   cl-cc/optimize::cons-slot-forward-replaces-car-with-original-car-register)
                  ("FR-015" :implemented
                   "packages/optimize/src/optimizer-flow-loop.lisp"
                   cl-cc/optimize::opt-pass-code-sinking
                   cl-cc/optimize::code-sinking-moves-const-into-target-block)
                  ("FR-351" :implemented
                   "packages/optimize/src/egraph-rules.lisp"
                   cl-cc/optimize::egraph-rule-register
                   cl-cc/optimize::egraph-rule-registry-complete)
                  ("FR-403" :implemented
                   "packages/optimize/src/ssa-construction.lisp"
                   cl-cc/optimize::ssa-destroy
                   cl-cc/optimize::ssa-destroy-places-phi-copies-before-terminator)
                  ("FR-404" :implemented
                   "packages/optimize/src/ssa-construction.lisp"
                   cl-cc/optimize::ssa-sequentialize-copies
                   cl-cc/optimize::ssa-seq-copies-behavior)))
    (destructuring-bind (feature-id status module api-symbol test-anchor) case
      (let ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence feature-id)))
        (assert-true evidence)
        (assert-eq status (cl-cc/optimize::opt-roadmap-evidence-status evidence))
        (assert-true
         (member module
                 (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                 :test #'string=))
        (assert-true
         (member api-symbol
                 (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                 :test #'equal))
        (assert-true
         (member test-anchor
                    (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)
                    :test #'equal))))))

(deftest optimize-backend-roadmap-phase40-frs-have-specific-evidence
  "Phase 40 partial-eval/specialization FRs expose concrete optimization anchors."
  (dolist (case '(("FR-209" :implemented
                   "packages/optimize/src/optimizer-pipeline-speculative.lisp"
                   cl-cc/optimize::opt-specialize-constant-args
                   optimize-specialize-constant-args-builds-residual-body)
                  ("FR-210" :implemented
                   "packages/optimize/src/optimizer-pipeline-speculative.lisp"
                   cl-cc/optimize::opt-sccp-analyze-binding-times
                   optimize-sccp-analyze-binding-times-classifies-lattice-values)
                  ("FR-211" :implemented
                   "packages/optimize/src/optimizer-pipeline-speculative.lisp"
                   cl-cc/optimize::opt-build-specialization-plan
                   optimize-build-specialization-plan-reuses-cache-for-constant-signature)))
    (destructuring-bind (feature-id status module api-symbol test-anchor) case
      (let ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence feature-id)))
        (assert-true evidence)
        (assert-eq status (cl-cc/optimize::opt-roadmap-evidence-status evidence))
        (assert-true
         (member module
                 (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                 :test #'string=))
        (assert-true
         (member api-symbol
                 (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                 :test #'equal))
        (assert-true
         (member test-anchor
                 (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)
                 :test #'%optimize-backend-test-anchor-name=))))))

(deftest optimize-backend-roadmap-fr-217-has-specific-evidence
  "FR-217 memory-SSA implementation is backed by concrete helpers and tests."
  (let ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence "FR-217")))
    (assert-true evidence)
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (dolist (module '("packages/optimize/src/optimizer-memory.lisp"
                      "packages/optimize/tests/optimizer-memory-tests.lisp"))
      (assert-true (member module
                           (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                           :test #'string=)))
    (dolist (api-symbol '(cl-cc/optimize::opt-compute-memory-ssa-snapshot
                          cl-cc/optimize::opt-memory-ssa-version-at))
      (assert-true (member api-symbol
                           (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                           :test #'equal)))
    (dolist (test-anchor '(memory-ssa-snapshot-assigns-monotonic-versions-for-def-use-chain
                           memory-ssa-snapshot-slot-location-uses-alias-root))
      (assert-true (member test-anchor
                           (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)
                           :test #'%optimize-backend-test-anchor-name=)))))

(deftest optimize-backend-roadmap-fr-251-has-specific-evidence
  "FR-251 abstract-interpretation framework is backed by generic domain helpers and tests."
  (let ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence "FR-251")))
    (assert-true evidence)
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (dolist (module '("packages/optimize/src/optimizer-dataflow.lisp"
                      "packages/optimize/tests/optimizer-dataflow-tests.lisp"))
      (assert-true (member module
                           (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                           :test #'string=)))
    (dolist (api-symbol '(cl-cc/optimize::make-opt-abstract-domain
                          cl-cc/optimize::opt-run-abstract-interpretation))
      (assert-true (member api-symbol
                           (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                           :test #'equal)))
    (dolist (test-anchor '(abstract-domain-struct-retains-operators
                           abstract-interpretation-runs-over-cfg-and-produces-result))
      (assert-true (member test-anchor
                           (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)
                           :test #'%optimize-backend-test-anchor-name=)))))

(deftest optimize-backend-roadmap-fr-252-has-specific-evidence
  "FR-252 interprocedural regalloc is backed by policy derivation and allocator integration tests."
  (let ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence "FR-252")))
    (assert-true evidence)
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (dolist (module '("packages/regalloc/src/regalloc.lisp"
                      "packages/regalloc/src/regalloc-allocate.lisp"
                      "packages/emit/tests/regalloc-tests.lisp"))
      (assert-true (member module
                           (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                           :test #'string=)))
    (dolist (api-symbol '(("CL-CC/REGALLOC" . "REGALLOC-BUILD-DIRECT-CALL-GRAPH")
                          ("CL-CC/REGALLOC" . "REGALLOC-COMPUTE-INTERPROCEDURAL-HINTS")
                          ("CL-CC/REGALLOC" . "REGALLOC-BUILD-ALLOCATION-POLICY-FROM-HINTS")
                          ("CL-CC/REGALLOC" . "ALLOCATE-REGISTERS")))
      (assert-true (member api-symbol
                           (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                           :test #'equal)))
    (dolist (test-anchor '(regalloc-interprocedural-hints-detect-leaf-and-leaf-callee-chain
                           regalloc-interprocedural-policy-hook-derives-preferences
                           regalloc-interprocedural-policy-caller-saved-respects-call-crossing-safety
                           regalloc-interprocedural-policy-end-to-end-keeps-call-crossing-safe
                           regalloc-interprocedural-policy-prefers-callee-saved-on-call-crossing))
      (assert-true (member test-anchor
                           (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)
                           :test #'%optimize-backend-test-anchor-name=)))))

(deftest optimize-backend-roadmap-fr-253-has-specific-evidence
  "FR-253 COW helper layer is backed by concrete copy/write APIs and tests."
  (let ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence "FR-253")))
    (assert-true evidence)
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (dolist (module '("packages/optimize/src/optimizer-pipeline-speculative.lisp"
                      "packages/optimize/tests/optimizer-pipeline-tests.lisp"))
      (assert-true (member module
                           (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                           :test #'string=)))
    (dolist (api-symbol '(cl-cc/optimize::make-opt-cow-object
                          cl-cc/optimize::opt-cow-copy
                          cl-cc/optimize::opt-cow-write))
      (assert-true (member api-symbol
                           (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                           :test #'equal)))
    (dolist (test-anchor '(optimize-cow-copy-is-constant-time-share
                           optimize-cow-write-detaches-when-shared))
      (assert-true (member test-anchor
                           (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)
                           :test #'%optimize-backend-test-anchor-name=)))))

(deftest optimize-backend-roadmap-fr-254-has-specific-evidence
  "FR-254 region helper layer is backed by bump/slab APIs and tests."
  (let ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence "FR-254")))
    (assert-true evidence)
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (dolist (module '("packages/optimize/src/optimizer-pipeline-speculative.lisp"
                      "packages/optimize/tests/optimizer-pipeline-tests.lisp"))
      (assert-true (member module
                           (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                           :test #'string=)))
    (dolist (api-symbol '(cl-cc/optimize::make-opt-bump-region
                          cl-cc/optimize::opt-bump-allocate
                          cl-cc/optimize::opt-bump-mark
                          cl-cc/optimize::opt-bump-reset
                          cl-cc/optimize::make-opt-slab-pool
                          cl-cc/optimize::opt-slab-allocate
                          cl-cc/optimize::opt-slab-free))
      (assert-true (member api-symbol
                           (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                           :test #'equal)))
    (dolist (test-anchor '(optimize-bump-region-mark-reset-restores-cursor
                           optimize-slab-pool-reuses-freed-object))
      (assert-true (member test-anchor
                           (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)
                           :test #'%optimize-backend-test-anchor-name=)))))

(deftest optimize-backend-roadmap-fr-008-has-specific-evidence
  "FR-008 float lane has concrete allocator/emitter evidence."
  (let ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence "FR-008")))
    (assert-true evidence)
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (dolist (module '("packages/regalloc/src/regalloc.lisp"
                      "packages/emit/tests/regalloc-tests.lisp"))
      (assert-true (member module
                           (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                           :test #'string=)))
    (assert-true (member 'regalloc-float-vregs-allocated-to-distinct-xmm-registers
                         (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)
                         :test #'%optimize-backend-test-anchor-name=))))

(deftest optimize-backend-roadmap-fr-283-has-specific-evidence
  "FR-283 multiply-high support is backed by concrete VM semantics, native encoder, and roadmap audit anchors."
  (let ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence "FR-283")))
    (assert-true evidence)
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (dolist (module '("packages/vm/src/vm-bitwise.lisp"
                      "packages/codegen/src/x86-64-sequences.lisp"
                      "packages/codegen/src/aarch64-codegen.lisp"
                      "packages/emit/tests/x86-64-encoding-tests.lisp"
                      "packages/emit/tests/aarch64-codegen-tests.lisp"))
      (assert-true (member module
                           (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                           :test #'string=)))
    (dolist (api-symbol '(("CL-CC" . "MAKE-VM-INTEGER-MUL-HIGH-U")
                          ("CL-CC" . "MAKE-VM-INTEGER-MUL-HIGH-S")
                          ("CL-CC/VM" . "%VM-INTEGER-MUL-HIGH-U")
                          ("CL-CC/VM" . "%VM-INTEGER-MUL-HIGH-S")
                          ("CL-CC/CODEGEN" . "EMIT-MUL-HIGH-SEQUENCE")
                          ("CL-CC/CODEGEN" . "ENCODE-UMULH")
                          ("CL-CC/CODEGEN" . "ENCODE-SMULH")))
      (assert-true (member api-symbol
                           (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                           :test #'equal)))
    (dolist (test-anchor '(vm-mul-high-64-semantics
                           x86-mul-rm64-high-encodings
                           x86-seq-mul-high-sequence-encodings
                           x86-mul-high-size-and-dispatch-registered
                           a64-mul-high-encoders
                           aarch64-mul-high-emitter-encodings
                           aarch64-mul-high-size-and-dispatch-registered))
      (assert-true (member test-anchor
                            (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)
                            :test #'%optimize-backend-test-anchor-name=)))))

(deftest optimize-backend-roadmap-fr-303-has-specific-evidence
  "FR-303 overflow detection is backed by checked VM instructions and native trap emitters."
  (let ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence "FR-303")))
    (assert-true evidence)
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (dolist (module '("packages/vm/src/vm-instructions.lisp"
                      "packages/codegen/src/x86-64-emit-ops.lisp"
                      "packages/codegen/src/aarch64-emitters.lisp"
                      "packages/emit/tests/x86-64-emit-ops-tests.lisp"
                      "packages/emit/tests/aarch64-emit-tests.lisp"))
      (assert-true (member module
                           (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                           :test #'string=)))
    (dolist (api-symbol '(("CL-CC" . "MAKE-VM-ADD-CHECKED")
                          ("CL-CC" . "MAKE-VM-SUB-CHECKED")
                          ("CL-CC" . "MAKE-VM-MUL-CHECKED")
                          ("CL-CC/CODEGEN" . "EMIT-VM-ADD-CHECKED")
                          ("CL-CC/CODEGEN" . "EMIT-A64-VM-ADD-CHECKED")))
      (assert-true (member api-symbol
                           (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                           :test #'equal)))
    (dolist (test-anchor '(x86-emit-add-checked-emits-14-bytes
                           x86-emit-sub-checked-emits-14-bytes
                           x86-emit-mul-checked-emits-15-bytes
                           aarch64-emit-add-checked-emits-12-bytes
                           aarch64-emit-sub-checked-emits-12-bytes
                           aarch64-emit-mul-checked-emits-24-bytes))
      (assert-true (member test-anchor
                           (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)
                           :test #'%optimize-backend-test-anchor-name=)))))

(deftest optimize-backend-roadmap-fr-295-has-specific-evidence
  "FR-295 PGO instrumentation has concrete counter-plan, pipeline, and CLI tests."
  (let ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence "FR-295")))
    (assert-true evidence)
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (dolist (module '("packages/optimize/src/optimizer-pipeline-speculative.lisp"
                      "packages/pipeline/src/pipeline.lisp"
                      "packages/compile/src/codegen.lisp"
                      "packages/cli/src/main-utils.lisp"
                      "packages/cli/src/handlers.lisp"
                      "packages/optimize/tests/optimizer-pipeline-tests.lisp"
                      "packages/compile/tests/pipeline-tests.lisp"
                      "packages/cli/tests/cli-tests.lisp"))
      (assert-true (member module
                           (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                           :test #'string=)))
    (dolist (api-symbol '(cl-cc/optimize::opt-pgo-build-counter-plan
                          cl-cc/optimize::opt-pgo-make-profile-template
                          ("CL-CC/COMPILE" . "COMPILATION-RESULT-PGO-COUNTER-PLAN")))
      (assert-true (member api-symbol
                           (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                           :test #'equal)))
    (dolist (test-anchor '(optimize-pgo-build-counter-plan-emits-deterministic-bb-and-edge-ids
                           optimize-pgo-make-profile-template-zero-initializes-counts
                           pipeline-compile-string-emits-pgo-counter-plan
                           cli-maybe-make-profiled-vm-state-enabled-for-pgo-generate
                           cli-write-pgo-profile-emits-file))
      (assert-true (member test-anchor
                           (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)
                           :test #'%optimize-backend-test-anchor-name=)))))

(deftest optimize-backend-roadmap-fr-352-has-specific-evidence
  "FR-352 bit-width analysis is backed by interval helpers, range propagation, and low-bit rewrites."
  (let ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence "FR-352")))
    (assert-true evidence)
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (dolist (module '("packages/optimize/src/optimizer-memory.lisp"
                      "packages/optimize/src/optimizer-memory-ranges.lisp"
                      "packages/optimize/src/optimizer.lisp"
                      "packages/optimize/tests/optimizer-memory-tests.lisp"
                      "packages/optimize/tests/optimizer-memory-pass-tests.lisp"))
      (assert-true (member module
                           (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                           :test #'string=)))
    (dolist (api-symbol '(cl-cc/optimize::opt-interval-logand
                          cl-cc/optimize::opt-interval-bit-width
                          cl-cc/optimize::opt-pass-elide-proven-overflow-checks
                          cl-cc/optimize::%opt-rewrite-logand-low-bit-test
                          cl-cc/optimize::opt-pass-fold))
      (assert-true (member api-symbol
                           (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                           :test #'equal)))
    (dolist (test-anchor '(value-ranges-logand-mask-with-unknown-input-narrows-to-8-bit
                           value-ranges-add-of-masked-8-bit-values-is-9-bit-wide
                           overflow-check-elim-rewrites-proven-8-bit-add-to-unchecked-integer-add
                           optimize-instructions-rewrites-logand-one-eq-zero-to-evenp))
      (assert-true (member test-anchor
                           (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)
                            :test #'%optimize-backend-test-anchor-name=)))))

(deftest optimize-backend-roadmap-fr-523-to-fr-528-have-fr-specific-evidence
  "FR-523..FR-528 keep per-FR module/API/test anchors instead of a shared generic bucket."
  (dolist (case '(("FR-523" cl-cc/optimize::opt-pass-affine-loop-analysis
                    optimize-affine-loop-summary-builds-descriptor)
                  ("FR-524" cl-cc/optimize::opt-pass-loop-interchange
                   optimize-pass-loop-interchange-handles-nested-canonical-loop)
                  ("FR-525" cl-cc/optimize::opt-pass-polyhedral-schedule
                   optimize-pass-polyhedral-schedule-reorders-loop-body)
                  ("FR-526" cl-cc/optimize::opt-pass-loop-fusion-fission
                   optimize-pass-loop-fusion-fission-splits-oversized-loop)
                  ("FR-527" cl-cc/optimize::opt-ml-inline-score-plan
                   optimize-ml-inline-score-plan-is-deterministic)
                  ("FR-528" cl-cc/optimize::opt-learned-codegen-cost-plan
                   optimize-learned-codegen-cost-plan-is-target-aware)))
    (destructuring-bind (feature-id api-symbol test-anchor) case
      (let ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence feature-id)))
        (assert-true evidence)
        (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
        (assert-true (member "packages/optimize/src/optimizer-pipeline-speculative.lisp"
                             (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                             :test #'string=))
        (assert-true (member "packages/optimize/tests/optimizer-pipeline-tests.lisp"
                             (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                             :test #'string=))
        (assert-true (member api-symbol
                             (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                             :test #'equal))
        (assert-true (member test-anchor
                             (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)
                             :test #'%optimize-backend-test-anchor-name=))))))

(deftest optimize-backend-roadmap-audited-fr-statuses-match-doc
  "The audited optimize-backend FRs keep their intended doc statuses after roadmap updates."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (feature (cl-cc/optimize:optimize-backend-roadmap-doc-features))
      (setf (gethash (cl-cc/optimize::opt-roadmap-feature-id feature) table)
            (cl-cc/optimize::opt-roadmap-feature-status feature)))
    (dolist (case '(("FR-303" :implemented)
                    ("FR-352" :implemented)
                    ("FR-360" :implemented)
                    ("FR-366" :implemented)
                    ("FR-370" :implemented)
                    ("FR-374" :implemented)
                    ("FR-376" :implemented)
                    ("FR-377" :implemented)
                     ("FR-379" :implemented)
                     ("FR-389" :implemented)
                     ("FR-391" :implemented)
                     ("FR-400" :implemented)
                     ("FR-401" :implemented)
                     ("FR-462" :implemented)
                     ("FR-463" :implemented)))
      (destructuring-bind (feature-id expected-status) case
        (assert-eq expected-status (gethash feature-id table))))))

(deftest optimize-backend-roadmap-audited-frs-have-specific-evidence
  "The audited optimize-backend FRs retain concrete module, API, and test anchors."
  (dolist (case '(("FR-303" :implemented
                   "packages/codegen/src/x86-64-emit-ops.lisp"
                   ("CL-CC/CODEGEN" . "EMIT-VM-ADD-CHECKED")
                   x86-emit-add-checked-emits-14-bytes)
                  ("FR-352" :implemented
                   "packages/optimize/src/optimizer-memory.lisp"
                   cl-cc/optimize::opt-interval-logand
                   overflow-check-elim-rewrites-proven-8-bit-add-to-unchecked-integer-add)
                  ("FR-360" :implemented
                    "packages/type/src/inference-handlers.lisp"
                    ("CL-CC/TYPE" . "INFER-THE")
                    infer-the-matching-type-is-fixnum)
                  ("FR-366" :implemented
                   "packages/expand/src/macros-runtime-support.lisp"
                   ("CL-CC/EXPAND" . "*LOAD-TIME-VALUE-CACHE*")
                   load-time-value-is-memoized-during-expansion)
                  ("FR-370" :implemented
                   "packages/compile/src/context.lisp"
                   ("CL-CC/COMPILE" . "*BUILTIN-SPECIAL-VARIABLES*")
                   ctx-initialization)
                   ("FR-374" :implemented
                    "packages/vm/src/vm-dispatch-gf.lisp"
                    ("CL-CC/VM" . "%VM-GF-EQL-METHODS")
                    gf-multi-single-dispatch-eql-index-hit-precedes-class)
                  ("FR-376" :implemented
                   "packages/expand/src/expander-control.lisp"
                   ("CL-CC/EXPAND" . "%EXPAND-HANDLER-CASE-FORM")
                   codegen-handler-case-run-cases)
                  ("FR-377" :implemented
                   "packages/compile/src/codegen-control.lisp"
                   ("CL-CC/COMPILE" . "COMPILE-AST")
                   codegen-unwind-protect-run-cases)
                  ("FR-379" :implemented
                   "packages/vm/src/vm-extensions.lisp"
                   ("CL-CC/VM" . "VM-SYMBOL-PLIST-READ-SNAPSHOT")
                   vm-set-symbol-plist-overwrites-and-promotes-long-plist)
                    ("FR-388" :implemented
                     "packages/codegen/src/x86-64-regs.lisp"
                     ("CL-CC/CODEGEN" . "*X86-64-OMIT-FRAME-POINTER*")
                     x86-vm-program-default-fpe-allocates-rsp-spill-frame)
                    ("FR-389" :implemented
                      "packages/codegen/src/x86-64-regs.lisp"
                      ("CL-CC/CODEGEN" . "X86-64-RED-ZONE-SPILL-P")
                      x86-vm-program-leaf-red-zone-spills-skip-rbp-frame)
                   ("FR-391" :implemented
                    "packages/codegen/src/x86-64-codegen.lisp"
                    ("CL-CC/CODEGEN" . "EMIT-X86-64-STACK-PROBES")
                    x86-stack-probe-count-thresholds)
                   ("FR-400" :implemented
                    "packages/expand/src/macros-control-flow-case.lisp"
                   ("CL-CC/EXPAND" . "%PRUNE-TYPECASE-CLAUSES")
                   ecase-expands-to-let-with-case)
                  ("FR-401" :implemented
                   "packages/expand/src/macros-control-flow-case.lisp"
                   ("CL-CC/EXPAND" . "%CASE-EXPAND-INTEGER-TABLE")
                   case-expands-dense-integer-keys-into-table-dispatch)
                   ("FR-462" :implemented
                    "packages/cli/src/main-utils.lisp"
                    ("CL-CC/CLI" . "%WRITE-FLAMEGRAPH-SVG")
                    cli-write-flamegraph-svg-emits-svg-document)
                   ("FR-463" :implemented
                    "packages/cli/src/main-dump.lisp"
                    ("CL-CC/CLI" . "%DUMP-IR-PHASE")
                    cli-dump-ir-phase-dispatches-all-phases)))
    (destructuring-bind (feature-id status module api-symbol test-anchor) case
      (let ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence feature-id)))
        (assert-true evidence)
        (assert-eq status (cl-cc/optimize::opt-roadmap-evidence-status evidence))
        (assert-true
         (member module
                 (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                 :test #'string=))
        (assert-true
         (member api-symbol
                 (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                 :test #'equal))
        (assert-true
         (member test-anchor
                  (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)
                  :test #'%optimize-backend-test-anchor-name=))))))

(deftest optimize-backend-roadmap-support-evidence-has-behavior
  "Backend support evidence exercises analysis, profile, allocation, guard, and layout helpers."
  (let* ((bottom (cl-cc/optimize::opt-lattice-bottom))
         (const-a (cl-cc/optimize::opt-lattice-constant :a))
         (const-b (cl-cc/optimize::opt-lattice-constant :b)))
    (assert-eq :constant
               (cl-cc/optimize::opt-lattice-value-kind
                (cl-cc/optimize::opt-lattice-meet bottom const-a)))
    (assert-eq :overdefined
               (cl-cc/optimize::opt-lattice-value-kind
                (cl-cc/optimize::opt-lattice-meet const-a const-b))))
  (let ((profile (cl-cc/optimize::make-opt-profile-data)))
    (assert-= 5 (cl-cc/optimize::opt-profile-record-edge profile :a :b 5))
    (assert-= 2 (cl-cc/optimize::opt-profile-record-call-chain profile '(:a :b) 2))
    (assert-equal (cons 3 32)
                  (cl-cc/optimize::opt-profile-record-allocation profile :site 32 3)))
  (let ((region (cl-cc/optimize::make-opt-bump-region :limit 16)))
    (assert-= 0 (cl-cc/optimize::opt-bump-allocate region 4))
    (assert-= 8 (cl-cc/optimize::opt-bump-allocate region 4 :alignment 8)))
  (let ((guard (cl-cc/optimize::make-opt-guard-state :executions 10)))
    (assert-eq :tag-bit-test (cl-cc/optimize::opt-guard-record guard t)))
  (let ((shape (cl-cc/optimize::make-opt-shape-descriptor-for-slots 1 '(:x :y :z))))
    (assert-= 2 (cl-cc/optimize::opt-shape-slot-offset shape :z))))

(deftest optimize-backend-roadmap-fr-463-has-specific-evidence
  "FR-463 Compiler Explorer compatible output is backed by CLI dump functions, phase table, and annotate-source support."
  (let ((evidence (cl-cc/optimize:lookup-opt-backend-roadmap-evidence "FR-463")))
    (assert-true evidence)
    (assert-eq :implemented (cl-cc/optimize::opt-roadmap-evidence-status evidence))
    (dolist (module '("packages/cli/src/main-dump.lisp"
                      "packages/cli/src/main-utils.lisp"
                      "packages/cli/src/args.lisp"
                      "packages/cli/src/handlers.lisp"
                      "packages/pipeline/src/pipeline.lisp"
                      "packages/cli/tests/main-dump-tests.lisp"
                      "packages/cli/tests/cli-tests.lisp"))
      (assert-true (member module
                           (cl-cc/optimize::opt-roadmap-evidence-modules evidence)
                           :test #'string=)))
    (dolist (api-symbol '(("CL-CC/CLI" . "%DUMP-IR-PHASE")
                          ("CL-CC/CLI" . "%DUMP-AST-PHASE")
                          ("CL-CC/CLI" . "%DUMP-CPS-PHASE")
                          ("CL-CC/CLI" . "%DUMP-SSA-PHASE")
                          ("CL-CC/CLI" . "%DUMP-VM-PHASE")
                          ("CL-CC/CLI" . "%DUMP-OPT-PHASE")
                          ("CL-CC/CLI" . "%DUMP-ASM-PHASE")
                          ("CL-CC/CLI" . "*IR-PHASE-DUMP-FNS*")
                          ("CL-CC/CLI" . "*IR-PHASES*")))
      (assert-true (member api-symbol
                           (cl-cc/optimize::opt-roadmap-evidence-api-symbols evidence)
                           :test #'equal)))
    (dolist (test-anchor '(cli-dump-ir-phase-dispatches-all-phases
                           cli-dump-ir-phase-annotate-source-writes-comment-for-ast
                           cli-dump-ir-phase-annotate-source-writes-comment-for-vm-and-opt
                           cli-dump-ir-phase-asm-output-is-ansi-colored
                           cli-dump-ir-phase-annotate-source-omits-comment-on-missing-location
                           cli-real-file-dump-ir-annotation-preserves-source-location
                            cli-do-compile-dump-ir-annotate-source-preserves-real-file-location
                            cli-do-compile-dump-ir-annotate-source-macro-forms-preserve-real-file-location
                            cli-dump-ir-phase-phase-table-covers-all-recognized-phases
                           cli-dump-ir-phase-invalid-signals-error))
      (assert-true (member test-anchor
                           (cl-cc/optimize::opt-roadmap-evidence-test-anchors evidence)
                           :test #'%optimize-backend-test-anchor-name=)))))
