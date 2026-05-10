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
    (assert-true (> not-implemented 0))
    (assert-true (< implemented (length ids)))
    (assert-true (> (length (remove-duplicates profiles :test #'equal)) 5))))

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
