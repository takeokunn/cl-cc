;;;; tests/unit/optimize/optimizer-pipeline-tests.lisp
;;;; Unit tests for optimizer-pipeline.lisp — pipeline mechanism
;;;;
;;;; Covers: opt-parse-pass-pipeline-string, opt-converged-p,
;;;;   opt-adaptive-max-iterations, opt-verify-instructions,
;;;;   opt-resolve-pass-pipeline, *opt-pass-registry* data, prolog-rewrite-stage.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── %opt-trim-whitespace ────────────────────────────────────────────────────

(deftest-each opt-trim-whitespace-cases
  "%opt-trim-whitespace strips leading/trailing spaces, tabs, and newlines."
  :cases (("spaces"      "hello"      "  hello  ")
          ("tabs"        "world"      (format nil "~Cworld~C" #\Tab #\Tab))
          ("newlines"    "foo"        (format nil "~Cfoo~C" #\Newline #\Newline))
          ("mixed"       "bar"        (format nil " ~C~C bar ~C~C " #\Tab #\Newline #\Newline #\Tab))
          ("no-trim"     "bare"       "bare")
          ("empty"       ""           ""))
  (expected input)
  (assert-equal expected (cl-cc/optimize::%opt-trim-whitespace input)))

;;; ─── opt-parse-pass-pipeline-string ─────────────────────────────────────────

(deftest-each parse-pass-pipeline-string-cases
  "opt-parse-pass-pipeline-string correctly tokenises pass name strings."
  :cases (("single-pass"              "sccp"         1 :SCCP nil  nil)
          ("multi-pass-comma-separated" "sccp,cse,dce" 3 :SCCP :CSE :DCE)
          ("trims-whitespace"         " sccp , cse " 2 :SCCP :CSE nil)
          ("empty-returns-nil"        ""             0 nil   nil  nil))
  (input expected-len first-kw second-kw third-kw)
  (let ((result (cl-cc/optimize::opt-parse-pass-pipeline-string input)))
    (if (zerop expected-len)
        (assert-null result)
        (progn
          (assert-= expected-len (length result))
          (when first-kw  (assert-eq first-kw  (first  result)))
          (when second-kw (assert-eq second-kw (second result)))
          (when third-kw  (assert-eq third-kw  (third  result)))))))

;;; ─── opt-converged-p ─────────────────────────────────────────────────────────

(deftest-each opt-converged-p-cases
  "opt-converged-p correctly detects convergence across nil, same-object, structural-equal, and length-mismatch cases."
  :cases (("both-nil"           :nil-case   t)
          ("same-object"        :same-case  t)
          ("structurally-equal" :equal-case t)
          ("different-length"   :diff-case  nil))
  (scenario expected-result)
  (let* ((i1 (make-vm-const :dst :r0 :value 1))
         (i2 (make-vm-ret  :reg :r0))
         (prog (list i1 i2))
         (a    (make-vm-const :dst :r0 :value 1))
         (b    (make-vm-const :dst :r0 :value 1)))
    (let ((result (ecase scenario
                    (:nil-case   (cl-cc/optimize::opt-converged-p nil nil))
                    (:same-case  (cl-cc/optimize::opt-converged-p prog prog))
                    (:equal-case (cl-cc/optimize::opt-converged-p (list a) (list b)))
                    (:diff-case  (cl-cc/optimize::opt-converged-p (list i1) (list i1 i2))))))
      (if expected-result
          (assert-true result)
          (assert-false result)))))

;;; ─── opt-adaptive-max-iterations ─────────────────────────────────────────────

(deftest-each adaptive-max-iterations-regions
  "opt-adaptive-max-iterations returns the correct budget for each size region."
  :cases (("tiny"   20  8)   ; < 50 insts: base-20 + (-12) = 8 (clamped to min 6)
          ("small"  100 14)  ; 50-149: base-20 + (-6) = 14
          ("medium" 200 20)  ; 150-399: base-20 + 0 = 20
          ("large"  500 28)  ; 400-799: base-20 + 8 = 28
          ("huge"   1000 35)) ; >= 800: base-20 + 15 = 35
  (n-insts expected)
  (let ((insts (make-list n-insts :initial-element (make-vm-const :dst :r0 :value 1))))
    (assert-= expected (cl-cc/optimize::opt-adaptive-max-iterations insts))))

(deftest adaptive-max-iterations-respects-max-cap
  "opt-adaptive-max-iterations never exceeds :max-iterations.
For n=2000 insts the natural computed value is base(20) + 15 = 35; pass a
max-iterations of 30 to actually exercise the cap clamping (35 → 30)."
  (let ((insts (make-list 2000 :initial-element (make-vm-const :dst :r0 :value 1))))
    (assert-= 30 (cl-cc/optimize::opt-adaptive-max-iterations insts :max-iterations 30))))

(deftest adaptive-max-iterations-respects-min-floor
  "opt-adaptive-max-iterations never returns less than :min-iterations."
  (let ((insts nil))  ; empty → smallest budget
    (assert-true (>= (cl-cc/optimize::opt-adaptive-max-iterations insts :min-iterations 6) 6))))

;;; ─── opt-verify-instructions ─────────────────────────────────────────────────

(deftest verify-instructions-simple-sequence-passes
  "opt-verify-instructions returns T for a simple const+ret sequence."
  (assert-true (cl-cc/optimize:opt-verify-instructions
                (list (make-vm-const :dst :r0 :value 1) (make-vm-ret :reg :r0)))))

(deftest verify-instructions-jump-with-known-label-passes
  "opt-verify-instructions returns T when a jump target label is defined in the sequence."
  (assert-true (cl-cc/optimize:opt-verify-instructions
                (list (make-vm-const :dst :r0 :value 1)
                      (make-vm-jump  :label "target")
                      (make-vm-label :name "target")
                      (make-vm-ret   :reg :r0)))))

(deftest-each verify-instructions-invalid-cases
  "opt-verify-instructions signals an error for duplicate labels or unknown jump targets."
  :cases (("duplicate-label"   (list (make-vm-const :dst :r0 :value 1)
                                     (make-vm-label :name "dup")
                                     (make-vm-label :name "dup")
                                     (make-vm-ret   :reg :r0)))
          ("unknown-target"    (list (make-vm-jump :label "ghost")
                                     (make-vm-ret  :reg :r0))))
  (insts)
  (assert-signals error
    (cl-cc/optimize:opt-verify-instructions insts)))

;;; ─── opt-resolve-pass-pipeline ───────────────────────────────────────────

(deftest resolve-pass-pipeline-nil-returns-convergence-passes
  "opt-resolve-pass-pipeline on nil returns *opt-convergence-passes*."
  (assert-eq cl-cc/optimize::*opt-convergence-passes*
             (cl-cc/optimize::opt-resolve-pass-pipeline nil)))

(deftest resolve-pass-pipeline-functions-pass-through-unchanged
  "opt-resolve-pass-pipeline passes function objects through as-is."
  (let* ((fn (lambda (x) x))
         (pipeline (list fn)))
    (assert-eq fn (first (cl-cc/optimize::opt-resolve-pass-pipeline pipeline)))))

(deftest resolve-pass-pipeline-keywords-resolve-to-functions
  "opt-resolve-pass-pipeline resolves keyword pass names to function objects."
  (let ((result (cl-cc/optimize::opt-resolve-pass-pipeline (list :fold :dce))))
    (assert-= 2 (length result))
    (assert-true (every #'functionp result))))

(deftest resolve-pass-pipeline-string-parses-and-resolves
  "opt-resolve-pass-pipeline on a comma-separated string parses and resolves to functions."
  (let ((result (cl-cc/optimize::opt-resolve-pass-pipeline "fold,dce")))
    (assert-= 2 (length result))
    (assert-true (every #'functionp result))))

(deftest resolve-pass-pipeline-unknown-pass-signals-error
  "opt-resolve-pass-pipeline signals an error for an unregistered pass keyword."
  (assert-signals error
    (cl-cc/optimize::opt-resolve-pass-pipeline (list :nonexistent-pass))))

;;; ─── *opt-convergence-passes* / *opt-pass-registry* data coverage ────────

(deftest opt-pass-data-integrity
  "*opt-convergence-passes* is a non-empty function list; registry includes early Prolog/egraph passes and core cleanup passes."
  (assert-true (listp cl-cc/optimize::*opt-convergence-passes*))
  (assert-true (> (length cl-cc/optimize::*opt-convergence-passes*) 10))
  (assert-true (every #'functionp cl-cc/optimize::*opt-convergence-passes*))
  (assert-true (gethash :prolog-rewrite cl-cc/optimize::*opt-pass-registry*))
  (assert-true (gethash :egraph cl-cc/optimize::*opt-pass-registry*))
  (assert-true (gethash :fold cl-cc/optimize::*opt-pass-registry*))
  (assert-true (gethash :cons-slot-forward cl-cc/optimize::*opt-pass-registry*))
  (assert-true (gethash :pure-call-optimization cl-cc/optimize::*opt-pass-registry*))
  (assert-true (gethash :dce  cl-cc/optimize::*opt-pass-registry*))
  (assert-true (gethash :cse  cl-cc/optimize::*opt-pass-registry*))
  (assert-eq #'cl-cc/optimize::%maybe-apply-prolog-rewrite (first cl-cc/optimize::*opt-convergence-passes*))
  (assert-false (member #'cl-cc/optimize:optimize-with-egraph cl-cc/optimize::*opt-convergence-passes*))
  (assert-false (member #'cl-cc/optimize::opt-pass-fold cl-cc/optimize::*opt-convergence-passes*))
  (assert-false (member #'cl-cc/optimize::opt-pass-strength-reduce cl-cc/optimize::*opt-convergence-passes*))
  (assert-equal '(:prolog-rewrite :call-site-splitting :devirtualize :inline :overflow-check-elim)
                (subseq cl-cc/optimize::*opt-default-convergence-pass-keys* 0 5))
  (assert-eq :sccp (sixth cl-cc/optimize::*opt-default-convergence-pass-keys*))
  (assert-eq :cons-slot-forward (seventh cl-cc/optimize::*opt-default-convergence-pass-keys*))
  (assert-true (member :pure-call-optimization cl-cc/optimize::*opt-default-convergence-pass-keys*))
  (assert-true (< (position :copy-prop cl-cc/optimize::*opt-default-convergence-pass-keys*)
                  (position :pure-call-optimization cl-cc/optimize::*opt-default-convergence-pass-keys*)))
  (assert-true (< (position :pure-call-optimization cl-cc/optimize::*opt-default-convergence-pass-keys*)
                  (position :gvn cl-cc/optimize::*opt-default-convergence-pass-keys*)))
  (assert-true (< (position :pure-call-optimization cl-cc/optimize::*opt-default-convergence-pass-keys*)
                  (position :cse cl-cc/optimize::*opt-default-convergence-pass-keys*)))
  (assert-true (< (position :pure-call-optimization cl-cc/optimize::*opt-default-convergence-pass-keys*)
                  (position :dce cl-cc/optimize::*opt-default-convergence-pass-keys*))))

;;; ─── *verify-optimizer-instructions* integration ──────────────────────────

(deftest verify-optimizer-flag-runs-verifier-on-valid-input
  "*verify-optimizer-instructions* T causes opt-verify-instructions to run; valid input succeeds."
  (let ((cl-cc/optimize:*verify-optimizer-instructions* t)
        (insts (list (make-vm-const :dst :r0 :value 42) (make-vm-ret :reg :r0))))
    (assert-true (listp (cl-cc/optimize:optimize-instructions insts)))))

(deftest verify-optimizer-flag-nil-skips-verifier
  "*verify-optimizer-instructions* NIL causes optimize-instructions to skip verification."
  (let ((cl-cc/optimize:*verify-optimizer-instructions* nil)
        (insts (list (make-vm-const :dst :r0 :value 1) (make-vm-ret :reg :r0))))
    (assert-true (listp (cl-cc/optimize:optimize-instructions insts)))))

(deftest pure-call-policy-gate-disables-pass
  "*opt-enable-pure-call-optimization* NIL disables :pure-call-optimization pass execution."
  (let* ((callee-label "pure-square")
         (insts (list (make-vm-closure :dst :r9 :label callee-label :params '(:r0) :captured nil)
                      (make-vm-label   :name callee-label)
                      (make-vm-mul     :dst :r1 :lhs :r0 :rhs :r0)
                      (make-vm-ret     :reg :r1)
                      (make-vm-closure :dst :r2 :label callee-label :params '(:r0) :captured nil)
                      (make-vm-const   :dst :r0 :value 7)
                      (make-vm-call    :dst :r3 :func :r2 :args '(:r0))
                      (make-vm-call    :dst :r4 :func :r2 :args '(:r0))
                      (make-vm-ret     :reg :r4)))
         (cl-cc/optimize::*opt-enable-pure-call-optimization* nil)
         (optimized (cl-cc/optimize:optimize-instructions
                     insts
                     :max-iterations 1
                     :pass-pipeline '(:pure-call-optimization))))
    (assert-= 2 (count-if (lambda (inst) (typep inst 'cl-cc/vm::vm-call)) optimized))
    (assert-false
     (some (lambda (inst)
             (and (typep inst 'cl-cc/vm::vm-move)
                  (eq (cl-cc/vm::vm-dst inst) :r4)
                  (eq (cl-cc/vm::vm-src inst) :r3)))
           optimized))))

(deftest optimize-policy-config-speed-threshold
  "opt-configure-optimization-policy toggles pure-call gate at speed >= 3."
  (let ((cl-cc/optimize::*opt-enable-pure-call-optimization* t))
    (assert-false (cl-cc/optimize:opt-configure-optimization-policy :speed 2))
    (assert-true  (cl-cc/optimize:opt-configure-optimization-policy :speed 3))))

(deftest optimize-instructions-speed-keyword-controls-pure-call-pass
  "optimize-instructions :speed drives pure-call optimization gate behavior."
  (let* ((callee-label "pure-square")
         (insts (list (make-vm-closure :dst :r9 :label callee-label :params '(:r0) :captured nil)
                      (make-vm-label   :name callee-label)
                      (make-vm-mul     :dst :r1 :lhs :r0 :rhs :r0)
                      (make-vm-ret     :reg :r1)
                      (make-vm-closure :dst :r2 :label callee-label :params '(:r0) :captured nil)
                      (make-vm-const   :dst :r0 :value 7)
                      (make-vm-call    :dst :r3 :func :r2 :args '(:r0))
                      (make-vm-call    :dst :r4 :func :r2 :args '(:r0))
                      (make-vm-ret     :reg :r4)))
         (slow (cl-cc/optimize:optimize-instructions
                insts :max-iterations 1 :speed 2 :pass-pipeline '(:pure-call-optimization)))
         (fast (cl-cc/optimize:optimize-instructions
                insts :max-iterations 1 :speed 3 :pass-pipeline '(:pure-call-optimization))))
    (assert-= 2 (count-if (lambda (inst) (typep inst 'cl-cc/vm::vm-call)) slow))
    (assert-= 1 (count-if (lambda (inst) (typep inst 'cl-cc/vm::vm-call)) fast))))

;;; ─── Prolog rewrite stage ──────────────────────────────────────────────────

(deftest prolog-rewrite-stage-disabled-is-identity
  "%maybe-apply-prolog-rewrite returns the input unchanged when the Prolog hook is disabled."
  (let ((cl-cc/optimize::*enable-prolog-peephole* nil)
        (insts (list (make-vm-const :dst :r0 :value 1)
                     (make-vm-ret :reg :r0))))
    (assert-eq insts (cl-cc/optimize::%maybe-apply-prolog-rewrite insts))))

(deftest prolog-rewrite-stage-invokes-prolog-backends
  "%maybe-apply-prolog-rewrite returns a list result when enabled."
  (let ((cl-cc/optimize::*enable-prolog-peephole* t)
        (insts (list (make-vm-const :dst :r0 :value 1)
                     (make-vm-ret :reg :r0))))
    (let ((result (cl-cc/optimize::%maybe-apply-prolog-rewrite insts)))
      (assert-true (listp result))
      (assert-= 2 (length result))
      (assert-equal (mapcar #'cl-cc/optimize::instruction->sexp insts)
                    (mapcar #'cl-cc/optimize::instruction->sexp result)))))

;;; ─── Copy-on-Write helper layer (FR-253 partial) ───────────────────────────

(deftest optimize-cow-copy-is-constant-time-share
  "opt-cow-copy increments refcount without duplicating payload immediately."
  (let* ((cow (cl-cc/optimize:make-opt-cow-object :payload '((a . 1)) :refcount 1))
         (shared (cl-cc/optimize:opt-cow-copy cow)))
    (assert-eq cow shared)
    (assert-= 2 (cl-cc/optimize:opt-cow-object-refcount cow))))

(deftest optimize-cow-write-detaches-when-shared
  "opt-cow-write detaches payload when refcount > 1 and preserves original payload."
  (let* ((original (cl-cc/optimize:make-opt-cow-object :payload '((a . 1)) :refcount 1))
         (shared (cl-cc/optimize:opt-cow-copy original))
         (written (cl-cc/optimize:opt-cow-write
                   shared
                   (lambda (payload)
                     (setf (cdar payload) 99)))))
    (assert-false (eq written original))
    (assert-= 1 (cl-cc/optimize:opt-cow-object-refcount original))
    (assert-= 1 (cl-cc/optimize:opt-cow-object-refcount written))
    (assert-equal '((a . 1)) (cl-cc/optimize:opt-cow-object-payload original))
    (assert-equal '((a . 99)) (cl-cc/optimize:opt-cow-object-payload written))))

;;; ─── Region/bump allocation helpers (FR-254 partial) ───────────────────────

(deftest optimize-bump-region-mark-reset-restores-cursor
  "opt-bump-mark/opt-bump-reset restore cursor to saved point."
  (let ((region (cl-cc/optimize:make-opt-bump-region :cursor 0 :limit 64 :marks nil)))
    (assert-= 0 (cl-cc/optimize:opt-bump-allocate region 8))
    (assert-= 8 (cl-cc/optimize:opt-bump-mark region))
    (assert-= 8 (cl-cc/optimize:opt-bump-allocate region 4))
    (assert-= 12 (cl-cc/optimize:opt-bump-region-cursor region))
    (cl-cc/optimize:opt-bump-reset region)
    (assert-= 8 (cl-cc/optimize:opt-bump-region-cursor region))))

(deftest optimize-slab-pool-reuses-freed-object
  "opt-slab-pool recycles freed objects through freelist."
  (let* ((pool (cl-cc/optimize:make-opt-slab-pool :object-size 2 :free-list nil :next-id 0 :allocated-count 0))
         (obj1 (cl-cc/optimize:opt-slab-allocate pool)))
    (cl-cc/optimize:opt-slab-free pool obj1)
    (let ((obj2 (cl-cc/optimize:opt-slab-allocate pool)))
      (assert-equal obj1 obj2))))

;;; ─── Inline cache helper layer (FR-009 / FR-019 partial) ──────────────────

(deftest optimize-ic-resolve-target-prefers-site-local-entry
  "opt-ic-resolve-target returns site-local target when IC entry exists."
  (let ((site (cl-cc/optimize::make-opt-ic-site :state :monomorphic :entries nil)))
    (cl-cc/optimize::opt-ic-transition site :shape-a :target-a)
    (multiple-value-bind (target source)
        (cl-cc/optimize::opt-ic-resolve-target site :shape-a)
      (assert-eq :target-a target)
      (assert-eq :site-local source))))

(deftest optimize-ic-resolve-target-uses-shared-megamorphic-cache
  "Megamorphic site misses consult shared megamorphic cache as fallback."
  (let* ((site (cl-cc/optimize::make-opt-ic-site
                :state :megamorphic :entries nil :max-polymorphic-entries 2))
         (cache (cl-cc/optimize::make-opt-megamorphic-cache :max-size 2)))
    (cl-cc/optimize::opt-mega-cache-put cache :shape-z :target-z)
    (multiple-value-bind (target source)
        (cl-cc/optimize::opt-ic-resolve-target site :shape-z cache)
      (assert-eq :target-z target)
      (assert-eq :megamorphic-shared source))))

;;; ─── ThinLTO / Tiered JIT / Deopt helpers (FR-301/310/312 partial) ───────

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

(deftest optimize-adaptive-compilation-threshold-reacts-to-warmup-pressure-and-failures
  "opt-adaptive-compilation-threshold lowers for warmup and increases for pressure/failures."
  (assert-= 300 (cl-cc/optimize::opt-adaptive-compilation-threshold :base 900 :warmup-p t))
  (assert-= 1800 (cl-cc/optimize::opt-adaptive-compilation-threshold :base 900 :cache-pressure 0.8))
  (assert-= 2700 (cl-cc/optimize::opt-adaptive-compilation-threshold :base 900 :failures 2))
  (assert-= 1800 (cl-cc/optimize::opt-adaptive-compilation-threshold
                  :base 900 :warmup-p t :cache-pressure 0.8 :failures 2)))

(deftest optimize-tier-transition-promotes-through-runtime-tiers
  "Tier transition helper promotes interpreter to baseline and baseline to optimized."
  (assert-eq :interpreter
             (cl-cc/optimize:opt-tier-transition :interpreter 99 :baseline-threshold 100))
  (assert-eq :baseline
             (cl-cc/optimize:opt-tier-transition :interpreter 100 :baseline-threshold 100))
  (assert-eq :baseline
             (cl-cc/optimize:opt-tier-transition :baseline 999 :optimized-threshold 1000))
  (assert-eq :optimized
             (cl-cc/optimize:opt-tier-transition :baseline 1000 :optimized-threshold 1000)))

(deftest optimize-materialize-deopt-state-maps-machine-registers-to-vm-registers
  "opt-materialize-deopt-state reconstructs VM register values from machine register snapshot."
  (let* ((frame (cl-cc/optimize::make-opt-deopt-frame
                 :vm-pc 42
                 :register-map '((:rax . :r0) (:rbx . :r1))
                 :inlined-frames nil))
         (machine '((:rax . 11) (:rbx . 22) (:rcx . 33)))
         (state (cl-cc/optimize::opt-materialize-deopt-state frame machine)))
    (assert-equal '((:r0 . 11) (:r1 . 22)) state)))

(deftest optimize-osr-trigger-p-uses-hotness-threshold
  "opt-osr-trigger-p fires when hotness reaches threshold."
  (let ((osr (cl-cc/optimize::make-opt-osr-point
              :loop-id :L1
              :vm-pc 77
              :live-registers nil
              :hotness 1200)))
    (assert-true (cl-cc/optimize::opt-osr-trigger-p osr :threshold 1000))
    (assert-false (cl-cc/optimize::opt-osr-trigger-p osr :threshold 2000))))

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

(deftest optimize-shape-descriptor-slots-map-to-stable-offsets
  "make-opt-shape-descriptor-for-slots assigns deterministic slot offsets."
  (let* ((shape (cl-cc/optimize::make-opt-shape-descriptor-for-slots
                 7 '(name age active))))
    (assert-= 0 (cl-cc/optimize::opt-shape-slot-offset shape 'name))
    (assert-= 1 (cl-cc/optimize::opt-shape-slot-offset shape 'age))
    (assert-= 2 (cl-cc/optimize::opt-shape-slot-offset shape 'active))))

(deftest optimize-shape-transition-cache-stores-forward-transitions
  "Shape transition cache stores and resolves forward-only transitions."
  (let ((cache (cl-cc/optimize::make-opt-shape-transition-cache :max-size 2)))
    (cl-cc/optimize::opt-shape-transition-put cache 1 'slot-a 2)
    (cl-cc/optimize::opt-shape-transition-put cache 2 'slot-b 3)
    (multiple-value-bind (child found)
        (cl-cc/optimize::opt-shape-transition-get cache 1 'slot-a)
      (assert-true found)
      (assert-= 2 child))))

(deftest optimize-ic-make-patch-plan-classifies-state-transitions
  "opt-ic-make-patch-plan assigns expected patch kinds for IC promotions."
  (assert-eq :install-monomorphic
             (cl-cc/optimize::opt-ic-patch-patch-kind
              (cl-cc/optimize::opt-ic-make-patch-plan :site1 :uninitialized :monomorphic :t1)))
  (assert-eq :promote-polymorphic
             (cl-cc/optimize::opt-ic-patch-patch-kind
              (cl-cc/optimize::opt-ic-make-patch-plan :site2 :monomorphic :polymorphic :t2)))
  (assert-eq :promote-megamorphic
             (cl-cc/optimize::opt-ic-patch-patch-kind
              (cl-cc/optimize::opt-ic-make-patch-plan :site3 :polymorphic :megamorphic :t3))))

(deftest optimize-build-inline-polymorphic-dispatch-builds-guard-chain
  "opt-build-inline-polymorphic-dispatch returns one guard record per observed shape."
  (let* ((entries '((:shape-a . :method-a) (:shape-b . :method-b)))
         (chain (cl-cc/optimize::opt-build-inline-polymorphic-dispatch entries :obj)))
    (assert-= 2 (length chain))
    (assert-eq :shape-a (getf (first chain) :shape))
    (assert-eq :obj (getf (first chain) :receiver))
    (assert-eq :method-b (getf (second chain) :target))))

(deftest optimize-build-async-state-machine-builds-linear-transitions
  "opt-build-async-state-machine creates one transition per await point."
  (let* ((sm (cl-cc/optimize::opt-build-async-state-machine '(:await-1 :await-2)))
         (trans (cl-cc/optimize::opt-async-sm-transitions sm)))
    (assert-= 3 (length (cl-cc/optimize::opt-async-sm-states sm)))
    (assert-= 2 (length trans))
    (assert-eq :await-1 (getf (first trans) :await))
    (assert-= 2 (getf (second trans) :to))))

(deftest optimize-choose-coroutine-lowering-strategy-prefers-stackful-when-needed
  "Strategy chooser returns stackful for call/cc or deep yield, stackless otherwise."
  (assert-eq :stackless
             (cl-cc/optimize::opt-choose-coroutine-lowering-strategy
              :supports-call/cc nil :deep-yield-p nil))
  (assert-eq :stackful
             (cl-cc/optimize::opt-choose-coroutine-lowering-strategy
              :supports-call/cc t :deep-yield-p nil))
  (assert-eq :stackful
             (cl-cc/optimize::opt-choose-coroutine-lowering-strategy
              :supports-call/cc nil :deep-yield-p t)))

(deftest optimize-channel-select-path-classifies-buffered-sync-and-contended-cases
  "Channel helper classifies fast buffered, synchronous, and contended paths."
  (let ((buffered (cl-cc/optimize::make-opt-channel-site
                   :buffer-size 8 :queue-depth 2 :contention 0 :select-arity 2))
        (sync (cl-cc/optimize::make-opt-channel-site
               :buffer-size 0 :queue-depth 0 :contention 0 :select-arity 2))
        (contended (cl-cc/optimize::make-opt-channel-site
                    :buffer-size 8 :queue-depth 2 :contention 9 :select-arity 2)))
    (assert-eq :fast-buffered (cl-cc/optimize::opt-channel-select-path buffered))
    (assert-eq :synchronous-rendezvous (cl-cc/optimize::opt-channel-select-path sync))
    (assert-eq :contended-fallback (cl-cc/optimize::opt-channel-select-path contended))))

(deftest optimize-channel-jump-table-select-threshold
  "Jump-table select is enabled when select arity meets threshold."
  (let ((small (cl-cc/optimize::make-opt-channel-site
                :buffer-size 1 :queue-depth 0 :contention 0 :select-arity 2))
        (large (cl-cc/optimize::make-opt-channel-site
                :buffer-size 1 :queue-depth 0 :contention 0 :select-arity 6)))
    (assert-false (cl-cc/optimize::opt-channel-should-jump-table-select-p small :threshold 4))
    (assert-true (cl-cc/optimize::opt-channel-should-jump-table-select-p large :threshold 4))))

(deftest optimize-stm-plan-skips-log-for-pure-block
  "STM helper marks pure block as log-free."
  (let ((plan (cl-cc/optimize::opt-stm-build-plan
               :reads '(:x) :writes nil :pure-p t)))
    (assert-false (cl-cc/optimize::opt-stm-needs-log-p plan))
    (assert-false (cl-cc/optimize::opt-stm-plan-inline-log-p plan))))

(deftest optimize-stm-plan-enables-log-for-impure-read-write
  "STM helper enables logging for impure transactional access."
  (let ((plan (cl-cc/optimize::opt-stm-build-plan
               :reads '(:x) :writes '(:y) :pure-p nil)))
    (assert-true (cl-cc/optimize::opt-stm-needs-log-p plan))
    (assert-true (cl-cc/optimize::opt-stm-plan-inline-log-p plan))))

(deftest optimize-lockfree-select-reclamation-chooses-policy-from-risk-and-contention
  "Lock-free helper selects reclamation strategy based on ABA risk/contestion."
  (assert-eq :none
             (cl-cc/optimize::opt-lockfree-select-reclamation
              :aba-risk-p nil :contention 10))
  (assert-eq :hazard-pointer
             (cl-cc/optimize::opt-lockfree-select-reclamation
              :aba-risk-p t :contention 1))
  (assert-eq :epoch
             (cl-cc/optimize::opt-lockfree-select-reclamation
              :aba-risk-p t :contention 6)))

(deftest optimize-build-cfi-plan-selects-target-specific-guards
  "CFI helper selects ENDBR64 for x86-64 and BTI for AArch64 when indirect calls exist."
  (let ((x86 (cl-cc/optimize::opt-build-cfi-plan :target :x86-64 :has-indirect-calls-p t))
        (arm (cl-cc/optimize::opt-build-cfi-plan :target :aarch64 :has-indirect-calls-p t)))
    (assert-true (cl-cc/optimize::opt-cfi-plan-insert-endbr64-p x86))
    (assert-false (cl-cc/optimize::opt-cfi-plan-insert-bti-p x86))
    (assert-true (cl-cc/optimize::opt-cfi-plan-insert-bti-p arm))
    (assert-false (cl-cc/optimize::opt-cfi-plan-insert-endbr64-p arm))))

(deftest optimize-cfi-entry-opcode-materializes-selected-marker
  "CFI entry opcode helper exposes the target marker selected by the CFI plan."
  (assert-eq :endbr64
             (cl-cc/optimize:opt-cfi-entry-opcode
              (cl-cc/optimize:opt-build-cfi-plan
               :target :x86-64 :has-indirect-calls-p t)))
  (assert-eq :bti-c
             (cl-cc/optimize:opt-cfi-entry-opcode
              (cl-cc/optimize:opt-build-cfi-plan
               :target :aarch64 :has-indirect-calls-p t)))
  (assert-eq :none
             (cl-cc/optimize:opt-cfi-entry-opcode
              (cl-cc/optimize:opt-build-cfi-plan
               :target :x86-64 :has-indirect-calls-p nil))))

(deftest optimize-should-use-retpoline-p-depends-on-target-and-ibrs
  "Retpoline helper enables mitigation only for x86-64 indirect branches without IBRS."
  (assert-true (cl-cc/optimize::opt-should-use-retpoline-p
                :target :x86-64 :has-indirect-branch-p t :supports-ibrs-p nil))
  (assert-false (cl-cc/optimize::opt-should-use-retpoline-p
                 :target :x86-64 :has-indirect-branch-p t :supports-ibrs-p t))
  (assert-false (cl-cc/optimize::opt-should-use-retpoline-p
                 :target :aarch64 :has-indirect-branch-p t :supports-ibrs-p nil)))

(deftest optimize-retpoline-thunk-name-is-target-register-specific
  "Retpoline thunk names are deterministic and register-specific."
  (assert-equal "__clcc_retpoline_r11"
                (cl-cc/optimize:opt-retpoline-thunk-name :r11))
  (assert-equal "__clcc_retpoline_rax"
                (cl-cc/optimize:opt-retpoline-thunk-name :rax)))

(deftest optimize-needs-stack-canary-p-follows-stack-buffer-presence
  "Stack canary helper is enabled when stack buffer is present."
  (assert-true (cl-cc/optimize::opt-needs-stack-canary-p :has-stack-buffer-p t))
  (assert-false (cl-cc/optimize::opt-needs-stack-canary-p :has-stack-buffer-p nil)))

(deftest optimize-stack-canary-emit-plan-describes-prologue-and-epilogue
  "Stack canary emission plan records guard slot and failure target only when enabled."
  (let ((enabled (cl-cc/optimize:opt-stack-canary-emit-plan
                  :has-stack-buffer-p t
                  :guard-slot -8
                  :failure-target 'panic))
        (disabled (cl-cc/optimize:opt-stack-canary-emit-plan
                   :has-stack-buffer-p nil)))
    (assert-true (getf enabled :enabled-p))
    (assert-= -8 (getf enabled :guard-slot))
    (assert-eq :tls-canary (getf enabled :load-source))
    (assert-eq 'panic (getf enabled :failure-target))
    (assert-false (getf disabled :enabled-p))
    (assert-null (getf disabled :guard-slot))))

(deftest optimize-stack-canary-sequences-describe-prologue-and-epilogue-ops
  "Stack canary sequence helpers expose backend-neutral prologue/epilogue ops."
  (let ((plan (cl-cc/optimize:opt-stack-canary-emit-plan
               :has-stack-buffer-p t
               :guard-slot -8
               :failure-target 'panic)))
    (assert-equal '((:op :load-canary :source :tls-canary :dst :tmp)
                    (:op :store-canary :src :tmp :slot -8))
                  (cl-cc/optimize:opt-stack-canary-prologue-seq
                   plan :temp-reg :tmp))
    (assert-equal '((:op :load-canary :source -8 :dst :tmp)
                    (:op :compare-canary :left :tmp :right :tls-canary)
                    (:op :branch-if-canary-mismatch :target panic))
                  (cl-cc/optimize:opt-stack-canary-epilogue-seq
                   plan :temp-reg :tmp))))

(deftest optimize-stack-canary-sequences-are-empty-when-disabled
  "Stack canary sequence helpers return NIL for disabled plans."
  (let ((plan (cl-cc/optimize:opt-stack-canary-emit-plan
               :has-stack-buffer-p nil)))
    (assert-null (cl-cc/optimize:opt-stack-canary-prologue-seq plan))
    (assert-null (cl-cc/optimize:opt-stack-canary-epilogue-seq plan))))

(deftest optimize-build-shadow-stack-plan-enables-only-for-x86-64-with-cet
  "Shadow stack helper enables only for x86-64 targets with CET SS support."
  (let ((enabled (cl-cc/optimize:opt-build-shadow-stack-plan
                  :target :x86-64
                  :supports-cet-ss-p t))
        (no-cet (cl-cc/optimize:opt-build-shadow-stack-plan
                 :target :x86-64
                 :supports-cet-ss-p nil))
        (wrong-arch (cl-cc/optimize:opt-build-shadow-stack-plan
                     :target :aarch64
                     :supports-cet-ss-p t)))
    (assert-true (cl-cc/optimize:opt-shadow-stack-plan-enabled-p enabled))
    (assert-eq :x86-64 (cl-cc/optimize:opt-shadow-stack-plan-target enabled))
    (assert-true (cl-cc/optimize:opt-shadow-stack-plan-needs-incsssp-p enabled))
    (assert-false (cl-cc/optimize:opt-shadow-stack-plan-needs-save-restore-p enabled))
    (assert-false (cl-cc/optimize:opt-shadow-stack-plan-enabled-p no-cet))
    (assert-false (cl-cc/optimize:opt-shadow-stack-plan-enabled-p wrong-arch))))

(deftest optimize-shadow-stack-plan-requires-save-restore-for-nonlocal-control
  "Shadow stack helper flags save/restore when continuations or setjmp-like flow exist."
  (let ((plan (cl-cc/optimize:opt-build-shadow-stack-plan
               :target :x86-64
               :supports-cet-ss-p t
               :has-nonlocal-control-p t)))
    (assert-true (cl-cc/optimize:opt-shadow-stack-plan-needs-save-restore-p plan))))

(deftest optimize-wasm-select-tailcall-opcode-uses-return-call-forms
  "Wasm tail-call helper selects return-call opcodes when enabled at tail position."
  (assert-eq :return-call
             (cl-cc/optimize::opt-wasm-select-tailcall-opcode
              :tail-position-p t :indirect-p nil :enabled-p t))
  (assert-eq :return-call-indirect
             (cl-cc/optimize::opt-wasm-select-tailcall-opcode
              :tail-position-p t :indirect-p t :enabled-p t))
  (assert-eq :call
             (cl-cc/optimize::opt-wasm-select-tailcall-opcode
              :tail-position-p t :indirect-p nil :enabled-p nil)))

(deftest optimize-build-wasm-gc-layout-preserves-kind-and-fields
  "Wasm GC helper stores layout kind/fields/nullability deterministically."
  (let ((layout (cl-cc/optimize::opt-build-wasm-gc-layout
                 :kind :struct
                 :fields '((slot-a . i32) (slot-b . externref))
                 :nullable-p t)))
    (assert-eq :struct (cl-cc/optimize::opt-wasm-gc-kind layout))
    (assert-equal '((slot-a . i32) (slot-b . externref))
                  (cl-cc/optimize::opt-wasm-gc-fields layout))
    (assert-true (cl-cc/optimize::opt-wasm-gc-nullable-p layout))))

(deftest optimize-build-dwarf-line-row-preserves-location-fields
  "DWARF helper materializes address and source location fields."
  (let* ((loc (cl-cc/optimize::make-opt-debug-loc
               :file "src/foo.lisp" :line 42 :column 7 :symbol 'foo))
         (row (cl-cc/optimize::opt-build-dwarf-line-row #x1000 loc)))
    (assert-eq #x1000 (getf row :address))
    (assert-equal "src/foo.lisp" (getf row :file))
    (assert-= 42 (getf row :line))
    (assert-= 7 (getf row :column))))

(deftest optimize-build-wasm-source-map-entry-preserves-offset-and-source
  "Wasm source-map helper keeps wasm offset and original source coordinates."
  (let* ((loc (cl-cc/optimize::make-opt-debug-loc
               :file "src/bar.lisp" :line 10 :column 3 :symbol 'bar))
         (entry (cl-cc/optimize::opt-build-wasm-source-map-entry 128 loc)))
    (assert-= 128 (getf entry :offset))
    (assert-equal "src/bar.lisp" (getf entry :source))
    (assert-= 10 (getf entry :line))
    (assert-= 3 (getf entry :column))))

(deftest optimize-format-diagnostic-reason-renders-rpass-like-message
  "Diagnostic helper formats pass outcome and reason consistently."
  (assert-equal "inline: skipped (callee too large)"
                (cl-cc/optimize::opt-format-diagnostic-reason
                 "inline" "skipped" "callee too large")))

(deftest optimize-build-tls-plan-selects-architecture-specific-base-register
  "TLS helper chooses FS on x86-64 and TPIDR_EL0 on AArch64 for hot accesses."
  (let ((x86 (cl-cc/optimize::opt-build-tls-plan :target :x86-64 :hot-access-p t))
        (arm (cl-cc/optimize::opt-build-tls-plan :target :aarch64 :hot-access-p t)))
    (assert-true (cl-cc/optimize::opt-tls-plan-uses-inline-tls-p x86))
    (assert-eq :fs (cl-cc/optimize::opt-tls-plan-base-register x86))
    (assert-eq :tpidr_el0 (cl-cc/optimize::opt-tls-plan-base-register arm))))

(deftest optimize-select-atomic-opcode-reflects-target-and-operation
  "Atomic helper picks target-specific representative opcodes for incf/cas."
  (assert-eq :lock-xadd
             (cl-cc/optimize::opt-select-atomic-opcode
              :target :x86-64 :operation :incf :memory-order :acq-rel))
  (assert-eq :lock-cmpxchg
             (cl-cc/optimize::opt-select-atomic-opcode
              :target :x86-64 :operation :cas :memory-order :seq-cst))
  (assert-eq :ldadd
             (cl-cc/optimize::opt-select-atomic-opcode
              :target :aarch64 :operation :incf :memory-order :acq-rel))
  (assert-eq :ldxr-stxr
             (cl-cc/optimize::opt-select-atomic-opcode
              :target :aarch64 :operation :cas :memory-order :seq-cst)))

(deftest optimize-build-htm-plan-enables-lock-elision-only-when-supported-and-low-contention
  "HTM helper enables lock elision only under support + low-contention preconditions."
  (let ((enabled (cl-cc/optimize::opt-build-htm-plan
                  :target :x86-64
                  :supports-htm-p t
                  :low-contention-p t))
        (disabled (cl-cc/optimize::opt-build-htm-plan
                   :target :x86-64
                   :supports-htm-p t
                   :low-contention-p nil)))
    (assert-true (cl-cc/optimize::opt-htm-plan-uses-htm-p enabled))
    (assert-eq :xbegin (cl-cc/optimize::opt-htm-plan-begin-opcode enabled))
    (assert-eq :xend (cl-cc/optimize::opt-htm-plan-end-opcode enabled))
    (assert-eq :xabort (cl-cc/optimize::opt-htm-plan-abort-opcode enabled))
    (assert-true (cl-cc/optimize::opt-htm-plan-fallback-lock-p enabled))
    (assert-false (cl-cc/optimize::opt-htm-plan-uses-htm-p disabled))))

(deftest optimize-build-concurrent-gc-plan-selects-satb-and-short-stw-for-latency-sensitive-mode
  "Concurrent GC helper selects SATB + short STW phases for latency-sensitive workloads."
  (let ((concurrent (cl-cc/optimize::opt-build-concurrent-gc-plan
                     :latency-sensitive-p t
                     :heap-size (* 128 1024 1024)))
        (stw (cl-cc/optimize::opt-build-concurrent-gc-plan
              :latency-sensitive-p nil
              :heap-size (* 128 1024 1024))))
    (assert-true (cl-cc/optimize::opt-conc-gc-plan-concurrent-mark-p concurrent))
    (assert-eq :satb (cl-cc/optimize::opt-conc-gc-plan-write-barrier concurrent))
    (assert-true (cl-cc/optimize::opt-conc-gc-plan-mutator-assist-p concurrent))
    (assert-equal '(:initial-mark :final-remark)
                  (cl-cc/optimize::opt-conc-gc-plan-stw-phases concurrent))
    (assert-false (cl-cc/optimize::opt-conc-gc-plan-concurrent-mark-p stw))
    (assert-eq :incremental-update (cl-cc/optimize::opt-conc-gc-plan-write-barrier stw))
    (assert-equal '(:full-mark-sweep)
                  (cl-cc/optimize::opt-conc-gc-plan-stw-phases stw))))

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
