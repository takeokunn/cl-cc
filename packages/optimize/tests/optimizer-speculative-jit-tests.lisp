;;;; tests/unit/optimize/optimizer-speculative-jit-tests.lisp
;;;; Unit tests for speculative JIT helpers — ThinLTO, PGO, tiered JIT, deopt,
;;;; OSR, shape transitions, IC patch, async state machines, channels, STM,
;;;; lock-free, CFI, retpoline, stack canary, shadow stack, Wasm GC,
;;;; DWARF/source-map debug, TLS, atomics, HTM, concurrent GC.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

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

(deftest-each optimize-tier-transition-cases
  "opt-tier-transition promotes interpreter→baseline at threshold, baseline→optimized at threshold."
  :cases (("interpreter-below-threshold" (cl-cc/optimize:opt-tier-transition :interpreter 99  :baseline-threshold 100)  :interpreter)
          ("interpreter-at-threshold"    (cl-cc/optimize:opt-tier-transition :interpreter 100 :baseline-threshold 100)  :baseline)
          ("baseline-below-threshold"    (cl-cc/optimize:opt-tier-transition :baseline 999   :optimized-threshold 1000) :baseline)
          ("baseline-at-threshold"       (cl-cc/optimize:opt-tier-transition :baseline 1000  :optimized-threshold 1000) :optimized))
  (result expected)
  (assert-eq expected result))

(deftest optimize-materialize-deopt-state-maps-machine-registers-to-vm-registers
  "opt-materialize-deopt-state reconstructs VM register values from machine register snapshot."
  (let* ((frame (cl-cc/optimize::make-opt-deopt-frame
                 :vm-pc 42
                 :register-map '((:rax . :r0) (:rbx . :r1))
                 :inlined-frames nil))
         (machine '((:rax . 11) (:rbx . 22) (:rcx . 33)))
         (state (cl-cc/optimize::opt-materialize-deopt-state frame machine)))
    (assert-equal '((:r0 . 11) (:r1 . 22)) state)))

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

(deftest-each optimize-shape-descriptor-slot-offsets
  "make-opt-shape-descriptor-for-slots assigns consecutive, deterministic slot offsets."
  :cases (("name-at-0"   'name   0)
          ("age-at-1"    'age    1)
          ("active-at-2" 'active 2))
  (slot-name expected-offset)
  (let ((shape (cl-cc/optimize::make-opt-shape-descriptor-for-slots 7 '(name age active))))
    (assert-= expected-offset (cl-cc/optimize::opt-shape-slot-offset shape slot-name))))

(deftest optimize-shape-transition-cache-stores-forward-transitions
  "Shape transition cache stores and resolves forward-only transitions."
  (let ((cache (cl-cc/optimize::make-opt-shape-transition-cache :max-size 2)))
    (cl-cc/optimize::opt-shape-transition-put cache 1 'slot-a 2)
    (cl-cc/optimize::opt-shape-transition-put cache 2 'slot-b 3)
    (multiple-value-bind (child found)
        (cl-cc/optimize::opt-shape-transition-get cache 1 'slot-a)
      (assert-true found)
      (assert-= 2 child))))

(deftest-each optimize-ic-patch-plan-state-transitions
  "opt-ic-make-patch-plan assigns the correct patch kind for each IC state promotion."
  :cases (("uninit-to-mono" :uninitialized :monomorphic  :install-monomorphic)
          ("mono-to-poly"   :monomorphic   :polymorphic  :promote-polymorphic)
          ("poly-to-mega"   :polymorphic   :megamorphic  :promote-megamorphic))
  (from to expected-kind)
  (assert-eq expected-kind
             (cl-cc/optimize::opt-ic-patch-patch-kind
              (cl-cc/optimize::opt-ic-make-patch-plan :site from to :target))))

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

(deftest-each optimize-stm-plan-pure-vs-impure
  "STM plan helper marks pure blocks log-free; impure read-write blocks need logs."
  :cases (("pure-no-log"     '(:x) nil   t   nil nil)
          ("impure-with-log" '(:x) '(:y) nil t   t))
  (reads writes pure-p expected-needs-log expected-inline-log)
  (let ((plan (cl-cc/optimize::opt-stm-build-plan
               :reads reads :writes writes :pure-p pure-p)))
    (assert-equal expected-needs-log   (not (null (cl-cc/optimize::opt-stm-needs-log-p plan))))
    (assert-equal expected-inline-log  (not (null (cl-cc/optimize::opt-stm-plan-inline-log-p plan))))))

(deftest-each optimize-lockfree-reclamation-policy-selection
  "Lock-free helper selects reclamation strategy from ABA risk + contention level."
  :cases (("no-risk-none"     nil 10 :none)
          ("aba-low-hazard"   t   1  :hazard-pointer)
          ("aba-high-epoch"   t   6  :epoch))
  (aba-risk-p contention expected)
  (assert-eq expected
             (cl-cc/optimize::opt-lockfree-select-reclamation
              :aba-risk-p aba-risk-p :contention contention)))

(deftest optimize-build-cfi-plan-selects-target-specific-guards
  "CFI helper selects ENDBR64 for x86-64 and BTI for AArch64 when indirect calls exist."
  (let ((x86 (cl-cc/optimize::opt-build-cfi-plan :target :x86-64 :has-indirect-calls-p t))
        (arm (cl-cc/optimize::opt-build-cfi-plan :target :aarch64 :has-indirect-calls-p t)))
    (assert-true (cl-cc/optimize::opt-cfi-plan-insert-endbr64-p x86))
    (assert-false (cl-cc/optimize::opt-cfi-plan-insert-bti-p x86))
    (assert-true (cl-cc/optimize::opt-cfi-plan-insert-bti-p arm))
    (assert-false (cl-cc/optimize::opt-cfi-plan-insert-endbr64-p arm))))

(deftest-each optimize-cfi-entry-opcode-by-target-and-calls
  "CFI entry opcode reflects target architecture and indirect-call presence."
  :cases (("x86-with-calls"  :x86-64  t   :endbr64)
          ("arm-with-calls"  :aarch64 t   :bti-c)
          ("x86-no-calls"    :x86-64  nil :none))
  (target has-indirect-calls-p expected)
  (assert-eq expected
             (cl-cc/optimize:opt-cfi-entry-opcode
              (cl-cc/optimize:opt-build-cfi-plan
               :target target :has-indirect-calls-p has-indirect-calls-p))))

(deftest-each optimize-should-use-retpoline-p-cases
  "opt-should-use-retpoline-p enables mitigation only for x86-64 with indirect branches and no IBRS."
  :cases (("x86-no-ibrs"   :x86-64  nil t)
          ("x86-with-ibrs" :x86-64  t   nil)
          ("aarch64"       :aarch64 nil nil))
  (target supports-ibrs-p expected)
  (assert-equal expected
                (cl-cc/optimize::opt-should-use-retpoline-p
                 :target target :has-indirect-branch-p t :supports-ibrs-p supports-ibrs-p)))

(deftest-each optimize-retpoline-thunk-names-are-register-specific
  "Retpoline thunk names are deterministic and register-specific."
  :cases (("r11" :r11 "__clcc_retpoline_r11")
          ("rax" :rax "__clcc_retpoline_rax"))
  (register expected)
  (assert-equal expected (cl-cc/optimize:opt-retpoline-thunk-name register)))

(deftest-each optimize-needs-stack-canary-p-cases
  "opt-needs-stack-canary-p is enabled iff :has-stack-buffer-p is true."
  :cases (("with-buffer"    t   t)
          ("without-buffer" nil nil))
  (has-stack-buffer-p expected)
  (assert-equal expected
                (cl-cc/optimize::opt-needs-stack-canary-p
                 :has-stack-buffer-p has-stack-buffer-p)))

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

(deftest-each optimize-wasm-tailcall-opcode-selection
  "Wasm tail-call helper selects the right opcode from (tail × indirect × enabled)."
  :cases (("tail-direct-enabled"   t nil t   :return-call)
          ("tail-indirect-enabled" t t   t   :return-call-indirect)
          ("tail-disabled"         t nil nil :call))
  (tail-position-p indirect-p enabled-p expected)
  (assert-eq expected
             (cl-cc/optimize::opt-wasm-select-tailcall-opcode
              :tail-position-p tail-position-p
              :indirect-p      indirect-p
              :enabled-p       enabled-p)))

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

(deftest optimize-wasm-gc-layout-validates-struct-and-array-shapes
  "Wasm GC validation helper accepts legal struct/array layouts only."
  (let ((struct-layout (cl-cc/optimize:opt-build-wasm-gc-layout
                        :kind :struct
                        :fields '((slot-a . i32) (slot-b . eqref))
                        :nullable-p t))
        (array-layout (cl-cc/optimize:opt-build-wasm-gc-layout
                       :kind :array
                       :fields '(eqref)
                       :nullable-p nil))
        (bad-array-layout (cl-cc/optimize:opt-build-wasm-gc-layout
                           :kind :array
                           :fields '(eqref i32)
                           :nullable-p nil)))
    (assert-true (cl-cc/optimize:opt-wasm-gc-layout-valid-p struct-layout))
    (assert-true (cl-cc/optimize:opt-wasm-gc-layout-valid-p array-layout))
    (assert-false (cl-cc/optimize:opt-wasm-gc-layout-valid-p bad-array-layout))))

(deftest optimize-wasm-gc-runtime-host-compatibility-requires-feature-and-valid-layout
  "Host-compatibility helper gates lowering on wasm-gc support and layout validity."
  (let ((layout (cl-cc/optimize:opt-build-wasm-gc-layout
                 :kind :struct
                 :fields '((slot-a . i32))
                 :nullable-p t))
        (bad-layout (cl-cc/optimize:opt-build-wasm-gc-layout
                     :kind :array
                     :fields '(eqref i32)
                     :nullable-p nil)))
    (assert-true
     (cl-cc/optimize:opt-wasm-gc-runtime-host-compatible-p
      layout
      :host-supports-wasm-gc-p t))
    (assert-false
     (cl-cc/optimize:opt-wasm-gc-runtime-host-compatible-p
      layout
      :host-supports-wasm-gc-p nil))
    (assert-false
     (cl-cc/optimize:opt-wasm-gc-runtime-host-compatible-p
      bad-layout
      :host-supports-wasm-gc-p t))))

(deftest optimize-wasm-gc-optimization-plan-reflects-layout-kind
  "Optimization-plan helper enables struct vs array specific lowering hints."
  (let* ((struct-layout (cl-cc/optimize:opt-build-wasm-gc-layout
                         :kind :struct
                         :fields '((slot-a . i32))
                         :nullable-p t))
         (array-layout (cl-cc/optimize:opt-build-wasm-gc-layout
                        :kind :array
                        :fields '(eqref)
                        :nullable-p nil))
         (struct-plan (cl-cc/optimize:opt-build-wasm-gc-optimization-plan struct-layout))
         (array-plan (cl-cc/optimize:opt-build-wasm-gc-optimization-plan array-layout)))
    (assert-true (getf struct-plan :layout-valid-p))
    (assert-true (getf struct-plan :inline-field-access-p))
    (assert-false (getf struct-plan :bounds-check-elision-p))
    (assert-true (getf array-plan :layout-valid-p))
    (assert-false (getf array-plan :inline-field-access-p))
    (assert-true (getf array-plan :bounds-check-elision-p))))

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

(deftest-each optimize-select-atomic-opcode-target-operation-matrix
  "Atomic helper picks the correct target-specific opcode for each (target × operation × order) combination."
  :cases (("x86-incf-acq-rel" :x86-64  :incf :acq-rel :lock-xadd)
          ("x86-cas-seq-cst"  :x86-64  :cas  :seq-cst :lock-cmpxchg)
          ("arm-incf-acq-rel" :aarch64 :incf :acq-rel :ldadd)
          ("arm-cas-seq-cst"  :aarch64 :cas  :seq-cst :ldxr-stxr))
  (target operation memory-order expected)
  (assert-eq expected
             (cl-cc/optimize::opt-select-atomic-opcode
              :target target :operation operation :memory-order memory-order)))

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
