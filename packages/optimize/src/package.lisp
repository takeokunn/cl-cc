;;;; packages/optimize/src/package.lisp — cl-cc/optimize package definition
;;;;
;;;; Optimizer subsystem: effect-kind classification, CFG (basic blocks,
;;;; RPO, dominators, loop depths), SSA (phi placement, renaming, round-trip),
;;;; E-graph (equality saturation, rewrite rules, extraction), and the
;;;; multi-pass optimizer pipeline (tables, strength reduction, copy propagation,
;;;; SCCP, CSE/GVN, LICM/PRE, inlining, memory/alias analysis, DCE/jump
;;;; threading, flow passes, algebraic identities).
;;;;
;;;; Phase 3b: Real ASDF system (owns all source files).

;;; Bootstrap provides binop/const/var/cmp atoms used by egraph-rules as
;;; Prolog predicate keys. cl-cc/vm provides all VM instruction types and
;;; accessors. cl-cc/prolog provides def-fact, query-all, and apply-prolog-peephole.
;;; cl-cc/type is accessed qualified (cl-cc/type:...) so not in :use.

(defpackage :cl-cc/optimize
  (:use :cl :cl-cc/bootstrap :cl-cc/vm :cl-cc/prolog)
  (:shadowing-import-from :cl-cc/vm
    ;; defpackage :shadow from facade-package-defpackage.lisp
    #:get-universal-time #:get-internal-real-time #:get-internal-run-time
    #:internal-time-units-per-second #:sleep #:time
    #:encode-universal-time #:decode-universal-time
    #:random-state #:random-state-p #:make-random-state #:*random-state* #:random
    #:*print-base* #:*print-radix* #:*print-circle*
    #:*print-pretty* #:*print-level* #:*print-length*
    #:*print-readably* #:*print-pprint-dispatch*
    #:with-standard-io-syntax
    #:pprint-logical-block #:pprint-indent #:pprint-newline #:pprint-tab
    #:copy-pprint-dispatch #:set-pprint-dispatch #:get-pprint-dispatch
    #:*readtable* #:copy-readtable
    #:set-macro-character #:get-macro-character
    #:set-dispatch-macro-character #:get-dispatch-macro-character
    #:readtable-case
    ;; eval-when (shadow ...) from vm.lisp
    #:lisp-implementation-type #:lisp-implementation-version
    #:machine-type #:machine-version #:machine-instance
    #:software-type #:software-version
    #:room #:apropos #:apropos-list)
  (:export
   ;; ─── effects.lisp — effect-kind classification ─────────────────────
   #:vm-inst-effect-kind
   #:opt-inst-pure-p
   #:opt-inst-dce-eligible-p
    #:opt-inst-cse-eligible-p
    #:effect-row->effect-kind
    #:*known-function-property-table*
    #:register-known-function-properties
    #:known-function-properties
    #:known-function-property-p
    #:known-function-effect-kind

    ;; ─── cfg.lisp — basic blocks and CFG ───────────────────────────────
   #:basic-block #:make-basic-block #:basic-block-p
   #:bb-id #:bb-label #:bb-instructions
   #:bb-predecessors #:bb-successors
   #:bb-idom #:bb-dom-children #:bb-dom-frontier
    #:bb-post-idom #:bb-post-children
     #:bb-vm-osr-entry
     #:bb-loop-depth #:bb-rpo-index
    #:opt-ddg-node #:make-opt-ddg-node #:opt-ddg-node-p
    #:opt-ddg-node-index #:opt-ddg-node-inst #:opt-ddg-node-latency
    #:opt-ddg-node-predecessors #:opt-ddg-node-successors
    #:cfg #:make-cfg #:cfg-p
   #:cfg-blocks #:cfg-entry #:cfg-exit
   #:cfg-label->block #:cfg-next-id
   #:cfg-build #:cfg-block-count
   #:cfg-get-block-by-label
   #:cfg-compute-rpo #:cfg-compute-dominators
   #:cfg-compute-post-dominators
   #:cfg-compute-dominance-frontiers
   #:cfg-dominates-p #:cfg-post-dominates-p #:cfg-idf
    #:cfg-flatten
     #:cfg-mark-osr-loop-headers
    #:cfg-split-critical-edges
    #:cfg-build-ddg #:cfg-ddg-add-edge #:cfg-ddg-edges #:cfg-compute-mii

   ;; ─── ssa.lisp — SSA data structures + phi placement ────────────────
   #:ssa-rename-state #:make-ssa-rename-state #:ssa-rename-state-p
   #:ssr-counters #:ssr-stacks
   #:ssr-push-new-version #:ssr-current-version #:ssr-pop-version
   #:ssa-phi #:make-ssa-phi #:ssa-phi-p
   #:phi-dst #:phi-args #:phi-reg
   #:ssa-versioned-reg
   #:ssa-construct #:ssa-destroy #:ssa-round-trip
   #:ssa-place-phis #:ssa-rename
   #:ssa-sequentialize-copies #:ssa-eliminate-trivial-phis

   ;; ─── egraph.lisp — E-graph data structures ─────────────────────────
   #:e-node #:make-e-node #:e-node-p
   #:en-op #:en-children #:en-eclass
   #:e-class #:make-e-class #:e-class-p
   #:ec-id #:ec-nodes #:ec-parents #:ec-data
   #:e-graph #:make-e-graph #:e-graph-p
   #:eg-classes #:eg-memo #:eg-union-find #:eg-worklist #:eg-next-id

   ;; ─── egraph-saturation.lisp — saturation + extraction ──────────────
   #:egraph-find #:egraph-add #:egraph-merge #:egraph-rebuild
   #:egraph-saturate #:egraph-extract #:egraph-default-cost
   #:egraph-stats #:egraph-pattern-var-p #:egraph-match-pattern

   ;; ─── egraph-rules.lisp — rewrite rules ─────────────────────────────
   #:defrule #:egraph-rule-register #:egraph-builtin-rules

   ;; ─── egraph-rules-advanced.lisp — entry point ──────────────────────
   #:optimize-with-egraph

   ;; ─── cfg.lisp — additional CFG utilities ─────────────────────────────
   #:cfg-compute-loop-depths

    ;; ─── cfg-layout.lisp — code layout ────────────────────────────────
    #:cfg-flatten-hot-cold
    #:opt-pass-hot-cold-layout

    ;; ─── optimizer-tables.lisp — instruction introspection ──────────────
    #:opt-inst-dst #:opt-inst-read-regs #:opt-falsep

    ;; ─── optimizer-dataflow.lisp — generic dataflow helpers ─────────────
    #:opt-dataflow-result #:make-opt-dataflow-result #:opt-dataflow-result-p
    #:opt-dataflow-result-cfg #:opt-dataflow-result-direction
    #:opt-dataflow-result-in #:opt-dataflow-result-out
    #:opt-run-dataflow #:define-dataflow-pass
    #:opt-compute-available-expressions
    #:opt-compute-reaching-definitions

     ;; ─── prolog-peephole.lisp — Prolog peephole rewriting ──────────────
     #:apply-prolog-peephole
     #:opt-pass-superopt #:*opt-superopt-max-length* #:*opt-superopt-input-space*

    ;; ─── optimizer-memory-alias.lisp — alias analysis ────────────────────────
    #:opt-compute-heap-aliases
    #:opt-compute-points-to
    #:opt-points-to-root
     #:opt-compute-simple-inductions
     #:opt-compute-loop-inductions
     #:opt-induction-trip-count
     #:opt-iv-reg #:opt-iv-init #:opt-iv-step #:opt-iv-update-inst
      #:opt-compute-cfg-value-ranges
      #:opt-compute-path-sensitive-ranges
      #:opt-block-reg-range
      #:opt-compute-value-ranges
    #:opt-array-bounds-check-eliminable-p
    #:opt-mark-bounds-check-eliminable
    #:opt-bounds-check-eliminable-metadata
    #:opt-bounds-check-eliminable-marked-p
    #:opt-interval-add  #:opt-interval-sub
    #:opt-interval-bit-width
    #:opt-interval-known-bits-mask
    #:opt-interval-fits-fixnum-width-p
    #:opt-interval-fits-fixnum-p
    #:opt-interval-widen
    #:opt-interval-logand
    #:opt-pass-elide-proven-overflow-checks
    #:opt-may-alias-by-type-p  #:opt-may-alias-p  #:opt-must-alias-p
    #:opt-compute-heap-type-facts
    #:opt-tbaa-must-not-alias-p
    #:opt-may-alias-with-tbaa-p
    #:opt-prune-dominated-safepoints
    #:opt-hoist-safepoints-to-back-edges
    #:opt-sink-allocations
    #:opt-analyze-memory-access-patterns
    #:opt-pass-cons-slot-forward
    #:opt-pass-bounds-check-elimination

     ;; ─── optimizer-inline.lisp ─────────────────────────────────────────
     #:opt-known-callee-labels
     #:opt-pass-call-site-splitting
     #:opt-pass-devirtualize
     #:opt-pass-closure-capture-dedup
     #:opt-pass-closure-thunk-sharing
     #:opt-demand-summary #:make-opt-demand-summary #:opt-demand-summary-p
     #:opt-demand-summary-function #:opt-demand-summary-params
     #:opt-demand-summary-demands #:opt-demand-summary-strict-params
     #:opt-demand-summary-absent-params #:*opt-demand-summary-table*
     #:opt-analyze-function-demand #:opt-analyze-program-demand
     #:opt-pass-demand-analysis

     ;; ─── optimizer-recognition.lisp ─────────────────────────────────────
      #:opt-pass-fill-recognition
       #:opt-pass-copy-recognition
       #:opt-pass-auto-vectorization
       #:opt-pass-slp-vectorize
       #:vm-simd-vector-op
      #:make-vm-simd-vector-op
      #:vm-simd-vector-op-p
      #:opt-pass-function-outlining
      #:opt-pass-safepoint-polling
      #:opt-pass-software-pipelining
      #:opt-modulo-schedule-loop-body
      #:opt-analyze-branch-weights
      #:opt-branch-weight

     ;; ─── optimizer-flow-core.lisp ───────────────────────────────────────────
    #:opt-pass-dominated-type-check-elim
     #:opt-pass-prefetch-insertion

     ;; ─── extended optimizer pass files ──────────────────────────────────
     #:opt-pass-loop-rotate
     #:opt-pass-dead-loop-elimination
     #:opt-pass-loop-unroll
     #:opt-pass-loop-unswitch
     #:opt-pass-dead-argument-elimination
     #:opt-pass-ipcp
     #:opt-pass-tail-duplication
     #:opt-pass-iv-strength-reduce
     #:opt-pass-div-by-const
     #:opt-pass-loop-peel
     #:opt-pass-idiom-recognition
     #:opt-pass-value-range-propagation
     #:opt-pass-bounds-check-elimination
      #:opt-pass-overflow-check-elimination
      #:opt-pass-bitwidth-reduction
      #:opt-pass-cps-reduce
      #:opt-pass-defunctionalize
      #:opt-pass-delimited-continuations
       #:opt-pass-escape-analysis
        #:opt-path-profile-block #:make-opt-path-profile-block #:opt-path-profile-block-p
        #:opt-path-profile-block-block-id #:opt-path-profile-block-label
        #:opt-path-profile-block-path-id #:opt-path-profile-block-successor-count
        #:opt-path-profile-block-execution-count
        #:opt-path-profile-block-path-count
        #:opt-ball-larus-edge #:make-opt-ball-larus-edge #:opt-ball-larus-edge-p
        #:opt-ball-larus-edge-from #:opt-ball-larus-edge-to
        #:opt-ball-larus-edge-value #:opt-ball-larus-edge-backedge-p
        #:opt-ball-larus-edge-exit-p
        #:opt-ball-larus-profile #:make-opt-ball-larus-profile #:opt-ball-larus-profile-p
        #:opt-ball-larus-profile-cfg #:opt-ball-larus-profile-blocks
        #:opt-ball-larus-profile-edges #:opt-ball-larus-profile-paths
        #:opt-ball-larus-profile-instrumented-instructions
        #:opt-block-version-plan #:make-opt-block-version-plan #:opt-block-version-plan-p
        #:opt-block-version-plan-hot-threshold #:opt-block-version-plan-versions
        #:opt-compute-path-profile #:opt-build-ball-larus-profile
        #:opt-instrument-path-profile #:opt-identify-hot-paths
        #:opt-build-block-version-plan #:opt-duplicate-hot-paths #:opt-pass-path-profiling
       #:opt-load-widening-candidate-p
       #:opt-pass-store-coalescing #:opt-pass-load-widening-store-coalescing
      #:opt-pass-optimization-remarks
        #:opt-pass-loop-fusion
        #:opt-pass-loop-fission
        #:opt-pass-loop-tile
        #:*autotune-simd-enabled*
        #:autotune-simd-cache-info
        #:autotune-simd-tile-sizes
        #:opt-pass-autotune-simd
        #:*abstract-interp-enabled*
       #:*abstract-interp-last-state*
       #:ai-alpha
       #:ai-gamma
       #:ai-compute-fixed-point
       #:opt-pass-abstract-interpretation
        #:*translation-validation-enabled*
        #:tv-symbolic-execute-block
        #:translation-validation-error
        #:validate-translation
        #:validate-optimizer-translation
        #:opt-pass-translation-validation
       #:*polyhedral-enabled*
      #:polyhedral-domain #:make-polyhedral-domain #:polyhedral-domain-p
      #:poly-domain-dimensions #:poly-domain-constraints
      #:polyhedral-access #:make-polyhedral-access #:polyhedral-access-p
      #:poly-access-array-reg #:poly-access-write-p
      #:poly-access-coefficients #:poly-access-offset
      #:polyhedral-statement #:make-polyhedral-statement #:polyhedral-statement-p
      #:poly-stmt-loops #:poly-stmt-domain #:poly-stmt-accesses
       #:poly-stmt-schedule #:poly-stmt-body
       #:polyhedral-build-domain #:polyhedral-loop-interchange
       #:polyhedral-tile #:polyhedral-fuse #:opt-pass-polyhedral
       #:opt-run-compiler-fuzz
       #:*mlgo-enabled*
      #:*mlgo-inline-weights*
      #:opt-mlgo-function-features
      #:opt-mlgo-inline-benefit
       #:opt-mlgo-inline-threshold
       #:opt-ml-inline-score-plan
       #:opt-pass-mlgo-inline
       #:opt-pass-ml-regalloc

       ;; ─── optimizer-pipeline.lisp — top-level entry point ───────────────
       #:optimize-instructions
       #:compiler-self-profiling-capabilities
       #:build-analytics-summary
      #:*optimization-report-stream*
       #:*block-compile*
       #:*max-inline-size*
       #:*skip-optimizer-passes*
     #:*verify-optimizer-instructions*
     #:*opt-enable-pure-call-optimization*
     #:*opt-enable-sealed-gf-devirtualization*
     #:opt-pass-schedule-local
     #:opt-configure-optimization-policy
    #:optimize-roadmap-doc-features
    #:optimize-roadmap-doc-fr-ids
    #:optimize-roadmap-register-doc-evidence
    #:lookup-opt-roadmap-evidence
    #:optimize-backend-roadmap-doc-features
    #:optimize-backend-roadmap-doc-fr-ids
    #:optimize-backend-roadmap-status-summary
    #:optimize-backend-roadmap-all-fr-complete-p
    #:optimize-backend-roadmap-fr-ids-by-status
    #:optimize-backend-roadmap-register-doc-evidence
    #:lookup-opt-backend-roadmap-evidence
    #:optimize-roadmap-evidence-well-formed-p
    #:optimize-roadmap-implementation-evidence-complete-p
    #:optimize-backend-roadmap-implementation-evidence-complete-p
    #:opt-roadmap-evidence-feature-id
    #:opt-roadmap-evidence-status
    #:opt-roadmap-evidence-modules
    #:opt-roadmap-evidence-api-symbols
    #:opt-roadmap-evidence-test-anchors
    #:opt-roadmap-evidence-summary
    #:make-opt-ic-site
    #:opt-ic-site-state
    #:opt-ic-site-misses
    #:opt-ic-site-megamorphic-fallback
    #:opt-ic-transition
    #:make-opt-megamorphic-cache
    #:opt-mega-cache-put
    #:opt-mega-cache-get
    #:opt-ic-resolve-target
    #:make-opt-ic-patch-plan
     #:opt-ic-make-patch-plan
     #:opt-build-inline-polymorphic-dispatch
     #:*opt-speculative-inline-dominance-threshold*
     #:opt-ic-dominant-type
     #:opt-speculative-inline-eligible-p
     #:opt-annotate-speculative-inline
     #:opt-pass-speculative-inline
     #:make-opt-speculation-log
    #:*opt-speculation-log*
    #:opt-record-speculation-failure
    #:opt-speculation-failed-p
    #:opt-speculation-allowed-p
    #:opt-clear-speculation-log
    #:opt-save-speculation-log
    #:opt-load-speculation-log
    #:make-opt-profile-data
    #:opt-profile-record-edge
    #:opt-profile-record-value
    #:opt-profile-top-values
     #:opt-profile-value-range
     #:opt-profile-record-call-chain
     #:opt-profile-record-allocation
     #:opt-pgo-best-successor
     #:opt-pgo-build-hot-chain
     #:opt-pgo-rotate-loop
     #:opt-pgo-build-counter-plan
     #:opt-pgo-make-profile-template
      #:opt-lattice-bottom
     #:opt-lattice-constant
     #:opt-lattice-overdefined
     #:opt-lattice-meet
     #:opt-lattice-value-kind
     #:opt-lattice-value-value
     #:make-opt-function-summary
     #:opt-function-summary-safe-to-inline-p
     #:opt-thinlto-should-import-p
    #:make-opt-slab-pool
    #:opt-slab-allocate
    #:opt-slab-free
    #:make-opt-bump-region
    #:opt-bump-region-cursor
    #:opt-bump-allocate
    #:opt-bump-mark
    #:opt-bump-reset
    #:make-opt-stack-map
    #:opt-stack-map-live-root-p
    #:make-opt-guard-state
    #:opt-guard-record
    #:opt-weaken-guard
    #:make-opt-jit-cache-entry
    #:opt-jit-cache-select-eviction
    #:make-opt-module-summary
    #:opt-merge-module-summaries
    #:make-opt-sea-node
    #:opt-sea-node-schedulable-p
    #:make-opt-deopt-frame
    #:opt-materialize-deopt-state
    #:make-opt-osr-point
    #:opt-osr-trigger-p
    #:opt-osr-materialize-entry
    #:make-opt-shape-descriptor-for-slots
    #:opt-shape-slot-offset
    #:make-opt-shape-transition-cache
     #:opt-shape-transition-put
     #:opt-shape-transition-get
     #:opt-adaptive-compilation-threshold
     #:opt-tier-transition
     #:make-opt-async-state-machine
    #:opt-build-async-state-machine
    #:opt-choose-coroutine-lowering-strategy
    #:make-opt-channel-site
    #:opt-channel-select-path
    #:opt-channel-should-jump-table-select-p
    #:make-opt-stm-plan
    #:opt-stm-build-plan
    #:opt-stm-needs-log-p
    #:make-opt-lockfree-plan
    #:opt-lockfree-select-reclamation
    #:opt-lockfree-build-plan
     #:make-opt-cfi-plan
     #:opt-build-cfi-plan
     #:opt-cfi-entry-opcode
     #:opt-should-use-retpoline-p
      #:opt-retpoline-thunk-name
      #:opt-needs-stack-canary-p
      #:opt-stack-canary-emit-plan
      #:opt-stack-canary-prologue-seq
      #:opt-stack-canary-epilogue-seq
      #:make-opt-shadow-stack-plan
      #:opt-shadow-stack-plan-enabled-p
      #:opt-shadow-stack-plan-target
      #:opt-shadow-stack-plan-needs-incsssp-p
      #:opt-shadow-stack-plan-needs-save-restore-p
      #:opt-build-shadow-stack-plan
     #:make-opt-wasm-tailcall-plan
     #:opt-wasm-select-tailcall-opcode
     #:opt-wasm-select-direct-tailcall-opcode
     #:opt-build-wasm-tailcall-plan
     #:make-opt-wasm-gc-layout
     #:opt-build-wasm-gc-layout
     #:opt-wasm-gc-layout-valid-p
     #:opt-wasm-gc-runtime-host-compatible-p
     #:opt-build-wasm-gc-optimization-plan
      #:make-opt-debug-loc
     #:opt-build-dwarf-line-row
     #:opt-build-wasm-source-map-entry
     #:opt-build-wasm-source-map-v3
     #:opt-format-diagnostic-reason
     #:opt-build-diagnostic-caret-line
     #:opt-diagnostic-did-you-mean
     #:opt-format-type-trace
     #:make-opt-tls-plan
    #:opt-tls-plan-target
    #:opt-tls-plan-base-register
    #:opt-tls-plan-model
    #:opt-tls-plan-notes
    #:opt-build-tls-plan
    #:make-opt-atomic-plan
    #:opt-atomic-plan-target
    #:opt-atomic-plan-operation
    #:opt-atomic-plan-memory-order
    #:opt-atomic-plan-opcode
    #:opt-select-atomic-opcode
    #:opt-build-atomic-plan
    #:make-opt-htm-plan
    #:opt-build-htm-plan
    #:make-opt-concurrent-gc-plan
     #:opt-build-concurrent-gc-plan
     #:make-opt-partial-specialization
     #:opt-partial-spec-original-name
     #:opt-partial-spec-specialized-name
     #:opt-partial-spec-signature
     #:opt-partial-spec-static-args
     #:opt-partial-spec-dynamic-args
     #:opt-partial-spec-residual-body
     #:make-opt-partial-eval-result
     #:opt-partial-eval-function-name
     #:opt-partial-eval-parameters
     #:opt-partial-eval-signature
     #:opt-partial-eval-binding-times
     #:opt-partial-eval-form-kinds
     #:opt-partial-eval-residual-body
     #:opt-partial-eval-dynamic-body
     #:opt-partial-eval-specialization
     #:make-opt-partial-program-result
     #:opt-partial-program-function-results
     #:opt-specialize-constant-args
  #:opt-partial-evaluate-function
  #:opt-partial-evaluate-program
  #:opt-partial-evaluate-modules
  #:opt-partial-evaluate-modules-cached
  #:opt-partial-evaluate-modules-incremental
  #:opt-partial-evaluate-functions-incremental
     #:make-opt-binding-time
     #:opt-binding-time-parameter
     #:opt-binding-time-kind
     #:opt-binding-time-value
     #:opt-binding-time-lattice
     #:opt-sccp-analyze-binding-times
     #:opt-run-binding-time-analysis
     #:opt-offline-bta-classify-form
     #:opt-offline-bta-analyze-body
     #:make-opt-specialization-plan
     #:opt-specialization-plan-callee-label
     #:opt-specialization-plan-specialized-name
     #:opt-specialization-plan-signature
     #:opt-specialization-plan-static-args
     #:opt-specialization-plan-dynamic-args
     #:opt-specialization-plan-clone-needed-p
     #:opt-specialization-plan-cache-hit-p
     #:opt-build-specialization-plan
     #:opt-pass-specialize-known-args
     #:opt-pass-partial-evaluation
     #:make-opt-cow-object
     #:opt-cow-object-payload
     #:opt-cow-object-refcount
    #:opt-cow-copy
    #:opt-cow-write
    #:schedule-pre-ra
    #:opt-verify-instructions))
