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
  (:export
   ;; ─── effects.lisp — effect-kind classification ─────────────────────
   #:vm-inst-effect-kind
   #:opt-inst-pure-p
   #:opt-inst-dce-eligible-p
   #:opt-inst-cse-eligible-p
   #:effect-row->effect-kind

   ;; ─── cfg.lisp — basic blocks and CFG ───────────────────────────────
   #:basic-block #:make-basic-block #:basic-block-p
   #:bb-id #:bb-label #:bb-instructions
   #:bb-predecessors #:bb-successors
   #:bb-idom #:bb-dom-children #:bb-dom-frontier
   #:bb-post-idom #:bb-post-children
   #:bb-loop-depth #:bb-rpo-index
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
   #:cfg-split-critical-edges

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

    ;; ─── optimizer-memory.lisp — alias analysis ────────────────────────
    #:opt-compute-heap-aliases
    #:opt-compute-points-to
    #:opt-points-to-root
     #:opt-compute-simple-inductions
     #:opt-compute-loop-inductions
     #:opt-induction-trip-count
     #:opt-iv-reg #:opt-iv-init #:opt-iv-step #:opt-iv-update-inst
     #:opt-compute-cfg-value-ranges
     #:opt-compute-path-sensitive-ranges
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
    #:opt-interval-logand
    #:opt-pass-elide-proven-overflow-checks
    #:opt-may-alias-by-type-p  #:opt-may-alias-p  #:opt-must-alias-p
    #:opt-pass-cons-slot-forward
    #:opt-pass-bounds-check-elimination

    ;; ─── optimizer-inline.lisp ─────────────────────────────────────────
    #:opt-known-callee-labels
    #:opt-pass-call-site-splitting
    #:opt-pass-devirtualize

    ;; ─── optimizer-recognition.lisp ─────────────────────────────────────
    #:opt-pass-fill-recognition

    ;; ─── optimizer-flow.lisp ───────────────────────────────────────────
    #:opt-pass-dominated-type-check-elim

    ;; ─── optimizer-pipeline.lisp — top-level entry point ───────────────
    #:optimize-instructions
    #:*skip-optimizer-passes*
    #:*verify-optimizer-instructions*
    #:*opt-enable-pure-call-optimization*
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
    #:opt-verify-instructions))
