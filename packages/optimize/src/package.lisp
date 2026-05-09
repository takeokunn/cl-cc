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
    #:opt-induction-trip-count
    #:opt-iv-reg #:opt-iv-init #:opt-iv-step #:opt-iv-update-inst
    #:opt-compute-cfg-value-ranges
    #:opt-compute-value-ranges
    #:opt-array-bounds-check-eliminable-p
    #:opt-interval-add  #:opt-interval-sub
    #:opt-may-alias-by-type-p  #:opt-may-alias-p  #:opt-must-alias-p
    #:opt-pass-cons-slot-forward

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
    #:optimize-roadmap-doc-features
    #:optimize-roadmap-doc-fr-ids
    #:optimize-roadmap-register-doc-evidence
    #:lookup-opt-roadmap-evidence
    #:optimize-backend-roadmap-doc-features
    #:optimize-backend-roadmap-doc-fr-ids
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
    #:make-opt-speculation-log
    #:opt-record-speculation-failure
    #:opt-speculation-failed-p
    #:make-opt-profile-data
    #:opt-profile-record-edge
    #:opt-profile-record-value
    #:opt-profile-record-call-chain
    #:opt-profile-record-allocation
    #:opt-lattice-bottom
    #:opt-lattice-constant
    #:opt-lattice-overdefined
    #:opt-lattice-meet
    #:opt-lattice-value-kind
    #:opt-lattice-value-value
    #:make-opt-function-summary
    #:opt-function-summary-safe-to-inline-p
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
    #:make-opt-shape-descriptor-for-slots
    #:opt-shape-slot-offset
    #:opt-adaptive-compilation-threshold
    #:opt-verify-instructions))
