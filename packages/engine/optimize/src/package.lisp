;;;; packages/engine/optimize/src/package.lisp — cl-cc/optimize package definition
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
;;; accessors. cl-cc/prolog provides def-fact and apply-prolog-peephole.
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
   #:bb-loop-depth #:bb-rpo-index
   #:cfg #:make-cfg #:cfg-p
   #:cfg-blocks #:cfg-entry #:cfg-exit
   #:cfg-label->block #:cfg-next-id
   #:cfg-build #:cfg-block-count
   #:cfg-get-block-by-label
   #:cfg-compute-rpo #:cfg-compute-dominators
   #:cfg-compute-dominance-frontiers
   #:cfg-dominates-p #:cfg-idf
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

   ;; ─── optimizer-pipeline.lisp — top-level entry point ───────────────
   #:optimize-instructions
   #:*skip-optimizer-passes*))
