;;;; packages/ir/src/package.lisp — cl-cc/ir package definition
;;;;
;;;; Compile-level SSA IR foundation: ir-value, ir-inst, ir-block,
;;;; ir-function, ir-module, CFG edges, RPO, dominators, SSA variable
;;;; tracking (Braun et al. 2013), and the human-readable printer.
;;;;
;;;; Extracted as a Phase 2 leaf sibling system (:cl-cc-ir). Depends only
;;;; on :cl — no references to other cl-cc features.

(defpackage :cl-cc/ir
  (:use :cl)
  (:export
   ;; ir-value
   #:ir-value #:make-ir-value #:ir-value-p
   #:irv-id #:irv-type #:irv-def
   ;; ir-inst
   #:ir-inst #:make-ir-inst #:ir-inst-p
   #:iri-result #:iri-block
   #:ir-operands
   ;; ir-block
   #:ir-block #:make-ir-block #:ir-block-p
   #:irb-id #:irb-label #:irb-params #:irb-insts #:irb-terminator
   #:irb-predecessors #:irb-successors #:irb-sealed-p #:irb-incomplete-phis
   ;; ir-function
   #:ir-function #:make-ir-function #:ir-function-p
   #:irf-name #:irf-params #:irf-entry #:irf-blocks #:irf-return-type
   #:irf-value-counter #:irf-block-counter #:irf-current-defs
   ;; ir-module
   #:ir-module #:make-ir-module #:ir-module-p
   #:irm-functions #:irm-globals
   ;; constructors / builders (types.lisp)
   #:ir-new-value #:ir-new-block #:ir-make-function
   ;; block.lisp — CFG + dominators + SSA verify
   #:ir-add-edge #:ir-emit #:ir-set-terminator
   #:ir-rpo #:ir-dominators #:ir-collect-uses #:ir-verify-ssa
   ;; ssa.lisp — SSA variable tracking
   #:ir-write-var #:ir-read-var #:ir-seal-block
   ;; printer.lisp
   #:ir-format-value #:ir-print-inst #:ir-print-block
   #:ir-print-function #:ir-function-to-string #:ir-print-module))
