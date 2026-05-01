;;;; packages/mir/src/package.lisp — cl-cc/mir package definition
;;;;
;;;; Machine IR: register-level SSA CFG intermediate representation.
;;;; Includes mir-value, mir-const, mir-inst, mir-block, mir-function,
;;;; mir-module, SSA variable tracking (Braun et al. 2013), RPO/dominators,
;;;; and printer.
;;;;
;;;; Target descriptors (target-desc, *x86-64-target*, etc.) live in
;;;; :cl-cc/target as of Phase 6. They are re-exported here for
;;;; backward compatibility with consumers using `cl-cc/mir:` prefixes.

(defpackage :cl-cc/mir
  (:use :cl :cl-cc/target)
  (:export
   ;; mir.lisp — structs + accessors
   #:mir-value #:make-mir-value #:mir-value-p
   #:mirv-id #:mirv-name #:mirv-type #:mirv-def-inst #:mirv-use-count
   #:mir-const #:make-mir-const #:mir-const-p
   #:mirc-value #:mirc-type
   #:mir-inst #:make-mir-inst #:mir-inst-p
   #:miri-op #:miri-dst #:miri-srcs #:miri-type #:miri-block #:miri-meta
   #:mir-block #:make-mir-block #:mir-block-p
   #:mirb-id #:mirb-label #:mirb-insts #:mirb-preds #:mirb-succs
   #:mirb-sealed-p #:mirb-phis #:mirb-incomplete-phis
   #:mir-function #:make-mir-function #:mir-function-p
   #:mirf-name #:mirf-params #:mirf-blocks #:mirf-entry
   #:mirf-current-defs #:mirf-value-counter #:mirf-block-counter
   #:mir-module #:make-mir-module #:mir-module-p
   #:mirm-functions #:mirm-globals #:mirm-string-table
   #:*mir-generic-ops*
   ;; mir-builder.lisp
   #:mir-new-value #:mir-new-block #:mir-make-function
   #:mir-emit #:mir-add-pred #:mir-add-succ
   #:mir-write-var #:mir-read-var #:mir-seal-block
   ;; mir-analysis.lisp
   #:mir-rpo #:mir-dominators
   #:mir-format-value #:mir-print-inst #:mir-print-block #:mir-print-function
   ;; Re-exports from :cl-cc/target (backward compat)
   #:target-desc #:make-target-desc #:target-desc-p
   #:target-name #:target-word-size #:target-endianness
   #:target-gpr-count #:target-gpr-names
   #:target-arg-regs #:target-ret-reg
   #:target-fp-arg-regs #:target-fp-ret-reg
   #:target-callee-saved #:target-scratch-regs
   #:target-stack-alignment #:target-legal-ops #:target-features
   #:*x86-64-target* #:*aarch64-target* #:*riscv64-target* #:*wasm32-target*
   #:*target-registry* #:register-target #:find-target
   #:target-64-bit-p #:target-has-feature-p
   #:target-allocatable-regs #:target-caller-saved
   #:target-reg-index #:target-op-legal-p #:target-op-expand))
