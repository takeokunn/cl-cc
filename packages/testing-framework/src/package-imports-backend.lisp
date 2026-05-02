(cl:in-package :cl-cc/test)

;;; MIR, optimizer, emitter, and macroexpander imports for the test framework
;;; package. This keeps backend-facing package wiring separate from the package
;;; definition itself.

(import '(cl-cc/mir:mir-value
          cl-cc/mir:make-mir-value
          cl-cc/mir:mir-value-p
          cl-cc/mir:mirv-id
          cl-cc/mir:mirv-name
          cl-cc/mir:mirv-type
          cl-cc/mir:mirv-def-inst
          cl-cc/mir:mirv-use-count
          cl-cc/mir:mir-const
          cl-cc/mir:make-mir-const
          cl-cc/mir:mir-const-p
          cl-cc/mir:mirc-value
          cl-cc/mir:mirc-type
          cl-cc/mir:mir-inst
          cl-cc/mir:make-mir-inst
          cl-cc/mir:mir-inst-p
          cl-cc/mir:miri-op
          cl-cc/mir:miri-dst
          cl-cc/mir:miri-srcs
          cl-cc/mir:miri-type
          cl-cc/mir:miri-block
          cl-cc/mir:miri-meta
          cl-cc/mir:mir-block
          cl-cc/mir:make-mir-block
          cl-cc/mir:mir-block-p
          cl-cc/mir:mirb-id
          cl-cc/mir:mirb-label
          cl-cc/mir:mirb-insts
          cl-cc/mir:mirb-preds
          cl-cc/mir:mirb-succs
          cl-cc/mir:mirb-sealed-p
          cl-cc/mir:mirb-phis
          cl-cc/mir:mirb-incomplete-phis
          cl-cc/mir:mir-function
          cl-cc/mir:make-mir-function
          cl-cc/mir:mir-function-p
          cl-cc/mir:mirf-name
          cl-cc/mir:mirf-params
          cl-cc/mir:mirf-blocks
          cl-cc/mir:mirf-entry
          cl-cc/mir:mirf-current-defs
          cl-cc/mir:mirf-value-counter
          cl-cc/mir:mirf-block-counter
          cl-cc/mir:mir-module
          cl-cc/mir:make-mir-module
          cl-cc/mir:mir-module-p
          cl-cc/mir:mirm-functions
          cl-cc/mir:mirm-globals
          cl-cc/mir:mirm-string-table
          cl-cc/mir:*mir-generic-ops*
          cl-cc/mir:mir-new-value
          cl-cc/mir:mir-new-block
          cl-cc/mir:mir-make-function
          cl-cc/mir:mir-emit
          cl-cc/mir:mir-add-pred
          cl-cc/mir:mir-add-succ
          cl-cc/mir:mir-write-var
          cl-cc/mir:mir-read-var
          cl-cc/mir:mir-seal-block
          cl-cc/mir:mir-rpo
          cl-cc/mir:mir-dominators
          cl-cc/mir:mir-format-value
          cl-cc/mir:mir-print-inst
          cl-cc/mir:mir-print-block
          cl-cc/mir:mir-print-function
          cl-cc/target:target-desc
          cl-cc/target:make-target-desc
          cl-cc/target:target-desc-p
          cl-cc/target:target-name
          cl-cc/target:target-word-size
          cl-cc/target:target-endianness
          cl-cc/target:target-gpr-count
          cl-cc/target:target-gpr-names
          cl-cc/target:target-arg-regs
          cl-cc/target:target-ret-reg
          cl-cc/target:target-fp-arg-regs
          cl-cc/target:target-fp-ret-reg
          cl-cc/target:target-callee-saved
          cl-cc/target:target-scratch-regs
          cl-cc/target:target-stack-alignment
          cl-cc/target:target-legal-ops
          cl-cc/target:target-features
          cl-cc/target:*x86-64-target*
          cl-cc/target:*aarch64-target*
          cl-cc/target:*riscv64-target*
          cl-cc/target:*wasm32-target*
          cl-cc/target:*target-registry*
          cl-cc/target:register-target
          cl-cc/target:find-target
          cl-cc/target:target-64-bit-p
          cl-cc/target:target-has-feature-p
          cl-cc/target:target-allocatable-regs
          cl-cc/target:target-caller-saved
          cl-cc/target:target-reg-index
          cl-cc/target:target-op-legal-p
          cl-cc/target:target-op-expand))

(import '(cl-cc/optimize:optimize-instructions))

(import '(cl-cc/emit:live-interval
          cl-cc/emit:make-live-interval
          cl-cc/emit:interval-vreg
          cl-cc/emit:interval-start
          cl-cc/emit:interval-end
          cl-cc/emit:interval-phys-reg
          cl-cc/emit:interval-spill-slot
          cl-cc/emit:regalloc-result
          cl-cc/emit:regalloc-spill-count
          cl-cc/emit:regalloc-lookup
          cl-cc/emit:instruction-defs
          cl-cc/emit:instruction-uses
          cl-cc/emit:compute-live-intervals
          cl-cc/emit:allocate-registers
          cl-cc/emit:vm-spill-store
          cl-cc/emit:vm-spill-load
          cl-cc/emit:compile-to-x86-64-bytes
          cl-cc/emit:compile-to-aarch64-bytes
          cl-cc/emit:compile-to-native
          cl-cc/emit:compile-file-to-native))

(import '(cl-cc/expand:our-macroexpand-1
          cl-cc/expand:our-macroexpand
          cl-cc/expand:our-macroexpand-all))
