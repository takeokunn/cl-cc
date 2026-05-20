;;;; packages/codegen/src/package.lisp — machine code generation package
;;;;
;;;; Per-target code generation: x86-64, AArch64, WASM backends.
;;;; Register allocation re-exports.  No compilation logic here.

(defpackage :cl-cc/codegen
  (:use :cl
        :cl-cc/bootstrap
        :cl-cc/vm
        :cl-cc/mir
        :cl-cc/target
        :cl-cc/optimize)
  (:import-from :cl-cc/regalloc
                #:live-interval
                #:make-live-interval
                #:interval-vreg
                #:interval-start
                #:interval-end
                #:interval-phys-reg
                #:interval-spill-slot
                #:regalloc-result
                #:regalloc-assignment
                #:regalloc-spill-map
                #:regalloc-spill-count
                #:regalloc-instructions
                #:regalloc-lookup
                #:instruction-defs
                #:instruction-uses
                 #:compute-live-intervals
                 #:linear-scan-allocate
                 #:color-allocate
                 #:allocate-registers
                #:vm-spill-store
                #:vm-spill-load
                #:make-vm-spill-store
                #:make-vm-spill-load
                #:vm-spill-src
                #:vm-spill-dst
                #:vm-spill-slot
                #:*current-regalloc*)
  (:export
   ;; ── Regalloc re-exports ──
   #:live-interval
   #:make-live-interval
   #:interval-vreg
   #:interval-start
   #:interval-end
   #:interval-phys-reg
   #:interval-spill-slot
   #:regalloc-result
   #:regalloc-assignment
   #:regalloc-spill-map
   #:regalloc-spill-count
   #:regalloc-instructions
   #:regalloc-lookup
   #:instruction-defs
   #:instruction-uses
    #:compute-live-intervals
    #:linear-scan-allocate
    #:color-allocate
    #:allocate-registers
   #:vm-spill-store
   #:vm-spill-load
   #:make-vm-spill-store
   #:make-vm-spill-load
   #:vm-spill-src
   #:vm-spill-dst
   #:vm-spill-slot
    #:*current-regalloc*
    #:schedule-post-ra

   ;; ── x86-64 backend ──
    #:target-regalloc
    #:calling-convention
    #:calling-convention-name
    #:calling-convention-arg-regs
    #:calling-convention-callee-saved
    #:calling-convention-omit-frame-pointer-p
    #:*internal-calling-convention*
    #:*external-calling-convention*
    #:vm-reg-to-x86-with-alloc
   #:*phys-reg-to-x86-code*
   #:*phys-reg-to-asm-string*
   #:emit-instruction
   #:x86-64-target #:target-spill-base-reg
     #:x86-64-red-zone-spill-p
     #:*shrink-wrap-enabled*
     #:compile-to-x86-64-bytes
     #:*x86-64-spectre-mitigations-enabled*

     ;; ── MIR instruction selection ──
     #:isel-diagnostic
     #:isel-diagnostic-message
     #:isel-rule
     #:isel-rule-name
     #:isel-rule-target
     #:isel-rule-pattern
     #:isel-rule-result-op
     #:isel-rule-cost
     #:isel-rule-size
     #:isel-rule-emitter
     #:register-isel-rule
     #:isel-rules-for-target
     #:isel-maximal-munch
     #:isel-vm-program
     #:vm-program->mir-module
     #:mir-module->vm-program
     #:optimize-mir-module-for-isel
     #:*isel-x86-64-rules*
     #:*isel-aarch64-rules*

   ;; ── AArch64 backend ──
     #:aarch64-target
     #:compile-to-aarch64-bytes

    ;; ── RISC-V backend ──
    #:riscv64-target
    #:compile-to-riscv64-bytes

    ;; ── WASM backend ──
   #:compile-to-wasm-wat))
