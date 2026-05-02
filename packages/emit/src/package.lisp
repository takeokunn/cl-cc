;;;; packages/emit/src/package.lisp — cl-cc/emit package definition
;;;;
;;;; Emit backend: machine code generation (x86-64, AArch64, WASM) and register allocation.

(defpackage :cl-cc/emit
  (:use :cl :cl-cc/vm :cl-cc/mir :cl-cc/optimize :cl-cc/codegen)
  (:export
   ;; Re-export regalloc + emit symbols ---
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
   #:allocate-registers
   #:vm-spill-store
   #:vm-spill-load
   #:make-vm-spill-store
   #:make-vm-spill-load
   #:vm-spill-src
   #:vm-spill-dst
   #:vm-spill-slot

   #:vm-reg-to-x86-with-alloc
   #:*current-regalloc*
   #:*phys-reg-to-x86-code*
   #:*phys-reg-to-asm-string*

   #:emit-instruction
   #:x86-64-target #:target-spill-base-reg
   #:aarch64-target
   #:compile-to-wasm-wat
   #:x86-64-red-zone-spill-p

   #:target-regalloc
   #:compile-to-native
   #:compile-file-to-native
   #:compile-to-x86-64-bytes
   #:compile-to-aarch64-bytes))
