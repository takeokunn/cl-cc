;;;; packages/backend/emit/src/package.lisp — cl-cc/emit package definition
;;;;
;;;; Emit backend subsystem: calling conventions, register allocation,
;;;; x86-64 / AArch64 / WASM code generation, and native compilation.
;;;;
;;;; Extracted as a Phase 2 sibling system (:cl-cc-emit). The package
;;;; facade is loaded first (no dependencies); the umbrella :cl-cc system
;;;; then sets up (use-package :cl-cc :cl-cc/emit) so the source files
;;;; can access VM instruction types and accessors unqualified.

(defpackage :cl-cc/emit
  (:use :cl)
  (:export
   ;; ─── calling-convention.lisp — calling convention descriptors ──────
   #:calling-convention
   #:cc-gpr-pool
   #:cc-caller-saved
   #:cc-callee-saved
   #:cc-arg-registers
   #:cc-return-register
   #:cc-scratch-register
   #:*x86-64-calling-convention*
   #:*aarch64-calling-convention*

   ;; ─── regalloc.lisp — register allocation ──────────────────────────
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

   ;; ─── regalloc-allocate.lisp — spill instructions ──────────────────
   #:vm-spill-store
   #:vm-spill-load
   #:make-vm-spill-store
   #:make-vm-spill-load
   #:vm-spill-src
   #:vm-spill-dst
   #:vm-spill-slot

   ;; ─── x86-64-regs.lisp — register mapping ─────────────────────────
   #:vm-reg-to-x86-with-alloc
   #:*current-regalloc*
   #:*phys-reg-to-x86-code*
   #:*phys-reg-to-asm-string*

   ;; ─── x86-64.lisp — target classes + emit-instruction ─────────────
   ;; NOTE: 'target' base class is NOT exported to avoid name conflict with CL-CC/VM.
   ;; Downstream packages use concrete subclasses (x86-64-target, aarch64-target).
   #:emit-instruction
   #:x86-64-target #:target-spill-base-reg

   ;; ─── aarch64.lisp — AArch64 target ─────────────────────────────
   #:aarch64-target

   ;; ─── wasm-emit.lisp — WASM backend ────────────────────────────
   #:compile-to-wasm-wat

   ;; ─── x86-64-regs.lisp — red zone helper ───────────────────────
   #:x86-64-red-zone-spill-p

   ;; ─── pipeline-native.lisp / codegen — native entry points ────────
   #:target-regalloc
   #:compile-to-native
   #:compile-file-to-native
   #:compile-to-x86-64-bytes
   #:compile-to-aarch64-bytes))
