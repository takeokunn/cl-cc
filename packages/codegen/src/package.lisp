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
  (:shadowing-import-from :cl-cc/vm
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
    #:lisp-implementation-type #:lisp-implementation-version
    #:machine-type #:machine-version #:machine-instance
    #:software-type #:software-version
    #:room #:apropos #:apropos-list)
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

     ;; ── Precise GC stack maps (FR-541) ──
     #:*precise-gc-stack-maps-enabled*
     #:cg-stack-map-entry #:make-cg-stack-map-entry #:cg-stack-map-entry-p
     #:cg-sme-kind #:cg-sme-location #:cg-sme-index
     #:cg-stack-map #:make-cg-stack-map #:cg-stack-map-p
     #:cg-sm-safepoint-id #:cg-sm-pc-offset
     #:cg-sm-register-roots #:cg-sm-stack-slot-roots
     #:cg-sm-register-bitmask #:cg-sm-stack-bitmask #:cg-sm-frame-size
     #:cg-generate-safepoint-stack-map
     #:cg-stack-map->section-record
     #:cg-safepoint-instruction-p
     #:cg-embed-stack-maps-after-safepoints

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
      #:*x86-64-use-retpoline*
       #:emit-x86-64-speculation-barrier
       #:x86-64-speculative-execution-mitigation-enabled-p
       #:x86-64-macro-fusion-compare-p
       #:x86-64-macro-fusion-branch-p
       #:x86-64-macro-fusion-candidate-p
       #:x86-64-preserve-macro-fusion
       #:*x86-64-stack-protector-enabled*
      #:*x86-64-cfi-enabled*
      #:*x86-64-shadow-stack-enabled*
      #:*x86-64-stack-clash-protection-enabled*
      #:*x86-64-safe-stack-enabled*
       #:*x86-64-avx512-enabled*
       #:*x86-64-apx-enabled*
       #:*x86-64-atomics-enabled*
       #:*amx-enabled*
       #:x86-64-supports-amx-p
       #:+tmm0+ #:+tmm1+ #:+tmm2+ #:+tmm3+
       #:+tmm4+ #:+tmm5+ #:+tmm6+ #:+tmm7+
       #:emit-tileloadd
       #:emit-tilestored
       #:emit-tdpbssd
       #:*asan-instrumentation-enabled*
       #:*ubsan-instrumentation-enabled*
       #:*zero-cost-eh-enabled*
       #:*eh-model*
       #:normalize-x86-64-eh-model
       #:x86-64-table-eh-enabled-p
       #:x86-64-personality-dispatch
       #:x86-64-eh-condition-matches-p
       #:x86-64-lsda-bytes

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
     #:*aarch64-cfi-enabled*
     #:*aarch64-pac-enabled*
     #:*aarch64-shadow-call-stack-enabled*
     #:*aarch64-stack-protector-enabled*
     #:*aarch64-stack-clash-protection-enabled*
     #:*aarch64-safe-stack-enabled*
      #:*aarch64-sve-enabled*
      #:*aarch64-sve2-enabled*
      #:*aarch64-atomics-enabled*
      #:*sme-enabled*
      #:+a64-za+
      #:encode-smstart
      #:encode-smstop
      #:encode-fmopa
      #:aarch64-supports-sme-p

      ;; ── POWER10 / PPC64 backend ──
       #:*ppc64-enabled*
       #:r0 #:r1 #:r2 #:r3 #:r4 #:r5 #:r6 #:r7
       #:r8 #:r9 #:r10 #:r11 #:r12 #:r13 #:r14 #:r15
       #:r16 #:r17 #:r18 #:r19 #:r20 #:r21 #:r22 #:r23
       #:r24 #:r25 #:r26 #:r27 #:r28 #:r29 #:r30 #:r31
       #:lr #:ctr
       #:emit-ppc64-instr
       #:encode-ppc64-nop
       #:encode-power10-addi
       #:encode-power10-add
       #:encode-power10-ld
       #:encode-power10-std
       #:encode-power10-cmp
       #:encode-power10-bc
       #:encode-power10-b
       #:encode-power10-bl
       #:encode-power10-mflr
       #:encode-power10-mtlr
       #:ppc64-elfv2-prologue
       #:ppc64-elfv2-epilogue
       #:ppc64-backend-available-p

       ;; ── Windows CFG ──
       #:*win-cfg-enabled*
       #:*win-cfg-function-table*
       #:win-cfg-invalid-target
       #:win-cfg-register-function
       #:win-cfg-unregister-function
       #:win-cfg-function-registered-p
       #:win-cfg-function-table-entries
       #:_guard_check_icall
       #:_guard_dispatch_icall
       #:win-cfg-emit-guard-check-icall-thunk
       #:win-cfg-emit-guard-dispatch-icall-thunk
       #:win-cfg-guard-fids-table-metadata
       #:win-cfg-guard-check-icall-symbol
       #:win-cfg-guard-dispatch-icall-symbol
       #:win-cfg-enabled-p

      ;; ── RISC-V backend ──
     #:riscv64-target
     #:compile-to-riscv64-bytes
     #:*riscv64-zicond-enabled*

    ;; ── WASM backend ──
   #:compile-to-wasm-wat
   #:compile-to-wasm-binary
   #:compile-to-aot-wasm
   #:wasm-aot-result
   #:wasm-aot-result-bytes
   #:wasm-aot-result-wat
   #:wasm-aot-result-metadata
   #:wasm-tool-available-p
   #:wasm-run-tool-to-string
   #:wasm-file-sri-hash
   #:wasm-file-content-hash))

;; Declare sanitizer variables as special before x86-64-codegen-emitters.lisp
;; is compiled. Without this, SBCL compiles the let bindings in
;; compile-to-x86-64-bytes as lexical (not dynamic), so sanitizer-enabled-p
;; never sees the enabled values.
(in-package :cl-cc/codegen)
(declaim (special *asan-instrumentation-enabled*
                  *ubsan-instrumentation-enabled*))
