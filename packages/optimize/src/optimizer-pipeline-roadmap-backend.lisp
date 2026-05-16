;;;; optimizer-pipeline-roadmap-backend.lisp — Backend roadmap evidence profile and lookup
;;;; Extracted from optimizer-pipeline-roadmap.lisp.
;;;; Load order: after optimizer-pipeline-roadmap.
(in-package :cl-cc/optimize)

(defparameter +opt-backend-roadmap-evidence-profile-ranges+
  ;; Each entry: (lo hi modules api-symbols test-anchors)
  ;; Entries checked in order; NIL hi means open-ended (>= lo).
  '((16 16
     ("packages/optimize/src/optimizer-memory.lisp"
      "packages/optimize/src/optimizer-memory-passes.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-memory-pass-tests.lisp"
      "packages/optimize/tests/optimizer-store-analysis-tests.lisp")
     (opt-compute-heap-aliases opt-pass-dead-store-elim)
     (dead-store-elim-overwrite-without-read-removes-earlier-store
      dead-store-elim-read-between-stores-preserves-both
      dead-store-elim-store-reaching-ret-is-preserved))
    (9 9
     ("packages/optimize/src/optimizer-pipeline-roadmap-backend.lisp"
       "packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp"
       "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
     (make-opt-ic-site opt-ic-site-state opt-ic-transition opt-ic-resolve-target)
     (optimize-ic-resolve-target-prefers-site-local-entry
      optimize-roadmap-runtime-helpers-have-concrete-behavior))
    (8 8
     ("packages/regalloc/src/regalloc.lisp"
      "packages/emit/tests/regalloc-tests.lisp")
     (("CL-CC/REGALLOC" . "COMPUTE-LIVE-INTERVALS")
      ("CL-CC/REGALLOC" . "ALLOCATE-REGISTERS"))
     (regalloc-float-vregs-allocated-to-distinct-xmm-registers))
    (19 19
     ("packages/optimize/src/optimizer-pipeline-roadmap-backend.lisp"
       "packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp"
       "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
     (make-opt-ic-site opt-ic-site-megamorphic-fallback opt-ic-transition
      make-opt-megamorphic-cache opt-mega-cache-put opt-mega-cache-get opt-ic-resolve-target)
     (optimize-ic-resolve-target-uses-shared-megamorphic-cache
      optimize-roadmap-runtime-helpers-have-concrete-behavior))
    (17 17
     ("packages/optimize/src/optimizer-memory.lisp"
      "packages/optimize/src/optimizer-memory-ranges.lisp"
      "packages/optimize/tests/optimizer-lowlevel-tests.lisp"
      "packages/optimize/tests/optimizer-memory-pass-tests.lisp")
     (opt-compute-heap-aliases opt-compute-heap-kinds opt-may-alias-by-type-p)
     (heap-kind-helper-distinguishes-object-classes
      heap-root-kind-table-integrity-and-lookup))
    (14 14
     ("packages/compile/src/codegen-core.lisp"
      "packages/compile/src/codegen.lisp"
      "packages/optimize/src/optimizer-memory-passes.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-memory-pass-tests.lisp")
     (opt-pass-cons-slot-forward)
     (cons-slot-forward-replaces-car-with-original-car-register
      cons-slot-forward-replaces-cdr-through-move-alias
      cons-slot-forward-source-overwrite-kills-fact
      cons-slot-forward-rplaca-kills-fact
      cons-slot-forward-cons-overwriting-source-is-conservative))
    (15 15
     ("packages/optimize/src/optimizer-flow-loop.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-flow-tests.lisp")
      (opt-pass-code-sinking)
      (code-sinking-moves-const-into-target-block
       code-sinking-noop-when-value-is-read-multiple-times
       code-sinking-moves-cons-into-target-block
       code-sinking-noop-for-cons-read-multiple-times))
     (216 216
      ("packages/optimize/src/optimizer-memory.lisp"
       "packages/optimize/src/optimizer-memory-passes.lisp"
      "packages/optimize/src/optimizer-memory-ranges.lisp"
       "packages/optimize/src/optimizer-pipeline.lisp"
       "packages/optimize/tests/optimizer-memory-pass-tests.lisp")
      (opt-pass-store-to-load-forward opt-compute-heap-aliases opt-must-alias-p)
      (store-to-load-forward-prior-store-replaces-get-global
       store-to-load-forward-cross-block-dominating-store-replaces-get-global
       store-to-load-forward-prior-slot-write-replaces-slot-read
       store-to-load-forward-no-prior-store-preserves-get-global
       store-to-load-forward-join-disagree-preserves-get-global))
     (217 217
      ("packages/optimize/src/optimizer-memory.lisp"
       "packages/optimize/tests/optimizer-memory-tests.lisp")
      (opt-compute-memory-ssa-snapshot opt-memory-ssa-version-at)
      (memory-ssa-snapshot-assigns-monotonic-versions-for-def-use-chain
       memory-ssa-snapshot-slot-location-uses-alias-root))
     (218 218
      ("packages/vm/src/vm-numeric.lisp"
       "packages/vm/tests/vm-numeric-tests.lisp")
      (("CL-CC/VM" . "VM-BIGNUM-DIGIT-VECTOR")
       ("CL-CC/VM" . "VM-BIGNUM-SCHOOLBOOK-MULTIPLY-DIGITS")
       ("CL-CC/VM" . "VM-BIGNUM-MULTIPLICATION-STRATEGY")
       ("CL-CC/VM" . "VM-BIGNUM-MULTIPLY-PLAN"))
      (vm-bignum-digit-vector-splits-little-endian-digits
       vm-bignum-schoolbook-multiply-digits-computes-product-digits
       vm-bignum-multiplication-strategy-selects-thresholded-plan
       vm-bignum-multiply-plan-records-digits-sign-and-strategy))
     (219 219
      ("packages/vm/src/vm-numeric.lisp"
       "packages/vm/tests/vm-numeric-tests.lisp")
      (("CL-CC/VM" . "VM-COMPLEX-UNBOX-PLAN")
       ("CL-CC/VM" . "VM-COMPLEX-UNBOXED-ADD-PLAN"))
      (vm-complex-unbox-plan-splits-local-complex
       vm-complex-unbox-plan-keeps-escaping-complex-boxed
       vm-complex-unboxed-add-plan-adds-components))
     (251 251
      ("packages/optimize/src/optimizer-dataflow.lisp"
       "packages/optimize/tests/optimizer-dataflow-tests.lisp")
      (make-opt-abstract-domain opt-run-abstract-interpretation)
      (abstract-domain-struct-retains-operators
       abstract-interpretation-runs-over-cfg-and-produces-result))
     (252 252
      ("packages/regalloc/src/regalloc.lisp"
       "packages/regalloc/src/regalloc-allocate.lisp"
       "packages/emit/tests/regalloc-tests.lisp")
      (("CL-CC/REGALLOC" . "REGALLOC-BUILD-DIRECT-CALL-GRAPH")
       ("CL-CC/REGALLOC" . "REGALLOC-COMPUTE-INTERPROCEDURAL-HINTS")
       ("CL-CC/REGALLOC" . "REGALLOC-BUILD-ALLOCATION-POLICY-FROM-HINTS")
       ("CL-CC/REGALLOC" . "ALLOCATE-REGISTERS"))
      (regalloc-interprocedural-hints-detect-leaf-and-leaf-callee-chain
       regalloc-interprocedural-policy-hook-derives-preferences
       regalloc-interprocedural-policy-caller-saved-respects-call-crossing-safety
       regalloc-interprocedural-policy-end-to-end-keeps-call-crossing-safe
       regalloc-interprocedural-policy-prefers-callee-saved-on-call-crossing))
     (253 253
      ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp")
      (make-opt-cow-object opt-cow-copy opt-cow-write)
      (optimize-cow-copy-is-constant-time-share
       optimize-cow-write-detaches-when-shared))
      (254 254
       ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
        "packages/optimize/tests/optimizer-pipeline-tests.lisp")
       (make-opt-bump-region opt-bump-allocate opt-bump-mark opt-bump-reset
        make-opt-slab-pool opt-slab-allocate opt-slab-free)
       (optimize-bump-region-mark-reset-restores-cursor
        optimize-slab-pool-reuses-freed-object))
      (297 297
       ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
        "packages/optimize/tests/optimizer-pipeline-tests.lisp")
       (opt-pgo-best-successor opt-pgo-build-hot-chain opt-pgo-rotate-loop)
       (optimize-pgo-build-hot-chain-prefers-hottest-successors
        optimize-pgo-rotate-loop-places-preferred-exit-at-bottom))
      (295 295
       ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
        "packages/pipeline/src/pipeline.lisp"
        "packages/compile/src/codegen.lisp"
        "packages/cli/src/main-utils.lisp"
        "packages/cli/src/handlers.lisp"
        "packages/optimize/tests/optimizer-pipeline-tests.lisp"
        "packages/compile/tests/pipeline-tests.lisp"
        "packages/cli/tests/cli-tests.lisp")
       (opt-pgo-build-counter-plan
        opt-pgo-make-profile-template
        ("CL-CC/COMPILE" . "COMPILATION-RESULT-PGO-COUNTER-PLAN"))
       (optimize-pgo-build-counter-plan-emits-deterministic-bb-and-edge-ids
        optimize-pgo-make-profile-template-zero-initializes-counts
        pipeline-compile-string-emits-pgo-counter-plan
        cli-maybe-make-profiled-vm-state-enabled-for-pgo-generate
        cli-write-pgo-profile-emits-file))
      (301 301
       ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
        "packages/optimize/tests/optimizer-pipeline-tests.lisp")
       (make-opt-module-summary opt-merge-module-summaries opt-thinlto-should-import-p)
       (optimize-merge-module-summaries-aggregates-exports-and-counts
        optimize-thinlto-import-decision-respects-budget-linkage-and-cycles))
      (310 310
       ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
        "packages/optimize/tests/optimizer-pipeline-tests.lisp")
       (opt-adaptive-compilation-threshold opt-tier-transition)
       (optimize-adaptive-compilation-threshold-reacts-to-warmup-pressure-and-failures
        optimize-tier-transition-promotes-through-runtime-tiers))
     (311 311
      ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp")
      (make-opt-osr-point opt-osr-trigger-p opt-osr-materialize-entry)
      (optimize-osr-trigger-p-uses-hotness-threshold
       optimize-osr-materialize-entry-maps-machine-to-vm-registers))
     (312 312
      ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp")
      (make-opt-deopt-frame opt-materialize-deopt-state)
      (optimize-materialize-deopt-state-maps-machine-registers-to-vm-registers))
     (444 444
      ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp")
      (make-opt-shape-descriptor-for-slots opt-shape-slot-offset)
      (optimize-shape-descriptor-slots-map-to-stable-offsets))
     (445 445
      ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp")
      (make-opt-shape-transition-cache opt-shape-transition-put opt-shape-transition-get)
      (optimize-shape-transition-cache-stores-forward-transitions))
     (446 446
      ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp")
      (make-opt-ic-patch-plan opt-ic-make-patch-plan)
      (optimize-ic-make-patch-plan-classifies-state-transitions))
     (447 447
      ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp")
      (opt-build-inline-polymorphic-dispatch)
      (optimize-build-inline-polymorphic-dispatch-builds-guard-chain))
     (449 449
      ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp")
      (make-opt-async-state-machine opt-build-async-state-machine)
      (optimize-build-async-state-machine-builds-linear-transitions))
     (450 450
      ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp")
      (opt-choose-coroutine-lowering-strategy)
      (optimize-choose-coroutine-lowering-strategy-prefers-stackful-when-needed))
     (451 451
      ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp")
      (make-opt-channel-site opt-channel-select-path opt-channel-should-jump-table-select-p)
      (optimize-channel-select-path-classifies-buffered-sync-and-contended-cases
       optimize-channel-jump-table-select-threshold))
     (452 452
      ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp")
      (make-opt-stm-plan opt-stm-build-plan opt-stm-needs-log-p)
      (optimize-stm-plan-skips-log-for-pure-block
       optimize-stm-plan-enables-log-for-impure-read-write))
     (453 453
      ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp")
      (make-opt-lockfree-plan opt-lockfree-select-reclamation opt-lockfree-build-plan)
      (optimize-lockfree-select-reclamation-chooses-policy-from-risk-and-contention))
      (315 315
       ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
        "packages/codegen/src/x86-64-codegen.lisp"
        "packages/codegen/src/aarch64-codegen.lisp"
        "packages/emit/tests/x86-64-codegen-tests.lisp"
        "packages/emit/tests/x86-64-codegen-insn-tests.lisp"
        "packages/emit/tests/aarch64-codegen-tests.lisp"
        "packages/optimize/tests/optimizer-pipeline-tests.lisp")
       (make-opt-cfi-plan opt-build-cfi-plan opt-cfi-entry-opcode
        ("CL-CC/CODEGEN" . "X86-64-CFI-PLAN")
        ("CL-CC/CODEGEN" . "EMIT-X86-64-CFI-ENTRY")
        ("CL-CC/CODEGEN" . "AARCH64-CFI-PLAN")
        ("CL-CC/CODEGEN" . "EMIT-AARCH64-CFI-ENTRY"))
       (optimize-build-cfi-plan-selects-target-specific-guards
        optimize-cfi-entry-opcode-materializes-selected-marker
        x86-64-cfi-entry-emits-endbr64-bytes
        x86-64-program-with-indirect-call-starts-with-endbr64
        x86-64-call-cfi-guard-avoids-clobbering-rax-target
        aarch64-cfi-entry-emits-bti-c-bytes
        aarch64-program-with-indirect-call-starts-with-bti-c))
      (316 316
       ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
        "packages/codegen/src/x86-64-codegen-dispatch.lisp"
        "packages/codegen/src/x86-64-regs.lisp"
        "packages/emit/tests/x86-64-codegen-insn-tests.lisp"
        "packages/cli/src/args.lisp"
        "packages/cli/src/main-dump.lisp"
        "packages/optimize/tests/optimizer-pipeline-tests.lisp")
       (opt-should-use-retpoline-p opt-retpoline-thunk-name
        ("CL-CC/CODEGEN" . "EMIT-VM-CALL-LIKE-INST")
        ("CL-CC/CODEGEN" . "EMIT-VM-TAIL-CALL-INST")
        ("CL-CC/CODEGEN" . "*X86-64-USE-RETPOLINE*"))
       (optimize-should-use-retpoline-p-depends-on-target-and-ibrs
        optimize-retpoline-thunk-name-is-target-register-specific
        x86-64-call-encoding-retpoline
        x86-64-tail-call-encoding-retpoline))
      (317 317
       ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
        "packages/codegen/src/x86-64-codegen.lisp"
        "packages/emit/tests/x86-64-codegen-tests.lisp"
        "packages/cli/src/args.lisp"
        "packages/cli/src/main-dump.lisp"
        "packages/optimize/tests/optimizer-pipeline-tests.lisp")
       (opt-needs-stack-canary-p opt-stack-canary-emit-plan
        opt-stack-canary-prologue-seq opt-stack-canary-epilogue-seq
        ("CL-CC/CODEGEN" . "X86-64-STACK-CANARY-PLAN")
        ("CL-CC/CODEGEN" . "EMIT-X86-64-STACK-CANARY-PROLOGUE")
        ("CL-CC/CODEGEN" . "EMIT-X86-64-STACK-CANARY-EPILOGUE"))
       (optimize-needs-stack-canary-p-follows-stack-buffer-presence
        optimize-stack-canary-emit-plan-describes-prologue-and-epilogue
        optimize-stack-canary-sequences-describe-prologue-and-epilogue-ops
        optimize-stack-canary-sequences-are-empty-when-disabled
        x86-64-stack-canary-plan-materializes-prologue-and-epilogue
        x86-64-stack-protector-emitter-signature-bytes))
      (318 318
       ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
        "packages/codegen/src/x86-64-codegen.lisp"
        "packages/codegen/src/x86-64-codegen-dispatch.lisp"
        "packages/emit/tests/x86-64-codegen-insn-tests.lisp"
        "packages/optimize/tests/optimizer-pipeline-tests.lisp")
       (make-opt-shadow-stack-plan opt-build-shadow-stack-plan
        ("CL-CC/CODEGEN" . "EMIT-VM-SHADOW-STACK-CONTROL-INST")
        ("CL-CC/CODEGEN" . "*X86-64-SHADOW-STACK-ENABLED*"))
       (optimize-build-shadow-stack-plan-enables-only-for-x86-64-with-cet
        optimize-shadow-stack-plan-requires-save-restore-for-nonlocal-control
        x86-64-shadow-stack-control-inst-emits-saveprevssp-when-enabled
        x86-64-shadow-stack-control-inst-uses-distinct-restore-marker
        x86-64-shadow-stack-control-inst-uses-incsspq-for-adjust-paths
        x86-64-shadow-stack-control-inst-covers-vm-establish-catch
        x86-64-shadow-stack-control-inst-covers-vm-throw))
     (320 320
      ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/codegen/src/wasm-emit.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp"
       "packages/emit/tests/wasm-tests.lisp")
      (opt-wasm-select-tailcall-opcode
       opt-wasm-select-direct-tailcall-opcode
       make-opt-wasm-tailcall-plan
       opt-build-wasm-tailcall-plan)
      (optimize-wasm-select-tailcall-opcode-uses-return-call-forms
       wasm-tail-call-dispatch-uses-return-call-indirect
       wasm-tail-call-direct-path-uses-return-call-when-callee-known))
     (321 321
      ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp")
      (make-opt-wasm-gc-layout
       opt-build-wasm-gc-layout
       opt-wasm-gc-layout-valid-p
       opt-wasm-gc-runtime-host-compatible-p
       opt-build-wasm-gc-optimization-plan)
      (optimize-build-wasm-gc-layout-preserves-kind-and-fields
       optimize-wasm-gc-layout-validates-struct-and-array-shapes
       optimize-wasm-gc-runtime-host-compatibility-requires-feature-and-valid-layout
       optimize-wasm-gc-optimization-plan-reflects-layout-kind))
     (330 330
      ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp")
      (make-opt-debug-loc opt-build-dwarf-line-row)
      (optimize-build-dwarf-line-row-preserves-location-fields))
     (331 331
      ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp")
      (make-opt-debug-loc opt-build-wasm-source-map-entry)
      (optimize-build-wasm-source-map-entry-preserves-offset-and-source))
     (333 333
      ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-pipeline-tests.lisp")
      (opt-format-diagnostic-reason)
      (optimize-format-diagnostic-reason-renders-rpass-like-message))
     (335 335
       ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
        "packages/optimize/tests/optimizer-pipeline-tests.lisp"
        "packages/codegen/src/x86-64-codegen.lisp"
        "packages/codegen/src/aarch64-codegen.lisp")
       (make-opt-tls-plan opt-build-tls-plan)
       (optimize-build-tls-plan-selects-architecture-specific-base-register))
     (336 336
       ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
        "packages/optimize/tests/optimizer-pipeline-tests.lisp"
        "packages/codegen/src/x86-64-codegen.lisp"
        "packages/codegen/src/aarch64-codegen.lisp")
       (make-opt-atomic-plan opt-select-atomic-opcode opt-build-atomic-plan)
       (optimize-select-atomic-opcode-reflects-target-and-operation))
      (337 337
       ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
        "packages/optimize/tests/optimizer-pipeline-tests.lisp"
        "packages/vm/src/hash.lisp"
        "packages/vm/src/hash-execute.lisp"
        "packages/vm/tests/hash-tests.lisp")
       (make-opt-htm-plan opt-build-htm-plan)
       (optimize-build-htm-plan-enables-lock-elision-only-when-supported-and-low-contention
        hash-lock-elision-wrapper-falls-back-after-abort
        hash-lock-elision-disabled-after-abort-threshold))
      (338 338
       ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
        "packages/optimize/tests/optimizer-pipeline-tests.lisp"
        "packages/runtime/src/gc-major.lisp"
        "packages/runtime/src/gc-write-barrier.lisp"
        "packages/runtime/tests/gc-sweep-major-tests.lisp"
        "packages/runtime/tests/gc-write-barrier-tests.lisp")
       (make-opt-concurrent-gc-plan opt-build-concurrent-gc-plan)
       (optimize-build-concurrent-gc-plan-selects-satb-and-short-stw-for-latency-sensitive-mode
        gc-configure-concurrent-mode-updates-runtime-flags
        gc-major-collect-enters-concurrent-state-when-enabled
        gc-concurrent-assist-marks-satb-old-pointers-with-budget
        gc-write-barrier-satb-snapshot-major-gc-concurrent-black-object))
      (209 209
       ("packages/optimize/src/optimizer-inline-cost.lisp"
         "packages/optimize/src/optimizer.lisp"
         "packages/optimize/src/optimizer-pipeline-speculative.lisp"
         "packages/optimize/tests/optimizer-inline-pass-tests-2.lisp")
      (opt-pass-inline opt-pass-fold opt-specialize-constant-args)
      (opt-pass-inline-propagates-constant-argument-into-inlined-body
       optimize-specialize-constant-args-builds-residual-body))
     (210 210
      ("packages/optimize/src/optimizer-inline-cost.lisp"
       "packages/optimize/src/optimizer-dataflow-sccp.lisp"
       "packages/optimize/src/optimizer-pipeline-speculative.lisp"
       "packages/optimize/tests/optimizer-inline-pass-tests-2.lisp")
       (opt-pass-inline opt-pass-sccp opt-sccp-analyze-binding-times)
       (opt-pass-inline-propagates-constant-argument-into-inlined-body
        optimize-sccp-analyze-binding-times-classifies-lattice-values))
     (211 211
      ("packages/optimize/src/optimizer-inline.lisp"
       "packages/optimize/src/optimizer-inline-cost.lisp"
        "packages/optimize/src/optimizer-pipeline-speculative.lisp"
        "packages/optimize/tests/optimizer-inline-tests.lisp"
        "packages/optimize/tests/optimizer-inline-pass-tests-2.lisp")
       (opt-pass-devirtualize opt-pass-inline opt-build-specialization-plan)
       (opt-pass-devirtualize-is-idempotent-for-already-direct-call
        opt-pass-inline-inlines-eligible-call
        optimize-build-specialization-plan-reuses-cache-for-constant-signature))
     (4 56
      ("packages/compile/src/codegen.lisp"
       "packages/compile/src/codegen-control.lisp"
       "packages/compile/src/codegen-core.lisp"
      "packages/optimize/src/optimizer-memory.lisp"
      "packages/optimize/src/optimizer-inline.lisp"
      "packages/optimize/tests/optimizer-memory-tests.lisp"
      "packages/optimize/tests/optimizer-inline-tests.lisp")
     (opt-compute-points-to opt-array-bounds-check-eliminable-p
      opt-known-callee-labels opt-function-summary-safe-to-inline-p)
     (optimize-backend-roadmap-analysis-evidence-is-loaded))
    (57 184
     ("packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/src/optimizer-dataflow.lisp"
      "packages/optimize/src/optimizer-purity.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp"
      "packages/optimize/tests/optimizer-dataflow-tests.lisp")
      (opt-lattice-meet opt-run-dataflow opt-profile-record-edge
       opt-profile-record-value opt-stack-map-live-root-p)
      (optimize-backend-roadmap-support-evidence-has-behavior))
    (236 236
     ("packages/expand/src/macros-control-flow-case.lisp"
      "packages/expand/tests/macros-control-flow-loop-tests.lisp")
     (("CL-CC/EXPAND" . "%CASE-EXPAND-INTEGER-TREE")
      ("CL-CC/EXPAND" . "%CASE-EXPAND-INTEGER-TABLE")
      ("CL-CC/EXPAND" . "%PRUNE-TYPECASE-CLAUSES")
      ("CL-CC/EXPAND" . "%TYPECASE-BUILD-TYPEP-CHAIN"))
     (case-expands-sparse-integer-keys-into-binary-search
      case-expands-dense-integer-keys-into-table-dispatch
      typecase-prunes-subsumed-later-clause
       case-collect-integer-pairs-extracts-default
      typecase-build-typep-chain-single))
    (255 255
     ("packages/vm/src/list.lisp"
      "packages/vm/src/list-execute.lisp"
      "packages/vm/src/exports-runtime.lisp"
      "packages/vm/src/exports-instructions-constructors-core.lisp"
      "packages/compile/src/builtin-registry-data-ext.lisp"
      "packages/expand/src/expander-data.lisp"
      "packages/vm/tests/list-tests.lisp"
      "packages/compile/tests/builtin-registry-data-ext-tests.lisp"
      "packages/compile/tests/pipeline-eval-tests.lisp")
     (vm-hash-cons vm-clear-hash-cons-table make-vm-hash-cons)
     (vm-hash-cons-behavior
      vm-hash-cons-instruction-reuses-identical-flat-pairs
      builtin-binary-custom-representative-entries
      pipeline-run-string-hash-cons-reuses-flat-pairs))
    (256 256
     ("packages/optimize/src/optimizer-inline-pass.lisp"
      "packages/optimize/src/optimizer-purity.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-inline-pass-tests.lisp"
      "packages/optimize/tests/optimizer-purity-tests.lisp")
     (opt-make-pure-function-memo-table opt-pure-function-memo-get
      opt-pure-function-memo-put opt-pass-pure-call-optimization)
     (opt-memo-roundtrip
      opt-memo-put-ignores-impure-label
      opt-pass-pure-call-reuses-repeated-known-direct-call
      opt-pass-pure-call-removes-dead-known-direct-call
      optimize-instructions-pass-pipeline-runs-pure-call-optimization))
    (185 256
     ("packages/optimize/src/optimizer-inline.lisp"
      "packages/optimize/src/optimizer-memory.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/vm/src/list.lisp"
      "packages/vm/tests/vm-tests.lisp")
     (opt-profile-record-call-chain opt-profile-record-allocation
      opt-bump-allocate opt-slab-allocate opt-merge-module-summaries)
     (optimize-backend-roadmap-support-evidence-has-behavior))
     (282 282
      ("packages/optimize/src/optimizer-strength.lisp"
       "packages/optimize/src/optimizer-pipeline.lisp"
       "packages/optimize/tests/optimizer-strength-tests.lisp")
     (opt-pass-strength-reduce opt-power-of-2-p
      %opt-find-verified-reciprocal-div-params
      %opt-div-by-verified-reciprocal-seq)
     (fr-282-div-by-2-emits-ash-neg-1
      fr-282-div-by-256-emits-ash-neg-8
       fr-282-div-by-3-bounded-nonnegative-dividend-emits-reciprocal-seq
       fr-282-div-by-7-bounded-nonnegative-dividend-emits-reciprocal-seq
       fr-282-div-by-3-unknown-dividend-not-transformed
       fr-282-div-by-7-unknown-dividend-not-transformed
       fr-282-div-by-3-negative-dividend-transformed-when-bounded
       fr-282-div-by-3-bounded-negative-dividend-emits-reciprocal-seq
       fr-282-div-by-7-bounded-mixed-sign-dividend-emits-reciprocal-seq
       fr-282-div-by-0-not-transformed
       fr-282-div-by-negative-not-transformed
       fr-282-div-non-constant-rhs-not-transformed))
     (283 283
      ("packages/vm/src/vm-instructions.lisp"
       "packages/vm/src/vm-bitwise.lisp"
       "packages/codegen/src/x86-64-encoding-instrs.lisp"
       "packages/codegen/src/x86-64-sequences.lisp"
       "packages/codegen/src/x86-64-emit-ops.lisp"
       "packages/codegen/src/x86-64-codegen-dispatch.lisp"
       "packages/codegen/src/x86-64-codegen.lisp"
       "packages/codegen/src/aarch64-codegen.lisp"
       "packages/codegen/src/aarch64-emitters.lisp"
       "packages/codegen/src/aarch64-program.lisp"
       "packages/codegen/src/aarch64-codegen-labels.lisp"
       "packages/vm/tests/vm-bitwise-tests.lisp"
       "packages/emit/tests/x86-64-encoding-tests.lisp"
       "packages/emit/tests/x86-64-sequences-tests.lisp"
       "packages/emit/tests/x86-64-emit-ops-tests.lisp"
       "packages/emit/tests/aarch64-encoding-tests.lisp"
       "packages/emit/tests/aarch64-codegen-tests.lisp"
       "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
      (("CL-CC" . "MAKE-VM-INTEGER-MUL-HIGH-U")
       ("CL-CC" . "MAKE-VM-INTEGER-MUL-HIGH-S")
       ("CL-CC/VM" . "%VM-INTEGER-MUL-HIGH-U")
       ("CL-CC/VM" . "%VM-INTEGER-MUL-HIGH-S")
       ("CL-CC/CODEGEN" . "EMIT-MUL-RM64")
       ("CL-CC/CODEGEN" . "EMIT-IMUL-RM64")
       ("CL-CC/CODEGEN" . "EMIT-MUL-HIGH-SEQUENCE")
       ("CL-CC/CODEGEN" . "EMIT-VM-INTEGER-MUL-HIGH-U")
       ("CL-CC/CODEGEN" . "EMIT-VM-INTEGER-MUL-HIGH-S")
       ("CL-CC/CODEGEN" . "ENCODE-UMULH")
       ("CL-CC/CODEGEN" . "ENCODE-SMULH")
       ("CL-CC/CODEGEN" . "EMIT-A64-VM-INTEGER-MUL-HIGH-U")
       ("CL-CC/CODEGEN" . "EMIT-A64-VM-INTEGER-MUL-HIGH-S"))
      (vm-mul-high-64-semantics
       x86-mul-rm64-high-encodings
       x86-seq-mul-high-sequence-encodings
       x86-emit-mul-high-emits-19-bytes
       x86-mul-high-size-and-dispatch-registered
       a64-mul-high-encoders
       aarch64-mul-high-emitter-encodings
       aarch64-mul-high-size-and-dispatch-registered
       optimize-backend-roadmap-fr-283-has-specific-evidence))
     (284 284
      ("packages/optimize/src/optimizer-recognition.lisp"
       "packages/optimize/src/optimizer-pipeline.lisp"
       "packages/codegen/src/aarch64-emitters.lisp"
      "packages/codegen/src/aarch64-codegen-labels.lisp"
      "packages/codegen/src/aarch64-program.lisp"
      "packages/emit/tests/x86-64-emit-ops-tests.lisp"
      "packages/emit/tests/aarch64-codegen-tests.lisp"
      "packages/optimize/tests/optimizer-store-analysis-tests.lisp"
      "packages/optimize/tests/optimizer-strength-tests.lisp")
     (opt-pass-rotate-recognition opt-rotate-recognition-match-at)
     (aarch64-rotate-emitter-encoding
      rotate-recognition-collapses-shift-or-tree
      rotate-recognition-collapses-rotate-idiom))
    (285 285
     ("packages/optimize/src/optimizer-recognition.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/codegen/src/aarch64-emitters.lisp"
      "packages/codegen/src/aarch64-codegen-labels.lisp"
      "packages/codegen/src/aarch64-program.lisp"
      "packages/emit/tests/aarch64-codegen-tests.lisp"
      "packages/emit/tests/x86-64-codegen-insn-tests.lisp"
      "packages/optimize/tests/optimizer-store-analysis-tests.lisp")
     (opt-pass-bswap-recognition opt-bswap-recognition-match-at)
     (aarch64-bswap-emitter-encoding
      bswap-recognition-collapses-byte-swap-tree))
    (286 286
     ("packages/vm/src/vm-transcendental.lisp"
       "packages/codegen/src/x86-64-encoding-instrs.lisp"
       "packages/codegen/src/x86-64-emit-ops.lisp"
       "packages/codegen/src/x86-64-regs.lisp"
       "packages/codegen/src/x86-64-codegen-dispatch.lisp"
       "packages/codegen/src/x86-64-codegen.lisp"
      "packages/codegen/src/aarch64-codegen.lisp"
      "packages/codegen/src/aarch64-emitters.lisp"
      "packages/codegen/src/aarch64-program.lisp"
      "packages/codegen/src/aarch64-codegen-labels.lisp"
      "packages/emit/tests/x86-64-encoding-tests.lisp"
      "packages/emit/tests/x86-64-emit-ops-tests.lisp"
      "packages/emit/tests/x86-64-codegen-tests.lisp"
      "packages/emit/tests/aarch64-encoding-tests.lisp"
      "packages/emit/tests/aarch64-codegen-tests.lisp")
      (("CL-CC" . "MAKE-VM-SQRT")
       ("CL-CC/CODEGEN" . "EMIT-SQRTSD-XX")
       ("CL-CC/CODEGEN" . "EMIT-VM-SQRT")
       ("CL-CC/CODEGEN" . "EMIT-VM-SIN")
       ("CL-CC/CODEGEN" . "EMIT-VM-COS")
       ("CL-CC/CODEGEN" . "EMIT-VM-EXP")
       ("CL-CC/CODEGEN" . "EMIT-VM-LOG")
       ("CL-CC/CODEGEN" . "EMIT-VM-TAN")
       ("CL-CC/CODEGEN" . "EMIT-VM-ASIN")
       ("CL-CC/CODEGEN" . "EMIT-VM-ACOS")
       ("CL-CC/CODEGEN" . "EMIT-VM-ATAN")
       ("CL-CC/CODEGEN" . "EMIT-A64-VM-SIN")
       ("CL-CC/CODEGEN" . "EMIT-A64-VM-COS")
       ("CL-CC/CODEGEN" . "EMIT-A64-VM-EXP")
       ("CL-CC/CODEGEN" . "EMIT-A64-VM-LOG")
       ("CL-CC/CODEGEN" . "EMIT-A64-VM-TAN")
       ("CL-CC/CODEGEN" . "EMIT-A64-VM-ASIN")
       ("CL-CC/CODEGEN" . "EMIT-A64-VM-ACOS")
       ("CL-CC/CODEGEN" . "EMIT-A64-VM-ATAN")
       ("CL-CC/CODEGEN" . "ENCODE-FSQRT")
       ("CL-CC/CODEGEN" . "EMIT-A64-VM-SQRT"))
      (x86-xmm-instruction-encoding
       x86-emit-sqrt-emits-sqrtsd-sequence
       x86-emit-libm-unary-emits-21-bytes
       x86-64-emitter-table-spot-checks
       a64-fsqrt-encoder
       aarch64-libm-unary-emitter-size
       aarch64-sqrt-emitter-encoding
       aarch64-sqrt-size-and-dispatch-registered))
    (302 302
     ("packages/optimize/src/optimizer-strength.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-strength-tests.lisp"
      "packages/optimize/tests/optimizer-strength-inline-tests.lisp")
     (opt-pass-strength-reduce)
     (strength-reduce-mod-by-power-of-2-emits-logand))
    (305 305
     ("packages/optimize/src/optimizer-strength.lisp"
      "packages/optimize/tests/optimizer-strength-inline-tests.lisp")
     (opt-pass-strength-reduce %opt-mul-by-const-seq)
     (strength-reduce-mul-by-const-decomposes
      mul-by-const-seq-cases mul-by-const-seq-correctness))
    (290 290
     ("packages/regalloc/src/regalloc.lisp"
      "packages/regalloc/src/regalloc-allocate.lisp"
      "packages/emit/tests/regalloc-tests.lisp")
     (("CL-CC/REGALLOC" . "COMPUTE-LIVE-INTERVALS")
      ("CL-CC/REGALLOC" . "LINEAR-SCAN-ALLOCATE")
      ("CL-CC/REGALLOC" . "ALLOCATE-REGISTERS"))
     (regalloc-liveness-three-overlapping-intervals
      regalloc-liveness-forward-branch-extends-interval
      regalloc-allocate-fits-in-physical-regs-with-distinct-assignments
      regalloc-spill-pressure-exceeds-pool-causes-spills))
    (292 292
     ("packages/regalloc/src/regalloc.lisp"
      "packages/regalloc/src/regalloc-allocate.lisp"
      "packages/emit/tests/regalloc-tests.lisp")
     (("CL-CC/REGALLOC" . "COMPUTE-LIVE-INTERVALS")
      ("CL-CC/REGALLOC" . "%LSA-TRY-COALESCE")
      ("CL-CC/REGALLOC" . "LINEAR-SCAN-ALLOCATE"))
     (regalloc-allocate-coalesces-move-to-same-physical-reg))
    (293 293
     ("packages/regalloc/src/regalloc-allocate.lisp"
      "packages/emit/tests/regalloc-tests.lisp")
     (("CL-CC/REGALLOC" . "INSERT-SPILL-CODE")
      ("CL-CC/REGALLOC" . "ALLOCATE-REGISTERS"))
     (regalloc-spill-pressure-exceeds-pool-causes-spills
      regalloc-spill-rewrite-two-spilled-srcs-use-distinct-scratch-regs
       regalloc-spill-rewrite-spilled-src-and-dst-use-separate-scratch
       regalloc-integration-rematerializes-spilled-constant-as-vm-const))
    (303 303
     ("packages/vm/src/vm-instructions.lisp"
      "packages/vm/src/vm-execute.lisp"
      "packages/codegen/src/x86-64-emit-ops.lisp"
      "packages/codegen/src/x86-64-codegen-dispatch.lisp"
      "packages/codegen/src/x86-64-codegen.lisp"
      "packages/codegen/src/aarch64-emitters.lisp"
      "packages/codegen/src/aarch64-program.lisp"
      "packages/codegen/src/aarch64-codegen-labels.lisp"
      "packages/emit/tests/x86-64-emit-ops-tests.lisp"
      "packages/emit/tests/aarch64-emit-tests.lisp")
     (("CL-CC" . "MAKE-VM-ADD-CHECKED")
      ("CL-CC" . "MAKE-VM-SUB-CHECKED")
      ("CL-CC" . "MAKE-VM-MUL-CHECKED")
      ("CL-CC/CODEGEN" . "EMIT-VM-ADD-CHECKED")
      ("CL-CC/CODEGEN" . "EMIT-VM-SUB-CHECKED")
      ("CL-CC/CODEGEN" . "EMIT-VM-MUL-CHECKED")
      ("CL-CC/CODEGEN" . "EMIT-A64-VM-ADD-CHECKED")
      ("CL-CC/CODEGEN" . "EMIT-A64-VM-SUB-CHECKED")
      ("CL-CC/CODEGEN" . "EMIT-A64-VM-MUL-CHECKED"))
     (x86-emit-add-checked-emits-14-bytes
      x86-emit-sub-checked-emits-14-bytes
      x86-emit-mul-checked-emits-15-bytes
      aarch64-emit-add-checked-emits-12-bytes
      aarch64-emit-sub-checked-emits-12-bytes
      aarch64-emit-mul-checked-emits-24-bytes))
    (257 305
     ("packages/optimize/src/optimizer.lisp"
      "packages/optimize/src/optimizer-memory.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-strength-tests.lisp"
      "packages/optimize/tests/optimizer-memory-tests.lisp")
     (opt-compute-cfg-value-ranges opt-compute-value-ranges
      opt-guard-record opt-weaken-guard opt-adaptive-compilation-threshold)
     (optimize-backend-roadmap-support-evidence-has-behavior))
    (350 350
     ("packages/optimize/src/egraph.lisp"
       "packages/optimize/src/egraph-match.lisp"
       "packages/optimize/src/egraph-saturation.lisp"
       "packages/optimize/src/egraph-rules.lisp"
       "packages/optimize/tests/egraph-extraction-tests.lisp")
     (egraph-add egraph-merge egraph-saturate egraph-extract egraph-default-cost)
     (egraph-saturate-empty-graph-terminates-at-iter-0
      egraph-saturate-with-empty-rules-terminates
      egraph-extract-nullary-node-returns-non-nil
      egraph-extract-binary-add-returns-compound))
     (351 351
     ("packages/optimize/src/egraph-rules.lisp"
      "packages/optimize/src/egraph-rules-advanced.lisp"
      "packages/optimize/src/optimizer-algebraic.lisp"
      "packages/optimize/src/optimizer.lisp"
      "packages/optimize/tests/egraph-rules-tests.lisp"
      "packages/optimize/tests/egraph-rules-bitwise-tests.lisp"
      "packages/optimize/tests/egraph-negation-tests.lisp"
      "packages/optimize/tests/optimizer-tests.lisp"
      "packages/optimize/tests/optimizer-tables-tests.lisp")
     (egraph-rule-register egraph-builtin-rules %opt-apply-algebraic-action)
     (optimizer-algebraic-identity
      egraph-rule-const-producing-rules-fire
      egraph-rule-registry-complete
      egraph-rule-double-neg-then-identity
      opt-apply-algebraic-action-move-lhs))
     (352 352
      ("packages/optimize/src/optimizer-memory.lisp"
       "packages/optimize/src/optimizer-memory-ranges.lisp"
       "packages/optimize/src/optimizer.lisp"
       "packages/optimize/tests/optimizer-memory-tests.lisp"
       "packages/optimize/tests/optimizer-memory-pass-tests.lisp")
      (opt-interval-logand opt-interval-bit-width
       opt-pass-elide-proven-overflow-checks
       %opt-rewrite-logand-low-bit-test opt-pass-fold)
      (value-ranges-logand-mask-with-unknown-input-narrows-to-8-bit
       value-ranges-add-of-masked-8-bit-values-is-9-bit-wide
       overflow-check-elim-rewrites-proven-8-bit-add-to-unchecked-integer-add
       optimize-instructions-rewrites-logand-one-eq-zero-to-evenp))
     (355 355
      ("packages/expand/src/expander-basic.lisp"
       "packages/expand/tests/expander-basic-tests.lisp"
       "packages/compile/src/codegen-io-ext.lisp")
     (("CL-CC/EXPAND" . "%FORMAT-DIRECTIVE-EXPANSION")
      ("CL-CC/EXPAND" . "%PARSE-FORMAT-LITERAL")
      ("CL-CC/EXPAND" . "%FORMAT-LITERAL-EXPANSION"))
     (expander-format-literal-single-aesthetic-directive
      expander-format-literal-supported-directives
      expander-format-literal-unsupported-directive-falls-back
      expander-format-literal-extra-args-fall-back))
     (380 380
      ("packages/compile/src/codegen-values.lisp"
       "packages/compile/src/codegen-values-helpers.lisp"
       "packages/compile/tests/codegen-functions-callsite-tests.lisp"
      "packages/compile/tests/compiler-tests-selfhost.lisp")
     (("CL-CC/COMPILE" . "%COMPILE-APPLY-LITERAL-SPREAD")
      ("CL-CC/COMPILE" . "%APPLY-ARGUMENT-PLAN")
      ("CL-CC/COMPILE" . "%LITERAL-APPLY-SPREAD-VALUES")
      ("CL-CC/COMPILE" . "%PROPER-LIST-P"))
     (codegen-apply-compilation
      codegen-apply-quoted-nil-compilation
      codegen-apply-improper-quoted-list-falls-back-to-vm-apply
      apply-spread-args-numeric
      apply-spread-quoted-nil-preserves-evaluation-order
      apply-improper-quoted-list-signals-error))
    (367 367
     ("packages/compile/src/codegen-core-let.lisp"
      "packages/compile/src/codegen-core-let-emit-pass.lisp"
      "packages/compile/tests/codegen-core-let-tests.lisp"
      "packages/compile/tests/codegen-core-tests.lisp")
     (("CL-CC/COMPILE" . "%AST-LET-BINDING-IGNORED-P"))
     (ast-let-binding-ignored-p
      codegen-let-binding-declaration-controls-own-move
      codegen-let-ignore-binding-enables-dce-of-unused-initializer))
    (363 363
     ("packages/expand/src/macros-runtime-support.lisp"
      "packages/expand/src/expander-data.lisp"
      "packages/compile/src/context.lisp"
      "packages/compile/src/codegen.lisp"
      "packages/compile/src/codegen-core-let-emit-pass.lisp"
      "packages/expand/tests/macros-runtime-support-tests.lisp"
      "packages/compile/tests/compiler-tests-extended-stdlib.lisp"
      "packages/compile/tests/codegen-control-tests.lisp")
     (("CL-CC/EXPAND" . "DECLARATION-OPTIMIZE-QUALITY")
      ("CL-CC/COMPILE" . "%GLOBAL-OPTIMIZE-QUALITY")
      ("CL-CC/COMPILE" . "%LOCAL-OPTIMIZE-QUALITY"))
     (declaim-optimize-updates-registry
      compile-declaim-optimize-form-records-global-policy
      compile-declaim-safety-zero-suppresses-later-defun-type-assertion
      compile-declaim-safety-zero-suppresses-top-level-the-type-assertion
      codegen-the-with-local-let-safety-zero-skips-typep))
    (364 364
     ("packages/expand/src/expander-data.lisp"
      "packages/expand/src/macro.lisp"
      "packages/expand/src/expander.lisp"
      "packages/expand/src/expander-basic.lisp"
      "packages/expand/src/macros-stdlib-ansi.lisp"
      "packages/compile/src/codegen.lisp"
      "packages/pipeline/src/pipeline.lisp"
      "packages/repl/src/pipeline-repl-load.lisp"
      "packages/expand/tests/macro-definition-tests.lisp")
     (("CL-CC/EXPAND" . "REGISTER-COMPILER-MACRO")
      ("CL-CC/EXPAND" . "COMPILER-MACRO-FUNCTION")
      ("CL-CC/EXPAND" . "COMPILER-MACROEXPAND-ALL"))
     (define-compiler-macro-expands-call
      compiler-macro-function-accesses-registered-expander
      define-compiler-macro-expands-funcall-function-designator
      define-compiler-macro-can-decline-with-whole-form
      define-compiler-macro-binds-environment
      compiler-macro-lambda-list-whole-and-environment))
    (360 360
     ("packages/compile/src/codegen-core.lisp"
      "packages/compile/src/codegen-core-control.lisp"
      "packages/type/src/inference-handlers.lisp"
      "packages/compile/tests/codegen-control-tests.lisp"
      "packages/type/tests/inference-forms-tests.lisp"
      "packages/compile/tests/compiler-tests-extended-stdlib.lisp")
     (("CL-CC/COMPILE" . "%COMPILE-IF-BRANCH")
      ("CL-CC/TYPE" . "INFER-THE")
      ("CL-CC/TYPE" . "INFER-SETQ"))
     (codegen-the-with-declared-integer-type-emits-typep
      infer-the-matching-type-is-fixnum
      infer-setq-returns-fixnum
      compile-declaim-safety-zero-suppresses-top-level-the-type-assertion))
    (366 366
     ("packages/expand/src/macros-runtime-support.lisp"
      "packages/cps/src/cps.lisp"
      "packages/expand/tests/macros-stdlib-io-tests.lisp")
     (("CL-CC/EXPAND" . "*LOAD-TIME-VALUE-CACHE*")
      ("CL-CC/CPS" . "%MAKE-CPS-SEXP-DISPATCH-TABLE"))
     (load-time-value-expands-to-quote
      load-time-value-is-memoized-during-expansion))
    (370 370
     ("packages/compile/src/context.lisp"
      "packages/compile/src/package.lisp"
      "packages/compile/tests/context-tests.lisp")
     (("CL-CC/COMPILE" . "*BUILTIN-SPECIAL-VARIABLES*")
      ("CL-CC/COMPILE" . "%RESOLVE-PACKAGE-SYMBOL-SPECS"))
     (ctx-initialization
      ctx-repl-state-persistence))
    (374 374
     ("packages/vm/src/vm-dispatch-gf.lisp"
      "packages/vm/src/vm-dispatch-gf-multi.lisp"
      "packages/vm/src/vm-clos-execute.lisp"
      "packages/vm/tests/vm-clos-tests.lisp"
      "packages/vm/tests/vm-dispatch-gf-multi-tests.lisp"
      "packages/runtime/tests/runtime-clos-tests.lisp"
      "packages/compile/tests/clos-dispatch-tests.lisp")
     (("CL-CC/VM" . "%VM-EXTRACT-EQL-SPECIALIZER-KEYS")
      ("CL-CC/VM" . "%VM-GF-EQL-METHODS"))
     (rt-call-generic-eql-dispatch
      rt-call-generic-eql-index-precedes-class-fallback
      eql-specializer-dispatch-index
      gf-multi-single-dispatch-eql-index-hit-precedes-class
      gf-multi-single-dispatch-eql-index-avoids-linear-scan
      clos-eql-specializer))
    (376 376
     ("packages/stdlib/src/stdlib-source-clos.lisp"
      "packages/expand/src/macros-stdlib-ansi.lisp"
      "packages/expand/src/expander-control.lisp"
      "packages/compile/src/codegen-control.lisp"
      "packages/expand/tests/macros-stdlib-ansi-tests.lisp"
      "packages/expand/tests/expander-control-helpers-tests.lisp"
      "packages/compile/tests/codegen-runtime-tests.lisp")
     (("CL-CC/EXPAND" . "%EXPAND-HANDLER-CASE-FORM")
      ("CL-CC/COMPILE" . "COMPILE-AST"))
     (define-condition-basic-structure
      handler-case-no-error-wraps-in-block
      codegen-handler-case-run-cases))
    (377 377
     ("packages/ast/src/ast.lisp"
      "packages/compile/src/codegen-control.lisp"
      "packages/compile/tests/codegen-runtime-tests.lisp"
      "packages/compile/tests/compiler-tests-runtime-hof-tests.lisp")
     (("CL-CC/AST" . "MAKE-AST-UNWIND-PROTECT")
      ("CL-CC/COMPILE" . "COMPILE-AST"))
     (codegen-unwind-protect-run-cases
      unwind-protect-cleanup-visible))
    (378 378
     ("packages/compile/src/codegen-values.lisp"
      "packages/compile/src/codegen-values-helpers.lisp"
      "packages/vm/src/vm-execute-mv.lisp"
      "packages/vm/src/vm-instructions.lisp"
      "packages/optimize/src/effects.lisp"
      "packages/optimize/src/optimizer-tables.lisp"
      "packages/compile/tests/codegen-runtime-tests.lisp"
      "packages/vm/tests/vm-execute-tests-2.lisp"
      "packages/vm/tests/primitives-tests.lisp")
     (("CL-CC/COMPILE" . "%COMPILE-MVB-VALUE-REGISTERS")
      ("CL-CC/COMPILE" . "%COMPILE-VALUES-LIST-REGISTERS")
      ("CL-CC/VM" . "VM-VALUES-TYPEP-CHECK"))
     (codegen-values-compilation
      codegen-mvb-compilation-cases
      codegen-mv-call-direct-path
      vm-execute-vm-values-stores-all
      vm-execute-mv-bind-distributes
      vm-execute-vm-values-buffer-management))
    (379 379
     ("packages/vm/src/vm-dsl.lisp"
      "packages/vm/src/io.lisp"
      "packages/vm/src/vm-extensions.lisp"
      "packages/vm/src/vm-dsl.lisp"
      "packages/vm/tests/vm-extensions-tests.lisp"
      "packages/runtime/tests/runtime-advanced-tests.lisp"
      "packages/expand/tests/expander-setf-places-tests.lisp")
     (("CL-CC" . "MAKE-VM-SYMBOL-GET")
      ("CL-CC" . "MAKE-VM-SET-SYMBOL-PLIST")
      ("CL-CC/VM" . "VM-SYMBOL-PLIST-READ-SNAPSHOT")
      ("CL-CC/VM" . "VM-SYSTEM-PROPERTY-SET")
      ("CL-CC/VM" . "VM-SYSTEM-PROPERTY-GET"))
     (vm-symbol-set-and-get-roundtrip-with-host-sync
      vm-set-symbol-plist-overwrites-and-promotes-long-plist
      vm-system-property-storage-is-separate
      vm-symbol-plist-lock-and-read-barrier-are-usable
      rt-symbol-plist-roundtrip
      expander-setf-get-place))
    (306 386
     ("packages/pipeline/src/pipeline.lisp"
      "packages/compile/src/codegen.lisp"
      "packages/vm/src/vm-dispatch.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp")
     (opt-ic-transition opt-record-speculation-failure
      opt-materialize-deopt-state opt-shape-slot-offset
      opt-jit-cache-select-eviction)
     (optimize-roadmap-runtime-helpers-have-concrete-behavior))
    (403 403
     ("packages/optimize/src/ssa-construction.lisp"
      "packages/optimize/src/ssa-phi-elim.lisp"
      "packages/optimize/src/cfg-analysis.lisp"
      "packages/optimize/tests/ssa-tests.lisp")
     (ssa-destroy ssa-sequentialize-copies cfg-split-critical-edges)
     (ssa-destroy-places-phi-copies-before-terminator
      ssa-destroy-keeps-conditional-edge-phi-copy-on-target-edge
      ssa-round-trip-cases))
    (404 404
     ("packages/optimize/src/ssa-construction.lisp"
      "packages/optimize/tests/ssa-tests.lisp")
     (ssa-sequentialize-copies)
     (ssa-seq-copies-behavior
      ssa-round-trip-cases))
     (405 405
       ("packages/optimize/src/cfg-analysis.lisp"
        "packages/optimize/src/cfg.lisp"
      "packages/optimize/src/package.lisp"
      "packages/optimize/src/optimizer-pipeline-roadmap.lisp"
      "packages/optimize/tests/cfg-tests.lisp"
       "packages/optimize/tests/optimizer-roadmap-tests.lisp")
      (cfg-split-critical-edges)
      (cfg-critical-edge-splitting-inserts-landing-pad
       optimize-roadmap-pipeline-includes-modern-optimization-passes))
    (388 388
     ("packages/codegen/src/x86-64-regs.lisp"
      "packages/codegen/src/x86-64-codegen.lisp"
      "packages/codegen/src/x86-64-codegen-dispatch.lisp"
      "packages/codegen/src/x86-64-encoding.lisp"
      "packages/codegen/src/aarch64-codegen.lisp"
      "packages/codegen/src/aarch64-program.lisp"
      "packages/codegen/src/aarch64-emitters.lisp"
      "packages/cli/src/args.lisp"
      "packages/cli/src/handlers.lisp"
      "packages/emit/tests/x86-64-codegen-tests.lisp"
      "packages/emit/tests/x86-64-encoding-size-tests.lisp"
      "packages/emit/tests/x86-64-encoding-tests.lisp"
      "packages/emit/tests/aarch64-codegen-tests.lisp"
      "packages/cli/tests/args-tests.lisp"
      "packages/cli/tests/cli-tests.lisp")
     (("CL-CC/CODEGEN" . "*X86-64-OMIT-FRAME-POINTER*")
      ("CL-CC/CODEGEN" . "X86-64-CODEGEN-TARGET")
      ("CL-CC/CODEGEN" . "X86-64-MEMORY-MOD")
      ("CL-CC/CODEGEN" . "*A64-OMIT-FRAME-POINTER*")
      ("CL-CC/CODEGEN" . "A64-CODEGEN-TARGET"))
     (x86-64-fpe-codegen-target-frees-rbp
      x86-64-empty-program-minimal-return-byte
      x86-64-leaf-and-nonleaf-without-spills-share-fpe-layout
      x86-vm-program-default-fpe-allocates-rsp-spill-frame
      x86-vm-program-debug-opt-out-keeps-rbp-spills
      x86-mov-memory-displacement-widths
      aarch64-fpe-codegen-target-frees-x29
      aarch64-leaf-and-nonleaf-without-spills-share-fpe-layout
      aarch64-default-fpe-uses-sp-relative-spill-frame
      aarch64-debug-opt-out-keeps-fp-lr-pair-and-fp-spills
      |ARGS-TESTS/CLI-ARGS-BOOL-FLAGS [debug]|
      cli-do-compile-debug-binds-backend-frame-pointer-switches))
    (389 389
     ("packages/codegen/src/x86-64-regs.lisp"
      "packages/compile/src/codegen-locals.lisp"
      "packages/emit/src/package.lisp"
      "packages/emit/tests/x86-64-regs-tests.lisp"
      "packages/emit/tests/x86-64-encoding-size-tests.lisp"
      "packages/emit/tests/x86-64-emit-tests.lisp")
     (("CL-CC/CODEGEN" . "X86-64-RED-ZONE-SPILL-P"))
     (x86-64-regs-red-zone-spill-leaf-within-limit-returns-true
      x86-vm-program-leaf-red-zone-spills-skip-rbp-frame
      x86-emit-spill-operations-rsp-red-zone))
    (391 391
     ("packages/codegen/src/x86-64-codegen.lisp"
      "packages/codegen/src/aarch64-program.lisp"
      "packages/emit/tests/x86-64-encoding-size-tests.lisp"
      "packages/emit/tests/aarch64-codegen-tests.lisp")
     (("CL-CC/CODEGEN" . "EMIT-X86-64-STACK-PROBES")
      ("CL-CC/CODEGEN" . "EMIT-A64-STACK-PROBES"))
     (x86-stack-probe-count-thresholds
      x86-stack-probe-emits-non-mutating-rsp-page-touch
      x86-large-spill-frame-inserts-stack-probe-before-rsp-allocation
       aarch64-stack-probe-emits-page-touch-sequence
       aarch64-large-spill-frame-inserts-stack-probe-before-prologue-when-fpe-disabled))
    (400 400
     ("packages/expand/src/macros-control-flow-case.lisp"
      "packages/expand/src/macros-stdlib.lisp"
      "packages/expand/tests/macros-control-flow-loop-tests.lisp"
      "packages/expand/tests/macro-ecase-tests.lisp"
      "packages/expand/tests/macro-etypecase-tests.lisp")
     (("CL-CC/EXPAND" . "%PRUNE-TYPECASE-CLAUSES")
      ("CL-CC/EXPAND" . "%TYPECASE-BUILD-TYPEP-CHAIN"))
     (case-expands-dense-integer-keys-into-table-dispatch
      ecase-expands-to-let-with-case
      etypecase-expands-to-let-with-typecase))
    (401 401
     ("packages/expand/src/macros-control-flow-case.lisp"
      "packages/expand/src/macros-stdlib.lisp"
      "packages/expand/tests/macros-control-flow-loop-tests.lisp"
      "packages/expand/tests/macro-ecase-tests.lisp"
      "packages/expand/tests/macro-etypecase-tests.lisp")
     (("CL-CC/EXPAND" . "%CASE-EXPAND-INTEGER-TABLE")
      ("CL-CC/EXPAND" . "%PRUNE-TYPECASE-CLAUSES"))
     (case-expands-dense-integer-keys-into-table-dispatch
      ecase-expands-to-let-with-case
      etypecase-expands-to-let-with-typecase))
    (462 462
     ("packages/cli/src/main-utils.lisp"
      "packages/cli/src/main-dump.lisp"
      "packages/cli/src/handlers.lisp"
      "packages/cli/tests/main-utils-tests.lisp"
      "packages/cli/tests/main-dump-tests.lisp")
     (("CL-CC/CLI" . "%WRITE-FLAMEGRAPH-SVG")
      ("CL-CC/CLI" . "%PARSE-COMPILE-OPTS")
      ("CL-CC/CLI" . "COMPILE-OPTS-FLAMEGRAPH-PATH"))
     (cli-write-flamegraph-svg-emits-svg-document
      cli-parse-compile-opts-reads-shared-flags))
    (463 463
     ("packages/cli/src/main-dump.lisp"
      "packages/cli/src/main-utils.lisp"
      "packages/cli/src/args.lisp"
      "packages/cli/src/handlers.lisp"
      "packages/pipeline/src/pipeline.lisp"
      "packages/cli/tests/main-dump-tests.lisp"
      "packages/cli/tests/cli-tests.lisp")
     (("CL-CC/CLI" . "%DUMP-IR-PHASE")
      ("CL-CC/CLI" . "%DUMP-AST-PHASE")
      ("CL-CC/CLI" . "%DUMP-CPS-PHASE")
      ("CL-CC/CLI" . "%DUMP-SSA-PHASE")
      ("CL-CC/CLI" . "%DUMP-VM-PHASE")
      ("CL-CC/CLI" . "%DUMP-OPT-PHASE")
      ("CL-CC/CLI" . "%DUMP-ASM-PHASE")
      ("CL-CC/CLI" . "*IR-PHASE-DUMP-FNS*")
      ("CL-CC/CLI" . "*IR-PHASES*"))
     (cli-dump-ir-phase-dispatches-all-phases
      cli-dump-ir-phase-annotate-source-writes-comment-for-ast
      cli-dump-ir-phase-annotate-source-writes-comment-for-vm-and-opt
      cli-dump-ir-phase-asm-output-is-ansi-colored
      cli-dump-ir-phase-annotate-source-omits-comment-on-missing-location
      cli-real-file-dump-ir-annotation-preserves-source-location
      cli-do-compile-dump-ir-annotate-source-preserves-real-file-location
      cli-do-compile-dump-ir-annotate-source-macro-forms-preserve-real-file-location
      cli-dump-ir-phase-phase-table-covers-all-recognized-phases
      cli-dump-ir-phase-invalid-signals-error))
     (465 465
      ("packages/pipeline/src/pipeline-native.lisp"
       "packages/compile/tests/pipeline-native-tests.lisp"
       "packages/compile/tests/pipeline-native-io-tests.lisp")
      (("CL-CC/PIPELINE" . "%COMPILE-CACHE-KEY")
       ("CL-CC/PIPELINE" . "%COMPILE-CACHE-PATH")
       ("CL-CC/PIPELINE" . "COMPILE-FILE-TO-NATIVE"))
      (pipeline-native-compile-file-cache-hit-copies-artifact
       pipeline-native-compile-file-cache-hit-skips-native-compilation
       pipeline-native-cache-key-differs-by-dimension
       pipeline-native-cache-key-ignores-observability-options
       pipeline-native-compile-file-cache-key-receives-option-plist))
     (387 502
      ("packages/codegen/src/x86-64-codegen.lisp"
       "packages/codegen/src/aarch64-codegen.lisp"
       "packages/regalloc/src/regalloc.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp")
     (opt-sea-node-schedulable-p opt-merge-module-summaries
      opt-stack-map-live-root-p opt-shape-slot-offset
      opt-adaptive-compilation-threshold)
     (optimize-roadmap-support-helpers-have-conservative-behavior))
    (516 516
     ("packages/optimize/src/optimizer-strength-ext.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-strength-ext-tests.lisp"
      "packages/optimize/tests/optimizer-strength-inline-tests.lisp")
     (opt-pass-reassociate opt-reassociate-commutative-p opt-copy-commutative-binop)
     (reassociate-commutative-p-true-for-commutative-ops
      reassociate-moves-constant-inward
      commutative-binop-table-coverage))
    (517 517
     ("packages/optimize/src/optimizer-dataflow.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-dataflow-tests.lisp")
     (opt-run-dataflow opt-compute-available-expressions opt-compute-reaching-definitions)
     (dataflow-worklist-forward-branch-join-converges
      available-expressions-join-intersects-predecessors
      reaching-definitions-join-unions-predecessors))
    (518 518
     ("packages/optimize/src/optimizer-dataflow.lisp"
      "packages/optimize/src/optimizer-cse-gvn.lisp"
      "packages/optimize/tests/optimizer-dataflow-tests.lisp"
      "packages/optimize/tests/optimizer-cse-gvn-tests.lisp")
     (opt-compute-available-expressions opt-compute-reaching-definitions opt-pass-gvn)
     (available-expressions-join-intersects-predecessors
      reaching-definitions-join-unions-predecessors
      gvn-uses-available-expressions-at-join))
    (519 519
     ("packages/optimize/src/egraph-saturation.lisp"
      "packages/optimize/src/egraph.lisp"
      "packages/optimize/tests/egraph-extraction-tests.lisp")
     (egraph-extract egraph-default-cost)
     (egraph-extract-nullary-node-returns-non-nil
      egraph-extract-binary-add-returns-compound))
    (520 520
     ("packages/optimize/src/optimizer-copyprop.lisp"
      "packages/optimize/src/optimizer-pipeline.lisp"
      "packages/optimize/tests/optimizer-copyprop-tests.lisp")
     (opt-pass-copy-prop %opt-copy-prop-merge %opt-copy-prop-rewrite-block)
     (copyprop-merge-disagreement-cases
      copyprop-pass-basic-rewrite
      copyprop-pass-chain-rewrite
      copy-prop-rewrite-block-rewrites-instructions))
    (521 521
     ("packages/optimize/src/optimizer-cse-gvn.lisp"
      "packages/optimize/src/cfg.lisp"
      "packages/optimize/tests/optimizer-cse-gvn-tests.lisp"
      "packages/optimize/tests/optimizer-tests-lowlevel2.lisp")
     (opt-pass-gvn cfg-compute-dominators)
     (gvn-uses-available-expressions-at-join
      gvn-redundant-overwrite-does-not-poison-same-syntax-expression
      optimizer-gvn-dominates-branch))
    (522 522
     ("packages/optimize/src/optimizer-purity.lisp"
      "packages/optimize/src/optimizer-inline-cost.lisp"
      "packages/optimize/src/optimizer-inline-pass.lisp"
      "packages/optimize/tests/optimizer-inline-tests.lisp")
      (opt-build-call-graph opt-call-graph-recursive-labels
       opt-pass-inline opt-pass-global-dce)
      (opt-build-call-graph-no-calls
       opt-call-graph-recursive-labels-no-recursion
       opt-call-graph-recursive-labels-direct-recursion
       opt-call-graph-recursive-labels-mutual-recursion
       opt-pass-inline-skips-recursive-callee))
    (523 523
     ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp"
      "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
     (opt-build-affine-loop-summary
      opt-pass-affine-loop-analysis)
     (optimize-affine-loop-summary-builds-descriptor
      optimize-pass-affine-loop-analysis-captures-real-loop-summary
      optimize-backend-roadmap-analysis-evidence-is-loaded
      optimize-backend-roadmap-evidence-covers-doc-fr-list))
    (524 524
     ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp"
      "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
     (opt-loop-interchange-plan
      opt-pass-loop-interchange)
     (optimize-loop-interchange-plan-requires-safety
      optimize-pass-loop-interchange-handles-nested-canonical-loop
      optimize-pass-loop-interchange-skips-side-effecting-loop
      optimize-backend-roadmap-analysis-evidence-is-loaded
      optimize-backend-roadmap-evidence-covers-doc-fr-list))
    (525 525
     ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp"
      "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
     (opt-polyhedral-schedule-plan
      opt-pass-polyhedral-schedule)
     (optimize-polyhedral-schedule-plan-preserves-objective
      optimize-pass-polyhedral-schedule-reorders-loop-body
      optimize-backend-roadmap-analysis-evidence-is-loaded
      optimize-backend-roadmap-evidence-covers-doc-fr-list))
    (526 526
     ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp"
      "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
     (opt-loop-fusion-fission-plan
      opt-pass-loop-fusion-fission)
     (optimize-loop-fusion-fission-plan-selects-strategy
      optimize-pass-loop-fusion-fission-fuses-adjacent-loops
      optimize-pass-loop-fusion-fission-skips-unsafe-fusion
      optimize-pass-loop-fusion-fission-splits-oversized-loop
      optimize-backend-roadmap-analysis-evidence-is-loaded
      optimize-backend-roadmap-evidence-covers-doc-fr-list))
    (527 527
     ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp"
      "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
     (opt-ml-inline-score-plan)
     (optimize-ml-inline-score-plan-is-deterministic
      optimize-backend-roadmap-analysis-evidence-is-loaded
      optimize-backend-roadmap-evidence-covers-doc-fr-list))
    (528 528
     ("packages/optimize/src/optimizer-pipeline-speculative.lisp"
      "packages/optimize/tests/optimizer-pipeline-tests.lisp"
      "packages/optimize/tests/optimizer-roadmap-backend-tests.lisp")
     (opt-learned-codegen-cost-plan)
     (optimize-learned-codegen-cost-plan-is-target-aware
      optimize-backend-roadmap-analysis-evidence-is-loaded
      optimize-backend-roadmap-evidence-covers-doc-fr-list))
    "Alist of (lo hi modules api-symbols test-anchors) range entries for
`%opt-backend-roadmap-evidence-profile'."))

(defun %opt-backend-roadmap-evidence-profile (feature-id)
  "Return audit-evidence anchors for a docs/optimize-backend.md FR.
Backend FRs span compiler analysis, runtime modelling, native code generation,
and tooling.  Status still comes from the roadmap heading: unmarked headings
are tracked as planned evidence, not completed implementation."
  (let ((n (%opt-roadmap-feature-number feature-id)))
    (let ((entry (and n
                      (find-if (lambda (e)
                                 (destructuring-bind (lo hi . _rest) e
                                   (declare (ignore _rest))
                                   (and (<= lo n)
                                        (or (null hi) (<= n hi)))))
                               +opt-backend-roadmap-evidence-profile-ranges+))))
      (if entry
          (destructuring-bind (_lo _hi modules api-symbols test-anchors) entry
            (declare (ignore _lo _hi))
            (values modules api-symbols test-anchors))
          (error "No optimize-backend evidence profile entry for ~A" feature-id)))))

(defun make-opt-roadmap-evidence-for-feature
    (feature &key (doc-module "docs/optimize-passes.md") profile-function)
  "Create subsystem-specific evidence for FEATURE."
  (let* ((feature-id (opt-roadmap-feature-id feature))
         (status (%opt-roadmap-evidence-status feature))
         (profile-function (or profile-function #'%opt-roadmap-evidence-profile)))
    (multiple-value-bind (modules api-symbols test-anchors)
        (funcall profile-function feature-id)
      (make-opt-roadmap-evidence
       :feature-id feature-id
       :status status
       :modules (remove-duplicates
                  (cons doc-module modules)
                  :test #'string=)
       :api-symbols api-symbols
       :test-anchors test-anchors
       :summary (format nil "~A [~A]: ~A"
                        feature-id status (opt-roadmap-feature-title feature))))))

(defun optimize-roadmap-register-doc-evidence (&optional (pathname (%opt-roadmap-doc-pathname)))
  "Populate `*opt-roadmap-evidence-registry*` from docs/optimize-passes.md."
  (let ((registry (make-hash-table :test #'equal)))
    (dolist (feature (optimize-roadmap-doc-features pathname))
      (let ((evidence (make-opt-roadmap-evidence-for-feature feature)))
        (setf (gethash (opt-roadmap-evidence-feature-id evidence) registry)
              evidence)))
    (setf *opt-roadmap-evidence-registry* registry)))

(defun optimize-backend-roadmap-register-doc-evidence
    (&optional (pathname (%opt-backend-roadmap-doc-pathname)))
  "Populate `*opt-backend-roadmap-evidence-registry*` from docs/optimize-backend.md."
  (let ((registry (make-hash-table :test #'equal)))
    (dolist (feature (optimize-backend-roadmap-doc-features pathname))
      (let ((evidence (make-opt-roadmap-evidence-for-feature
                       feature
                       :doc-module "docs/optimize-backend.md"
                       :profile-function #'%opt-backend-roadmap-evidence-profile)))
        (setf (gethash (opt-roadmap-evidence-feature-id evidence) registry)
              evidence)))
    (setf *opt-backend-roadmap-evidence-registry* registry)))

(defun lookup-opt-roadmap-evidence (feature-id)
  "Return implementation evidence for FEATURE-ID, populating the registry lazily."
  (let ((registry *opt-roadmap-evidence-registry*))
    (when (zerop (hash-table-count registry))
      (setf registry (optimize-roadmap-register-doc-evidence)))
    (gethash feature-id registry)))

(defun lookup-opt-backend-roadmap-evidence (feature-id)
  "Return optimize-backend implementation evidence for FEATURE-ID."
  (let ((registry *opt-backend-roadmap-evidence-registry*))
    (when (zerop (hash-table-count registry))
      (setf registry (optimize-backend-roadmap-register-doc-evidence)))
    (gethash feature-id registry)))

(defun optimize-roadmap-implementation-evidence-complete-p (evidence)
  "Return T when EVIDENCE references concrete modules, APIs, and tests."
  (and evidence
       (eq :implemented (opt-roadmap-evidence-status evidence))
       (optimize-roadmap-evidence-well-formed-p evidence)))

(defun optimize-backend-roadmap-implementation-evidence-complete-p (evidence)
  "Return T only when optimize-backend EVIDENCE is marked implemented and its anchors resolve."
  (optimize-roadmap-implementation-evidence-complete-p evidence))

(defstruct (opt-ic-site (:conc-name opt-ic-site-))
  "Polymorphic inline-cache state for one call site."
  (state :uninitialized :type keyword)
  (entries nil :type list)
  (max-polymorphic-entries 4 :type integer)
  (misses 0 :type integer)
  (megamorphic-fallback nil))
