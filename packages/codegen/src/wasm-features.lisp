;;;; packages/codegen/src/wasm-features.lisp — Wasm Feature Flags
;;;;
;;;; Centralized feature gates for all Wasm proposals implemented in
;;;; the cl-cc wasm backend. Each flag defaults to the most appropriate
;;;; value given current browser/engine support as of 2026.
;;;;
;;;; Default-NIL policy: flags that change the ABI, require security headers,
;;;; depend on unstable proposals, or need host/runtime coordination stay opt-in.
;;;; These defaults are intentionally conservative and must not be flipped by
;;;; documentation-only or metadata-only Wasm work.

(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Phase 4 Standard (all major browsers) — enabled by default
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *wasm-non-trapping-float-to-int-enabled* t
  "FR-233: Use i32.trunc_sat_f32_s instead of i32.trunc_f32_s.") ;; MVP v1.1

(defparameter *wasm-sign-extension-enabled* t
  "FR-234: Use i32.extend8_s/i32.extend16_s/i64.extend*_s opcodes.") ;; MVP v1.1

(defparameter *wasm-bulk-memory-enabled* t
  "FR-228: Use memory.copy/memory.fill/memory.init/data.drop.") ;; MVP v1.1

(defparameter *wasm-bulk-table-enabled* t
  "FR-237: Use table.init/table.copy/table.fill/elem.drop.") ;; MVP v1.1

;;; ABI-sensitive proposal defaults remain NIL until all call boundaries agree.

(defparameter *wasm-multi-value-enabled* nil
  "FR-235: Use wasm multi-value returns for CL values. WARNING: changes ABI.") ;; MVP v1.1

(defparameter *wasm-js-bigint-i64-enabled* nil
  "FR-236: Enable JS BigInt <-> i64 conversion at boundary. Default off for compat.")

(defparameter *wasm-ref-types-externref-enabled* t
  "FR-226: Use externref for opaque JS object references.") ;; Chrome 91+

(defparameter *wasm-simd128-enabled* t
  "FR-202: Enable SIMD128 instructions (v128.load, i32x4.add, f64x2.mul, etc.).")

(defparameter *wasm-relaxed-simd-enabled* t
  "FR-214: Enable Relaxed SIMD (f32x4.relaxed_madd, etc.).") ;; Chrome 114+

(defparameter *wasm-threads-enabled* nil
  "FR-203: Enable shared memory, atomic operations, and worker threads.") ;; All browsers, security-sensitive

(defparameter *wasm-exception-handling-enabled* t
  "FR-204: Use native try/catch/throw for CL exception handling.") ;; Chrome 95+

(defparameter *wasm-exception-handling-v2-enabled* t
  "FR-252: Use try_table/throw_ref/exnref for EH v2.") ;; Chrome 123+

;;; Threading/memory expansion defaults remain NIL unless the host can provide
;;; SharedArrayBuffer isolation, memory64/table64 support, and matching runtime ABI.

(defparameter *wasm-multiple-memories-enabled* nil
  "FR-208: Use multiple linear memories (stack/heap/strings).") ;; Chrome 92+

(defconstant +wasm-memory-stack+ 0
  "FR-208: Memory index 0, used for stack-oriented linear-memory helpers.")

(defconstant +wasm-memory-gc-heap+ 1
  "FR-208: Memory index 1, used for GC heap linear-memory helpers.")

(defconstant +wasm-memory-strings+ 2
  "FR-208: Memory index 2, used for string interning/transcoding helpers.")

(defparameter *wasm-typed-func-refs-enabled* t
  "FR-212: Use typed function references (call_ref, ref.func).") ;; Chrome 113+

(defparameter *wasm-extended-const-exprs-enabled* t
  "FR-215: Use global.get + arithmetic in global initializers.") ;; Chrome 113+

(defparameter *wasm-tail-call-enabled* t
  "FR-143: Use return_call/return_call_indirect for tail calls.") ;; Chrome 112+

(defparameter *wasm-memory64-enabled* nil
  "FR-213: Use 64-bit linear memory addressing.") ;; Chrome 119+, opt-in

(defparameter *wasm-table64-enabled* nil
  "FR-229: Use 64-bit function table indices. Requires Memory64.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Wasm GC Proposal — enabled by default (Chrome 119+, Firefox 120+, Safari 17.4+)
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *wasm-gc-enabled* t
  "Master flag for Wasm GC proposal features (i31ref, struct, array).")

(defparameter *wasm-i31ref-optimize-enabled* t
  "FR-209: Use i31ref for fixnum native boxing.")

(defparameter *wasm-gc-struct-types-enabled* t
  "FR-210: Generate dedicated Wasm struct types for CL classes.")

(defparameter *wasm-gc-array-types-enabled* t
  "FR-211: Use typed GC arrays (fixnum/f64/char arrays).")

(defparameter *wasm-gc-full-hierarchy-enabled* t
  "FR-231: Use anyref/eqref/i31ref/structref/arrayref hierarchy with br_on_cast.")

(defparameter *wasm-gc-rec-types-enabled* t
  "FR-254: Use rec type groups for recursive struct definitions.")

(defparameter *wasm-gc-null-safety-enabled* t
  "FR-270: Use br_on_null/ref.as_non_null for null-safe access.")

(defparameter *wasm-gc-packed-fields-enabled* t
  "FR-283: Use i8/i16 packed fields in GC structs.")

(defparameter *wasm-gc-bulk-array-ops-enabled* t
  "FR-284: Use array.fill/array.init_data/array.init_elem.")

(defparameter *wasm-ref-eq-enabled* t
  "FR-285: Use ref.eq for eqref identity comparison.")

(defparameter *wasm-any-extern-convert-enabled* t
  "FR-286: Use any.convert_extern/extern.convert_any for JS<->GC bridging.")

(defparameter *wasm-gc-frozen-values-enabled* t
  "FR-247: Use struct.new_immutable/array.new_immutable for frozen constants.")

(defparameter *wasm-gc-more-array-constructors-enabled* t
  "FR-249: Use array.new_default/array.new_data/array.new_elem.")

(defparameter *wasm-gc-multibyte-array-access-enabled* t
  "FR-250: Use array.load2_u/array.load4_u for packed element access.")

(defparameter *wasm-gc-finalization-enabled* nil
  "FR-255: Use WeakRef/FinalizationRegistry for CL finalize. Phase 1, experimental.")

(defparameter *wasm-gc-write-barriers-enabled* nil
  "FR-267: Emit write barriers for generational GC cooperation. Phase 1, experimental.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Compiler / Toolchain Features
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *wasm-ref-cast-elimination-enabled* t
  "FR-142: Eliminate redundant ref.cast after struct.new.")

(defparameter *wasm-integer-range-annotation-enabled* t
  "FR-145: Skip unnecessary fixnum unbox/box when range is known.")

(defparameter *wasm-typed-closure-env-enabled* t
  "FR-144: Use array.new_fixed for typed closure environments.")

(defparameter *wasm-aot-mode-enabled* nil
  "FR-219: Generate complete static .wasm binary in single pass.")

(defparameter *wasm-pgo-enabled* nil
  "FR-220: Profile-guided optimization for wasm.")

(defparameter *wasm-dead-import-elimination-enabled* t
  "FR-221: Only emit imports for actually-used host functions.")

(defparameter *wasm-lto-enabled* nil
  "FR-260: Link-time cross-module inlining via Binaryen.")

(defparameter *wasm-tiered-compilation-hints-enabled* t
  "FR-259: Emit Liftoff/TurboFan tiering hints.")

(defparameter *wasm-deterministic-builds-enabled* nil
  "FR-265: Generate reproducible .wasm binaries.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Debug / Development
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *wasm-dwarf-debug-info-enabled* nil
  "FR-222: Embed DWARF 5 debug info in Wasm custom sections.")

(defparameter *wasm-source-map-enabled* t
  "FR-223: Generate Source Map v3 for browser debugging.")

(defparameter *wasm-source-map-url* "module.wasm.map"
  "FR-223: sourceMappingURL payload embedded in the Wasm custom section.")

(defparameter *wasm-extended-names-enabled* t
  "FR-242: Embed local variable names in Name Section.")

(defparameter *wasm-branch-hinting-enabled* t
  "FR-216: Emit branch hint metadata for V8/SpiderMonkey.")

(defparameter *wasm-custom-annotations-enabled* t
  "FR-225: Emit cl-cc.annotations custom section for compiler hints.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; JS / Browser Integration
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *wasm-js-promise-integration-enabled* t
  "FR-217: Use WebAssembly.suspending/promising for async JS calls.")

(defparameter *wasm-string-builtins-enabled* t
  "FR-218: Use Wasm String Builtins for efficient string operations.")

(defparameter *wasm-esm-integration-enabled* t
  "FR-230: Generate ESM-compatible .wasm exports.")

(defparameter *wasm-streaming-compilation-enabled* t
  "FR-232: Use instantiateStreaming for parallel download+compile.")

(defparameter *wasm-exception-js-api-enabled* t
  "FR-262: Use WebAssembly.Exception JS API for CL<->JS exception bridge.")

(defparameter *wasm-type-reflection-js-api-enabled* nil
  "FR-263: Use WebAssembly.Descriptor API for runtime type inspection.")

(defparameter *wasm-coop-coep-headers-enabled* nil
  "FR-297: Emit COOP/COEP headers for SharedArrayBuffer support.")

(defparameter *wasm-service-worker-enabled* nil
  "FR-302: Generate Service Worker + manifest.json for PWA.")

(defparameter *wasm-coop-enabled* nil
  "FR-297: Emit Cross-Origin-Opener-Policy guidance for wasm deployment.")

(defparameter *wasm-coep-enabled* nil
  "FR-297: Emit Cross-Origin-Embedder-Policy guidance for wasm deployment.")

(defparameter *wasm-sri-hashes-enabled* nil
  "FR-307: Generate SHA-256 SRI integrity hashes.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Dynamic Linking / Runtime
;;; ─────────────────────────────────────────────────────────────────────────────

;;; Runtime integration defaults remain NIL because they require loader/linker or
;;; REPL coordination outside the core single-module compilation path.

(defparameter *wasm-dynamic-linking-enabled* nil
  "FR-227: Support side-module dynamic linking.")

(defparameter *wasm-hot-code-reload-enabled* nil
  "FR-317: Support runtime function patching via table.set.")

(defparameter *wasm-repl-incremental-compilation-enabled* nil
  "FR-288: Support incremental compilation for REPL.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Security
;;; ─────────────────────────────────────────────────────────────────────────────

;;; Security opt-ins default to NIL when they can alter semantics or deployment
;;; requirements; metadata-only hardening flags may stay enabled.

(defparameter *wasm-typed-select-enabled* t
  "FR-279: Typed select for reference type conditional selection.")

(defparameter *wasm-csp-compliant-enabled* t
  "FR-261: Avoid wasm-unsafe-eval CSP directive requirement.")

(defparameter *wasm-constant-time-enabled* nil
  "FR-261: Convert crypto branches to select for timing safety.")

(defparameter *wasm-cfi-enabled* t
  "FR-261: Emit CFI-oriented typed call_ref/call_indirect metadata.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Experimental / Phase 1-3 Proposals (default OFF)
;;; ─────────────────────────────────────────────────────────────────────────────

;;; Experimental flags in this section default to NIL until proposal syntax,
;;; browser/toolchain support, and cl-cc runtime semantics are all stable.

(defparameter *wasm-wide-arithmetic-enabled* nil
  "FR-238: Use i64.add128/sub128/mul_wide for 128-bit bignum ops. Phase 3.")

(defparameter *wasm-custom-page-sizes-enabled* nil
  "FR-239: Use 4KB memory pages for finer GC heap. Phase 3.")

(defparameter *wasm-custom-page-size* 4096
  "FR-239: Requested custom wasm memory page size in bytes when enabled.")

(defparameter *wasm-compact-import-section-enabled* nil
  "FR-240: Compress import names in binary. Phase 3.")

(defparameter *wasm-custom-descriptors-enabled* nil
  "FR-241: Use typed externref descriptors. Phase 3.")

(defparameter *wasm-half-precision-enabled* nil
  "FR-248: Use f16 type for ML workloads. Phase 1.")

(defparameter *wasm-memory-control-enabled* nil
  "FR-243: Use memory.discard for page return to OS. Phase 1.")

(defparameter *wasm-type-imports-enabled* nil
  "FR-244: Import types across module boundaries. Phase 1.")

(defparameter *wasm-jit-interface-enabled* nil
  "FR-245: Use Wasm JIT feedback hooks. Phase 1.")

(defparameter *wasm-flexible-vectors-enabled* nil
  "FR-246: Support variable SIMD width (128/256/512-bit). Phase 1.")

(defparameter *wasm-reference-typed-strings-enabled* nil
  "FR-251: Use stringref type for native string representation. Phase 1.")

(defparameter *wasm-stack-switching-enabled* nil
  "FR-205: Use cont.new/cont.bind/resume/suspend for coroutines. Phase 2, experimental.")

(defparameter *wasm-shared-everything-threads-enabled* nil
  "FR-224: Use (shared struct/array) for worker-shared GC objects. Phase 2, experimental.")

(defparameter *wasm-component-model-enabled* nil
  "FR-206: Generate .wasm components with WIT type mapping. Phase 2.")

(defparameter *wasm-wasi-p2-enabled* nil
  "FR-207: Full WASI Preview 2 capability-based interface.")

(defparameter *wasm-wasi-p3-enabled* nil
  "FR-257: WASI 0.3 async-first I/O. Phase 2, experimental.")

(defparameter *wasm-wasi-p1-compat-enabled* nil
  "FR-321: WASI Preview 1 compatibility shim layer.")

(defparameter *wasm-startup-snapshots-enabled* nil
  "FR-287: Use V8 heap serialization for fast startup. Phase 0, experimental.")

(defparameter *wasm-memory-profiler-enabled* nil
  "FR-318: Chrome DevTools heap profiler integration.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; Remaining FRs (verified by Oracle gap analysis — Phase 48-53 missing flags)
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *wasm-return-call-ref-enabled* t
  "FR-253: return_call_ref for typed closure tail calls.")

(defparameter *wasm-atomic-fence-enabled* t
  "FR-256: atomic.fence for memory barriers.")

(defparameter *wasm-profiles-enabled* t
  "FR-258: Wasm Profiles feature set declarations.")

(defparameter *wasm-lazy-function-bodies-enabled* nil
  "FR-264: Lazy function bodies for deferred compilation. Phase 2.")

(defparameter *wasm-global-atomic-enabled* nil
  "FR-266: global.atomic for shared globals. Phase 1 experimental.")

(defparameter *wasm-decimal-enabled* nil
  "FR-268: Wasm Decimal — IEEE 754 decimal floating point. Phase 1.")

(defparameter *wasm-call-stack-inspection-enabled* nil
  "FR-269: call_stack inspection for debugging. Phase 1.")

(defparameter *wasm-exnref-enabled* t
  "FR-271: exnref exception reference type.")

(defparameter *wasm-effect-handlers-enabled* nil
  "FR-272: Algebraic Effect Handlers for CL condition system. Phase 2.")

(defparameter *wasm-struct-atomic-enabled* nil
  "FR-273: struct.atomic/array.atomic for GC object atomic field ops. Phase 2.")

(defparameter *wasm-wasi-worlds-full-enabled* nil
  "FR-274: WASI Worlds — wasi:nn/wasi:http/wasi:cli. Phase 2.")

(defparameter *wasm-module-postmessage-enabled* nil
  "FR-275: WebAssembly.Module postMessage for Worker sharing.")

(defparameter *wasm-import-maps-enabled* nil
  "FR-276: Wasm Import Maps for browser module name resolution. Phase 2.")

(defparameter *wasm-cl-abi-mangling-enabled* t
  "FR-277: CL ABI / symbol name mangling for cross-language FFI.")

(defparameter *wasm-simd-complete-enabled* t
  "FR-278: SIMD instruction completion — i8x16.shuffle/dot/convert/narrow/widen.")

(defparameter *wasm-module-init-order-enabled* t
  "FR-280: Wasm initialization order — __wasm_call_ctors / toplevel execution.")

(defparameter *wasm-array-new-elem-enabled* t
  "FR-281: array.new_elem from element segments.")

(defparameter *wasm-abstract-types-enabled* nil
  "FR-282: Wasm Abstract Types — opaque host objects. Phase 1.")

(defparameter *wasm-struct-exception-payload-enabled* t
  "FR-289: GC Struct as Exception Payload — structured exception values.")

(defparameter *wasm-func-bind-enabled* nil
  "FR-290: func.bind for Wasm-level partial application. Phase 1.")

(defparameter *wasm-extended-const-ref-func-enabled* t
  "FR-291: Extended Const ref.func for function reference globals.")

(defparameter *wasm-call-indirect-table64-integration-enabled* nil
  "FR-292: call_indirect × table64 integration.")

(defparameter *wasm-multi-memory-atomics-enabled* nil
  "FR-293: Multi-memory Atomics — shared memory atomic ops. Phase 2.")

(defparameter *wasm-passive-element-data-segments-enabled* t
  "FR-294: Passive Element/Data Segments — deferred segment loading.")

(defparameter *wasm-string-transcoding-complete-enabled* nil
  "FR-295: String Transcoding complete — all string.decode/encode variants. Phase 2.")

(defparameter *wasm-wasi-extended-worlds-enabled* nil
  "FR-296: WASI Extended Worlds — wasi:keyvalue/messaging/sql. Phase 2.")

(defparameter *wasm-i8x16-swizzle-enabled* t
  "FR-298: i8x16.swizzle — variable-index SIMD lane permutation.")

(defparameter *wasm-wasi-random-crypto-enabled* t
  "FR-299: WASI wasi:random / wasi:crypto — secure RNG/crypto.")

(defparameter *wasm-multi-memory-bulk-copy-enabled* nil
  "FR-300: Multi-memory Bulk Copy — cross-memory memory.copy. Phase 2.")

(defparameter *wasm-cont-throw-enabled* nil
  "FR-301: cont.throw — exception propagation into suspended continuations. Phase 2.")

(defparameter *wasm-simd-lane-load-store-enabled* t
  "FR-303: SIMD Lane Load/Store — v128.load_lane / v128.load_splat.")

(defparameter *wasm-simd-boolean-reduction-enabled* t
  "FR-304: v128.any_true / SIMD Boolean Reduction.")

(defparameter *wasm-validate-api-enabled* t
  "FR-305: WebAssembly.validate() static validation API.")

(defparameter *wasm-atomic-wait-timeout-enabled* t
  "FR-306: memory.atomic.wait timeout — finite-time blocking wait.")

(defparameter *wasm-relaxed-dead-code-validation-enabled* t
  "FR-308: Relaxed Dead Code Validation — simplified codegen after unreachable/br.")

(defparameter *wasm-rounding-variants-enabled* t
  "FR-309: Rounding Variants — f64.nearest_int / Banker's Rounding.")

(defparameter *wasm-exception-tag-import-export-enabled* t
  "FR-310: Exception Tag Import/Export — cross-module condition type sharing.")

(defparameter *wasm-c-api-enabled* nil
  "FR-311: wasm-c-api — native C embedding API.")

(defparameter *wasm-runtime-feature-detection-enabled* t
  "FR-312: Wasm runtime feature detection.")

(defparameter *wasm-simd-load32-load64-zero-enabled* t
  "FR-313: v128.load32_zero / v128.load64_zero — zero-extending SIMD loads.")

(defparameter *wasm-relaxed-simd-integer-ops-enabled* nil
  "FR-314: Relaxed SIMD integer ops — Q15/INT8 dot/BF16. Phase 2.")

(defparameter *wasm-catch-all-ref-enabled* t
  "FR-315: catch_all_ref — catch all exceptions as exnref.")

(defparameter *wasm-js-primitive-builtins-enabled* t
  "FR-316: JS Primitive Builtins — numeric type conversion optimization.")

(defparameter *wasm-component-model-tests-enabled* nil
  "FR-319: Component Model test infrastructure — WIT interface verification. Phase 2.")

(defparameter *wasm-string-iterator-views-enabled* nil
  "FR-320: String Iterator Views — stringview_wtf16/stringview_iter. Phase 2.")

(defparameter *wasm-binary-tool-integration-enabled* t
  "FR-322: Wasm binary tool integration — wat2wasm/wasm-objdump.")

(defparameter *wasm-mvp-bit-ops-enabled* t
  "FR-323: Wasm MVP Bit Operations — clz/ctz/popcnt.")

(defparameter *wasm-copysign-enabled* t
  "FR-324: f32.copysign / f64.copysign — sign bit copy.")

(defparameter *wasm-simd-nan-semantics-documented* t
  "FR-325: Wasm SIMD NaN non-canonicalization — vector NaN semantics documented.")

(defparameter *wasm-memory-grow-oom-check-enabled* t
  "FR-326: memory.grow OOM detection — robust allocation failure handling.")

(defparameter *wasm-sub-word-atomics-enabled* nil
  "FR-327: Sub-word Atomic Operations — 8/16bit CAS. Phase 2.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; CLI Flags tracking
;;; ─────────────────────────────────────────────────────────────────────────────

(defparameter *wasm-cli-target-flags*
  '(:wasm32 :wasm64 :wasm32-wasi :wasm64-unknown-unknown)
  "Valid CLI target identifiers for the wasm backend.")

(defparameter *wasm-cli-feature-flags*
  '(("--aot" . *wasm-aot-mode-enabled*)
    ("--bigint" . *wasm-js-bigint-i64-enabled*)
    ("--threads" . *wasm-threads-enabled*)
    ("--no-threads" . *wasm-threads-enabled*)  ; negated
    ("--gc" . *wasm-gc-enabled*)
    ("--no-gc" . *wasm-gc-enabled*)  ; negated
    ("--tail-calls" . *wasm-tail-call-enabled*)
    ("--no-tail-calls" . *wasm-tail-call-enabled*)  ; negated
    ("--simd" . *wasm-simd128-enabled*)
    ("--no-simd" . *wasm-simd128-enabled*)  ; negated
    ("--exception-handling" . *wasm-exception-handling-enabled*)
    ("--source-map" . *wasm-source-map-enabled*)
    ("--debug-info" . *wasm-dwarf-debug-info-enabled*)
    ("--emit-debug-info" . *wasm-dwarf-debug-info-enabled*)
    ("--emit-names" . *wasm-extended-names-enabled*)
    ("--type-reflection" . *wasm-type-reflection-js-api-enabled*)
    ("--stack-inspection" . *wasm-call-stack-inspection-enabled*)
    ("--memory-profiler" . *wasm-memory-profiler-enabled*)
    ("--hot-reload" . *wasm-hot-code-reload-enabled*)
    ("--incremental-repl" . *wasm-repl-incremental-compilation-enabled*)
    ("--streaming" . *wasm-streaming-compilation-enabled*)
    ("--pgo" . *wasm-pgo-enabled*)
    ("--memory64" . *wasm-memory64-enabled*)
    ("--constant-time" . *wasm-constant-time-enabled*))
  "Mapping from CLI flags to wasm feature flag variables.")
