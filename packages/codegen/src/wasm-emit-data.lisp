;;;; packages/emit/src/wasm-emit.lisp — WASM emit-instruction Methods and Entry Points
;;;;
;;;; Per-instruction emit-instruction methods for the wasm-target class,
;;;; plus the compile-to-wasm-wat entry point.
;;;;
;;;; WAT module structure emitters (emit-wat-type-section, emit-wasm-module, etc.)
;;;; and the wasm-target class are in wasm.lisp (loads before this file).
;;;;
;;;; Load order: after wasm.lisp.

(in-package :cl-cc/codegen)

(defparameter *wasm-tail-call-enabled* t
  "Feature gate for wasm tail-call opcodes.

When NIL, wasm tail-call lowering falls back to call_indirect.")

(defparameter *wasm-eh-enabled* t
  "Feature gate for Wasm Exception Handling proposal (try/catch/throw).
FR-204, FR-252, FR-262, FR-271, FR-289, FR-310.")

(defparameter *wasm-typed-refs-enabled* nil
  "Feature gate for Wasm Typed Function References (call_ref/ref.func).
FR-212, FR-253. Default NIL to preserve call_indirect compatibility.")

(defparameter *wasm-eh-v2-enabled* t
  "Feature gate for Wasm Exception Handling v2 (try_table/throw_ref).
FR-252, FR-271.")

(defparameter *wasm-js-exception-bridge-enabled* t
  "Feature gate for Wasm JS Exception Bridge (WebAssembly.Exception).
FR-262.")

(defparameter *wasm-exnref-enabled* t
  "Feature gate for Wasm exnref type (exception references).
FR-271.")

(defparameter *wasm-gc-exception-payload-enabled* t
  "Feature gate for GC struct as exception payload.
FR-289.")

(defparameter *wasm-extended-const-ref-func-enabled* t
  "Feature gate for Extended Const Expressions with ref.func.
FR-215, FR-291.")

(defparameter *wasm-continuation-exceptions-enabled* t
  "Feature gate for continuation exception injection (cont.throw).
FR-301.")

(defparameter *wasm-exception-tag-linking-enabled* t
  "Feature gate for exception tag import/export across modules.
FR-310.")

(defparameter *wasm-catch-all-ref-enabled* t
  "Feature gate for catch_all_ref in try_table.
Catch-all with reference capture.")

(defparameter *wasm-simd-enabled* t
  "Feature gate for Wasm SIMD128 proposal.
FR-202, FR-214, FR-278, FR-298, FR-303, FR-304.
Override: CLCC_WASM_SIMD=1|0")

(defparameter *wasm-relaxed-simd-enabled* t
  "Feature gate for Wasm Relaxed SIMD (fused multiply-add, relaxed min/max).
FR-214. Requires *wasm-simd-enabled* to be T.
Override: CLCC_WASM_RELAXED_SIMD=1|0")

(defparameter *wasm-strict-nan* nil
  "When T, use strict IEEE 754 NaN semantics (disables relaxed SIMD).
Override: CLCC_WASM_STRICT_NAN=1|0")

(defparameter *wasm-threads-enabled* nil
  "Feature gate for Wasm Threads/Atomics proposal.
FR-203, FR-256, FR-266, FR-293, FR-306.
Default NIL (opt-in). Override: CLCC_WASM_THREADS=1|0")

(defparameter *wasm-multiple-memories-enabled* nil
  "Feature gate for Wasm Multiple Memories proposal.
FR-208. Default NIL. Override: CLCC_WASM_MULTIPLE_MEMORIES=1|0")

(defparameter *wasm-shared-everything-enabled* nil
  "Feature gate for Wasm Shared Everything Threads (shared GC types).
FR-224, FR-273. Requires *wasm-threads-enabled*. Override: CLCC_WASM_SHARED_EVERYTHING=1|0")

(defparameter *wasm-memory64-enabled* nil
  "Feature gate for Wasm Memory64 proposal (64-bit address space).
FR-213. Default NIL. Override: CLCC_WASM_MEMORY64=1|0")

(defparameter *wasm-branch-hints-enabled* nil
  "Feature gate for Wasm Branch Hinting proposal.
FR-216. Override: CLCC_WASM_BRANCH_HINTS=1|0")

(defparameter *wasm-js-promise-enabled* nil
  "Feature gate for Wasm JS Promise Integration.
FR-217. Override: CLCC_WASM_JS_PROMISE=1|0")

(defparameter *wasm-string-builtins-enabled* nil
  "Feature gate for Wasm String Builtins proposal.
FR-218, FR-295. Override: CLCC_WASM_STRING_BUILTINS=1|0")

(defparameter *wasm-dwarf-enabled* nil
  "Feature gate for DWARF debug info emission.
FR-222. Override: CLCC_WASM_DWARF=1|0")

(defparameter *wasm-wide-arithmetic-enabled* nil
  "Feature gate for Wasm Wide Arithmetic (128-bit operations).
FR-238. Override: CLCC_WASM_WIDE_ARITH=1|0")

(defparameter *wasm-custom-page-sizes-enabled* nil
  "Feature gate for Wasm Custom Page Sizes proposal.
FR-239. Override: CLCC_WASM_CUSTOM_PAGE_SIZES=1|0")

(defparameter *wasm-gc-packed-fields-enabled* t
  "Feature gate for Wasm GC packed fields (i8/i16).
FR-283.")

(defparameter *wasm-gc-bulk-array-ops-enabled* t
  "Feature gate for Wasm GC bulk array operations (array.fill/init_data/init_elem).
FR-284, FR-281, FR-249.")

(defparameter *wasm-gc-recursive-types-enabled* t
  "Feature gate for Wasm GC recursive type groups.
FR-254.")

(defparameter *wasm-gc-null-safety-enabled* t
  "Feature gate for Wasm GC null safety (br_on_null/ref.as_non_null).
FR-270.")

(defparameter *wasm-multi-value-enabled* t
  "Feature gate for Wasm Multi-value returns.
FR-235.")

(defparameter *wasm-bigint-enabled* nil
  "Feature gate for JS BigInt ↔ i64 integration.
FR-236. Default NIL for backward compatibility. Override: CLCC_WASM_BIGINT=1|0")

(defparameter *wasm-bulk-table-enabled* t
  "Feature gate for Wasm Bulk Table Operations (table.init/copy/fill).
FR-237, FR-294.")

(defparameter *wasm-extern-to-any-enabled* t
  "Feature gate for JS externref ↔ anyref conversion.
FR-226, FR-286.")

(defparameter *wasm-ref-eq-enabled* t
  "Feature gate for Wasm GC ref.eq identity comparison.
FR-285.")

(defparameter *wasm-typed-select-enabled* t
  "Feature gate for Wasm typed select (reference types).
FR-279.")

(defparameter *wasm-func-bind-enabled* nil
  "Feature gate for Wasm func.bind (partial application).
FR-290.")

(defparameter *wasm-element-init-enabled* t
  "Feature gate for Wasm element segment array initialization.
FR-281, FR-294.")

(defparameter *wasm-import-maps-enabled* nil
  "Feature gate for Wasm Import Maps.
FR-276.")

(defparameter *wasm-pgo-enabled* nil
  "Feature gate for Profile-Guided Optimization.
FR-220.")

(defparameter *wasm-tiered-compilation-enabled* nil
  "Feature gate for Tiered Compilation hints (Liftoff/TurboFan).
FR-259.")
