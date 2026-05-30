;;;; packages/codegen/src/wasm-feature-params.lisp — trailing feature-gate defparameters
;;;
;;; Supplementary feature-gate variables for WASM backends (FR-213 through FR-325).
;;; These complement the core feature flags in wasm-features.lisp with additional
;;; opt-in/opt-out knobs for newer or less common proposals.

(in-package :cl-cc/codegen)

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-213: Memory64 — 64-bit address space declaration
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-memory64-enabled* nil
  "Feature gate for WASM Memory64 (FR-213). When T, emit (memory i64 ...) instead
of (memory i32 ...) and use i64.load/i64.store addressing.")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-208: Multiple Memories — separate memory for heap/stack/strings
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-multiple-memories* nil
  "Feature gate for WASM Multiple Memories (FR-208).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-206: Component Model — WIT interface generation
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-component-model* nil
  "Feature gate for WASM Component Model / WIT bindings (FR-206).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-207: WASI 0.2 — capability-based system interface
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-wasi-02-enabled* nil
  "Feature gate for WASI 0.2 (Preview 2) system interface (FR-207).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-230: ESM Integration — import/export native .wasm modules
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-esm-integration* nil
  "Feature gate for WASM ESM Integration (FR-230).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-216: Branch Hinting — custom section for branch prediction
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-branch-hinting* nil
  "When T, emit @metadata.code.branch_hint custom section (FR-216).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-215: Extended Constant Expressions — complex global initializers
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-extended-const-exprs* nil
  "When T, use global.get/i32.add etc. in global initializer expressions (FR-215).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-214: Relaxed SIMD — fused multiply-add, relaxed min/max
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-relaxed-simd-enabled* nil
  "Feature gate for WASM Relaxed SIMD proposal (FR-214).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-250: Multibyte Array Access — array.load2_u/load4_u
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-multibyte-array-access* nil
  "Feature gate for WASM Multibyte Array Access (FR-250).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-294: Passive Element/Data Segments — lazy segment loading
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-passive-segments* nil
  "When T, use passive data/element segments for lazy loading (FR-294).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-280: Initialization order — __wasm_call_ctors convention
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-call-ctors* t
  "When T, emit __wasm_call_ctors for ordered module initialization (FR-280).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-322: WASM binary tool integration — wat2wasm/wasm-objdump
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-binary-tools-path* nil
  "Path to WABT (WebAssembly Binary Toolkit) tools for wasm2wat/wasm-objdump (FR-322).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-297: COOP/COEP headers — SharedArrayBuffer prerequisites
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-coop-coep-headers* nil
  "When T, generate COOP/COEP headers for SharedArrayBuffer (FR-297).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-302: ServiceWorker + WASM — offline PWA support
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-service-worker* nil
  "When T, generate Service Worker registration for PWA (FR-302).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-277: CL ABI / Symbol Name Mangling — cross-language FFI
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-abi-mangling* nil
  "When T, mangle CL symbol names for C/Rust/JS FFI compatibility (FR-277).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-276: WASM Import Maps — external module name resolution
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-import-maps* nil
  "When T, generate import map support for module URL resolution (FR-276).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-258: WASM Profiles — feature profile declaration
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-profile* nil
  "WASM feature profile for compilation target: :minimal, :gc, :threads, :full (FR-258).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-259: Tiered Compilation Hints — Liftoff/TurboFan optimization
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-tiered-compilation-hints* t
  "When T, emit optimization hints for V8 Liftoff/TurboFan tiering (FR-259).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-260: LTO — Link-Time Optimization cross-module inlining
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-lto-enabled* nil
  "When T, enable Binaryen LTO for cross-module inlining (FR-260).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-264: Lazy Function Bodies — deferred function compilation
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-lazy-function-bodies* nil
  "When T, use stub functions with lazy compilation (FR-264).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-275: Worker postMessage Module Transfer — compiled module sharing
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-worker-module-transfer* nil
  "When T, transfer compiled WASM modules via Worker postMessage (FR-275).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-288: Incremental REPL Compilation — on-the-fly WASM module generation
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-incremental-repl* nil
  "When T, support incremental WASM compilation in REPL (FR-288).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-317: Hot Code Reloading — runtime function patching via table.set
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-hot-reload* nil
  "When T, support runtime function patching for hot reload (FR-317).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-312: Runtime Feature Detection — WebAssembly.featureDetect
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-runtime-feature-detect* nil
  "When T, generate runtime WASM feature detection code (FR-312).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-311: wasm-c-api — Native C embedding API
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-c-api-enabled* nil
  "When T, generate wasm-c-api compatible host embedding code (FR-311).")

;;; ─────────────────────────────────────────────────────────────────────────────
;;; FR-325: SIMD NaN semantics — strict NaN flag
;;; ─────────────────────────────────────────────────────────────────────────────
(defparameter *wasm-simd-strict-nan* nil
  "When T, enforce IEEE 754 NaN semantics in SIMD operations (FR-325).")
