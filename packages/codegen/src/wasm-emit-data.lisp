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
