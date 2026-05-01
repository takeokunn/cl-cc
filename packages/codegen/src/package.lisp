;;;; packages/codegen/src/package.lisp — feature package for cl-cc/codegen
;;;;
;;;; Phase 6: Strict-packaging migration. Files in packages/codegen/src/
;;;; previously straddled :cl-cc/compile (codegen-* + builtin-registry-*)
;;;; and :cl-cc/emit (x86-64-*, aarch64-*, wasm-*). They are now unified
;;;; into :cl-cc/codegen.
;;;;
;;;; To preserve test references like `cl-cc/compile::%foo` and
;;;; `cl-cc/emit::live-interval`, the legacy :cl-cc/compile and :cl-cc/emit
;;;; packages :use :cl-cc/codegen so symbols are inherited. CL `find-symbol`
;;;; semantics return inherited symbols, so qualified `pkg::sym` references
;;;; transparently resolve to the codegen home symbol.
;;;;
;;;; :use list combines everything codegen source files reference:
;;;;   - cl-cc/bootstrap, cl-cc/ast, cl-cc/parse, cl-cc/prolog: AST + parsing
;;;;   - cl-cc/type, cl-cc/optimize: type system, optimizer
;;;;   - cl-cc/vm, cl-cc/mir, cl-cc/target, cl-cc/regalloc: instruction set
;;;;   - cl-cc/cps, cl-cc/expand: compilation pipeline glue

(defpackage :cl-cc/codegen
  (:use :cl
        :cl-cc/bootstrap
        :cl-cc/ast
        :cl-cc/prolog
        :cl-cc/parse
        :cl-cc/optimize
        :cl-cc/vm
        :cl-cc/mir
        :cl-cc/expand
        :cl-cc/cps)
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
   ;; ── Re-exported regalloc symbols (originally in :cl-cc/emit) ──
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
   #:*current-regalloc*

   ;; ── x86-64-regs.lisp / x86-64.lisp / aarch64.lisp (was :cl-cc/emit) ──
   #:vm-reg-to-x86-with-alloc
   #:*phys-reg-to-x86-code*
   #:*phys-reg-to-asm-string*
   #:emit-instruction
   #:x86-64-target #:target-spill-base-reg
   #:aarch64-target
   #:x86-64-red-zone-spill-p

   ;; ── pipeline native entry points ──
   #:target-regalloc
   #:compile-to-native
   #:compile-file-to-native
   #:compile-to-x86-64-bytes
   #:compile-to-aarch64-bytes

   ;; ── wasm backend ──
   #:compile-to-wasm-wat

   ;; ── context.lisp — compiler context class + helpers (was :cl-cc/compile) ──
   #:compiler-context
   #:ctx-instructions #:ctx-next-register #:ctx-next-label
   #:ctx-env #:ctx-type-env #:ctx-safety
   #:ctx-block-env #:ctx-tagbody-env
   #:ctx-global-functions #:ctx-global-variables
   #:ctx-global-classes #:ctx-global-generics #:ctx-global-generic-params
   #:ctx-current-function-name #:ctx-current-function-label
   #:ctx-current-function-params #:ctx-current-function-simple-p
   #:ctx-top-level-p #:ctx-boxed-vars
   #:ctx-noescape-cons-bindings #:ctx-noescape-array-bindings
   #:ctx-noescape-instance-bindings #:ctx-noescape-closure-bindings
   #:ctx-tail-position
   #:*builtin-special-variables* #:*repl-global-variables*
   #:*repl-label-counter* #:*repl-capture-label-counter*
   #:*labels-boxed-fns* #:*compiling-typed-fn*
   #:make-register #:make-label #:emit #:lookup-var
   #:*cps-compile-unsupported-ast-types*
   #:*cps-native-compile-unsupported-ast-types*
   #:*enable-cps-vm-primary-path*
   #:*compile-expression-cps-recursion-guard*
   #:%cps-compile-safe-ast-p
   #:%cps-vm-compile-safe-ast-p
   #:%cps-native-compile-safe-ast-p
   #:%cps-identity-entry-form

   ;; ── codegen-fold-optimize.lisp ──
   #:optimize-ast

   ;; ── codegen-locals.lisp ──
   #:target-instance #:type-check-ast
   #:emit-assembly

   ;; ── codegen-core.lisp ──
   #:compile-ast

   ;; ── codegen.lisp — top-level + compilation-result ──
   #:compile-toplevel-forms
   #:compilation-result
   #:make-compilation-result
   #:compilation-result-program
   #:compilation-result-assembly
   #:compilation-result-globals
   #:compilation-result-type
   #:compilation-result-type-env
   #:compilation-result-cps
   #:compilation-result-ast
   #:compilation-result-vm-instructions
   #:compilation-result-optimized-instructions

   ;; ── pipeline.lisp — entry points (defined in pipeline package, but
   ;;     symbols pre-interned here for legacy compat) ──
   #:compile-expression
   #:compile-string
   #:run-string
   #:run-string-typed
   #:run-string-repl
   #:reset-repl-state
   #:*repl-vm-state*
   #:our-load))
