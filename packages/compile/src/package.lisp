;;;; packages/compile/src/package.lisp --- cl-cc/compile package definition
;;;;
;;;; Compilation engine: AST→VM instruction transformation.
;;;; context, builtin-registry, codegen-core, codegen-functions, etc.
;;;; Depends on :cl-cc/codegen for machine code emission primitives.

(defpackage :cl-cc/compile
  (:use :cl
        :cl-cc/bootstrap
        :cl-cc/ast
        :cl-cc/prolog
        :cl-cc/parse
        :cl-cc/optimize
        :cl-cc/vm
        :cl-cc/expand
        :cl-cc/cps
        :cl-cc/codegen)
  (:import-from :cl-cc/type
                 #:parse-type-specifier
                 #:type-env-empty
                 #:type-env-lookup
                 #:type-env-extend
                 #:type-env-extend*
                 #:type-to-scheme
                 #:instantiate
                 #:is-subtype-p
                 #:narrow-union-type
                 #:extract-type-guard
                 #:type-union
                 #:type-to-string
                 #:type-mismatch-error
                 #:type-mismatch-error-expected
                 #:type-mismatch-error-actual
                 #:type-inference-error
                 #:type-unknown-p
                 #:type-equal-p
                 #:check
                 #:reset-type-vars!
                 #:infer
                 #:zonk)

  (:import-from :cl-cc/target
                #:find-target)
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
  (:import-from :cl-cc/cps
                #:cps-transform
                #:cps-transform*
                #:cps-transform-ast
                #:cps-transform-ast*
                #:cps-transform-sequence
                #:cps-transform-eval)
  (:export
   ;; ── context.lisp ──
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

   ;; ── codegen.lisp ──
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

   ;; ── CPS transformation re-exports ──
   #:cps-transform
   #:cps-transform*
   #:cps-transform-ast
   #:cps-transform-ast*
   #:cps-transform-sequence
   #:cps-transform-eval

   ;; ── pipeline entry points (pre-interned here for legacy compat) ──
   #:compile-expression
   #:compile-string
   #:run-string
   #:run-string-typed
   #:run-string-repl
   #:reset-repl-state
   #:*repl-vm-state*
   #:our-load))
