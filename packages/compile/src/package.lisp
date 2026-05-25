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
  (:shadowing-import-from :cl-cc/expand
    #:define-syntax)
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
    #:room #:apropos #:apropos-list
    #:*read-base* #:*print-right-margin* #:*print-lines*
    #:*print-case* #:*print-escape* #:*print-gensym* #:*print-array*
    #:*trace-output*
    #:sequence #:elt #:length #:subseq
    #:stream-external-format
    #:break #:describe #:inspect #:ed #:dribble
    #:invoke-debugger #:trace #:untrace #:step
    #:*read-suppress* #:*read-default-float-format* #:*read-eval*
    #:*features* #:*modules*
    #:method-qualifiers #:compute-applicable-methods
    #:find-method #:add-method #:remove-method #:ensure-generic-function
    #:open #:*load-pathname* #:*load-truename* #:*compile-file-pathname*
    #:*compile-file-truename* #:*compile-print* #:*compile-verbose*
    #:*load-print* #:*load-verbose*
    #:quit #:exit
    #:getenv #:setenv
    #:*command-line-args* #:*default-external-format*
    #:read-preserving-whitespace #:read-delimited-list
    #:set-syntax-from-char
)
  (:import-from :cl-cc/type
                 #:parse-type-specifier
                 #:type-env-empty
                 #:type-env-lookup
                 #:type-env-extend
                 #:type-env-extend*
                  #:type-any
                  #:register-class-method-type
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
                  #:looks-like-type-specifier-p
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
   #:ctx-global-functions #:ctx-global-variables #:ctx-function-conventions
    #:ctx-global-classes #:ctx-global-generics #:ctx-global-generic-params
    #:ctx-current-function-name #:ctx-current-function-label
    #:ctx-current-function-params #:ctx-current-function-simple-p
    #:ctx-pending-inline-policy
     #:ctx-top-level-p #:ctx-boxed-vars
   #:ctx-noescape-cons-bindings #:ctx-noescape-array-bindings
        #:ctx-noescape-instance-bindings #:ctx-noescape-closure-bindings
        #:ctx-hash-table-test-bindings
     #:ctx-tail-position
     #:ctx-target
      #:ctx-diagnostics
    #:compilation-environment
    #:make-compilation-environment
    #:compilation-environment-p
    #:copy-compilation-environment
    #:compilation-environment-parent
    #:compilation-environment-variables
    #:compilation-environment-functions
    #:compilation-environment-declarations
    #:augment-environment
    #:variable-information
    #:function-information
    #:declaration-information
    #:define-declaration
    #:parse-macro
    #:compilation-environment-from-context
    #:*builtin-special-variables* #:*repl-global-variables*
   #:*repl-label-counter* #:*repl-capture-label-counter*
  #:*labels-boxed-fns* #:*local-tail-jump-fns* #:*compiling-typed-fn*
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
   #:*string-literal-pool*
    #:%emit-constant

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
     #:compilation-result-pgo-counter-plan
     #:compilation-result-errors
     #:compilation-result-warnings

   ;; ── CPS transformation re-exports ──
   #:cps-transform
   #:cps-transform*
   #:cps-transform-ast
   #:cps-transform-ast*
   #:cps-transform-sequence
   #:cps-transform-eval

   ;; ── pipeline entry points (canonical public API symbols) ──
   #:compile-expression
   #:compile-string
   #:run-string
   #:run-string-typed
   #:run-string-repl
   #:reset-repl-state
   #:*repl-vm-state*
   #:our-load))
