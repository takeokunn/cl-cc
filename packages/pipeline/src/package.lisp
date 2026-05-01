;;;; packages/pipeline/src/package.lisp — feature package for cl-cc/pipeline
;;;;
;;;; Phase 4 strict-packaging: pipeline files moved from
;;;; packages/pipeline/src/. Pipeline owns the public compilation
;;;; API (compile-expression, compile-string, run-string, etc.).
;;;;
;;;; The umbrella :cl-cc package :uses :cl-cc/pipeline so downstream
;;;; consumers can continue to reference cl-cc:compile-expression unqualified.
;;;;
;;;; Symbols already pre-interned/exported elsewhere (compile-expression,
;;;; compile-string, run-string in :cl-cc/compile; compile-to-native in
;;;; :cl-cc/emit) are shared via :use — defining them in pipeline files
;;;; updates the same symbol object.
;;;;
;;;; %-prefixed helpers are exported so cl-cc/selfhost and cl-cc/repl
;;;; (and tests via cl-cc::%foo inheritance) can reference them. The
;;;; one exception is %top-level-in-package-form-p, which is also
;;;; defined in cl-cc/compile (codegen.lisp); we keep both copies
;;;; internal to avoid name-conflicts on use-package.

(defpackage :cl-cc/pipeline
  (:use :cl
        :cl-cc/bootstrap
        :cl-cc/ast
        :cl-cc/prolog
        :cl-cc/parse
        :cl-cc/optimize
        :cl-cc/emit
        :cl-cc/expand
        :cl-cc/compile
        :cl-cc/vm
        :cl-cc/stdlib)
  (:export
   ;; Public
   #:compile-string-with-stdlib
   #:parse-source-for-language
   #:get-stdlib-forms
   #:warm-stdlib-cache  ; defined in :cl-cc/selfhost; pre-interned here so
                        ; pipeline-runtime can call it as bare symbol
   #:pipeline-opts
   ;; Stdlib cache state
   #:*stdlib-vm-snapshot*
   #:*stdlib-accessor-slot-map*
   #:*stdlib-defstruct-slot-registry*
   #:*stdlib-expanded-cache*
   #:*stdlib-expanded-cache-source*
   #:*stdlib-expanded-cache-eval-fn*
   ;; Native cache parameters
   #:*compile-cache-root*
   #:*native-command-timeout-seconds*
   ;; Internal helpers exported so cross-package callers (selfhost/repl)
   ;; and tests can resolve them as bare or cl-cc:: symbols.
   #:%prepare-ast
   #:%type-check-safe
   #:%definition-form-p
   #:%maybe-cps-toplevel-forms
   #:%maybe-compile-expression-via-cps
   #:%make-direct-compilation-result
   ;; pipeline-opts defstruct constructor (referenced by pipeline-tests via cl-cc::%make-pipeline-opts)
   #:%make-pipeline-opts
   ;; Typeclass macros — registered in pipeline-native-typeclass.lisp via
   ;; (register-macro 'deftype-class ...). Exporting here ensures the symbol
   ;; is visible in :cl-cc so tests using cl-cc::deftype-class resolve to the
   ;; same symbol that owns the macro registration.
   #:deftype-class
   #:deftype-instance
   #:%compile-string-forms
   #:%opts->compile-kwargs
   #:%opts->optimize-kwargs
   #:%snapshot-macro-env-table
   #:%restore-macro-env-table
   #:%build-stdlib-expanded-cache
   #:%copy-snapshot-ht
   #:%write-native-binary
   #:%compile-cache-key
   #:%compile-cache-path
   #:%compile-native-expression
   #:%compile-native-source
   #:%compile-native-string
   #:%compile-native-toplevel-forms
   #:%compile-native-lisp-forms
   #:%maybe-compile-native-via-cps
   #:%make-native-opts
   #:%non-package-top-level-forms
   #:%run-short-native-command
   #:%copy-file-bytes
   #:*definition-and-declaration-form-heads*
   #:*cps-host-eval-unsafe-ast-types*))
