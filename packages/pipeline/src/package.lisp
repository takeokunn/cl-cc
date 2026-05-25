;;;; packages/pipeline/src/package.lisp — feature package for cl-cc/pipeline
;;;;
;;;; Pipeline: public compilation API (compile-expression, compile-string,
;;;; run-string, etc.) and stdlib cache management.

(defpackage :cl-cc/pipeline
  (:use :cl
        :cl-cc/bootstrap
        :cl-cc/ast
        :cl-cc/prolog
        :cl-cc/parse
        :cl-cc/php
        :cl-cc/optimize
        :cl-cc/emit
        :cl-cc/expand
         :cl-cc/compile
         :cl-cc/vm
         :cl-cc/mir
         :cl-cc/stdlib)
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
  (:export
   ;; Public
   #:compile-string-with-stdlib
   #:parse-source-for-language
   #:get-stdlib-forms
   #:warm-stdlib-cache  ; defined in :cl-cc/selfhost; pre-interned here so
                        ; pipeline-runtime can call it as bare symbol
    #:pipeline-opts
    #:*compilation-tier*
    #:normalize-compilation-tier
    #:*incremental-cache-directory*
    #:pipeline-source-hash
    #:pipeline-incremental-current-p
    #:pipeline-record-incremental-state
    #:write-perf-map-entry
    #:write-perf-map-for-native-code
    #:perf-map-line-valid-p
    #:perf-map-path
    #:*perf-map-stream*
    #:*perf-map-output-dir*
    #:*perf-map-base-address*
    #:*lto-enabled*
    #:make-lto-module
    #:lto-serialize-module
    #:serialize-lto-ir
    #:lto-deserialize-module
    #:deserialize-lto-ir
    #:lto-merge-modules
    #:lto-optimize-modules
    #:thin-lto-build-summary
    #:generate-thin-lto-summaries
    #:thin-lto-optimize-modules
     #:prepare-incremental-compilation
     #:commit-incremental-compilation
     #:incremental-state-dirty-p
     #:incremental-state-reason
     #:read-autofdo-profile
     #:load-autofdo-profile-data
     #:read-perf-data-binary
     #:autofdo-hot-cold-layout-decisions
     #:autofdo-apply-layout-decisions
     #:read-bolt-profile
     #:pipeline-bolt-optimize-program
     #:maybe-pipeline-bolt-optimize-program
    #:make-compilation-arena
    #:arena-alloc
    #:with-compilation-arena
    #:compile-files-to-native-parallel
    #:compile-files-parallel
    #:hot-reload-call
    #:hot-reload-swap
    #:make-hot-reload-entry
   ;; Stdlib cache state
    #:*stdlib-vm-snapshot*
    #:*stdlib-accessor-slot-map*
    #:*stdlib-defstruct-read-only-accessor-map*
    #:*stdlib-defstruct-slot-registry*
    #:*stdlib-defstruct-type-registry*
    #:*stdlib-setf-compound-place-handlers*
     #:*stdlib-expanded-cache*
    #:*stdlib-expanded-cache-source*
    #:*stdlib-expanded-cache-eval-fn*
    #:*stdlib-cache-directory*
    #:*stdlib-symbol-index*
    #:stdlib-symbol-defined-p
   ;; Native cache parameters
    #:*compile-cache-root*
    #:*native-command-timeout-seconds*
    #:*use-mir-pipeline*
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
     #:pipeline-reorder-functions
    #:%maybe-compile-native-via-cps
   #:%make-native-opts
   #:%non-package-top-level-forms
   #:%run-short-native-command
   #:%copy-file-bytes
   #:*definition-and-declaration-form-heads*
   #:*cps-host-eval-unsafe-ast-types*))
