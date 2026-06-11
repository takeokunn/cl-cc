(defpackage :cl-cc/javascript
  (:use :cl :cl-cc/ast :cl-cc/bootstrap :cl-cc/parse)
  (:export
   ;; Entry points
   #:tokenize-js-source
   #:parse-js-source
   #:parse-js-module
   #:js-program-forms
   #:%js-make-console

   ;; JS-specific unary / binary operators
   #:%js-typeof
   #:%js-instanceof
   #:%js-void
   #:%js-delete
   #:%js-in
   #:%js-loose-eq
   #:%js-strict-eq
   #:%js-nullish-coalesce
   #:%js-optional-chain
   #:%js-optional-call
   #:%js-optional-method-call
   #:%js-spread

   ;; Destructuring
   #:%js-destructure-array
   #:%js-destructure-object

   ;; String / template helpers
   #:%js-template-string
   #:%js-to-string
   #:%js-concat

   ;; Property access / object construction
   #:%js-get-prop
   #:%js-set-prop
   #:%js-make-object
   #:%js-make-array

   ;; Private class fields
   #:%js-class-private-field-get
   #:%js-class-private-field-set
   #:%js-has-private-field

   ;; Promise type
   #:js-promise-p
   #:js-promise-value
   #:js-promise-rejected-p

   ;; Async / generator
   #:%js-yield
   #:%js-yield-from
   #:%js-await
   #:%js-make-async
   #:%js-make-async-generator
   #:%js-make-generator
   #:%js-generator-next
   #:%js-wrap-generator-body
   #:*%js-yield-collector*
   ;; Temporal API (ES2026)
   #:*js-temporal-global*
   #:%js-temporal-instant
   #:%js-temporal-plain-date
   #:%js-temporal-plain-time
   #:%js-temporal-plain-datetime
   #:%js-temporal-zoned-datetime
   #:%js-temporal-duration
   #:%js-temporal-plain-year-month
   #:%js-temporal-plain-month-day
   ;; ES2025 TypedArray
   #:%js-uint8-from-hex
   #:%js-uint8-from-base64
   #:%js-uint8-to-hex
   #:%js-uint8-to-base64

   ;; Exception handling
   #:js-exception
   #:js-exception-value
   #:%js-throw
   #:%js-try-catch-finally

   ;; Iteration protocols
   #:%js-for-in
   #:%js-for-of
   #:%js-for-await-of

   ;; Module system
   #:%js-new
   #:%js-import
   #:%js-export
   #:%js-debugger
   #:%js-import-meta
   #:%js-new-target

   ;; Operator helpers (bitwise, shift, unary, increment)
   #:%js-bitwise-not
   #:%js-bitwise-or
   #:%js-bitwise-and
   #:%js-bitwise-xor
   #:%js-shift-left
   #:%js-shift-right
   #:%js-unsigned-shift-right
   #:%js-unary-plus
   #:%js-postfix-inc
   #:%js-postfix-dec
   #:%js-prefix-inc
   #:%js-prefix-dec

   ;; Class / accessor helpers
   #:%js-accessor
   #:%js-make-regex
   #:%js-field-init
   #:%js-assign-pattern

   ;; Resource management
   #:%js-using-register

   ;; Console helpers
   #:%js-console-log
   #:%js-console-error
   #:%js-console-warn

   ;; Truthiness / nullish helpers
   #:%js-truthy
   #:%js-not-nullish

   ;; Global number-parsing helpers (referenced in js-program-forms)
   #:%js-parse-int
   #:%js-parse-float
   #:%js-is-nan
   #:%js-is-finite

   ;; Built-in dispatch table
   #:*js-builtin-map*

   ;; Math built-ins
   #:%js-math-abs
   #:%js-math-ceil
   #:%js-math-floor
   #:%js-math-round
   #:%js-math-max
   #:%js-math-min
   #:%js-math-pow
   #:%js-math-sqrt
   #:%js-math-log
   #:%js-math-log2
   #:%js-math-log10
   #:%js-math-exp
   #:%js-math-sin
   #:%js-math-cos
   #:%js-math-tan
   #:%js-math-asin
   #:%js-math-acos
   #:%js-math-atan
   #:%js-math-atan2
   #:%js-math-hypot
   #:%js-math-trunc
   #:%js-math-sign
   #:%js-math-clz32
   #:%js-math-fround
   #:%js-math-imul
   #:%js-math-random

   ;; Array built-ins
   #:%js-array-for-each
   #:%js-array-push
   #:%js-array-pop
   #:%js-array-shift
   #:%js-array-unshift
   #:%js-array-splice
   #:%js-array-slice
   #:%js-array-concat
   #:%js-array-join
   #:%js-array-reverse
   #:%js-array-sort
   #:%js-array-flat
   #:%js-array-flat-map
   #:%js-array-map
   #:%js-array-filter
   #:%js-array-reduce
   #:%js-array-reduce-right
   #:%js-array-find
   #:%js-array-find-index
   #:%js-array-every
   #:%js-array-some
   #:%js-array-includes
   #:%js-array-index-of
   #:%js-array-last-index-of
   #:%js-array-fill
   #:%js-array-copy-within
   #:%js-array-entries
   #:%js-array-keys
   #:%js-array-from
   #:%js-array-is-array

   ;; Object built-ins
   #:%js-object-keys
   #:%js-object-values
   #:%js-object-entries
   #:%js-object-assign
   #:%js-object-create
   #:%js-object-define-property
   #:%js-object-define-properties
   #:%js-object-get-prototype-of
   #:%js-object-set-prototype-of
   #:%js-object-get-own-property-descriptor
   #:%js-object-has-own
   #:%js-object-from-entries
   #:%js-object-is

   ;; String built-ins (ES2015+, ES2024)
   #:%js-string-substring
   #:%js-string-to-well-formed
   #:%js-string-is-well-formed
   #:%js-string-to-locale-lower-case
   #:%js-string-to-locale-upper-case
   #:%js-string-locale-compare
   #:%js-string-length
   #:%js-string-char-at
   #:%js-string-char-code-at
   #:%js-string-concat
   #:%js-string-includes
   #:%js-string-starts-with
   #:%js-string-ends-with
   #:%js-string-index-of
   #:%js-string-last-index-of
   #:%js-string-match
   #:%js-string-match-all
   #:%js-string-replace
   #:%js-string-replace-all
   #:%js-string-search
   #:%js-string-slice
   #:%js-string-split
   #:%js-string-trim
   #:%js-string-trim-start
   #:%js-string-trim-end
   #:%js-string-pad-start
   #:%js-string-pad-end
   #:%js-string-repeat
   #:%js-string-to-lower-case
   #:%js-string-to-upper-case
   #:%js-string-normalize
   #:%js-string-from-char-code
   #:%js-string-from-code-point
   #:%js-string-raw
   #:%js-string-at

   ;; Promise built-ins
   #:%js-promise-resolve
   #:%js-promise-reject
   #:%js-promise-all
   #:%js-promise-all-settled
   #:%js-promise-any
   #:%js-promise-race
   #:%js-promise-with-resolvers
   #:%js-promise-then
   #:%js-promise-finally

   ;; Set built-ins
   #:%js-set-add
   #:%js-set-delete
   #:%js-set-has
   #:%js-set-clear
   #:%js-set-size
   #:%js-set-entries
   #:%js-set-keys
   #:%js-set-for-each
   #:%js-set-union
   #:%js-set-intersection
   #:%js-set-difference
   #:%js-set-symmetric-difference
   #:%js-set-is-subset-of
   #:%js-set-is-disjoint-from
   #:%js-set-is-superset-of

   ;; Map built-ins (ES2015+)
   #:%js-map-p
   #:%js-make-map
   #:%js-map-set
   #:%js-map-get
   #:%js-map-has
   #:%js-map-delete
   #:%js-map-clear
   #:%js-map-size
   #:%js-map-keys
   #:%js-map-values
   #:%js-map-entries
   #:%js-map-for-each

   ;; WeakMap built-ins (ES2015+)
   #:%js-weak-map-p
   #:%js-make-weak-map
   #:%js-weak-map-set
   #:%js-weak-map-get
   #:%js-weak-map-has
   #:%js-weak-map-delete

   ;; WeakSet built-ins (ES2015+)
   #:%js-weak-set-p
   #:%js-make-weak-set
   #:%js-weak-set-add
   #:%js-weak-set-has
   #:%js-weak-set-delete

   ;; WeakRef / FinalizationRegistry (ES2021)
   #:%js-make-weak-ref
   #:%js-weak-ref-deref
   #:%js-make-finalization-registry
   #:%js-finreg-register
   #:%js-finreg-unregister

   ;; Symbol (ES2015+)
   #:%js-symbol-p
   #:%js-make-symbol
   #:%js-symbol-for
   #:%js-symbol-key-for
   #:%js-symbol-to-string
   #:%js-symbol-description
   #:%js-symbol-as-key
   #:*js-symbol-registry*
   #:*js-symbol-global*
   ;; TypedArray (ES2015+)
   #:%js-typed-array-p
   #:%js-make-typed-array
   #:%js-ta-get
   #:%js-ta-set
   #:%js-ta-to-array
   #:*js-typed-array-method-table*

   ;; RegExp (ES2015+ native engine)
   #:%js-regexp-p
   #:%js-make-regex
   #:%js-regex-exec
   #:%js-regex-test
   #:%js-string-match-regex
   #:%js-string-search-regex
   #:%js-string-replace-regex
   #:%js-string-replace-all-regex
   #:%js-string-split-regex

   ;; ES2023 Array methods
   #:%js-array-to-reversed
   #:%js-array-to-sorted
   #:%js-array-to-spliced
   #:%js-array-with
   #:%js-array-find-last
   #:%js-array-find-last-index
   #:%js-array-at
   #:%js-array-of

   ;; Date (ES2015+)
   #:%js-date-p
   #:%js-make-date
   #:%js-date-now
   #:%js-date-get-time
   #:%js-date-get-full-year
   #:%js-date-get-month
   #:%js-date-get-date
   #:%js-date-get-hours
   #:%js-date-get-minutes
   #:%js-date-get-seconds
   #:%js-date-get-day
   #:%js-date-to-iso-string
   #:%js-date-to-string
   #:*js-date-method-table*

   ;; Well-known symbols
   #:%js-symbol-iterator
   #:%js-symbol-to-primitive
   #:%js-symbol-to-string-tag
   #:%js-symbol-has-instance
   #:%js-symbol-species
   #:%js-symbol-async-iterator

   ;; Object extended built-ins
   #:%js-object-without-keys
   #:%js-object-group-by

   ;; Iterator helpers (stage-3 / ES2025)
   #:%js-iterator-map
   #:%js-iterator-filter
   #:%js-iterator-reduce
   #:%js-iterator-take
   #:%js-iterator-drop
   #:%js-iterator-flat-map
   #:%js-iterator-for-each
   #:%js-iterator-some
   #:%js-iterator-every
   #:%js-iterator-find
   #:%js-iterator-to-array))
