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

   ;; Async / generator
   #:%js-yield
   #:%js-await
   #:%js-make-generator
   #:%js-generator-next

   ;; Exception handling
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

   ;; Console helpers
   #:%js-console-log
   #:%js-console-error
   #:%js-console-warn

   ;; Truthiness / nullish helpers
   #:%js-truthy
   #:%js-not-nullish

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

   ;; String built-ins
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
