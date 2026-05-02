;;;; packages/parse/src/package.lisp - CL-CC Parse Package
;;;;
;;;; Phase 3 of the package-by-feature monorepo migration. This package owns
;;;; the CST/lexer/parser/grammar/PHP frontend and diagnostic infrastructure.
;;;; The facade :cl-cc package uses :cl-cc/parse so compiler modules continue
;;;; to see parse-* / cst-* / lexer-* / diagnostic-* symbols unqualified.
;;;;
;;;; Symbols pre-interned by :cl-cc/prolog (MAKE-CST-TOKEN, LEXER-TOKEN-P,
;;;; LEXER-TOKEN-TYPE, LEXER-TOKEN-VALUE) come from :cl-cc/bootstrap and are
;;;; intentionally excluded from exports to avoid NAME-CONFLICT errors. They
;;;; remain exported from :cl-cc via package-exports-1.lisp.
;;;;
;;;; DCG symbols (def-dcg-rule, phrase, phrase-rest, phrase-all, dcg-fresh-var,
;;;; dcg-reset-counter) are already exported by :cl-cc/prolog and excluded here.

(defpackage :cl-cc/parse
  (:use :cl :cl-cc/ast :cl-cc/bootstrap)
  (:export
   ;; CST node types
   #:cst-node #:cst-node-p #:cst-node-kind
   #:cst-node-start-byte #:cst-node-end-byte #:cst-node-source-file
   #:cst-token #:cst-token-p #:cst-token-value
   ;; NOTE: make-cst-token excluded (prolog pre-intern conflict)
   #:cst-interior #:cst-interior-p #:cst-interior-children #:make-cst-interior
   #:cst-error #:cst-error-p
   #:cst-error-message #:make-cst-error
   #:cst-child #:cst-children #:cst-walk #:cst-collect-errors
   #:cst-to-sexp #:sexp-to-cst #:sexp-head-to-kind
   #:cst-location-string
   #:cst-trivia #:make-cst-trivia
   #:cst-equal-p
   ;; Lexer (token struct accessors excluded: prolog pre-intern conflict)
   #:lexer-token
   #:lexer-token-start-byte #:lexer-token-end-byte
   #:lexer-token-line #:lexer-token-column #:lexer-token-trivia
   ;; NOTE: lexer-token-p, lexer-token-type, lexer-token-value excluded
   #:make-lexer #:lex-all #:lexer-read-token
   ;; CL parser
   #:parse-cl-source #:parse-cl-source-single
   ;; Diagnostics
   #:diagnostic #:make-diagnostic
   #:diagnostic-severity #:diagnostic-message #:diagnostic-span
   #:make-parse-error #:make-parse-warning
   #:parse-failure
   #:byte-offset-to-line-col #:source-line-at
   #:format-diagnostic #:format-diagnostic-list
   ;; Incremental parsing
   #:edit-operation #:make-edit-operation
   #:parse-incremental
   #:cache-lookup #:cache-store #:invalidate-parse-cache
   ;; CST lowering
   #:lower-cst-to-ast #:lower-cst-list-to-ast
   #:parse-and-lower #:parse-and-lower-one
   #:parse-source #:parse-all-forms
   ;; Lambda-list / slot-spec parsing
   #:parse-compiler-lambda-list #:lambda-list-has-extended-p
   #:parse-slot-spec
   ;; Roundtrip
   #:lower-sexp-to-ast
   ;; Grammar combinator engine
   #:*grammar-rules* #:def-grammar-rule #:query-grammar #:clear-grammar-rules
   #:parse-combinator #:parse-ok-p #:parse-with-grammar
   ;; Multi-language dispatch
   #:parse-source-for-language))
