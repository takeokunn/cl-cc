(cl:in-package :cl-cc/test)

;;; Compiler and parser imports for the test framework package.
;;; Extracted from the monolithic package.lisp to keep package definition
;;; data-oriented and readable.

(import '(cl-cc/compile:compile-string
          cl-cc/compile:run-string
          cl-cc/compile:run-string-repl
          cl-cc/compile:reset-repl-state
          cl-cc/compile:run-string-typed
          cl-cc/compile:compile-expression
          cl-cc/cps:cps-transform
          cl-cc/cps:cps-transform-ast
          cl-cc/cps:cps-transform-ast*
          cl-cc/cps:cps-transform-eval
          cl-cc/compile:compilation-result
          cl-cc/compile:make-compilation-result
          cl-cc/compile:compilation-result-program
          cl-cc/compile:compilation-result-assembly
          cl-cc/compile:compilation-result-globals
          cl-cc/compile:compilation-result-type
          cl-cc/compile:compilation-result-cps))

(import '(cl-cc:our-eval))

(import '(cl-cc/parse:ast-to-sexp
          cl-cc/parse:lower-sexp-to-ast
          cl-cc/parse:parse-slot-spec
          cl-cc/parse:slot-def-to-sexp
          cl-cc/parse:*grammar-rules*
          cl-cc/parse:def-grammar-rule
          cl-cc/parse:query-grammar
          cl-cc/parse:clear-grammar-rules
          cl-cc/parse:parse-combinator
          cl-cc/parse:parse-ok-p
          cl-cc/parse:parse-with-grammar
          cl-cc/parse:tokenize-php-source
          cl-cc/parse:parse-php-source))
