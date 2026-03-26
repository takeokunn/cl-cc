;;;; tests/pbt/package.lisp - Property-Based Testing Framework Package
;;;
;;; This package provides a property-based testing framework for CL-CC,
;;; inspired by QuickCheck and similar tools.

(defpackage :cl-cc/pbt
  (:use :cl)
  (:import-from :cl-cc/test
                #:cl-cc-suite
                #:defsuite
                #:in-suite
                #:deftest
                #:run-suite
                #:%fail-test
                #:test-failure
                #:*test-registry*
                #:*current-suite*
                #:*suite-registry*
                #:assert-true
                #:assert-false
                #:assert-=
                #:assert-eq
                #:assert-equal
                #:assert-null
                #:assert-type
                #:assert-signals)
  (:import-from :cl-cc
                ;; AST classes for generators
                :ast-node
                :ast-int
                :ast-var
                :ast-binop
                :ast-if
                :ast-progn
                :ast-print
                :ast-let
                :ast-lambda
                :ast-function
                :ast-flet
                :ast-labels
                :ast-block
                :ast-return-from
                :ast-tagbody
                :ast-go
                :ast-setq
                :ast-multiple-value-call
                :ast-multiple-value-prog1
                :ast-catch
                :ast-throw
                :ast-unwind-protect
                :ast-call
                :ast-quote
                :ast-the
                ;; AST constructors (defstruct BOA)
                :make-ast-int
                :make-ast-var
                :make-ast-binop
                :make-ast-if
                :make-ast-progn
                :make-ast-print
                :make-ast-let
                :make-ast-lambda
                :make-ast-function
                :make-ast-flet
                :make-ast-labels
                :make-ast-block
                :make-ast-return-from
                :make-ast-tagbody
                :make-ast-go
                :make-ast-setq
                :make-ast-multiple-value-call
                :make-ast-multiple-value-prog1
                :make-ast-call
                :make-ast-quote
                :make-ast-the
                :make-ast-catch
                :make-ast-throw
                :make-ast-unwind-protect
                ;; AST operations
                :ast-to-sexp
                :lower-sexp-to-ast
                ;; AST accessors
                :ast-int-value
                :ast-var-name
                :ast-binop-op
                :ast-binop-lhs
                :ast-binop-rhs
                :ast-if-cond
                :ast-if-then
                :ast-if-else
                :ast-progn-forms
                :ast-print-expr
                :ast-let-bindings
                :ast-let-body
                :ast-lambda-params
                :ast-lambda-body
                :ast-function-name
                :ast-flet-bindings
                :ast-flet-body
                :ast-labels-bindings
                :ast-labels-body
                :ast-block-name
                :ast-block-body
                :ast-return-from-name
                :ast-return-from-value
                :ast-tagbody-tags
                :ast-go-tag
                :ast-setq-var
                :ast-setq-value
                :ast-mv-call-func
                :ast-mv-call-args
                :ast-mv-prog1-first
                :ast-mv-prog1-forms
                :ast-catch-tag
                :ast-catch-body
                :ast-throw-tag
                :ast-throw-value
                :ast-unwind-protected
                :ast-unwind-cleanup
                :ast-call-func
                :ast-call-args
                :ast-quote-value
                :ast-the-type
                :ast-the-value
                ;; CPS
                :cps-transform
                ;; VM State and heap operations
                :vm-state
                :vm-heap-counter
                :vm-cons-cell
                :vm-closure-object
                :vm-closure-entry-label
                :vm-closure-params
                :vm-closure-captured-values
                ;; VM Instruction classes
                :vm-const
                :vm-move
                :vm-add
                :vm-sub
                :vm-mul
                :vm-cons
                :vm-car
                :vm-cdr
                :vm-rplaca
                :vm-rplacd
                :vm-make-closure
                :vm-closure-ref-idx
                ;; VM Instruction constructors (defstruct)
                :make-vm-add :make-vm-car :make-vm-cdr
                :make-vm-closure-ref-idx :make-vm-cons :make-vm-const
                :make-vm-make-closure :make-vm-move :make-vm-mul
                :make-vm-rplaca :make-vm-rplacd :make-vm-sub
                ;; VM Helpers
                :vm-reg-get
                :vm-reg-set
                :vm-heap-get
                :execute-instruction
                :instruction->sexp
                :sexp->instruction
                ;; VM Instruction accessors
                :vm-dst
                :vm-src
                :vm-lhs
                :vm-rhs
                :vm-value
                :vm-car-reg
                :vm-cdr-reg
                :vm-cons-reg
                :vm-val-reg
                :vm-closure-reg
                :vm-closure-index
                ;; Prolog
                :unify
                 :unify-failed-p
                :substitute-variables
                :logic-var-p)
  (:export
   ;; Core macros
   #:defproperty
   #:defgenerator
   #:for-all
   #:check

   ;; Configuration
   #:*test-count*
   #:*max-list-length*
   #:*max-string-length*
   #:*max-type-depth*
   #:*max-mach-o-sections*
   #:*size*
   #:*pbt-random-state*

   ;; Built-in generators
   #:gen-integer
   #:gen-boolean
   #:gen-symbol
   #:gen-list
   #:gen-string
   #:gen-ast-node
   #:gen-expr
   #:gen-float
   #:gen-character
   #:gen-cons
   #:gen-vector

   ;; Combinators
   #:gen-one-of
   #:gen-tuple
   #:gen-list-of
   #:gen-alist
   #:gen-map
   #:gen-bind
   #:gen-fmap
   #:gen-such-that
   #:gen-resize
   #:gen-scale

   ;; Shrinking
   #:shrink
   #:shrink-integer
   #:shrink-list

   ;; Test utilities
   #:run-property-tests
   #:report-failure

   ;; Generator protocol
   #:generator
   #:generate
   #:gen-fn
   #:shrink-value
   #:make-generator

   ;; Type Expression Generators
   #:gen-primitive-type
   #:gen-type-variable
   #:gen-simple-compound-type
   #:gen-values-type
   #:gen-fn-type-args
   #:gen-fn-type
   #:gen-array-type
   #:gen-cons-type
   #:gen-type-expr
   #:gen-type-specifier

   ;; Mach-O Constants
   #:+mh-magic+
   #:+mh-magic-64+
   #:+mh-cigam+
   #:+mh-cigam-64+
   #:+cpu-type-x86+
   #:+cpu-type-x86-64+
   #:+cpu-type-arm+
   #:+cpu-type-arm64+
   #:+cpu-subtype-x86-all+
   #:+cpu-subtype-arm-all+
   #:+cpu-subtype-arm64-all+
   #:+mh-object+
   #:+mh-execute+
   #:+mh-fvm+
   #:+mh-core+
   #:+mh-preload+
   #:+mh-dylib+
   #:+mh-dylinker+
   #:+mh-bundle+
   #:+mh-noundefs+
   #:+mh-dyldlink+
   #:+mh-pie+
   #:+lc-segment+
   #:+lc-segment-64+
   #:+lc-symtab+
   #:+lc-dysymtab+
   #:+lc-load-dylib+
   #:+lc-id-dylib+
   #:+lc-load-weak-dylib+
   #:+lc-uuid+
   #:+lc-rpath+
   #:+lc-code-signature+
   #:+lc-reexport-dylib+
   #:+lc-version-min-macosx+
   #:+lc-build-version+
   #:+vm-prot-read+
   #:+vm-prot-write+
   #:+vm-prot-execute+

   ;; Mach-O Structure Generators
   #:gen-mach-magic
   #:gen-mach-cpu-type
   #:gen-mach-cpu-subtype
   #:gen-mach-file-type
   #:gen-mach-flags
   #:gen-mach-header
   #:gen-segment-permissions
   #:gen-segment-name
   #:gen-section-name
   #:gen-mach-section
   #:gen-mach-segment-command
   #:gen-mach-load-command-type

   ;; Mach-O Structures
   #:mach-header
   #:make-mach-header-raw
   #:mach-header-magic
   #:mach-header-cputype
   #:mach-header-cpusubtype
   #:mach-header-filetype
   #:mach-header-ncmds
   #:mach-header-sizeofcmds
   #:mach-header-flags
   #:mach-header-reserved
   #:mach-segment-command
   #:make-mach-segment-raw
   #:mach-segment-command-cmd
   #:mach-segment-command-cmdsize
   #:mach-segment-command-segname
   #:mach-segment-command-vmaddr
   #:mach-segment-command-vmsize
   #:mach-segment-command-fileoff
   #:mach-segment-command-filesize
   #:mach-segment-command-maxprot
   #:mach-segment-command-initprot
   #:mach-segment-command-nsects
   #:mach-segment-command-flags
   #:mach-segment-command-sections
   #:mach-section
   #:make-mach-section-raw
   #:mach-section-sectname
   #:mach-section-segname
   #:mach-section-addr
   #:mach-section-size
   #:mach-section-offset
   #:mach-section-align
   #:mach-section-reloff
   #:mach-section-nreloc
   #:mach-section-flags
   #:mach-section-reserved1
   #:mach-section-reserved2
   #:mach-section-reserved3

   ;; Typed AST Structures
   #:typed-ast
   #:make-typed-ast-raw
   #:typed-ast-node-type
   #:typed-ast-source-node
   #:typed-ast-int
   #:make-typed-ast-int-raw
   #:typed-ast-int-value
   #:typed-ast-float
   #:make-typed-ast-float-raw
   #:typed-ast-float-value
   #:typed-ast-string
   #:make-typed-ast-string-raw
   #:typed-ast-string-value
   #:typed-ast-boolean
   #:make-typed-ast-boolean-raw
   #:typed-ast-boolean-value
   #:typed-ast-var
   #:make-typed-ast-var-raw
   #:typed-ast-var-name
   #:typed-ast-binop
   #:make-typed-ast-binop-raw
   #:typed-ast-binop-op
   #:typed-ast-binop-lhs
   #:typed-ast-binop-rhs
   #:typed-ast-if
   #:make-typed-ast-if-raw
   #:typed-ast-if-cond
   #:typed-ast-if-then
   #:typed-ast-if-else
   #:typed-ast-lambda
   #:make-typed-ast-lambda-raw
   #:typed-ast-lambda-params
   #:typed-ast-lambda-body
   #:typed-ast-call
   #:make-typed-ast-call-raw
   #:typed-ast-call-func
   #:typed-ast-call-func-type
   #:typed-ast-call-args
   #:typed-ast-let
   #:make-typed-ast-let-raw
   #:typed-ast-let-bindings
   #:typed-ast-let-body

   ;; Typed AST Generators
   #:gen-typed-primitive-value
   #:gen-typed-terminal
   #:gen-typed-binop
   #:gen-typed-if
   #:gen-typed-param
   #:gen-typed-lambda
   #:gen-typed-call
   #:gen-typed-let
   #:gen-typed-ast-node

   ;; Typed AST Utilities
   #:typed-ast-to-sexp
   #:extract-type-from-ast))

(in-package :cl-cc/pbt)
