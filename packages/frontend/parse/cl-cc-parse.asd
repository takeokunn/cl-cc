;;;; cl-cc-parse.asd — True-encapsulation ASDF system for the parse subsystem
;;;;
;;;; Phase 3 of the package-by-feature monorepo migration.
;;;; All parse source files are now (in-package :cl-cc/parse).
;;;; The umbrella :cl-cc uses :cl-cc/parse and re-exports via do-external-symbols.

(asdf:defsystem :cl-cc-parse
  :description "CL-CC Parse subsystem: CST, lexer, parser, lowering, and PHP frontend"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-ast :cl-cc-bootstrap)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "cst")
   (:file "diagnostics")
   (:file "lexer")
   (:file "lexer-readers")
   (:file "lexer-skip")
   (:file "lexer-dispatch")
   (:file "incremental")
   (:file "pratt")
   (:file "combinators")
   (:module "cl"
    :serial t
    :components
    ((:file "parser")
     (:file "parser-sexp-lowering")
     (:file "lower")
     (:file "lower-setf")
     (:file "lower-definitions")
     (:file "lower-clos")
     (:file "parser-roundtrip")
      (:file "grammar-token-stream")
      (:file "grammar")
      (:file "grammar-entrypoints")))
   (:module "php"
    :serial t
    :components
    ((:file "lexer")
     (:file "lexer-ops")
     (:file "parser")
     (:file "parser-expr")
     (:file "parser-stmt")
     (:file "parser-class")
     (:file "grammar")
     (:file "grammar-stmt")))
   (:file "cst-to-ast")))
