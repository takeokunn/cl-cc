;;;; cl-cc-javascript.asd — JavaScript frontend: lexer, parser, runtime helpers

(asdf:defsystem :cl-cc-javascript
  :description "CL-CC JavaScript frontend: lexer, parser, and runtime helpers"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-ast :cl-cc-bootstrap :cl-cc-parse)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "lexer")
   (:file "lexer-template")
   (:file "lexer-regex")
   (:file "parser")
   (:file "parser-expr")
   (:file "parser-stmt")
   (:file "parser-class")
   (:file "parser-module")
   (:file "parser-pattern")
   (:file "ast-lower")
   (:file "runtime")))
