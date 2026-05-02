;;;; cl-cc-php.asd — PHP frontend: lexer, parser, grammar

(asdf:defsystem :cl-cc-php
  :description "CL-CC PHP frontend: lexer, parser, and grammar"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-ast :cl-cc-bootstrap :cl-cc-parse)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "lexer")
   (:file "lexer-ops")
   (:file "parser")
   (:file "parser-expr")
   (:file "parser-stmt")
   (:file "parser-class")
   (:file "grammar")
   (:file "grammar-stmt")))
