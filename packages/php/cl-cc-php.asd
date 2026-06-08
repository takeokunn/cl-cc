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
    (:file "runtime-helpers")
    ;; PHP runtime builtins (count, array_*, str*, math, type predicates, and the
    ;; dispatch registry). Previously omitted, so their bodies were never compiled
    ;; — runtime calls such as array_find -> %php-array-pairs / %php-callable-function
    ;; hit undefined functions. register loads last (it references the others).
    (:file "runtime-builtins-core")
    (:file "runtime-builtins-array")
    (:file "runtime-builtins-string")
    (:file "runtime-builtins-regex")
    (:file "runtime-builtins-math")
    (:file "runtime-builtins-types")
    (:file "runtime-builtins-io")
    (:file "runtime-builtins-register")
     (:file "parser")
     (:file "parser-support")
     (:file "parser-attributes")
     (:file "parser-expr")
     (:file "parser-expr-advanced")
   (:file "parser-stmt-lowering")
   (:file "parser-stmt-decls")
    (:file "parser-class")
    (:file "parser-trait")
    (:file "parser-interface")
    (:file "php84-features")
    (:file "unsupported")
    (:file "grammar")
   (:file "grammar-stmt")))
