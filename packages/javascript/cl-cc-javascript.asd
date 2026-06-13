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
   (:file "lexer-operator")
   (:file "lexer-number")
   (:file "lexer-template")
   (:file "lexer-regex")
   (:file "parser")
   (:file "parser-stmt-binding")
   (:file "parser-expr")
   (:file "parser-expr-literal")
   (:file "parser-expr-postfix")
   (:file "parser-expr-unary")
   (:file "parser-expr-primary")
   (:file "parser-arrow")
   (:file "parser-stmt")
   (:file "parser-stmt-fn")
   (:file "parser-stmt-control")
   (:file "parser-stmt-flow")
   (:file "parser-class")
   (:file "parser-class-lower")
   (:file "parser-module")
   (:file "parser-module-export")
   (:file "parser-pattern")
   (:file "parser-pattern-lower")
   ;; NOTE: there is intentionally no separate ast-lower pass. The parser lowers
   ;; JS-specific forms inline (e.g. %js-lower-assignment for &&=/||=/??=, %js-this
   ;; emitted directly), matching the PHP frontend's inline-lowering model. The
   ;; former ast-lower.lisp was dead, uncalled, and inconsistent — removed.
   (:file "runtime")
   (:file "runtime-property")
   (:file "runtime-control")
   (:file "runtime-array")
   (:file "runtime-array-es2023")
   (:file "runtime-object")
   (:file "runtime-string")
   (:file "runtime-math")
   (:file "runtime-collections")
   (:file "runtime-async")
   (:file "runtime-map")
   (:file "runtime-symbol")
   (:file "runtime-date")
   (:file "runtime-regex")
   (:file "runtime-typed-arrays")
   (:file "runtime-typed-arrays-methods")
   (:file "runtime-class")
   (:file "runtime-ops")
   (:file "runtime-temporal")
   (:file "runtime-builtins")
   (:file "runtime-builtins-table")
   (:file "runtime-method-resolver")))
