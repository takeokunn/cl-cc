;;;; cl-cc-type.asd — independent ASDF system for the type system
;;;;
;;;; Phase 2 of the package-by-feature monorepo migration. Files live in the
;;;; :cl-cc/type package (kind, multiplicity, types, inference, checker, etc.).
;;;; Depends on :cl-cc-ast for AST node types referenced during constraint
;;;; collection and type inference (solver-collect, inference, inference-forms,
;;;; inference-effects).

(asdf:defsystem :cl-cc-type
  :description "cl-cc type system — kinds, multiplicity, HM inference, type classes, effects"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-ast)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "kind")
   (:file "multiplicity")
   (:file "types-core")
   (:file "types-extended")
   (:file "types-env")
   (:file "substitution")
   (:file "substitution-schemes")
   (:file "unification")
   (:file "subtyping")
   (:file "effect")
   (:file "row")
   (:file "constraint")
   (:file "parser")
   (:file "parser-typed")
   (:file "typeclass")
   (:file "typeclass-compat")
   (:file "typeclass-compat-legacy")
   (:file "solver")
   (:file "solver-collect")
   (:file "inference")
   (:file "inference-forms")
   (:file "inference-conditions")
   (:file "inference-effects")
   (:file "bidirectional")
   (:file "checker")
   (:file "printer")
   (:file "exhaustiveness")))
