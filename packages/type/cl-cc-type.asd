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
     (:file "types-extended-concurrency")
     (:file "types-extended-units")
     (:file "types-extended-routing-types")
     (:file "types-extended-ffi")
     (:file "types-extended-registries")
     (:file "types-extended-qtt")
     (:file "types-extended-dependent")
     (:file "types-extended-advanced-meta")
     (:file "types-extended-advanced-meta-validators")
     (:file "types-extended-advanced-validators")
     (:file "types-extended-advanced-data")
     (:file "types-extended-advanced-evidence-data")
     (:file "types-extended-advanced-validate")
     (:file "types-extended-advanced-init")
     (:file "types-extended-nodes")
     (:file "types-env")
     (:file "substitution")
   (:file "substitution-schemes")
   (:file "unification")
   (:file "subtyping")
   (:file "effect")
   (:file "row")
   (:file "constraint")
   (:file "parser")
   (:file "parser-extended")
        (:file "parser-typed")
         (:file "typeclass")
         (:file "solver")
   (:file "solver-collect")
   (:file "inference")
   (:file "inference-handlers")
   (:file "inference-forms")
   (:file "inference-forms-advanced")
   (:file "inference-forms-advanced-validators")
   (:file "inference-forms-advanced-init")
   (:file "inference-conditions")
   (:file "inference-effects")
   (:file "bidirectional")
    (:file "checker")
    (:file "printer")
    (:file "printer-unparse")
    (:file "exhaustiveness")
    ;; FR-1602/1701/1702/1803/1804/2202-2206/3303-3305 utility modules
    ;; Keep this order: channels before actors/stm/coroutines/simd.
    (:file "generics")
    (:file "channels")
    (:file "actors")
    (:file "stm")
    (:file "coroutines")
    (:file "simd")
    (:file "routing")
    (:file "utils")))
