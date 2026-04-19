;;;; cl-cc-prolog.asd — independent ASDF system for the Prolog engine
;;;;
;;;; Phase 1.1 of the package-by-feature monorepo migration. Files live in
;;;; the :cl-cc/prolog package; the facade :cl-cc (:use :cl-cc/prolog) so
;;;; downstream compiler modules see unify/solve-goal/query-* unqualified.

(asdf:defsystem :cl-cc-prolog
  :description "cl-cc Prolog engine — terms, unification, solver, DCG, peephole rules"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "prolog-data")
   (:file "prolog")
   (:file "prolog-builtins")
   (:file "prolog-solver")
   (:file "prolog-peephole")
   (:file "dcg")))
