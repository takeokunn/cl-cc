;;;; cl-cc-ast.asd — independent ASDF system for the AST node types
;;;;
;;;; Phase 1.2 of the package-by-feature monorepo migration. Files live in
;;;; the :cl-cc/ast package; the facade :cl-cc package (:use :cl-cc/ast) so
;;;; downstream compiler modules see AST symbols unqualified.

(asdf:defsystem :cl-cc-ast
  :description "cl-cc AST node types and protocol (ast-children, ast-bound-names)"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "ast")
   (:file "ast-functions")))
