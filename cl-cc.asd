;;;; cl-cc.asd
;;;; CL-CC: Common Lisp Compiler Collection — umbrella ASDF system.
;;;;
;;;; All subsystems live in packages/**/. This file wires them together:
;;;;   eval-when: pre-load 15 package .asd files + cl-cc-test.asd (16 total)
;;;;   :cl-cc: umbrella package (src/package.lisp) + compile-pipeline
;;;;   :cl-cc/bin: CLI tool
;;;;
;;;; Test systems (:cl-cc/test, :cl-cc/test-clos) are in cl-cc-test.asd (loaded below).

(eval-when (:load-toplevel :execute)
  ;; Skip the monorepo load-asd bootstrap when sibling systems are already
  ;; registered (e.g. via Nix sbcl.withPackages). Re-registering would
  ;; redirect systems to in-tree paths and invalidate pre-built FASLs.
  (unless (asdf:find-system :cl-cc-bootstrap nil)
    (let ((here (make-pathname :defaults (or *load-pathname* *compile-file-pathname*)
                               :name nil :type nil)))
      (asdf:load-asd (merge-pathnames "packages/foundation/bootstrap/cl-cc-bootstrap.asd" here))
      (asdf:load-asd (merge-pathnames "packages/foundation/ast/cl-cc-ast.asd" here))
      (asdf:load-asd (merge-pathnames "packages/prolog/prolog/cl-cc-prolog.asd" here))
      (asdf:load-asd (merge-pathnames "packages/backend/binary/cl-cc-binary.asd" here))
      (asdf:load-asd (merge-pathnames "packages/backend/runtime/cl-cc-runtime.asd" here))
      (asdf:load-asd (merge-pathnames "packages/backend/bytecode/cl-cc-bytecode.asd" here))
      (asdf:load-asd (merge-pathnames "packages/foundation/ir/cl-cc-ir.asd" here))
      (asdf:load-asd (merge-pathnames "packages/foundation/mir/cl-cc-mir.asd" here))
      (asdf:load-asd (merge-pathnames "packages/type/type/cl-cc-type.asd" here))
      (asdf:load-asd (merge-pathnames "packages/engine/optimize/cl-cc-optimize.asd" here))
      (asdf:load-asd (merge-pathnames "packages/backend/emit/cl-cc-emit.asd" here))
      (asdf:load-asd (merge-pathnames "packages/frontend/parse/cl-cc-parse.asd" here))
      (asdf:load-asd (merge-pathnames "packages/frontend/expand/cl-cc-expand.asd" here))
      (asdf:load-asd (merge-pathnames "packages/engine/compile/cl-cc-compile.asd" here))
      (asdf:load-asd (merge-pathnames "packages/engine/vm/cl-cc-vm.asd" here))
      (asdf:load-asd (merge-pathnames "cl-cc-test.asd" here)))))

(asdf:defsystem :cl-cc
  :description "CL-CC: Common Lisp Compiler Collection"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap :cl-cc-ast :cl-cc-prolog :cl-cc-parse :cl-cc-binary
               :cl-cc-runtime :cl-cc-bytecode :cl-cc-ir :cl-cc-mir :cl-cc-type
               :cl-cc-optimize :cl-cc-emit :cl-cc-expand :cl-cc-compile :cl-cc-vm)
  :components
  ((:module "src"
    :pathname "packages/umbrella/src"
    :serial t
    :components
    ((:file "package")))
   ;; compile-pipeline: final stage that wires umbrella API (compile-expression, run-string).
   ;; Uses (in-package :cl-cc) — loads after src/package establishes the umbrella.
   (:module "compile-pipeline"
    :pathname "packages/engine/pipeline/src"
    :serial t
    :components
    ((:file "stdlib-source")
     (:file "stdlib-source-ext")
     (:file "pipeline-stdlib")
     (:file "pipeline")
     (:file "pipeline-native")
     (:file "pipeline-repl")))))

(asdf:defsystem :cl-cc/bin
  :description "CL-CC CLI tool"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc)
  :components
  ((:module "cli"
    :pathname "packages/cli/src"
    :serial t
    :components
    ((:file "package")
     (:file "args")
     (:file "selfhost") ; Self-hosting verification data + phases (before main.lisp)
     (:file "main")       ; Help system (%print-global-help, %print-command-help)
     (:file "main-utils") ; Utilities, flamegraph, SSA block name helpers
     (:file "main-dump")  ; ANSI colors, dump-*-phase functions, compile-opts struct
     (:file "handlers"))))) ; Subcommand handlers + main dispatcher
