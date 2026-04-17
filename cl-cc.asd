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
  (let ((here (make-pathname :defaults (or *load-pathname* *compile-file-pathname*)
                             :name nil :type nil)))
    ;; :cl-cc/bin and :cl-cc/tests-framework are NOT pre-built by Nix — always
    ;; register them so ASDF can find them by name regardless of the Nix fast
    ;; path. They live under packages/ and are always in the build sandbox.
    (asdf:load-asd (merge-pathnames "packages/cli/cl-cc-cli.asd" here))
    (asdf:load-asd (merge-pathnames "packages/testing/framework/cl-cc-testing-framework.asd" here))
    ;; cl-cc-test.asd is only bundled in the test/coverage derivations, not the
    ;; default build — probe-file before loading so the default build succeeds.
    (let ((test-asd (merge-pathnames "cl-cc-test.asd" here)))
      (when (probe-file test-asd)
        (asdf:load-asd test-asd)))
    ;; Skip the sibling-system load-asd bootstrap when those systems are already
    ;; registered (e.g. via Nix sbcl.withPackages). Re-registering would
    ;; redirect systems to in-tree paths and invalidate pre-built FASLs.
    (unless (asdf:find-system :cl-cc-bootstrap nil)
      (asdf:load-asd (merge-pathnames "packages/foundation/bootstrap/cl-cc-bootstrap.asd" here))
      (asdf:load-asd (merge-pathnames "packages/foundation/ast/cl-cc-ast.asd" here))
      (asdf:load-asd (merge-pathnames "packages/foundation/prolog/cl-cc-prolog.asd" here))
      (asdf:load-asd (merge-pathnames "packages/backend/binary/cl-cc-binary.asd" here))
      (asdf:load-asd (merge-pathnames "packages/backend/runtime/cl-cc-runtime.asd" here))
      (asdf:load-asd (merge-pathnames "packages/backend/bytecode/cl-cc-bytecode.asd" here))
      (asdf:load-asd (merge-pathnames "packages/foundation/ir/cl-cc-ir.asd" here))
      (asdf:load-asd (merge-pathnames "packages/foundation/mir/cl-cc-mir.asd" here))
      (asdf:load-asd (merge-pathnames "packages/foundation/type/cl-cc-type.asd" here))
      (asdf:load-asd (merge-pathnames "packages/engine/optimize/cl-cc-optimize.asd" here))
      (asdf:load-asd (merge-pathnames "packages/backend/emit/cl-cc-emit.asd" here))
      (asdf:load-asd (merge-pathnames "packages/frontend/parse/cl-cc-parse.asd" here))
      (asdf:load-asd (merge-pathnames "packages/frontend/expand/cl-cc-expand.asd" here))
      (asdf:load-asd (merge-pathnames "packages/engine/compile/cl-cc-compile.asd" here))
      (asdf:load-asd (merge-pathnames "packages/engine/vm/cl-cc-vm.asd" here)))))

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
    :pathname "packages/umbrella/pipeline"
    :serial t
    :components
    ((:file "stdlib-source")
     (:file "stdlib-source-ext")
     (:file "pipeline-stdlib")
     (:file "pipeline")
     (:file "pipeline-native")
     (:file "pipeline-repl")))))

;; :cl-cc/bin is defined in packages/cli/cl-cc-cli.asd.
;; :cl-cc/tests-framework is defined in packages/testing/framework/cl-cc-testing-framework.asd.
