;;;; cl-cc.asd
;;;; CL-CC: Common Lisp Compiler Collection — umbrella ASDF system.
;;;;
;;;; All subsystems live in packages/**/. This file wires them together:
;;;;   eval-when: pre-load 15 package .asd files + cl-cc-test.asd (16 total)
;;;;   :cl-cc: umbrella package (src/package.lisp) + compile-pipeline
;;;;   :cl-cc-cli: CLI tool
;;;;
;;;; Test systems (:cl-cc-test, :cl-cc-test/clos) are in cl-cc-test.asd (loaded below).

(eval-when (:load-toplevel :execute)
  (require :asdf)
  (flet ((ensure-system-asd (system-name relative-asd here)
           (unless (asdf:find-system system-name nil)
             (load (merge-pathnames relative-asd here)))))
    (let ((here (make-pathname :defaults (or *load-pathname* *compile-file-pathname*)
                               :name nil :type nil)))
      (ensure-system-asd :cl-cc-bootstrap "packages/foundation/bootstrap/cl-cc-bootstrap.asd" here)
      (ensure-system-asd :cl-cc-ast "packages/foundation/ast/cl-cc-ast.asd" here)
      (ensure-system-asd :cl-cc-prolog "packages/foundation/prolog/cl-cc-prolog.asd" here)
      (ensure-system-asd :cl-cc-binary "packages/backend/binary/cl-cc-binary.asd" here)
      (ensure-system-asd :cl-cc-runtime "packages/backend/runtime/cl-cc-runtime.asd" here)
      (ensure-system-asd :cl-cc-bytecode "packages/backend/bytecode/cl-cc-bytecode.asd" here)
      (ensure-system-asd :cl-cc-ir "packages/foundation/ir/cl-cc-ir.asd" here)
      (ensure-system-asd :cl-cc-mir "packages/foundation/mir/cl-cc-mir.asd" here)
      (ensure-system-asd :cl-cc-type "packages/foundation/type/cl-cc-type.asd" here)
      (ensure-system-asd :cl-cc-optimize "packages/engine/optimize/cl-cc-optimize.asd" here)
      (ensure-system-asd :cl-cc-emit "packages/backend/emit/cl-cc-emit.asd" here)
      (ensure-system-asd :cl-cc-parse "packages/frontend/parse/cl-cc-parse.asd" here)
      (ensure-system-asd :cl-cc-expand "packages/frontend/expand/cl-cc-expand.asd" here)
      (ensure-system-asd :cl-cc-compile "packages/engine/compile/cl-cc-compile.asd" here)
      (ensure-system-asd :cl-cc-vm "packages/engine/vm/cl-cc-vm.asd" here))))

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
    :pathname "packages/umbrella/pipeline/src"
    :serial t
    :components
     ((:file "stdlib-source-core")
      (:file "stdlib-source")
      (:file "stdlib-source-ext")
      (:file "stdlib-source-clos")
      (:file "pipeline-stdlib")
      (:file "pipeline-data")
      (:file "pipeline-cps")
      (:file "pipeline")
      (:file "pipeline-runtime")
      (:file "pipeline-selfhost")
     (:file "pipeline-native")
     (:file "pipeline-native-typeclass")
     (:file "pipeline-repl-state")
      (:file "pipeline-repl-data")
      (:file "pipeline-repl-load")
      (:file "pipeline-repl-ourload")))))

(eval-when (:load-toplevel :execute)
  (require :asdf)
  (flet ((maybe-load-asd (system-name relative-asd here)
           (let ((asd-path (merge-pathnames relative-asd here)))
             (when (and (probe-file asd-path)
                        (not (asdf:find-system system-name nil)))
               (load asd-path)))))
    (let ((here (make-pathname :defaults (or *load-pathname* *compile-file-pathname*)
                               :name nil :type nil)))
      ;; :cl-cc-cli and :cl-cc-testing-framework depend on :cl-cc.
      ;; These .asd files are only present in development/test builds; probe-file
      ;; guards so the production Nix derivation succeeds without them.
      (maybe-load-asd :cl-cc-cli "packages/cli/cl-cc-cli.asd" here)
      (maybe-load-asd :cl-cc-testing-framework "packages/testing/framework/cl-cc-testing-framework.asd" here)
      (maybe-load-asd :cl-cc-test "cl-cc-test.asd" here))))

;; :cl-cc-cli is defined in packages/cli/cl-cc-cli.asd.
;; :cl-cc/tests-framework is defined in packages/testing/framework/cl-cc-testing-framework.asd.
