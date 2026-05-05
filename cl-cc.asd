;;;; cl-cc.asd
;;;; CL-CC: Common Lisp Compiler Collection — umbrella ASDF system.
;;;;
;;;; All subsystems live in packages/**/. This file wires them together:
;;;;   eval-when: pre-load 15 package .asd files + cl-cc-test.asd (16 total)
;;;;   :cl-cc: umbrella package (src/package.lisp) + compile-pipeline
;;;;   :cl-cc-cli: CLI tool
;;;;
;;;; Test systems (:cl-cc-test, :cl-cc-test/e2e) are in cl-cc-test.asd (loaded below).

(eval-when (:load-toplevel :execute)
  (require :asdf)
  (flet ((ensure-system-asd (system-name relative-asd here)
           (unless (asdf:find-system system-name nil)
             (load (merge-pathnames relative-asd here)))))
    (let ((here (make-pathname :defaults (or *load-pathname* *compile-file-pathname*)
                               :name nil :type nil)))
      (ensure-system-asd :cl-cc-bootstrap "packages/bootstrap/cl-cc-bootstrap.asd" here)
      (ensure-system-asd :cl-cc-ast "packages/ast/cl-cc-ast.asd" here)
      (ensure-system-asd :cl-cc-prolog "packages/prolog/cl-cc-prolog.asd" here)
      (ensure-system-asd :cl-cc-binary "packages/binary/cl-cc-binary.asd" here)
      (ensure-system-asd :cl-cc-runtime "packages/runtime/cl-cc-runtime.asd" here)
      (ensure-system-asd :cl-cc-bytecode "packages/bytecode/cl-cc-bytecode.asd" here)
      (ensure-system-asd :cl-cc-ir "packages/ir/cl-cc-ir.asd" here)
      (ensure-system-asd :cl-cc-mir "packages/mir/cl-cc-mir.asd" here)
      (ensure-system-asd :cl-cc-target "packages/target/cl-cc-target.asd" here)
      (ensure-system-asd :cl-cc-type "packages/type/cl-cc-type.asd" here)
      (ensure-system-asd :cl-cc-optimize "packages/optimize/cl-cc-optimize.asd" here)
      (ensure-system-asd :cl-cc-regalloc "packages/regalloc/cl-cc-regalloc.asd" here)
      (ensure-system-asd :cl-cc-parse "packages/parse/cl-cc-parse.asd" here)
      (ensure-system-asd :cl-cc-expand "packages/expand/cl-cc-expand.asd" here)
      (ensure-system-asd :cl-cc-cps "packages/cps/cl-cc-cps.asd" here)
      (ensure-system-asd :cl-cc-codegen "packages/codegen/cl-cc-codegen.asd" here)
      (ensure-system-asd :cl-cc-emit "packages/emit/cl-cc-emit.asd" here)
      (ensure-system-asd :cl-cc-compile "packages/compile/cl-cc-compile.asd" here)
      (ensure-system-asd :cl-cc-vm "packages/vm/cl-cc-vm.asd" here)
      (ensure-system-asd :cl-cc-stdlib "packages/stdlib/cl-cc-stdlib.asd" here)
      (ensure-system-asd :cl-cc-pipeline "packages/pipeline/cl-cc-pipeline.asd" here)
      (ensure-system-asd :cl-cc-selfhost "packages/selfhost/cl-cc-selfhost.asd" here)
      (ensure-system-asd :cl-cc-repl "packages/repl/cl-cc-repl.asd" here))))

(asdf:defsystem :cl-cc
  :description "CL-CC: Common Lisp Compiler Collection"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc-bootstrap :cl-cc-ast :cl-cc-prolog :cl-cc-parse :cl-cc-binary
               :cl-cc-runtime :cl-cc-bytecode :cl-cc-ir :cl-cc-mir :cl-cc-target
               :cl-cc-type :cl-cc-optimize :cl-cc-regalloc :cl-cc-emit :cl-cc-expand
               :cl-cc-compile :cl-cc-cps :cl-cc-codegen :cl-cc-vm :cl-cc-stdlib
               :cl-cc-pipeline :cl-cc-selfhost :cl-cc-repl)
  :components
  ((:module "src"
    :pathname "packages/umbrella-src"
    :serial t
    :components
    ((:file "package")))))

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
      (maybe-load-asd :cl-cc-testing-framework "packages/testing-framework/cl-cc-testing-framework.asd" here)
      (maybe-load-asd :cl-cc-test "cl-cc-test.asd" here))))

;; :cl-cc-cli is defined in packages/cli/cl-cc-cli.asd.
;; :cl-cc/tests-framework is defined in packages/testing-framework/cl-cc-testing-framework.asd.
