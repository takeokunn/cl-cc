;;;; cl-cc-cli.asd
;;;; Extracted from cl-cc.asd as part of the packages/ reorganization.
;;;; System name :cl-cc/bin preserved for backward compatibility with the
;;;; build-cli.lisp script (packages/cli/scripts/build-cli.lisp) and
;;;; cl-cc-test.asd dependencies.

(asdf:defsystem :cl-cc/bin
  :description "CL-CC CLI tool"
  :author "CL-CC"
  :license "MIT"
  :version "0.1.0"
  :depends-on (:cl-cc)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "args")
   (:file "main")         ; Help system (%print-global-help, %print-command-help)
   (:file "main-utils")   ; Utilities, flamegraph, SSA block name helpers
    (:file "main-dump")    ; ANSI colors, dump-*-phase functions, compile-opts struct
   (:file "handlers")))   ; Subcommand handlers only
