;;;; cl-cc-cli.asd
;;;; Extracted from cl-cc.asd as part of the packages/ reorganization.
;;;; Canonical ASDF system for the CLI package.

(asdf:defsystem :cl-cc-cli
  :description "CL-CC CLI tool"
  :author "takeokunn"
  :license "MIT"
  :homepage "https://github.com/nerima-lisp/cl-cc"
  :version "0.1.0"
  :depends-on (:cl-cc :cl-cc-docgen :cl-cli :cl-boundary-kit :cl-tty-kit :cl-dataflow-kit)
  :pathname "src"
  :serial t
  :components
   ((:file "package")
    (:file "boundaries")        ; cl-boundary-kit process I/O context (%cli-exit)
    (:file "args")
    (:file "main")              ; Help system (%print-global-help, %print-command-help)
    (:file "main-utils")        ; Utilities, flamegraph, SSA block name helpers
    (:file "plugins")           ; FR-720 plugin architecture (extension registries)
    (:file "flamegraph")
    (:file "main-dump")         ; ANSI colors, dump-*-phase functions, compile-opts struct
    (:file "tty")               ; cl-tty-kit-backed terminal styling (REPL colors)
    (:file "handlers-ql")       ; Quicklisp/ASDF integration (FR-763)
    (:file "handlers-wasm")     ; Wasm AOT toolchain helpers
    (:file "handlers")          ; Thin handler module aggregator
    (:file "handlers-utils")    ; Shared handler utilities
    (:file "handlers-tooling")  ; Tooling / observability handlers
    (:file "handlers-run")      ; run handler
    (:file "handlers-build")    ; save-core and selfhost handlers
    (:file "handlers-compile")  ; compile handler
    (:file "handlers-eval")     ; eval handler
    (:file "handlers-repl")     ; REPL handler
    (:file "handlers-advanced") ; check and advanced/stub handlers
    (:file "dep-graph")         ; FR-361: Dependency graph visualization
    (:file "cli-spec")))        ; cl-cli app spec: completion/docs/version
