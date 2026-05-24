;;;; cli/src/main.lisp — CL-CC CLI Entry Point
;;;;
;;;; Subcommands:
;;;;   run     <file> [--lang lisp|php] [--stdlib] [--verbose]
;;;;   compile <file> [-o out] [--arch x86-64|arm64] [--lang lisp|php] [--verbose]
;;;;   eval    <expr> [--stdlib] [--verbose]
;;;;   check   <file> [--lang lisp|php] [--strict] [--verbose]
;;;;   selfhost [file] [--profile]
;;;;   help    [command]

(in-package :cl-cc/cli)

(defparameter *version* "0.1.0"
  "CL-CC version string.")

;;; ─────────────────────────────────────────────────────────────────────────
;;; Help text
;;; ─────────────────────────────────────────────────────────────────────────

(defun %print-global-help ()
  (format t "Usage: cl-cc <command> [options] [file]

Commands:
  run      <file>         Compile and run source file
  compile  <file>         Compile to native binary
  eval     <expr>         Evaluate an expression inline
  repl                    Start interactive REPL
  check    <file>         Type-check only, no execution
  selfhost [file]         Run the self-hosting profile workload
  install  <system.asd>   Register/compile a local ASDF system
  uninstall <system>      Remove a registered local system
  fuzz     [--seed N]     Compiler fuzzing (FR-794)
  reduce   <file>         Test case reduction (FR-796)
  audit                   Dependency vulnerability scan (FR-814)
  doc      <path>         API documentation generation (FR-902)
  doctest  <path>         Run docstring code examples (FR-903)
  show-types <file>       Show inferred type signatures (FR-904)
  assert-density <path>   Assertion density analysis (FR-905)
  abi-dump  <file>        Dump ABI manifest (FR-777)
  abi-check <old> <new>   Check ABI compatibility (FR-777)
  demangle  <name>        Demangle C++ ABI symbol (FR-776)
  disasm    [--wat] <wasm> Disassemble Wasm via wabt tools (FR-322)
  inspect   <wasm>         Inspect Wasm sections/disassembly (FR-322)
  objdump   <file>        Inspect binary internals (FR-808)
  macrostep <file>        Step through macro expansion (FR-836)
  bisect    [range]       Find regression commit (FR-809)
  features                List feature flags (FR-812)
  generate  <schema>      Build-time code generation (FR-815)
  update    [pkg]         Update dependencies (FR-813)
  help     [command]      Show this help or command-specific help

Options:
  -o, --output <file>     Output file (compile only)
  --arch x86-64|arm64|wasm32 Target architecture (default: x86-64)
  --lang lisp|php         Source language (auto-detect from file extension)
  --dump-ir <phase>       Dump IR for phase: ast, cps, ssa, vm, opt, asm
  --annotate-source       Add source-location comments when available
  --debug                 Keep frame pointers for native compile debugging
  --retpoline             Enable retpoline mitigation for indirect branches
  --spectre-mitigations   Enable x86-64 LFENCE + retpoline hardening
  --jit-cache-stats       Print JIT code-cache hit/eviction statistics
  --stack-protector       Enable stack canary checks in native prologue/epilogue
  --shadow-stack          Enable CET shadow-stack planning/integration path
  --eh-model sjlj|table   Select legacy SJLJ or DWARF table zero-cost EH
  --compress              Add compressed code payload sections when supported
  --no-compress           Disable code payload compression (default)
  --memory64              Enable opt-in Wasm Memory64/Table64 codegen
  --aot                   Generate AOT static .wasm when targeting wasm
  --streaming             Generate instantiateStreaming JS glue for wasm
  --validate              Validate generated wasm before deployment
  --sri                   Generate SRI hashes and metadata
  --stdlib                Eagerly prepend standard library (run/eval/repl)
  --no-stdlib             Disable lazy stdlib auto-require
  --opt-remarks <mode>    Print optimizer remarks: all, changed, missed
   --optimization-report   Print per-optimization debugging report lines
   --tier N               Compilation tier: 0 fast, 1 optimized (default: 0)
   --verbose               Show compilation details on stderr
   --pass-pipeline <spec>  Optimizer pipeline (e.g. fold,dce)
   --block-compile       Enable file/LTO cross-function inlining (max 30 insts)
   --print-pass-timings    Print per-pass optimizer timings
  --time-passes          Alias for --print-pass-timings
  --trace-json <file>     Write Chrome trace JSON for optimizer passes
  --pgo-generate <file>   Write lightweight optimizer profile data
  --pgo-use <file>        Load optimizer profile data (speed/policy hint)
  --bolt                  Enable BOLT-style native binary layout optimization
  --profile               Enable VM profile collection/reporting
  --flamegraph <file>     Write a sampled VM flame graph SVG (run/eval only)
  --stats                 Print per-pass optimizer stats
  --trace-emit            Print VM/OPT/ASM compilation stages
  --strict                Treat type warnings as errors (check only)
  --timeout <seconds>     Maximum execution time (default: 30 seconds)
  --no-timeout            Disable CLI timeout for debugging
  --dump-image <file>     Dump an initialized SBCL image/executable
  --system <name>         Compile an ASDF system and dependencies
  --source-map            Emit external .wasm.map and sourceMappingURL
  --debug-info            Emit DWARF custom sections for wasm targets
  --emit-names            Emit extended wasm name metadata
  --deterministic         Enable reproducible build mode (FR-917)

Version: ~A~%" *version*))

(defparameter *cli-help-strings*
  '(("run" . "Usage: cl-cc run [options] <file>

  Compile and run a source file using the CL-CC VM.

Options:
  --lang lisp|php   Source language (auto-detect from .php extension)
  --dump-ir <phase>  Dump IR for phase: ast, cps, ssa, vm, opt, asm
  --annotate-source  Add source-location comments when available
  --stdlib          Prepend standard library eagerly
  --no-stdlib       Disable lazy stdlib auto-require
  --opt-remarks <mode>  Print optimizer remarks: all, changed, missed
  --optimization-report Print per-optimization debugging report lines
  --tier N          Compilation tier: 0 fast, 1 optimized
  --verbose         Show compilation details on stderr
  --pass-pipeline <spec>  Optimizer pipeline (e.g. fold,dce)
   --block-compile       Enable file/LTO cross-function inlining (max 30 insts)
  --print-pass-timings    Print per-pass optimizer timings
  --time-passes          Alias for --print-pass-timings
   --trace-json <file>     Write Chrome trace JSON for optimizer passes
   --pgo-generate <file>   Write lightweight optimizer profile data
     --pgo-use <file>        Load optimizer profile data (speed/policy hint)
     --bolt                  Enable BOLT-style native binary layout optimization
   --profile               Print a sampled VM frame report
    --flamegraph <file>     Write a sampled VM flame graph SVG
  --stats                 Print per-pass optimizer stats
  --trace-emit            Print VM/OPT/ASM compilation stages
  --timeout <seconds>     Maximum execution time (default: 30 seconds)
  --no-timeout            Disable CLI timeout for debugging
  --source-map            Emit external .wasm.map for wasm targets
  --debug-info            Emit DWARF custom sections for wasm targets
  --emit-names            Emit extended wasm name metadata
 ")
    ("compile" . "Usage: cl-cc compile [options] <file>

  Compile source to a native Mach-O binary or AOT .wasm.

Options:
  -o, --output <file>   Output file (default: input without extension)
  --system <name>       Compile ASDF system NAME instead of a single file
  --arch x86-64|arm64|wasm32 Target architecture (default: x86-64)
  --lang lisp|php       Source language (auto-detect from .php extension)
  --dump-ir <phase>     Dump IR for phase: ast, cps, ssa, vm, opt, asm
  --annotate-source     Add source-location comments when available
  --debug               Keep frame pointers for native compile debugging
  --retpoline           Enable retpoline mitigation for indirect branches
  --spectre-mitigations Enable x86-64 LFENCE + retpoline hardening
  --jit-cache-stats     Print JIT code-cache hit/eviction statistics
  --stack-protector     Enable stack canary checks in native prologue/epilogue
  --shadow-stack        Enable CET shadow-stack planning/integration path
  --eh-model sjlj|table Select legacy SJLJ or DWARF table zero-cost EH
  --compress            Add compressed code payload sections when supported
  --memory64            Enable opt-in Wasm Memory64/Table64 codegen
  --no-compress         Disable code payload compression (default)
  --deterministic       Strip volatile build timestamps where possible
  --aot                 Generate AOT static .wasm when targeting wasm
  --streaming           Generate instantiateStreaming JS glue for wasm
  --validate            Run WebAssembly.validate/wasmtime validation if available
  --sri                 Generate SHA-256/SHA-384 SRI metadata
  --build-id <id|auto>  Embed a build id/hash marker in native output
  --opt-remarks <mode>  Print optimizer remarks: all, changed, missed
  --optimization-report Print per-optimization debugging report lines
  --tier N              Compilation tier: 0 fast, 1 optimized
  --verbose             Show compilation details on stderr
  --pass-pipeline <spec>  Optimizer pipeline (e.g. fold,dce)
  --block-compile         Enable file/LTO cross-function inlining (max 30 insts)
  --print-pass-timings    Print per-pass optimizer timings
  --time-passes          Alias for --print-pass-timings
   --trace-json <file>     Write Chrome trace JSON for optimizer passes
   --pgo-generate <file>   Write lightweight optimizer profile data
    --pgo-use <file>        Load optimizer profile data (speed/policy hint)
   --profile               Print a sampled VM frame report
    --flamegraph <file>     Write a sampled VM flame graph SVG
  --stats                 Print per-pass optimizer stats
  --trace-emit            Print VM/OPT/ASM compilation stages
  --timeout <seconds>     Maximum execution time (default: 30 seconds)
  --no-timeout            Disable CLI timeout for debugging
 ")
    ("eval" . "Usage: cl-cc eval [options] <expr>

  Evaluate a CL-CC expression and print the result.

Options:
  --stdlib   Eagerly prepend standard library
  --no-stdlib Disable lazy stdlib auto-require
  --opt-remarks <mode>  Print optimizer remarks: all, changed, missed
  --optimization-report Print per-optimization debugging report lines
  --tier N          Compilation tier: 0 fast, 1 optimized
  --verbose  Show compilation details on stderr
  --pass-pipeline <spec>  Optimizer pipeline (e.g. fold,dce)
  --block-compile         Enable file/LTO cross-function inlining (max 30 insts)
  --print-pass-timings    Print per-pass optimizer timings
  --time-passes          Alias for --print-pass-timings
   --trace-json <file>     Write Chrome trace JSON for optimizer passes
   --pgo-generate <file>   Write lightweight optimizer profile data
    --pgo-use <file>        Load optimizer profile data (speed/policy hint)
   --profile               Print a sampled VM frame report
    --flamegraph <file>     Write a sampled VM flame graph SVG
  --stats                 Print per-pass optimizer stats
  --trace-emit            Print VM/OPT/ASM compilation stages
  --timeout <seconds>     Maximum execution time (default: 30 seconds)
  --no-timeout            Disable CLI timeout for debugging
 ")
    ("repl" . "Usage: cl-cc repl [options]

  Start an interactive ANSI Common Lisp REPL.
  Definitions persist across expressions within the session.
  ANSI REPL variables *, **, ***, +, ++, +++, /, //, /// are maintained.

Options:
  --stdlib   Prepend standard library on startup
  --no-stdlib Disable lazy stdlib auto-require
  --timeout <seconds>     Per-form execution timeout (default: 30 seconds)
  --no-timeout            Disable REPL form timeout for debugging

Commands:
  :inspect <expr>    Print the evaluated object's type and slots/contents
  :describe <name>   Print documentation and host description

Examples:
  * (defun square (x) (* x x))
  * (square 7)
  => 49
  * (exit) or Ctrl+D to quit
")
    ("check" . "Usage: cl-cc check [options] <file>

  Run type inference on a source file without executing it.

 Options:
  --lang lisp|php   Source language
  --strict          Treat type warnings as errors (exit 1)
  --verbose         Show type-inference details on stderr
  --timeout <seconds>     Maximum execution time (default: 30 seconds)
  --no-timeout            Disable CLI timeout for debugging
   ")
    ("selfhost" . "Usage: cl-cc selfhost [options] [file]

  Run a self-hosting workload through the CL-CC VM.
  With --profile, write .cl-cc-profile.sexp as an instruction histogram used by later compilations.

Options:
  --profile         Collect VM instruction frequencies
  --stdlib          Eagerly prepend standard library
  --no-stdlib       Disable lazy stdlib auto-require
  --verbose         Show compilation details on stderr
  --timeout <seconds>     Maximum execution time (default: 30 seconds)
  --no-timeout            Disable CLI timeout for debugging
 ")
    ("install" . "Usage: cl-cc install <system.asd>

  Register a local ASDF system file for cl-cc system compilation.
")
    ("uninstall" . "Usage: cl-cc uninstall <system>

  Remove a previously registered local ASDF system from the cl-cc registry.
")
  )
  "Alist mapping command name strings to their help text strings.")

(defun %print-command-help (command)
  (let ((entry (assoc command *cli-help-strings* :test #'string=)))
    (if entry
        (format t "~A" (cdr entry))
        (progn
          (format *error-output* "Unknown command: ~A~%" command)
          (%print-global-help)))))

(defun %print-help (&optional command)
  (if command
      (%print-command-help command)
      (%print-global-help)))


;;; ─────────────────────────────────────────────────────────────────────────
;;; Main dispatcher
;;; ─────────────────────────────────────────────────────────────────────────

(defparameter *cli-command-dispatch*
  '(("run"      . %do-run)
    ("compile"  . %do-compile)
    ("eval"     . %do-eval)
    ("repl"     . %do-repl)
    ("check"    . %do-check)
    ("selfhost" . %do-selfhost)
    ;; ── Phase 129-160: Advanced Compilation III commands ──
    ("fuzz"         . %do-fuzz)
    ("reduce"       . %do-reduce)
    ("audit"        . %do-audit)
    ("doc"          . %do-doc)
    ("doctest"      . %do-doctest)
    ("show-types"   . %do-show-types)
    ("assert-density" . %do-assert-density)
    ("abi-dump"     . %do-abi-dump)
     ("abi-check"    . %do-abi-check)
     ("demangle"     . %do-demangle)
     ("disasm"       . %do-disasm)
     ("inspect"      . %do-inspect)
     ("objdump"      . %do-objdump)
    ("macrostep"    . %do-macrostep)
    ("bisect"       . %do-bisect)
    ("features"     . %do-features)
    ("generate"     . %do-generate)
    ("update"       . %do-update))
  "Alist mapping command name strings to their handler functions.")

(defun main ()
  "CL-CC CLI entry point.
Reads POSIX argv via UIOP:COMMAND-LINE-ARGUMENTS, parses flags and
subcommands, then dispatches to the appropriate handler."
  (let* ((argv   (uiop:command-line-arguments))
         (parsed (handler-case (parse-args argv)
                   (arg-parse-error (e)
                     (format *error-output* "~A~%" (arg-parse-error-message e))
                     (terpri *error-output*)
                     (%print-global-help)
                     (uiop:quit 2)))))
    (when (or (flag parsed "--help") (flag parsed "-h"))
      (%print-help (or (parsed-args-command parsed)
                       (car (parsed-args-positional parsed))))
      (uiop:quit 0))

    (when (flag parsed "--dump-image")
      (%dump-image-and-exit parsed))

    (when (flag parsed "--deterministic")
      (configure-reproducible-build))

    (let ((command (parsed-args-command parsed)))
      (cond
        ((null command)
         (%print-global-help)
         (uiop:quit 0))
        ((string= command "help")
          (%print-help (car (parsed-args-positional parsed)))
          (uiop:quit 0))
        ((string= command "install")
         (%do-install parsed))
        ((string= command "uninstall")
         (%do-uninstall parsed))
          (t
          (let ((entry (assoc command *cli-command-dispatch* :test #'string=)))
            (if entry
                (let ((cl-cc/optimize:*optimization-report-stream*
                        (if (flag parsed "--optimization-report")
                            *standard-output*
                            cl-cc/optimize:*optimization-report-stream*))
                      (cl-cc/pipeline:*compilation-tier*
                        (cl-cc/pipeline:normalize-compilation-tier
                         (or (flag parsed "--tier") 0))))
                  (funcall (cdr entry) parsed))
                (progn
                  (format *error-output* "Unknown command: ~A~%~%" command)
                  (%print-global-help)
                  (uiop:quit 2)))))))))


;;; (Utilities, dump functions, and compile options
;;;  are in main-utils.lisp which loads after this file.)
