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
  help     [command]      Show this help or command-specific help

Options:
  -o, --output <file>     Output file (compile only)
  --arch x86-64|arm64     Target architecture (default: x86-64)
  --lang lisp|php         Source language (auto-detect from file extension)
  --dump-ir <phase>       Dump IR for phase: ast, cps, ssa, vm, opt, asm
  --annotate-source       Add source-location comments when available
  --debug                 Keep frame pointers for native compile debugging
  --retpoline             Enable retpoline mitigation for indirect branches
  --spectre-mitigations   Enable x86-64 LFENCE + retpoline hardening
  --jit-cache-stats       Print JIT code-cache hit/eviction statistics
  --stack-protector       Enable stack canary checks in native prologue/epilogue
  --shadow-stack          Enable CET shadow-stack planning/integration path
  --compress              Add compressed code payload sections when supported
  --no-compress           Disable code payload compression (default)
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
  --profile               Enable VM profile collection/reporting
  --flamegraph <file>     Write a sampled VM flame graph SVG (run/eval only)
  --stats                 Print per-pass optimizer stats
  --trace-emit            Print VM/OPT/ASM compilation stages
  --strict                Treat type warnings as errors (check only)
  --timeout <seconds>     Maximum execution time (run/compile/eval/repl)
  --dump-image <file>     Dump an initialized SBCL image/executable

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
   --profile               Print a sampled VM frame report
    --flamegraph <file>     Write a sampled VM flame graph SVG
  --stats                 Print per-pass optimizer stats
  --trace-emit            Print VM/OPT/ASM compilation stages
 ")
    ("compile" . "Usage: cl-cc compile [options] <file>

  Compile source to a native Mach-O binary.

Options:
  -o, --output <file>   Output file (default: input without extension)
  --arch x86-64|arm64   Target architecture (default: x86-64)
  --lang lisp|php       Source language (auto-detect from .php extension)
  --dump-ir <phase>     Dump IR for phase: ast, cps, ssa, vm, opt, asm
  --annotate-source     Add source-location comments when available
  --debug               Keep frame pointers for native compile debugging
  --retpoline           Enable retpoline mitigation for indirect branches
  --spectre-mitigations Enable x86-64 LFENCE + retpoline hardening
  --jit-cache-stats     Print JIT code-cache hit/eviction statistics
  --stack-protector     Enable stack canary checks in native prologue/epilogue
  --shadow-stack        Enable CET shadow-stack planning/integration path
  --compress            Add compressed code payload sections when supported
  --no-compress         Disable code payload compression (default)
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
 ")
    ("repl" . "Usage: cl-cc repl [options]

  Start an interactive ANSI Common Lisp REPL.
  Definitions persist across expressions within the session.

Options:
  --stdlib   Prepend standard library on startup
  --no-stdlib Disable lazy stdlib auto-require

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
  ")
    ("selfhost" . "Usage: cl-cc selfhost [options] [file]

  Run a self-hosting workload through the CL-CC VM.
  With --profile, write .cl-cc-profile.sexp as an instruction histogram used by later compilations.

Options:
  --profile         Collect VM instruction frequencies
  --stdlib          Eagerly prepend standard library
  --no-stdlib       Disable lazy stdlib auto-require
  --verbose         Show compilation details on stderr
  --timeout <sec>   Maximum execution time
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
    ("selfhost" . %do-selfhost))
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

    (let ((command (parsed-args-command parsed)))
      (cond
        ((null command)
         (%print-global-help)
         (uiop:quit 0))
        ((string= command "help")
         (%print-help (car (parsed-args-positional parsed)))
         (uiop:quit 0))
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
