;;;; tests/unit/cli/args-tests.lisp — CLI argument parser unit tests
;;;;
;;;; Covers: parse-args command detection, boolean flags, string flags
;;;;         (long form, short form, --key=value inline form), error cases,
;;;;         and combined multi-flag invocations.

(in-package :cl-cc/test)

(in-suite cl-cc-cli-serial-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Helpers
;;; ─────────────────────────────────────────────────────────────────────────

(defun %flags (parsed key)
  "Return value of KEY in the parsed-args flag hash-table."
  (gethash key (cl-cc/cli:parsed-args-flags parsed)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Command detection
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cli-args-empty-argv-yields-nil-command-and-positional
  "parse-args on empty argv returns nil command and nil positionals."
  (let ((p (cl-cc/cli:parse-args '())))
    (assert-null (cl-cc/cli:parsed-args-command p))
    (assert-null (cl-cc/cli:parsed-args-positional p))))

(deftest cli-args-command-only-has-no-positionals
  "parse-args on a lone command token has no positionals."
  (let ((p (cl-cc/cli:parse-args '("run"))))
    (assert-string= "run" (cl-cc/cli:parsed-args-command p))
    (assert-null (cl-cc/cli:parsed-args-positional p))))

(deftest cli-args-command-with-file-puts-file-in-positionals
  "parse-args on command+file stores the file in positionals."
  (let ((p (cl-cc/cli:parse-args '("run" "foo.lisp"))))
    (assert-string= "run" (cl-cc/cli:parsed-args-command p))
    (assert-equal '("foo.lisp") (cl-cc/cli:parsed-args-positional p))))

(deftest cli-args-multiple-positionals-after-command-all-collected
  "parse-args collects all tokens after the command as positionals."
  (let ((p (cl-cc/cli:parse-args '("eval" "(+ 1 2)" "extra"))))
    (assert-string= "eval" (cl-cc/cli:parsed-args-command p))
    (assert-equal '("(+ 1 2)" "extra") (cl-cc/cli:parsed-args-positional p))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Boolean flags
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cli-args-bool-flags
  "Boolean flags are stored as T when present"
  :cases (("stdlib"  '("run" "f.lisp" "--stdlib")  "--stdlib")
          ("verbose" '("run" "f.lisp" "--verbose") "--verbose")
          ("pass timings" '("eval" "(+ 1 2)" "--print-pass-timings") "--print-pass-timings")
          ("time-passes alias" '("eval" "(+ 1 2)" "--time-passes") "--time-passes")
          ("stats" '("eval" "(+ 1 2)" "--stats") "--stats")
          ("trace-emit" '("eval" "(+ 1 2)" "--trace-emit") "--trace-emit")
          ("strict"  '("check" "f.lisp" "--strict") "--strict")
          ("help"    '("--help")                   "--help"))
  (argv flag-key)
  (let ((p (cl-cc/cli:parse-args argv)))
    (assert-true (%flags p flag-key))))

(deftest cli-args-bool-flag-absent
  "An absent boolean flag returns NIL"
  (let ((p (cl-cc/cli:parse-args '("run" "f.lisp"))))
    (assert-null (%flags p "--stdlib"))
    (assert-null (%flags p "--verbose"))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; String flags — --key value form
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cli-args-output-long-and-short
  "parse-args: output flag in long and short form"
  :cases (("long form" '("compile" "f.lisp" "--output" "out")   "--output" "out")
          ("short form" '("compile" "f.lisp" "-o" "mybin")       "-o"       "mybin"))
  (argv flag-key expected)
  (let ((p (cl-cc/cli:parse-args argv)))
    (assert-string= expected (%flags p flag-key))))

(deftest-each cli-args-string-flags
  "parse-args: arch and lang string flags (--key value form)"
  :cases (("arch arm64"  '("compile" "f.lisp" "--arch" "arm64") "--arch" "arm64")
          ("lang php"    '("run" "f.php"  "--lang" "php")        "--lang" "php")
          ("lang lisp"   '("run" "f.lisp" "--lang" "lisp")       "--lang" "lisp")
          ("pass pipeline" '("eval" "(+ 1 2)" "--pass-pipeline" "fold,dce") "--pass-pipeline" "fold,dce")
          ("opt remarks" '("eval" "(+ 1 2)" "--opt-remarks" "changed") "--opt-remarks" "changed")
          ("trace json" '("eval" "(+ 1 2)" "--trace-json" "trace.json") "--trace-json" "trace.json")
          ("flamegraph" '("eval" "(+ 1 2)" "--flamegraph" "flame.svg") "--flamegraph" "flame.svg"))
  (argv flag-key expected)
  (let ((p (cl-cc/cli:parse-args argv)))
    (assert-string= expected (%flags p flag-key))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; String flags — --key=value inline form
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cli-args-equals-form
  "parse-args: inline --key=value form for output, arch, and lang"
  :cases (("output=mybin" '("compile" "f.lisp" "--output=mybin") "--output" "mybin")
          ("arch=arm64"   '("compile" "f.lisp" "--arch=arm64")   "--arch"   "arm64")
          ("lang=php"     '("run"     "f.php"  "--lang=php")     "--lang"   "php")
          ("pipeline=fold,dce" '("eval" "(+ 1 2)" "--pass-pipeline=fold,dce") "--pass-pipeline" "fold,dce")
          ("opt-remarks=missed" '("eval" "(+ 1 2)" "--opt-remarks=missed") "--opt-remarks" "missed")
          ("trace-json=trace.json" '("eval" "(+ 1 2)" "--trace-json=trace.json") "--trace-json" "trace.json")
          ("flamegraph=flame.svg" '("eval" "(+ 1 2)" "--flamegraph=flame.svg") "--flamegraph" "flame.svg"))
  (argv flag-key expected)
  (let ((p (cl-cc/cli:parse-args argv)))
    (assert-string= expected (%flags p flag-key))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; flag / flag-or accessor helpers
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cli-flag-helper
  "flag: returns value for a known flag, nil for absent flag"
  (let ((p (cl-cc/cli:parse-args '("compile" "f.lisp" "--arch" "x86-64"))))
    (assert-string= "x86-64" (cl-cc/cli:flag p "--arch"))
    (assert-null (cl-cc/cli:flag p "--output"))))

(deftest cli-flag-or-long-form-wins-when-present
  "flag-or returns the long-form value when the long flag is present."
  (let ((p (cl-cc/cli:parse-args '("compile" "f.lisp" "--output" "long-out"))))
    (assert-string= "long-out" (cl-cc/cli:flag-or p "--output" "-o"))))

(deftest cli-flag-or-falls-back-to-short-form-when-long-absent
  "flag-or returns the short-form value when only the short flag is present."
  (let ((p (cl-cc/cli:parse-args '("compile" "f.lisp" "-o" "short-out"))))
    (assert-string= "short-out" (cl-cc/cli:flag-or p "--output" "-o"))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Edge cases — special tokens
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cli-args-single-dash-is-positional
  "parse-args: a lone '-' is not a flag — it becomes the command"
  (let ((p (cl-cc/cli:parse-args '("-"))))
    (assert-string= "-" (cl-cc/cli:parsed-args-command p))
    (assert-null (cl-cc/cli:parsed-args-positional p))))

(deftest cli-args-empty-inline-value
  "parse-args: '--output=' stores the empty string as the value"
  (let ((p (cl-cc/cli:parse-args '("compile" "f.lisp" "--output="))))
    (assert-string= "" (%flags p "--output"))))

(deftest cli-args-duplicate-flag-last-wins
  "parse-args: when the same flag appears twice the last value is kept"
  (let ((p (cl-cc/cli:parse-args '("compile" "f.lisp" "--arch" "arm64"
                                    "--arch" "x86-64"))))
    (assert-string= "x86-64" (%flags p "--arch"))))

(deftest cli-args-value-resembles-flag
  "parse-args: a string value that starts with '--' is stored verbatim, not parsed as a flag"
  ;; '--output --verbose' — '--verbose' is the VALUE of --output, not a bool flag
  (let ((p (cl-cc/cli:parse-args '("compile" "f.lisp" "--output" "--verbose"))))
    (assert-string= "--verbose" (%flags p "--output"))
    (assert-null (%flags p "--verbose"))))

(deftest cli-args-flags-interleaved-with-positionals
  "parse-args: flags appearing after the file positional are handled correctly"
  ;; Common form: cl-cc compile file --arch arm64
  (let ((p (cl-cc/cli:parse-args '("compile" "foo.lisp" "--arch" "arm64"))))
    (assert-string= "compile" (cl-cc/cli:parsed-args-command p))
    (assert-equal '("foo.lisp") (cl-cc/cli:parsed-args-positional p))
    (assert-string= "arm64" (%flags p "--arch"))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Error cases
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cli-args-error-cases
  "Various invalid argv inputs signal arg-parse-error."
  :cases (("unknown-flag"         '("run" "--totally-unknown"))
          ("double-dash"          '("--"))
          ("bool-with-value"      '("run" "--stdlib=true"))
          ("triple-dash"          '("run" "---foo"))
          ("missing-output-long"  '("compile" "f.lisp" "--output"))
          ("missing-output-short" '("compile" "f.lisp" "-o")))
  (argv)
  (assert-signals cl-cc/cli:arg-parse-error (cl-cc/cli:parse-args argv)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Combined invocations
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cli-args-full-compile-invocation
  "parse-args correctly handles a full compile command with arch, output, and verbose flags."
  (let ((p (cl-cc/cli:parse-args
            '("compile" "foo.lisp" "--arch" "arm64" "--output=mybin" "--verbose"))))
    (assert-string= "compile" (cl-cc/cli:parsed-args-command p))
    (assert-equal  '("foo.lisp") (cl-cc/cli:parsed-args-positional p))
    (assert-string= "arm64"  (%flags p "--arch"))
    (assert-string= "mybin"  (%flags p "--output"))
    (assert-true             (%flags p "--verbose"))))

(deftest cli-args-run-with-php-flags
  "parse-args correctly handles a run command with PHP lang, stdlib, and verbose flags."
  (let ((p (cl-cc/cli:parse-args
            '("run" "script.php" "--lang=php" "--stdlib" "--verbose"))))
    (assert-string= "run" (cl-cc/cli:parsed-args-command p))
    (assert-equal  '("script.php") (cl-cc/cli:parsed-args-positional p))
    (assert-string= "php" (%flags p "--lang"))
    (assert-true           (%flags p "--stdlib"))
    (assert-true           (%flags p "--verbose"))))

(deftest cli-args-flags-before-command-are-parsed
  "parse-args correctly handles flags that appear before the command."
  (let ((p (cl-cc/cli:parse-args '("--verbose" "run" "foo.lisp"))))
    (assert-string= "run" (cl-cc/cli:parsed-args-command p))
    (assert-equal '("foo.lisp") (cl-cc/cli:parsed-args-positional p))
    (assert-true (%flags p "--verbose"))))
