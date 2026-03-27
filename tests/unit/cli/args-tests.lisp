;;;; tests/unit/cli/args-tests.lisp — CLI argument parser unit tests
;;;;
;;;; Covers: parse-args command detection, boolean flags, string flags
;;;;         (long form, short form, --key=value inline form), error cases,
;;;;         and combined multi-flag invocations.

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Helpers
;;; ─────────────────────────────────────────────────────────────────────────

(defun %flags (parsed key)
  "Return value of KEY in the parsed-args flag hash-table."
  (gethash key (cl-cc/cli:parsed-args-flags parsed)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Command detection
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cli-args-empty
  "parse-args with empty argv yields nil command and nil positional"
  (let ((p (cl-cc/cli:parse-args '())))
    (assert-null (cl-cc/cli:parsed-args-command p))
    (assert-null (cl-cc/cli:parsed-args-positional p))))

(deftest cli-args-command-only
  "parse-args with just a command stores it in :command"
  (let ((p (cl-cc/cli:parse-args '("run"))))
    (assert-string= "run" (cl-cc/cli:parsed-args-command p))
    (assert-null (cl-cc/cli:parsed-args-positional p))))

(deftest cli-args-command-and-file
  "parse-args: command followed by a file path"
  (let ((p (cl-cc/cli:parse-args '("run" "foo.lisp"))))
    (assert-string= "run" (cl-cc/cli:parsed-args-command p))
    (assert-equal '("foo.lisp") (cl-cc/cli:parsed-args-positional p))))

(deftest cli-args-command-multiple-positionals
  "parse-args: all tokens after the command become positionals"
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
          ("lang lisp"   '("run" "f.lisp" "--lang" "lisp")       "--lang" "lisp"))
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
          ("lang=php"     '("run"     "f.php"  "--lang=php")     "--lang"   "php"))
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

(deftest cli-flag-or-long-wins
  "flag-or: returns long form value when both long and short are present"
  (let ((p (cl-cc/cli:parse-args '("compile" "f.lisp" "--output" "long-out"))))
    (assert-string= "long-out" (cl-cc/cli:flag-or p "--output" "-o"))))

(deftest cli-flag-or-short-fallback
  "flag-or: falls back to short form when long form is absent"
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

(deftest cli-args-double-dash-signals-error
  "parse-args: '--' alone is an unknown flag, not a separator"
  (assert-signals cl-cc/cli:arg-parse-error
    (cl-cc/cli:parse-args '("--"))))

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

(deftest cli-args-unknown-flag
  "parse-args: unknown flag signals arg-parse-error"
  (assert-signals cl-cc/cli:arg-parse-error
    (cl-cc/cli:parse-args '("run" "--totally-unknown"))))

(deftest cli-args-missing-value-for-output
  "parse-args: --output without a value signals arg-parse-error"
  (assert-signals cl-cc/cli:arg-parse-error
    (cl-cc/cli:parse-args '("compile" "f.lisp" "--output"))))

(deftest cli-args-missing-value-for-short
  "parse-args: -o without a value signals arg-parse-error"
  (assert-signals cl-cc/cli:arg-parse-error
    (cl-cc/cli:parse-args '("compile" "f.lisp" "-o"))))

(deftest cli-args-bool-with-inline-value-errors
  "parse-args: --stdlib=true is invalid (bool flag takes no value)"
  (assert-signals cl-cc/cli:arg-parse-error
    (cl-cc/cli:parse-args '("run" "--stdlib=true"))))

(deftest cli-args-triple-dash-is-unknown
  "parse-args: '---foo' is not in the flag spec — signals arg-parse-error"
  (assert-signals cl-cc/cli:arg-parse-error
    (cl-cc/cli:parse-args '("run" "---foo"))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Combined invocations
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cli-args-combined-compile
  "parse-args: full compile invocation with multiple flags"
  (let ((p (cl-cc/cli:parse-args
            '("compile" "foo.lisp" "--arch" "arm64"
              "--output=mybin" "--verbose"))))
    (assert-string= "compile" (cl-cc/cli:parsed-args-command p))
    (assert-equal  '("foo.lisp") (cl-cc/cli:parsed-args-positional p))
    (assert-string= "arm64"  (%flags p "--arch"))
    (assert-string= "mybin"  (%flags p "--output"))
    (assert-true             (%flags p "--verbose"))))

(deftest cli-args-combined-run-php
  "parse-args: run with PHP language and stdlib flags"
  (let ((p (cl-cc/cli:parse-args
            '("run" "script.php" "--lang=php" "--stdlib" "--verbose"))))
    (assert-string= "run" (cl-cc/cli:parsed-args-command p))
    (assert-equal  '("script.php") (cl-cc/cli:parsed-args-positional p))
    (assert-string= "php" (%flags p "--lang"))
    (assert-true           (%flags p "--stdlib"))
    (assert-true           (%flags p "--verbose"))))

(deftest cli-args-flags-before-command
  "parse-args: flags that appear before the command are handled correctly"
  (let ((p (cl-cc/cli:parse-args '("--verbose" "run" "foo.lisp"))))
    (assert-string= "run" (cl-cc/cli:parsed-args-command p))
    (assert-equal '("foo.lisp") (cl-cc/cli:parsed-args-positional p))
    (assert-true (%flags p "--verbose"))))
