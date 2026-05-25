(in-package :cl-cc/test)

(defsuite script-mode-suite
  :description "Runtime-stdlib-2 script-mode CLI scaffold tests"
  :parent cl-cc-unit-suite)

(in-suite script-mode-suite)

(deftest script-mode-cli-system-loads
  "The CLI system remains loadable with script-mode test scaffolding."
  :timeout 5
  (assert-true (asdf:find-system :cl-cc-cli nil)))

(deftest script-mode-parse-args-accepts-script-flag
  "FR-808: --script is accepted without a subcommand and preserves script argv."
  :timeout 10
  (let ((parsed (cl-cc/cli:parse-args '("--script" "tool.lisp" "a" "--" "--literal"))))
    (assert-true (cl-cc/cli:flag parsed "--script"))
    (assert-string= "tool.lisp" (cl-cc/cli:parsed-args-command parsed))
    (assert-equal '("a" "--literal") (cl-cc/cli:parsed-args-positional parsed))))

(deftest script-mode-getopt-separates-positionals
  "FR-809: GETOPT recognizes short/long flags and -- positional separator."
  :timeout 10
  (multiple-value-bind (opts positionals)
      (cl-cc/cli:getopt "my-script"
                         '((:verbose #\v "verbose" "Enable verbose output"))
                         '("-v" "--" "--not-a-flag"))
    (assert-true (getf opts :verbose))
    (assert-equal '("--not-a-flag") positionals)))

(deftest script-mode-source-wrapper-installs-argv-and-main-call
  "FR-808/809: script wrapper installs argv and calls CL-CC:MAIN when present."
  :timeout 10
  (let ((wrapped (cl-cc/cli::%source-with-script-bindings "(defun cl-cc:main (args) args)" '("x" "y"))))
    (assert-true (search "*SCRIPT-ARGV*" (string-upcase wrapped)))
    (assert-true (search "CL-CC:MAIN" (string-upcase wrapped)))))

(deftest script-mode-parse-args-accepts-reproducible-alias
  "FR-917: --reproducible is accepted as the public deterministic build flag."
  :timeout 10
  (let ((parsed (cl-cc/cli:parse-args '("compile" "tool.lisp" "--reproducible"))))
    (assert-true (cl-cc/cli:flag parsed "--reproducible"))
    (assert-true (cl-cc/cli::compile-opts-deterministic
                  (cl-cc/cli::%parse-compile-opts parsed)))))
