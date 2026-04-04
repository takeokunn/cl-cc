;;;; tests/unit/cli/main-tests.lisp — CLI main help tests
(in-package :cl-cc/test)
(in-suite cl-cc-suite)

(deftest cli-print-global-help
  "Global help text includes the command usage banner and version."
  (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-global-help)))))
    (assert-true (search "Usage: cl-cc <command>" out))
    (assert-true (search "--opt-remarks <mode>" out))
    (assert-true (search "Version: 0.1.0" out))))

(deftest cli-print-command-help
  "Command help text is available for representative commands."
  (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "run")))))
    (assert-true (search "Usage: cl-cc run" out))
    (assert-true (search "--opt-remarks <mode>" out))
    (assert-true (search "Prepend standard library" out))))

(deftest cli-print-unknown-command-falls-back
  "Unknown command help prints an error and falls back to global help."
  (let ((out (make-string-output-stream))
        (err (make-string-output-stream)))
    (let ((*standard-output* out)
          (*error-output* err))
      (cl-cc/cli::%print-command-help "not-a-command"))
    (let ((stdout (get-output-stream-string out))
          (stderr (get-output-stream-string err)))
      (assert-true (search "Usage: cl-cc <command>" stdout))
      (assert-true (search "Unknown command: not-a-command" stderr)))))
