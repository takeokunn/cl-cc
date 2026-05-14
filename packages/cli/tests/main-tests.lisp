;;;; tests/unit/cli/main-tests.lisp — CLI main help tests
(in-package :cl-cc/test)

(defsuite cl-cc-cli-serial-suite
  :description "Serial CLI unit tests that temporarily override process-wide quit hooks"
  :parent cl-cc-unit-suite
  :parallel nil)

(in-suite cl-cc-cli-serial-suite)

(defmacro %with-cli-function-overrides (bindings &body body)
  "Temporarily override global function bindings used by CLI unit tests."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (list ,@(mapcar (lambda (binding)
                                     `(cons ',(first binding)
                                            (symbol-function ',(first binding))))
                                   bindings))))
       (unwind-protect
            (progn
              ,@(mapcar (lambda (binding)
                          `(setf (symbol-function ',(first binding)) ,(second binding)))
                        bindings)
              ,@body)
         (dolist (entry ,saved)
           (setf (symbol-function (car entry)) (cdr entry)))))))

(defmacro %with-captured-quit (&body body)
  "Run BODY with uiop:quit modeled as a non-local exit carrying the exit code."
  `(%with-cli-function-overrides
       ((uiop:quit (lambda (&optional code)
                     (throw 'cl-cc-cli-quit code))))
     (catch 'cl-cc-cli-quit
       ,@body)))

(deftest-each cli-print-global-help
  "Global help text includes the command usage banner, shared flags, and version."
  :cases (("usage"       "Usage: cl-cc <command>")
          ("debug"       "--debug")
          ("opt-remarks" "--opt-remarks <mode>")
          ("time-passes" "--time-passes")
          ("trace-json"  "--trace-json <file>")
          ("flamegraph"  "--flamegraph <file>")
          ("stats"       "--stats")
          ("trace-emit"  "--trace-emit")
          ("version"     "Version: 0.1.0"))
  (expected-str)
  (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-global-help)))))
    (assert-true (search expected-str out))))

(deftest-each cli-print-command-help
  "Command help for 'run' includes shared flags and its own description."
  :cases (("usage"       "Usage: cl-cc run")
          ("opt-remarks" "--opt-remarks <mode>")
          ("time-passes" "--time-passes")
          ("trace-json"  "--trace-json <file>")
          ("flamegraph"  "--flamegraph <file>")
          ("stats"       "--stats")
          ("trace-emit"  "--trace-emit")
          ("stdlib"      "Prepend standard library"))
  (expected-str)
  (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "run")))))
    (assert-true (search expected-str out))))

(deftest cli-print-compile-help-includes-debug-flag
  "Compile help text documents the --debug frame-pointer opt-out flag."
  (let ((out (with-output-to-string (s)
               (let ((*standard-output* s)
                     (*error-output* s))
                 (cl-cc/cli::%print-command-help "compile")))))
    (assert-true (search "--debug" out))))

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

(deftest cli-main-shows-global-help-with-no-command
  "main prints global help and exits 0 when no command is supplied."
  (%with-cli-function-overrides
      ((uiop:command-line-arguments (lambda () '())))
    (let ((out (with-output-to-string (s)
                 (let ((*standard-output* s)
                       (*error-output* s))
                   (assert-equal 0 (%with-captured-quit (cl-cc/cli:main)))))))
      (assert-true (search "Usage: cl-cc <command>" out)))))

(deftest cli-main-top-level-help-uses-command-help
  "main dispatches top-level --help through %print-help and exits 0."
  (let ((printed-command :unset))
    (%with-cli-function-overrides
        ((uiop:command-line-arguments (lambda () '("run" "--help")))
         (cl-cc/cli::%print-help (lambda (&optional command) (setf printed-command command))))
      (assert-equal 0 (%with-captured-quit (cl-cc/cli:main)))
      (assert-string= "run" printed-command))))

(deftest cli-main-help-subcommand-prints-positional-help
  "main handles the explicit help subcommand and forwards its positional command."
  (let ((printed-command :unset))
    (%with-cli-function-overrides
        ((uiop:command-line-arguments (lambda () '("help" "compile")))
         (cl-cc/cli::%print-help (lambda (&optional command) (setf printed-command command))))
      (assert-equal 0 (%with-captured-quit (cl-cc/cli:main)))
      (assert-string= "compile" printed-command))))

(deftest cli-main-unknown-command-prints-error-and-exits-2
  "main reports unknown commands, prints global help, and exits 2."
  (let ((printed-help nil))
    (%with-cli-function-overrides
        ((uiop:command-line-arguments (lambda () '("bogus")))
         (cl-cc/cli::%print-global-help (lambda () (setf printed-help t))))
      (let ((err (with-output-to-string (s)
                   (let ((*standard-output* s)
                         (*error-output* s))
                     (assert-equal 2 (%with-captured-quit (cl-cc/cli:main)))))))
        (assert-true printed-help)
        (assert-true (search "Unknown command: bogus" err))))))

(deftest cli-main-parse-error-prints-message-and-exits-2
  "main catches arg-parse-error, prints the message and help, and exits 2."
  (let ((printed-help nil))
    (%with-cli-function-overrides
        ((uiop:command-line-arguments (lambda () '("--bad")))
         (cl-cc/cli:parse-args (lambda (argv)
                                 (declare (ignore argv))
                                 (error 'cl-cc/cli:arg-parse-error :message "boom")))
         (cl-cc/cli::%print-global-help (lambda () (setf printed-help t))))
      (let ((err (with-output-to-string (s)
                   (let ((*standard-output* s)
                         (*error-output* s))
                     (assert-equal 2 (%with-captured-quit (cl-cc/cli:main)))))))
        (assert-true printed-help)
        (assert-true (search "boom" err))))))
