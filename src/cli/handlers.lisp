;;;; src/cli/handlers.lisp — CL-CC CLI Subcommand Handlers
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;
;;; Contains: %do-run, %do-compile, %do-eval, %count-parens, %do-repl,
;;; %do-check, *cli-command-dispatch*, main.
;;;
;;; Help text, argument parsing helpers, and utility functions are in
;;; main.lisp (loads before this file).
;;;
;;; Load order: after main.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(in-package :cl-cc/cli)

(defun %do-run (parsed)
  (let ((file (car (parsed-args-positional parsed))))
    (unless file
      (format *error-output* "Error: 'run' requires a file argument.~%")
      (%print-help "run")
      (uiop:quit 2))
    (let* ((lang-flag (or (flag parsed "--lang") ""))
           (language  (%detect-language file lang-flag))
           (stdlib    (flag parsed "--stdlib"))
           (verbose   (flag parsed "--verbose"))
           (opts      (%parse-compile-opts parsed))
           (source    (handler-case (%read-file file)
                        (error (e)
                          (format *error-output* "Error reading ~A: ~A~%" file e)
                          (uiop:quit 1)))))
      (when verbose
        (format *error-output* "; cl-cc run: ~A  lang=~A  stdlib=~A~%"
                file language (if stdlib "yes" "no")))
      (flet ((run-result (result vm-state)
               (when (compile-opts-trace-emit opts)
                 (%trace-emit-stages result *standard-output*))
               (prog1 (run-compiled (compilation-result-program result) :state vm-state)
                 (when (compile-opts-flamegraph-path opts)
                   (%write-flamegraph-svg (compile-opts-flamegraph-path opts)
                                          (cl-cc::vm-profile-samples vm-state))))))
        (handler-case
            (%call-with-optional-output-file (compile-opts-trace-json-path opts)
              (lambda (stream)
                (let ((vm-state (when (compile-opts-flamegraph-path opts)
                                  (make-instance 'cl-cc::vm-io-state :output-stream *standard-output*))))
                  (when vm-state
                    (setf (cl-cc::vm-profile-enabled-p vm-state) t
                          (cl-cc::vm-profile-call-stack vm-state) (list "<toplevel>")))
                  (let ((kwargs (%compile-opts-kwargs opts stream)))
                    (cond
                      ((and stdlib (eq language :lisp))
                       (run-result (apply #'cl-cc::compile-string-with-stdlib source :target :vm kwargs)
                                   vm-state))
                      ((eq language :php)
                       (run-result (apply #'compile-string source :target :vm :language :php kwargs)
                                   vm-state))
                      (t
                       (run-result (apply #'compile-string source :target :vm kwargs)
                                   vm-state))))
                  (uiop:quit 0))))
          (error (e)
            (format *error-output* "Error: ~A~%" e)
            (uiop:quit 1)))))))

(defun %do-compile (parsed)
  (let ((file (car (parsed-args-positional parsed))))
    (unless file
      (format *error-output* "Error: 'compile' requires a file argument.~%")
      (%print-help "compile")
      (uiop:quit 2))
    (let* ((arch-str  (or (flag parsed "--arch") "x86-64"))
           (arch      (%arch-keyword arch-str))
           (output    (flag-or parsed "--output" "-o"))
           (lang-flag (or (flag parsed "--lang") ""))
           (language  (let ((l (%detect-language file lang-flag)))
                        (if (string= lang-flag "") nil l)))
           (dump-ir   (flag parsed "--dump-ir"))
           (annotate  (flag parsed "--annotate-source"))
           (verbose   (flag parsed "--verbose"))
           (opts      (%parse-compile-opts parsed)))
      (when verbose
        (format *error-output* "; cl-cc compile: ~A  arch=~A  output=~A~%"
                file arch-str (or output "(auto)")))
      (handler-case
          (if dump-ir
              (let ((phase (%parse-ir-phase dump-ir)))
                (unless phase
                  (format *error-output* "Error: unknown IR phase ~A~%" dump-ir)
                  (uiop:quit 2))
                (let* ((source (%read-file file))
                       (result (apply #'compile-string source
                                      :target (%compile-target-keyword arch-str)
                                      :language (or language :lisp)
                                      (%compile-opts-kwargs opts nil))))
                  (%dump-ir-phase phase result *standard-output* annotate)
                  (uiop:quit 0)))
              (%call-with-optional-output-file (compile-opts-trace-json-path opts)
                (lambda (stream)
                  (let* ((source (%read-file file))
                         (kwargs (%compile-opts-kwargs opts stream))
                         (trace-result (when (compile-opts-trace-emit opts)
                                         (apply #'compile-string source
                                                :target (%compile-target-keyword arch-str)
                                                :language (or language :lisp)
                                                kwargs)))
                         (result (apply #'compile-file-to-native file
                                        :arch arch
                                        :output-file output
                                        :language language
                                        kwargs)))
                    (when trace-result
                      (%trace-emit-stages trace-result *standard-output* :annotate-source annotate))
                    (format t "~A~%" result)
                    (uiop:quit 0)))))
        (error (e)
          (format *error-output* "Error: ~A~%" e)
          (uiop:quit 1))))))

(defun %do-eval (parsed)
  (let ((expr (car (parsed-args-positional parsed))))
    (unless expr
      (format *error-output* "Error: 'eval' requires an expression argument.~%")
      (%print-help "eval")
      (uiop:quit 2))
    (let* ((stdlib  (flag parsed "--stdlib"))
           (verbose (flag parsed "--verbose"))
           (opts    (%parse-compile-opts parsed)))
      (when verbose
        (format *error-output* "; cl-cc eval: ~S~%" expr))
      (handler-case
          (%call-with-optional-output-file (compile-opts-trace-json-path opts)
            (lambda (stream)
              (let* ((vm-state (when (compile-opts-flamegraph-path opts)
                                 (make-instance 'cl-cc::vm-io-state :output-stream *standard-output*)))
                     (kwargs   (%compile-opts-kwargs opts stream)))
                (when vm-state
                  (setf (cl-cc::vm-profile-enabled-p vm-state) t
                        (cl-cc::vm-profile-call-stack vm-state) (list "<toplevel>")))
                (let* ((compiled (if stdlib
                                     (apply #'cl-cc::compile-string-with-stdlib expr :target :vm kwargs)
                                     (apply #'compile-string expr :target :vm kwargs)))
                       (result   (run-compiled (compilation-result-program compiled) :state vm-state)))
                  (when (compile-opts-trace-emit opts)
                    (%trace-emit-stages compiled *standard-output*))
                  (when (compile-opts-flamegraph-path opts)
                    (%write-flamegraph-svg (compile-opts-flamegraph-path opts)
                                           (cl-cc::vm-profile-samples vm-state)))
                  (format t "~S~%" result)
                  (uiop:quit 0)))))
        (error (e)
          (format *error-output* "Error: ~A~%" e)
          (uiop:quit 1))))))

(defun %count-parens (str)
  "Return (values open close) paren counts in STR."
  (let ((open 0) (close 0) (in-string nil) (escaped nil))
    (loop for c across str do
      (cond
        (escaped (setf escaped nil))
        ((and (not in-string) (char= c #\\)) (setf escaped t))
        ((char= c #\") (setf in-string (not in-string)))
        ((not in-string)
         (cond ((char= c #\() (incf open))
               ((char= c #\)) (incf close))))))
    (values open close)))

(defun %do-repl (parsed)
  "Start the interactive CL-CC REPL."
  (let ((stdlib (flag parsed "--stdlib")))
    ;; Reset any leftover state from a previous session
    (cl-cc:reset-repl-state)
    ;; Optionally prime the REPL with the standard library
    (when stdlib
      (handler-case (cl-cc:run-string-repl cl-cc::*standard-library-source*)
        (error () nil)))
    (format t "CL-CC ~A  —  ANSI Common Lisp~%" *version*)
    (format t "Type a CL form and press Return. (exit) or Ctrl+D to quit.~%~%")
    (force-output)
    (loop
      (format t "* ")
      (force-output)
      ;; Read lines until parentheses are balanced
      (let ((buffer ""))
        (loop
          (let ((line (handler-case (read-line *standard-input* nil nil)
                        (error () nil))))
            (when (null line)
              (format t "~%Goodbye.~%")
              (uiop:quit 0))
            (setf buffer (if (string= buffer "")
                             line
                             (concatenate 'string buffer " " line)))
            (multiple-value-bind (open close) (%count-parens buffer)
              (when (>= close open)
                (return)))))
        ;; Process the accumulated buffer
        (let ((trimmed (string-trim '(#\Space #\Tab #\Newline) buffer)))
          (cond
            ((string= trimmed "") nil)
            ((or (string= trimmed "(exit)")
                 (string= trimmed ":quit")
                 (string= trimmed ":q"))
             (format t "Goodbye.~%")
             (uiop:quit 0))
            (t
             (handler-case
                 (let ((result (cl-cc:run-string-repl trimmed)))
                   (when (not (null result))
                     (format t "=> ~S~%" result))
                   (force-output))
               (error (e)
                 (format t "; Error: ~A~%" e)
                 (force-output))))))))))

(defun %do-check (parsed)
  (let ((file (car (parsed-args-positional parsed))))
    (unless file
      (format *error-output* "Error: 'check' requires a file argument.~%")
      (%print-help "check")
      (uiop:quit 2))
    (let* ((strict  (flag parsed "--strict"))
           (verbose (flag parsed "--verbose"))
           (mode    (if strict :strict :warn))
           (source  (handler-case (%read-file file)
                      (error (e)
                        (format *error-output* "Error reading ~A: ~A~%" file e)
                        (uiop:quit 1)))))
      (when verbose
        (format *error-output* "; cl-cc check: ~A  mode=~A~%" file mode))
      (handler-case
          (multiple-value-bind (result inferred-type)
              (run-string-typed source :mode mode)
            (declare (ignore result))
            (if inferred-type
                (format t "~A~%" (type-to-string inferred-type))
                (format t "<no type inferred>~%"))
            (uiop:quit 0))
        (error (e)
          (format *error-output* "Type error: ~A~%" e)
          (uiop:quit 1))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Main dispatcher
;;; (Selfhost verification is in selfhost.lisp, which loads before this file.)
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
    ;; Top-level --help / -h overrides everything
    (when (or (flag parsed "--help") (flag parsed "-h"))
      (%print-help (or (parsed-args-command parsed)
                       (car (parsed-args-positional parsed))))
      (uiop:quit 0))

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
               (funcall (cdr entry) parsed)
               (progn
                 (format *error-output* "Unknown command: ~A~%~%" command)
                 (%print-global-help)
                 (uiop:quit 2)))))))))
