;;;; cli/src/handlers.lisp — CL-CC CLI Subcommand Handlers
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;
;;; Contains subcommand implementations only:
;;;   %do-run, %do-compile, %do-eval, %count-parens, %do-repl, %do-check.
;;;
;;; Top-level help and dispatch live in main.lisp.
;;; Shared argument/file/output helpers live in main-utils.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(in-package :cl-cc/cli)

;; `compile-opts` is defined in main-dump.lisp, which loads after this file.
;; Prevent premature inlining warnings for its accessors during compile-file.
(declaim (notinline compile-opts-flamegraph-path
                    compile-opts-trace-json-path
                    compile-opts-trace-emit))

(defmacro %with-cli-error-handler (&body body)
  "Wrap BODY; on any error print to *error-output* and exit with status 1."
  `(handler-case
       (progn ,@body)
     (error (e)
       (format *error-output* "Error: ~A~%" e)
       (uiop:quit 1))))

(defun %run-compiled-result (result vm-state opts)
  "Execute RESULT's program in VM-STATE, emitting trace/flamegraph when opts request it."
  (when (compile-opts-trace-emit opts)
    (%trace-emit-stages result *standard-output*))
  (prog1 (run-compiled (compilation-result-program result) :state vm-state)
    (when (compile-opts-flamegraph-path opts)
      (%write-flamegraph-svg (compile-opts-flamegraph-path opts)
                             (cl-cc/vm::%vm-profile-samples vm-state)))))

(defun %do-run (parsed)
  (let* ((file (%required-file-arg parsed "run"))
         (lang-flag (or (flag parsed "--lang") ""))
         (language (%detect-language file lang-flag))
         (stdlib (flag parsed "--stdlib"))
         (verbose (flag parsed "--verbose"))
         (opts (%parse-compile-opts parsed))
         (source (%read-command-source file)))
    (when verbose
      (format *error-output* "; cl-cc run: ~A  lang=~A  stdlib=~A~%"
              file language (if stdlib "yes" "no")))
    (%with-cli-error-handler
      (%call-with-optional-output-file
       (compile-opts-trace-json-path opts)
       (lambda (stream)
         (let* ((vm-state (%maybe-make-profiled-vm-state opts))
                (kwargs (%compile-opts-kwargs opts stream)))
           (cond
             ((and stdlib (eq language :lisp))
              (%run-compiled-result (apply #'cl-cc::compile-string-with-stdlib source :target :vm kwargs)
                                    vm-state opts))
             ((eq language :php)
              (%run-compiled-result (apply #'compile-string source :target :vm :language :php kwargs)
                                    vm-state opts))
             (t
              (%run-compiled-result (apply #'compile-string source :target :vm kwargs)
                                    vm-state opts)))
           (uiop:quit 0)))))))

(defun %do-compile (parsed)
  (let* ((file (%required-file-arg parsed "compile"))
         (arch-str (or (flag parsed "--arch") "x86-64"))
         (arch (%arch-keyword arch-str))
         (output (flag-or parsed "--output" "-o"))
         (lang-flag (or (flag parsed "--lang") ""))
         (language (let ((l (%detect-language file lang-flag)))
                     (if (string= lang-flag "") nil l)))
         (dump-ir (flag parsed "--dump-ir"))
         (annotate (flag parsed "--annotate-source"))
         (verbose (flag parsed "--verbose"))
         (opts (%parse-compile-opts parsed)))
    (when verbose
      (format *error-output* "; cl-cc compile: ~A  arch=~A  output=~A~%"
              file arch-str (or output "(auto)")))
    (%with-cli-error-handler
      (if dump-ir
          (let ((phase (%parse-ir-phase dump-ir)))
            (unless phase
              (format *error-output* "Error: unknown IR phase ~A~%" dump-ir)
              (uiop:quit 2))
            (let* ((source (%read-command-source file))
                   (result (apply #'compile-string source
                                  :target (%compile-target-keyword arch-str)
                                  :language (or language :lisp)
                                  (%compile-opts-kwargs opts nil))))
              (%dump-ir-phase phase result *standard-output* annotate)
              (uiop:quit 0)))
          (%call-with-optional-output-file
           (compile-opts-trace-json-path opts)
           (lambda (stream)
             (let* ((source (%read-command-source file))
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
               (uiop:quit 0))))))))

(defun %do-eval (parsed)
  (let ((expr (car (parsed-args-positional parsed))))
    (unless expr
      (format *error-output* "Error: 'eval' requires an expression argument.~%")
      (%print-help "eval")
      (uiop:quit 2))
    (let* ((stdlib (flag parsed "--stdlib"))
           (verbose (flag parsed "--verbose"))
           (opts (%parse-compile-opts parsed)))
      (when verbose
        (format *error-output* "; cl-cc eval: ~S~%" expr))
      (%with-cli-error-handler
        (%call-with-optional-output-file
         (compile-opts-trace-json-path opts)
         (lambda (stream)
           (let* ((vm-state (%maybe-make-profiled-vm-state opts))
                  (kwargs (%compile-opts-kwargs opts stream))
                  (compiled (if stdlib
                                (apply #'cl-cc::compile-string-with-stdlib expr :target :vm kwargs)
                                (apply #'compile-string expr :target :vm kwargs)))
                  (result (run-compiled (compilation-result-program compiled) :state vm-state)))
             (when (compile-opts-trace-emit opts)
               (%trace-emit-stages compiled *standard-output*))
             (when (compile-opts-flamegraph-path opts)
               (%write-flamegraph-svg (compile-opts-flamegraph-path opts)
                                      (cl-cc/vm::vm-profile-samples vm-state)))
             (format t "~S~%" result)
             (uiop:quit 0))))))))

(defun %count-parens (str)
  "Return (values open close) paren counts in STR."
  (let ((open 0)
        (close 0)
        (in-string nil)
        (escaped nil))
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
    (cl-cc:reset-repl-state)
    (when stdlib
      (handler-case (cl-cc:run-string-repl cl-cc::*standard-library-source*)
        (error () nil)))
    (format t "CL-CC ~A  —  ANSI Common Lisp~%" *version*)
    (format t "Type a CL form and press Return. (exit) or Ctrl+D to quit.~%~%")
    (force-output)
    (loop
      (format t "* ")
      (force-output)
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
  (let* ((file (%required-file-arg parsed "check"))
         (strict (flag parsed "--strict"))
         (verbose (flag parsed "--verbose"))
         (mode (if strict :strict :warn))
         (source (%read-command-source file)))
    (when verbose
      (format *error-output* "; cl-cc check: ~A  mode=~A~%" file mode))
    (handler-case
        (progn
          (multiple-value-bind (result inferred-type)
              (run-string-typed source :mode mode)
            (declare (ignore result))
            (if inferred-type
                (format t "~A~%" (type-to-string inferred-type))
                (format t "<no type inferred>~%")))
          (uiop:quit 0))
      (error (e)
        (format *error-output* "Type error: ~A~%" e)
        (uiop:quit 1)))))
