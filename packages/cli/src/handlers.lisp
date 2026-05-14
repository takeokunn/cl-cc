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
                    compile-opts-pgo-generate-path
                    compile-opts-pgo-use-path
                    compile-opts-trace-json-path
                    compile-opts-trace-emit))

(defun %pgo-profile-instructions (result)
  "Return instruction list to profile from RESULT, preferring optimized stream."
  (or (cl-cc/compile:compilation-result-optimized-instructions result)
      (cl-cc/compile:compilation-result-vm-instructions result)
      (cl-cc/vm:vm-program-instructions (cl-cc/compile:compilation-result-program result))))

(defun %write-pgo-profile (path result &optional vm-state)
  "Write a lightweight opcode-frequency profile for RESULT to PATH."
  (let ((counts (make-hash-table :test #'equal))
        (insts (%pgo-profile-instructions result))
        (bb (and vm-state (cl-cc/vm:vm-get-profile-bb-counts vm-state)))
        (branches (and vm-state (cl-cc/vm:vm-get-profile-branch-counts vm-state))))
    (dolist (inst insts)
      (let ((op (string-upcase (symbol-name (type-of inst)))))
        (incf (gethash op counts 0))))
    (ensure-directories-exist path)
    (with-open-file (out path
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format out "(:format :cl-cc-pgo-v1~%")
      (format out " :total-instructions ~D~%" (length insts))
      (format out " :op-counts (~%")
      (maphash (lambda (k v)
                 (format out "   (~S . ~D)~%" k v))
               counts)
      (format out " )~%")
      (format out " :bb-counts (~%")
      (when bb
        (maphash (lambda (k v)
                   (format out "   (~S . ~D)~%" k v))
                 bb))
      (format out " )~%")
      (format out " :branch-counts (~%")
      (when branches
        (maphash (lambda (k v)
                   (format out "   (~S . ~D)~%" k v))
                 branches))
      (format out " ))~%"))))

(defun %maybe-write-pgo-profile (opts result &optional vm-state)
  "Emit a profile file when --pgo-generate is set."
  (let ((path (compile-opts-pgo-generate-path opts)))
    (when path
      (%write-pgo-profile path result vm-state))))

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
                             (cl-cc/vm:vm-get-profile-samples vm-state)))))

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
               (let ((result (apply #'cl-cc:compile-string-with-stdlib source :target :vm kwargs)))
                 (let ((ret (%run-compiled-result result vm-state opts)))
                   (%maybe-write-pgo-profile opts result vm-state)
                   ret)))
             ((eq language :php)
               (let ((result (apply #'compile-string source :target :vm :language :php kwargs)))
                 (let ((ret (%run-compiled-result result vm-state opts)))
                   (%maybe-write-pgo-profile opts result vm-state)
                   ret)))
              (t
               (let ((result (apply #'compile-string source :target :vm kwargs)))
                 (let ((ret (%run-compiled-result result vm-state opts)))
                   (%maybe-write-pgo-profile opts result vm-state)
                   ret))))
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
         (debug (flag parsed "--debug"))
         (annotate (flag parsed "--annotate-source"))
         (verbose (flag parsed "--verbose"))
          (opts (%parse-compile-opts parsed)))
    (when verbose
      (format *error-output* "; cl-cc compile: ~A  arch=~A  output=~A~%"
              file arch-str (or output "(auto)")))
     (%with-cli-error-handler
       (flet ((compile-source (source &rest kwargs)
                (if (eq (or language :lisp) :lisp)
                    (apply #'compile-string source :source-file file :language :lisp kwargs)
                    (apply #'compile-string source :language (or language :lisp) kwargs))))
       (let ((cl-cc/codegen::*x86-64-omit-frame-pointer*
               (if debug nil cl-cc/codegen::*x86-64-omit-frame-pointer*))
             (cl-cc/codegen::*a64-omit-frame-pointer*
               (if debug nil cl-cc/codegen::*a64-omit-frame-pointer*)))
         (if dump-ir
             (let ((phase (%parse-ir-phase dump-ir)))
              (unless phase
                (format *error-output* "Error: unknown IR phase ~A~%" dump-ir)
                (uiop:quit 2))
               (let* ((source (%read-command-source file))
                      (result (apply #'compile-source source
                                     :target (%compile-target-keyword arch-str)
                                     (%compile-opts-kwargs opts nil))))
                 (%dump-ir-phase phase result *standard-output* annotate)
                 (uiop:quit 0)))
             (%call-with-optional-output-file
              (compile-opts-trace-json-path opts)
              (lambda (stream)
                (let* ((source (%read-command-source file))
                       (kwargs (%compile-opts-kwargs opts stream))
                       (trace-result (when (compile-opts-trace-emit opts)
                                       (apply #'compile-source source
                                              :target (%compile-target-keyword arch-str)
                                              kwargs)))
                       (result (apply #'compile-file-to-native file
                                       :arch arch
                                       :output-file output
                                      :language language
                                      kwargs)))
                   (when (and (compile-opts-pgo-generate-path opts)
                              (null trace-result))
                     (setf trace-result
                           (apply #'compile-source source
                                  :target (%compile-target-keyword arch-str)
                                  kwargs)))
                   (when trace-result
                     (%maybe-write-pgo-profile opts trace-result))
                   (when trace-result
                     (%trace-emit-stages trace-result *standard-output* :annotate-source annotate))
                   (format t "~A~%" result)
                   (uiop:quit 0))))))))))

(defun %compile-and-run-eval-form (expr stdlib kwargs vm-state)
  "Compile and run EXPR for the eval command.
When --stdlib is requested, try the ordinary fast path first so core CL forms
do not pay the full stdlib cold-compile cost. Fall back to the full stdlib path
only when the fast path cannot compile or run the form."
  (labels ((compile-and-run (compile-fn)
             (let* ((compiled (apply compile-fn expr :target :vm kwargs))
                    (result (run-compiled (compilation-result-program compiled)
                                          :state vm-state)))
               (values result compiled))))
    (if stdlib
        (handler-case
            (compile-and-run #'compile-string)
          (error ()
            (compile-and-run #'cl-cc:compile-string-with-stdlib)))
        (compile-and-run #'compile-string))))

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
                   (result nil)
                   (compiled nil))
               (multiple-value-setq (result compiled)
                 (%compile-and-run-eval-form expr stdlib kwargs vm-state))
               (%maybe-write-pgo-profile opts compiled vm-state)
               (when (compile-opts-trace-emit opts)
                 (%trace-emit-stages compiled *standard-output*))
              (when (compile-opts-flamegraph-path opts)
                (%write-flamegraph-svg (compile-opts-flamegraph-path opts)
                                       (cl-cc/vm:vm-get-profile-samples vm-state)))
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
      (handler-case (cl-cc:run-string-repl cl-cc:*standard-library-source*)
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
