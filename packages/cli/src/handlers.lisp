;;;; cli/src/handlers.lisp — CL-CC CLI Subcommand Handlers
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;;
;;; Contains subcommand implementations only:
;;;   run, compile, eval, repl, and check handlers plus REPL helpers.
;;;
;;; Top-level help and dispatch live in main.lisp.
;;; Shared argument/file/output helpers live in main-utils.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(in-package :cl-cc/cli)

;; `compile-opts` is defined in main-dump.lisp, which loads after this file.
;; Prevent premature inlining warnings for its accessors during compile-file.
(declaim (notinline compile-opts-flamegraph-path
                     compile-opts-profile
                      compile-opts-pgo-generate-path
                      compile-opts-pgo-use-path
                      compile-opts-block-compile
                      compile-opts-spectre-mitigations
                      compile-opts-jit-cache-stats
                      compile-opts-trace-json-path
                     compile-opts-trace-emit
                     compile-opts-retpoline
                     compile-opts-stack-protector
                     compile-opts-shadow-stack
                     compile-opts-asan
                     compile-opts-msan
                     compile-opts-tsan
                     compile-opts-ubsan
                     compile-opts-hwasan))

(defun %call-with-runtime-sanitizer-flags (opts thunk)
  "Execute THUNK with runtime sanitizer toggles derived from OPTS."
  (let ((cl-cc/runtime::*rt-asan-enabled* (not (null (compile-opts-asan opts))))
        (cl-cc/runtime::*rt-msan-enabled* (not (null (compile-opts-msan opts))))
        (cl-cc/runtime::*rt-tsan-enabled* (not (null (compile-opts-tsan opts))))
        (cl-cc/runtime::*rt-hwasan-enabled* (not (null (compile-opts-hwasan opts))))
        (cl-cc/runtime::*rt-ubsan-enabled* (not (null (compile-opts-ubsan opts)))))
    (funcall thunk)))

(defun %pgo-profile-instructions (result)
  "Return instruction list to profile from RESULT, preferring optimized stream."
  (or (cl-cc/compile:compilation-result-optimized-instructions result)
      (cl-cc/compile:compilation-result-vm-instructions result)
      (cl-cc/vm:vm-program-instructions (cl-cc/compile:compilation-result-program result))))

(defun %write-pgo-profile (path result &optional vm-state)
  "Write a lightweight PGO profile for RESULT to PATH.

RESULT supplies instruction streams and the optional counter plan. VM-STATE,
when provided, contributes runtime basic-block, branch, and counter counts.
The file is written as a readable plist-like form and PATH's parent directory
is created as needed."
  (let ((counts (make-hash-table :test #'equal))
         (insts (%pgo-profile-instructions result))
         (bb (and vm-state (cl-cc/vm:vm-get-profile-bb-counts vm-state)))
         (branches (and vm-state (cl-cc/vm:vm-get-profile-branch-counts vm-state)))
         (calls (and vm-state (cl-cc/vm:vm-get-profile-call-counts vm-state)))
         (type-feedback (and vm-state (cl-cc/vm:vm-get-profile-type-feedback vm-state)))
        (counter-plan (cl-cc/compile:compilation-result-pgo-counter-plan result))
        (counter-template nil)
        (bb-counter-counts nil)
        (edge-counter-counts nil))
    (when counter-plan
      (setf counter-template (cl-cc/optimize:opt-pgo-make-profile-template counter-plan))
      (setf bb-counter-counts
            (loop for (bb-id . pc) in (getf counter-plan :bb-runtime-keys)
                  collect (cons bb-id (if bb (gethash pc bb 0) 0))))
      (setf edge-counter-counts
            (loop for (edge-id . runtime-key) in (getf counter-plan :edge-runtime-keys)
                  collect (cons edge-id (if branches (gethash runtime-key branches 0) 0)))))
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
      (format out " )~%")
      (format out " :function-call-counts (~%")
      (when calls
        (maphash (lambda (k v)
                   (format out "   (~S . ~D)~%" k v))
                 calls))
      (format out " )~%")
      (format out " :type-feedback (~%")
      (when type-feedback
        (maphash (lambda (k v)
                   (format out "   (~S . ~D)~%" k v))
                 type-feedback))
      (format out " )~%")
      (when counter-plan
        (format out " :counter-plan ~S~%" counter-plan))
      (when counter-template
        (format out " :counter-template ~S~%" counter-template))
      (when counter-plan
        (format out " :bb-counter-counts ~S~%" bb-counter-counts))
      (when counter-plan
        (format out " :edge-counter-counts ~S~%" edge-counter-counts))
      (format out " )~%"))))

(defun %print-jit-cache-stats (&optional (stream *standard-output*))
  "Print runtime JIT code-cache statistics when requested by the CLI."
  (let ((stats (cl-cc/runtime:rt-code-cache-stats)))
    (format stream "JIT code cache: size=~D capacity=~D entries=~D hits=~D misses=~D hit-rate=~,2F%% evictions=~D~%"
            (getf stats :size)
            (getf stats :capacity)
            (getf stats :entries)
            (getf stats :hits)
            (getf stats :misses)
            (* 100.0 (getf stats :hit-rate))
            (getf stats :evictions))))

(defun %maybe-print-jit-cache-stats (opts)
  "Print JIT cache stats when --jit-cache-stats is set."
  (when (compile-opts-jit-cache-stats opts)
    (%print-jit-cache-stats)))

(defun %maybe-write-pgo-profile (opts result &optional vm-state)
  "Emit a profile file when --pgo-generate is set."
  (let ((path (compile-opts-pgo-generate-path opts)))
    (when path
      (%write-pgo-profile path result vm-state))))

(defun %write-selfhost-instruction-profile (&optional (path *selfhost-profile-path*))
  "Write the self-hosting VM instruction histogram to PATH."
  (cl-cc/vm:vm-write-instruction-profile path)
  (format *error-output* "; cl-cc selfhost: wrote VM instruction profile to ~A~%" path))

(defun print-profile (vm-state &optional (stream *standard-output*))
  "Print a simple collapsed-stack profile report for VM-STATE."
  (let ((samples (and vm-state (cl-cc/vm:vm-get-profile-samples vm-state))))
    (when samples
      (format stream "~&Profile samples:~%")
      (let ((rows nil))
        (maphash (lambda (stack count) (push (cons stack count) rows)) samples)
        (dolist (row (sort rows #'> :key #'cdr))
          (format stream "~D ~A~%" (cdr row) (car row)))))))

(defmacro %with-cli-error-handler (&body body)
  "Evaluate BODY and convert unhandled errors into CLI diagnostics.

Expands to a HANDLER-CASE around BODY. On ERROR, the condition message is
formatted through the optimizer diagnostic formatter, written to
*ERROR-OUTPUT*, and the process exits with status 1."
  `(handler-case
       (progn ,@body)
     (error (e)
       (format *error-output* "~A~%"
               (cl-cc/optimize:opt-format-diagnostic-reason
                "cli"
                "failed"
                (princ-to-string e)))
       (uiop:quit 1))))

(defun %first-line (text)
  "Return first line of TEXT, or empty string when TEXT is NIL."
  (let* ((s (or text ""))
         (newline-pos (position #\Newline s)))
    (if newline-pos
        (subseq s 0 newline-pos)
        s)))

(defun %source-line-at (source line-number)
  "Return LINE-NUMBER (1-based) from SOURCE, or first line when unavailable."
  (if (and (integerp line-number) (plusp line-number))
      (with-input-from-string (in (or source ""))
        (loop for idx from 1
              for line = (read-line in nil nil)
              while line
              when (= idx line-number) do (return line)
              finally (return (%first-line source))))
      (%first-line source)))

(defun %extract-line-column-from-location (location)
  "Parse LOCATION like file:line:column or file:line.

Returns two values: the parsed 1-based line number and optional column number.
Unreadable or absent fields return NIL rather than signaling."
  (when (stringp location)
    (let* ((last-colon (position #\: location :from-end t))
           (prev-colon (and last-colon
                            (position #\: location :from-end t :end last-colon))))
      (cond
        ((and prev-colon last-colon)
         (values (ignore-errors (parse-integer (subseq location (1+ prev-colon) last-colon)))
                 (ignore-errors (parse-integer (subseq location (1+ last-colon))))))
        (last-colon
         (values (ignore-errors (parse-integer (subseq location (1+ last-colon))))
                 nil))
        (t
         (values nil nil))))))

(defun %run-compiled-result (result vm-state opts)
  "Execute RESULT's program in VM-STATE under runtime options from OPTS.

When OPTS request trace emission or flamegraph output, write those artifacts
around the execution. Returns the value produced by RUN-COMPILED."
  (when (compile-opts-trace-emit opts)
    (%trace-emit-stages result *standard-output*))
  (%call-with-runtime-sanitizer-flags
   opts
         (lambda ()
      (prog1 (run-compiled (compilation-result-program result) :state vm-state)
        (%maybe-print-jit-cache-stats opts)
        (when (compile-opts-profile opts)
          (print-profile vm-state))
        (when (compile-opts-flamegraph-path opts)
         (%write-flamegraph-svg (compile-opts-flamegraph-path opts)
                                 (cl-cc/vm:vm-get-profile-samples vm-state)))))))

(defun %compile-lisp-with-auto-stdlib (source kwargs stdlib no-stdlib)
  "Compile Lisp SOURCE, lazily falling back to stdlib on first unresolved use.
--stdlib keeps the old eager behaviour; --no-stdlib disables the fallback."
  (cond
    (stdlib
     (apply #'cl-cc:compile-string-with-stdlib source :target :vm kwargs))
    (no-stdlib
     (apply #'compile-string source :target :vm kwargs))
    (t
     (handler-case
         (apply #'compile-string source :target :vm kwargs)
       (error ()
         (apply #'cl-cc:compile-string-with-stdlib source :target :vm kwargs))))))

(defun %do-run (parsed)
  "Handle the `cl-cc run' subcommand using PARSED command-line arguments.

Reads the required input file, detects Lisp or PHP mode, compiles to the VM,
executes the program, optionally emits trace/flamegraph/PGO artifacts, and
exits with status 0 on success."
  (let* ((file (%required-file-arg parsed "run"))
         (lang-flag (or (flag parsed "--lang") ""))
         (language (%detect-language file lang-flag))
         (stdlib (flag parsed "--stdlib"))
          (verbose (flag parsed "--verbose"))
          (timeout (%get-timeout parsed))
         (opts (%parse-compile-opts parsed))
          (source (%read-command-source file))
          (no-stdlib (flag parsed "--no-stdlib")))
    (when verbose
      (format *error-output* "; cl-cc run: ~A  lang=~A  stdlib=~A~%"
              file language (if stdlib "yes" "no")))
    (%with-cli-error-handler
      (%call-with-cli-timeout timeout
       (lambda ()
         (%call-with-optional-output-file
          (compile-opts-trace-json-path opts)
          (lambda (stream)
            (let* ((vm-state (%maybe-make-profiled-vm-state opts))
                   (kwargs (%compile-opts-kwargs opts stream)))
              (cond
                ((eq language :lisp)
                  (let ((result (%compile-lisp-with-auto-stdlib source kwargs stdlib no-stdlib)))
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
              (uiop:quit 0)))))
       "run"))))

(defun %default-selfhost-file ()
  "Return the default self-hosting workload file."
  (or (probe-file #p"example/selfhost.lisp")
      (probe-file #p"./example/selfhost.lisp")
      (error "Default selfhost workload not found: example/selfhost.lisp")))

(defun %do-selfhost (parsed)
  "Handle `cl-cc selfhost' and optionally emit VM instruction feedback."
  (let* ((file (or (car (parsed-args-positional parsed))
                   (namestring (%default-selfhost-file))))
         (stdlib (flag parsed "--stdlib"))
         (no-stdlib (flag parsed "--no-stdlib"))
         (verbose (flag parsed "--verbose"))
         (timeout (%get-timeout parsed))
         (opts (%parse-compile-opts parsed))
         (source (%read-command-source file)))
    (when verbose
      (format *error-output* "; cl-cc selfhost: ~A  profile=~A~%"
              file (if (compile-opts-profile opts) "yes" "no")))
    (%with-cli-error-handler
      (%call-with-cli-timeout
       timeout
       (lambda ()
         (%call-with-optional-output-file
          (compile-opts-trace-json-path opts)
          (lambda (stream)
            (let* ((vm-state (%maybe-make-profiled-vm-state opts))
                   (kwargs (%compile-opts-kwargs opts stream)))
              (when (compile-opts-profile opts)
                (cl-cc/vm:vm-reset-instruction-profile))
              (let ((ret (let ((cl-cc/vm:*vm-instruction-profile-enabled*
                                  (not (null (compile-opts-profile opts)))))
                           (let* ((result (%compile-lisp-with-auto-stdlib source kwargs stdlib no-stdlib))
                                  (value (%run-compiled-result result vm-state opts)))
                             (%maybe-write-pgo-profile opts result vm-state)
                             value))))
                (when (compile-opts-profile opts)
                  (%write-selfhost-instruction-profile))
                (format t "~S~%" ret)
                (uiop:quit 0))))))
       "selfhost"))))

(defun %do-compile (parsed)
  "Handle the `cl-cc compile' subcommand using PARSED arguments.

Reads the source file, applies target architecture and compile options, either
dumps the requested IR phase or writes a native binary, prints the output path,
and exits with status 0 on success."
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
          (compress (and (flag parsed "--compress")
                         (not (flag parsed "--no-compress"))))
          (timeout (%get-timeout parsed))
         (opts (%parse-compile-opts parsed)))
    (when verbose
      (format *error-output* "; cl-cc compile: ~A  arch=~A  output=~A~%"
              file arch-str (or output "(auto)")))
     (%with-cli-error-handler
       (%call-with-cli-timeout timeout
        (lambda ()
          (flet ((compile-source (source &rest kwargs)
                   (if (eq (or language :lisp) :lisp)
                       (apply #'compile-string source :source-file file :language :lisp kwargs)
                       (apply #'compile-string source :language (or language :lisp) kwargs))))
            (let ((cl-cc/codegen::*x86-64-omit-frame-pointer*
                    (if (or debug (compile-opts-stack-protector opts))
                        nil
                        cl-cc/codegen::*x86-64-omit-frame-pointer*))
                   (cl-cc/codegen::*x86-64-use-retpoline*
                    (if (or (compile-opts-retpoline opts)
                            (compile-opts-spectre-mitigations opts))
                        t
                        cl-cc/codegen::*x86-64-use-retpoline*))
                   (cl-cc/codegen::*x86-64-spectre-mitigations-enabled*
                    (if (compile-opts-spectre-mitigations opts)
                        t
                        cl-cc/codegen::*x86-64-spectre-mitigations-enabled*))
                  (cl-cc/codegen::*x86-64-stack-protector-enabled*
                    (if (compile-opts-stack-protector opts) t cl-cc/codegen::*x86-64-stack-protector-enabled*))
                  (cl-cc/codegen::*x86-64-shadow-stack-enabled*
                    (if (compile-opts-shadow-stack opts) t cl-cc/codegen::*x86-64-shadow-stack-enabled*))
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
                                            :compress compress
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
                       (uiop:quit 0))))))))
        "compile"))))

(defun %compile-and-run-eval-form (expr stdlib no-stdlib kwargs vm-state opts)
  "Compile and run EXPR for the eval command.
STDLIB selects whether the stdlib-aware compiler may be used. KWARGS are
forwarded to the compiler, VM-STATE is passed to RUN-COMPILED, and OPTS supply
runtime sanitizer flags. Returns two values: the evaluated result and the
compilation-result object used to produce it."
  (labels ((compile-and-run (compile-fn)
             (let* ((compiled (apply compile-fn expr :target :vm kwargs))
                    (result (%call-with-runtime-sanitizer-flags
                             opts
                             (lambda ()
                               (run-compiled (compilation-result-program compiled)
                                             :state vm-state)))))
                (values result compiled))))
    (cond
      (stdlib
       (compile-and-run #'cl-cc:compile-string-with-stdlib))
      (no-stdlib
       (compile-and-run #'compile-string))
      (t
       (handler-case
           (compile-and-run #'compile-string)
         (error ()
           (compile-and-run #'cl-cc:compile-string-with-stdlib)))))))

(defun %do-eval (parsed)
  "Handle the `cl-cc eval' subcommand using PARSED arguments.

Compiles and executes the single expression argument in the VM, prints the
result with READ syntax, optionally emits profiling artifacts, and exits with
status 0 on success or 2 when the expression is missing."
  (let ((expr (car (parsed-args-positional parsed))))
    (unless expr
      (format *error-output* "Error: 'eval' requires an expression argument.~%")
      (%print-help "eval")
      (uiop:quit 2))
    (let* ((stdlib (flag parsed "--stdlib"))
            (verbose (flag parsed "--verbose"))
            (no-stdlib (flag parsed "--no-stdlib"))
           (timeout (%get-timeout parsed))
           (opts (%parse-compile-opts parsed)))
      (when verbose
        (format *error-output* "; cl-cc eval: ~S~%" expr))
      (%with-cli-error-handler
        (%call-with-cli-timeout
         timeout
         (lambda ()
           (%call-with-optional-output-file
            (compile-opts-trace-json-path opts)
            (lambda (stream)
              (let* ((vm-state (%maybe-make-profiled-vm-state opts))
                     (kwargs (%compile-opts-kwargs opts stream))
                     (result nil)
                     (compiled nil))
                (multiple-value-setq (result compiled)
                   (%compile-and-run-eval-form expr stdlib no-stdlib kwargs vm-state opts))
                (%maybe-write-pgo-profile opts compiled vm-state)
                (when (compile-opts-profile opts)
                  (print-profile vm-state))
                (%maybe-print-jit-cache-stats opts)
                (when (compile-opts-trace-emit opts)
                  (%trace-emit-stages compiled *standard-output*))
                (when (compile-opts-flamegraph-path opts)
                  (%write-flamegraph-svg (compile-opts-flamegraph-path opts)
                                         (cl-cc/vm:vm-get-profile-samples vm-state)))
                (format t "~S~%" result)
                (uiop:quit 0)))))
          "eval")))))

(defun %count-parens (str)
  "Count top-level parentheses in STR while ignoring parentheses in strings.

Returns two values: the number of opening parentheses and the number of closing
parentheses observed outside string literals. Backslash escapes are honored
inside strings for REPL input balancing."
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
  "Handle the `cl-cc repl' subcommand using PARSED arguments.

Resets persistent REPL state, optionally preloads the standard library, reads
balanced forms from *STANDARD-INPUT*, evaluates them through RUN-STRING-REPL,
prints non-NIL results, and exits cleanly on EOF or quit commands."
  (let ((stdlib (flag parsed "--stdlib"))
        (no-stdlib (flag parsed "--no-stdlib"))
        (timeout (%get-timeout parsed)))
    (cl-cc:reset-repl-state)
    (when stdlib
      (handler-case (cl-cc:run-string-repl cl-cc:*standard-library-source*)
        (error () nil)))
    (format t "CL-CC ~A  —  ANSI Common Lisp~%" *version*)
    (format t "Type a CL form and press Return. (exit) or Ctrl+D to quit.~%~%")
    (force-output)
    (let ((stdlib-loaded-p stdlib))
      (flet ((eval-and-print (form)
             (let ((result (handler-case (cl-cc:run-string-repl form)
                             (error (e)
                               (if (or no-stdlib stdlib-loaded-p)
                                   (error e)
                                   (progn
                                     (cl-cc:run-string-repl cl-cc:*standard-library-source*)
                                     (setf stdlib-loaded-p t)
                                     (cl-cc:run-string-repl form)))))))
                (when (not (null result))
                  (format t "=> ~S~%" result))
                (force-output))))
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
              (multiple-value-bind (edited-line candidates edited-p)
                  (cl-cc:repl-edit-input-line line)
                (declare (ignore edited-p))
                (when candidates
                  (format t "; Completions: ~{~A~^ ~}~%" candidates)
                  (force-output))
                (setf line edited-line))
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
               (cl-cc:%repl-record-history trimmed)
               (handler-case
                   (if timeout
                       (handler-case
                           (sb-ext:with-timeout timeout
                             (eval-and-print trimmed))
                         (sb-ext:timeout (c)
                           (declare (ignore c))
                           (format t "; Timeout after ~A second~:P~%" timeout)
                           (force-output)))
                       (eval-and-print trimmed))
                 (error (e)
                   (format t "; Error: ~A~%" e)
                    (force-output))))))))))))

(defun %do-check (parsed)
  "Handle the `cl-cc check' subcommand using PARSED arguments.

Reads the required source file, runs type inference in warning or strict mode,
prints the inferred type when available, and reports structured diagnostics
with a source caret and type trace on failure."
  (let* ((file (%required-file-arg parsed "check"))
         (strict (flag parsed "--strict"))
         (verbose (flag parsed "--verbose"))
         (timeout (%get-timeout parsed))
         (mode (if strict :strict :warn))
         (source (%read-command-source file)))
    (when verbose
      (format *error-output* "; cl-cc check: ~A  mode=~A~%" file mode))
    (%call-with-cli-timeout timeout
     (lambda ()
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
           (let* ((message (princ-to-string e))
                  (location (and (typep e 'cl-cc/ast:ast-compilation-error)
                                 (cl-cc/ast:ast-error-location e)))
                  (line-number nil)
                  (column-number nil))
             (multiple-value-setq (line-number column-number)
               (%extract-line-column-from-location location))
             (let* ((line-text (%source-line-at source line-number))
                    (snippet (cl-cc/optimize:opt-build-diagnostic-caret-line
                              line-text
                              (or column-number 1)))
                    (trace (cl-cc/optimize:opt-format-type-trace
                            (list message))))
               (format *error-output* "~A~%"
                       (cl-cc/optimize:opt-format-diagnostic-reason
                        "type-check"
                        "failed"
                        message))
               (when location
                 (format *error-output* "at ~A~%" location))
               (format *error-output* "~A~%" snippet)
               (format *error-output* "~A~%" trace)))
           (uiop:quit 1))))
     "check")))
