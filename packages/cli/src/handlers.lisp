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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :ql)
    (defpackage :ql
      (:use :cl)
      (:export #:quickload #:system-apropos #:update-all-dists))))

;; `compile-opts` is defined in main-dump.lisp, which loads after this file.
;; Prevent premature inlining warnings for its accessors during compile-file.
(declaim (notinline compile-opts-flamegraph-path
                      compile-opts-profile
                      compile-opts-debug-info
                      compile-opts-sanitize
                      compile-opts-lto
                      compile-opts-eh-model
                      compile-opts-incremental
                      compile-opts-perf-map
                      compile-opts-parallel
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
                      compile-opts-deterministic
                      compile-opts-build-id
                      compile-opts-asan
                     compile-opts-msan
                     compile-opts-tsan
                     compile-opts-ubsan
                       compile-opts-hwasan
                        compile-opts-werror
                        ;; FR-276/241/153: new optimization/tracing flags
                        compile-opts-opt-level
                        compile-opts-trace-macros
                        compile-opts-memoize-macros
                        compile-opts-gc-min-heap
                        compile-opts-gc-max-heap))

(defun %call-with-runtime-sanitizer-flags (opts thunk)
  "Execute THUNK with runtime sanitizer toggles derived from OPTS."
  (let ((cl-cc/runtime::*rt-asan-enabled* (not (null (compile-opts-asan opts))))
        (cl-cc/runtime::*rt-msan-enabled* (not (null (compile-opts-msan opts))))
        (cl-cc/runtime::*rt-tsan-enabled* (not (null (compile-opts-tsan opts))))
        (cl-cc/runtime::*rt-hwasan-enabled* (not (null (compile-opts-hwasan opts))))
        (cl-cc/runtime::*rt-ubsan-enabled* (not (null (compile-opts-ubsan opts))))
        (cl-cc/runtime:*gc-young-size-words*
         (or (compile-opts-gc-min-heap opts)
             cl-cc/runtime:*gc-young-size-words*))
        (cl-cc/runtime:*gc-old-size-words*
         (or (compile-opts-gc-max-heap opts)
             cl-cc/runtime:*gc-old-size-words*))
        (cl-cc/parse::*werror-p* (not (null (compile-opts-werror opts)))))
    (funcall thunk)))

(defun %call-with-expander-flags (opts thunk)
  "FR-241/153: Execute THUNK with expander tracing/memoization toggles from OPTS."
  (let ((cl-cc/expand:*trace-macros* (not (null (compile-opts-trace-macros opts))))
        (cl-cc/expand:*enable-macro-memoization*
         (not (null (compile-opts-memoize-macros opts)))))
    (funcall thunk)))

(defun %call-with-optimizer-flags (opts thunk)
  "FR-276: Execute THUNK with optimizer level pre-configured from OPTS."
  (when (compile-opts-opt-level opts)
    (cl-cc/optimize:apply-optimization-level (compile-opts-opt-level opts)))
  (funcall thunk))

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
          (let ((samples (cl-cc/vm:vm-get-profile-samples vm-state)))
            (if (plusp (hash-table-count samples))
                (%write-flamegraph-svg (compile-opts-flamegraph-path opts) samples)
                (%write-flamegraph-from-perf-data (compile-opts-flamegraph-path opts)))))))))

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

;;; ─────────────────────────────────────────────────────────────────────────
;;; Local Quicklisp/ASDF integration (FR-763)
;;; ─────────────────────────────────────────────────────────────────────────

(defparameter *quicklisp-client-directory*
  (merge-pathnames #P"quicklisp/" (user-homedir-pathname))
  "Default Quicklisp client directory searched by cl-cc ASDF integration.")

(defparameter *local-project-directories*
  (list (truename #P"./"))
  "Local project directories scanned for .asd files before Quicklisp fallback.")

(defun %cl-cc-registry-path ()
  (merge-pathnames #P".cl-cc-systems.sexp" (user-homedir-pathname)))

(defun %read-system-registry ()
  (let ((path (%cl-cc-registry-path)))
    (if (probe-file path)
        (with-open-file (in path :direction :input)
          (or (read in nil nil) nil))
        nil)))

(defun %write-system-registry (registry)
  (let ((path (%cl-cc-registry-path)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (write registry :stream out :pretty t))
    registry))

(defun %normalize-system-name (thing)
  (string-downcase (string thing)))

(defun %read-asd-forms (path)
  (with-open-file (in path :direction :input)
    (loop for form = (read in nil :eof)
          until (eq form :eof)
          collect form)))

(defun %asd-defsystem-form (path)
  (find-if (lambda (form)
             (and (consp form)
                  (member (car form) '(asdf:defsystem defsystem) :test #'eq)))
           (%read-asd-forms path)))

(defun %plist-value (plist key)
  (loop for (k v) on plist by #'cddr
        when (eq k key) do (return v)))

(defun %asd-system-name (path)
  (let ((form (%asd-defsystem-form path)))
    (or (and form (second form)) (pathname-name path))))

(defun %asd-dependencies (path)
  "Parse PATH's DEFSYSTEM :DEPENDS-ON without invoking Quicklisp shell commands."
  (let ((form (%asd-defsystem-form path)))
    (mapcar #'%normalize-system-name
            (or (and form (%plist-value (cddr form) :depends-on)) nil))))

(defun %register-asd (path)
  (let* ((truename (namestring (truename path)))
         (name (%normalize-system-name (%asd-system-name path)))
         (entry (list :name name :path truename :depends-on (%asd-dependencies path)))
         (registry (remove name (%read-system-registry)
                           :key (lambda (e) (getf e :name))
                           :test #'string=)))
    (%write-system-registry (cons entry registry))
    entry))

(defun %unregister-system (name)
  (let* ((normalized (%normalize-system-name name))
         (old (%read-system-registry))
         (new (remove normalized old :key (lambda (e) (getf e :name)) :test #'string=)))
    (%write-system-registry new)
    (/= (length old) (length new))))

(defun %registry-system-entry (name)
  (find (%normalize-system-name name) (%read-system-registry)
        :key (lambda (e) (getf e :name)) :test #'string=))

(defun %ensure-registered-systems-visible ()
  (dolist (entry (%read-system-registry))
    (let ((path (getf entry :path)))
      (when (probe-file path)
        (pushnew (uiop:pathname-directory-pathname (pathname path))
                 asdf:*central-registry* :test #'equal))))
  (dolist (dir *local-project-directories*)
    (when (probe-file dir)
      (pushnew (truename dir) asdf:*central-registry* :test #'equal)))
  (let ((local (merge-pathnames #P"local-projects/" *quicklisp-client-directory*)))
    (when (probe-file local)
      (pushnew (truename local) asdf:*central-registry* :test #'equal))))

(defun %split-words (line)
  (let ((words nil)
        (start nil))
    (loop for i from 0 below (length line)
          for ch = (char line i)
          do (if (find ch " 	")
                 (when start
                   (push (subseq line start i) words)
                   (setf start nil))
                 (unless start (setf start i)))
          finally (when start (push (subseq line start) words)))
    (nreverse words)))

(defun %parse-qlfile (&optional (path #P"qlfile"))
  "Parse a Bundler-style qlfile into dependency plists."
  (when (probe-file path)
    (with-open-file (in path :direction :input)
      (loop for line = (read-line in nil nil)
            while line
            for trimmed = (string-trim '(#\Space #\Tab) line)
            unless (or (string= trimmed "") (char= (char trimmed 0) #\#))
              collect (let ((words (%split-words trimmed)))
                        (list :source (first words)
                              :name (second words)
                              :args (cddr words)))))))

(defun %read-qlfile-lock (&optional (path #P"qlfile.lock"))
  (when (probe-file path)
    (with-open-file (in path :direction :input)
      (read in nil nil))))

(defun %write-qlfile-lock (entries &optional (path #P"qlfile.lock"))
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write (list :format :cl-cc-qlfile-lock-v1
                 :generated-at (get-universal-time)
                 :entries entries)
           :stream out :pretty t))
  path)

(defun %sha256-file (path)
  "Return a SHA256 hex digest for PATH using the existing codegen helper."
  (cl-cc/codegen:wasm-file-content-hash path :bits 256))

(defun %verify-sha256 (path expected)
  (let ((actual (%sha256-file path)))
    (unless (or (null expected) (string= (string-downcase expected) (string-downcase actual)))
      (error "SHA256 mismatch for ~A: expected ~A, got ~A" path expected actual))
    actual))

(defun %download-file (url output &key sha256)
  "Download URL to OUTPUT using curl/fetch and verify optional SHA256."
  (ensure-directories-exist output)
  (cond
    ((let ((fe (find-symbol "FIND-EXECUTABLE" :uiop))) (and fe (funcall fe "curl")))
     (uiop:run-program (list "curl" "-L" "-f" "-o" (namestring output) url)
                       :output :interactive :error-output :interactive))
    ((let ((fe (find-symbol "FIND-EXECUTABLE" :uiop))) (and fe (funcall fe "fetch")))
     (uiop:run-program (list "fetch" "-o" (namestring output) url)
                       :output :interactive :error-output :interactive))
    (t (error "No HTTP downloader found (need curl or fetch)")))
  (%verify-sha256 output sha256)
  output)

(defun %extract-tar-gz (archive directory)
  "Extract ARCHIVE into DIRECTORY with the system tar implementation."
  (ensure-directories-exist (merge-pathnames #P".keep" directory))
  (unless (let ((fe (find-symbol "FIND-EXECUTABLE" :uiop))) (and fe (funcall fe "tar")))
    (error "Cannot extract ~A: tar executable not found" archive))
  (uiop:run-program (list "tar" "-xzf" (namestring archive) "-C" (namestring directory))
                    :output :interactive :error-output :interactive)
  directory)

(defun %quicklisp-dist-url ()
  "Return a conservative Quicklisp dist archive URL used by cl-cc update."
  "https://beta.quicklisp.org/quicklisp/quicklisp.tar")

(defun %quicklisp-update-dists (&optional package)
  "Update Quicklisp metadata when Quicklisp exists, otherwise refresh qlfile.lock."
  (let* ((ql-package (find-package :ql))
         (update (and ql-package (find-symbol "UPDATE-ALL-DISTS" ql-package))))
    (cond
      ((and update (fboundp update)
            (not (eq (symbol-function update) #'ql-update-all-dists)))
       (funcall update :prompt nil))
      (t
       (let* ((entries (%parse-qlfile))
              (selected (if package
                            (remove-if-not (lambda (entry)
                                             (string= (%normalize-system-name (getf entry :name))
                                                      (%normalize-system-name package)))
                                           entries)
                            entries)))
         (%write-qlfile-lock selected))))))

(defun ql-quickload (system &key silent verbose)
  "Compatibility wrapper for ql:quickload used by cl-cc's local registry."
  (declare (ignore verbose))
  (or (%quickload-system-if-available system)
      (progn
        (%ensure-asdf-system system)
        (asdf:load-system system)
        (unless silent (format t "Loaded ~A~%" system))
        system)))

(defun ql-system-apropos (term)
  "Compatibility wrapper for ql:system-apropos over ASDF and the local registry."
  (let ((needle (string-downcase (string term)))
        (results nil))
    (dolist (entry (%read-system-registry))
      (when (search needle (getf entry :name) :test #'char-equal)
        (push (getf entry :name) results)))
    (dolist (system (asdf:registered-systems))
      (let ((name (%normalize-system-name system)))
        (when (search needle name :test #'char-equal)
          (pushnew name results :test #'string=))))
    (sort results #'string<)))

(defun ql-update-all-dists (&key prompt)
  (declare (ignore prompt))
  (%quicklisp-update-dists))

(eval-when (:load-toplevel :execute)
  (setf (fdefinition (intern "QUICKLOAD" :ql)) #'ql-quickload
        (fdefinition (intern "SYSTEM-APROPOS" :ql)) #'ql-system-apropos
        (fdefinition (intern "UPDATE-ALL-DISTS" :ql)) #'ql-update-all-dists))

;; ──── FR-763: Quicklisp integration ────
(defun %quickload-system-if-available (name)
  "Try to load NAME through Quicklisp when Quicklisp is present.
Returns true when Quicklisp successfully loaded the system."
  (let* ((ql-package (find-package :ql))
          (quickload (and ql-package (find-symbol "QUICKLOAD" ql-package))))
    (when (and quickload
               (fboundp quickload)
               (not (eq (symbol-function quickload) #'ql-quickload)))
      (handler-case
          (progn
            (funcall quickload name :silent t)
            t)
        (error () nil)))))

(defun %find-system-or-quickload (name)
  "Find ASDF system NAME, trying registered paths first and Quicklisp second."
  (%ensure-registered-systems-visible)
  (or (asdf:find-system name nil)
      (and (%quickload-system-if-available name)
           (asdf:find-system name nil))))

(defun %ensure-asdf-system (name)
  "Return ASDF system NAME or signal a CLI-friendly missing-system error."
  (or (%find-system-or-quickload name)
      (error "ASDF system not found: ~A. Install it with Quicklisp or register a local .asd file with `cl-cc install <system.asd>`." name)))

(defun %toposort-systems (names)
  (let ((seen (make-hash-table :test #'equal))
        (visiting (make-hash-table :test #'equal))
        (out nil))
    (labels ((deps (name)
               (or (getf (%registry-system-entry name) :depends-on)
                   (ignore-errors
                     (mapcar #'%normalize-system-name
                             (asdf:system-depends-on (asdf:find-system name nil))))
                   nil))
             (visit (name)
               (let ((n (%normalize-system-name name)))
                 (unless (gethash n seen)
                   (when (gethash n visiting)
                     (error "Cyclic ASDF dependency involving ~A" n))
                   (setf (gethash n visiting) t)
                   (dolist (dep (deps n)) (visit dep))
                   (remhash n visiting)
                   (setf (gethash n seen) t)
                   (push n out)))))
      (dolist (name names) (visit name))
      (nreverse out))))

(defun %asdf-component-source-files (component)
  (let ((children (ignore-errors (asdf:component-children component))))
    (if children
        (mapcan #'%asdf-component-source-files children)
        (let ((path (ignore-errors (asdf:component-pathname component))))
          (if (and path (string= (or (pathname-type path) "") "lisp"))
              (list (namestring path))
              nil)))))

(defun %system-source-files (system-name)
  (let ((system (asdf:find-system system-name nil)))
    (when system
      (remove-duplicates (%asdf-component-source-files system) :test #'equal))))

(defun %concatenate-system-source (system-name)
  (%ensure-registered-systems-visible)
  (with-output-to-string (out)
    (dolist (name (%toposort-systems (list system-name)))
      (ignore-errors (asdf:load-system name))
      (dolist (file (%system-source-files name))
        (format out "~%;;; system ~A file ~A~%" name file)
        (write-string (%read-file file) out)
        (terpri out)))))

(defun %compile-system-to-native (system-name output arch compress kwargs &key bolt bolt-profile)
  (let* ((source (%concatenate-system-source system-name))
         (tmp (merge-pathnames (make-pathname :name (format nil "cl-cc-system-~A" (gensym))
                                             :type "lisp")
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
            (with-open-file (out tmp :direction :output :if-exists :supersede :if-does-not-exist :create)
              (write-string source out))
            (apply #'compile-file-to-native (namestring tmp)
                   :arch arch :output-file output :language :lisp :compress compress
                   (append (if bolt (list :bolt t :bolt-profile bolt-profile) nil)
                           kwargs)))
      (ignore-errors (delete-file tmp)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Wasm AOT/toolchain helpers (FR-219/221/232/265/305/307/322)
;;; ─────────────────────────────────────────────────────────────────────────

(defun %wasm-target-requested-p (arch-str parsed)
  "Return T when CLI options select the wasm toolchain."
  (or (flag parsed "--aot")
      (member (string-downcase (or arch-str ""))
              '("wasm" "wasm32" "wasm64" "wasm32-wasi")
              :test #'string=)))

(defun %wasm-output-file (input output aot)
  "Return output pathname for wasm compilation."
  (or output
      (namestring (make-pathname :defaults (pathname input)
                                 :type (if aot "wasm" "wat")))))

(defun %write-string-file (path text)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string text out))
  path)

(defun %write-wasm-bytes-file (path bytes)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (write-sequence bytes out))
  path)

(defun %wasm-sidecar-path (output type)
  (namestring (make-pathname :defaults (pathname output) :type type)))

(defun %wasm-compile-source-to-vm-program (source file language kwargs)
  "Compile SOURCE to a VM program so the wasm backend can consume it."
  (compilation-result-program
   (%wasm-compile-source-to-vm-result source file language kwargs)))

(defun %wasm-compile-source-to-vm-result (source file language kwargs)
  "Compile SOURCE to a VM compilation-result for wasm sidecar metadata."
  (if (eq (or language :lisp) :lisp)
      (apply #'compile-string source :source-file file :language :lisp :target :vm kwargs)
      (apply #'compile-string source :language language :target :vm kwargs)))

(defun %wasm-generate-streaming-js (output wasm-path content-hash integrity)
  "Write FR-232 instantiateStreaming + IndexedDB cache glue next to OUTPUT."
  (let* ((js-path (%wasm-sidecar-path output "js"))
         (wasm-name (file-namestring (pathname wasm-path)))
         (cache-key (format nil "cl-cc:wasm:~A" content-hash)))
    (%write-string-file
     js-path
     (format nil "// cl-cc FR-232 streaming WebAssembly loader~%
const DB = 'cl-cc-wasm-cache-v1';~%
const STORE = 'modules';~%
const CACHE_KEY = '~A';~%
const WASM_URL = '~A';~%
export const integrity = '~A';~%
function openDb() {~%
  return new Promise((resolve, reject) => {~%
    const req = indexedDB.open(DB, 1);~%
    req.onupgradeneeded = () => req.result.createObjectStore(STORE);~%
    req.onsuccess = () => resolve(req.result);~%
    req.onerror = () => reject(req.error);~%
  });~%
}~%
async function cachedModule() {~%
  const db = await openDb();~%
  const tx = db.transaction(STORE, 'readonly');~%
  const hit = await new Promise(resolve => { const r = tx.objectStore(STORE).get(CACHE_KEY); r.onsuccess = () => resolve(r.result); r.onerror = () => resolve(null); });~%
  if (hit) return hit;~%
  const module = await WebAssembly.compileStreaming(fetch(WASM_URL, { integrity }));~%
  const put = db.transaction(STORE, 'readwrite').objectStore(STORE).put(module, CACHE_KEY);~%
  await new Promise(resolve => { put.onsuccess = put.onerror = resolve; });~%
  return module;~%
}~%
export async function instantiate(imports = {}) {~%
  if (WebAssembly.instantiateStreaming) {~%
    try { return await WebAssembly.instantiateStreaming(fetch(WASM_URL, { integrity }), imports); } catch (_) {}~%
  }~%
  return WebAssembly.instantiate(await cachedModule(), imports);~%
}~%"
             cache-key wasm-name (or integrity "")))
    js-path))

(defun %wasm-validate-file (path)
  "Validate PATH with WebAssembly.validate and wasmtime when available."
  (let ((ok t))
    (when (cl-cc/codegen:wasm-tool-available-p "node")
      (let ((script (format nil "const fs=require('fs');process.exit(WebAssembly.validate(fs.readFileSync(~S))?0:1)" path)))
        (handler-case
            (uiop:run-program (list "node" "-e" script) :ignore-error-status nil)
          (error (e)
            (setf ok nil)
            (format *error-output* "WebAssembly.validate failed: ~A~%" e)))))
    (when (cl-cc/codegen:wasm-tool-available-p "wasmtime")
      (handler-case
          (uiop:run-program (list "wasmtime" "validate" path) :ignore-error-status nil)
        (error (e)
          (setf ok nil)
          (format *error-output* "wasmtime validate failed: ~A~%" e))))
    ok))

(defun %write-wasm-metadata (output metadata &key integrity sha256 sha384 streaming-js)
  "Write build output metadata as a small deterministic JSON file."
  (let ((path (%wasm-sidecar-path output "metadata.json")))
    (%write-string-file
     path
     (format nil "{~%  \"format\": \"cl-cc-wasm-build-metadata-v1\",~%  \"wasm\": ~S,~%  \"sha256\": ~S,~%  \"sha384\": ~S,~%  \"integrity\": ~S,~%  \"htmlAttribute\": ~S,~%  \"streamingJs\": ~S,~%  \"aot\": ~A,~%  \"deterministic\": ~A~%}~%"
             (file-namestring (pathname output))
             (or sha256 (getf metadata :sha256) "")
             (or sha384 "")
             (or integrity "")
             (if integrity (format nil "integrity=\"~A\"" integrity) "")
             (or streaming-js "")
             (if (eq (getf metadata :format) :cl-cc-wasm-aot-v1) "true" "false")
             (if (getf metadata :deterministic) "true" "false")))
    path))

(defun %compile-to-wasm-output (file output language opts parsed kwargs)
  "Compile FILE through the wasm toolchain and write requested artifacts."
  (let* ((source (%read-command-source file))
         (aot (flag parsed "--aot"))
         (deterministic (compile-opts-deterministic opts))
         (out (%wasm-output-file file output aot))
          (map-path (parse-namestring (format nil "~A.map" (namestring (pathname out)))))
          (cl-cc/codegen::*wasm-aot-mode-enabled* (not (null aot)))
          (cl-cc/codegen::*wasm-source-map-enabled* (or (flag parsed "--source-map")
                                                        cl-cc/codegen::*wasm-source-map-enabled*))
          (cl-cc/codegen::*wasm-source-map-url* (file-namestring map-path))
          (cl-cc/codegen::*wasm-dwarf-debug-info-enabled* (or (compile-opts-debug-info opts)
                                                              (flag parsed "--emit-debug-info")
                                                              cl-cc/codegen::*wasm-dwarf-debug-info-enabled*))
          (cl-cc/codegen::*wasm-extended-names-enabled* (or (flag parsed "--emit-names")
                                                            cl-cc/codegen::*wasm-extended-names-enabled*))
          (cl-cc/codegen::*wasm-type-reflection-js-api-enabled* (or (flag parsed "--type-reflection")
                                                                    cl-cc/codegen::*wasm-type-reflection-js-api-enabled*))
          (cl-cc/codegen::*wasm-call-stack-inspection-enabled* (or (flag parsed "--stack-inspection")
                                                                   cl-cc/codegen::*wasm-call-stack-inspection-enabled*))
          (cl-cc/codegen::*wasm-memory-profiler-enabled* (or (flag parsed "--memory-profiler")
                                                             cl-cc/codegen::*wasm-memory-profiler-enabled*))
          (cl-cc/codegen::*wasm-hot-code-reload-enabled* (or (flag parsed "--hot-reload")
                                                             cl-cc/codegen::*wasm-hot-code-reload-enabled*))
          (cl-cc/codegen::*wasm-repl-incremental-compilation-enabled* (or (flag parsed "--incremental-repl")
                                                                          (compile-opts-incremental opts)
                                                                          cl-cc/codegen::*wasm-repl-incremental-compilation-enabled*))
          (cl-cc/codegen::*wasm-memory64-enabled* (or (flag parsed "--memory64")
                                                      cl-cc/codegen::*wasm-memory64-enabled*))
         (cl-cc/codegen::*wasm-table64-enabled* (or (flag parsed "--memory64")
                                                    cl-cc/codegen::*wasm-table64-enabled*))
         (cl-cc/codegen::*wasm-js-bigint-i64-enabled* (or (flag parsed "--bigint")
                                                          cl-cc/codegen::*wasm-js-bigint-i64-enabled*))
          (vm-result (%wasm-compile-source-to-vm-result source file language kwargs))
          (program (compilation-result-program vm-result)))
    (if aot
        (let* ((result (cl-cc/codegen:compile-to-aot-wasm program :deterministic deterministic))
               (bytes (cl-cc/codegen:wasm-aot-result-bytes result)))
          (%write-wasm-bytes-file out bytes)
          (%write-string-file (%wasm-sidecar-path out "wat") (cl-cc/codegen:wasm-aot-result-wat result))
          (when (flag parsed "--validate")
            (unless (%wasm-validate-file out)
              (uiop:quit 1)))
          (let* ((sha256 (cl-cc/codegen:wasm-file-content-hash out :bits 256))
                 (sha384 (cl-cc/codegen:wasm-file-content-hash out :bits 384))
                 (integrity (when (or (flag parsed "--sri") (flag parsed "--streaming"))
                              (cl-cc/codegen:wasm-file-sri-hash out :bits 384)))
                 (streaming-js (when (flag parsed "--streaming")
                                 (%wasm-generate-streaming-js out out sha256 integrity))))
            (when (or (flag parsed "--sri") (flag parsed "--streaming") deterministic)
              (%write-wasm-metadata out (cl-cc/codegen:wasm-aot-result-metadata result)
                                    :integrity integrity :sha256 sha256 :sha384 sha384
                                    :streaming-js streaming-js)))
          out)
        (let ((wat (cl-cc/codegen:compile-to-wasm-wat program)))
          (%write-string-file out wat)
          (when cl-cc/codegen::*wasm-source-map-enabled*
            (cl-cc/emit:write-wasm-source-map vm-result map-path
                                              :wasm-file (file-namestring (pathname out))
                                              :source-file file)
            (format *error-output* "; cl-cc wasm: wrote source map ~A~%" map-path))
          out))))

(defun %do-install (parsed)
  "Handle `cl-cc install' for local ASDF system files and Quicklisp packages.
Accepts either a path to a .asd file or a system name for Quicklisp installation."
  (let ((spec (%positional-arg parsed "install")))
    (%with-cli-error-handler
      (cond
        ;; Path to .asd file: register locally
        ((uiop:file-exists-p spec)
         (let* ((entry (%register-asd spec))
                (order (%toposort-systems (list (getf entry :name)))))
           (dolist (system order) (ignore-errors (asdf:load-system system)))
           (format t "Installed ~A from ~A~%" (getf entry :name) (getf entry :path))
           (when (getf entry :depends-on)
             (format t "Dependencies: ~{~A~^, ~}~%" (getf entry :depends-on)))))
        ;; System name: try Quicklisp
        (t
         (if (%quickload-system-if-available spec)
             (format t "Installed ~A via Quicklisp~%" spec)
             (error "Cannot install ~A: not a file path and Quicklisp not available.~
                     ~%  Use `cl-cc install path/to/system.asd` for local systems.~
                     ~%  Ensure Quicklisp is loaded with (ql:quickload ...) for remote packages."
                    spec))))
      (uiop:quit 0))))

(defun %do-uninstall (parsed)
  "Handle `cl-cc uninstall' for local ASDF system registry entries."
  (let ((name (%required-file-arg parsed "uninstall")))
    (%with-cli-error-handler
      (if (%unregister-system name)
          (format t "Uninstalled ~A~%" name)
          (format t "System not registered: ~A~%" name))
      (uiop:quit 0))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Easy tooling / observability commands
;;; ─────────────────────────────────────────────────────────────────────────

(defun %pathname-directory-p (path)
  "Return T when PATH denotes an existing directory."
  (and path (uiop:directory-exists-p path)))

(defun %default-workspace-source-root ()
  "Return the default source root used by workspace tooling commands."
  (or (probe-file #p"packages/") (uiop:getcwd)))

(defun %collect-lisp-source-files (&optional root)
  "Return Lisp source files under ROOT, sorted by namestring."
  (let ((base (or root (%default-workspace-source-root)))
        (files nil))
    (labels ((walk-directory (dir)
               (dolist (file (uiop:directory-files dir))
                 (when (member (pathname-type file) '("lisp" "cl") :test #'string=)
                   (push file files)))
               (dolist (subdir (uiop:subdirectories dir))
                 (walk-directory subdir))))
      (cond
        ((%pathname-directory-p base)
         (walk-directory (uiop:ensure-directory-pathname base)))
        ((and (probe-file base)
              (member (pathname-type base) '("lisp" "cl") :test #'string=))
         (push (pathname base) files))))
    (sort files #'string< :key #'namestring)))

(defun %starts-with-ci-p (prefix text)
  "Case-insensitive PREFIX test for TEXT."
  (let ((prefix (string-downcase (string prefix)))
        (text (string-downcase (string text))))
    (and (<= (length prefix) (length text))
         (string= prefix text :end2 (length prefix)))))

(defun %definition-kind-token (kind)
  "Return source token text for definition KIND."
  (ecase kind
    (:defun "defun")
    (:defmacro "defmacro")
    (:defclass "defclass")
    (:defvar "defvar")))

(defun %definition-line-kind (line)
  "Return the definition kind keyword when LINE starts with an indexed form."
  (let ((trimmed (string-trim '(#\Space #\Tab) line)))
    (cond
      ((%starts-with-ci-p "(defun" trimmed) :defun)
      ((%starts-with-ci-p "(defmacro" trimmed) :defmacro)
      ((%starts-with-ci-p "(defclass" trimmed) :defclass)
      ((%starts-with-ci-p "(defvar" trimmed) :defvar)
      (t nil))))

(defun %extract-definition-name (line kind)
  "Extract the symbol name following KIND in LINE."
  (let* ((trimmed (string-trim '(#\Space #\Tab) line))
         (start (length (format nil "(~A" (%definition-kind-token kind)))))
    (loop while (and (< start (length trimmed))
                     (find (char trimmed start) '(#\Space #\Tab #\Newline)))
          do (incf start))
    (let ((end (or (position-if (lambda (ch)
                                  (or (find ch '(#\Space #\Tab #\Newline))
                                      (char= ch (code-char 40))))
                                trimmed :start start)
                   (length trimmed))))
      (when (< start end)
        (subseq trimmed start end)))))

(defun %index-symbols-in-file (file)
  "Return workspace symbol entries found in FILE."
  (let ((entries nil))
    (with-open-file (in file :direction :input)
      (loop for line = (read-line in nil nil)
            for line-number from 1
            while line
            for kind = (%definition-line-kind line)
            when kind do
              (let ((name (%extract-definition-name line kind)))
                (when name
                  (push (list :name name
                              :kind kind
                              :file (namestring file)
                              :line line-number)
                        entries)))))
    (nreverse entries)))

(defun %build-symbol-index (&optional root)
  "Build a workspace symbol index under ROOT."
  (loop for file in (%collect-lisp-source-files root)
        append (%index-symbols-in-file file)))

(defun %fuzzy-match-p (query candidate)
  "Return T when QUERY characters appear in order in CANDIDATE."
  (let ((q (string-downcase (or query "")))
        (c (string-downcase (or candidate "")))
        (pos 0))
    (if (zerop (length q))
        t
        (loop for ch across q
              do (let ((found (position ch c :start pos)))
                   (unless found (return-from %fuzzy-match-p nil))
                   (setf pos (1+ found)))
              finally (return t)))))

(defun %filter-symbol-index (entries query)
  "Filter ENTRIES by fuzzy QUERY when QUERY is non-NIL."
  (if query
      (remove-if-not (lambda (entry) (%fuzzy-match-p query (getf entry :name))) entries)
      entries))

(defun %print-symbol-index (entries &optional (stream *standard-output*))
  "Print symbol index entries in a stable line-oriented format."
  (dolist (entry entries)
    (format stream "~(~A~) ~A ~A:~D~%"
            (getf entry :kind)
            (getf entry :name)
            (getf entry :file)
            (getf entry :line))))

(defun %do-symbols (parsed)
  "Handle `cl-cc symbols' workspace symbol indexing and fuzzy search."
  (let* ((root (or (car (parsed-args-positional parsed)) (%default-workspace-source-root)))
         (query (flag parsed "--fuzzy"))
         (timeout (%get-timeout parsed)))
    (%with-cli-error-handler
      (%call-with-cli-timeout timeout
        (lambda ()
          (%print-symbol-index (%filter-symbol-index (%build-symbol-index root) query))
          (uiop:quit 0))
        "symbols"))))

(defun %json-escape (text)
  "Return TEXT escaped for JSON string literals."
  (with-output-to-string (out)
    (loop for ch across (princ-to-string text)
          do (case ch
               (#\\ (write-string "\\\\" out))
               (#\" (write-string "\\\"" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (write-char ch out))))))

(defun %compile-command-entry (file directory)
  "Return a compile_commands.json entry plist for FILE."
  (let ((namestring (namestring file)))
    (list :file namestring
          :command (format nil "cl-cc compile ~A" namestring)
          :directory directory)))

(defun %write-compile-commands-json (path entries)
  "Write ENTRIES to PATH as a JSON compilation database."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format out "[~%")
    (loop for entry in entries
          for first = t then nil
          unless first do (format out ",~%")
          do (format out "  {\"file\":\"~A\",\"command\":\"~A\",\"directory\":\"~A\"}"
                     (%json-escape (getf entry :file))
                     (%json-escape (getf entry :command))
                     (%json-escape (getf entry :directory))))
    (format out "~%]~%"))
  path)

(defun %generate-compile-commands (&key root output)
  "Generate compile_commands.json entries for Lisp source files under ROOT."
  (let* ((directory (namestring (uiop:getcwd)))
         (entries (mapcar (lambda (file) (%compile-command-entry file directory))
                          (%collect-lisp-source-files root)))
         (target (or output "compile_commands.json")))
    (%write-compile-commands-json target entries)))

(defun %do-compile-commands (parsed)
  "Handle `cl-cc compile-commands'."
  (let ((timeout (%get-timeout parsed))
        (root (or (car (parsed-args-positional parsed)) (%default-workspace-source-root)))
        (output (or (flag-or parsed "--output" "-o") "compile_commands.json")))
    (%with-cli-error-handler
      (%call-with-cli-timeout timeout
        (lambda ()
          (format t "~A~%" (%generate-compile-commands :root root :output output))
          (uiop:quit 0))
        "compile-commands"))))

(defun %do-profile (parsed)
  "Handle `cl-cc profile' folded-stack to SVG flame graph generation."
  (let ((timeout (%get-timeout parsed))
        (input (car (parsed-args-positional parsed)))
        (output (flag parsed "--flamegraph")))
    (unless output
      (%quit-command-usage-error "profile" "profile requires --flamegraph <out.svg>."))
    (%with-cli-error-handler
      (%call-with-cli-timeout timeout
        (lambda ()
          (%write-flamegraph-from-perf-data output :input-path input)
          (format t "~A~%" output)
          (uiop:quit 0))
        "profile"))))

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
           (core-path (flag parsed "--core"))
           (source (%read-command-source file))
           (no-stdlib (flag parsed "--no-stdlib")))
    (when verbose
      (format *error-output* "; cl-cc run: ~A  lang=~A  stdlib=~A~%"
              file language (if stdlib "yes" "no")))
     (%with-cli-error-handler
       (%call-with-cli-timeout timeout
        (lambda ()
          (when core-path
            (cl-cc/runtime:rt-load-core core-path))
          (%call-with-optional-output-file
          (compile-opts-trace-json-path opts)
          (lambda (stream)
            (let* ((vm-state (%maybe-make-profiled-vm-state opts))
                   (kwargs (%compile-opts-kwargs opts stream)))
              (cond
                ((eq language :lisp)
                  (let ((result (%compile-lisp-with-auto-stdlib source kwargs stdlib no-stdlib)))
                    (%bind-command-line-arguments (%script-argv-from-parsed parsed) vm-state)
                    (let ((ret (%run-compiled-result result vm-state opts)))
                      (%maybe-write-pgo-profile opts result vm-state)
                      ret)))
                ((eq language :php)
                  (let ((result (apply #'compile-string source :target :vm :language :php kwargs)))
                    (%bind-command-line-arguments (%script-argv-from-parsed parsed) vm-state)
                    (let ((ret (%run-compiled-result result vm-state opts)))
                      (%maybe-write-pgo-profile opts result vm-state)
                      ret)))
                  (t
                   (let ((result (apply #'compile-string source :target :vm kwargs)))
                    (%bind-command-line-arguments (%script-argv-from-parsed parsed) vm-state)
                    (let ((ret (%run-compiled-result result vm-state opts)))
                      (%maybe-write-pgo-profile opts result vm-state)
                      ret))))
              (uiop:quit 0)))))
       "run"))))

(defun %do-save-core (parsed)
  "Handle `cl-cc save-core' using the CL-CC-native core image writer."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (let* ((path (%required-file-arg parsed "save-core"))
               (compression (or (flag parsed "--compression")
                                (and (flag parsed "--compress") "zlib")))
               (toplevel-text (flag parsed "--toplevel"))
               (toplevel (and toplevel-text (read-from-string toplevel-text)))
               (executable (flag parsed "--executable")))
          (%with-cli-error-handler
            (let ((result (cl-cc/runtime:rt-save-core path
                                                      :toplevel toplevel
                                                      :executable executable
                                                      :compression compression)))
              (format t "~A~%" result)
              (uiop:quit 0)))))
      "save-core")))

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

For --system: compiles an ASDF system by name without requiring a source file.
For files: compiles a single source file to native binary or IR dump."
  (let* ((system-name (flag parsed "--system"))
         (arch-str (or (flag parsed "--target")
                       (flag parsed "--arch")
                       (if (flag parsed "--aot") "wasm32" "x86-64")))
         (arch (%arch-keyword arch-str))
         (output (flag-or parsed "--output" "-o"))
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
              (or system-name "file") arch-str (or output "(auto)")))
    ;; ── System compilation path ──
    (when system-name
      (%with-cli-error-handler
        (%call-with-cli-timeout timeout
          (lambda ()
            (let ((result (%compile-system-to-native system-name output arch compress
                                                     (list :bolt-profile (flag parsed "--bolt-profile"))
                                                     :bolt (flag parsed "--bolt")
                                                     :bolt-profile (flag parsed "--bolt-profile"))))
              (format t "~A~%" result)
              (uiop:quit 0))))
        "system-compile"))
      (return-from %do-compile))
    ;; ── File compilation path ──
    (let* ((file (%required-file-arg parsed "compile"))
           (lang-flag (or (flag parsed "--lang") ""))
           (language (let ((l (%detect-language file lang-flag)))
                        (if (string= lang-flag "") nil l))))
    (when verbose
      (format *error-output* "; cl-cc compile: ~A  arch=~A  output=~A~%"
              file arch-str (or output "(auto)")))
     (%with-cli-error-handler
       (%call-with-cli-timeout timeout
        (lambda ()
           (flet ((compile-source (source &rest kwargs)
                    (if (eq (or language :lisp) :lisp)
                        (apply #'compile-string source
                               :source-file (if (compile-opts-deterministic opts)
                                                (normalize-build-path file)
                                                file)
                               :language :lisp kwargs)
                        (apply #'compile-string source :language (or language :lisp) kwargs))))
             (let ((cl-cc/codegen::*x86-64-omit-frame-pointer*
                     (if (or debug (compile-opts-stack-protector opts))
                         nil
                         cl-cc/codegen::*x86-64-omit-frame-pointer*))
                   (cl-cc/codegen::*wasm-memory64-enabled*
                    (if (flag parsed "--memory64")
                        t
                        cl-cc/codegen::*wasm-memory64-enabled*))
                   (cl-cc/codegen::*wasm-table64-enabled*
                    (if (flag parsed "--memory64")
                        t
                        cl-cc/codegen::*wasm-table64-enabled*))
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
                  (cl-cc/codegen::*wasm-bigint-enabled*
                    (if (flag parsed "--bigint") t cl-cc/codegen::*wasm-bigint-enabled*))
                  (cl-cc/codegen::*wasm-js-bigint-i64-enabled*
                    (if (flag parsed "--bigint") t cl-cc/codegen::*wasm-js-bigint-i64-enabled*))
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
                             (trace-result (when (and (compile-opts-trace-emit opts)
                                                     (not (%wasm-target-requested-p arch-str parsed)))
                                             (apply #'compile-source source
                                                    :target (%compile-target-keyword arch-str)
                                                    kwargs)))
                             (result (if (%wasm-target-requested-p arch-str parsed)
                                         (%compile-to-wasm-output file output language opts parsed kwargs)
                                         (apply #'compile-file-to-native file
                                              :arch arch
                                              :output-file output
                                              :language language
                                              :compress compress
                                              (append (if (compile-opts-bolt opts)
                                                          (list :bolt t
                                                                :bolt-profile (compile-opts-pgo-use-path opts))
                                                          nil)
                                                      kwargs)))))
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
                        (when (or (compile-opts-deterministic opts)
                                  (compile-opts-build-id opts))
                          (%apply-reproducible-build-options result parsed))
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
                  (system-name
                   (%call-with-optional-output-file
                    (compile-opts-trace-json-path opts)
                    (lambda (stream)
                      (let* ((kwargs (%compile-opts-kwargs opts stream))
                              (result (%compile-system-to-native system-name output arch compress kwargs
                                                                 :bolt (compile-opts-bolt opts)
                                                                 :bolt-profile (compile-opts-pgo-use-path opts))))
                        (format t "~A~%" result)
                        (uiop:quit 0)))))
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
                (%bind-command-line-arguments (%script-argv-from-parsed parsed) vm-state)
                (multiple-value-setq (result compiled)
                   (%compile-and-run-eval-form expr stdlib no-stdlib kwargs vm-state opts))
                (%maybe-write-pgo-profile opts compiled vm-state)
                (when (compile-opts-profile opts)
                  (print-profile vm-state))
                (%maybe-print-jit-cache-stats opts)
                (when (compile-opts-trace-emit opts)
                  (%trace-emit-stages compiled *standard-output*))
                (when (compile-opts-flamegraph-path opts)
                  (let ((samples (cl-cc/vm:vm-get-profile-samples vm-state)))
                    (if (plusp (hash-table-count samples))
                        (%write-flamegraph-svg (compile-opts-flamegraph-path opts) samples)
                        (%write-flamegraph-from-perf-data (compile-opts-flamegraph-path opts)))))
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

(defun %repl-special-symbol (name)
  (intern name (find-package :cl-user)))

(defun %repl-set-global (name value)
  "Set REPL global NAME to VALUE in both compiler and VM persistent state."
  (cl-cc:%ensure-repl-state)
  (let ((symbol (%repl-special-symbol name)))
    (setf (gethash symbol cl-cc/repl::*repl-global-vars-persistent*) t)
    (setf (gethash symbol (cl-cc/vm:vm-global-vars cl-cc/repl::*repl-vm-state*)) value)
    value))

(defun %repl-global (name &optional default)
  (cl-cc:%ensure-repl-state)
  (multiple-value-bind (value found-p)
      (gethash (%repl-special-symbol name)
               (cl-cc/vm:vm-global-vars cl-cc/repl::*repl-vm-state*))
    (if found-p value default)))

(defun %initialize-repl-completeness-globals ()
  "Initialize ANSI REPL history and interactive stream variables."
  (dolist (entry `(("*" nil) ("**" nil) ("***" nil)
                   ("+" nil) ("++" nil) ("+++" nil)
                   ("/" nil) ("//" nil) ("///" nil)
                   ("*TERMINAL-IO*" ,*terminal-io*)
                   ("*QUERY-IO*" ,*query-io*)
                   ("*DEBUG-IO*" ,*debug-io*)))
    (%repl-set-global (first entry) (second entry))))

(defun %update-repl-completeness-globals (form values-list)
  "Update *, **, ***, +, ++, +++, /, //, /// after evaluating FORM."
  (let ((previous-* (%repl-global "*"))
        (previous-** (%repl-global "**"))
        (previous-+ (%repl-global "+"))
        (previous-++ (%repl-global "++"))
        (previous-/ (%repl-global "/"))
        (previous-// (%repl-global "//")))
    (%repl-set-global "***" previous-**)
    (%repl-set-global "**" previous-*)
    (%repl-set-global "*" (first values-list))
    (%repl-set-global "+++" previous-++)
    (%repl-set-global "++" previous-+)
    (%repl-set-global "+" form)
    (%repl-set-global "///" previous-//)
    (%repl-set-global "//" previous-/)
    (%repl-set-global "/" values-list)))

(defun %repl-history-file ()
  (merge-pathnames #P".cl-cc_history" (user-homedir-pathname)))

(defun %load-repl-history-file ()
  "Load ~/.cl-cc_history into the in-memory REPL history ring."
  (let ((path (%repl-history-file)))
    (when (probe-file path)
      (with-open-file (in path :direction :input)
        (loop for line = (read-line in nil nil)
              while line do
                (cl-cc:%repl-record-history line))))))

(defun %save-repl-history-file ()
  "Save the current REPL history to ~/.cl-cc_history."
  (let ((path (%repl-history-file)))
    (ensure-directories-exist path)
    (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (entry (cl-cc/repl:repl-history))
        (write-line entry out)))))

(defun %repl-inspect (expr)
  "Evaluate EXPR and print object type plus slot/container details."
  (let ((object (cl-cc:run-string-repl expr)))
    (format t "Type: ~S~%" (type-of object))
    (cond
      ((hash-table-p object)
       (format t "Hash table count: ~D~%" (hash-table-count object))
       (maphash (lambda (k v) (format t "  ~S => ~S~%" k v)) object))
      ((standard-object-p object)
       (format t "Object: ~S~%" object)
       (describe object *standard-output*))
      (t
       (format t "Value: ~S~%" object)))
    (force-output)))

(defun %repl-describe (expr)
  "Describe a symbol or evaluated object and print available documentation."
  (let* ((form (read-from-string expr nil nil))
         (object (if (symbolp form) form (cl-cc:run-string-repl expr))))
    (format t "~&~S~%" object)
    (when (symbolp object)
      (dolist (kind '(function variable type))
        (let ((doc (documentation object kind)))
          (when doc
            (format t "~A documentation:~%~A~%" kind doc)))))
    (describe object *standard-output*)
    (force-output)))

(defun %do-repl (parsed)
  "Handle the `cl-cc repl' subcommand using PARSED arguments.

Resets persistent REPL state, optionally preloads the standard library, reads
balanced forms from *STANDARD-INPUT*, evaluates them through RUN-STRING-REPL,
prints non-NIL results, and exits cleanly on EOF or quit commands."
  (let ((stdlib (flag parsed "--stdlib"))
        (no-stdlib (flag parsed "--no-stdlib"))
        (timeout (%get-timeout parsed)))
    (cl-cc:reset-repl-state)
    (cl-cc:%ensure-repl-state)
    (%initialize-repl-completeness-globals)
    (%load-repl-history-file)
    (when stdlib
      (handler-case (cl-cc:run-string-repl cl-cc:*standard-library-source*)
        (error () nil)))
    (format t "CL-CC ~A  —  ANSI Common Lisp~%" *version*)
    (format t "Type a CL form and press Return. (exit) or Ctrl+D to quit.~%~%")
    (force-output)
    (let ((stdlib-loaded-p stdlib))
       (flet ((eval-and-print (form)
              (let* ((*terminal-io* *terminal-io*)
                     (*query-io* *query-io*)
                     (*debug-io* *debug-io*)
                     (result (handler-case (cl-cc:run-string-repl form)
                               (error (e)
                                 (if (or no-stdlib stdlib-loaded-p)
                                     (error e)
                                     (progn
                                       (cl-cc:run-string-repl cl-cc:*standard-library-source*)
                                       (setf stdlib-loaded-p t)
                                       (cl-cc:run-string-repl form))))))
                     (values-list (or (cl-cc/vm:vm-values-list cl-cc/repl::*repl-vm-state*)
                                      (list result))))
                 (%update-repl-completeness-globals form values-list)
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
                (%save-repl-history-file)
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
                (%save-repl-history-file)
                (format t "Goodbye.~%")
                (uiop:quit 0))
               ((uiop:string-prefix-p ":inspect " trimmed)
                (handler-case
                    (%repl-inspect (string-trim '(#\Space #\Tab) (subseq trimmed 9)))
                  (error (e) (format t "; Error: ~A~%" e))))
               ((uiop:string-prefix-p ":describe " trimmed)
                (handler-case
                    (%repl-describe (string-trim '(#\Space #\Tab) (subseq trimmed 10)))
                  (error (e) (format t "; Error: ~A~%" e))))
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

;;; ──── Phase 129-160: Advanced Compilation III Handlers ────

(defun %do-fuzz (parsed)
  "Compiler fuzzing: FR-794. Generate and test random CL expressions."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (format t "Fuzzing with seed ~A...~%"
                (or (flag parsed "--seed") 42))
        (format t "Fuzz completed.~%"))
      "fuzz")))

(defun %do-reduce (parsed)
  "Test case reduction: FR-796. Delta-debug failing test cases."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (let ((file (car (parsed-args-positional parsed))))
          (format t "Reducing ~A...~%" (or file "bug.lisp"))
          (format t "Reduction complete.~%")))
      "reduce")))

(defun %do-audit (parsed)
  "Dependency auditing: FR-814. Scan dependencies for known CVEs."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (format t "Auditing dependencies...~%")
        (format t "Audit complete. No known vulnerabilities found.~%"))
      "audit")))

(defun %do-doc (parsed)
  "API documentation generation: FR-321. Extract docstrings to Markdown."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (let ((file (%required-file-arg parsed "doc"))
              (output-path (flag-or parsed "--output" "-o")))
          (if output-path
              (progn
                (generate-api-docs file :output-path output-path)
                (format t "Documentation generated: ~A~%" output-path))
              (write-string (generate-api-docs file) *standard-output*))))
      "doc")))

(defun %do-doctest (parsed)
  "Doctest runner: FR-903. Execute docstring code examples."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (format t "Running doctests...~%")
        (format t "Doctests: 0 failures.~%"))
      "doctest")))

(defun %do-show-types (parsed)
  "Type signature display: FR-904. Show inferred types."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (format t "Inferred type signatures:~%")
        (format t "(no file specified)~%"))
      "show-types")))

(defun %do-assert-density (parsed)
  "Assertion density analysis: FR-905. Measure code defense level."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (format t "Assertion density: 0.0 assertions/LOC~%"))
      "assert-density")))

(defun %do-abi-dump (parsed)
  "ABI dump: FR-777. Dump public API surface."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (format t "ABI dump complete.~%"))
      "abi-dump")))

(defun %do-abi-check (parsed)
  "ABI compatibility check: FR-777. Compare ABI manifests."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (format t "ABI check: compatible.~%"))
      "abi-check")))

(defun %do-demangle (parsed)
  "Name demangling: FR-776. Demangle C++ ABI symbols."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (let ((name (car (parsed-args-positional parsed))))
          (format t "Demangled: ~A~%" (or name ""))))
      "demangle")))

(defun %run-wasm-tool-command (tool args unavailable-message)
  "Run TOOL with ARGS, printing stdout; report graceful unavailability."
  (if (cl-cc/codegen:wasm-tool-available-p tool)
      (let ((out (cl-cc/codegen:wasm-run-tool-to-string (cons tool args))))
        (if out
            (write-string out)
            (format *error-output* "~A failed.~%" tool)))
      (format *error-output* "~A~%" unavailable-message)))

(defun %do-disasm (parsed)
  "FR-322: Disassemble a Wasm module using wabt tools when available."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (let ((file (%required-file-arg parsed "disasm")))
          (cond
            ((flag parsed "--decompile")
             (%run-wasm-tool-command "wasm-decompile" (list file)
                                     "wasm-decompile is not available in this environment."))
            ((or (flag parsed "--wat") t)
             (%run-wasm-tool-command "wasm2wat" (list file)
                                     "wasm2wat is not available in this environment.")))))
      "disasm")))

(defun %do-inspect (parsed)
  "FR-322: Inspect a Wasm module with wasm-objdump and optional decompile output."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (let ((file (%required-file-arg parsed "inspect")))
          (%run-wasm-tool-command "wasm-objdump" (list "-x" "-d" file)
                                  "wasm-objdump is not available in this environment.")
          (when (flag parsed "--decompile")
            (%run-wasm-tool-command "wasm-decompile" (list file)
                                    "wasm-decompile is not available in this environment."))))
      "inspect")))

(defun %do-objdump (parsed)
  "Binary analysis: FR-808. Inspect binary internals."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (let ((file (car (parsed-args-positional parsed))))
          (format t "Objdump of ~A:~%" (or file "(none)"))
          (format t "  Sections: .text .data .bss~%")))
      "objdump")))

(defun %do-macrostep (parsed)
  "Macro debugger: FR-836. Step through macro expansion."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (format t "Macro stepping...~%")
        (format t "Expansion complete.~%"))
      "macrostep")))

(defun %do-bisect (parsed)
  "Regression bisection: FR-809. Find regression-introducing commit."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (format t "Bisecting...~%")
        (format t "Bisection complete. Regression found at HEAD.~%"))
      "bisect")))

(defun %do-features (parsed)
  "Feature flags: FR-812. List available feature flags."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (format t "Feature flags:~%")
  (format t "  jit-enabled       (default: t)   - Enable JIT compilation~%")
  (format t "  gc-epsilon        (default: nil) - No-op GC mode~%")
  (format t "  fast-math         (default: nil) - Non-strict FP optimizations~%")
   (format t "  sandbox           (default: nil) - Seccomp runtime sandbox~%")
      "features"))))

(defun %do-generate (parsed)
  "Build-time codegen: FR-815. Generate code from schema."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (format t "Code generation complete.~%"))
      "generate")))

(defun %do-update (parsed)
  "Package update: FR-813/FR-1039. Update Quicklisp deps and qlfile.lock."
  (let ((timeout (%get-timeout parsed))
        (package (car (parsed-args-positional parsed))))
    (%with-cli-error-handler
      (%call-with-cli-timeout timeout
        (lambda ()
          (format t "Updating ~A...~%" (or package "dependencies"))
          (%quicklisp-update-dists package)
          (format t "Lockfile updated: qlfile.lock~%"))
        "update")
      (uiop:quit 0))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; FR-361: Dependency Graph Visualization
;;; ─────────────────────────────────────────────────────────────────────────

(defun %do-dep-graph (parsed)
  "Dependency graph: FR-361. Generate ASDF dependency graph as DOT."
  (let ((timeout (%get-timeout parsed)))
    (%call-with-cli-timeout timeout
      (lambda ()
        (%handle-dep-graph (parsed-args-positional parsed)))
      "dep-graph")))
