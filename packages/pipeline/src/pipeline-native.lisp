(in-package :cl-cc/pipeline)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Compiler Pipeline — Native Code Generation Helpers
;;;
;;; Contains: %write-native-binary, *compile-cache-root*, %compile-cache-key,
;;; %compile-cache-path, %copy-file-bytes, compile-to-native,
;;; and compile-file-to-native.
;;;
;;; Core pipeline (compile-expression, compile-string, run-string, our-eval,
;;; run-string-typed) is in pipeline.lisp (loads before).
;;;
;;; Load order: after pipeline.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; Native Executable Generation (Mach-O)

(defparameter *native-command-timeout-seconds* 10
  "Timeout in seconds for short external helper commands used by native compilation.")

(defun %run-short-native-command (argv)
  "Run a short external helper command with an explicit timeout.
Returns NIL on timeout or command failure so native compilation can continue
without hanging forever on platform utility calls like chmod."
  (handler-case
      (sb-ext:with-timeout *native-command-timeout-seconds*
        (uiop:run-program argv :ignore-error-status t))
    (sb-ext:timeout () nil)
    (error () nil)))

(defun %write-native-binary (builder code-bytes output-path)
  "Finalize BUILDER with CODE-BYTES, write Mach-O to OUTPUT-PATH, and mark it executable."
  (cl-cc/binary:add-text-segment builder code-bytes)
  (cl-cc/binary:add-symbol builder "_main" :value 0 :type #x0F :sect 1)
  (let ((mach-o-bytes (cl-cc/binary:build-mach-o builder code-bytes)))
    (cl-cc/binary:write-mach-o-file output-path mach-o-bytes))
  (%run-short-native-command (list "chmod" "+x" (namestring output-path)))
  output-path)

(defparameter *compile-cache-root*
  #P".cache/cl-cc/native/"
  "Directory for cached native build outputs.")

(defun %compile-cache-key (source arch language)
  (format nil "~A-~A-~A"
          (sxhash source)
          arch
          language))

(defun %compile-cache-path (key output-file)
  (merge-pathnames
   (make-pathname :directory (list :relative key)
                  :name (pathname-name output-file)
                  :type (pathname-type output-file))
   *compile-cache-root*))

(defun %copy-file-bytes (from to)
  (with-open-file (in from :direction :input :element-type '(unsigned-byte 8))
    (with-open-file (out to :direction :output :if-exists :supersede
                            :if-does-not-exist :create
                            :element-type '(unsigned-byte 8))
      (loop for byte = (read-byte in nil nil)
            while byte do (write-byte byte out))))
  to)

(defun %non-package-top-level-forms (forms)
  "Return FORMS with in-package declarations removed, mirroring compile-toplevel-forms."
  (remove-if (lambda (f) (and (consp f) (eq (car f) 'in-package))) forms))

;;; Native compilation options — bundle 9 recurring keyword params into a plist.
;;; Internal functions accept (target opts) and apply opts directly as &key args.

(defun %make-native-opts (&key pass-pipeline print-pass-timings timing-stream
                               print-opt-remarks opt-remarks-stream (opt-remarks-mode :all)
                               print-pass-stats stats-stream trace-json-stream)
  "Build a native-compile options plist suitable for APPLYing to compile-* functions."
  (list :pass-pipeline       pass-pipeline
        :print-pass-timings  print-pass-timings
        :timing-stream       timing-stream
        :print-opt-remarks   print-opt-remarks
        :opt-remarks-stream  opt-remarks-stream
        :opt-remarks-mode    opt-remarks-mode
        :print-pass-stats    print-pass-stats
        :stats-stream        stats-stream
        :trace-json-stream   trace-json-stream))

(defun %maybe-compile-native-via-cps (form target opts &key type-check (safety 1))
  "Try compiling FORM through a narrow CPS-backed native path.
Returns two values: the compilation result and whether the CPS-native path was used."
  (let ((ast (optimize-ast (%prepare-ast form))))
    (if (%cps-native-compile-safe-ast-p ast)
        (let ((cps (cps-transform-ast* ast)))
          (values (apply #'compile-expression (%cps-identity-entry-form cps)
                         :target target :type-check type-check :safety safety
                         opts)
                  t))
        (values nil nil))))

(defun %compile-native-string (source target language opts)
  "Compile SOURCE through the generic string entrypoint for native codegen."
  (apply #'compile-string source :target target :language language opts))

(defun %compile-native-expression (form target opts)
  "Compile a single already-read FORM through the generic native entrypoint."
  (apply #'compile-expression form :target target opts))

(defun %compile-native-toplevel-forms (forms target opts)
  "Compile FORMS through the generic native top-level entrypoint after optional CPS rewriting."
  (let ((native-opts (apply #'%make-pipeline-opts :target target opts)))
    (apply #'compile-toplevel-forms
           (%maybe-cps-toplevel-forms forms native-opts)
           :target target opts)))

(defun %compile-native-lisp-forms (forms target opts)
  "Compile Lisp FORMS for native emission, preferring the CPS route for a single safe form."
  (let ((effective-forms (%non-package-top-level-forms forms)))
    (if (= (length effective-forms) 1)
        (multiple-value-bind (cps-result cps-used)
            (%maybe-compile-native-via-cps (first effective-forms) target opts)
          (if cps-used
              cps-result
              (%compile-native-toplevel-forms forms target opts)))
        (%compile-native-toplevel-forms forms target opts))))

(defun %compile-native-source (source target language opts)
  "Compile SOURCE for native emission, choosing the narrowest readable entrypoint."
  (if (stringp source)
      (if (eq language :lisp)
          (%compile-native-lisp-forms (parse-all-forms source) target opts)
          (%compile-native-string source target language opts))
      (multiple-value-bind (cps-result cps-used)
          (%maybe-compile-native-via-cps source target opts)
        (if cps-used
            cps-result
            (%compile-native-expression source target opts)))))

(defun %native-target-for-arch (arch)
  (if (eq arch :x86-64)
      :x86_64
      (if (eq arch :arm64)
          :arm64
          (error "Unknown native architecture: ~S" arch))))

(defun %native-code-bytes-for-arch (arch program)
  (if (eq arch :x86-64)
      (compile-to-x86-64-bytes program)
      (if (eq arch :arm64)
          (compile-to-aarch64-bytes program)
          (error "Unknown native architecture: ~S" arch))))

(defun compile-to-native (source &key (arch :x86-64) (output-file "a.out") (language :lisp)
                                  pass-pipeline print-pass-timings timing-stream
                                  print-opt-remarks opt-remarks-stream (opt-remarks-mode :all)
                                  print-pass-stats stats-stream trace-json-stream)
  "Compile SOURCE to a native Mach-O executable.
SOURCE can be a string (single expression) or a list of forms.
ARCH is :X86-64 or :ARM64.
OUTPUT-FILE is the path for the executable.
LANGUAGE is :LISP (default) or :PHP.

Returns the output file path on success."
  (let* ((native-target (%native-target-for-arch arch))
         (opts (%make-native-opts :pass-pipeline pass-pipeline
                                  :print-pass-timings print-pass-timings
                                  :timing-stream timing-stream
                                  :print-opt-remarks print-opt-remarks
                                  :opt-remarks-stream opt-remarks-stream
                                  :opt-remarks-mode opt-remarks-mode
                                  :print-pass-stats print-pass-stats
                                  :stats-stream stats-stream
                                  :trace-json-stream trace-json-stream))
         (result (%compile-native-source source native-target language opts))
         (program (compilation-result-program result))
         (code-bytes (%native-code-bytes-for-arch arch program))
         (builder (cl-cc/binary:make-mach-o-builder arch)))
    (%write-native-binary builder code-bytes output-file)))

(defun %native-file-language (input-file language)
  (if language
      language
      (let ((file-type (pathname-type input-file)))
        (if file-type
            (if (string= file-type "php")
                :php
                :lisp)
            :lisp))))

(defun %native-output-file (input-file output-file)
  (if output-file
      output-file
      (make-pathname :type nil :defaults input-file)))

(defun %native-read-lisp-forms-loop (stream eof-marker forms)
  (let ((form (read stream nil eof-marker)))
    (if (eq form eof-marker)
        (%native-reverse-list forms nil)
        (%native-read-lisp-forms-loop stream eof-marker (cons form forms)))))

(defun %native-reverse-list (items acc)
  (if (consp items)
      (%native-reverse-list (cdr items) (cons (car items) acc))
      acc))

(defun %native-read-character-stream-loop (in buf index limit)
  (if (< index limit)
      (let ((ch (read-char in nil nil)))
        (if ch
            (progn
              (setf (aref buf index) ch)
              (%native-read-character-stream-loop in buf (+ index 1) limit))
            buf))
      buf))

(defun %native-read-character-file (input-file)
  (let ((in (open input-file :direction :input :element-type 'character)))
    (unwind-protect
        (let ((buf (make-string (file-length in))))
          (%native-read-character-stream-loop in buf 0 (length buf)))
      (close in))))

(defun %native-read-lisp-file (input-file)
  (let ((in (open input-file :direction :input)))
    (unwind-protect
        (let ((*read-eval* nil))
          (%native-read-lisp-forms-loop in (list :eof) nil))
      (close in))))

(defun %native-read-file-source (input-file language)
  (if (eq language :php)
      (%native-read-character-file input-file)
      (%native-read-lisp-file input-file)))

(defun %compile-native-file-source (source target language opts)
  (if (eq language :php)
      (%compile-native-string source target :php opts)
      (%compile-native-lisp-forms source target opts)))

(defun compile-file-to-native (input-file &key (arch :x86-64) (output-file nil) (language nil)
                                          pass-pipeline print-pass-timings timing-stream
                                          print-opt-remarks opt-remarks-stream (opt-remarks-mode :all)
                                          print-pass-stats stats-stream trace-json-stream)
  "Compile a CL-CC source file to a native Mach-O executable.
INPUT-FILE is the path to the source file.
OUTPUT-FILE defaults to INPUT-FILE with no extension.
LANGUAGE is :LISP or :PHP. When nil, auto-detected from the file extension."
  (let* ((effective-language (%native-file-language input-file language))
         (output (%native-output-file input-file output-file))
         (source (%native-read-file-source input-file effective-language))
         (opts (%make-native-opts :pass-pipeline pass-pipeline
                                  :print-pass-timings print-pass-timings
                                  :timing-stream timing-stream
                                  :print-opt-remarks print-opt-remarks
                                  :opt-remarks-stream opt-remarks-stream
                                  :opt-remarks-mode opt-remarks-mode
                                  :print-pass-stats print-pass-stats
                                  :stats-stream stats-stream
                                  :trace-json-stream trace-json-stream))
         (cache-key (%compile-cache-key source arch effective-language))
         (cache-path (%compile-cache-path cache-key output))
         (native-target (%native-target-for-arch arch))
         (result (%compile-native-file-source source native-target effective-language opts))
         (program (compilation-result-program result))
         (code-bytes (%native-code-bytes-for-arch arch program))
         (builder (cl-cc/binary:make-mach-o-builder arch)))
    (ensure-directories-exist cache-path)
    (if (probe-file cache-path)
        (progn
          (format *error-output* "; cache hit ~A~%" cache-path)
          (%copy-file-bytes cache-path output)
          (%run-short-native-command (list "chmod" "+x" (namestring output)))
          output)
        (progn
          (%write-native-binary builder code-bytes output)
          (%copy-file-bytes output cache-path)
          output))))

;;; Typeclass macros (deftype-class, deftype-instance) are registered in
;;; pipeline-native-typeclass.lisp (loaded after this file).
