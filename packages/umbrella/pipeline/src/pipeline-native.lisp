(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Compiler Pipeline — Native Code Generation and Typeclass Macros
;;;
;;; Contains: %write-native-binary, *compile-cache-root*, %compile-cache-key,
;;; %compile-cache-path, %copy-file-bytes, compile-to-native,
;;; compile-file-to-native, and typeclass macros (define-typeclass,
;;; define-typeclass-instance) registered at pipeline load time.
;;;
;;; Core pipeline (compile-expression, compile-string, run-string, our-eval,
;;; run-string-typed) is in pipeline.lisp (loads before).
;;;
;;; Load order: after pipeline.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; Native Executable Generation (Mach-O)

(defun %write-native-binary (builder code-bytes output-path)
  "Finalize BUILDER with CODE-BYTES, write Mach-O to OUTPUT-PATH, and mark it executable."
  (cl-cc/binary:add-text-segment builder code-bytes)
  (cl-cc/binary:add-symbol builder "_main" :value 0 :type #x0F :sect 1)
  (let ((mach-o-bytes (cl-cc/binary:build-mach-o builder code-bytes)))
    (cl-cc/binary:write-mach-o-file output-path mach-o-bytes))
  (uiop:run-program (list "chmod" "+x" (namestring output-path)) :ignore-error-status t)
  output-path)

(defparameter *compile-cache-root*
  (merge-pathnames #P".cache/cl-cc/native/" (truename #P"./"))
  "Directory for cached native build outputs.")

(defun %compile-cache-key (source arch language)
  (format nil "~A-~A-~A"
          (content-hash source)
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
      (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
        (loop for count = (read-sequence buffer in)
              while (plusp count) do
                (write-sequence buffer out :end count)))))
  to)

(defun %top-level-in-package-form-p (form)
  "Return T when FORM is an in-package declaration ignored by top-level compilation." 
  (and (consp form)
       (eq (car form) 'in-package)))

(defun %non-package-top-level-forms (forms)
  "Return FORMS with in-package declarations removed, mirroring compile-toplevel-forms." 
  (remove-if #'%top-level-in-package-form-p forms))

(defun %maybe-compile-native-via-cps (form &key (target :x86_64) type-check (safety 1) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  "Try compiling FORM through a narrow CPS-backed native path.
Returns two values: the compilation result and whether the CPS-native path was used."
  (let ((ast (optimize-ast (%prepare-ast form))))
    (if (%cps-native-compile-safe-ast-p ast)
         (let ((cps (maybe-cps-transform ast)))
           (if cps
               (values (compile-expression (%cps-identity-entry-form cps)
                                           :target target :type-check type-check :safety safety
                                           :pass-pipeline pass-pipeline
                                           :print-pass-timings print-pass-timings
                                          :timing-stream timing-stream
                                          :print-pass-stats print-pass-stats
                                          :stats-stream stats-stream
                                          :trace-json-stream trace-json-stream
                                          :print-opt-remarks print-opt-remarks
                                          :opt-remarks-stream opt-remarks-stream
                                          :opt-remarks-mode opt-remarks-mode)
                      t)
              (values nil nil)))
        (values nil nil))))

(defun compile-to-native (source &key (arch :x86-64) (output-file "a.out") (language :lisp) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  "Compile SOURCE to a native Mach-O executable.
SOURCE can be a string (single expression) or a list of forms.
ARCH is :X86-64 or :ARM64.
OUTPUT-FILE is the path for the executable.
LANGUAGE is :LISP (default) or :PHP.

Returns the output file path on success."
  (let* ((result (if (stringp source)
                     (if (eq language :lisp)
                         (let ((forms (%non-package-top-level-forms (parse-all-forms source))))
                           (if (= (length forms) 1)
                               (multiple-value-bind (cps-result cps-used)
                                   (%maybe-compile-native-via-cps (first forms)
                                                                  :target :vm
                                                                  :pass-pipeline pass-pipeline
                                                                  :print-pass-timings print-pass-timings
                                                                  :timing-stream timing-stream
                                                                  :print-pass-stats print-pass-stats
                                                                  :stats-stream stats-stream
                                                                  :trace-json-stream trace-json-stream
                                                                  :print-opt-remarks print-opt-remarks
                                                                  :opt-remarks-stream opt-remarks-stream
                                                                  :opt-remarks-mode opt-remarks-mode)
                                 (if cps-used
                                     cps-result
                                     (compile-string source :target :vm :language language
                                                     :pass-pipeline pass-pipeline
                                                     :print-pass-timings print-pass-timings
                                                     :timing-stream timing-stream
                                                     :print-pass-stats print-pass-stats
                                                     :stats-stream stats-stream
                                                     :trace-json-stream trace-json-stream
                                                     :print-opt-remarks print-opt-remarks
                                                     :opt-remarks-stream opt-remarks-stream
                                                     :opt-remarks-mode opt-remarks-mode)))
                               (compile-string source :target :vm :language language
                                               :pass-pipeline pass-pipeline
                                               :print-pass-timings print-pass-timings
                                               :timing-stream timing-stream
                                               :print-pass-stats print-pass-stats
                                               :stats-stream stats-stream
                                               :trace-json-stream trace-json-stream
                                               :print-opt-remarks print-opt-remarks
                                               :opt-remarks-stream opt-remarks-stream
                                               :opt-remarks-mode opt-remarks-mode)))
                         (compile-string source :target :vm :language language
                                         :pass-pipeline pass-pipeline
                                         :print-pass-timings print-pass-timings
                                         :timing-stream timing-stream
                                         :print-pass-stats print-pass-stats
                                         :stats-stream stats-stream
                                         :trace-json-stream trace-json-stream
                                         :print-opt-remarks print-opt-remarks
                                         :opt-remarks-stream opt-remarks-stream
                                         :opt-remarks-mode opt-remarks-mode))
                     (multiple-value-bind (cps-result cps-used)
                         (%maybe-compile-native-via-cps source
                                                        :target :vm
                                                        :pass-pipeline pass-pipeline
                                                        :print-pass-timings print-pass-timings
                                                        :timing-stream timing-stream
                                                        :print-pass-stats print-pass-stats
                                                        :stats-stream stats-stream
                                                        :trace-json-stream trace-json-stream
                                                        :print-opt-remarks print-opt-remarks
                                                        :opt-remarks-stream opt-remarks-stream
                                                        :opt-remarks-mode opt-remarks-mode)
                       (if cps-used
                           cps-result
                           (compile-expression source :target :vm
                                               :pass-pipeline pass-pipeline
                                               :print-pass-timings print-pass-timings
                                               :timing-stream timing-stream
                                               :print-pass-stats print-pass-stats
                                               :stats-stream stats-stream
                                               :trace-json-stream trace-json-stream
                                               :print-opt-remarks print-opt-remarks
                                               :opt-remarks-stream opt-remarks-stream
                                               :opt-remarks-mode opt-remarks-mode)))))
          (program    (compilation-result-program result))
         (code-bytes (ecase arch
                       (:x86-64 (compile-to-x86-64-bytes program))
                       (:arm64  (compile-to-aarch64-bytes program))))
         (builder    (cl-cc/binary:make-mach-o-builder arch)))
    (%write-native-binary builder code-bytes output-file)))

(defun compile-file-to-native (input-file &key (arch :x86-64) (output-file nil) (language nil) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  "Compile a CL-CC source file to a native Mach-O executable.
INPUT-FILE is the path to the source file.
OUTPUT-FILE defaults to INPUT-FILE with no extension.
LANGUAGE is :LISP or :PHP. When nil, auto-detected from the file extension."
  (let* ((effective-language (or language
                                 (cond ((string= (pathname-type input-file) "php") :php)
                                       (t :lisp))))
         (output (or output-file
                     (make-pathname :type nil :defaults input-file)))
         (source (if (eq effective-language :php)
                     (with-open-file (in input-file :direction :input
                                                     :element-type 'character)
                       (let ((buf (make-string (file-length in))))
                         (read-sequence buf in)
                         buf))
                     (with-open-file (in input-file :direction :input)
                       (let ((forms nil)
                             (*read-eval* nil))
                         (handler-case
                             (loop (push (read in) forms))
                           (end-of-file () nil))
                         (nreverse forms)))))
         (cache-key (%compile-cache-key source arch effective-language))
         (cache-path (%compile-cache-path cache-key output))
         (result (if (eq effective-language :php)
                     (compile-string source :target :vm :language :php
                                     :pass-pipeline pass-pipeline
                                     :print-pass-timings print-pass-timings
                                     :timing-stream timing-stream
                                     :print-pass-stats print-pass-stats
                                     :stats-stream stats-stream
                                     :trace-json-stream trace-json-stream
                                     :print-opt-remarks print-opt-remarks
                                     :opt-remarks-stream opt-remarks-stream
                                     :opt-remarks-mode opt-remarks-mode)
                     (let ((forms (%non-package-top-level-forms source)))
                       (if (= (length forms) 1)
                           (multiple-value-bind (cps-result cps-used)
                               (%maybe-compile-native-via-cps (first forms)
                                                              :target :vm
                                                              :pass-pipeline pass-pipeline
                                                              :print-pass-timings print-pass-timings
                                                              :timing-stream timing-stream
                                                              :print-pass-stats print-pass-stats
                                                              :stats-stream stats-stream
                                                              :trace-json-stream trace-json-stream
                                                              :print-opt-remarks print-opt-remarks
                                                              :opt-remarks-stream opt-remarks-stream
                                                              :opt-remarks-mode opt-remarks-mode)
                             (if cps-used
                                 cps-result
                                  (compile-toplevel-forms (%maybe-cps-toplevel-forms source :target :vm) :target :vm
                                                          :pass-pipeline pass-pipeline
                                                          :print-pass-timings print-pass-timings
                                                         :timing-stream timing-stream
                                                         :print-pass-stats print-pass-stats
                                                         :stats-stream stats-stream
                                                         :trace-json-stream trace-json-stream
                                                         :print-opt-remarks print-opt-remarks
                                                         :opt-remarks-stream opt-remarks-stream
                                                         :opt-remarks-mode opt-remarks-mode)))
                            (compile-toplevel-forms (%maybe-cps-toplevel-forms source :target :vm) :target :vm
                                                   :pass-pipeline pass-pipeline
                                                   :print-pass-timings print-pass-timings
                                                  :timing-stream timing-stream
                                                  :print-pass-stats print-pass-stats
                                                  :stats-stream stats-stream
                                                  :trace-json-stream trace-json-stream
                                                  :print-opt-remarks print-opt-remarks
                                                  :opt-remarks-stream opt-remarks-stream
                                                  :opt-remarks-mode opt-remarks-mode)))))
          (program    (compilation-result-program result))
         (code-bytes (ecase arch
                       (:x86-64 (compile-to-x86-64-bytes program))
                       (:arm64  (compile-to-aarch64-bytes program))))
         (builder    (cl-cc/binary:make-mach-o-builder arch)))
    (ensure-directories-exist cache-path)
    (if (probe-file cache-path)
        (progn
          (format *error-output* "; cache hit ~A~%" cache-path)
          (%copy-file-bytes cache-path output)
          (uiop:run-program (list "chmod" "+x" (namestring output)) :ignore-error-status t)
          output)
        (progn
          (%write-native-binary builder code-bytes output)
          (%copy-file-bytes output cache-path)
          output))))

;;; Typeclass macros (deftype-class, deftype-instance) are registered in
;;; pipeline-native-typeclass.lisp (loaded after this file).
