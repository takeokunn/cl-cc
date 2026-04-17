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
  (merge-pathnames #P".cache/cl-cc/native/"
                   (user-homedir-pathname))
  "Directory for cached native build outputs.")

(defun %compile-cache-key (source arch language)
  (format nil "~A-~A-~A"
          (content-hash source)
          arch
          language))

(defun %compile-cache-path (key output-file)
  (merge-pathnames
   (make-pathname :directory `(:relative ,key)
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

(defun compile-to-native (source &key (arch :x86-64) (output-file "a.out") (language :lisp) pass-pipeline print-pass-timings timing-stream print-opt-remarks opt-remarks-stream (opt-remarks-mode :all) print-pass-stats stats-stream trace-json-stream)
  "Compile SOURCE to a native Mach-O executable.
SOURCE can be a string (single expression) or a list of forms.
ARCH is :X86-64 or :ARM64.
OUTPUT-FILE is the path for the executable.
LANGUAGE is :LISP (default) or :PHP.

Returns the output file path on success."
  (let* ((result (if (stringp source)
                     (compile-string source :target :vm :language language
                                     :pass-pipeline pass-pipeline
                                     :print-pass-timings print-pass-timings
                                      :timing-stream timing-stream
                                      :print-pass-stats print-pass-stats
                                      :stats-stream stats-stream
                                      :trace-json-stream trace-json-stream
                                      :print-opt-remarks print-opt-remarks
                                     :opt-remarks-stream opt-remarks-stream
                                     :opt-remarks-mode opt-remarks-mode)
                     (compile-expression source :target :vm
                                         :pass-pipeline pass-pipeline
                                         :print-pass-timings print-pass-timings
                                          :timing-stream timing-stream
                                          :print-pass-stats print-pass-stats
                                          :stats-stream stats-stream
                                          :trace-json-stream trace-json-stream
                                          :print-opt-remarks print-opt-remarks
                                         :opt-remarks-stream opt-remarks-stream
                                         :opt-remarks-mode opt-remarks-mode)))
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
                     (compile-toplevel-forms source :target :vm
                                              :pass-pipeline pass-pipeline
                                              :print-pass-timings print-pass-timings
                                               :timing-stream timing-stream
                                               :print-pass-stats print-pass-stats
                                               :stats-stream stats-stream
                                               :trace-json-stream trace-json-stream
                                               :print-opt-remarks print-opt-remarks
                                              :opt-remarks-stream opt-remarks-stream
                                              :opt-remarks-mode opt-remarks-mode)))
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

;;; Typeclass Macros (Phase 4) — registered here because cl-cc/type loads before compiler

(register-macro 'deftype-class
  (lambda (form env)
    (declare (ignore env))
    ;; Syntax: (deftype-class Name [(:super S1 S2)] (type-param) method-specs...)
    (let* ((class-name (second form))
           (rest (cddr form))
           ;; Check for optional superclass declaration
           (superclasses (when (and (consp (first rest))
                                    (eq (caar rest) :super))
                           (cdar rest)))
           (rest2 (if superclasses (cdr rest) rest))
           (type-param-list (first rest2))
           (type-param (first type-param-list))
           (method-specs (rest rest2))
           (methods-forms nil)
           (default-forms nil))
      (dolist (spec method-specs)
        (let ((method-name (first spec))
              (type-spec (second spec))
              (rest-spec (cddr spec)))
          (push `(cons ',method-name
                       (cl-cc/type:parse-type-specifier ',type-spec))
                methods-forms)
          (when (and rest-spec (eq (first rest-spec) :default))
            (push `(cons ',method-name ,(second rest-spec))
                  default-forms))))
      `(progn
         (cl-cc/type:register-typeclass
          ',class-name
          (cl-cc/type:make-type-class
           :name ',class-name
           :type-param (cl-cc/type:make-type-variable ',type-param)
           :methods (list ,@(nreverse methods-forms))
           :defaults (list ,@(nreverse default-forms))
           :superclasses ',superclasses))
         ',class-name))))

(register-macro 'deftype-instance
  (lambda (form env)
    (declare (ignore env))
    (destructuring-bind (_ class-name type-spec &rest method-impls) form
      (declare (ignore _))
      (let* ((dict-var (intern (format nil "*~A-~A-DICT*"
                                       class-name type-spec)
                               :cl-cc))
             (method-forms
              (mapcar (lambda (impl)
                        (destructuring-bind (name impl-form) impl
                          `(cons ',name ,impl-form)))
                      method-impls)))
        `(progn
           ;; Register in type inference registry
           (cl-cc/type:register-typeclass-instance
            ',class-name
            (cl-cc/type:parse-type-specifier ',type-spec)
            (list ,@method-forms))
           ;; Store dictionary as global variable for VM access
           (defvar ,dict-var (list ,@method-forms))
           ',class-name)))))
