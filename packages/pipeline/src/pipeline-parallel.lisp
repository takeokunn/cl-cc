;;;; packages/pipeline/src/pipeline-parallel.lisp — Parallel compilation (FR-632)

(in-package :cl-cc/pipeline)

(defparameter *num-workers*
  (max 1 (or (ignore-errors
               (let ((value (cl-cc/runtime:rt-getenv "CLCC_PARALLEL")))
                 (and value (parse-integer value :junk-allowed nil))))
             (ignore-errors
               (let ((sym (find-symbol "CPU-COUNT" :uiop)))
                 (and sym (fboundp sym) (funcall sym))))
             1))
  "Default number of native compilation workers.")

(defun %parallel-file-namestring (file)
  (namestring (truename file)))

(defun %read-source-forms-for-deps (file)
  "Read top-level forms from FILE for dependency analysis."
  (handler-case
      (with-open-file (in file :direction :input)
        (let ((eof (list :eof))
              (forms nil)
              (*read-eval* nil))
          (loop for form = (read in nil eof)
                until (eq form eof)
                do (push form forms))
          (nreverse forms)))
    (error () nil)))

(defun %form-source-dependencies (form)
  "Extract file-like dependencies from one top-level FORM."
  (when (consp form)
    (case (car form)
      ((load require)
       (let ((dep (second form)))
         (when (or (stringp dep) (pathnamep dep))
           (list (namestring dep)))))
      (otherwise nil))))

(defun build-source-dependency-graph (files)
  "Return an EQUAL hash table FILE -> dependency FILE list from source analysis.

The graph recognizes simple top-level LOAD/REQUIRE forms. Unknown or external
dependencies are kept as namestrings but do not block scheduling unless they are
also present in FILES. Macro environments are assumed read-only during this
parallel native phase."
  (let ((graph (make-hash-table :test #'equal))
        (known (make-hash-table :test #'equal)))
    (dolist (file files)
      (setf (gethash (%parallel-file-namestring file) known) t))
    (dolist (file files graph)
      (let* ((key (%parallel-file-namestring file))
             (base (pathname file))
             (deps (loop for form in (%read-source-forms-for-deps file)
                         nconc (%form-source-dependencies form))))
        (setf (gethash key graph)
              (remove-if-not (lambda (dep)
                               (gethash (namestring (merge-pathnames dep base)) known))
                             (mapcar (lambda (dep)
                                       (namestring (merge-pathnames dep base)))
                                     deps)))))))

(defun independent-source-files (files &optional (graph (build-source-dependency-graph files)))
  "Return FILES that have no known inter-file dependencies."
  (remove-if-not (lambda (file)
                   (null (gethash (%parallel-file-namestring file) graph)))
                 files))

(defun %parallel-ready-file (pending completed graph)
  "Return a pending file whose known dependencies are all COMPLETED."
  (find-if (lambda (file)
             (every (lambda (dep) (gethash dep completed))
                    (gethash (%parallel-file-namestring file) graph)))
           pending))

(defun %parallel-worker-count (workers files)
  (max 1 (min (length files)
              (or workers *num-workers* 1))))

(defun compile-files-to-native-parallel (files &key (workers *num-workers*) (arch :x86-64)
                                                output-directory language
                                                pass-pipeline speed inline-threshold-scale
                                                block-compile compilation-tier)
  "Compile FILES with a dependency-aware worker pool.

Each worker takes one ready file at a time and invokes COMPILE-FILE-TO-NATIVE
with local dynamic compilation state. Shared macro tables are treated as
read-only during the parallel phase. Returns an alist (FILE . OUTPUT)."
  (let* ((graph (build-source-dependency-graph files))
         (pending (mapcar #'%parallel-file-namestring files))
         (completed (make-hash-table :test #'equal))
         (results nil)
         (errors nil)
          (lock (cl-cc/runtime:rt-make-mutex :name "cl-cc parallel compiler"))
         (worker-count (%parallel-worker-count workers files)))
    (labels ((file-path (name) (find name files :key #'%parallel-file-namestring :test #'string=))
             (output-path (file)
               (when output-directory
                 (merge-pathnames (make-pathname :name (pathname-name file) :type nil)
                                  output-directory)))
             (next-file ()
               (cl-cc/runtime::rt-with-mutex (lock)
                 (let ((ready (%parallel-ready-file pending completed graph)))
                   (when ready
                     (setf pending (remove ready pending :test #'string=))
                     ready))))
             (record-result (name output)
               (cl-cc/runtime::rt-with-mutex (lock)
                 (setf (gethash name completed) t)
                 (push (cons name output) results)))
             (record-error (name condition)
               (cl-cc/runtime::rt-with-mutex (lock)
                 (push (cons name condition) errors)
                 (setf (gethash name completed) t)))
             (worker ()
               (loop for name = (next-file)
                     while name
                     do (handler-case
                            (let* ((file (file-path name))
                                   (*current-compilation-arena* nil)
                                   (out (compile-file-to-native
                                         file
                                         :arch arch
                                         :output-file (output-path file)
                                         :language language
                                         :pass-pipeline pass-pipeline
                                         :speed speed
                                         :inline-threshold-scale (or inline-threshold-scale 1)
                                         :block-compile block-compile
                                         :compilation-tier (or compilation-tier *compilation-tier*))))
                              (record-result name out))
                          (error (e) (record-error name e))))))
      (let ((threads (loop for i below worker-count
                           collect (sb-thread:make-thread #'worker
                                                          :name (format nil "cl-cc-compile-worker-~D" i)))))
        (dolist (thread threads) (sb-thread:join-thread thread)))
      (when errors
        (error "Parallel compilation failed: ~S" errors))
      (nreverse results))))
