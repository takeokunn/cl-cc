;;;; pipeline-incremental.lisp — source hashing and deps cache

(in-package :cl-cc/pipeline)

(defparameter *incremental-cache-directory* #p".cl-cc-cache/"
  "Directory used for incremental compilation hash and dependency sidecars.")

(defun %incremental-read-file-string (path)
  "Return PATH contents as a string."
  (with-open-file (in path :direction :input :element-type 'character)
    (let* ((size (file-length in))
           (buffer (make-string size)))
      (read-sequence buffer in)
      buffer)))

(defun %incremental-djb2-hash-string (text)
  "Return a stable 64-bit DJB2 hash for TEXT."
  (let ((hash 5381)
        (mask (1- (ash 1 64))))
    (loop for ch across text do
      (setf hash (logand mask (+ (ash hash 5) hash (char-code ch)))))
    hash))

(defun pipeline-source-hash (path)
  "Return a deterministic hexadecimal content hash for source file PATH.

CL-CC intentionally avoids external crypto dependencies here. The API is named
generically so a host SHA-256 primitive can replace this stable fallback later."
  (format nil "~16,'0X" (%incremental-djb2-hash-string
                         (%incremental-read-file-string path))))

(defun %incremental-cache-directory (cache-dir)
  (merge-pathnames (or cache-dir *incremental-cache-directory*) (uiop:getcwd)))

(defun %incremental-key-for-path (path)
  (format nil "~A" (%incremental-djb2-hash-string (namestring (truename path)))))

(defun pipeline-cache-hash-path (source-file &key cache-dir)
  "Return the hash sidecar pathname for SOURCE-FILE."
  (merge-pathnames (format nil "~A.hash" (%incremental-key-for-path source-file))
                   (%incremental-cache-directory cache-dir)))

(defun pipeline-deps-path (source-file &key cache-dir)
  "Return the .deps sidecar pathname for SOURCE-FILE."
  (merge-pathnames (format nil "~A.deps" (%incremental-key-for-path source-file))
                   (%incremental-cache-directory cache-dir)))

(defun %incremental-read-first-line (path)
  (when (probe-file path)
    (with-open-file (in path :direction :input)
      (read-line in nil nil))))

(defun pipeline-incremental-current-p (source-file &key cache-dir)
  "Return T when SOURCE-FILE's cached hash matches its current content hash."
  (let ((cached (%incremental-read-first-line
                 (pipeline-cache-hash-path source-file :cache-dir cache-dir))))
    (and cached (string= cached (pipeline-source-hash source-file)))))

(defun pipeline-write-deps (source-file dependencies &key cache-dir)
  "Write SOURCE-FILE dependency sidecar as `source-file :depends-on (...)`."
  (let ((deps-path (pipeline-deps-path source-file :cache-dir cache-dir)))
    (ensure-directories-exist deps-path)
    (with-open-file (out deps-path :direction :output :if-exists :supersede
                                   :if-does-not-exist :create)
      (format out "~S :depends-on (~{~S~^ ~})~%"
              (namestring (truename source-file))
              (mapcar (lambda (dep) (namestring (truename dep))) dependencies)))
    deps-path))

(defun pipeline-record-incremental-state (source-file &key dependencies cache-dir)
  "Persist SOURCE-FILE's content hash and dependency list."
  (let ((hash-path (pipeline-cache-hash-path source-file :cache-dir cache-dir)))
    (ensure-directories-exist hash-path)
    (with-open-file (out hash-path :direction :output :if-exists :supersede
                                    :if-does-not-exist :create)
      (format out "~A~%" (pipeline-source-hash source-file)))
    (pipeline-write-deps source-file (or dependencies '()) :cache-dir cache-dir)
    hash-path))
