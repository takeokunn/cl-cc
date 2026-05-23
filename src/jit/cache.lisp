;;;; src/jit/cache.lisp — FR-554 JIT Code Cache Persistence
;;;; Serialize/deserialize JIT-compiled code to disk cache.
;;;; Chromium V8 Code Cache / Android ART AOT compiler equivalent.

(in-package :cl-cc/jit)

;;; ──── Configuration ────
(defvar *jit-cache-dir* nil
  "Directory for JIT code cache files. Default: ~/.cache/cl-cc/jit/")

(defun jit-cache-dir ()
  "Return (and create if needed) the JIT cache directory."
  (or *jit-cache-dir*
      (setf *jit-cache-dir*
            (merge-pathnames ".cache/cl-cc/jit/"
                            (user-homedir-pathname)))))

;;; ──── Cache key computation ────
(defun compute-cache-key (source-hash cpu-features optimize-level)
  "Compute a unique cache key from source hash + CPU features + optimization level.
Returns a string suitable for a filename."
  (format nil "~A-~A-O~D"
          source-hash
          cpu-features
          optimize-level))

(defun cache-file-path (cache-key)
  "Return the full path to the cache file for CACHE-KEY."
  (merge-pathnames (make-pathname :name cache-key :type "jit")
                   (jit-cache-dir)))

;;; ──── Code cache structure ────
(defstruct (jit-cache-entry (:conc-name jce-))
  "A cached JIT compilation result."
  (source-hash "" :type string)     ; SHA256 of source
  (cpu-features "" :type string)    ; CPU feature string (e.g., "avx2,sse4.2")
  (optimize-level 2 :type fixnum)   ; Optimization level used
  (compiled-code nil)               ; The compiled code bytes (vector)
  (stack-maps nil)                  ; Associated stack maps
  (timestamp 0 :type integer)       ; Unix timestamp of compilation
  (signature nil))                  ; Cryptographic signature (FR-378)

;;; ──── In-memory cache ────
(defvar *jit-cache* (make-hash-table :test #'equal)
  "In-memory cache: cache-key → jit-cache-entry.")

(defun jit-cache-lookup (source-hash cpu-features optimize-level)
  "Look up a cached JIT compilation result.
Returns the jit-cache-entry or NIL if not found."
  (let* ((key (compute-cache-key source-hash cpu-features optimize-level))
         (entry (or (gethash key *jit-cache*)
                    (jit-cache-deserialize key))))
    (when entry
      (setf (gethash key *jit-cache*) entry))
    entry))

(defun jit-cache-insert (entry)
  "Insert a JIT compilation result into the cache.
Both in-memory and on-disk."
  (let ((key (compute-cache-key (jce-source-hash entry)
                                (jce-cpu-features entry)
                                (jce-optimize-level entry))))
    (setf (gethash key *jit-cache*) entry)
    (jit-cache-serialize key entry)))

;;; ──── Serialization ────
(defun jit-cache-serialize (cache-key entry)
  "Serialize JIT-CACHE-ENTRY to disk under CACHE-KEY.
Writes compiled code bytes + metadata to a .jit file."
  (let ((path (cache-file-path cache-key)))
    (ensure-directories-exist path)
    (with-open-file (out path
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :element-type '(unsigned-byte 8))
      ;; Write magic bytes: #xCL #xCC #xJIT
      (write-byte #x43 out) ; 'C'
      (write-byte #x4C out) ; 'L'
      (write-byte #x4A out) ; 'J'
      ;; Write header: version, flags, size
      (write-byte 1 out) ; version
      (write-byte (jce-optimize-level entry) out)
      ;; Write code length (32-bit LE)
      (let ((code (jce-compiled-code entry)))
        (write-sequence (encode-int32 (if code (length code) 0)) out)
        ;; Write code bytes
        (when code
          (write-sequence code out))))
    path))

(defun jit-cache-deserialize (cache-key)
  "Deserialize a JIT cache entry from disk.
Returns jit-cache-entry or NIL if file not found/corrupt."
  (let ((path (cache-file-path cache-key)))
    (when (probe-file path)
      (with-open-file (in path
                          :direction :input
                          :element-type '(unsigned-byte 8))
        (handler-case
            (let ((magic1 (read-byte in))
                  (magic2 (read-byte in))
                  (magic3 (read-byte in)))
              (unless (and (= magic1 #x43) (= magic2 #x4C) (= magic3 #x4A))
                (return-from jit-cache-deserialize nil))
              (let ((version (read-byte in))
                    (opt-level (read-byte in)))
                (declare (ignore version))
                ;; Read code length
                (let ((code-len (decode-int32 in)))
                  ;; Read code bytes
                  (let ((code (make-array code-len
                                          :element-type '(unsigned-byte 8))))
                    (read-sequence code in)
                    (make-jit-cache-entry
                     :source-hash cache-key
                     :optimize-level opt-level
                     :compiled-code code
                     :timestamp (get-universal-time))))))
          (error ()
            nil))))))

;;; ──── Helpers ────
(defun encode-int32 (value)
  "Encode a 32-bit integer as 4 bytes (little-endian)."
  (let ((buf (make-array 4 :element-type '(unsigned-byte 8))))
    (setf (aref buf 0) (logand value #xFF)
          (aref buf 1) (logand (ash value -8) #xFF)
          (aref buf 2) (logand (ash value -16) #xFF)
          (aref buf 3) (logand (ash value -24) #xFF))
    buf))

(defun decode-int32 (stream)
  "Read a 32-bit little-endian integer from STREAM."
  (let ((b0 (read-byte stream))
        (b1 (read-byte stream))
        (b2 (read-byte stream))
        (b3 (read-byte stream)))
    (logior b0 (ash b1 8) (ash b2 16) (ash b3 24))))

;;; ──── Cache statistics ────
(defun jit-cache-stats ()
  "Return cache hit/miss statistics."
  (let ((hits 0) (misses 0)
        (total-size 0))
    (maphash (lambda (k v)
               (declare (ignore k))
               (let ((code (jce-compiled-code v)))
                 (when code
                   (incf total-size (length code)))))
             *jit-cache*)
    (list :entries (hash-table-count *jit-cache*)
          :total-bytes total-size)))

(defun jit-cache-clear ()
  "Clear the in-memory JIT cache."
  (clrhash *jit-cache*)
  (values))
