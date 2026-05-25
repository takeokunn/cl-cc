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
  "Write SOURCE-FILE dependency sidecar as a single list: (source :depends-on (dep ...))."
  (let ((deps-path (pipeline-deps-path source-file :cache-dir cache-dir)))
    (ensure-directories-exist deps-path)
    (with-open-file (out deps-path :direction :output :if-exists :supersede
                                    :if-does-not-exist :create)
      (format out "~S~%"
              (list (namestring (truename source-file))
                    :depends-on
                    (mapcar (lambda (dep) (namestring (truename dep))) dependencies))))
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

;;; ── FR-640: Pipeline integration stubs ─────────────────────────────────────
;;;
;;; Wire the incremental compilation cache into the compilation pipeline.
;;; These functions bridge the gap between the source-hashing infrastructure
;;; above and the compile pipeline's decision of whether to re-compile.

(defvar *incremental-dirty-reason* nil
  "FR-640: Reason string for why incremental state is dirty, or NIL if clean.")

(defvar *incremental-dirty-state* nil
  "FR-640: T when the incremental compilation state is dirty (needs recompile).")

(defun incremental-state-dirty-p ()
  "FR-640: Return T when the incremental compilation state indicates a recompile
is needed."
  *incremental-dirty-state*)

(defun incremental-state-reason ()
  "FR-640: Return a human-readable reason string for why incremental state is
dirty, or NIL if the state is clean."
  (values *incremental-dirty-reason*
          (not (null *incremental-dirty-reason*))))

(defun prepare-incremental-compilation (source-file &key cache-dir)
  "FR-640: Prepare incremental compilation for SOURCE-FILE.
Returns T when recompilation is needed (state is dirty), NIL when cached state
is still current and compilation can be skipped.
Sets *INCREMENTAL-DIRTY-STATE* and *INCREMENTAL-DIRTY-REASON*."
  ;; Reset state at function start to avoid stale previous values
  (setf *incremental-dirty-state* nil
        *incremental-dirty-reason* nil)
  (let* ((cache-dir (or cache-dir *incremental-cache-directory*))
         (dep-files nil))
    (cond
      ((not (probe-file source-file))
       (setf *incremental-dirty-state* nil
             *incremental-dirty-reason* "source file does not exist")
       nil)
       ((not (pipeline-incremental-current-p source-file :cache-dir cache-dir))
        (setf *incremental-dirty-state* t
              *incremental-dirty-reason* "source hash changed")
        t)
       (t
        ;; Check dependency freshness
        (let ((deps-path (pipeline-deps-path source-file :cache-dir cache-dir)))
          (when (probe-file deps-path)
            (with-open-file (in deps-path :direction :input)
              (let ((form (read in nil nil)))
                ;; Form is: "source-path :depends-on (dep1 dep2 ...)"
                (when (and (consp form) (eq (second form) :depends-on))
                  (setf dep-files (third form)))))))
        (dolist (dep dep-files)
          ;; Only recompile if a dependency is MISSING or its content hash changed
          (when (or (not (probe-file dep))
                    (not (pipeline-incremental-current-p dep :cache-dir cache-dir)))
            (setf *incremental-dirty-state* t
                  *incremental-dirty-reason*
                  (format nil "dependency ~A changed or missing" dep))
            (return t)))
        (when (not *incremental-dirty-state*)
          (setf *incremental-dirty-state* nil
                *incremental-dirty-reason* nil))
        *incremental-dirty-state*))))

(defun commit-incremental-compilation (source-file &key dependencies cache-dir)
  "FR-640: Commit the incremental compilation state for SOURCE-FILE after a
successful compilation. Writes the hash and dependency sidecar files."
  (pipeline-record-incremental-state source-file
                                     :dependencies dependencies
                                     :cache-dir cache-dir)
  (setf *incremental-dirty-state* nil
        *incremental-dirty-reason* nil)
  t)

;;; ── FR-641: Hot Reload infrastructure ─────────────────────────────────────
;;;
;;; Provides a lock-protected function entry that can be atomically swapped
;;; via HOT-RELOAD-SWAP and safely called via HOT-RELOAD-CALL.  Active-call
;;; tracking prevents swaps while the function is executing.

(defstruct (hot-reload-entry (:constructor make-hot-reload-entry (name fn)))
  "FR-641: Lock-protected hot-reloadable function entry.
NAME is a debug identifier; FN is the current function implementation.
The LOCK protects SWAP operations; ACTIVE-COUNT tracks concurrent callers
to prevent swapping during execution."
  (name        nil :type symbol :read-only t)
  (fn          nil :type function)
  (lock        (sb-thread:make-mutex :name (format nil "hot-reload ~A" name))
               :type sb-thread:mutex :read-only t)
  (active-count 0 :type fixnum))

(defun hot-reload-swap (entry new-fn)
  "FR-641: Atomically swap the function in ENTRY to NEW-FN.
Blocks until no active callers are using the entry (active-count = 0).
Returns the old function."
  ;; Poll with yield until all active callers drain
  (loop while (plusp (hot-reload-entry-active-count entry))
        do (sleep 0.001))
  (sb-thread:with-mutex ((hot-reload-entry-lock entry))
    (let ((old (hot-reload-entry-fn entry)))
      (setf (hot-reload-entry-fn entry) new-fn)
      old)))

(defun hot-reload-call (entry)
  "FR-641: Call the current function in ENTRY with active-call tracking.
Increments ACTIVE-COUNT before the call, decrements after.
Returns whatever the function returns."
  (sb-thread:with-mutex ((hot-reload-entry-lock entry))
    (incf (hot-reload-entry-active-count entry)))
  (unwind-protect
       (funcall (hot-reload-entry-fn entry))
    (sb-thread:with-mutex ((hot-reload-entry-lock entry))
      (decf (hot-reload-entry-active-count entry)))))
