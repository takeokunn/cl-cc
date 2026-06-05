(in-package :cl-cc/vm)

;;; ─── FR-899/FR-902: FASL paging hooks and PGO persistence ────────────────
;;;
;;; This file provides FASL demand-paging hooks (fasl-toc-read,
;;; fasl-page-fault, load-fasl-demand-paged) and the lightweight PGO
;;; persistence layer (%pgo-write-object, %pgo-read-object, save-pgo-data,
;;; load-pgo-data).  Extracted from vm.lisp to allow cl-cc-vm.asd to load
;;; these before the main vm.lisp file.
;;;

(defvar *fasl-toc-enabled* nil
  "When true, FASL loading may consult a table-of-contents for demand paging.")

(defvar *fasl-preload-forms* nil
  "List of FASL TOC keys/forms to eagerly preload before demand paging.")

(defvar *pgo-data-path* #P".cl-cc-pgo.msgpack"
  "Default path for lightweight persisted PGO data.")

(defstruct fasl-toc
  "Minimal FASL table-of-contents descriptor for demand-loading hooks."
  (path nil)
  (entries (make-hash-table :test #'equal))
  (mapping nil))

(defun fasl-toc-read (path)
  "Return a minimal FASL TOC for PATH.
This intentionally accepts a simple leading plist `(:toc ((key offset length) ...))'
when present and otherwise returns an empty TOC descriptor."
  (let ((entries (make-hash-table :test #'equal)))
    (when (probe-file path)
      (ignore-errors
        (with-open-file (in path :direction :input :element-type 'character)
          (let* ((header (read in nil nil))
                 (toc-entries (and (consp header)
                                   (or (getf header :toc) (getf header :entries)))))
            (when toc-entries
              (dolist (entry toc-entries)
                (destructuring-bind (key offset length &rest metadata) entry
                  (setf (gethash key entries)
                        (list :offset offset :length length :metadata metadata)))))))))
    (make-fasl-toc :path path :entries entries)))

(defun fasl-page-fault (toc key)
  "Demand-load KEY from TOC and return its byte vector, or NIL if unavailable."
  (let ((entry (and toc (gethash key (fasl-toc-entries toc)))))
    (when entry
      (let ((offset (getf entry :offset))
            (length (getf entry :length)))
        (when (and (integerp offset) (integerp length) (probe-file (fasl-toc-path toc)))
          (with-open-file (in (fasl-toc-path toc) :direction :input :element-type '(unsigned-byte 8))
            (file-position in offset)
            (let ((buffer (make-array length :element-type '(unsigned-byte 8))))
              (read-sequence buffer in)
              buffer)))))))

(defun load-fasl-demand-paged (path &key (preload *fasl-preload-forms*))
  "Open PATH through the VM mmap abstraction and return a FASL TOC descriptor.
This is a configuration hook only; callers can use FASL-PAGE-FAULT for actual
lazy byte retrieval."
  (let* ((toc (fasl-toc-read path))
         (mapping (and *fasl-toc-enabled* (probe-file path)
                       (mmap-file path :protection :read :flags :private))))
    (setf (fasl-toc-mapping toc) mapping)
    (dolist (key preload)
      (fasl-page-fault toc key))
    toc))

(defun %pgo-write-object (object stream)
  "Write OBJECT using a tiny MessagePack-inspired tagged text encoding."
  (cond
    ((hash-table-p object)
     (write-char #\H stream)
     (write (hash-table-count object) :stream stream)
     (write-char #\Space stream)
     (maphash (lambda (k v)
                (%pgo-write-object k stream)
                (%pgo-write-object v stream))
              object))
    ((stringp object) (write-char #\S stream) (write object :stream stream))
    ((integerp object) (write-char #\I stream) (write object :stream stream))
    ((symbolp object) (write-char #\Y stream) (write object :stream stream))
    ((consp object)
     (write-char #\L stream)
     (write (length object) :stream stream)
     (write-char #\Space stream)
     (dolist (item object) (%pgo-write-object item stream)))
    ((null object) (write-char #\N stream))
    (t (write-char #\R stream) (write object :stream stream)))
  (write-char #\Newline stream))

(defun %pgo-read-object (stream)
  "Read an object written by %PGO-WRITE-OBJECT."
  (loop for ch = (peek-char nil stream nil nil)
        while (and ch (member ch '(#\Space #\Tab #\Return #\Newline)))
        do (read-char stream nil nil))
  (let ((tag (read-char stream nil nil)))
    (case tag
      (#\H (let ((count (read stream nil 0))
                 (table (make-hash-table :test #'equal)))
             (dotimes (_ count table)
               (declare (ignore _))
               (setf (gethash (%pgo-read-object stream) table)
                     (%pgo-read-object stream)))))
      (#\L (loop repeat (read stream nil 0) collect (%pgo-read-object stream)))
      (#\S (read stream nil ""))
      (#\I (read stream nil 0))
      (#\Y (read stream nil nil))
      (#\N nil)
      (#\R (read stream nil nil))
      ((nil) nil)
      (otherwise (read stream nil nil)))))

(defun save-pgo-data (data &optional (path *pgo-data-path*))
  "Persist DATA to PATH using the lightweight PGO tagged encoding."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-line "CLCC-PGO-MSGPACK-1" out)
    (%pgo-write-object data out))
  path)

(defun load-pgo-data (&optional (path *pgo-data-path*))
  "Load PGO data previously written by SAVE-PGO-DATA, or NIL when absent."
  (when (probe-file path)
    (with-open-file (in path :direction :input)
      (let ((magic (read-line in nil nil)))
        (unless (string= magic "CLCC-PGO-MSGPACK-1")
          (error "Unsupported PGO data file: ~A" path))
        (%pgo-read-object in)))))

(export '(*fasl-toc-enabled* *fasl-preload-forms* *pgo-data-path*
          fasl-toc-read fasl-page-fault load-fasl-demand-paged
          save-pgo-data load-pgo-data))
