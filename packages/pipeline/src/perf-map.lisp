(in-package :cl-cc/pipeline)

;;; FR-553 — Linux perf JIT map support.
;;;
;;; perf map files are plain text files named /tmp/perf-<pid>.map with one
;;; mapping per line:
;;;
;;;   HEX_START HEX_SIZE SYMBOL_NAME
;;;
;;; Native backends currently return a single code byte vector rather than a
;;; per-symbol layout map.  We therefore emit a conservative synthetic layout
;;; that covers the generated code range and partitions it by the function
;;; blocks visible in the VM stream.  This keeps the file format consumable by
;;; perf and gives stable symbolic names until backend-level symbol offsets are
;;; exposed.

(defvar *perf-map-stream* nil
  "When non-NIL, stream used for perf map writes during native code generation.")

(defparameter *perf-map-output-dir* "/tmp"
  "Directory used for Linux perf JIT map files.")

(defparameter *perf-map-base-address* #x10000000
  "Synthetic base address used when the native backend does not expose load addresses.")

(defun %perf-map-pid ()
  "Return the current process id, falling back to 0 when unavailable."
  (or (ignore-errors
        (require :sb-posix)
        (let* ((pkg (find-package "SB-POSIX"))
               (sym (and pkg (find-symbol "GETPID" pkg))))
          (and sym (funcall sym))))
      0))

(defun perf-map-path (&optional (pid (%perf-map-pid)))
  "Return the perf map pathname for PID."
  (merge-pathnames (format nil "perf-~D.map" pid)
                   (uiop:ensure-directory-pathname *perf-map-output-dir*)))

(defun %perf-map-symbol-name (name)
  "Return NAME formatted for perf map consumption."
  (let ((s (princ-to-string (or name :anonymous))))
    (substitute #\_ #\Space s)))

(defun %write-perf-map-entry-to-stream (stream start size name)
  "Write one perf map entry to STREAM."
  (format stream "~X ~X ~A~%" start (max 1 size) (%perf-map-symbol-name name))
  (finish-output stream))

(defun write-perf-map-entry (&rest args)
  "Write one perf map entry.

Supported call shapes are (STREAM CODE-START CODE-SIZE FUNCTION-NAME) for tests
and explicit streams, or (CODE-START CODE-SIZE FUNCTION-NAME) to append to the
current process perf map under *PERF-MAP-OUTPUT-DIR*."
  (ecase (length args)
    (3 (destructuring-bind (start size name) args
         (let ((path (perf-map-path)))
           (ensure-directories-exist path)
           (with-open-file (out path :direction :output :if-exists :append
                                     :if-does-not-exist :create)
             (%write-perf-map-entry-to-stream out start size name))
           path)))
    (4 (destructuring-bind (stream start size name) args
         (%write-perf-map-entry-to-stream stream start size name)))))

(defun %perf-map-function-blocks (program)
  "Return function blocks visible in PROGRAM, or NIL when no blocks are present."
  (when (typep program 'cl-cc/vm:vm-program)
    (multiple-value-bind (_skeleton blocks)
        (%pipeline-extract-function-blocks (cl-cc/vm:vm-program-instructions program))
      (declare (ignore _skeleton))
      blocks)))

(defun %perf-map-block-weight (block)
  "Return a positive byte-weight estimate for BLOCK."
  (max 1 (length (pipeline-function-block-instructions block))))

(defun %perf-map-write-blocks (stream blocks code-size &key (base *perf-map-base-address*))
  "Write BLOCKS into STREAM using CODE-SIZE bytes starting at BASE."
  (let* ((total-weight (max 1 (reduce #'+ blocks :key #'%perf-map-block-weight :initial-value 0)))
         (remaining-size (max 1 code-size))
         (addr base))
    (loop for block in blocks
          for rest = blocks then (rest rest)
          for last-p = (null (rest rest))
          for weight = (%perf-map-block-weight block)
          for size = (if last-p
                         remaining-size
                         (max 1 (floor (* (max 1 code-size) weight) total-weight)))
          do (write-perf-map-entry stream addr size (pipeline-function-block-name block))
             (incf addr size)
             (decf remaining-size size))))

(defun write-perf-map-for-native-code (program code-bytes &key output-file)
  "Append perf map entries for PROGRAM/CODE-BYTES and return the map path.
OUTPUT-FILE is used as a fallback symbol name when function blocks are absent."
  (let ((path (perf-map-path))
        (code-size (length code-bytes)))
    (ensure-directories-exist path)
    (if *perf-map-stream*
        (let ((blocks (%perf-map-function-blocks program)))
          (if blocks
              (%perf-map-write-blocks *perf-map-stream* blocks code-size)
              (write-perf-map-entry *perf-map-stream* *perf-map-base-address*
                                    code-size (or output-file "_main"))))
        (with-open-file (out path :direction :output :if-exists :append
                                  :if-does-not-exist :create)
          (let ((*perf-map-stream* out))
            (write-perf-map-for-native-code program code-bytes :output-file output-file))))
    path))

(defun perf-map-line-valid-p (line)
  "Return true when LINE has perf map HEX_ADDR HEX_SIZE SYMBOL_NAME shape."
  (let ((parts (uiop:split-string line :separator '(#\Space #\Tab))))
    (and (>= (length parts) 3)
         (ignore-errors (parse-integer (first parts) :radix 16))
         (ignore-errors (parse-integer (second parts) :radix 16))
         (> (length (third parts)) 0))))
