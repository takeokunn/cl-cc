;;;; Core Image Save/Restore (FR-350, FR-352)
(in-package :cl-cc/runtime)

(defconstant +image-magic+ #x434C4343)
(defconstant +rt-image-version+ 1)
(defconstant +rt-image-format+ :cl-cc-runtime-image)
(defconstant +rt-image-crc32-polynomial+ #xedb88320)

(defconstant +rt-image-value-nil+ 0)
(defconstant +rt-image-value-fixnum+ 1)
(defconstant +rt-image-value-string+ 2)
(defconstant +rt-image-value-symbol-ref+ 3)
(defconstant +rt-image-value-function-code+ 4)
(defconstant +rt-image-value-cons+ 5)

(defstruct rt-image-header
  (magic +image-magic+ :type integer)
  (version +rt-image-version+ :type integer)
  (format +rt-image-format+)
  (created-at 0 :type integer))

(defstruct rt-image
  (header (make-rt-image-header))
  (globals nil)
  (heap nil)
  (code-version 0 :type integer))

(defvar *rt-image-globals* nil)
(defvar *rt-image-restore-hooks* nil)
(defvar *rt-code-version* 0)

(defun rt-image-register-global (sym)
  (check-type sym symbol)
  (pushnew sym *rt-image-globals* :test #'eq)
  sym)

(defun rt-image-register-restore-hook (fn)
  (check-type fn function)
  (pushnew fn *rt-image-restore-hooks*)
  fn)

(defun %rt-global-pair (sym)
  (list sym (when (boundp sym) (symbol-value sym))))

(defun rt-capture-image-state (&key globals heap)
  (let ((symbols (or globals *rt-image-globals*)))
    (make-rt-image
     :header (make-rt-image-header :created-at (get-universal-time))
     :globals (mapcar #'%rt-global-pair symbols)
     :heap heap
     :code-version *rt-code-version*)))

(defun rt-validate-image (image)
  (unless (typep image 'rt-image) (error "Not a runtime image: ~s" image))
  (let ((header (rt-image-header image)))
    (unless (= (rt-image-header-magic header) +image-magic+)
      (error "Bad image magic: ~x" (rt-image-header-magic header)))
    (unless (= (rt-image-header-version header) +rt-image-version+)
      (error "Bad image version: ~d" (rt-image-header-version header))))
  t)

(defun %rt-image-make-buffer ()
  (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun %rt-image-push-byte (buffer byte)
  (vector-push-extend (logand byte #xff) buffer))

(defun %rt-image-write-u16 (buffer value)
  (when (or (< value 0) (> value #xffff))
    (error "Value does not fit in u16: ~d" value))
  (%rt-image-push-byte buffer (ldb (byte 8 8) value))
  (%rt-image-push-byte buffer (ldb (byte 8 0) value)))

(defun %rt-image-write-u32 (buffer value)
  (when (or (< value 0) (> value #xffffffff))
    (error "Value does not fit in u32: ~d" value))
  (%rt-image-push-byte buffer (ldb (byte 8 24) value))
  (%rt-image-push-byte buffer (ldb (byte 8 16) value))
  (%rt-image-push-byte buffer (ldb (byte 8 8) value))
  (%rt-image-push-byte buffer (ldb (byte 8 0) value)))

(defun %rt-image-read-u16 (bytes offset)
  (values (logior (ash (aref bytes offset) 8)
                  (aref bytes (1+ offset)))
          (+ offset 2)))

(defun %rt-image-read-u32 (bytes offset)
  (values (logior (ash (aref bytes offset) 24)
                  (ash (aref bytes (+ offset 1)) 16)
                  (ash (aref bytes (+ offset 2)) 8)
                  (aref bytes (+ offset 3)))
          (+ offset 4)))

(defun %rt-image-string-bytes (string)
  (let ((bytes (%rt-image-make-buffer)))
    (loop for ch across string
          for code = (char-code ch)
          do (progn
               (when (> code #xff)
                 (error "Image strings currently require one-byte character codes: ~s" string))
               (%rt-image-push-byte bytes code)))
    bytes))

(defun %rt-image-bytes-string (bytes &key (start 0) end)
  (let* ((end (or end (length bytes)))
         (string (make-string (- end start))))
    (loop for i from start below end
          for j from 0
          do (setf (aref string j) (code-char (aref bytes i))))
    string))

(defun %rt-image-readable-string (value)
  (let ((*print-circle* t)
        (*print-readably* t)
        (*package* (find-package :cl-user)))
    (with-output-to-string (s)
      (prin1 value s))))

(defun %rt-image-read-readable (bytes)
  (let ((*read-eval* nil)
        (*package* (find-package :cl-user)))
    (with-input-from-string (s (%rt-image-bytes-string bytes))
      (read s))))

(defun %rt-image-classify-value (value &optional function-code-p)
  (cond
    (function-code-p +rt-image-value-function-code+)
    ((null value) +rt-image-value-nil+)
    ((typep value 'fixnum) +rt-image-value-fixnum+)
    ((stringp value) +rt-image-value-string+)
    ((symbolp value) +rt-image-value-symbol-ref+)
    ((functionp value) +rt-image-value-function-code+)
    (t +rt-image-value-cons+)))

(defun %rt-image-crc32 (bytes &key (start 0) end)
  (let ((crc #xffffffff)
        (end (or end (length bytes))))
    (loop for i from start below end
          do (setf crc (logxor crc (aref bytes i)))
             (dotimes (bit-index 8)
               (setf crc (if (oddp crc)
                             (logxor (ash crc -1) +rt-image-crc32-polynomial+)
                             (ash crc -1)))
               (setf crc (ldb (byte 32 0) crc))))
    (ldb (byte 32 0) (logxor crc #xffffffff))))

(defun %rt-image-symbol-token (kind sym)
  (%rt-image-readable-string (list kind sym)))

(defun %rt-image-function-code (sym)
  "Return readable function restoration metadata for SYM's fdefinition."
  ;; Host function objects are not generally readably printable.  The runtime
  ;; image therefore records the fdefinition slot as code metadata keyed by the
  ;; registered symbol; loading reattaches the currently available fdefinition.
  `(:named-function ,sym))

(defun %rt-image-global-entries (symbols)
  (loop for sym in symbols
        append (append
                (when (boundp sym)
                  (list (list (%rt-image-symbol-token :value sym)
                              (%rt-image-classify-value (symbol-value sym))
                              (symbol-value sym))))
                (when (fboundp sym)
                  (list (list (%rt-image-symbol-token :function sym)
                              +rt-image-value-function-code+
                              (%rt-image-function-code sym))))
                (list (list (%rt-image-symbol-token :plist sym)
                            (%rt-image-classify-value (symbol-plist sym))
                            (symbol-plist sym))))))

(defun %rt-image-write-entry (buffer entry)
  (destructuring-bind (name type value) entry
    (let* ((name-bytes (%rt-image-string-bytes name))
           (payload-bytes (if (= type +rt-image-value-nil+)
                              (%rt-image-make-buffer)
                              (%rt-image-string-bytes (%rt-image-readable-string value)))))
      (%rt-image-write-u16 buffer (length name-bytes))
      (loop for byte across name-bytes do (%rt-image-push-byte buffer byte))
      (%rt-image-push-byte buffer type)
      ;; The entry format's value-bytes field is length-prefixed so printed
      ;; strings, symbols, and conses can be read unambiguously from a binary
      ;; stream while retaining the required print/read representation.
      (%rt-image-write-u32 buffer (length payload-bytes))
      (loop for byte across payload-bytes do (%rt-image-push-byte buffer byte)))))

(defun %rt-image-read-entry (bytes offset)
  (multiple-value-bind (name-length offset) (%rt-image-read-u16 bytes offset)
    (let* ((name-end (+ offset name-length))
           (name (%rt-image-bytes-string bytes :start offset :end name-end))
           (type (aref bytes name-end)))
      (multiple-value-bind (value-length payload-start)
          (%rt-image-read-u32 bytes (1+ name-end))
        (let* ((payload-end (+ payload-start value-length))
               (payload (subseq bytes payload-start payload-end))
               (value (unless (= type +rt-image-value-nil+)
                        (%rt-image-read-readable payload))))
          (values (list name type value) payload-end))))))

(defun %rt-image-restore-entry (entry)
  (destructuring-bind (name type value) entry
    (declare (ignore type))
    (destructuring-bind (kind sym)
        (%rt-image-read-readable (%rt-image-string-bytes name))
      (ecase kind
        (:value
         (setf (symbol-value sym) value))
        (:function
         (when (fboundp sym)
           (setf (symbol-function sym) (symbol-function sym))))
        (:plist
         (setf (symbol-plist sym) value)))
      (rt-image-register-global sym))))

(defun rt-save-image (path &key globals heap)
  "Serialize registered runtime globals as a binary heap image.

The image contains a fixed magic number, format version, registered global
symbol entries, and a CRC32 checksum.  Values are encoded with PRINT/READ under
*PRINT-READABLY* so cons/object graphs reachable from registered globals are
preserved without requiring a concrete runtime heap object.  HEAP is accepted
for API compatibility but is not required."
  (declare (ignore heap))
  (let* ((symbols (remove-duplicates (or globals *rt-image-globals*) :test #'eq))
         (entries (%rt-image-global-entries symbols))
         (buffer (%rt-image-make-buffer)))
    (%rt-image-write-u32 buffer +image-magic+)
    (%rt-image-write-u16 buffer +rt-image-version+)
    (%rt-image-write-u32 buffer (length entries))
    (dolist (entry entries)
      (%rt-image-write-entry buffer entry))
    (%rt-image-write-u32 buffer (%rt-image-crc32 buffer))
    (with-open-file (s path :direction :output :if-exists :supersede
                           :if-does-not-exist :create
                           :element-type '(unsigned-byte 8))
      (write-sequence buffer s))
    path))

(defun rt-restore-image-state (image)
  (rt-validate-image image)
  (dolist (pair (rt-image-globals image))
    (destructuring-bind (sym value) pair
      (setf (symbol-value sym) value)
      (rt-image-register-global sym)))
  (setf *rt-code-version* (rt-image-code-version image))
  (dolist (hook *rt-image-restore-hooks*)
    (funcall hook image))
  image)

(defun rt-load-image (path)
  (with-open-file (s path :direction :input :element-type '(unsigned-byte 8))
    (let* ((length (file-length s))
           (bytes (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence bytes s)
      (when (< length 14)
        (error "Runtime image is too small: ~a" path))
      (multiple-value-bind (magic offset) (%rt-image-read-u32 bytes 0)
        (unless (= magic +image-magic+)
          (error "Bad image magic: ~x" magic))
        (multiple-value-bind (version offset) (%rt-image-read-u16 bytes offset)
          (unless (= version +rt-image-version+)
            (error "Bad image version: ~d" version))
          (multiple-value-bind (entry-count offset) (%rt-image-read-u32 bytes offset)
            (multiple-value-bind (stored-crc _) (%rt-image-read-u32 bytes (- length 4))
              (declare (ignore _))
              (let ((computed-crc (%rt-image-crc32 bytes :end (- length 4))))
                (unless (= stored-crc computed-crc)
                  (error "Bad image CRC: expected ~8,'0x, got ~8,'0x"
                         stored-crc computed-crc))))
            (dotimes (entry-index entry-count)
              (multiple-value-bind (entry next-offset)
                  (%rt-image-read-entry bytes offset)
                (when (> next-offset (- length 4))
                  (error "Runtime image entry overruns CRC"))
                (%rt-image-restore-entry entry)
                (setf offset next-offset)))
            (unless (= offset (- length 4))
              (error "Runtime image contains trailing data before CRC"))
            (dolist (hook *rt-image-restore-hooks*)
              (funcall hook path))
            t))))))

(defun rt-hot-reload (thunk &key preserve-globals)
  "Run THUNK as a hot-code reload transaction while preserving selected globals."
  (let ((snapshot (mapcar #'%rt-global-pair preserve-globals)))
    (incf *rt-code-version*)
    (unwind-protect (funcall thunk)
      (dolist (pair snapshot)
        (setf (symbol-value (first pair)) (second pair))))
    *rt-code-version*))

(defun rt-image-init ()
  (setf *rt-image-globals* nil
        *rt-image-restore-hooks* nil
        *rt-code-version* 0)
  t)
