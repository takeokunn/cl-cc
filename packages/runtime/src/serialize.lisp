(in-package :cl-cc/runtime)

(defmacro define-serialization-tags (&body specs)
  `(progn
     ,@(loop for (name value) in specs
             collect `(defconstant ,name ,value))))

(define-serialization-tags
  (+ser-tag-nil+ 0)
  (+ser-tag-integer+ 1)
  (+ser-tag-float+ 2)
  (+ser-tag-string+ 3)
  (+ser-tag-symbol+ 4)
  (+ser-tag-cons+ 5)
  (+ser-tag-vector+ 6)
  (+ser-tag-hash-table+ 7)
  (+ser-tag-reference+ 8)
  (+ser-tag-t+ 9)
  (+ser-tag-character+ 10)
  (+ser-tag-standard-object+ 11))

(defstruct (ser-ctx (:constructor make-ser-ctx)
                    (:conc-name ser-ctx-))
  (written (make-hash-table :test #'eq) :type hash-table)
  (read (make-hash-table :test #'eql) :type hash-table)
  (next-id 0 :type integer))

(defstruct (ser-writer (:constructor make-ser-writer)
                       (:conc-name ser-writer-))
  (buffer (make-array 128
                      :element-type '(unsigned-byte 8)
                      :adjustable t
                      :fill-pointer 0)))

(defstruct (ser-reader (:constructor make-ser-reader (bytes))
                       (:conc-name ser-reader-))
  (bytes #() :type vector)
  (index 0 :type fixnum))

(defun %serialization-error (control &rest args)
  (error "Serialization error: ~?" control args))

(defun %write-byte* (byte sink)
  (let ((octet (logand byte #xff)))
    (etypecase sink
      (ser-writer (vector-push-extend octet (ser-writer-buffer sink)))
      (stream (write-byte octet sink)))))

(defun %read-byte* (source)
  (etypecase source
    (ser-reader
     (let ((index (ser-reader-index source))
           (bytes (ser-reader-bytes source)))
       (when (>= index (length bytes))
         (%serialization-error "unexpected end of byte vector"))
       (prog1 (aref bytes index)
         (setf (ser-reader-index source) (1+ index)))))
    (stream
     (let ((byte (read-byte source nil :eof)))
       (when (eq byte :eof)
         (%serialization-error "unexpected end of stream"))
       byte))))

(defun %write-tag (tag sink)
  (%write-byte* tag sink))

(defun %write-unsigned-integer (value sink)
  (loop with rest = value
        do (let ((byte (logand rest #x7f)))
             (setf rest (ash rest -7))
             (when (plusp rest)
               (setf byte (logior byte #x80)))
             (%write-byte* byte sink)
             (when (zerop rest)
               (return)))))

(defun %read-unsigned-integer (source)
  (loop with result = 0
        with shift = 0
        for byte = (%read-byte* source)
        do (setf result (logior result (ash (logand byte #x7f) shift)))
           (if (zerop (logand byte #x80))
               (return result)
               (incf shift 7))))

(defun %write-signed-integer-payload (value sink)
  (%write-unsigned-integer (abs value) sink)
  (%write-byte* (if (minusp value) 1 0) sink))

(defun %read-signed-integer-payload (source)
  (let ((magnitude (%read-unsigned-integer source)))
    (if (= (%read-byte* source) 1)
        (- magnitude)
        magnitude)))

(defun %write-integer (value sink)
  (%write-tag +ser-tag-integer+ sink)
  (%write-signed-integer-payload value sink))

(defun %write-raw-string (string sink)
  (%write-unsigned-integer (length string) sink)
  (loop for character across string
        do (%write-unsigned-integer (char-code character) sink)))

(defun %read-raw-string (source)
  (let* ((length (%read-unsigned-integer source))
         (string (make-string length)))
    (dotimes (index length string)
      (setf (aref string index)
            (code-char (%read-unsigned-integer source))))))

(defun %write-raw-symbol (symbol sink)
  (let ((package (symbol-package symbol)))
    (%write-byte* (if package 1 0) sink)
    (when package
      (%write-raw-string (package-name package) sink))
    (%write-raw-string (symbol-name symbol) sink)))

(defun %read-raw-symbol (source)
  (let* ((package-present-p (= (%read-byte* source) 1))
         (package-name (when package-present-p
                         (%read-raw-string source)))
         (symbol-name (%read-raw-string source)))
    (if package-present-p
        (intern symbol-name package-name)
        (make-symbol symbol-name))))

(defun %write-float (value sink)
  (%write-tag +ser-tag-float+ sink)
  (%write-raw-string (prin1-to-string value) sink))

(defun %read-float (source)
  (let ((*read-eval* nil))
    (read-from-string (%read-raw-string source))))

(defun %write-string (value sink)
  (%write-tag +ser-tag-string+ sink)
  (%write-raw-string value sink))

(defun %write-symbol (value sink)
  (%write-tag +ser-tag-symbol+ sink)
  (%write-raw-symbol value sink))

(defun %write-character (value sink)
  (%write-tag +ser-tag-character+ sink)
  (%write-unsigned-integer (char-code value) sink))

(defun %standard-object-p (object)
  (typep object 'standard-object))

(defun %referenceable-object-p (object)
  (or (stringp object)
      (consp object)
      (vectorp object)
      (hash-table-p object)
      (%standard-object-p object)))

(defun %remember-written-object (object ctx sink)
  (when (%referenceable-object-p object)
    (multiple-value-bind (id present-p) (gethash object (ser-ctx-written ctx))
      (when present-p
        (%write-tag +ser-tag-reference+ sink)
        (%write-integer id sink)
        (return-from %remember-written-object nil))
      (setf (gethash object (ser-ctx-written ctx)) (ser-ctx-next-id ctx))
      (incf (ser-ctx-next-id ctx))))
  t)

(defun %remember-read-object (object ctx)
  (setf (gethash (ser-ctx-next-id ctx) (ser-ctx-read ctx)) object)
  (incf (ser-ctx-next-id ctx))
  object)

(defun %write-cons (object ctx sink)
  (%write-tag +ser-tag-cons+ sink)
  (%write-object (car object) ctx sink)
  (%write-object (cdr object) ctx sink))

(defun %write-vector (object ctx sink)
  (%write-tag +ser-tag-vector+ sink)
  (%write-unsigned-integer (length object) sink)
  (dotimes (index (length object))
    (%write-object (aref object index) ctx sink)))

(defun %write-hash-table (object ctx sink)
  (%write-tag +ser-tag-hash-table+ sink)
  (%write-unsigned-integer (hash-table-count object) sink)
  (loop for key being the hash-keys of object
          using (hash-value value)
        do (%write-object key ctx sink)
           (%write-object value ctx sink)))

(defun %standard-object-slot-names (object)
  (let ((class (class-of object)))
    (sb-mop:finalize-inheritance class)
    (mapcar #'sb-mop:slot-definition-name
            (sb-mop:class-slots class))))

(defun %write-standard-object (object ctx sink)
  (%write-tag +ser-tag-standard-object+ sink)
  (%write-raw-symbol (class-name (class-of object)) sink)
  (let ((bound-slots (remove-if-not (lambda (slot-name)
                                      (slot-boundp object slot-name))
                                    (%standard-object-slot-names object))))
    (%write-unsigned-integer (length bound-slots) sink)
    (dolist (slot-name bound-slots)
      (%write-raw-symbol slot-name sink)
      (%write-object (slot-value object slot-name) ctx sink))))

(defun %write-object (object ctx sink)
  (when (%remember-written-object object ctx sink)
    (cond
      ((null object) (%write-tag +ser-tag-nil+ sink))
      ((eq object t) (%write-tag +ser-tag-t+ sink))
      ((integerp object) (%write-integer object sink))
      ((floatp object) (%write-float object sink))
      ((characterp object) (%write-character object sink))
      ((stringp object) (%write-string object sink))
      ((symbolp object) (%write-symbol object sink))
      ((consp object) (%write-cons object ctx sink))
      ((vectorp object) (%write-vector object ctx sink))
      ((hash-table-p object) (%write-hash-table object ctx sink))
      ((%standard-object-p object) (%write-standard-object object ctx sink))
      (t (%serialization-error "unsupported object ~S of type ~S"
                               object
                               (type-of object))))))

(defun serialize (object &optional stream)
  "Serialize OBJECT to STREAM, or return a byte vector when STREAM is NIL."
  (let ((ctx (make-ser-ctx))
        (sink (or stream (make-ser-writer))))
    (%write-object object ctx sink)
    (if stream
        object
        (copy-seq (ser-writer-buffer sink)))))

(defun %read-string (ctx source)
  (%remember-read-object (%read-raw-string source) ctx))

(defun %read-symbol (source)
  (%read-raw-symbol source))

(defun %read-character (source)
  (code-char (%read-unsigned-integer source)))

(defun %read-cons (ctx source)
  (let ((cell (cons nil nil)))
    (%remember-read-object cell ctx)
    (setf (car cell) (%read-object ctx source)
          (cdr cell) (%read-object ctx source))
    cell))

(defun %read-vector (ctx source)
  (let* ((length (%read-unsigned-integer source))
         (vector (make-array length)))
    (%remember-read-object vector ctx)
    (dotimes (index length vector)
      (setf (aref vector index) (%read-object ctx source)))))

(defun %read-hash-table (ctx source)
  (let* ((count (%read-unsigned-integer source))
         (table (make-hash-table :test #'equal :size count)))
    (%remember-read-object table ctx)
    (dotimes (index count table)
      (setf (gethash (%read-object ctx source) table)
            (%read-object ctx source)))))

(defun %read-standard-object (ctx source)
  (let* ((class-name (%read-raw-symbol source))
         (class (find-class class-name))
         (object (allocate-instance class))
         (slot-count (%read-unsigned-integer source)))
    (%remember-read-object object ctx)
    (dotimes (index slot-count object)
      (let ((slot-name (%read-raw-symbol source)))
        (setf (slot-value object slot-name)
              (%read-object ctx source))))))

(defun %read-reference (ctx source)
  (let* ((id (%read-object ctx source))
         (object (gethash id (ser-ctx-read ctx) :missing)))
    (when (eq object :missing)
      (%serialization-error "unknown reference id ~D" id))
    object))

(defun %read-object (ctx source)
  (let ((tag (%read-byte* source)))
    (case tag
      (#.+ser-tag-nil+ nil)
      (#.+ser-tag-t+ t)
      (#.+ser-tag-integer+ (%read-signed-integer-payload source))
      (#.+ser-tag-float+ (%read-float source))
      (#.+ser-tag-character+ (%read-character source))
      (#.+ser-tag-string+ (%read-string ctx source))
      (#.+ser-tag-symbol+ (%read-symbol source))
      (#.+ser-tag-cons+ (%read-cons ctx source))
      (#.+ser-tag-vector+ (%read-vector ctx source))
      (#.+ser-tag-hash-table+ (%read-hash-table ctx source))
      (#.+ser-tag-standard-object+ (%read-standard-object ctx source))
      (#.+ser-tag-reference+ (%read-reference ctx source))
      (otherwise (%serialization-error "unknown tag ~D" tag)))))

(defun %deserialize-source (source)
  (etypecase source
    (stream source)
    (vector (make-ser-reader source))))

(defun deserialize (source)
  "Deserialize an object from SOURCE, a byte vector or binary stream."
  (%read-object (make-ser-ctx) (%deserialize-source source)))
