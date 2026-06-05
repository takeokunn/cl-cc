;;; vm-serialize.lisp — FR-1042: Object serialization
;;;
;;; make-load-form, FASL serialization, and print-unreadable-object.

(in-package :cl-cc/vm)

(defun vm-make-load-form (object &optional environment)
  (handler-case
      (make-load-form object environment)
    (error () `',object)))

(defun vm-make-load-form-saving-slots (object &key slot-names environment)
  (declare (ignore environment))
  (let ((slots (or slot-names (%vm-serialize-slot-names object))))
    `(make-instance ',(class-name (class-of object))
                    ,@(loop for name in slots
                            when (slot-boundp object name)
                              append (list (intern (symbol-name name) :keyword)
                                           `',(slot-value object name))))))

(defun %vm-serialize-slot-names (object)
  (handler-case
      (loop for slot in (class-slots (class-of object))
            for name = (slot-definition-name slot)
            when (slot-boundp object name) collect name)
    (error () nil)))

(defvar *fasl-code-version* 1)
(defvar +vm-fasl-magic+ "CLCC-FASL1")

(defun vm-write-to-fasl (value stream)
  (write-string +vm-fasl-magic+ stream)
  (terpri stream)
  (let ((*print-circle* t))
    (write value :stream stream :circle t :readably nil))
  value)

(defun vm-read-from-fasl (stream)
  (let ((magic (read-line stream nil nil)))
    (unless (string= magic +vm-fasl-magic+)
      (error "Invalid CL-CC FASL magic: ~S" magic))
    (read stream nil nil)))

(defun vm-compile-file-to-fasl (source-path &key output-file)
  (compile-file source-path :output-file (or output-file
        (make-pathname :type "fasl" :defaults source-path))))

(defun vm-load-fasl (fasl-path) (load fasl-path))

(defmacro vm-print-unreadable-object ((object stream &key type identity) &body body)
  `(progn
     (write-string "#<" ,stream)
     (when ,type
       (write-string (symbol-name (class-name (class-of ,object))) ,stream)
       (write-char #\Space ,stream))
     ,@body
     (when ,identity
         (format ,stream " {~X}" (sxhash ,object)))
     (write-char #\> ,stream)))

(export '(vm-make-load-form vm-make-load-form-saving-slots
          vm-write-to-fasl vm-read-from-fasl vm-compile-file-to-fasl
          vm-load-fasl vm-print-unreadable-object *fasl-code-version*))
