;;;; types-extended-ffi.lisp — FFI type descriptors
(in-package :cl-cc/type)

(define-condition ffi-validation-error (error)
  ((detail :initarg :detail :reader ffi-validation-error-detail))
  (:report (lambda (condition stream)
             (format stream "Invalid FFI descriptor: ~A" (ffi-validation-error-detail condition)))))

(defparameter +ffi-scalar-kinds+
  '(:void :char :short :int :long :float :double :string :bool :size-t)
  "Supported scalar FFI kinds.")

(defstruct (ffi-scalar-type (:constructor %make-ffi-scalar-type))
  "A scalar C-side type."
  (kind :int))

(defstruct (ffi-pointer-type (:constructor %make-ffi-pointer-type))
  "A typed pointer into foreign memory."
  (pointee nil)
  (borrowed-p t :type boolean)
  (nullable-p nil :type boolean))

(defstruct (ffi-callback-type (:constructor %make-ffi-callback-type))
  "A type-checked callback signature."
  (argument-types nil :type list)
  (return-type nil))

(defstruct (ffi-function-descriptor (:constructor %make-ffi-function-descriptor))
  "A named foreign function boundary descriptor."
  (name nil)
  (argument-types nil :type list)
  (return-type nil)
  (abi :c))

(defun make-ffi-scalar-type (kind)
  "Construct a scalar FFI type."
  (let ((normalized (if (keywordp kind) kind (intern (string-upcase (symbol-name kind)) :keyword))))
    (unless (member normalized +ffi-scalar-kinds+ :test #'eq)
      (error 'ffi-validation-error :detail (format nil "unknown scalar kind ~S" kind)))
    (%make-ffi-scalar-type :kind normalized)))

(defun make-ffi-pointer-type (pointee &key (borrowed-p t) nullable-p)
  "Construct a typed foreign pointer."
  (%make-ffi-pointer-type :pointee pointee :borrowed-p borrowed-p :nullable-p nullable-p))

(defun make-ffi-callback-type (argument-types return-type)
  "Construct a typed callback signature."
  (%make-ffi-callback-type :argument-types argument-types :return-type return-type))

(defun make-ffi-function-descriptor (name argument-types return-type &key (abi :c))
  "Construct a typed function descriptor."
  (%make-ffi-function-descriptor :name name
                                 :argument-types argument-types
                                 :return-type return-type
                                 :abi abi))

(defun ffi-type-valid-p (descriptor)
  "Return T when DESCRIPTOR is a recursively valid FFI type descriptor."
  (cond
    ((ffi-scalar-type-p descriptor)
     (member (ffi-scalar-type-kind descriptor) +ffi-scalar-kinds+ :test #'eq))
    ((ffi-pointer-type-p descriptor)
     (ffi-type-valid-p (ffi-pointer-type-pointee descriptor)))
    ((ffi-callback-type-p descriptor)
     (and (every #'ffi-type-valid-p (ffi-callback-type-argument-types descriptor))
          (ffi-type-valid-p (ffi-callback-type-return-type descriptor))))
    ((ffi-function-descriptor-p descriptor)
     (and (or (symbolp (ffi-function-descriptor-name descriptor))
              (stringp (ffi-function-descriptor-name descriptor)))
          (every #'ffi-type-valid-p (ffi-function-descriptor-argument-types descriptor))
          (ffi-type-valid-p (ffi-function-descriptor-return-type descriptor))))
    (t nil)))

(defun ffi-lisp-type-compatible-p (lisp-type descriptor)
  "Return T when LISP-TYPE is compatible with DESCRIPTOR."
  (cond
    ((ffi-scalar-type-p descriptor)
     (case (ffi-scalar-type-kind descriptor)
       (:int (member lisp-type '(integer fixnum) :test #'eq))
       (:float (member lisp-type '(single-float float) :test #'eq))
       (:double (member lisp-type '(double-float float) :test #'eq))
       (:string (eq lisp-type 'string))
       (:bool (member lisp-type '(boolean bool) :test #'eq))
       (:void t)
       (t t)))
    ((ffi-pointer-type-p descriptor)
     (member lisp-type '(system-area-pointer foreign-pointer pointer) :test #'eq))
    ((ffi-callback-type-p descriptor)
     (eq lisp-type 'function))
    (t nil)))

(defun ffi-descriptor-form-valid-p (value)
  "Return T when VALUE is a well-formed raw FR-2103 descriptor form."
  (cond
    ((typep value 'type-node) t)
    ((atom value) t)
    ((not (consp value)) nil)
    ((member (string-upcase (symbol-name (first value))) '("C-INT" "C-STRING" "C-PTR") :test #'string=)
     (= (length value) 2))
    ((string= (string-upcase (symbol-name (first value))) "C-CALLBACK")
     (and (= (length value) 2)
          (second value)))
    (t (every #'ffi-descriptor-form-valid-p (rest value)))))

(defun %ffi-symbol-name (value)
  "Return VALUE's uppercase symbolic name, or NIL."
  (cond
    ((symbolp value) (string-upcase (symbol-name value)))
    ((stringp value) (string-upcase value))
    (t nil)))

(defun %ffi-scalar-kind-from-name (name)
  "Map a raw C descriptor NAME to an FFI scalar keyword."
  (let ((normalized (and name
                         (if (and (> (length name) 2)
                                  (string= (subseq name 0 2) "C-"))
                             (subseq name 2)
                             name))))
    (cdr (assoc normalized
                '(("VOID" . :void) ("CHAR" . :char) ("SHORT" . :short)
                  ("INT" . :int) ("INTEGER" . :int) ("LONG" . :long)
                  ("FLOAT" . :float) ("DOUBLE" . :double)
                  ("STRING" . :string) ("BOOL" . :bool) ("BOOLEAN" . :bool)
                  ("SIZE-T" . :size-t))
                :test #'string=))))

(defun ffi-descriptor-from-form (value)
  "Normalize raw FFI descriptor VALUE into structured FFI descriptor objects."
  (cond
    ((or (ffi-scalar-type-p value)
         (ffi-pointer-type-p value)
         (ffi-callback-type-p value)
         (ffi-function-descriptor-p value))
     value)
    ((or (symbolp value) (stringp value))
     (let ((kind (%ffi-scalar-kind-from-name (%ffi-symbol-name value))))
       (unless kind
         (error 'ffi-validation-error :detail (format nil "unknown FFI scalar descriptor ~S" value)))
       (make-ffi-scalar-type kind)))
    ((consp value)
     (let* ((head (first value))
            (head-name (%ffi-symbol-name head)))
       (cond
         ((and head-name
               (member head-name '("C-PTR" "PTR" "POINTER" "FOREIGN-POINTER") :test #'string=))
          (unless (= (length value) 2)
            (error 'ffi-validation-error :detail (format nil "pointer descriptor needs pointee: ~S" value)))
          (make-ffi-pointer-type (ffi-descriptor-from-form (second value))))
         ((and head-name (string= head-name "C-CALLBACK"))
          (unless (>= (length value) 3)
            (error 'ffi-validation-error :detail (format nil "callback descriptor needs args and return: ~S" value)))
          (make-ffi-callback-type (mapcar #'ffi-descriptor-from-form (second value))
                                  (ffi-descriptor-from-form (third value))))
         ((and head-name
               (member head-name '("FOREIGN" "FOREIGN-FUNCTION" "FFI-FUNCTION") :test #'string=))
          (unless (= (length value) 4)
            (error 'ffi-validation-error :detail (format nil "foreign function descriptor needs name, args, return: ~S" value)))
          (make-ffi-function-descriptor (second value)
                                        (mapcar #'ffi-descriptor-from-form (third value))
                                        (ffi-descriptor-from-form (fourth value))))
         ((%ffi-scalar-kind-from-name head-name)
          (ffi-descriptor-from-form head))
         (t
          (error 'ffi-validation-error :detail (format nil "unknown FFI descriptor form ~S" value))))))
    (t
     (error 'ffi-validation-error :detail (format nil "unsupported FFI descriptor ~S" value)))))

(defun ffi-descriptor-lisp-type (descriptor)
  "Return the CL-CC type-node produced by DESCRIPTOR."
  (let ((normalized (ffi-descriptor-from-form descriptor)))
    (cond
      ((ffi-scalar-type-p normalized)
       (case (ffi-scalar-type-kind normalized)
         ((:int :short :long :size-t) type-int)
         ((:float :double) type-float)
         (:string type-string)
         (:bool type-bool)
         (:void type-null)
         (:char type-char)
         (t type-any)))
      ((ffi-pointer-type-p normalized)
       (make-type-primitive :name 'foreign-pointer))
      ((ffi-callback-type-p normalized)
       (make-type-arrow (mapcar #'ffi-descriptor-lisp-type
                                (ffi-callback-type-argument-types normalized))
                        (ffi-descriptor-lisp-type
                         (ffi-callback-type-return-type normalized))))
      ((ffi-function-descriptor-p normalized)
       (make-type-arrow (mapcar #'ffi-descriptor-lisp-type
                                (ffi-function-descriptor-argument-types normalized))
                        (ffi-descriptor-lisp-type
                         (ffi-function-descriptor-return-type normalized))))
      (t type-any))))

