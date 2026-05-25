(defpackage :cl-cc/ffi
  (:use :cl)
  (:export #:dl-lib #:dl-lib-p #:dl-lib-name #:dl-lib-handle #:dl-lib-loaded
           #:load-shared-library #:load-framework #:unload-shared-library
           #:list-loaded-libraries #:find-foreign-symbol))

(in-package :cl-cc/ffi)
(defstruct dl-lib (name "" :type string) (handle nil) (loaded nil))
(defvar *loaded-libs* (make-hash-table :test #'equal))
(defun %load-shared-object (path)
  #+sbcl
  (sb-alien:load-shared-object path)
  #-sbcl
  (error "Dynamic library loading is not implemented for this Lisp host: ~A"
         (lisp-implementation-type)))

(defun %foreign-symbol-address (name)
  #+sbcl
  (sb-sys:find-dynamic-foreign-symbol-address name)
  #-sbcl
  (declare (ignore name))
  #-sbcl
  (error "Foreign symbol lookup is not implemented for this Lisp host: ~A"
         (lisp-implementation-type)))

(defun %make-foreign-function (name address)
  #+sbcl
  (declare (ignore name))
  #+sbcl
  (let ((symbol-address address))
    (lambda (&rest args)
      (let ((alien (sb-alien:sap-alien (sb-sys:int-sap symbol-address)
                                       (function sb-alien:int))))
        (apply #'sb-alien:alien-funcall alien args))))
  #-sbcl
  (declare (ignore name address))
  #-sbcl
  (error "Foreign function calls are not implemented for this Lisp host: ~A"
         (lisp-implementation-type)))

(defun load-shared-library (path &key (if-not-found :error))
  (let ((ex (gethash path *loaded-libs*))) (when ex (return-from load-shared-library ex)))
  (let ((resolved (or (probe-file path) (probe-file (format nil "~A.dylib" path)) (probe-file (format nil "~A.so" path)) (probe-file (format nil "/usr/lib/~A" path)) (probe-file (format nil "/usr/local/lib/~A" path))
                      #+sbcl path)))
    (if resolved
        (let ((resolved-path (etypecase resolved
                               (pathname (namestring resolved))
                               (string resolved))))
          (handler-case
              (let ((lib (make-dl-lib :name path :handle (%load-shared-object resolved-path) :loaded t))) (setf (gethash path *loaded-libs*) lib) lib)
            (error (condition)
              (if (eq if-not-found :error)
                  (error "Library not found: ~S (~A)" path condition)
                  nil))))
        (if (eq if-not-found :error)
            (error "Library not found: ~S" path)
            nil))))
(defun load-framework (name) (load-shared-library (format nil "/System/Library/Frameworks/~A.framework/~A" name name)))
(defun unload-shared-library (lib) (setf (dl-lib-loaded lib) nil) (remhash (dl-lib-name lib) *loaded-libs*) (values))
(defun list-loaded-libraries () (loop for lib being the hash-values of *loaded-libs* when (dl-lib-loaded lib) collect (dl-lib-name lib)))
(defun find-foreign-symbol (name lib)
  "Find foreign symbol NAME in dynamic library LIB.
Note: Currently uses the host's global symbol table (sb-sys:find-dynamic-foreign-symbol-address),
not library-scoped resolution.  The LIB argument is validated for loaded state only.
Library-scoped dlsym via sb-alien:extern-alien or native C FFI is deferred to FR-719."
  (unless (and (typep lib 'dl-lib) (dl-lib-loaded lib))
    (error "Cannot look up foreign symbol ~S in an unloaded library: ~S" name lib))
  (let ((address (%foreign-symbol-address name)))
    (and address (%make-foreign-function name address))))
