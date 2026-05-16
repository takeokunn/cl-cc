(in-package :cl-cc/expand)
;;;; DEFSTRUCT CLOS-backed expansion helpers.

;;; ── Standard CLOS-based defstruct expansion ───────────────────────────────

(defun %defstruct-register-accessors (name conc-name own-slots)
  "Register defstruct accessors for SETF expansion metadata.

Writable accessors are stored in `*accessor-slot-map*`.
Read-only accessors are recorded in `*defstruct-read-only-accessor-map*`."
  (dolist (slot own-slots)
    (let* ((slot-name  (first slot))
           (read-only-p (third slot))
           (accessor (%defstruct-accessor-name conc-name slot-name)))
      (unless read-only-p
        (setf (gethash accessor *accessor-slot-map*)
              (cons name slot-name)))
      (when read-only-p
        (setf (gethash accessor *defstruct-read-only-accessor-map*) t)))))

(defun %defstruct-defclass-slots (conc-name own-slots)
  "Build DEFCLASS slot specs for own defstruct slots."
  (loop for slot in own-slots
        collect (%expander-form (first slot)
                                :initarg  (%defstruct-make-keyword (first slot))
                                :initform (second slot)
                                (if (third slot) :reader :accessor)
                                (%defstruct-accessor-name conc-name (first slot)))))

(defun %defstruct-print-form (name print-fn print-fn-opt)
  "Emit an optional PRINT-OBJECT method for NAME."
  (when print-fn
    (let* ((obj    (gensym "OBJ"))
           (stream (gensym "STR"))
           (args   (%expander-form (%expander-form obj name) stream))
           (body   (if print-fn-opt
                       (%expander-form 'funcall (%expander-form 'function print-fn) obj stream 0)
                       (%expander-form 'funcall (%expander-form 'function print-fn) obj stream))))
      (%expander-form 'defmethod 'print-object args body))))

(defun %defstruct-derive-form (name derived-classes)
  "Emit registration forms for :deriving classes."
  (when derived-classes
    (let ((forms (loop for class in derived-classes
                       collect (%expander-form 'cl-cc/type:register-typeclass-instance
                                               (%expander-form 'quote class)
                                               (%expander-form 'quote name)
                                               nil))))
      (cons 'eval-when (cons '(:load-toplevel :execute) forms)))))

(defun %defstruct-clos-expansion (model)
  "Emit the standard CLOS-based defstruct expansion for MODEL."
  (let ((name            (getf model :name))
        (conc-name       (getf model :conc-name))
        (ctor-name       (getf model :ctor-name))
        (boa-args        (getf model :boa-args))
        (parent-name     (getf model :parent-name))
        (own-slots       (getf model :own-slots))
        (all-slots       (getf model :all-slots))
        (pred-name       (getf model :pred-name))
        (copier-name     (getf model :copier-name))
        (print-fn        (getf model :print-fn))
        (print-fn-opt    (getf model :print-fn-opt))
        (derived-classes (getf model :derived-classes)))
    (%defstruct-register-accessors name conc-name own-slots)
    (let* ((defclass-parents (if parent-name (%expander-form parent-name) '()))
           (defclass-form    (%expander-form 'defclass name defclass-parents
                                             (%defstruct-defclass-slots conc-name own-slots)))
           (ctor-form        (when ctor-name
                               (%defstruct-make-constructor ctor-name name boa-args all-slots)))
           (pred-form        (when pred-name
                               (%expander-form 'defun pred-name
                                               (%expander-form 'obj)
                                               (%expander-form 'typep 'obj
                                                               (%expander-form 'quote name)))))
           (copier-form      (%defstruct-copier-form copier-name name all-slots nil))
           (print-form       (%defstruct-print-form name print-fn print-fn-opt))
           (derive-form      (%defstruct-derive-form name derived-classes)))
      (cons 'progn
            (append (list defclass-form)
                    (when ctor-form   (list ctor-form))
                    (when pred-form   (list pred-form))
                    (when copier-form (list copier-form))
                    (when print-form  (list print-form))
                    (when derive-form (list derive-form))
                    (list (%expander-form 'quote name)))))))
