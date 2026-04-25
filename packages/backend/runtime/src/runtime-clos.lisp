;;;; packages/backend/runtime/src/runtime-clos.lisp - CL-CC Runtime: CLOS Descriptors + Generic Dispatch
;;;
;;; Contains: *rt-class-registry*, rt-defclass, rt-make-instance, rt-slot-*,
;;; rt-class-*, *rt-primitive-type-classifiers*, rt-register-method, rt-call-generic.
;;;
;;; Depends on runtime.lisp. Load order: after runtime-misc.lisp.

(in-package :cl-cc/runtime)

;;; ------------------------------------------------------------
;;; Class Registry
;;; ------------------------------------------------------------

(defvar *rt-class-registry* (make-hash-table :test #'eq)
  "Runtime class registry for native/self-hosted CLOS descriptors.")

(defun %rt-compute-class-precedence-list (class-name)
  "Compute a simple class precedence list from *rt-class-registry*."
  (labels ((walk (name seen)
             (if (member name seen :test #'eq)
                 seen
                 (let* ((class-ht (gethash name *rt-class-registry*))
                        (supers (and class-ht (gethash :__superclasses__ class-ht))))
                   (reduce (lambda (acc super) (walk super acc))
                           supers
                           :initial-value (append seen (list name)))))))
    (walk class-name '())))

(defun rt-defclass (name direct-supers slots)
  (let ((class-ht (or (gethash name *rt-class-registry*)
                      (make-hash-table :test #'eq))))
    (setf (gethash :__name__         class-ht) name
          (gethash :__superclasses__ class-ht) direct-supers
          (gethash :__slots__        class-ht) slots
          (gethash :__methods__      class-ht) (or (gethash :__methods__  class-ht)
                                                   (make-hash-table :test #'equal))
          (gethash :__eql-index__    class-ht) (or (gethash :__eql-index__ class-ht)
                                                   (make-hash-table :test #'equal))
          (gethash name *rt-class-registry*)   class-ht
          (gethash :__cpl__          class-ht) (%rt-compute-class-precedence-list name))
    class-ht))

;;; ------------------------------------------------------------
;;; Instance Access
;;; ------------------------------------------------------------

(defun rt-make-instance (class &rest initargs)
  (apply #'make-instance class initargs))

(defun rt-make-instance-0 (class)
  (make-instance class))

(defun rt-slot-value (obj slot-name)
  (slot-value obj slot-name))

(defun rt-slot-set (obj slot-name val)
  (setf (slot-value obj slot-name) val))

(defun rt-slot-boundp (obj slot-name)
  (if (slot-boundp obj slot-name) 1 0))

(defun rt-slot-makunbound (obj slot-name)
  (slot-makunbound obj slot-name))

(defun rt-slot-exists-p (obj slot-name)
  (if (slot-exists-p obj slot-name) 1 0))

(defun rt-class-name (class)
  (if (hash-table-p class)
      (gethash :__name__ class)
      (class-name class)))

(defun rt-class-of (obj) (class-of obj))

(defun rt-find-class (name)
  (or (gethash name *rt-class-registry*)
      (find-class name nil)))

;;; ------------------------------------------------------------
;;; EQL Specializer Support
;;; ------------------------------------------------------------

(defun %rt-eql-specializer-p (key)
  (and (consp key) (eq (car key) 'eql)))

(defun %rt-extract-eql-specializer-keys (specializer)
  (cond
    ((%rt-eql-specializer-p specializer)
     (list (second specializer)))
    ((and (consp specializer)
          (= (length specializer) 1)
          (%rt-eql-specializer-p (car specializer)))
     (list (second (car specializer))))
    (t nil)))

;;; ------------------------------------------------------------
;;; Generic Dispatch
;;; ------------------------------------------------------------

(defparameter *rt-primitive-type-classifiers*
  '((integer . integer)
    (string  . string)
    (symbol  . symbol))
  "Ordered (CL-type . dispatch-name) pairs for primitive argument classification.
Used by %rt-classify-arg for generic dispatch on non-CLOS values.")

(defun %rt-classify-arg (arg)
  "Return the dispatch type name for ARG.
CLOS descriptor hash tables → slot :__name__; primitives → *rt-primitive-type-classifiers*."
  (if (hash-table-p arg)
      (let ((class-ht (gethash :__class__ arg)))
        (if class-ht (gethash :__name__ class-ht) t))
      (or (cdr (assoc-if (lambda (type) (typep arg type))
                         *rt-primitive-type-classifiers*))
          t)))

(defun rt-register-method (gf specs method)
  (unless (hash-table-p gf)
    (error "rt-register-method expects a generic-function descriptor hash table, got ~S" gf))
  (let ((methods-ht (or (gethash :__methods__ gf)
                        (setf (gethash :__methods__ gf) (make-hash-table :test #'equal))))
        (eql-index (or (gethash :__eql-index__ gf)
                       (setf (gethash :__eql-index__ gf) (make-hash-table :test #'equal)))))
    (setf (gethash specs methods-ht) method)
    (dolist (key (%rt-extract-eql-specializer-keys specs))
      (pushnew method (gethash key eql-index) :test #'eq))
    method))

(defun %rt-lookup-method-by-class (methods-ht class-name)
  (or (gethash class-name methods-ht)
      (let ((class-ht (gethash class-name *rt-class-registry*)))
        (when class-ht
          (loop for ancestor in (cdr (gethash :__cpl__ class-ht))
                for method = (gethash ancestor methods-ht)
                when method return method)))
      (gethash t methods-ht)))

(defun rt-call-generic (gf &rest args)
  (if (hash-table-p gf)
      (let* ((methods-ht  (gethash :__methods__ gf))
             (eql-index   (gethash :__eql-index__ gf))
             (first-arg   (first args))
             (class-name  (%rt-classify-arg first-arg))
             (method      (or (and eql-index (first (gethash first-arg eql-index)))
                              (%rt-lookup-method-by-class methods-ht class-name))))
        (unless method
          (error "No applicable runtime generic method for ~S on ~S"
                 (gethash :__name__ gf) class-name))
        (apply method args))
      (apply gf args)))
