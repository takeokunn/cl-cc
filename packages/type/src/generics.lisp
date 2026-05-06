;;;; generics.lisp — FR-1602 datatype-generic programming

(in-package :cl-cc/type)

(defstruct generic-u1
  "Unit representation for datatype-generic programming.")

(defstruct generic-k1
  "Constant-field representation carrying VALUE and its TYPE designator."
  value
  type)

(defstruct generic-m1
  "Metadata wrapper around a generic representation."
  meta
  representation)

(defstruct generic-product
  "Product representation for multiple fields."
  left
  right)

(defstruct generic-sum
  "Sum representation for variants; TAG names the selected alternative."
  tag
  value)

(defstruct generic-instance
  "Registered generic representation behavior for one host type designator."
  type
  representation
  show
  traverse)

(defvar *generic-instance-registry* (make-hash-table :test #'equal)
  "Maps type designators to FR-1602 datatype-generic instance descriptors.")

(defun register-generic-instance (type representation &key show traverse)
  "Register TYPE as Generic with REPRESENTATION and optional SHOW/TRAVERSE hooks."
  (let ((instance (make-generic-instance :type type
                                         :representation representation
                                         :show show
                                         :traverse traverse)))
    (setf (gethash type *generic-instance-registry*) instance)
    instance))

(defun lookup-generic-instance (type)
  "Return the registered Generic instance for TYPE, if present."
  (gethash type *generic-instance-registry*))

(defun generic-representation-of (value)
  "Return a concrete U1/K1/M1/:*:/:+: representation for VALUE."
  (let ((instance (lookup-generic-instance (type-of value))))
    (cond
      (instance
       (let ((representation (generic-instance-representation instance)))
         (if (functionp representation)
             (funcall representation value)
             representation)))
      ((null value)
       (make-generic-u1))
      ((consp value)
       (make-generic-product :left (generic-representation-of (car value))
                             :right (generic-representation-of (cdr value))))
      (t
       (make-generic-k1 :value value :type (type-of value))))))

(defun generic-show (value)
  "Render VALUE through its Generic instance or structural representation."
  (let ((instance (lookup-generic-instance (type-of value))))
    (cond
      ((and instance (generic-instance-show instance))
       (funcall (generic-instance-show instance) value))
      ((consp value)
       (format nil "(~{~A~^ ~})" (mapcar #'generic-show value)))
      (t
       (princ-to-string value)))))

(defun generic-transform (function value)
  "Apply FUNCTION to every leaf in VALUE using FR-1602 structural traversal."
  (let ((instance (lookup-generic-instance (type-of value))))
    (cond
      ((and instance (generic-instance-traverse instance))
       (funcall (generic-instance-traverse instance) function value))
      ((consp value)
       (mapcar (lambda (item) (generic-transform function item)) value))
      (t
       (funcall function value)))))

(defun generic-query (predicate value)
  "Collect all leaves in VALUE satisfying PREDICATE using generic traversal."
  (let (matches)
    (generic-transform (lambda (leaf)
                         (when (funcall predicate leaf)
                           (push leaf matches))
                         leaf)
                       value)
    (nreverse matches)))

(defun generic-representation-valid-p (representation)
  "Return T when REPRESENTATION is a supported FR-1602 representation node."
  (cond
    ((generic-u1-p representation) t)
    ((generic-k1-p representation) t)
    ((generic-m1-p representation)
     (generic-representation-valid-p (generic-m1-representation representation)))
    ((generic-product-p representation)
     (and (generic-representation-valid-p (generic-product-left representation))
          (generic-representation-valid-p (generic-product-right representation))))
    ((generic-sum-p representation) t)
    (t nil)))
