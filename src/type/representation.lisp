;;;; representation.lisp - Type Representation Classes
;;;;
;;;; This module provides CLOS classes for representing types in a
;;;; Hindley-Milner type system with gradual typing support.
;;;;
;;;; Type hierarchy:
;;;;   type-node (base class)
;;;;     ├── type-primitive (int, string, bool, etc.)
;;;;     ├── type-variable (type variables for polymorphism ?a, ?b)
;;;;     ├── type-function (function types T1 -> T2 -> R)
;;;;     ├── type-tuple (product types (T1, T2, ...))
;;;;     ├── type-union (union types T1 | T2 for gradual typing)
;;;;     ├── type-intersection (intersection types T1 & T2)
;;;;     └── type-unknown (escape hatch for gradual typing)

(in-package :cl-cc/type)

;;; ----------------------------------------------------------------------------
;;; Type Base Class
;;; ----------------------------------------------------------------------------

(defclass type-node ()
  ((source-location :initarg :source-location
                    :initform nil
                    :accessor type-node-source-location
                    :documentation "Optional source location for error messages."))
  (:documentation "Base class for all type representations in the HM type system."))

;;; ----------------------------------------------------------------------------
;;; Primitive Types
;;; ----------------------------------------------------------------------------

(defclass type-primitive (type-node)
  ((name :initarg :name
         :reader type-primitive-name
         :type symbol
         :documentation "Name of the primitive type (e.g., fixnum, string, boolean)."))
  (:documentation "Represents a primitive type like fixnum, string, boolean, etc."))

;;; ----------------------------------------------------------------------------
;;; Type Variables (for polymorphism)
;;; ----------------------------------------------------------------------------

(defvar *type-variable-counter* 0
  "Global counter for generating unique type variable IDs.")

(defun make-type-variable (&optional name)
  "Create a fresh type variable with a unique ID.
   NAME is an optional human-readable name for debugging."
  (incf *type-variable-counter*)
  (make-instance 'type-variable
                 :id *type-variable-counter*
                 :name name))

(defclass type-variable (type-node)
  ((id :initarg :id
       :reader type-variable-id
       :type integer
       :documentation "Unique identifier for this type variable.")
   (name :initarg :name
         :initform nil
         :reader type-variable-name
         :type (or null symbol)
         :documentation "Optional human-readable name for debugging."))
  (:documentation "Represents a type variable for polymorphic types (e.g., ?a, ?b)."))

(defun reset-type-variable-counter ()
  "Reset the type variable counter. Useful for testing."
  (setf *type-variable-counter* 0))

;;; ----------------------------------------------------------------------------
;;; Function Types
;;; ----------------------------------------------------------------------------

(defclass type-function (type-node)
  ((params :initarg :params
           :reader type-function-params
           :type list
           :documentation "List of parameter types.")
   (return :initarg :return
           :reader type-function-return
           :type type-node
           :documentation "Return type of the function."))
  (:documentation "Represents a function type: T1 -> T2 -> ... -> R."))

(defun make-type-function (params return)
  "Convenience constructor for function types.
   PARAMS is a list of type nodes, RETURN is the return type."
  (make-instance 'type-function
                 :params params
                 :return return))

;;; ----------------------------------------------------------------------------
;;; Tuple/Product Types
;;; ----------------------------------------------------------------------------

(defclass type-tuple (type-node)
  ((elements :initarg :elements
             :reader type-tuple-elements
             :type list
             :documentation "List of element types in the tuple."))
  (:documentation "Represents a tuple/product type: (T1, T2, ...)."))

(defun make-type-tuple (elements)
  "Convenience constructor for tuple types.
   ELEMENTS is a list of type nodes."
  (make-instance 'type-tuple
                 :elements elements))

;;; ----------------------------------------------------------------------------
;;; Union Types (for gradual typing)
;;; ----------------------------------------------------------------------------

(defclass type-union (type-node)
  ((types :initarg :types
          :reader type-union-types
          :type list
          :documentation "List of types in the union."))
  (:documentation "Represents a union type: T1 | T2 | ... for gradual typing."))

(defun make-type-union (types)
  "Convenience constructor for union types.
   TYPES is a list of type nodes."
  (make-instance 'type-union
                 :types types))

;;; ----------------------------------------------------------------------------
;;; Intersection Types
;;; ----------------------------------------------------------------------------

(defclass type-intersection (type-node)
  ((types :initarg :types
          :reader type-intersection-types
          :type list
          :documentation "List of types in the intersection."))
  (:documentation "Represents an intersection type: T1 & T2 & ..."))

(defun make-type-intersection (types)
  "Convenience constructor for intersection types.
   TYPES is a list of type nodes."
  (make-instance 'type-intersection
                 :types types))

;;; ----------------------------------------------------------------------------
;;; Type Constructor (parametric/generic types)
;;; ----------------------------------------------------------------------------

(defclass type-constructor (type-node)
  ((name :initarg :name
         :reader type-constructor-name
         :type symbol
         :documentation "Name of the type constructor (e.g., List, Option, Pair).")
   (args :initarg :args
         :reader type-constructor-args
         :type list
         :documentation "List of type arguments applied to this constructor."))
  (:documentation "Represents a parametric type like (List fixnum), (Option string).
                   The NAME is the type constructor, ARGS are the type parameters."))

(defun make-type-constructor (name args)
  "Convenience constructor for parametric types.
   NAME is a symbol, ARGS is a list of type nodes."
  (make-instance 'type-constructor
                 :name name
                 :args args))

;;; ----------------------------------------------------------------------------
;;; Unknown Type (escape hatch for gradual typing)
;;; ----------------------------------------------------------------------------

(defclass type-unknown (type-node)
  ()
  (:documentation "Represents an unknown type - escape hatch for gradual typing.
                   Used when type information is unavailable or when explicit
                   gradual typing is desired."))

;;; ----------------------------------------------------------------------------
;;; Singleton Type Instances
;;; ----------------------------------------------------------------------------

(defvar type-int (make-instance 'type-primitive :name 'fixnum)
  "Singleton instance for fixnum type.")

(defvar type-string (make-instance 'type-primitive :name 'string)
  "Singleton instance for string type.")

(defvar type-bool (make-instance 'type-primitive :name 'boolean)
  "Singleton instance for boolean type.")

(defvar type-symbol (make-instance 'type-primitive :name 'symbol)
  "Singleton instance for symbol type.")

(defvar type-cons (make-instance 'type-primitive :name 'cons)
  "Singleton instance for cons type.")

(defvar type-null (make-instance 'type-primitive :name 'null)
  "Singleton instance for null type.")

(defvar type-any (make-instance 'type-primitive :name 't)
  "Singleton instance for universal type (top type).")

(defvar +type-unknown+ (make-instance 'type-unknown)
  "Singleton instance for unknown type (escape hatch for gradual typing).")

;;; ----------------------------------------------------------------------------
;;; Type Utility Functions
;;; ----------------------------------------------------------------------------

(defun type-equal-p (type1 type2)
  "Check structural equality of two types."
  (cond
    ;; Same class and primitive names
    ((and (typep type1 'type-primitive)
          (typep type2 'type-primitive))
     (eq (type-primitive-name type1)
         (type-primitive-name type2)))
    
    ;; Type variables - compare by ID
    ((and (typep type1 'type-variable)
          (typep type2 'type-variable))
     (= (type-variable-id type1)
        (type-variable-id type2)))
    
    ;; Unknown types are always equal
    ((and (typep type1 'type-unknown)
          (typep type2 'type-unknown))
     t)
    
    ;; Function types - compare params and return
    ((and (typep type1 'type-function)
          (typep type2 'type-function))
     (and (= (length (type-function-params type1))
             (length (type-function-params type2)))
          (every #'type-equal-p
                 (type-function-params type1)
                 (type-function-params type2))
          (type-equal-p (type-function-return type1)
                        (type-function-return type2))))
    
    ;; Tuple types - compare elements
    ((and (typep type1 'type-tuple)
          (typep type2 'type-tuple))
     (and (= (length (type-tuple-elements type1))
             (length (type-tuple-elements type2)))
          (every #'type-equal-p
                 (type-tuple-elements type1)
                 (type-tuple-elements type2))))
    
    ;; Union types - compare types (order-independent would be better)
    ((and (typep type1 'type-union)
          (typep type2 'type-union))
     (and (= (length (type-union-types type1))
             (length (type-union-types type2)))
          (every #'type-equal-p
                 (type-union-types type1)
                 (type-union-types type2))))
    
    ;; Intersection types - compare types
    ((and (typep type1 'type-intersection)
          (typep type2 'type-intersection))
     (and (= (length (type-intersection-types type1))
             (length (type-intersection-types type2)))
          (every #'type-equal-p
                 (type-intersection-types type1)
                 (type-intersection-types type2))))

    ;; Type constructors - compare name and args
    ((and (typep type1 'type-constructor)
          (typep type2 'type-constructor))
     (and (eq (type-constructor-name type1)
              (type-constructor-name type2))
          (= (length (type-constructor-args type1))
             (length (type-constructor-args type2)))
          (every #'type-equal-p
                 (type-constructor-args type1)
                 (type-constructor-args type2))))

    ;; Different classes
    (t nil)))

(defgeneric type-to-string (type)
  (:documentation "Convert a type to a human-readable string representation."))

(defmethod type-to-string ((type type-primitive))
  (symbol-name (type-primitive-name type)))

(defmethod type-to-string ((type type-variable))
  (if (type-variable-name type)
      (format nil "?~A" (type-variable-name type))
      (format nil "?t~D" (type-variable-id type))))

(defmethod type-to-string ((type type-function))
  (let ((params (mapcar #'type-to-string (type-function-params type)))
        (ret (type-to-string (type-function-return type))))
    (if (= 1 (length params))
        (format nil "~A -> ~A" (first params) ret)
        (format nil "(~{~A~^ -> ~}) -> ~A" params ret))))

(defmethod type-to-string ((type type-tuple))
  (format nil "(~{~A~^, ~})" (mapcar #'type-to-string (type-tuple-elements type))))

(defmethod type-to-string ((type type-union))
  (format nil "(~{~A~^ | ~})" (mapcar #'type-to-string (type-union-types type))))

(defmethod type-to-string ((type type-intersection))
  (format nil "(~{~A~^ & ~})" (mapcar #'type-to-string (type-intersection-types type))))

(defmethod type-to-string ((type type-constructor))
  (format nil "(~A~{ ~A~})"
          (type-constructor-name type)
          (mapcar #'type-to-string (type-constructor-args type))))

(defmethod type-to-string ((type type-unknown))
  "?")
