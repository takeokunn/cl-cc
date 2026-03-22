;;;; representation.lisp - Type Representation Classes
;;;;
;;;; This module provides structs for representing types in a
;;;; Hindley-Milner type system with gradual typing support.
;;;;
;;;; Type hierarchy:
;;;;   type-node (base struct)
;;;;     ├── type-primitive (int, string, bool, etc.)
;;;;     ├── type-variable (type variables for polymorphism ?a, ?b)
;;;;     ├── type-function (function types T1 -> T2 -> R)
;;;;     ├── type-tuple (product types (T1, T2, ...))
;;;;     ├── type-union (union types T1 | T2 for gradual typing)
;;;;     ├── type-intersection (intersection types T1 & T2)
;;;;     └── type-unknown (escape hatch for gradual typing)

(in-package :cl-cc/type)

;;; Type Base Struct

(defstruct type-node
  "Base type for all type representations in the HM type system."
  (source-location nil))

;;; Primitive Types

(defstruct (type-primitive (:include type-node))
  "Represents a primitive type like fixnum, string, boolean, etc."
  (name nil :type symbol))

;;; Type Variables (for polymorphism)

(defvar *type-variable-counter* 0
  "Global counter for generating unique type variable IDs.")

(defstruct (type-variable (:include type-node) (:constructor make-type-variable-raw))
  "Represents a type variable for polymorphic types (e.g., ?a, ?b)."
  (id 0 :type integer)
  (name nil))

(defun make-type-variable (&optional name)
  "Create a fresh type variable with a unique ID.
   NAME is an optional human-readable name for debugging."
  (incf *type-variable-counter*)
  (make-type-variable-raw :id *type-variable-counter* :name name))

(defun reset-type-variable-counter ()
  "Reset the type variable counter. Useful for testing."
  (setf *type-variable-counter* 0))

;;; Function Types

(defstruct (type-function (:include type-node) (:constructor make-type-function-raw))
  "Represents a function type: T1 -> T2 -> ... -> R."
  (params nil :type list)
  (return nil))

(defun make-type-function (params return)
  "Convenience constructor for function types.
   PARAMS is a list of type nodes, RETURN is the return type."
  (make-type-function-raw :params params :return return))

;;; Tuple/Product Types

(defstruct (type-tuple (:include type-node) (:constructor make-type-tuple-raw))
  "Represents a tuple/product type."
  (elements nil :type list))

(defun make-type-tuple (elements)
  "Convenience constructor for tuple types.
   ELEMENTS is a list of type nodes."
  (make-type-tuple-raw :elements elements))

;;; Union Types (for gradual typing)

(defstruct (type-union (:include type-node) (:constructor make-type-union-raw))
  "Represents a union type."
  (types nil :type list))

(defun make-type-union (types)
  "Convenience constructor for union types.
   TYPES is a list of type nodes."
  (make-type-union-raw :types types))

;;; Intersection Types

(defstruct (type-intersection (:include type-node) (:constructor make-type-intersection-raw))
  "Represents an intersection type."
  (types nil :type list))

(defun make-type-intersection (types)
  "Convenience constructor for intersection types.
   TYPES is a list of type nodes."
  (make-type-intersection-raw :types types))

;;; Type Constructor (parametric/generic types)

(defstruct (type-constructor (:include type-node) (:constructor make-type-constructor-raw))
  "Represents a parametric type like (List fixnum)."
  (name nil :type symbol)
  (args nil :type list))

(defun make-type-constructor (name args)
  "Convenience constructor for parametric types.
   NAME is a symbol, ARGS is a list of type nodes."
  (make-type-constructor-raw :name name :args args))

;;; Unknown Type (escape hatch for gradual typing)

(defstruct (type-unknown (:include type-node))
  "Represents an unknown type - escape hatch for gradual typing.")

;;; Singleton Type Instances

(defvar type-int (make-type-primitive :name 'fixnum)
  "Singleton instance for fixnum type.")

(defvar type-string (make-type-primitive :name 'string)
  "Singleton instance for string type.")

(defvar type-bool (make-type-primitive :name 'boolean)
  "Singleton instance for boolean type.")

(defvar type-symbol (make-type-primitive :name 'symbol)
  "Singleton instance for symbol type.")

(defvar type-cons (make-type-primitive :name 'cons)
  "Singleton instance for cons type.")

(defvar type-null (make-type-primitive :name 'null)
  "Singleton instance for null type.")

(defvar type-any (make-type-primitive :name 't)
  "Singleton instance for universal type (top type).")

(defvar +type-unknown+ (make-type-unknown)
  "Singleton instance for unknown type (escape hatch for gradual typing).")

;;; Type Utility Functions

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
