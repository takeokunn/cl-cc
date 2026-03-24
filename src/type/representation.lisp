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

    ;; Forall types (Rank-N) - compare var and body
    ((and (typep type1 'type-forall)
          (typep type2 'type-forall))
     (and (type-equal-p (type-forall-var type1) (type-forall-var type2))
          (type-equal-p (type-forall-type type1) (type-forall-type type2))))

    ;; Skolem constants - compare by ID
    ((and (typep type1 'type-skolem) (typep type2 'type-skolem))
     (type-skolem-equal-p type1 type2))

    ;; Effect types - compare names
    ((and (typep type1 'type-effect)
          (typep type2 'type-effect))
     (eq (type-effect-name type1) (type-effect-name type2)))

    ;; Effect row types - compare effects and row var
    ((and (typep type1 'type-effect-row)
          (typep type2 'type-effect-row))
     (and (= (length (type-effect-row-effects type1))
             (length (type-effect-row-effects type2)))
          (every #'type-equal-p
                 (type-effect-row-effects type1)
                 (type-effect-row-effects type2))
          (if (and (type-effect-row-row-var type1)
                   (type-effect-row-row-var type2))
              (type-equal-p (type-effect-row-row-var type1)
                            (type-effect-row-row-var type2))
              (eq (type-effect-row-row-var type1)
                  (type-effect-row-row-var type2)))))

    ;; Qualified types - compare constraints and type
    ((and (typep type1 'type-qualified)
          (typep type2 'type-qualified))
     (and (= (length (type-qualified-constraints type1))
             (length (type-qualified-constraints type2)))
          (type-equal-p (type-qualified-type type1)
                        (type-qualified-type type2))))

    ;; Typeclass constraint - compare class name and type arg
    ((and (typep type1 'type-class-constraint)
          (typep type2 'type-class-constraint))
     (and (eq (type-class-constraint-class-name type1)
              (type-class-constraint-class-name type2))
          (type-equal-p (type-class-constraint-type-arg type1)
                        (type-class-constraint-type-arg type2))))

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

;;; Phase 4: Typeclass Representation
;;;
;;; Haskell-style typeclasses: class Eq a where (==) :: a -> a -> Bool
;;;   type-class: the class definition
;;;   type-class-constraint: (Eq a) constraint on a type variable
;;;   type-qualified: (Eq a) => a -> a -> Bool — a type with constraints

(defstruct (type-class (:include type-node))
  "A typeclass definition. NAME is the class symbol (e.g. EQ, NUM).
TYPE-PARAM is the class type parameter. METHODS is an alist (name . type-function).
DEFAULTS is an alist (name . impl) for default method implementations.
SUPERCLASSES is a list of superclass name symbols."
  (name nil :type symbol)
  (type-param nil)
  (methods nil :type list)
  (defaults nil :type list)
  (superclasses nil :type list))

(defstruct (type-class-constraint (:include type-node))
  "A typeclass constraint: (Num a), (Eq a), (Show a), etc.
CLASS-NAME is the class symbol. TYPE-ARG is the constrained type (often a variable)."
  (class-name nil :type symbol)
  (type-arg nil))

(defstruct (type-qualified (:include type-node))
  "A type qualified by typeclass constraints: (Num a) => a -> a -> a.
CONSTRAINTS is a list of type-class-constraint. TYPE is the underlying type."
  (constraints nil :type list)
  (type nil))

;;; Phase 5: Effect Types
;;;
;;; Row-based effect system (à la Koka/Frank):
;;;   type-effect: a single effect label (IO, State, Error)
;;;   type-effect-row: a set of effects + optional polymorphic row variable
;;;   type-effectful-function: function type annotated with an effect row

(defstruct (type-effect (:include type-node))
  "A single effect label, e.g., IO, State, Error, Exn."
  (name nil :type symbol))

(defstruct (type-effect-row (:include type-node))
  "An effect row: a (possibly polymorphic) set of effects.
EFFECTS is a list of type-effect. ROW-VAR is an optional type-variable for open rows."
  (effects nil :type list)
  (row-var nil))

(defvar +pure-effect-row+
  (make-type-effect-row :effects nil :row-var nil)
  "The empty (pure) effect row — no effects.")

(defvar +io-effect-row+
  (make-type-effect-row :effects (list (make-type-effect :name 'io)) :row-var nil)
  "The IO effect row.")

(defstruct (type-effectful-function (:include type-function))
  "A function type annotated with an effect row: A -> B ! {IO, State}.
Inherits PARAMS and RETURN from type-function. EFFECTS is a type-effect-row."
  (effects nil))

;;; Phase 6: Rank-N Polymorphism
;;;
;;; Standard HM infers ∀ only at let-bindings (rank-1). Rank-N allows
;;; ∀ in argument positions:  (∀a. a -> a) -> Int -> Int
;;;   type-forall: explicit universal quantification

(defstruct (type-forall (:include type-node))
  "Explicit universal quantification: ∀ VAR . TYPE.
VAR is a type-variable (the bound variable). TYPE is the body type."
  (var nil)
  (type nil))

;;; Skolem Constants (for Rank-N type checking)
;;;
;;; When checking an expression against (forall a T), we introduce a skolem
;;; constant — a rigid type variable that cannot be unified with other type
;;; variables. After checking, we verify the skolem does not escape its scope.

(defstruct (type-skolem (:include type-node) (:constructor make-type-skolem-raw))
  "A skolem constant (rigid type variable) for Rank-N type checking.
ID is a unique integer. NAME is a human-readable debug name.
Skolems cannot be unified with type variables — they represent
unknown-but-fixed types introduced by universal quantification."
  (id 0 :type integer)
  (name nil))

(defvar *skolem-counter* 0
  "Global counter for generating unique skolem IDs.")

(defun make-type-skolem (&optional name)
  "Create a fresh skolem constant with a unique ID."
  (incf *skolem-counter*)
  (make-type-skolem-raw :id *skolem-counter* :name name))

(defun type-skolem-equal-p (s1 s2)
  "Check if two skolems are the same (by ID)."
  (and (typep s1 'type-skolem)
       (typep s2 'type-skolem)
       (= (type-skolem-id s1) (type-skolem-id s2))))

;;; type-to-string methods for Phase 4-6 types

(defmethod type-to-string ((type type-class))
  (format nil "class ~A ~A { ~{~A~^; ~} }"
          (type-class-name type)
          (if (type-class-type-param type)
              (type-to-string (type-class-type-param type))
              "_")
          (mapcar (lambda (m) (format nil "~A :: ~A" (car m) (type-to-string (cdr m))))
                  (type-class-methods type))))

(defmethod type-to-string ((type type-class-constraint))
  (format nil "(~A ~A)"
          (type-class-constraint-class-name type)
          (type-to-string (type-class-constraint-type-arg type))))

(defmethod type-to-string ((type type-qualified))
  (if (null (type-qualified-constraints type))
      (type-to-string (type-qualified-type type))
      (format nil "(~{~A~^, ~}) => ~A"
              (mapcar #'type-to-string (type-qualified-constraints type))
              (type-to-string (type-qualified-type type)))))

(defmethod type-to-string ((type type-effect))
  (symbol-name (type-effect-name type)))

(defmethod type-to-string ((type type-effect-row))
  (let ((effects (type-effect-row-effects type))
        (row-var (type-effect-row-row-var type)))
    (cond
      ((and (null effects) (null row-var)) "{}")
      ((null row-var)
       (format nil "{~{~A~^, ~}}" (mapcar #'type-to-string effects)))
      ((null effects)
       (format nil "{~A..}" (type-to-string row-var)))
      (t
       (format nil "{~{~A~^, ~} | ~A}"
               (mapcar #'type-to-string effects)
               (type-to-string row-var))))))

(defmethod type-to-string ((type type-effectful-function))
  (let ((params (mapcar #'type-to-string (type-function-params type)))
        (ret (type-to-string (type-function-return type)))
        (eff (type-to-string (type-effectful-function-effects type))))
    (if (= 1 (length params))
        (format nil "~A -[~A]-> ~A" (first params) eff ret)
        (format nil "(~{~A~^ -> ~}) -[~A]-> ~A" params eff ret))))

(defmethod type-to-string ((type type-forall))
  (format nil "(∀~A. ~A)"
          (type-to-string (type-forall-var type))
          (type-to-string (type-forall-type type))))

(defmethod type-to-string ((type type-skolem))
  (if (type-skolem-name type)
      (format nil "sk~A[~A]" (type-skolem-id type) (type-skolem-name type))
      (format nil "sk~A" (type-skolem-id type))))
