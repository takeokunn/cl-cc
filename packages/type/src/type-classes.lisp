;;;; packages/type/src/type-classes.lisp — FR-711 Type Classes
;;;; Haskell-style type classes with implicit dictionary passing.
;;;; Rust traits / Scala implicits / Swift protocols equivalent.

(in-package :cl-cc/type)

(defvar *type-class-dicts* (make-hash-table :test #'eq)
  "Type class → (method-name . signature) alist.")

(defmacro define-type-class (name &rest methods)
  "Define a type class NAME with METHODS.
Usage: (define-type-class Eq (eq (-> a a boolean)))"
  `(setf (gethash ',name *type-class-dicts*) ',methods))

(defvar *type-class-instance-impls* (make-hash-table :test #'equal)
  "(class . type) → implementation alist.")

(defun register-instance-impl (class-name type-name impl)
  "Register IMPL as the implementation of CLASS-NAME for TYPE-NAME."
  (setf (gethash (cons class-name type-name) *type-class-instance-impls*) impl))

(defun resolve-instance-impl (class-name type-name)
  "Resolve the implementation of CLASS-NAME for TYPE-NAME at compile time."
  (gethash (cons class-name type-name) *type-class-instance-impls*))

;;; ──── Dictionary generation ────
(defun generate-type-class-dict (class-name type-name)
  "Generate a type class dictionary (vtable) for (CLASS-NAME TYPE-NAME)."
  (let* ((methods (gethash class-name *type-class-dicts*))
         (impl (resolve-instance-impl class-name type-name)))
    (unless impl
      (error "No ~A instance for ~A" class-name type-name))
    ;; Generate a struct with function pointers for each method
    (loop for (method _) in methods
          for impl-fn = (getf impl method)
          collect (cons method impl-fn))))

;;; ──── Common type classes ────
(define-type-class 'Eq (eq (-> 'a 'a 'boolean)))
(define-type-class 'Ord (lt (-> 'a 'a 'boolean))
                        (le (-> 'a 'a 'boolean)))
(define-type-class 'Show (show (-> 'a 'string)))
(define-type-class 'From (from (-> 'a 'b)))
