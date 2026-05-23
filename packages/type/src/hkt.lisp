;;;; packages/type/src/hkt.lisp — FR-592 Higher-Kinded Types
;;;; Type constructors as type parameters: Functor, Monad, etc.
;;;; Haskell type classes / Scala implicits / Rust traits equivalent.

(in-package :cl-cc/type)

;;; ──── Kind representation ────
;; Kinds classify types: * (concrete), *→* (type constructor), (*→*)→* (higher-kinded)
(defstruct (kind (:conc-name kind-))
  "A kind: * (star/concrete) | (-> arg-kind result-kind) | (kind-var id)"
  (kind-var nil :type (or null symbol))
  (arg-kind nil)
  (result-kind nil))

(defconstant +star-kind+ (make-kind)
  "The kind of concrete types (kind *).")

(defun arrow-kind (from to)
  "Create a function kind: FROM → TO."
  (make-kind :arg-kind from :result-kind to))

(defun kind-var (id)
  "Create a kind variable identified by ID."
  (make-kind :kind-var id))

;;; ──── Kind checking ────
(defvar *kind-env* (make-hash-table :test #'eq)
  "Type constructor → kind mapping.")

(defun register-type-constructor (name kind)
  "Register a type constructor NAME with its KIND."
  (setf (gethash name *kind-env*) kind))

(defun kind-of (type-name)
  "Return the kind of TYPE-NAME, or * if unknown."
  (or (gethash type-name *kind-env*) +star-kind+))

(defun check-kind (type-expr expected-kind)
  "Verify TYPE-EXPR has EXPECTED-KIND."
  (let ((actual-kind (infer-kind type-expr)))
    (unless (kind-equal-p actual-kind expected-kind)
      (error "Kind mismatch: expected ~A, got ~A"
             (kind-to-string expected-kind)
             (kind-to-string actual-kind)))))

(defun infer-kind (type-expr)
  "Infer the kind of TYPE-EXPR."
  (typecase type-expr
    (symbol (kind-of type-expr))
    (cons
     (let ((fn-kind (infer-kind (car type-expr))))
       (if (kind-arg-kind fn-kind)
           (kind-result-kind fn-kind)
           +star-kind+)))
    (t +star-kind+)))

;;; ──── Kind equality ────
(defun kind-equal-p (k1 k2)
  "Return T if K1 and K2 are equal kinds."
  (or (eq k1 k2)
      (and (kind-arg-kind k1) (kind-arg-kind k2)
           (kind-equal-p (kind-arg-kind k1) (kind-arg-kind k2))
           (kind-equal-p (kind-result-kind k1) (kind-result-kind k2)))
      (and (kind-kind-var k1) (kind-kind-var k2)
           (eq (kind-kind-var k1) (kind-kind-var k2)))))

;;; ──── Kind printing ────
(defun kind-to-string (kind)
  "Convert KIND to a human-readable string."
  (cond
    ((null (kind-arg-kind kind)) "*")
    ((kind-kind-var kind) (format nil "k~A" (kind-kind-var kind)))
    (t (format nil "~A → ~A"
               (kind-to-string (kind-arg-kind kind))
               (kind-to-string (kind-result-kind kind))))))

;;; ──── Type class interface (HKT-based) ────
(defstruct (type-class (:conc-name tc-))
  "A type class definition: interface that types can implement."
  (name nil :type symbol)           ; class name
  (type-param nil :type symbol)     ; type parameter (e.g., 'f in Functor f)
  (kind nil)                        ; kind of type parameter
  (methods nil :type list))         ; list of method signatures

(defvar *type-classes* (make-hash-table :test #'eq)
  "Type class name → type-class struct.")

(defmacro defclass-type (name type-param kind &rest methods)
  "Define a type class (Haskell-style type class).
Usage: (defclass-type Functor (f) (* → *) (fmap (-> (-> a b) (f a) (f b))))"
  `(setf (gethash ',name *type-classes*)
         (make-type-class :name ',name
                          :type-param ',type-param
                          :kind ,kind
                          :methods ',methods)))

;;; ──── Instance definitions ────
(defvar *type-class-instances* (make-hash-table :test #'eq)
  "Type class name → ((type . impl) ...) alist.")

(defun register-instance (class-name type-name implementation)
  "Register IMPLEMENTATION of CLASS-NAME for TYPE-NAME."
  (let ((instances (gethash class-name *type-class-instances*)))
    (push (cons type-name implementation)
          (or instances
              (setf (gethash class-name *type-class-instances*)
                    (list (cons type-name implementation)))))
    (values)))

(defun lookup-instance (class-name type-name)
  "Look up the implementation of CLASS-NAME for TYPE-NAME."
  (cdr (assoc type-name (gethash class-name *type-class-instances*)
              :test #'eq)))

;;; ──── Common kinds ────
;; Register built-in type constructors with their kinds
(eval-when (:load-toplevel :execute)
  (register-type-constructor 'list (arrow-kind +star-kind+ +star-kind+))
  (register-type-constructor 'vector (arrow-kind +star-kind+ +star-kind+))
  (register-type-constructor 'function
    (arrow-kind +star-kind+ (arrow-kind +star-kind+ +star-kind+)))
  (register-type-constructor 'optional (arrow-kind +star-kind+ +star-kind+))
  (register-type-constructor 'either
    (arrow-kind +star-kind+ (arrow-kind +star-kind+ +star-kind+))))
