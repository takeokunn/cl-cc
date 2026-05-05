;;;; packages/type/src/inference.lisp - HM Inference Registries and Helpers
;;;
;;; Contains: class type and alias registries, %infer-fn-binding, %find-fn-type-declaration.
;;;
;;; Load order: before inference-handlers.lisp.

(in-package :cl-cc/type)

;;; Type Inference State

(defvar *class-type-registry* (make-hash-table :test #'eq)
  "Maps class names to class type descriptors.
   Each entry is an alist of (slot-name . type-node) pairs.")

(defun register-class-type (name slot-types)
  "Register a class with its slot types for type inference.
   SLOT-TYPES is an alist of (slot-name . type-node)."
  (setf (gethash name *class-type-registry*) slot-types))

(defun lookup-class-type (name)
  "Look up registered slot types for class NAME. Returns alist or nil."
  (gethash name *class-type-registry*))

(defun lookup-slot-type (class-name slot-name)
  "Look up the type of SLOT-NAME in CLASS-NAME. Returns type-node or nil."
  (let ((slots (lookup-class-type class-name)))
    (when slots
      (cdr (assoc slot-name slots :test #'eq)))))

(defvar *class-method-type-registry* (make-hash-table :test #'eq)
  "Maps class names to protocol-visible method signatures.
Each entry is an alist of (method-name . type-node) pairs.")

(defun register-class-method-type (class-name method-name &optional (method-type type-any))
  "Register METHOD-NAME as structurally available on CLASS-NAME."
  (let* ((methods (gethash class-name *class-method-type-registry*))
         (entry (assoc method-name methods :test #'eq)))
    (if entry
        (setf (cdr entry) method-type)
        (push (cons method-name method-type) methods))
    (setf (gethash class-name *class-method-type-registry*) methods)))

(defun lookup-class-method-types (class-name)
  "Return protocol-visible method types for CLASS-NAME."
  (gethash class-name *class-method-type-registry*))

(defun lookup-class-method-type (class-name method-name)
  "Look up METHOD-NAME in CLASS-NAME as a structural protocol member."
  (let ((methods (lookup-class-method-types class-name)))
    (when methods
      (cdr (assoc method-name methods :test #'eq)))))

(defvar *type-alias-registry* (make-hash-table :test #'eq)
  "Maps type alias names to their expanded type specifiers.
   E.g., integer-or-string → (or fixnum string)")

(defun register-type-alias (name type-spec)
  "Register a type alias NAME expanding to TYPE-SPEC."
  (setf (gethash name *type-alias-registry*) type-spec))

(defun lookup-type-alias (name)
  "Look up a type alias. Returns expanded type-spec or nil."
  (gethash name *type-alias-registry*))

;;; NOTE: *type-var-counter*, fresh-type-var, reset-type-vars! are defined
;;;       in representation.lisp.
;;;       type-env struct and type-env-{empty,lookup,extend,extend*,to-alist,free-vars}
;;;       are defined in representation.lisp.
;;;       dict-env-extend / dict-env-lookup are defined in typeclass.lisp.
;;;       Do NOT redefine any of those here.

;;; Helpers

(defun %infer-fn-binding (binding lookup-env result-env)
  "Infer the type scheme for a single (name params . body) binding and extend RESULT-ENV.
   Parameters are looked up in LOOKUP-ENV (= parent env for flet, = mutual env for labels).
   Returns the new type environment."
  (let* ((name         (first binding))
         (params       (second binding))
         (body         (cddr binding))
         (param-types  (mapcar (lambda (p) (declare (ignore p)) (fresh-type-var)) params))
         (param-env    (type-env-extend* (mapcar (lambda (pname ptype)
                                                   (cons pname (make-type-scheme nil ptype)))
                                                 params param-types)
                                         lookup-env)))
    (multiple-value-bind (body-type subst) (infer-body body param-env)
      (let* ((fn-type (make-type-arrow-raw
                       :params (mapcar (lambda (p) (zonk p subst)) param-types)
                       :return body-type))
        (scheme  (generalize result-env fn-type)))
        (type-env-extend name scheme result-env)))))

;;; FR-004: Polymorphic Recursion Helpers

(defun %find-fn-type-declaration (fn-name declarations)
  "Scan DECLARATIONS for (type type-spec fn-name ...) and return type-spec.
   DECLARATIONS is the list from ast-callable-declarations.
   Returns a type specifier s-expression, or nil if not found."
  (when fn-name
    (loop for decl in declarations
          when (and (consp decl)
                    (eq (car decl) 'type)
                    (>= (length decl) 3)
                    (member fn-name (cddr decl) :test #'eq))
          return (second decl))))

;;; infer-* handlers, dispatcher (infer), and infer-binop → see inference-handlers.lisp
