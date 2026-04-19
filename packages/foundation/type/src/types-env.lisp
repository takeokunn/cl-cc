;;;; types-env.lisp — Type Schemes, Environments, and Backward-Compat API
;;;;
;;;; type-scheme, type-env (with lookup/extend/free-vars operations),
;;;; backward-compatibility aliases for old callers, deftype forms for
;;;; typep-based tests, and the type-to-string stub.
;;;; Loads after types-extended.lisp.

(in-package :cl-cc/type)

;;; ─── Type Schemes ─────────────────────────────────────────────────────────

(defstruct (type-scheme (:constructor %make-type-scheme))
  "A type scheme ∀ā. T — type with universally quantified variables.
QUANTIFIED-VARS: list of type-var nodes (the bound variables).
TYPE:            the body type."
  (quantified-vars nil :type list)
  (type            nil))

(defun make-type-scheme (quantified-vars ty)
  (%make-type-scheme :quantified-vars quantified-vars :type ty))

(defun type-to-scheme (ty)
  "Wrap TY in a monomorphic scheme (no quantified variables)."
  (make-type-scheme nil ty))

;;; ─── Type Environment ─────────────────────────────────────────────────────

(defstruct type-env
  "A type environment mapping symbols to type-scheme nodes.
BINDINGS:      alist of (symbol . type-scheme).
DICT-BINDINGS: alist of ((class-name . type-key) . method-alist) for typeclass dicts."
  (bindings      nil :type list)
  (dict-bindings nil :type list))

(defun type-env-empty ()
  "Return an empty type environment."
  (make-type-env :bindings nil))

(defun type-env-lookup (name env)
  "Look up NAME in ENV. Returns (values scheme found-p)."
  (let ((entry (assoc name (type-env-bindings env))))
    (if entry
        (values (cdr entry) t)
        (values nil nil))))

(defun type-env-extend (name scheme env)
  "Return a new environment with NAME bound to SCHEME."
  (make-type-env :bindings      (acons name scheme (type-env-bindings env))
                 :dict-bindings (type-env-dict-bindings env)))

(defun type-env-extend* (bindings env)
  "Extend ENV with a list of (NAME . SCHEME) bindings."
  (make-type-env :bindings      (append bindings (type-env-bindings env))
                 :dict-bindings (type-env-dict-bindings env)))

(defun type-env-to-alist (env)
  (type-env-bindings env))

(defun type-env-free-vars (env)
  "All free type-vars in ENV's type schemes."
  (remove-duplicates
   (mapcan (lambda (entry)
             (let ((s (cdr entry)))
               (let ((inner (if (type-scheme-p s) (type-scheme-type s) s)))
                 (type-free-vars inner))))
           (type-env-bindings env))
   :test #'type-var-equal-p))

;;; Backward-compat aliases moved to types-env-compat.lisp.
