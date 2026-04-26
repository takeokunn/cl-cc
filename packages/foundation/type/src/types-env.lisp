;;;; types-env.lisp — Type Schemes, Environments, Unknown Sentinels, and Constructor Views
;;;;
;;;; type-scheme, type-env (with lookup/extend/free-vars operations),
;;;; the canonical unknown sentinel, constructor view helpers for curried type-app
;;;; nodes, deftype forms for typep-based tests, and the early type-to-string stub.
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

(defun type-env-free-vars (env)
  "All free type-vars in ENV's type schemes."
  (remove-duplicates
   (mapcan (lambda (entry)
             (let ((s (cdr entry)))
               (let ((inner (if (type-scheme-p s) (type-scheme-type s) s)))
                 (type-free-vars inner))))
            (type-env-bindings env))
   :test #'type-var-equal-p))

;;; ─── Unknown sentinel ─────────────────────────────────────────────────────

(defun type-unknown-p (value)
  (and (type-error-p value)
       (string= (type-error-message value) "unknown")))

(defvar +type-unknown+ (make-type-error :message "unknown")
  "Singleton unknown sentinel used for error recovery and gradual edges.")

;;; ─── Constructor view over curried type-app chains ───────────────────────

(defun %option-type-constructor-p (value)
  (and (type-union-p value)
       (= (length (type-union-types value)) 2)
       (some (lambda (ty) (type-equal-p ty type-null))
             (type-union-types value))))

(defun type-constructor-p (value)
  (or (type-app-p value)
      (%option-type-constructor-p value)))

(defun type-constructor-name (ty)
  "Return the head constructor name from a curried type-app chain." 
  (cond
    ((type-app-p ty)
     (loop for node = ty then (type-app-fun node)
           while (type-app-p node)
           finally (return (when (type-primitive-p node)
                             (type-primitive-name node)))))
    ((%option-type-constructor-p ty)
     (or (type-union-constructor-name ty)
         (intern "OPTION" *package*)))))

(defun type-constructor-args (ty)
  "Return all arguments from a curried type-app chain as a flat list." 
  (cond
    ((type-app-p ty)
     (loop for node = ty then (type-app-fun node)
           while (type-app-p node)
           collect (type-app-arg node) into args-rev
           finally (return (nreverse args-rev))))
    ((%option-type-constructor-p ty)
     (remove-if (lambda (ty0) (type-equal-p ty0 type-null))
                (type-union-types ty)))))

(defun make-type-constructor (name args)
  "Build a curried type-app chain for constructor NAME with ARGS list." 
  (reduce (lambda (acc arg) (make-type-app :fun acc :arg arg))
          args
          :initial-value (make-type-primitive :name name)))

;;; ─── Deftype names used in tests / structural assertions ─────────────────

(deftype type-constructor ()
  '(or type-app (and type-union (satisfies %option-type-constructor-p))))

;;; ─── Type printer protocol ────────────────────────────────────────────────

(defgeneric type-to-string (ty)
  (:documentation "Convert a type-node TY to a human-readable string.")
  (:method (ty)
    (format nil "#<type ~A>" (type-of ty))))

(defmethod type-to-string ((ty null))
  "NIL")

(defmethod type-to-string ((ty type-primitive))
  (symbol-name (type-primitive-name ty)))

(defmethod type-to-string ((ty type-var))
  (let ((link (type-var-link ty)))
    (cond
      (link              (type-to-string link))
      ((type-var-name ty) (format nil "?~A" (type-var-name ty)))
      (t                 (format nil "?t~D" (type-var-id ty))))))

(defmethod type-to-string ((ty type-rigid))
  (if (type-rigid-name ty)
      (format nil "sk~D[~A]" (type-rigid-id ty) (type-rigid-name ty))
      (format nil "sk~D"     (type-rigid-id ty))))

(defmethod type-to-string ((ty type-error))
  (if (string= (type-error-message ty) "unknown")
      "?"
      (type-error-message ty)))
