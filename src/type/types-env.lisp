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

;;; ─── Backward-compat aliases ──────────────────────────────────────────────
;;; Kept so the compiler pipeline (which uses old names) still loads cleanly.
;;; They can be removed once those callers are updated.

(defun make-type-variable (&optional name)
  "Alias for fresh-type-var — backward compatibility."
  (fresh-type-var name))

(defun type-variable-p (x)       (type-var-p x))
(defun type-variable-id (v)      (type-var-id v))
(defun type-variable-name (v)    (type-var-name v))
(defun type-variable-equal-p (a b) (type-var-equal-p a b))

(defun make-type-tuple (types)
  "Alias for (make-type-product :elems types) — backward compatibility."
  (make-type-product :elems types))

(defun make-type-tuple-raw (&key elems source-location)
  (declare (ignore source-location))
  (make-type-product :elems elems))

(defun type-tuple-elements (x) (type-product-elems x))

(defun make-type-function (params return)
  "Alias for make-type-arrow (pure, ω) — backward compatibility."
  (make-type-arrow params return))

(defun make-type-function-raw (&key params return source-location)
  (declare (ignore source-location))
  (make-type-arrow params return))

(defun type-function-p (x) (type-arrow-p x))
(defun type-function-params (a) (type-arrow-params a))
(defun type-function-return (a) (type-arrow-return a))

(defun make-type-unknown ()
  "Backward compat: create a type-error sentinel as unknown."
  (make-type-error :message "unknown"))

(defun type-unknown-p (x)
  (and (type-error-p x) (string= (type-error-message x) "unknown")))

(defvar +type-unknown+ (make-type-error :message "unknown")
  "Backward-compat singleton for the old gradual typing escape hatch.")

;;; ─── type-constructor backward-compat API over curried type-app ──────────
;;; Old code used (make-type-constructor 'list (list T)) which produced a flat
;;; struct with :name and :args slots. New code uses curried type-app chains.
;;; These adapters encode/decode the curried representation.

(defun type-constructor-p (x) (type-app-p x))

(defun type-constructor-name (ty)
  "Return the head constructor name from a curried type-app chain."
  (when (type-app-p ty)
    (loop for node = ty then (type-app-fun node)
          while (type-app-p node)
          finally (return (when (type-primitive-p node)
                            (type-primitive-name node))))))

(defun type-constructor-args (ty)
  "Return all arguments from a curried type-app chain as a flat list."
  (when (type-app-p ty)
    (loop for node = ty then (type-app-fun node)
          while (type-app-p node)
          collect (type-app-arg node) into args-rev
          finally (return (nreverse args-rev)))))

(defun make-type-constructor (name args)
  "Build a curried type-app chain for constructor NAME with ARGS list."
  (reduce (lambda (acc arg) (make-type-app :fun acc :arg arg))
          args
          :initial-value (make-type-primitive :name name)))

(defun make-type-constructor-raw (name args)
  "Alias for make-type-constructor — backward compatibility."
  (make-type-constructor name args))

;;; ─── deftype aliases for typep-based test assertions ─────────────────────
;;; Tests call (typep x 'type-variable) etc. via assert-type macro.
;;; These deftype forms make those type names valid CL type specifiers.

(deftype type-variable   () 'type-var)
(deftype type-function   () 'type-arrow)
(deftype type-unknown    () '(and type-error (satisfies type-unknown-p)))
(deftype type-tuple      () 'type-product)
(deftype type-constructor () 'type-app)

;;; ─── type-to-string forward declaration ──────────────────────────────────
;;; The real implementation is in parser.lisp / printer.lisp (loaded later).
;;; We define a stub here so other type module files can call it early;
;;; the stub is overridden once the printer loads.

(defun type-to-string (ty)
  "Convert a type to a human-readable string.
This stub is overridden by the printer module when loaded."
  (cond
    ((null ty) "NIL")
    ((type-primitive-p ty) (symbol-name (type-primitive-name ty)))
    ((type-var-p ty)
     (if (type-var-name ty)
         (format nil "?~A" (type-var-name ty))
         (format nil "?t~D" (type-var-id ty))))
    ((type-rigid-p ty)
     (if (type-rigid-name ty)
         (format nil "!~A" (type-rigid-name ty))
         (format nil "!r~D" (type-rigid-id ty))))
    ((type-arrow-p ty)
     (let ((ps (mapcar #'type-to-string (type-arrow-params ty)))
           (r  (type-to-string (type-arrow-return ty))))
       (if (null ps)
           (format nil "() -> ~A" r)
           (format nil "~{~A~^ ~} -> ~A" ps r))))
    ((type-product-p ty)
     (format nil "(~{~A~^, ~})" (mapcar #'type-to-string (type-product-elems ty))))
    ((type-union-p ty)
     (format nil "(~{~A~^ | ~})" (mapcar #'type-to-string (type-union-types ty))))
    ((type-intersection-p ty)
     (format nil "(~{~A~^ & ~})" (mapcar #'type-to-string (type-intersection-types ty))))
    ((type-forall-p ty)
     (format nil "(forall ~A . ~A)"
             (type-to-string (type-forall-var ty))
             (type-to-string (type-forall-body ty))))
    ((type-effect-row-p ty)
     (let ((effs (type-effect-row-effects ty))
           (rv   (type-effect-row-row-var ty)))
       (if (and (null effs) (null rv))
           "{}"
           (format nil "{~{~A~^, ~}~A}"
                   (mapcar (lambda (e)
                              (if (type-effect-op-p e)
                                  (symbol-name (type-effect-op-name e))
                                  "#<eff>"))
                           effs)
                   (if rv (format nil " | ~A" (type-to-string rv)) "")))))
    ((type-effect-op-p ty)
     (symbol-name (type-effect-op-name ty)))
    ((type-error-p ty)
     (if (string= (type-error-message ty) "unknown") "?" (type-error-message ty)))
    ((type-app-p ty)
     (format nil "(~A ~A)"
             (type-to-string (type-app-fun ty))
             (type-to-string (type-app-arg ty))))
    ((type-constraint-p ty)
     (format nil "(~A ~A)"
             (type-constraint-class-name ty)
             (type-to-string (type-constraint-type-arg ty))))
    ((type-qualified-p ty)
     (format nil "(~{~A~^, ~} => ~A)"
             (mapcar #'type-to-string (type-qualified-constraints ty))
             (type-to-string (type-qualified-body ty))))
    (t (format nil "#<~A>" (type-of ty)))))
