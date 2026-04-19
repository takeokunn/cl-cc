(in-package :cl-cc/type)

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

(defun %option-type-constructor-p (x)
  (and (type-union-p x)
       (= (length (type-union-types x)) 2)
       (some (lambda (ty) (type-equal-p ty type-null))
              (type-union-types x))))

(defun type-constructor-p (x)
  (or (type-app-p x)
      (%option-type-constructor-p x)))

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
(deftype type-constructor ()
  '(or type-app (and type-union (satisfies %option-type-constructor-p))))

;;; ─── type-to-string forward declaration ──────────────────────────────────
;;; The real implementation is in parser.lisp / printer.lisp (loaded later).
;;; We define a stub here so other type module files can call it early;
;;; the stub is overridden once the printer loads.

(defun type-to-string (ty)
  "Convert a type to a human-readable string.
This stub is overridden by the printer module when loaded."
  (typecase ty
    (null "NIL")
    (type-primitive (symbol-name (type-primitive-name ty)))
    (type-var
     (if (type-var-name ty)
         (format nil "?~A" (type-var-name ty))
         (format nil "?t~D" (type-var-id ty))))
    (type-rigid
     (if (type-rigid-name ty)
         (format nil "!~A" (type-rigid-name ty))
         (format nil "!r~D" (type-rigid-id ty))))
    (type-arrow
     (let ((ps (mapcar #'type-to-string (type-arrow-params ty)))
           (r  (type-to-string (type-arrow-return ty))))
       (if (null ps)
           (format nil "() -> ~A" r)
           (format nil "~{~A~^ ~} -> ~A" ps r))))
    (type-product
     (format nil "(~{~A~^, ~})" (mapcar #'type-to-string (type-product-elems ty))))
    (type-union
     (format nil "(~{~A~^ | ~})" (mapcar #'type-to-string (type-union-types ty))))
    (type-intersection
     (format nil "(~{~A~^ & ~})" (mapcar #'type-to-string (type-intersection-types ty))))
    (type-forall
     (format nil "(forall ~A . ~A)"
             (type-to-string (type-forall-var ty))
             (type-to-string (type-forall-body ty))))
    (type-effect-row
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
    (type-effect-op
     (symbol-name (type-effect-op-name ty)))
    (type-error
     (if (string= (type-error-message ty) "unknown") "?" (type-error-message ty)))
    (type-app
     (format nil "(~A ~A)"
             (type-to-string (type-app-fun ty))
             (type-to-string (type-app-arg ty))))
    (type-constraint
     (format nil "(~A ~A)"
             (type-constraint-class-name ty)
             (type-to-string (type-constraint-type-arg ty))))
    (type-qualified
     (format nil "(~{~A~^, ~} => ~A)"
             (mapcar #'type-to-string (type-qualified-constraints ty))
             (type-to-string (type-qualified-body ty))))
    (t (format nil "#<~A>" (type-of ty)))))
