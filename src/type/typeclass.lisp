;;;; typeclass.lisp - Multi-Parameter Typeclass System with Dictionary Passing
;;;;
;;;; Implements:
;;;;   typeclass-def     — typeclass definition (name, params, superclasses, methods,
;;;;                       associated types, functional dependencies)
;;;;   typeclass-instance — instance definition (class, type, constraints, methods)
;;;;   *typeclass-registry* / *typeclass-instance-registry*
;;;;   dict-env          — alist mapping (class-name . type-key) -> method alist
;;;;
;;;; Backward-compat aliases for the old package API:
;;;;   type-class          — struct wrapping typeclass-def
;;;;   type-class-p        — predicate
;;;;   type-class-name     — accessor
;;;;   type-class-type-param  — first type parameter
;;;;   type-class-methods  — method alist
;;;;   type-class-superclasses — superclass list
;;;;
;;;; The old inference.lisp kept *typeclass-registry* and *typeclass-instance-registry*
;;;; together with the inference code.  This file provides the authoritative
;;;; definitions; inference.lisp will forward to them.

(in-package :cl-cc/type)

;;; ─── typeclass-def defstruct ──────────────────────────────────────────────

(defstruct typeclass-def
  "A multi-parameter typeclass definition.
NAME:             the typeclass symbol (e.g., 'EQ, 'NUM, 'FUNCTOR).
TYPE-PARAMS:      list of type-var nodes (the class's type parameters).
SUPERCLASSES:     list of typeclass name symbols that this class inherits from.
METHODS:          alist of (method-name . type-arrow) — the typeclass interface.
DEFAULTS:         alist of (method-name . implementation) used when instances
                  omit a method.
ASSOCIATED-TYPES: alist of (name . kind-node) — associated type families.
FUNCTIONAL-DEPS:  list of (from-params . to-params) — functional dependencies."
  (name             nil :type symbol)
  (type-params      nil :type list)
  (superclasses     nil :type list)
  (methods          nil :type list)
  (defaults         nil :type list)
  (associated-types nil :type list)
  (functional-deps  nil :type list))

;;; ─── *typeclass-registry* ─────────────────────────────────────────────────

(defvar *typeclass-registry*
  (make-hash-table :test #'eq)
  "Maps typeclass name symbol -> typeclass-def.")

(defun register-typeclass (name tc-def)
  "Register TC-DEF under NAME in *typeclass-registry*.
TC-DEF may be a typeclass-def or the old type-class struct — both are accepted."
  (setf (gethash name *typeclass-registry*) tc-def)
  name)

(defun %typeclass-instance-overlaps-p (type-a type-b)
  "Return T when TYPE-A and TYPE-B can unify, meaning the instances overlap.

This is a conservative coherence check: if two instances for the same class
could both match the same concrete type, we reject the later registration."
  (and (not (type-equal-p type-a type-b))
       (multiple-value-bind (_subst ok)
           (type-unify type-a type-b)
         (declare (ignore _subst))
         ok)))

(defun %typeclass-default-methods (class-name)
  "Return the default method alist for CLASS-NAME, or NIL."
  (let ((tc-def (lookup-typeclass class-name)))
    (cond
      ((typeclass-def-p tc-def) (typeclass-def-defaults tc-def))
      ((type-class-p tc-def)    (type-class-defaults tc-def))
      (t nil))))

(defun %merge-default-methods (class-name method-impls)
  "Merge class defaults into METHOD-IMPLS, preserving explicit implementations."
  (let ((defaults (%typeclass-default-methods class-name)))
    (append method-impls
            (remove-if (lambda (pair)
                         (assoc (car pair) method-impls :test #'eq))
                       defaults))))

(defun %typeclass-param-name (param)
  "Return a stable identifier for PARAM, which may be a type-var or symbol."
  (cond
    ((type-var-p param) (type-var-name param))
    ((symbolp param) param)
    (t param)))

(defun %typeclass-instance-args (type)
  "Return the ordered instance arguments encoded by TYPE.
Single-parameter instances use TYPE itself; multi-parameter instances use a type-product."
  (if (type-product-p type)
      (type-product-elems type)
      (list type)))

(defun %typeclass-instance-arg-at (type index)
  (nth index (%typeclass-instance-args type)))

(defun %typeclass-fundep-pairs (tc-def)
  "Normalize functional dependencies from TC-DEF into ((from...) . (to...)) pairs."
  (mapcar (lambda (dep)
            (cons (if (listp (car dep)) (car dep) (list (car dep)))
                  (if (listp (cdr dep)) (cdr dep) (list (cdr dep)))))
          (typeclass-def-functional-deps tc-def)))

(defun %typeclass-fundep-violation-p (tc-def existing-type new-type)
  "Return T when NEW-TYPE would violate TC-DEF functional dependencies
relative to EXISTING-TYPE. Uses positional matching over type params."
  (let* ((params (typeclass-def-type-params tc-def))
         (param-names (mapcar #'%typeclass-param-name params))
         (existing-args (%typeclass-instance-args existing-type))
         (new-args (%typeclass-instance-args new-type)))
    (dolist (dep (%typeclass-fundep-pairs tc-def) nil)
      (let* ((from-names (car dep))
             (to-names   (cdr dep))
             (from-indices (mapcar (lambda (name)
                                     (position name param-names :test #'equal))
                                   from-names))
             (to-indices   (mapcar (lambda (name)
                                     (position name param-names :test #'equal))
                                   to-names)))
        (when (and (every #'numberp from-indices)
                   (every #'numberp to-indices))
          (let ((existing-from (mapcar (lambda (i) (nth i existing-args)) from-indices))
                (new-from      (mapcar (lambda (i) (nth i new-args)) from-indices)))
            (when (every #'type-equal-p existing-from new-from)
              (let ((existing-to (mapcar (lambda (i) (nth i existing-args)) to-indices))
                    (new-to      (mapcar (lambda (i) (nth i new-args)) to-indices)))
                (unless (every #'type-equal-p existing-to new-to)
                  (return t))))))))))

(defun lookup-typeclass (name)
  "Return the typeclass-def (or type-class) for NAME, or nil if not registered."
  (gethash name *typeclass-registry*))

;;; ─── typeclass-instance defstruct ─────────────────────────────────────────

(defstruct (typeclass-instance (:constructor %make-typeclass-instance))
  "A typeclass instance declaration.
CLASS-NAME:   the typeclass symbol.
INSTANCE-TYPE: the type-node this instance is for.
CONSTRAINTS:  list of type-constraint (the instance's context / supers).
METHODS:      alist of (method-name . function) — the method implementations."
  (class-name    nil :type symbol)
  (instance-type nil)
  (constraints   nil :type list)
  (methods       nil :type list))

;;; ─── *typeclass-instance-registry* ───────────────────────────────────────

(defvar *typeclass-instance-registry*
  (make-hash-table :test #'equal)
  "Maps (class-name . type-key-string) to typeclass-instance.
The key uses EQUAL comparison so different type-nodes with the same string
representation each get their own bucket.")

(defparameter *default-numeric-type* type-int
  "Default concrete type for unresolved numeric constraints.")

(defparameter *default-numeric-class-names* '("NUM" "NUMERIC")
  "Typeclass names eligible for numeric defaulting.")

(defun %type-instance-key (class-name type)
  "Build the hash key for (CLASS-NAME, TYPE)."
  (cons class-name (type-to-string type)))

(defun register-typeclass-instance (class-name type method-impls)
  "Register that TYPE implements CLASS-NAME with METHOD-IMPLS.
METHOD-IMPLS is an alist of (method-name . implementation)."
  (when (lookup-typeclass-instance class-name type)
    (error 'type-inference-error
           :message (format nil "Duplicate typeclass instance for ~A / ~A"
                            class-name (type-to-string type))))
  (dolist (existing (loop for k being the hash-keys of *typeclass-instance-registry*
                          using (hash-value inst)
                          when (and (consp k) (eq (car k) class-name))
                          collect inst))
    (when (%typeclass-instance-overlaps-p (typeclass-instance-instance-type existing) type)
      (error 'type-inference-error
             :message (format nil
                              "Overlapping typeclass instances for ~A: ~A and ~A"
                              class-name
                              (type-to-string (typeclass-instance-instance-type existing))
                              (type-to-string type)))))
  (let ((tc-def (lookup-typeclass class-name)))
    (when (typeclass-def-p tc-def)
      (dolist (existing (loop for k being the hash-keys of *typeclass-instance-registry*
                              using (hash-value inst)
                              when (and (consp k) (eq (car k) class-name))
                              collect inst))
        (when (%typeclass-fundep-violation-p tc-def
                                             (typeclass-instance-instance-type existing)
                                             type)
          (error 'type-inference-error
                 :message (format nil
                                  "Functional dependency violation for ~A: ~A vs ~A"
                                  class-name
                                  (type-to-string (typeclass-instance-instance-type existing))
                                  (type-to-string type)))))))
  (let ((inst (%make-typeclass-instance
                :class-name    class-name
                :instance-type type
                :constraints   nil
                :methods       (%merge-default-methods class-name method-impls))))
    (setf (gethash (%type-instance-key class-name type)
                   *typeclass-instance-registry*)
          inst)
    inst))

(defun lookup-typeclass-instance (class-name type)
  "Return the typeclass-instance for (CLASS-NAME, TYPE), or nil."
  (gethash (%type-instance-key class-name type)
           *typeclass-instance-registry*))

(defun has-typeclass-instance-p (class-name type)
  "Return T if TYPE has a registered instance for CLASS-NAME,
including instances inherited via superclass relationships."
  (or (not (null (lookup-typeclass-instance class-name type)))
      ;; Check superclasses via the typeclass-def or type-class if available
      (let ((tc-def (lookup-typeclass class-name)))
        (when tc-def
          (let ((supers (cond
                          ((typeclass-def-p tc-def)
                           (typeclass-def-superclasses tc-def))
                          ((type-class-p tc-def)
                           (type-class-superclasses tc-def))
                          (t nil))))
             (some (lambda (super)
                     (has-typeclass-instance-p super type))
                   supers))))))

(defun default-numeric-typeclass-p (class-name)
  "Return T if CLASS-NAME should default unresolved numeric type variables."
  (and (symbolp class-name)
       (member (symbol-name class-name)
               *default-numeric-class-names*
               :test #'string-equal)))

(defun check-typeclass-constraint (class-name type env)
  "Check if TYPE satisfies CLASS-NAME.
ENV is accepted for interface compatibility.
Returns nil on success, signals type-inference-error if unsatisfied.
Gradual typing: unknown and free type-vars are always accepted."
  (declare (ignore env))
  (unless (or (type-error-p type)
              (type-var-p type)
              (has-typeclass-instance-p class-name type))
    (error 'type-inference-error
           :message (format nil "No instance of ~A for ~A"
                             class-name (type-to-string type)))))

;;; ─── dict-env operations ──────────────────────────────────────────────────
;;;
;;; A dict-env is an alist of ((class-name . type-key-string) . methods)
;;; stored inside the type-env's dict-bindings slot.

(defun dict-env-extend (class-name type methods env)
  "Return a new type-env that extends ENV with a method dictionary for
(CLASS-NAME, TYPE).  METHODS is an alist of (method-name . implementation)."
  (let ((key (cons class-name (type-to-string type))))
    (make-type-env
     :bindings      (type-env-bindings env)
     :dict-bindings (acons key methods (type-env-dict-bindings env)))))

(defun dict-env-lookup (class-name type env)
  "Look up the method alist for (CLASS-NAME, TYPE) in ENV's dict-bindings.
Returns the methods alist or nil if missing."
  (let ((key (cons class-name (type-to-string type))))
    (let ((entry (assoc key (type-env-dict-bindings env) :test #'equal)))
      (when entry (cdr entry)))))

;;; ─── Built-in default numeric instance ───────────────────────────────────

(eval-when (:load-toplevel :execute)
  (unless (lookup-typeclass 'num)
    (register-typeclass 'num (make-typeclass-def
                              :name 'num
                              :type-params nil
                              :methods nil)))
  (unless (lookup-typeclass-instance 'num type-int)
    (register-typeclass-instance 'num type-int nil)))

;;; ─── Backward-compat: type-class struct ──────────────────────────────────
;;;
;;; The old package defined a `type-class` defstruct with these slots:
;;;   name, type-param, superclasses, methods
;;;
;;; The tests do:
;;;   (make-type-class :name 'EQ-TEST
;;;                    :type-param (make-type-variable 'a)
;;;                    :methods (...))
;;;   (type-class-p tc)
;;;   (type-class-name tc)
;;;   (type-class-type-param tc)
;;;   (type-class-methods tc)
;;;   (type-class-superclasses tc)
;;;
;;; We re-implement the struct here so old code compiles unchanged.

(defstruct (type-class (:constructor make-type-class))
  "Backward-compat typeclass node (old API).
Wraps the information needed by existing tests and inference code.
For new code, prefer typeclass-def."
  (name         nil :type symbol)
  (type-param   nil)
  (superclasses nil :type list)
  (methods      nil :type list)
  (defaults     nil :type list))

;;; ─── Backward-compat: type-class-constraint ──────────────────────────────
;;;
;;; Old package exported type-class-constraint as a separate struct distinct
;;; from type-constraint.  Tests do:
;;;   (make-type-class-constraint :class-name 'NUM :type-arg a)
;;;   (type-class-constraint-p c)
;;;   (type-class-constraint-class-name c)
;;;   (type-class-constraint-type-arg c)
;;;
;;; representation.lisp already defines type-constraint with :class-name and
;;; :type-arg slots; but it's a type-node.  To keep old tests green, we
;;; define type-class-constraint as an alias struct.

(defstruct (type-class-constraint
            (:constructor make-type-class-constraint))
  "Backward-compat typeclass constraint node (old API).
Use type-constraint (from representation.lisp) for new code."
  (class-name nil :type symbol)
  (type-arg   nil))

;;; ─── Backward-compat: type-qualified-type alias ─────────────────────────
;;;
;;; representation.lisp uses `type-qualified-body`; old tests use
;;; `type-qualified-type`.  make-type-qualified is defined there and accepts
;;; both :body and :type keywords — no shadowing needed here.

(defun type-qualified-type (q)
  "Backward-compat: alias for type-qualified-body."
  (type-qualified-body q))

;;; ─── Backward-compat: type-effect struct ─────────────────────────────────
;;;
;;; Old package had `type-effect` with a single :name slot.
;;; representation.lisp has `type-effect-op` with :name and :args.
;;; Tests do:
;;;   (make-type-effect :name 'IO)
;;;   (type-effect-p e)
;;;   (type-effect-name e)
;;;
;;; We define a thin struct, then define a polymorphic type-effect-name
;;; that works on BOTH type-effect and type-effect-op.
;;; The defstruct auto-generates type-effect-name as a slot accessor;
;;; we shadow it afterward with a function that handles both types.

(defstruct (type-effect (:constructor make-type-effect)
                        ;; Suppress the auto-generated type-effect-name
                        ;; so we can define a more general version below.
                        (:conc-name %type-effect-))
  "Backward-compat effect label node (old API).
New code should use type-effect-op."
  (name nil :type symbol))

;;; Polymorphic accessor: works on type-effect (old) and type-effect-op (new).
(defun type-effect-name (e)
  "Return the effect name symbol for E, which may be a type-effect or type-effect-op."
  (cond
    ((type-effect-p e)    (%type-effect-name e))
    ((type-effect-op-p e) (type-effect-op-name e))
    (t (error "type-effect-name: not an effect node: ~S" e))))

;;; ─── Backward-compat: type-effectful-function struct ─────────────────────
;;;
;;; Old package had `type-effectful-function` with slots: params, return, effects.
;;; Tests do:
;;;   (make-type-effectful-function :params ... :return ... :effects ...)
;;;   (typep fn 'type-effectful-function)
;;;   (type-effectful-function-effects fn)
;;;   (type-function-params fn)
;;;   (type-function-return fn)
;;;
;;; We define it as a struct that also satisfies `type-function-p` via
;;; :include type-arrow, so that type-function-params / type-function-return
;;; work on it.

(defstruct (type-effectful-function
            (:include type-arrow)
            (:constructor make-type-effectful-function
                          (&key params return effects mult
                           &aux (params params) (return return)
                                (effects effects) (mult (or mult :omega)))))
  "A function type annotated with an explicit effect row.
Extends type-arrow; the :effects slot is from type-arrow itself.")

;;; The effects slot is inherited from type-arrow; its accessor type-arrow-effects
;;; is also accessible as type-effectful-function-effects via the :include
;;; conc-name expansion generated by defstruct above.

;;; ─── Backward-compat: type-forall-type alias ────────────────────────────
;;;
;;; representation.lisp uses `type-forall-body`; old tests use `type-forall-type`.
;;; make-type-forall is defined there and accepts both :body and :type keywords.

(defun type-forall-type (fa)
  "Backward-compat: alias for type-forall-body."
  (type-forall-body fa))

;;; ─── Backward-compat: type-skolem ────────────────────────────────────────
;;;
;;; The old code had a `type-skolem` distinct from `type-rigid`.
;;; The new representation uses type-rigid for skolems.
;;; We build a thin compatibility layer so `make-type-skolem`,
;;; `type-skolem-p`, `type-skolem-id`, `type-skolem-name`, and
;;; `type-skolem-equal-p` all work.

(defvar *skolem-counter* 0
  "Counter for unique skolem IDs.")

(defstruct (type-skolem (:constructor %make-type-skolem))
  "Backward-compat skolem / rigid variable (old API).
Maps to type-rigid semantics."
  (id   0   :type fixnum)
  (name nil))

(defun make-type-skolem (&optional (name "a"))
  "Create a fresh skolem constant with NAME."
  (incf *skolem-counter*)
  (%make-type-skolem :id *skolem-counter* :name name))

(defun type-skolem-equal-p (s1 s2)
  "True iff S1 and S2 are the same skolem (by ID)."
  (and (type-skolem-p s1) (type-skolem-p s2)
       (= (type-skolem-id s1) (type-skolem-id s2))))

;;; ─── Extend type-to-string for backward-compat types ─────────────────────
;;;
;;; representation.lisp's type-to-string does not know about type-class-constraint
;;; or type-skolem (defined here, after representation.lisp).  We shadow
;;; type-to-string here to add those cases, delegating to the prior definition
;;; for all other node types.

(let ((prior-type-to-string (if (fboundp 'type-to-string)
                               #'type-to-string
                               nil)))
  (defun type-to-string (ty)
    "Convert a type to a human-readable string.
Handles all type-nodes including backward-compat types (type-class-constraint,
type-skolem, type-effect) in addition to the representation.lisp core nodes."
    (typecase ty
      (type-class-constraint
       (format nil "(~A ~A)"
               (type-class-constraint-class-name ty)
               (if prior-type-to-string
                   (funcall prior-type-to-string
                            (type-class-constraint-type-arg ty))
                   (format nil "~A" (type-class-constraint-type-arg ty)))))
      (type-skolem  (format nil "!sk~D" (type-skolem-id ty)))
      (type-effect  (symbol-name (%type-effect-name ty)))
      (t            (if prior-type-to-string
                        (funcall prior-type-to-string ty)
                        (format nil "~A" ty))))))
