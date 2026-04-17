;;;; typeclass-compat-legacy.lisp — Backward-compat type structs
;;;
;;; Extracted from typeclass-compat.lisp.
;;; Contains backward-compatibility shims for old typeclass API:
;;;   type-class, type-class-constraint, type-qualified-type,
;;;   type-effect, type-effectful-function, type-forall-type,
;;;   type-skolem, and extended type-to-string.
;;;
;;; Depends on typeclass-compat.lisp and representation.lisp.
;;; Load order: immediately after typeclass-compat.lisp.

(in-package :cl-cc/type)

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
