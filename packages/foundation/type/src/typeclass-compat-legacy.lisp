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

;;; Accessor aliases and skolem/effect compatibility helpers moved to
;;; typeclass-compat-legacy-aliases.lisp.
