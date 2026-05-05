;;;; types-core.lisp — Core Type Node Hierarchy
;;;;
;;;; Base defstruct type-node and all scalar/composite type constructors:
;;;; type-primitive, type-var, type-rigid, type-arrow, type-product,
;;;; type-record, type-variant, type-union, type-intersection,
;;;; type-forall, type-exists, type-app, type-lambda, type-mu,
;;;; type-refinement, type-linear, type-capability.

(in-package :cl-cc/type)

;;; ─── Base ─────────────────────────────────────────────────────────────────

(defstruct type-node
  "Base for all type representations.
SOURCE-LOCATION: optional source range for error messages.
KIND: the kind of this type (a kind-node); nil means not yet inferred."
  (source-location nil)
  (kind            nil))

;;; ─── type-primitive ───────────────────────────────────────────────────────

(defstruct (type-primitive (:include type-node))
  "A primitive ground type: fixnum, string, boolean, symbol, etc."
  (name nil :type symbol))

;;; ─── type-var ─────────────────────────────────────────────────────────────

(defvar *type-var-counter* 0)

(defstruct (type-var (:include type-node) (:constructor %make-type-var))
  "A unification variable — the mutable cell of constraint solving.
ID uniquely identifies this variable. NAME is a debug hint.
LINK: when non-nil, this variable has been unified with LINK (path compression).
UPPER-BOUND / LOWER-BOUND carry bounded-polymorphism constraints for the
variable itself: LOWER-BOUND <: this-var <: UPPER-BOUND."
  (id          0   :type fixnum)
  (name        nil)
  (link        nil)
  (upper-bound nil)
  (lower-bound nil))

(defun fresh-type-var (&optional name &key upper-bound lower-bound)
  "Return a fresh unification variable with a unique ID.
UPPER-BOUND and LOWER-BOUND are optional type-node bounds used by bounded
polymorphism."
  (incf *type-var-counter*)
  (%make-type-var :id *type-var-counter* :name name
                  :upper-bound upper-bound
                  :lower-bound lower-bound))

(defun reset-type-vars! ()
  "Reset the type variable counter — use only in tests."
  (setf *type-var-counter* 0))

(defun type-var-equal-p (v1 v2)
  "Two type-vars are equal iff they have the same ID."
  (and (type-var-p v1) (type-var-p v2) (= (type-var-id v1) (type-var-id v2))))

;;; ─── type-rigid ───────────────────────────────────────────────────────────

(defvar *rigid-var-counter* 0)

(defstruct (type-rigid (:include type-node) (:constructor %make-type-rigid))
  "A skolem / rigid variable — introduced by checking against ∀.
Cannot be unified with any other type. Scope-checked at generalisation."
  (id   0   :type fixnum)
  (name nil))

(defun fresh-rigid-var (&optional name)
  "Return a fresh rigid (skolem) variable."
  (incf *rigid-var-counter*)
  (%make-type-rigid :id *rigid-var-counter* :name name))

(defun type-rigid-equal-p (r1 r2)
  (and (type-rigid-p r1) (type-rigid-p r2) (= (type-rigid-id r1) (type-rigid-id r2))))

;;; ─── type-arrow ───────────────────────────────────────────────────────────

(defstruct (type-arrow (:include type-node)
                       (:constructor make-type-arrow-raw))
  "An arrow type A₁ … Aₙ -[ε,q]-> B.
PARAMS:   list of argument types.
RETURN:   return type.
EFFECTS:  a type-effect-row (default: pure, filled in after effect.lisp loads).
MULT:     multiplicity grade for the arrow (:zero :one :omega)."
  (params  nil  :type list)
  (return  nil)
  (effects nil)               ; type-effect-row or nil (filled by make-type-arrow)
  (mult    :omega))

(defun make-type-arrow (params return &key (effects nil) (mult :omega))
  "Convenience constructor. EFFECTS defaults to the pure (empty) effect row."
  (make-type-arrow-raw :params params :return return
                       :effects effects :mult (or mult :omega)))

;;; ─── type-product ─────────────────────────────────────────────────────────

(defstruct (type-product (:include type-node))
  "A tuple / product type (A, B, C, …)."
  (elems nil :type list))

;;; ─── type-record ──────────────────────────────────────────────────────────

(defstruct (type-record (:include type-node))
  "A row-polymorphic record type {l₁: T₁, …, lₙ: Tₙ | ρ}.
FIELDS:   alist of (label . type).
ROW-VAR:  an open row variable (type-var) or nil for closed records."
  (fields  nil :type list)
  (row-var nil))

;;; ─── type-variant ─────────────────────────────────────────────────────────

(defstruct (type-variant (:include type-node))
  "A row-polymorphic variant / sum type <L₁: T₁, …, Lₙ: Tₙ | ρ>.
CASES:    alist of (label . type).
ROW-VAR:  an open row variable or nil for closed variants."
  (cases   nil :type list)
  (row-var nil))

;;; ─── type-union ───────────────────────────────────────────────────────────

(defstruct (type-union (:include type-node)
                        (:constructor make-type-union-raw))
  "A union type A | B. TYPES is a list of alternatives (canonical, de-duplicated).
CONSTRUCTOR-NAME preserves sugar provenance for round-tripping forms like OPTION."
  (types nil :type list)
  (constructor-name nil :type (or symbol null)))

(defun make-type-union (types &key constructor-name)
  "Positional convenience constructor: (make-type-union '(type-a type-b))."
  (make-type-union-raw :types types :constructor-name constructor-name))

;;; ─── type-intersection ────────────────────────────────────────────────────

(defstruct (type-intersection (:include type-node)
                               (:constructor make-type-intersection-raw))
  "An intersection type A & B."
  (types nil :type list))

(defun make-type-intersection (types)
  "Positional convenience constructor: (make-type-intersection '(type-a type-b))."
  (make-type-intersection-raw :types types))

;;; ─── type-forall ──────────────────────────────────────────────────────────

(defstruct (type-forall (:include type-node)
                        (:constructor %make-type-forall-raw))
  "Universal quantification ∀(a : κ). T.
VAR:  a type-var (the bound variable — treated as rigid inside).
KIND: the declared kind of VAR (kind-node or nil for inferred).
BODY: the body type."
  (var  nil)
  (knd  nil)
  (body nil))

(defun make-type-forall (&key var knd body)
  "Constructor for type-forall using the canonical :body slot."
  (%make-type-forall-raw :var var :knd knd :body body))

;;; ─── type-exists ──────────────────────────────────────────────────────────

(defstruct (type-exists (:include type-node))
  "Existential quantification ∃(a : κ). T."
  (var  nil)
  (knd  nil)
  (body nil))

;;; ─── type-app ─────────────────────────────────────────────────────────────

(defstruct (type-app (:include type-node))
  "Higher-kinded type application: F A.
FUN: the type constructor (kind κ₁ -> κ₂).
ARG: the argument type (kind κ₁)."
  (fun nil)
  (arg nil))

;;; ─── type-lambda ──────────────────────────────────────────────────────────

(defstruct (type-lambda (:include type-node))
  "Type-level lambda λ(a : κ). T — a type constructor as a first-class value."
  (var  nil)
  (knd  nil)
  (body nil))

;;; ─── type-mu ──────────────────────────────────────────────────────────────

(defstruct (type-mu (:include type-node))
  "Iso-recursive type μ(a). T.  fold/unfold are explicit.
VAR:  the recursion variable (type-var).
BODY: the body (may contain VAR free)."
  (var  nil)
  (body nil))

;;; ─── type-refinement ──────────────────────────────────────────────────────

(defstruct (type-refinement (:include type-node))
  "Refinement type {x : T | φ(x)}.
BASE:      the base type T.
PREDICATE: a CL lambda (lambda (x) ...) encoding the invariant."
  (base      nil)
  (predicate nil))

;;; ─── type-linear ──────────────────────────────────────────────────────────

(defstruct (type-linear (:include type-node))
  "Graded modal type !_q T where q ∈ {:zero :one :omega}.
BASE:  the inner type T.
GRADE: a multiplicity keyword."
  (base  nil)
  (grade :omega))

;;; ─── type-capability ──────────────────────────────────────────────────────

(defstruct (type-capability (:include type-node))
  "A capability-tagged type T^{cap}.
BASE: the underlying type.
CAP:  the capability name (symbol)."
  (base nil)
  (cap  nil :type symbol))
