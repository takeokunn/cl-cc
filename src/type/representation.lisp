;;;; representation.lisp - Type Node Hierarchy
;;;;
;;;; All type-node defstructs for the 2026 type system.
;;;; Uses :include hierarchy — every node IS-A type-node.
;;;;
;;;; type-node (base)
;;;;   type-primitive      ; fixnum, string, bool, etc.
;;;;   type-var            ; unification variables (mutable link cell)
;;;;   type-rigid          ; skolem / rigid variables (cannot be unified)
;;;;   type-arrow          ; A -[ε,q]-> B  (effect row + multiplicity)
;;;;   type-product        ; (A, B, C)     tuples
;;;;   type-record         ; {x: A, y: B | ρ}
;;;;   type-variant        ; <X: A, Y: B | ρ>
;;;;   type-union          ; A | B
;;;;   type-intersection   ; A & B
;;;;   type-forall         ; ∀(a : κ). T
;;;;   type-exists         ; ∃(a : κ). T
;;;;   type-app            ; F A  (HKT application)
;;;;   type-lambda         ; λ(a : κ). T
;;;;   type-mu             ; μα. T  (iso-recursive)
;;;;   type-refinement     ; {x : T | φ(x)}
;;;;   type-linear         ; !_q T  (graded modal)
;;;;   type-capability     ; T^{cap}
;;;;   type-effect-row     ; <ε₁, ε₂ | ρ>
;;;;   type-effect-op      ; single effect label, possibly applied: (State Int)
;;;;   type-handler        ; effect handler type
;;;;   type-gadt-con       ; GADT constructor with index constraint
;;;;   type-constraint     ; typeclass constraint (C T)
;;;;   type-qualified      ; (C₁ a, C₂ b) => T
;;;;   type-error          ; error sentinel for recovery

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
LINK: when non-nil, this variable has been unified with LINK (path compression)."
  (id   0   :type fixnum)
  (name nil)
  (link nil))

(defun fresh-type-var (&optional name)
  "Return a fresh unification variable with a unique ID."
  (incf *type-var-counter*)
  (%make-type-var :id *type-var-counter* :name name))

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
  "A union type A | B. TYPES is a list of alternatives (canonical, de-duplicated)."
  (types nil :type list))

(defun make-type-union (types)
  "Positional convenience constructor: (make-type-union '(type-a type-b))."
  (make-type-union-raw :types types))

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

(defun make-type-forall (&key var knd body type)
  "Constructor for type-forall. Accepts :body (canonical) or :type (alias for :body)."
  (%make-type-forall-raw :var var :knd knd :body (or body type)))

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

;;; ─── type-effect-row ──────────────────────────────────────────────────────

(defstruct (type-effect-row (:include type-node))
  "An effect row <ε₁, …, εₙ | ρ>.
EFFECTS: list of type-effect-op (concrete effect labels/applications).
ROW-VAR: an open row variable (type-var) or nil for closed rows."
  (effects nil :type list)
  (row-var nil))

(defvar +pure-effect-row+ (make-type-effect-row :effects nil :row-var nil)
  "The empty (pure) closed effect row.")

;;; ─── type-effect-op ───────────────────────────────────────────────────────

(defstruct (type-effect-op (:include type-node))
  "A single (possibly applied) effect label.
NAME: the effect name symbol (e.g., 'IO, 'STATE).
ARGS: type arguments (e.g., (list type-int) for (State Int))."
  (name nil :type symbol)
  (args nil :type list))

;;; ─── type-handler ─────────────────────────────────────────────────────────

(defstruct (type-handler (:include type-node))
  "The type of an effect handler.
EFFECT:   the effect being handled (type-effect-op).
INPUT:    the type of the handled computation's result.
OUTPUT:   the overall return type after handling."
  (effect nil)
  (input  nil)
  (output nil))

;;; ─── type-gadt-con ────────────────────────────────────────────────────────

(defstruct (type-gadt-con (:include type-node))
  "A GADT constructor type — carries its index constraint.
NAME:       the constructor name symbol.
ARG-TYPES:  list of argument types.
INDEX-TYPE: the index type (the return type's type parameter)."
  (name       nil :type symbol)
  (arg-types  nil :type list)
  (index-type nil))

;;; ─── type-constraint ──────────────────────────────────────────────────────

(defstruct (type-constraint (:include type-node))
  "A typeclass constraint C T — the constraint that T has an instance of class C.
CLASS-NAME: the typeclass symbol (e.g., 'EQ, 'NUM, 'SHOW).
TYPE-ARG:   the constrained type (often a type-var)."
  (class-name nil :type symbol)
  (type-arg   nil))

;;; ─── type-qualified ───────────────────────────────────────────────────────

(defstruct (type-qualified (:include type-node)
                           (:constructor %make-type-qualified-raw))
  "A type qualified by typeclass constraints: (C₁ a, C₂ b) => T.
CONSTRAINTS: list of type-constraint.
BODY:        the underlying type."
  (constraints nil :type list)
  (body        nil))

(defun make-type-qualified (&key constraints body type)
  "Constructor for type-qualified. Accepts :body (canonical) or :type (alias for :body)."
  (%make-type-qualified-raw :constraints constraints :body (or body type)))

;;; ─── type-error ───────────────────────────────────────────────────────────

(defstruct (type-error (:include type-node))
  "Error sentinel produced during type inference for error recovery.
MESSAGE: a human-readable description of the type error."
  (message "" :type string))

;;; ─── Well-known primitive singletons ──────────────────────────────────────

(defvar type-int     (make-type-primitive :name 'fixnum)  "fixnum type.")
(defvar type-float   (make-type-primitive :name 'float)   "float type.")
(defvar type-string  (make-type-primitive :name 'string)  "string type.")
(defvar type-bool    (make-type-primitive :name 'boolean) "boolean type.")
(defvar type-symbol  (make-type-primitive :name 'symbol)  "symbol type.")
(defvar type-cons    (make-type-primitive :name 'cons)    "cons type.")
(defvar type-null    (make-type-primitive :name 'null)    "null/nil type.")
(defvar type-any     (make-type-primitive :name 't)       "top type (any).")
(defvar type-char    (make-type-primitive :name 'character) "character type.")
(defvar type-unit    (make-type-primitive :name 'null)    "unit type (alias null).")

;;; ─── Effect row singletons ────────────────────────────────────────────────

(defvar +io-effect-row+
  (make-type-effect-row
   :effects (list (make-type-effect-op :name 'io))
   :row-var nil)
  "Closed IO effect row.")

;;; ─── Type equality (structural) ───────────────────────────────────────────

(defun type-equal-p (t1 t2)
  "Structural equality of two types (no substitution applied)."
  (cond
    ((eq t1 t2) t)
    ;; primitives
    ((and (type-primitive-p t1) (type-primitive-p t2))
     (eq (type-primitive-name t1) (type-primitive-name t2)))
    ;; type-var — by ID
    ((and (type-var-p t1) (type-var-p t2))
     (type-var-equal-p t1 t2))
    ;; rigid — by ID
    ((and (type-rigid-p t1) (type-rigid-p t2))
     (type-rigid-equal-p t1 t2))
    ;; arrow
    ((and (type-arrow-p t1) (type-arrow-p t2))
     (and (= (length (type-arrow-params t1)) (length (type-arrow-params t2)))
          (every #'type-equal-p (type-arrow-params t1) (type-arrow-params t2))
          (type-equal-p (type-arrow-return t1) (type-arrow-return t2))
          (eq (type-arrow-mult t1) (type-arrow-mult t2))))
    ;; product
    ((and (type-product-p t1) (type-product-p t2))
     (and (= (length (type-product-elems t1)) (length (type-product-elems t2)))
          (every #'type-equal-p (type-product-elems t1) (type-product-elems t2))))
    ;; union
    ((and (type-union-p t1) (type-union-p t2))
     (and (= (length (type-union-types t1)) (length (type-union-types t2)))
          (every #'type-equal-p (type-union-types t1) (type-union-types t2))))
    ;; intersection
    ((and (type-intersection-p t1) (type-intersection-p t2))
     (and (= (length (type-intersection-types t1)) (length (type-intersection-types t2)))
          (every #'type-equal-p (type-intersection-types t1) (type-intersection-types t2))))
    ;; forall
    ((and (type-forall-p t1) (type-forall-p t2))
     (and (type-var-equal-p (type-forall-var t1) (type-forall-var t2))
          (type-equal-p (type-forall-body t1) (type-forall-body t2))))
    ;; exists
    ((and (type-exists-p t1) (type-exists-p t2))
     (and (type-var-equal-p (type-exists-var t1) (type-exists-var t2))
          (type-equal-p (type-exists-body t1) (type-exists-body t2))))
    ;; type application
    ((and (type-app-p t1) (type-app-p t2))
     (and (type-equal-p (type-app-fun t1) (type-app-fun t2))
          (type-equal-p (type-app-arg t1) (type-app-arg t2))))
    ;; mu
    ((and (type-mu-p t1) (type-mu-p t2))
     (and (type-var-equal-p (type-mu-var t1) (type-mu-var t2))
          (type-equal-p (type-mu-body t1) (type-mu-body t2))))
    ;; linear
    ((and (type-linear-p t1) (type-linear-p t2))
     (and (eq (type-linear-grade t1) (type-linear-grade t2))
          (type-equal-p (type-linear-base t1) (type-linear-base t2))))
    ;; effect-row
    ((and (type-effect-row-p t1) (type-effect-row-p t2))
     (and (= (length (type-effect-row-effects t1))
             (length (type-effect-row-effects t2)))
          (every #'type-equal-p
                 (type-effect-row-effects t1)
                 (type-effect-row-effects t2))
          (let ((rv1 (type-effect-row-row-var t1))
                (rv2 (type-effect-row-row-var t2)))
            (if (and rv1 rv2)
                (type-equal-p rv1 rv2)
                (eq rv1 rv2)))))
    ;; effect-op
    ((and (type-effect-op-p t1) (type-effect-op-p t2))
     (and (eq (type-effect-op-name t1) (type-effect-op-name t2))
          (= (length (type-effect-op-args t1)) (length (type-effect-op-args t2)))
          (every #'type-equal-p (type-effect-op-args t1) (type-effect-op-args t2))))
    ;; constraint
    ((and (type-constraint-p t1) (type-constraint-p t2))
     (and (eq (type-constraint-class-name t1) (type-constraint-class-name t2))
          (type-equal-p (type-constraint-type-arg t1) (type-constraint-type-arg t2))))
    ;; qualified
    ((and (type-qualified-p t1) (type-qualified-p t2))
     (and (= (length (type-qualified-constraints t1))
             (length (type-qualified-constraints t2)))
          (every #'type-equal-p
                 (type-qualified-constraints t1)
                 (type-qualified-constraints t2))
          (type-equal-p (type-qualified-body t1) (type-qualified-body t2))))
    ;; error — always distinct (no two errors are equal)
    ((and (type-error-p t1) (type-error-p t2)) nil)
    (t nil)))

;;; ─── Free type variables ──────────────────────────────────────────────────

(defun type-free-vars (ty)
  "Return the list of free type-var nodes in TY (deduplicated by ID)."
  (labels ((rec (t0)
             (cond
               ((type-var-p t0)   (list t0))
               ((type-arrow-p t0)
                (append (mapcan #'rec (type-arrow-params t0))
                        (rec (type-arrow-return t0))
                        (when (type-arrow-effects t0)
                          (rec (type-arrow-effects t0)))))
               ((type-product-p t0)     (mapcan #'rec (type-product-elems t0)))
               ((type-union-p t0)       (mapcan #'rec (type-union-types t0)))
               ((type-intersection-p t0)(mapcan #'rec (type-intersection-types t0)))
               ((type-record-p t0)
                (append (mapcan (lambda (f) (rec (cdr f))) (type-record-fields t0))
                        (when (type-record-row-var t0) (rec (type-record-row-var t0)))))
               ((type-variant-p t0)
                (append (mapcan (lambda (c) (rec (cdr c))) (type-variant-cases t0))
                        (when (type-variant-row-var t0) (rec (type-variant-row-var t0)))))
               ((type-forall-p t0)
                (remove-if (lambda (v) (type-var-equal-p v (type-forall-var t0)))
                           (rec (type-forall-body t0))))
               ((type-exists-p t0)
                (remove-if (lambda (v) (type-var-equal-p v (type-exists-var t0)))
                           (rec (type-exists-body t0))))
               ((type-app-p t0)
                (append (rec (type-app-fun t0)) (rec (type-app-arg t0))))
               ((type-lambda-p t0)
                (remove-if (lambda (v) (type-var-equal-p v (type-lambda-var t0)))
                           (rec (type-lambda-body t0))))
               ((type-mu-p t0)
                (remove-if (lambda (v) (type-var-equal-p v (type-mu-var t0)))
                           (rec (type-mu-body t0))))
               ((type-refinement-p t0)  (rec (type-refinement-base t0)))
               ((type-linear-p t0)      (rec (type-linear-base t0)))
               ((type-capability-p t0)  (rec (type-capability-base t0)))
               ((type-effect-row-p t0)
                (append (mapcan #'rec (type-effect-row-effects t0))
                        (when (type-effect-row-row-var t0)
                          (rec (type-effect-row-row-var t0)))))
               ((type-effect-op-p t0)   (mapcan #'rec (type-effect-op-args t0)))
               ((type-handler-p t0)
                (append (rec (type-handler-effect t0))
                        (rec (type-handler-input t0))
                        (rec (type-handler-output t0))))
               ((type-constraint-p t0)  (rec (type-constraint-type-arg t0)))
               ((type-qualified-p t0)
                (append (mapcan #'rec (type-qualified-constraints t0))
                        (rec (type-qualified-body t0))))
               (t nil))))
    (remove-duplicates (rec ty) :test #'type-var-equal-p)))

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

(defun reset-type-vars!-alias ()
  (reset-type-vars!))
