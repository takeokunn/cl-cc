;;;; types-extended.lisp — Effect, Constraint, and Extended Type Nodes
;;;;
;;;; Effect rows, handler, GADT, constraint, qualified, error, primitive
;;;; singletons, type-equal-p, and type-free-vars.
;;;; Loads after types-core.lisp.

(in-package :cl-cc/type)

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
  "Structural equality of two types (no substitution applied).
Type-error nodes are never considered equal (each represents a distinct error point),
even if they happen to be the same object."
  (cond
    ;; error — always distinct, checked before the (eq t1 t2) shortcut
    ((or (type-error-p t1) (type-error-p t2)) nil)
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
