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

(defun %type-list-equal-p (ls1 ls2)
  "True iff LS1 and LS2 have equal length and pairwise type-equal-p elements."
  (and (= (length ls1) (length ls2))
       (every #'type-equal-p ls1 ls2)))

(defun type-equal-p (t1 t2)
  "Structural equality of two types (no substitution applied).
Type-error nodes are never considered equal (each represents a distinct error point),
even if they happen to be the same object."
  (cond
    ((or (type-error-p t1) (type-error-p t2)) nil)   ; errors always distinct
    ((eq t1 t2) t)
    ((and (type-primitive-p t1) (type-primitive-p t2))
     (eq (type-primitive-name t1) (type-primitive-name t2)))
    ((and (type-var-p t1)   (type-var-p t2))   (type-var-equal-p t1 t2))
    ((and (type-rigid-p t1) (type-rigid-p t2)) (type-rigid-equal-p t1 t2))
    ((and (type-arrow-p t1) (type-arrow-p t2))
     (and (%type-list-equal-p (type-arrow-params t1) (type-arrow-params t2))
          (type-equal-p (type-arrow-return t1) (type-arrow-return t2))
          (eq (type-arrow-mult t1) (type-arrow-mult t2))))
    ((and (type-product-p t1)      (type-product-p t2))
     (%type-list-equal-p (type-product-elems t1) (type-product-elems t2)))
    ((and (type-union-p t1)        (type-union-p t2))
     (%type-list-equal-p (type-union-types t1) (type-union-types t2)))
    ((and (type-intersection-p t1) (type-intersection-p t2))
     (%type-list-equal-p (type-intersection-types t1) (type-intersection-types t2)))
    ((and (type-forall-p t1) (type-forall-p t2))
     (and (type-var-equal-p (type-forall-var t1) (type-forall-var t2))
          (type-equal-p (type-forall-body t1) (type-forall-body t2))))
    ((and (type-exists-p t1) (type-exists-p t2))
     (and (type-var-equal-p (type-exists-var t1) (type-exists-var t2))
          (type-equal-p (type-exists-body t1) (type-exists-body t2))))
    ((and (type-app-p t1) (type-app-p t2))
     (and (type-equal-p (type-app-fun t1) (type-app-fun t2))
          (type-equal-p (type-app-arg t1) (type-app-arg t2))))
    ((and (type-mu-p t1) (type-mu-p t2))
     (and (type-var-equal-p (type-mu-var t1) (type-mu-var t2))
          (type-equal-p (type-mu-body t1) (type-mu-body t2))))
    ((and (type-linear-p t1) (type-linear-p t2))
     (and (eq (type-linear-grade t1) (type-linear-grade t2))
          (type-equal-p (type-linear-base t1) (type-linear-base t2))))
    ((and (type-refinement-p t1) (type-refinement-p t2))
     (and (type-equal-p (type-refinement-base t1) (type-refinement-base t2))
          (equal (type-refinement-predicate t1) (type-refinement-predicate t2))))
    ((and (type-effect-row-p t1) (type-effect-row-p t2))
     (and (%type-list-equal-p (type-effect-row-effects t1) (type-effect-row-effects t2))
          (let ((rv1 (type-effect-row-row-var t1))
                (rv2 (type-effect-row-row-var t2)))
            (if (and rv1 rv2) (type-equal-p rv1 rv2) (eq rv1 rv2)))))
    ((and (type-effect-op-p t1) (type-effect-op-p t2))
     (and (eq (type-effect-op-name t1) (type-effect-op-name t2))
          (%type-list-equal-p (type-effect-op-args t1) (type-effect-op-args t2))))
    ((and (type-constraint-p t1) (type-constraint-p t2))
     (and (eq (type-constraint-class-name t1) (type-constraint-class-name t2))
          (type-equal-p (type-constraint-type-arg t1) (type-constraint-type-arg t2))))
    ((and (type-qualified-p t1) (type-qualified-p t2))
     (and (%type-list-equal-p (type-qualified-constraints t1) (type-qualified-constraints t2))
          (type-equal-p (type-qualified-body t1) (type-qualified-body t2))))
    (t nil)))

;;; ─── Structural traversal helpers ──────────────────────────────────────────

(defun type-bound-var (ty)
  "Return the type variable bound by TY, or NIL when TY is not a binder.
This is the shared data-layer hook used by traversal code like type-free-vars."
  (typecase ty
    (type-forall (type-forall-var ty))
    (type-exists (type-exists-var ty))
    (type-lambda (type-lambda-var ty))
    (type-mu     (type-mu-var ty))
    (t nil)))

(defun type-children (ty)
  "Return TY's immediate child types as a flat list.
The function is intentionally structural and side-effect free so traversal logic
can live elsewhere (free vars, occurs check, zonking, printers, etc.)."
  (cond
    ((or (null ty)
         (type-primitive-p ty)
         (type-var-p ty)
         (type-rigid-p ty)
         (type-error-p ty)
         (type-unknown-p ty)
         (and (fboundp 'type-skolem-p) (type-skolem-p ty))
         (and (fboundp 'type-effect-p) (type-effect-p ty))
         (and (fboundp 'type-class-p) (type-class-p ty)))
     nil)
    ((type-arrow-p ty)
     (append (type-arrow-params ty)
             (list (type-arrow-return ty))
             (when (type-arrow-effects ty)
               (list (type-arrow-effects ty)))))
    ((type-product-p ty)
     (copy-list (type-product-elems ty)))
    ((type-record-p ty)
     (append (mapcar #'cdr (type-record-fields ty))
             (when (type-record-row-var ty)
               (list (type-record-row-var ty)))))
    ((type-variant-p ty)
     (append (mapcar #'cdr (type-variant-cases ty))
             (when (type-variant-row-var ty)
               (list (type-variant-row-var ty)))))
    ((type-union-p ty)
     (copy-list (type-union-types ty)))
    ((type-intersection-p ty)
     (copy-list (type-intersection-types ty)))
    ((type-forall-p ty)
     (list (type-forall-body ty)))
    ((type-exists-p ty)
     (list (type-exists-body ty)))
    ((type-app-p ty)
     (list (type-app-fun ty) (type-app-arg ty)))
    ((type-lambda-p ty)
     (list (type-lambda-body ty)))
    ((type-mu-p ty)
     (list (type-mu-body ty)))
    ((type-refinement-p ty)
     (list (type-refinement-base ty)))
    ((type-linear-p ty)
     (list (type-linear-base ty)))
    ((type-capability-p ty)
     (list (type-capability-base ty)))
    ((type-effect-row-p ty)
     (append (copy-list (type-effect-row-effects ty))
             (when (type-effect-row-row-var ty)
               (list (type-effect-row-row-var ty)))))
    ((type-effect-op-p ty)
     (copy-list (type-effect-op-args ty)))
    ((type-handler-p ty)
     (list (type-handler-effect ty)
           (type-handler-input ty)
           (type-handler-output ty)))
    ((type-gadt-con-p ty)
     (append (copy-list (type-gadt-con-arg-types ty))
             (when (type-gadt-con-index-type ty)
               (list (type-gadt-con-index-type ty)))))
    ((type-constraint-p ty)
     (list (type-constraint-type-arg ty)))
    ((and (fboundp 'type-class-constraint-p) (type-class-constraint-p ty))
     (list (type-class-constraint-type-arg ty)))
    ((type-qualified-p ty)
     (append (copy-list (type-qualified-constraints ty))
             (list (type-qualified-body ty))))
    (t nil)))

;;; ─── Free type variables ──────────────────────────────────────────────────

(defun type-free-vars (ty)
  "Return the list of free type-var nodes in TY (deduplicated by ID)."
  (labels ((rec (t0)
             (cond
               ((type-var-p t0) (list t0))
               (t
                (let* ((bound-var (type-bound-var t0))
                       (child-vars (mapcan #'rec (type-children t0))))
                  (if bound-var
                      (remove-if (lambda (v) (type-var-equal-p v bound-var)) child-vars)
                      child-vars))))))
    (remove-duplicates (rec ty) :test #'type-var-equal-p)))
