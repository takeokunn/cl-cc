;;;; constraint.lisp - OutsideIn(X) Constraint Language
;;;;
;;;; The constraint language supports:
;;;;   (:equal τ₁ τ₂)                 — type equality
;;;;   (:subtype τ₁ τ₂)               — subtyping
;;;;   (:typeclass C τ)               — typeclass membership
;;;;   (:implication vars given wanted) — ∀vars. given ⊃ wanted (GADT)
;;;;   (:effect-subset ε₁ ε₂)         — effect row inclusion
;;;;   (:mult-leq q₁ q₂)              — multiplicity ordering
;;;;   (:row-lacks ρ label)            — row restriction
;;;;   (:kind-equal κ₁ κ₂)            — kind equality
;;;;
;;;; Public API:
;;;;   constraint defstruct           — (kind . args)
;;;;   make-equal-constraint          — (:equal t1 t2)
;;;;   make-subtype-constraint        — (:subtype t1 t2)
;;;;   make-typeclass-constraint      — (:typeclass class-name type)
;;;;   make-implication-constraint    — (:implication vars given wanted)
;;;;   make-effect-subset-constraint  — (:effect-subset e1 e2)
;;;;   make-mult-leq-constraint       — (:mult-leq q1 q2)
;;;;   make-row-lacks-constraint      — (:row-lacks rho label)
;;;;   make-kind-equal-constraint     — (:kind-equal k1 k2)
;;;;   constraint-free-vars           — free type-vars in a constraint
;;;;   constraint-substitute          — apply substitution to a constraint

(in-package :cl-cc/type)

;;; ─── Constraint struct ────────────────────────────────────────────────────

(defstruct (constraint (:constructor %make-constraint))
  "A single type-system constraint.
KIND: a keyword identifying the constraint form (one of the forms above).
ARGS: a list of arguments appropriate to the constraint kind."
  (kind nil :type (or keyword null))
  (args nil :type list))

;;; ─── Smart constructors ───────────────────────────────────────────────────

(defun make-equal-constraint (t1 t2)
  "Build an equality constraint τ₁ ~ τ₂."
  (%make-constraint :kind :equal :args (list t1 t2)))

(defun make-subtype-constraint (t1 t2)
  "Build a subtyping constraint τ₁ ≤ τ₂."
  (%make-constraint :kind :subtype :args (list t1 t2)))

(defun make-typeclass-constraint (class-name type)
  "Build a typeclass membership constraint (C τ)."
  (%make-constraint :kind :typeclass :args (list class-name type)))

(defun make-implication-constraint (vars given wanted)
  "Build an implication constraint ∀vars. given ⊃ wanted.
VARS:   list of type-var (universally quantified).
GIVEN:  list of constraint (the hypotheses).
WANTED: list of constraint (the goals to prove under the hypotheses)."
  (%make-constraint :kind :implication :args (list vars given wanted)))

(defun make-effect-subset-constraint (e1 e2)
  "Build an effect-row inclusion constraint ε₁ ⊆ ε₂."
  (%make-constraint :kind :effect-subset :args (list e1 e2)))

(defun make-mult-leq-constraint (q1 q2)
  "Build a multiplicity ordering constraint q₁ ≤ q₂."
  (%make-constraint :kind :mult-leq :args (list q1 q2)))

(defun make-row-lacks-constraint (rho label)
  "Build a row-lacks constraint: row RHO does not contain LABEL."
  (%make-constraint :kind :row-lacks :args (list rho label)))

(defun make-kind-equal-constraint (k1 k2)
  "Build a kind equality constraint κ₁ ~ κ₂."
  (%make-constraint :kind :kind-equal :args (list k1 k2)))

;;; ─── Binary constraint dispatch table ────────────────────────────────────
;;;
;;; :equal, :subtype, and :effect-subset all have two type arguments and share
;;; the same free-variable and substitution logic.  A Prolog-style fact table
;;; dispatches these uniformly; only the structurally distinct kinds are cased.

(defparameter *binary-constraint-constructors*
  (list (cons :equal          #'make-equal-constraint)
        (cons :subtype        #'make-subtype-constraint)
        (cons :effect-subset  #'make-effect-subset-constraint))
  "Prolog-style fact table: constraint kind → two-type-arg smart constructor.")

(defun %two-type-free-vars (args)
  "Free vars in a constraint whose two arguments are both type nodes."
  (remove-duplicates
   (append (type-free-vars (first args))
           (type-free-vars (second args)))
   :test #'type-var-equal-p))

;;; ─── Free variables ───────────────────────────────────────────────────────

(defun constraint-free-vars (c)
  "Return a deduplicated list of type-var nodes free in constraint C."
  (let ((kind (constraint-kind c))
        (args (constraint-args c)))
    (if (assoc kind *binary-constraint-constructors*)
        (%two-type-free-vars args)
        (ecase kind
          (:typeclass
           (type-free-vars (second args)))
          (:implication
           (let* ((qvars  (first args))
                  (all-fv (remove-duplicates
                           (append (mapcan #'constraint-free-vars (second args))
                                   (mapcan #'constraint-free-vars (third args)))
                           :test #'type-var-equal-p)))
             (remove-if (lambda (v) (member v qvars :test #'type-var-equal-p))
                        all-fv)))
          (:mult-leq  nil)
          (:row-lacks
           (let ((rho (first args)))
             (when (type-node-p rho) (type-free-vars rho))))
          (:kind-equal nil)))))

;;; ─── Substitution ─────────────────────────────────────────────────────────

(defun constraint-substitute (c subst)
  "Apply type substitution SUBST to constraint C, returning a new constraint."
  (let* ((kind  (constraint-kind c))
         (args  (constraint-args c))
         (ctor  (cdr (assoc kind *binary-constraint-constructors*))))
    (if ctor
        (funcall ctor (zonk (first args) subst) (zonk (second args) subst))
        (ecase kind
          (:typeclass
           (make-typeclass-constraint (first args) (zonk (second args) subst)))
          (:implication
           (make-implication-constraint
            (first args)
            (mapcar (lambda (g) (constraint-substitute g subst)) (second args))
            (mapcar (lambda (w) (constraint-substitute w subst)) (third args))))
          (:mult-leq  c)
          (:row-lacks
           (make-row-lacks-constraint (zonk (first args) subst) (second args)))
          (:kind-equal c)))))
