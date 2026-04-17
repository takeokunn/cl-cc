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

;;; ─── Free variables ───────────────────────────────────────────────────────

(defun constraint-free-vars (c)
  "Return a deduplicated list of type-var nodes free in constraint C."
  (ecase (constraint-kind c)
    (:equal
     (let ((args (constraint-args c)))
       (remove-duplicates
        (append (type-free-vars (first args))
                (type-free-vars (second args)))
        :test #'type-var-equal-p)))
    (:subtype
     (let ((args (constraint-args c)))
       (remove-duplicates
        (append (type-free-vars (first args))
                (type-free-vars (second args)))
        :test #'type-var-equal-p)))
    (:typeclass
     ;; class-name is a symbol; only the type argument carries type-vars
     (type-free-vars (second (constraint-args c))))
    (:implication
     ;; ∀vars. given ⊃ wanted
     ;; Free vars = free in given/wanted minus the quantified vars
     (let* ((args    (constraint-args c))
            (qvars   (first args))
            (given   (second args))
            (wanted  (third args))
            (gfv     (mapcan #'constraint-free-vars given))
            (wfv     (mapcan #'constraint-free-vars wanted))
            (all-fv  (remove-duplicates (append gfv wfv) :test #'type-var-equal-p)))
       (remove-if (lambda (v)
                    (member v qvars :test #'type-var-equal-p))
                  all-fv)))
    (:effect-subset
     (let ((args (constraint-args c)))
       (remove-duplicates
        (append (type-free-vars (first args))
                (type-free-vars (second args)))
        :test #'type-var-equal-p)))
    (:mult-leq
     ;; Multiplicity grades are keywords, not types — no type-vars
     nil)
    (:row-lacks
     ;; RHO may be a type-var (open row variable)
     (let ((rho (first (constraint-args c))))
       (if (type-node-p rho)
           (type-free-vars rho)
           nil)))
    (:kind-equal
     ;; Kind nodes carry no type-vars
     nil)))

;;; ─── Substitution ─────────────────────────────────────────────────────────

(defun constraint-substitute (c subst)
  "Apply type substitution SUBST to constraint C, returning a new constraint."
  (ecase (constraint-kind c)
    (:equal
     (let ((args (constraint-args c)))
       (make-equal-constraint (zonk (first args) subst)
                              (zonk (second args) subst))))
    (:subtype
     (let ((args (constraint-args c)))
       (make-subtype-constraint (zonk (first args) subst)
                                (zonk (second args) subst))))
    (:typeclass
     (let ((args (constraint-args c)))
       ;; first arg is the class-name symbol — not a type
       (make-typeclass-constraint (first args)
                                  (zonk (second args) subst))))
    (:implication
     (let* ((args   (constraint-args c))
            (qvars  (first args))
            (given  (mapcar (lambda (g) (constraint-substitute g subst)) (second args)))
            (wanted (mapcar (lambda (w) (constraint-substitute w subst)) (third args))))
       (make-implication-constraint qvars given wanted)))
    (:effect-subset
     (let ((args (constraint-args c)))
       (make-effect-subset-constraint (zonk (first args) subst)
                                      (zonk (second args) subst))))
    (:mult-leq
     ;; Multiplicities are not types — substitution is identity
     c)
    (:row-lacks
     (let ((args (constraint-args c)))
       (make-row-lacks-constraint (zonk (first args) subst)
                                  (second args))))
    (:kind-equal
     ;; Kind nodes are not types — substitution is identity
     c)))
