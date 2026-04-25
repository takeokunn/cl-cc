;;;; solver.lisp - OutsideIn(X) Constraint Solver
;;;;
;;;; Implements the constraint solver used by inference.lisp.
;;;;
;;;; Public API:
;;;;   solve-constraints  (constraints subst)
;;;;       -> (values new-subst residual-constraints)
;;;;   collect-constraints (ast env)
;;;;       -> (values type constraints)

(in-package :cl-cc/type)

;;; ─── solve-constraints ────────────────────────────────────────────────────

(defun solve-constraints (constraints subst)
  "OutsideIn(X) constraint solver.
CONSTRAINTS is a list of constraint structs (from constraint.lisp).
SUBST       is a substitution (from substitution.lisp).

Returns (values new-subst residual-constraints).
  new-subst   — the substitution extended by equality solving.
  residual    — constraints that could not be discharged (typeclass, implication)."
  (unless subst (setf subst (make-substitution)))
  (let ((residual nil)
        (current-subst subst))
    (dolist (c constraints)
      ;; Apply current substitution before inspecting
      (let ((c (constraint-substitute c current-subst)))
        (ecase (constraint-kind c)

          ;; (:equal t1 t2) ─ standard unification
          (:equal
           (let ((args (constraint-args c)))
             (multiple-value-bind (new-subst ok)
                 (type-unify (first args) (second args) current-subst)
               (if ok
                   (setf current-subst new-subst)
                   ;; Unification failed: leave as residual (error recovery)
                   (push c residual)))))

          ;; (:subtype t1 t2) ─ subtyping check
          (:subtype
           (let* ((args (constraint-args c))
                  (t1  (zonk (first args)  current-subst))
                  (t2  (zonk (second args) current-subst)))
             (unless (is-subtype-p t1 t2)
               ;; Record as residual (don't blow up, let caller decide)
               (push c residual))))

          ;; (:typeclass C tau) ─ instance check
           (:typeclass
            (let* ((args       (constraint-args c))
                   (class-name (first args))
                   (tau        (zonk (second args) current-subst)))
              (cond
                ;; Free variable: defer (becomes residual until zonked)
                ((type-var-p tau)
                  (if (and (default-numeric-typeclass-p class-name)
                           *default-numeric-type*)
                      (multiple-value-bind (new-subst ok)
                          (type-unify tau *default-numeric-type* current-subst)
                        (if ok
                            (setf current-subst new-subst)
                            (push c residual)))
                      (push c residual)))
                ;; Type errors do not satisfy constraints; keep them residual so
                ;; callers can surface the failure instead of silently accepting.
                ((type-error-p tau)
                 (push c residual))
               ;; Check instance
               ((has-typeclass-instance-p class-name tau)
                nil)
               ;; No instance found: residual (caller may report error)
               (t
                (push c residual)))))

          ;; (:implication vars given wanted) ─ GADT / rank-N constraint
          ;; Simplified: locally solve given+wanted with fresh vars
          (:implication
           (let* ((args   (constraint-args c))
                  (qvars  (first args))
                  (given  (second args))
                  (wanted (third args)))
             ;; Extend subst with fresh vars for qvars (local scope)
             (let ((local-subst (make-substitution)))
               (dolist (v qvars)
                 (subst-extend! v (fresh-type-var (type-var-name v)) local-subst))
               ;; Solve given locally
               (multiple-value-bind (s2 _r)
                   (solve-constraints (mapcar (lambda (g)
                                                (constraint-substitute g local-subst))
                                              given)
                                      local-subst)
                 (declare (ignore _r))
                 ;; Solve wanted under given's solution
                 (multiple-value-bind (_s3 r2)
                     (solve-constraints (mapcar (lambda (w)
                                                  (constraint-substitute w s2))
                                                wanted)
                                        s2)
                   (declare (ignore _s3))
                   ;; Residual wanted propagate as implication residuals
                   (when r2
                     (push c residual)))))))

          ;; (:effect-subset e1 e2) ─ effect row inclusion
          (:effect-subset
           (let* ((args (constraint-args c))
                  (e1   (zonk (first args)  current-subst))
                  (e2   (zonk (second args) current-subst)))
             (when (and (type-effect-row-p e1) (type-effect-row-p e2))
               (unless (effect-row-subset-p e1 e2)
                 (push c residual)))))

          ;; (:mult-leq q1 q2) ─ multiplicity ordering
          (:mult-leq
           (let* ((args (constraint-args c))
                  (q1   (first args))
                  (q2   (second args)))
             (unless (mult-leq q1 q2)
               (push c residual))))

           ;; (:row-lacks rho label) ─ row does not contain label
           (:row-lacks
            (let* ((args  (constraint-args c))
                   (rho   (zonk (first args) current-subst))
                   (label (second args)))
               ;; Only discharge the constraint when the row is concrete and
               ;; the label is definitely absent.  Open row variables are
               ;; accepted here; concrete open rows stay residual so the caller
               ;; can refine them later.
               (cond
                 ((type-var-p rho)
                  nil)
                 ((or (type-record-p rho)
                      (type-variant-p rho)
                      (type-effect-row-p rho))
                  (if (row-closed-p rho)
                      (when (or (row-select label rho)
                                (and (type-effect-row-p rho)
                                     (effect-row-member-p label rho)))
                        (push c residual))
                      nil))
                 (t
                  (push c residual)))))

          ;; (:kind-equal k1 k2) ─ kind equality
          (:kind-equal
           (let* ((args (constraint-args c))
                  (k1   (first args))
                  (k2   (second args)))
             (unless (kind-equal-p k1 k2)
               (push c residual)))))))

    (values current-subst (nreverse residual))))

;;; collect-constraints (AST walker that generates equality constraints)
;;; is in solver-collect.lisp (loaded next).
