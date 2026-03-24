;;;; solver.lisp - OutsideIn(X) Constraint Solver
;;;;
;;;; Implements the constraint solver used by inference.lisp.
;;;;
;;;; Public API:
;;;;   solve-constraints  (constraints subst)
;;;;       -> (values new-subst residual-constraints)
;;;;   collect-constraints (ast env)
;;;;       -> (values type constraints)
;;;;   make-constraint    — alias for %make-constraint (backward compat)

(in-package :cl-cc/type)

;;; ─── make-constraint backward compat alias ────────────────────────────────

(defun make-constraint (left right)
  "Backward-compat: build an equality constraint (left ~ right).
Calls the real %make-constraint from constraint.lisp."
  (%make-constraint :kind :equal :args (list left right)))

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
                (push c residual))
               ;; Gradual typing / error: accept
               ((or (type-error-p tau) (type-unknown-p tau))
                nil)
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
             ;; If rho is a record and label is present, it's a violation
             (when (and (type-record-p rho)
                        (row-select label rho))
               (push c residual))))

          ;; (:kind-equal k1 k2) ─ kind equality
          (:kind-equal
           (let* ((args (constraint-args c))
                  (k1   (first args))
                  (k2   (second args)))
             (unless (kind-equal-p k1 k2)
               (push c residual)))))))

    (values current-subst (nreverse residual))))

;;; ─── collect-constraints ──────────────────────────────────────────────────

(defun collect-constraints (ast env)
  "Generate constraints by walking AST.
Returns (values type constraints) where TYPE is the expected/inferred type
and CONSTRAINTS is a list of constraint structs.

This is a simplified constraint-generation pass: it walks the AST once,
assigns fresh type variables, and emits equality constraints at each node.
The actual solving is done by solve-constraints."
  (let ((constraints nil))
    (labels
        ((emit (c) (push c constraints))
         (emit= (t1 t2) (emit (make-equal-constraint t1 t2)))

         (gen (node env)
           (cond
             ;; Literals
             ((typep node 'cl-cc:ast-int)
              type-int)

             ;; Variable reference
             ((typep node 'cl-cc:ast-var)
              (multiple-value-bind (scheme found-p)
                  (type-env-lookup (cl-cc:ast-var-name node) env)
                (if found-p
                    (instantiate scheme)
                    ;; Unbound: signal error (consistent with %infer-ast behavior)
                    (error 'unbound-variable-error
                           :message (format nil "Unbound variable: ~A"
                                            (cl-cc:ast-var-name node))
                           :variable-name (cl-cc:ast-var-name node)))))

             ;; Quote
             ((typep node 'cl-cc:ast-quote)
              (let ((val (cl-cc:ast-quote-value node)))
                (cond ((integerp val) type-int)
                      ((stringp  val) type-string)
                      ((symbolp  val) type-symbol)
                      ((consp    val) type-cons)
                      (t +type-unknown+))))

             ;; If expression
             ((typep node 'cl-cc:ast-if)
              (let* ((then-ty (gen (cl-cc:ast-if-then node) env))
                     (else-ty (gen (cl-cc:ast-if-else node) env))
                     (result  (fresh-type-var "if")))
                (gen (cl-cc:ast-if-cond node) env)
                (emit= result then-ty)
                (emit= result else-ty)
                result))

             ;; Let binding
             ((typep node 'cl-cc:ast-let)
              (let ((new-env env))
                (dolist (binding (cl-cc:ast-let-bindings node))
                  (let* ((name    (car binding))
                         (rhs     (cdr binding))
                         (rhs-ty  (gen rhs new-env))
                         (scheme  (generalize new-env rhs-ty)))
                    (setf new-env (type-env-extend name scheme new-env))))
                (let ((result type-null))
                  (dolist (form (cl-cc:ast-let-body node))
                    (setf result (gen form new-env)))
                  result)))

             ;; Lambda
             ((typep node 'cl-cc:ast-lambda)
              (let* ((params (cl-cc:ast-lambda-params node))
                     (p-types (mapcar (lambda (p)
                                        (declare (ignore p))
                                        (fresh-type-var "p"))
                                      params))
                     (body-env (type-env-extend*
                                (mapcar (lambda (name ty)
                                          (cons name (type-to-scheme ty)))
                                        params p-types)
                                env))
                     ;; ast-lambda-body is a list (from ast-callable)
                     (body-forms (cl-cc:ast-lambda-body node))
                     (body-ty (if (null body-forms)
                                  type-null
                                  (let ((last-ty type-null))
                                    (dolist (f body-forms)
                                      (setf last-ty (gen f body-env)))
                                    last-ty))))
                (make-type-arrow p-types body-ty)))

             ;; Call
             ((typep node 'cl-cc:ast-call)
              (let* ((fn-ty  (gen (cl-cc:ast-call-func node) env))
                     (arg-tys (mapcar (lambda (a) (gen a env))
                                      (cl-cc:ast-call-args node)))
                     (ret-ty (fresh-type-var "r")))
                (emit= fn-ty (make-type-arrow arg-tys ret-ty))
                ret-ty))

             ;; Progn
             ((typep node 'cl-cc:ast-progn)
              (let ((forms (cl-cc:ast-progn-forms node)))
                (if (null forms)
                    type-null
                    (let ((result type-null))
                      (dolist (f forms)
                        (setf result (gen f env)))
                      result))))

             ;; Defun (body is a list)
             ((typep node 'cl-cc:ast-defun)
              (let* ((params (cl-cc:ast-defun-params node))
                     (p-types (mapcar (lambda (p)
                                        (declare (ignore p))
                                        (fresh-type-var "p"))
                                      params))
                     (body-env (type-env-extend*
                                (mapcar (lambda (name ty)
                                          (cons name (type-to-scheme ty)))
                                        params p-types)
                                env))
                     (body-forms (cl-cc:ast-defun-body node)))
                (dolist (f body-forms)
                  (gen f body-env))
                type-symbol))

             ;; Defvar / setq
             ((typep node 'cl-cc:ast-defvar)
              (when (cl-cc:ast-defvar-value node)
                (gen (cl-cc:ast-defvar-value node) env))
              type-symbol)

             ((typep node 'cl-cc:ast-setq)
              (let ((val-ty (gen (cl-cc:ast-setq-value node) env)))
                (multiple-value-bind (scheme found-p)
                    (type-env-lookup (cl-cc:ast-setq-var node) env)
                  (when found-p
                    (emit= val-ty (instantiate scheme))))
                val-ty))

             ;; Unknown: fresh variable (gradual typing)
             (t
              (fresh-type-var "?")))))

      (let ((ty (gen ast env)))
        (values ty (nreverse constraints))))))
