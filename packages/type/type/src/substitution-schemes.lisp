;;;; substitution-schemes.lisp — Backward-compat, occurs-check, generalize/instantiate
;;;
;;; Extracted from substitution.lisp.
;;; Contains: backward-compat aliases (type-substitute, apply-subst, apply-subst-env),
;;;           occurs check (type-occurs-p), generalize/instantiate operations,
;;;           normalize-type-variables, apply-unification, environment-free-vars.
;;;
;;; Depends on substitution.lisp (substitution struct, zonk, subst-lookup).
;;; Load order: immediately after substitution.lisp.

(in-package :cl-cc/type)

;;; ─── Backward-compat: type-substitute ────────────────────────────────────

(defun type-substitute (ty subst)
  "Backward-compat alias: apply substitution SUBST to TY."
  (zonk ty subst))

(defun apply-subst (env subst)
  "Apply SUBST to an alist environment (backward compat)."
  (mapcar (lambda (entry)
            (cons (car entry)
                  (let ((val (cdr entry)))
                    (if (type-scheme-p val)
                        (make-type-scheme (type-scheme-quantified-vars val)
                                          (zonk (type-scheme-type val) subst))
                        (zonk val subst)))))
          env))

(defun apply-subst-env (env subst)
  "Apply SUBST to a type-env struct (backward compat)."
  (make-type-env :bindings      (apply-subst (type-env-bindings env) subst)
                 :dict-bindings (type-env-dict-bindings env)))

;;; ─── Occurs check ─────────────────────────────────────────────────────────

(defun type-occurs-p (var ty subst)
  "True iff type-var VAR appears free in TY (under SUBST)."
  (labels ((occ (t0)
             (typecase t0
               (type-var
                (multiple-value-bind (bound found-p) (subst-lookup t0 subst)
                  (if found-p (occ bound) (type-var-equal-p var t0))))
               (type-arrow
                (or (some #'occ (type-arrow-params t0))
                    (occ (type-arrow-return t0))
                    (and (type-arrow-effects t0) (occ (type-arrow-effects t0)))))
               (type-product      (some #'occ (type-product-elems t0)))
               (type-union        (some #'occ (type-union-types t0)))
               (type-intersection (some #'occ (type-intersection-types t0)))
               (type-record
                (or (some (lambda (f) (occ (cdr f))) (type-record-fields t0))
                    (and (type-record-row-var t0) (occ (type-record-row-var t0)))))
               (type-variant
                (or (some (lambda (c) (occ (cdr c))) (type-variant-cases t0))
                    (and (type-variant-row-var t0) (occ (type-variant-row-var t0)))))
               (type-forall  (occ (type-forall-body t0)))
               (type-exists  (occ (type-exists-body t0)))
               (type-app     (or (occ (type-app-fun t0)) (occ (type-app-arg t0))))
               (type-mu      (occ (type-mu-body t0)))
               (type-linear  (occ (type-linear-base t0)))
               (type-effect-row
                (or (some #'occ (type-effect-row-effects t0))
                    (and (type-effect-row-row-var t0) (occ (type-effect-row-row-var t0)))))
               (type-constraint (occ (type-constraint-type-arg t0)))
               (type-qualified
                (or (some #'occ (type-qualified-constraints t0))
                    (occ (type-qualified-body t0))))
               (t nil))))
    (occ ty)))

;;; ─── Generalize / Instantiate ─────────────────────────────────────────────

(defun generalize (env ty)
  "Generalize TY by quantifying free vars not free in ENV.
ENV may be a type-env struct, an alist (backward compat), or nil (empty env)."
  (let* ((ty-fv  (type-free-vars ty))
         (env-fv (cond
                   ((null env)       nil)
                   ((type-env-p env) (type-env-free-vars env))
                   ((listp env)      (environment-free-vars env))
                   (t                (type-env-free-vars env))))
         (to-q   (set-difference ty-fv env-fv :test #'type-var-equal-p)))
    (make-type-scheme to-q ty)))

(defun generalize-in-env (env ty)
  "Alias for generalize — backward compat."
  (generalize env ty))

(defun instantiate (scheme)
  "Instantiate SCHEME with fresh type vars."
  (let* ((qvars (type-scheme-quantified-vars scheme))
         (body  (type-scheme-type scheme))
         (subst (make-substitution)))
    (dolist (v qvars)
      (subst-extend! v (fresh-type-var (type-var-name v)) subst))
    (zonk body subst)))

(defun instantiate-scheme (scheme)
  "Alias for instantiate — backward compat."
  (instantiate scheme))

(defun normalize-type-variables (ty)
  "Rename type vars in TY to canonical names ?a, ?b, ... (for display)."
  (let ((mapping (make-hash-table :test #'eql))
        (counter 0))
    (labels ((canonical (v)
               (or (gethash (type-var-id v) mapping)
                   (let* ((name (aref "abcdefghijklmnopqrstuvwxyz" (mod counter 26)))
                          (nv   (fresh-type-var name)))
                     (incf counter)
                     (setf (gethash (type-var-id v) mapping) nv)
                     nv)))
             (norm (t0)
               (typecase t0
                 (type-var (canonical t0))
                 (type-arrow
                  (make-type-arrow-raw
                   :params (mapcar #'norm (type-arrow-params t0))
                   :return (norm (type-arrow-return t0))
                   :effects (when (type-arrow-effects t0) (norm (type-arrow-effects t0)))
                   :mult (type-arrow-mult t0)))
                 (type-product
                  (make-type-product :elems (mapcar #'norm (type-product-elems t0))))
                 (t t0))))
      (norm ty))))

(defun apply-unification (ty subst)
  "Apply SUBST to TY — convenience wrapper."
  (when subst (zonk ty subst)))

(defun environment-free-vars (env)
  "Backward-compat: free vars in an alist env."
  (remove-duplicates
   (mapcan (lambda (entry) (type-free-vars (cdr entry))) env)
   :test #'type-var-equal-p))
