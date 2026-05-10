;;;; substitution-schemes.lisp — occurs-check, generalize/instantiate utilities
;;;
;;; Extracted from substitution.lisp.
;;; Contains: occurs check (type-occurs-p), generalize/instantiate operations,
;;;           normalize-type-variables, and apply-unification.
;;;
;;; Depends on substitution.lisp (substitution struct, zonk, subst-lookup).
;;; Load order: immediately after substitution.lisp.

(in-package :cl-cc/type)

;;; ─── Occurs check ─────────────────────────────────────────────────────────

(defun type-occurs-p (var ty subst)
  "True iff type-var VAR appears free in TY (under SUBST)."
  (typecase ty
    (type-var
     (multiple-value-bind (bound found-p) (subst-lookup ty subst)
       (if found-p
           (type-occurs-p var bound subst)
           (type-var-equal-p var ty))))
    (t
     (let ((bound-var (type-bound-var ty)))
       (if (and bound-var (type-var-equal-p var bound-var))
           nil
           (some (lambda (child) (type-occurs-p var child subst))
                 (type-children ty)))))))

;;; ─── Generalize / Instantiate ─────────────────────────────────────────────

(defun generalize (env ty)
  "Generalize TY by quantifying free vars not free in ENV.
ENV may be a type-env struct or nil (empty env)."
  (let* ((env    (or env (type-env-empty)))
         (ty-fv  (type-free-vars ty))
         (env-fv (type-env-free-vars env))
         (to-q   (set-difference ty-fv env-fv :test #'type-var-equal-p)))
    (make-type-scheme to-q ty)))

(defun instantiate (scheme)
  "Instantiate SCHEME with fresh type vars, preserving quantified bounds."
  (let* ((qvars (type-scheme-quantified-vars scheme))
         (body  (type-scheme-type scheme))
         (subst (make-substitution)))
    (dolist (v qvars)
      (subst-extend! v (fresh-type-var :name (type-var-name v)) subst))
    (dolist (v qvars)
      (let ((fresh (zonk v subst)))
        (when (type-var-p fresh)
          (setf (type-var-upper-bound fresh)
                (and (type-var-upper-bound v)
                     (zonk (type-var-upper-bound v) subst))
                (type-var-lower-bound fresh)
                (and (type-var-lower-bound v)
                     (zonk (type-var-lower-bound v) subst))))))
    (zonk body subst)))

(defun %nv-canonical (v mapping counter-cell)
  "Return or create the canonical renamed type variable for V."
  (or (gethash (type-var-id v) mapping)
      (let* ((name (aref "abcdefghijklmnopqrstuvwxyz" (mod (car counter-cell) 26)))
             (nv   (fresh-type-var :name name)))
        (incf (car counter-cell))
        (setf (gethash (type-var-id v) mapping) nv)
        nv)))

(defun %nv-norm (t0 mapping counter-cell)
  "Recursively rename type variables in T0 using MAPPING and COUNTER-CELL."
  (typecase t0
    (type-var (%nv-canonical t0 mapping counter-cell))
    (type-arrow
     (make-type-arrow-raw
      :params (mapcar (lambda (p) (%nv-norm p mapping counter-cell)) (type-arrow-params t0))
      :return (%nv-norm (type-arrow-return t0) mapping counter-cell)
      :effects (when (type-arrow-effects t0) (%nv-norm (type-arrow-effects t0) mapping counter-cell))
      :mult (type-arrow-mult t0)))
    (type-product
     (make-type-product :elems (mapcar (lambda (e) (%nv-norm e mapping counter-cell)) (type-product-elems t0))))
    (t t0)))

(defun normalize-type-variables (ty)
  "Rename type vars in TY to canonical names ?a, ?b, ... (for display)."
  (let ((mapping      (make-hash-table :test #'eql))
        (counter-cell (list 0)))
    (%nv-norm ty mapping counter-cell)))

(defun apply-unification (ty subst)
  "Apply SUBST to TY — convenience wrapper."
  (when subst
    (zonk ty subst)))
