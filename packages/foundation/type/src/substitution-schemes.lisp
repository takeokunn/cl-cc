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
  (labels ((occ (t0)
              (typecase t0
                (type-var
                 (multiple-value-bind (bound found-p) (subst-lookup t0 subst)
                   (if found-p (occ bound) (type-var-equal-p var t0))))
                (t
                 (let ((bound-var (type-bound-var t0)))
                   (if (and bound-var (type-var-equal-p var bound-var))
                       nil
                       (some #'occ (type-children t0))))))))
    (occ ty)))

;;; ─── Generalize / Instantiate ─────────────────────────────────────────────

(defun generalize (env ty)
  "Generalize TY by quantifying free vars not free in ENV.
ENV may be a type-env struct or nil (empty env)."
  (let* ((ty-fv  (type-free-vars ty))
         (env-fv (cond
                    ((null env)       nil)
                    ((type-env-p env) (type-env-free-vars env))
                    (t                (type-env-free-vars env))))
         (to-q   (set-difference ty-fv env-fv :test #'type-var-equal-p)))
    (make-type-scheme to-q ty)))

(defun instantiate (scheme)
  "Instantiate SCHEME with fresh type vars."
  (let* ((qvars (type-scheme-quantified-vars scheme))
         (body  (type-scheme-type scheme))
         (subst (make-substitution)))
    (dolist (v qvars)
      (subst-extend! v (fresh-type-var (type-var-name v)) subst))
    (zonk body subst)))

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
