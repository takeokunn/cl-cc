;;;; substitution.lisp - Hash-Table Substitution + Zonking
;;;;
;;;; Replaces the old alist substitution with a hash-table:
;;;;   O(1) lookup vs O(n) for alists.
;;;;   Generation stamps enable snapshot/rollback for GADT constraint solving.
;;;;
;;;; Public API:
;;;;   (make-substitution)            — empty substitution
;;;;   (subst-lookup var subst)       — (values type found-p)
;;;;   (subst-extend! var type subst) — destructive extend (common case)
;;;;   (subst-extend  var type subst) — functional extend (returns new subst)
;;;;   (subst-compose s1 s2)          — s1 ∘ s2 (apply s2 first, then s1)
;;;;   (zonk type subst)              — eagerly apply subst to type
;;;;   (zonk-env env subst)           — zonk all bindings in env

(in-package :cl-cc/type)

;;; ─── Substitution struct ──────────────────────────────────────────────────

(defstruct (substitution (:constructor %make-substitution))
  "A mapping from type-var IDs to types, implemented as a hash-table.
BINDINGS:   hash-table mapping fixnum (var ID) -> type-node.
GENERATION: monotonically increasing stamp; incremented on each extend."
  (bindings   (make-hash-table :test #'eql) :type hash-table)
  (generation 0                             :type fixnum))

(defun make-substitution ()
  "Return a fresh empty substitution."
  (%make-substitution))

(defun empty-subst ()
  "Alias — returns a fresh empty substitution."
  (make-substitution))

;;; ─── Lookup ───────────────────────────────────────────────────────────────

(defun subst-lookup (var subst)
  "Look up type-var VAR in SUBST.
Returns (values type t) if found, (values nil nil) otherwise."
  (if (null subst)
      (values nil nil)
      (let* ((id  (type-var-id var))
             (val (gethash id (substitution-bindings subst))))
        (if val
            (values val t)
            (values nil nil)))))

;;; ─── Extend (functional) ──────────────────────────────────────────────────

(defun subst-extend (var ty subst)
  "Return a NEW substitution that is SUBST extended with VAR -> TY.
Does not modify SUBST."
  (let ((new-bindings (make-hash-table :test #'eql))
        (old-bindings (substitution-bindings subst)))
    ;; Copy old bindings
    (maphash (lambda (k v) (setf (gethash k new-bindings) v)) old-bindings)
    ;; Add new binding
    (setf (gethash (type-var-id var) new-bindings) ty)
    (%make-substitution :bindings   new-bindings
                        :generation (1+ (substitution-generation subst)))))

;;; ─── Extend (destructive — for performance in hot paths) ─────────────────

(defun subst-extend! (var ty subst)
  "Destructively extend SUBST with VAR -> TY.  Returns SUBST."
  (setf (gethash (type-var-id var) (substitution-bindings subst)) ty)
  (incf (substitution-generation subst))
  subst)

;;; ─── Backward-compat wrappers (alist API) ─────────────────────────────────

(defun extend-subst (var ty subst)
  "Functional extend — backward-compat alias for subst-extend."
  (subst-extend var ty subst))

;;; ─── Composition ──────────────────────────────────────────────────────────

(defun subst-compose (s1 s2)
  "Compose substitutions: (s1 ∘ s2).
The result maps v to (zonk (s2 v) s1) for v in dom(s2),
and s1(v) for v in dom(s1) \\ dom(s2)."
  (let ((result (make-substitution)))
    ;; s2 entries, with s1 applied to their ranges
    (maphash (lambda (id ty)
               (setf (gethash id (substitution-bindings result))
                     (zonk ty s1)))
             (substitution-bindings s2))
    ;; s1 entries not already covered by s2
    (maphash (lambda (id ty)
               (unless (gethash id (substitution-bindings s2))
                 (setf (gethash id (substitution-bindings result)) ty)))
             (substitution-bindings s1))
    result))

(defun compose-subst (s1 s2)
  "Alias — backward compat."
  (subst-compose s1 s2))

;;; ─── Zonking ──────────────────────────────────────────────────────────────
;;;
;;; Zonking is an eager, single-pass substitution application.
;;; After constraint solving is complete, we zonk the entire AST once
;;; to eliminate all indirection through the substitution map.

(defun zonk (ty subst)
  "Eagerly apply SUBST to TY, following variable chains to fixpoints."
  (cond
    ;; nil / non-type
    ((null ty) nil)
    ;; Follow type-var links (path compression)
    ((type-var-p ty)
     (let ((link (type-var-link ty)))
       (cond
         (link
          ;; Chain: follow link (with path compression)
          (let ((resolved (zonk link subst)))
            (setf (type-var-link ty) resolved)
            resolved))
         (t
          ;; Check substitution map
          (multiple-value-bind (bound found-p) (subst-lookup ty subst)
            (if found-p
                (let ((resolved (zonk bound subst)))
                  (setf (type-var-link ty) resolved)
                  resolved)
                ty))))))
    ;; Rigid — never zonked
    ((type-rigid-p ty) ty)
    ;; Primitive / error — leaf types
    ((or (type-primitive-p ty) (type-error-p ty)) ty)
    ;; Arrow
    ((type-arrow-p ty)
     (make-type-arrow-raw
      :params  (mapcar (lambda (p) (zonk p subst)) (type-arrow-params ty))
      :return  (zonk (type-arrow-return ty) subst)
      :effects (when (type-arrow-effects ty) (zonk (type-arrow-effects ty) subst))
      :mult    (type-arrow-mult ty)))
    ;; Product
    ((type-product-p ty)
     (make-type-product :elems (mapcar (lambda (e) (zonk e subst)) (type-product-elems ty))))
    ;; Record
    ((type-record-p ty)
     (make-type-record
      :fields  (mapcar (lambda (f) (cons (car f) (zonk (cdr f) subst)))
                       (type-record-fields ty))
      :row-var (when (type-record-row-var ty) (zonk (type-record-row-var ty) subst))))
    ;; Variant
    ((type-variant-p ty)
     (make-type-variant
      :cases   (mapcar (lambda (c) (cons (car c) (zonk (cdr c) subst)))
                       (type-variant-cases ty))
      :row-var (when (type-variant-row-var ty) (zonk (type-variant-row-var ty) subst))))
    ;; Union
    ((type-union-p ty)
     (make-type-union-raw
      :types (mapcar (lambda (t0) (zonk t0 subst)) (type-union-types ty))))
    ;; Intersection
    ((type-intersection-p ty)
     (make-type-intersection-raw
      :types (mapcar (lambda (t0) (zonk t0 subst)) (type-intersection-types ty))))
    ;; Forall — zonk body, leave var alone
    ((type-forall-p ty)
     (make-type-forall :var (type-forall-var ty)
                       :knd (type-forall-knd ty)
                       :body (zonk (type-forall-body ty) subst)))
    ;; Exists
    ((type-exists-p ty)
     (make-type-exists :var (type-exists-var ty)
                       :knd (type-exists-knd ty)
                       :body (zonk (type-exists-body ty) subst)))
    ;; App
    ((type-app-p ty)
     (make-type-app :fun (zonk (type-app-fun ty) subst)
                    :arg (zonk (type-app-arg ty) subst)))
    ;; Lambda
    ((type-lambda-p ty)
     (make-type-lambda :var (type-lambda-var ty)
                       :knd (type-lambda-knd ty)
                       :body (zonk (type-lambda-body ty) subst)))
    ;; Mu
    ((type-mu-p ty)
     (make-type-mu :var (type-mu-var ty)
                   :body (zonk (type-mu-body ty) subst)))
    ;; Refinement
    ((type-refinement-p ty)
     (make-type-refinement :base (zonk (type-refinement-base ty) subst)
                           :predicate (type-refinement-predicate ty)))
    ;; Linear
    ((type-linear-p ty)
     (make-type-linear :base (zonk (type-linear-base ty) subst)
                       :grade (type-linear-grade ty)))
    ;; Capability
    ((type-capability-p ty)
     (make-type-capability :base (zonk (type-capability-base ty) subst)
                           :cap  (type-capability-cap ty)))
    ;; Effect-row
    ((type-effect-row-p ty)
     (let ((effects (mapcar (lambda (e) (zonk e subst)) (type-effect-row-effects ty)))
           (rv (type-effect-row-row-var ty)))
       (if rv
           (let ((resolved (zonk rv subst)))
             (if (type-effect-row-p resolved)
                 ;; Merge rows if row-var resolved to another row
                 (make-type-effect-row
                  :effects (append effects (type-effect-row-effects resolved))
                  :row-var (type-effect-row-row-var resolved))
                 (make-type-effect-row :effects effects :row-var resolved)))
           (make-type-effect-row :effects effects :row-var nil))))
    ;; Effect-op
    ((type-effect-op-p ty)
     (make-type-effect-op :name (type-effect-op-name ty)
                          :args (mapcar (lambda (a) (zonk a subst))
                                        (type-effect-op-args ty))))
    ;; Handler
    ((type-handler-p ty)
     (make-type-handler :effect (zonk (type-handler-effect ty) subst)
                        :input  (zonk (type-handler-input  ty) subst)
                        :output (zonk (type-handler-output ty) subst)))
    ;; Constraint
    ((type-constraint-p ty)
     (make-type-constraint :class-name (type-constraint-class-name ty)
                           :type-arg   (zonk (type-constraint-type-arg ty) subst)))
    ;; Qualified
    ((type-qualified-p ty)
     (make-type-qualified
      :constraints (mapcar (lambda (c) (zonk c subst))
                           (type-qualified-constraints ty))
      :body        (zonk (type-qualified-body ty) subst)))
    ;; Fallthrough
    (t ty)))

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
  (make-type-env :bindings (apply-subst (type-env-bindings env) subst)))

;;; ─── Occurs check ─────────────────────────────────────────────────────────

(defun type-occurs-p (var ty subst)
  "True iff type-var VAR appears free in TY (under SUBST)."
  (labels ((occ (t0)
             (cond
               ((type-var-p t0)
                (multiple-value-bind (bound found-p) (subst-lookup t0 subst)
                  (if found-p
                      (occ bound)
                      (type-var-equal-p var t0))))
               ((type-arrow-p t0)
                (or (some #'occ (type-arrow-params t0))
                    (occ (type-arrow-return t0))
                    (and (type-arrow-effects t0) (occ (type-arrow-effects t0)))))
               ((type-product-p t0)      (some #'occ (type-product-elems t0)))
               ((type-union-p t0)        (some #'occ (type-union-types t0)))
               ((type-intersection-p t0) (some #'occ (type-intersection-types t0)))
               ((type-record-p t0)
                (or (some (lambda (f) (occ (cdr f))) (type-record-fields t0))
                    (and (type-record-row-var t0) (occ (type-record-row-var t0)))))
               ((type-variant-p t0)
                (or (some (lambda (c) (occ (cdr c))) (type-variant-cases t0))
                    (and (type-variant-row-var t0) (occ (type-variant-row-var t0)))))
               ((type-forall-p t0) (occ (type-forall-body t0)))
               ((type-exists-p t0) (occ (type-exists-body t0)))
               ((type-app-p t0) (or (occ (type-app-fun t0)) (occ (type-app-arg t0))))
               ((type-mu-p t0) (occ (type-mu-body t0)))
               ((type-linear-p t0) (occ (type-linear-base t0)))
               ((type-effect-row-p t0)
                (or (some #'occ (type-effect-row-effects t0))
                    (and (type-effect-row-row-var t0) (occ (type-effect-row-row-var t0)))))
               ((type-constraint-p t0) (occ (type-constraint-type-arg t0)))
               ((type-qualified-p t0)
                (or (some #'occ (type-qualified-constraints t0))
                    (occ (type-qualified-body t0))))
               (t nil))))
    (occ ty)))

;;; ─── Generalize / Instantiate ─────────────────────────────────────────────

(defun generalize (env ty)
  "Generalize TY by quantifying free vars not free in ENV."
  (let* ((ty-fv  (type-free-vars ty))
         (env-fv (type-env-free-vars env))
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
               (cond
                 ((type-var-p t0) (canonical t0))
                 ((type-arrow-p t0)
                  (make-type-arrow-raw
                   :params (mapcar #'norm (type-arrow-params t0))
                   :return (norm (type-arrow-return t0))
                   :effects (when (type-arrow-effects t0) (norm (type-arrow-effects t0)))
                   :mult (type-arrow-mult t0)))
                 ((type-product-p t0)
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
