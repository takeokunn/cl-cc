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
Does not modify SUBST.  Handles NIL as the empty substitution."
  (let ((new-bindings (make-hash-table :test #'eql)))
    ;; Copy old bindings (subst may be nil — treated as empty)
    (when subst
      (maphash (lambda (k v) (setf (gethash k new-bindings) v))
               (substitution-bindings subst)))
    ;; Add new binding
    (setf (gethash (type-var-id var) new-bindings) ty)
    (%make-substitution :bindings   new-bindings
                        :generation (if subst (1+ (substitution-generation subst)) 1))))

;;; ─── Extend (destructive — for performance in hot paths) ─────────────────

(defun subst-extend! (var ty subst)
  "Destructively extend SUBST with VAR -> TY.  Returns SUBST."
  (setf (gethash (type-var-id var) (substitution-bindings subst)) ty)
  (incf (substitution-generation subst))
  subst)

;;; ─── Composition ──────────────────────────────────────────────────────────

(defun subst-compose (s1 s2)
  "Compose substitutions: (s1 ∘ s2).
The result maps v to (zonk (s2 v) s1) for v in dom(s2),
and s1(v) for v in dom(s1) \\ dom(s2).
Nil is treated as the empty substitution (identity element)."
  (cond
    ;; Identity shortcuts: avoid allocating a new subst when one side is empty
    ((null s2) (or s1 (make-substitution)))
    ((null s1) s2)
    (t
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
       result))))

;;; ─── Zonking ──────────────────────────────────────────────────────────────
;;;
;;; Zonking is an eager, single-pass substitution application.
;;; After constraint solving is complete, we zonk the entire AST once
;;; to eliminate all indirection through the substitution map.

(defun zonk (ty subst)
  "Eagerly apply SUBST to TY, following variable chains to fixpoints."
  (typecase ty
    (null nil)
    (type-var
     (let ((link (type-var-link ty)))
       (if link
           (let ((resolved (zonk link subst)))
             (setf (type-var-link ty) resolved)
             resolved)
           (multiple-value-bind (bound found-p) (subst-lookup ty subst)
             (if found-p
                 (let ((resolved (zonk bound subst)))
                   (setf (type-var-link ty) resolved)
                   resolved)
                 ty)))))
    (type-rigid ty)
    ((or type-primitive type-error) ty)
    (type-arrow
     (make-type-arrow-raw
      :params  (mapcar (lambda (p) (zonk p subst)) (type-arrow-params ty))
      :return  (zonk (type-arrow-return ty) subst)
      :effects (when (type-arrow-effects ty) (zonk (type-arrow-effects ty) subst))
      :mult    (type-arrow-mult ty)))
    (type-product
     (make-type-product :elems (mapcar (lambda (e) (zonk e subst)) (type-product-elems ty))))
    (type-record
     (make-type-record
      :fields  (mapcar (lambda (f) (cons (car f) (zonk (cdr f) subst)))
                       (type-record-fields ty))
      :row-var (when (type-record-row-var ty) (zonk (type-record-row-var ty) subst))))
    (type-variant
     (make-type-variant
      :cases   (mapcar (lambda (c) (cons (car c) (zonk (cdr c) subst)))
                       (type-variant-cases ty))
      :row-var (when (type-variant-row-var ty) (zonk (type-variant-row-var ty) subst))))
    (type-union
      (make-type-union-raw
       :types (mapcar (lambda (t0) (zonk t0 subst)) (type-union-types ty))
       :constructor-name (type-union-constructor-name ty)))
    (type-intersection
     (make-type-intersection-raw
      :types (mapcar (lambda (t0) (zonk t0 subst)) (type-intersection-types ty))))
    (type-forall
     (make-type-forall :var (type-forall-var ty)
                       :knd (type-forall-knd ty)
                       :body (zonk (type-forall-body ty) subst)))
    (type-exists
     (make-type-exists :var (type-exists-var ty)
                       :knd (type-exists-knd ty)
                       :body (zonk (type-exists-body ty) subst)))
    (type-app
     (make-type-app :fun (zonk (type-app-fun ty) subst)
                    :arg (zonk (type-app-arg ty) subst)))
    (type-lambda
     (make-type-lambda :var (type-lambda-var ty)
                       :knd (type-lambda-knd ty)
                       :body (zonk (type-lambda-body ty) subst)))
    (type-mu
     (make-type-mu :var (type-mu-var ty)
                   :body (zonk (type-mu-body ty) subst)))
    (type-refinement
     (make-type-refinement :base (zonk (type-refinement-base ty) subst)
                           :predicate (type-refinement-predicate ty)))
    (type-linear
     (make-type-linear :base (zonk (type-linear-base ty) subst)
                       :grade (type-linear-grade ty)))
    (type-capability
     (make-type-capability :base (zonk (type-capability-base ty) subst)
                           :cap  (type-capability-cap ty)))
    (type-effect-row
     (let* ((effects  (mapcar (lambda (e) (zonk e subst)) (type-effect-row-effects ty)))
            (rv       (type-effect-row-row-var ty))
            (resolved (when rv (zonk rv subst))))
       (cond
         ((null rv) (make-type-effect-row :effects effects :row-var nil))
         ((type-effect-row-p resolved)
          ;; Row-var resolved to another row: merge
          (make-type-effect-row
           :effects (append effects (type-effect-row-effects resolved))
           :row-var (type-effect-row-row-var resolved)))
         (t (make-type-effect-row :effects effects :row-var resolved)))))
    (type-effect-op
     (make-type-effect-op :name (type-effect-op-name ty)
                          :args (mapcar (lambda (a) (zonk a subst))
                                        (type-effect-op-args ty))))
    (type-handler
     (make-type-handler :effect (zonk (type-handler-effect ty) subst)
                        :input  (zonk (type-handler-input  ty) subst)
                        :output (zonk (type-handler-output ty) subst)))
    (type-constraint
     (make-type-constraint :class-name (type-constraint-class-name ty)
                           :type-arg   (zonk (type-constraint-type-arg ty) subst)))
    (type-qualified
     (make-type-qualified
      :constraints (mapcar (lambda (c) (zonk c subst))
                           (type-qualified-constraints ty))
      :body        (zonk (type-qualified-body ty) subst)))
     (t ty)))

(defun zonk-env (env subst)
  "Return a type environment with SUBST eagerly applied to all bindings."
  (make-type-env
   :bindings (mapcar (lambda (entry)
                       (cons (car entry)
                             (let ((value (cdr entry)))
                               (if (type-scheme-p value)
                                   (make-type-scheme (type-scheme-quantified-vars value)
                                                     (zonk (type-scheme-type value) subst))
                                   (zonk value subst)))))
                     (type-env-bindings env))
   :dict-bindings (type-env-dict-bindings env)))

;; type-occurs-p (occurs check), generalize, instantiate,
;; normalize-type-variables, apply-unification are in substitution-schemes.lisp
;; (loaded next).
