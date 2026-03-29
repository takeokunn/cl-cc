;;;; unification.lisp - Type Unification (Equality Constraints Only)
;;;;
;;;; Implements structural type unification for the type inference engine.
;;;; All substitution data structures and operations live in substitution.lisp.
;;;;
;;;; Public API:
;;;;   (type-unify t1 t2 &optional subst) — unify two types → (values subst ok)
;;;;   (type-unify-lists ts1 ts2 subst)    — pairwise unification

(in-package :cl-cc/type)

;;; ─── Helper ───────────────────────────────────────────────────────────────

(defun %effect-label (e)
  "Extract the name symbol from a type-effect or type-effect-op node."
  (cond ((type-effect-op-p e) (type-effect-op-name e))
        ((type-effect-p   e) (type-effect-name e))
        (t nil)))

;;; ─── Type Unification ─────────────────────────────────────────────────────

(defun type-unify (t1 t2 &optional (subst (make-substitution)))
  "Unify two type-nodes, returning (values substitution success-p).

Uses Prolog-style unification with occurs check.

Returns:
  - (values updated-substitution T) on success
  - (values NIL NIL) on unification failure

Examples:
  (type-unify type-int type-int)    => (values <subst> T)
  (type-unify ?a type-int)          => (values <subst-with-a=int> T)
  (type-unify type-int type-string) => (values NIL NIL)"
  (macrolet ((succeed (s) `(values ,s t))
             (fail () `(values nil nil)))
    (cond
      ;; Same object - success
      ((eq t1 t2) (succeed subst))

      ;; T1 is type variable (new-style type-var)
      ((type-var-p t1)
       (multiple-value-bind (binding found-p) (subst-lookup t1 subst)
         (if found-p
             (type-unify binding t2 subst)
             (if (and (type-var-p t2) (type-var-equal-p t1 t2))
                 (succeed subst)
                 (if (typep t2 'type-forall)
                     (error 'type-inference-error
                            :message (format nil
                                             "Impredicative type: cannot unify ~A with ~A. ~
                                              Rank-N types must appear in argument positions."
                                             (type-to-string t1)
                                             (type-to-string t2)))
                     (if (type-occurs-p t1 t2 subst)
                         (fail)
                         (succeed (extend-subst t1 t2 subst))))))))

      ;; T2 is type variable
      ((type-var-p t2)
       (multiple-value-bind (binding found-p) (subst-lookup t2 subst)
         (if found-p
             (type-unify t1 binding subst)
             (if (typep t1 'type-forall)
                 (error 'type-inference-error
                        :message (format nil
                                         "Impredicative type: cannot unify ~A with ~A."
                                         (type-to-string t1)
                                         (type-to-string t2)))
                 (if (type-occurs-p t2 t1 subst)
                     (fail)
                     (succeed (extend-subst t2 t1 subst)))))))

      ;; Arrow types — (deftype type-function () 'type-arrow) so this covers both APIs
      ((and (type-arrow-p t1) (type-arrow-p t2))
       (let ((params1 (type-arrow-params t1))
             (params2 (type-arrow-params t2)))
         (unless (= (length params1) (length params2))
           (return-from type-unify (fail)))
         (multiple-value-bind (subst-params ok)
             (type-unify-lists params1 params2 subst)
           (if ok
               (type-unify (type-arrow-return t1)
                           (type-arrow-return t2)
                           subst-params)
               (fail)))))

      ;; Product types — (deftype type-tuple () 'type-product) so this covers both APIs
      ((and (type-product-p t1) (type-product-p t2))
       (type-unify-lists (type-product-elems t1)
                         (type-product-elems t2)
                         subst))

      ;; One side is a union, other is a plain type — check membership
      ;; (union right / left introduction rules for subtyping-style unification)
      ((and (type-union-p t1) (not (type-union-p t2)))
       (if (some (lambda (member)
                   (multiple-value-bind (s ok)
                       (type-unify member t2 subst)
                     (declare (ignore s))
                     ok))
                 (type-union-types t1))
           (succeed subst)
           (fail)))

      ((and (type-union-p t2) (not (type-union-p t1)))
       (if (some (lambda (member)
                   (multiple-value-bind (s ok)
                       (type-unify t1 member subst)
                     (declare (ignore s))
                     ok))
                 (type-union-types t2))
           (succeed subst)
           (fail)))

      ;; Both are union types — canonical sort for order-independence
      ((and (type-union-p t1) (type-union-p t2))
       (let ((types1 (sort (copy-list (type-union-types t1)) #'string< :key #'type-to-string))
             (types2 (sort (copy-list (type-union-types t2)) #'string< :key #'type-to-string)))
         (unless (= (length types1) (length types2))
           (return-from type-unify (fail)))
         (type-unify-lists types1 types2 subst)))

      ;; Both are intersection types
      ((and (type-intersection-p t1) (type-intersection-p t2))
       (let ((types1 (type-intersection-types t1))
             (types2 (type-intersection-types t2)))
         (unless (= (length types1) (length types2))
           (return-from type-unify (fail)))
         (type-unify-lists types1 types2 subst)))

      ;; Both are type constructors (parametric types)
      ((and (typep t1 'type-constructor) (typep t2 'type-constructor))
       (if (eq (type-constructor-name t1) (type-constructor-name t2))
           (let ((args1 (type-constructor-args t1))
                 (args2 (type-constructor-args t2)))
             (unless (= (length args1) (length args2))
               (return-from type-unify (fail)))
             (type-unify-lists args1 args2 subst))
           (fail)))

      ;; Both are primitive types
      ((and (type-primitive-p t1) (type-primitive-p t2))
       (if (eq (type-primitive-name t1) (type-primitive-name t2))
           (succeed subst)
           (fail)))

      ;; type-error (error sentinel / backward-compat unknown) — unifies with anything
      ((or (type-error-p t1) (type-error-p t2))
       (succeed subst))

      ;; Both are effect rows
      ((and (type-effect-row-p t1) (type-effect-row-p t2))
       (unify-effect-rows t1 t2 subst))

      ;; Both are effectful functions
      ((and (typep t1 'type-effectful-function)
            (typep t2 'type-effectful-function))
       (let ((params1 (type-function-params t1))
             (params2 (type-function-params t2)))
         (unless (= (length params1) (length params2))
           (return-from type-unify (fail)))
         (multiple-value-bind (subst-params ok)
             (type-unify-lists params1 params2 subst)
           (if ok
               (multiple-value-bind (subst-ret ok2)
                   (type-unify (type-function-return t1)
                               (type-function-return t2)
                               subst-params)
                 (if ok2
                     (type-unify (type-effectful-function-effects t1)
                                 (type-effectful-function-effects t2)
                                 subst-ret)
                     (fail)))
                (fail)))))

      ;; Refinement types unify through their base type.
      ((type-refinement-p t1)
       (type-unify (type-refinement-base t1) t2 subst))

      ((type-refinement-p t2)
       (type-unify t1 (type-refinement-base t2) subst))

      ;; Different type constructors - fail
      (t (fail)))))

(defun unify-effect-rows (row1 row2 subst)
  "Unify two effect rows.
{e1..en | r1} and {f1..fm | r2}:
- Compute symmetric difference of concrete effects
- If row1 has a row-var, bind it to remaining effects from row2 (and vice versa)
- If neither has a row-var, the effect sets must be equal"
  (let* ((effs1 (type-effect-row-effects row1))
         (effs2 (type-effect-row-effects row2))
         (rv1   (type-effect-row-row-var row1))
         (rv2   (type-effect-row-row-var row2))
         (names1 (mapcar #'%effect-label effs1))
         (names2 (mapcar #'%effect-label effs2))
         (only-in-1 (remove-if (lambda (n) (member n names2)) names1))
         (only-in-2 (remove-if (lambda (n) (member n names1)) names2)))
    (macrolet ((succeed (s) `(values ,s t))
               (fail () `(values nil nil)))
      (cond
        ;; Both sides equal — unify row variables if present
        ((and (null only-in-1) (null only-in-2))
         (cond
           ((and rv1 rv2) (type-unify rv1 rv2 subst))
           ((and (null rv1) (null rv2)) (succeed subst))
           (rv1 (type-unify rv1 (make-type-effect-row :effects nil :row-var nil) subst))
           (rv2 (type-unify rv2 (make-type-effect-row :effects nil :row-var nil) subst))))
        ;; row2 has effects not in row1 — bind rv1 if possible
        ((and (null only-in-1) only-in-2)
         (if rv1
             (let ((extension (make-type-effect-row
                               :effects (mapcar (lambda (n) (make-type-effect :name n))
                                                only-in-2)
                               :row-var rv2)))
               (type-unify rv1 extension subst))
             (fail)))
        ;; row1 has effects not in row2 — bind rv2 if possible
        ((and only-in-1 (null only-in-2))
         (if rv2
             (let ((extension (make-type-effect-row
                               :effects (mapcar (lambda (n) (make-type-effect :name n))
                                                only-in-1)
                               :row-var rv1)))
               (type-unify rv2 extension subst))
             (fail)))
        ;; Both have unique effects — need both row vars
        (t
         (if (and rv1 rv2)
             (let* ((fresh-var (fresh-type-var 'r))
                    (ext1 (make-type-effect-row
                           :effects (mapcar (lambda (n) (make-type-effect :name n)) only-in-2)
                           :row-var fresh-var))
                    (ext2 (make-type-effect-row
                           :effects (mapcar (lambda (n) (make-type-effect :name n)) only-in-1)
                           :row-var fresh-var)))
               (multiple-value-bind (s1 ok1) (type-unify rv1 ext1 subst)
                 (if ok1
                     (type-unify rv2 ext2 s1)
                     (fail))))
             (fail)))))))

(defun type-unify-lists (types1 types2 subst)
  "Unify two lists of types element-wise.
Returns (values updated-substitution success-p)."
  (cond
    ((and (null types1) (null types2)) (values subst t))
    ((or  (null types1) (null types2)) (values nil nil))
    (t
     (multiple-value-bind (subst1 ok)
         (type-unify (car types1) (car types2) subst)
       (if ok
           (type-unify-lists (cdr types1) (cdr types2) subst1)
           (values nil nil))))))
