;;;; unification.lisp - Type Unification for Hindley-Milner Type Inference
;;;;
;;;; This module implements Prolog-style unification for types with:
;;;; - Occurs check to prevent infinite types
;;;; - Substitution composition and application
;;;; - Generalization and instantiation for let-polymorphism

(in-package :cl-cc/type)

;;; Substitution Operations

(defun empty-subst ()
  "Return an empty substitution."
  nil)

(defun subst-lookup (var subst)
  "Look up VAR in SUBST. Returns (values binding found-p)."
  (let ((entry (find var subst :key #'car :test #'type-variable-equal-p)))
    (if entry
        (values (cdr entry) t)
        (values nil nil))))

(defun type-variable-equal-p (var1 var2)
  "Check if two type variables are equal (by ID)."
  (and (typep var1 'type-variable)
       (typep var2 'type-variable)
       (= (type-variable-id var1) (type-variable-id var2))))

(defun extend-subst (var type subst)
  "Extend SUBST with binding VAR -> TYPE.
   Returns new substitution (does not modify original)."
  (acons var type subst))

;;; Type Substitution Application

(defun type-substitute (type subst)
  "Apply substitution SUBST to TYPE, replacing type variables.
   
   Returns the type with all bound variables replaced by their bindings.
   Unbound variables remain unchanged."
  (cond
    ;; Empty substitution - return unchanged
    ((null subst) type)
   
    ;; Type variable - look up and recursively substitute
    ((type-variable-p type)
     (multiple-value-bind (binding found-p) (subst-lookup type subst)
       (if found-p
           ;; Recursively substitute in case of transitive bindings
           (type-substitute binding subst)
           ;; Unbound variable remains unchanged
           type)))
   
    ;; Function type - substitute in params and return
    ((typep type 'type-function)
     (make-type-function-raw
                    :params (mapcar (lambda (p) (type-substitute p subst))
                                    (type-function-params type))
                    :return (type-substitute (type-function-return type) subst)
                    :source-location (type-node-source-location type)))
   
    ;; Tuple type - substitute in elements
    ((typep type 'type-tuple)
     (make-type-tuple-raw
                    :elements (mapcar (lambda (e) (type-substitute e subst))
                                      (type-tuple-elements type))
                    :source-location (type-node-source-location type)))
   
     ;; Union type - substitute in alternatives
     ((typep type 'type-union)
      (make-type-union-raw
                     :types (mapcar (lambda (ty) (type-substitute ty subst))
                                    (type-union-types type))
                     :source-location (type-node-source-location type)))
    
     ;; Intersection type - substitute in components
     ((typep type 'type-intersection)
      (make-type-intersection-raw
                     :types (mapcar (lambda (ty) (type-substitute ty subst))
                                    (type-intersection-types type))
                     :source-location (type-node-source-location type)))

     ;; Type constructor - substitute in type arguments
     ((typep type 'type-constructor)
      (make-type-constructor-raw
                     :name (type-constructor-name type)
                     :args (mapcar (lambda (a) (type-substitute a subst))
                                   (type-constructor-args type))
                     :source-location (type-node-source-location type)))

    ;; Primitive, unknown, and other types - return unchanged
    (t type)))

;;; Occurs Check

(defun type-occurs-p (var type subst)
  "Check if VAR occurs in TYPE (prevents infinite types).
   
   A variable occurs in a type if:
   - The type IS the variable
   - The variable appears in any sub-component of a compound type
   
   SUBST is used to follow existing bindings."
  (cond
    ;; Type variable
    ((type-variable-p type)
     (multiple-value-bind (binding found-p) (subst-lookup type subst)
       (if found-p
           ;; Follow the binding and check recursively
           (type-occurs-p var binding subst)
           ;; Check if this is the same variable
           (type-variable-equal-p var type))))
   
    ;; Function type - check params and return
    ((typep type 'type-function)
     (or (some (lambda (p) (type-occurs-p var p subst))
               (type-function-params type))
         (type-occurs-p var (type-function-return type) subst)))
    
    ;; Tuple type - check elements
    ((typep type 'type-tuple)
      (some (lambda (e) (type-occurs-p var e subst))
            (type-tuple-elements type)))
     
     ;; Union type - check alternatives
     ((typep type 'type-union)
      (some (lambda (ty) (type-occurs-p var ty subst))
            (type-union-types type)))
     
     ;; Intersection type - check components
     ((typep type 'type-intersection)
      (some (lambda (ty) (type-occurs-p var ty subst))
            (type-intersection-types type)))

     ;; Type constructor - check type arguments
     ((typep type 'type-constructor)
      (some (lambda (a) (type-occurs-p var a subst))
            (type-constructor-args type)))

     ;; Other types (primitive, unknown) - variable cannot occur
     (t nil)))

;;; Type Unification

(defun type-unify (t1 t2 &optional (subst nil subst-supplied-p))
  "Unify two type-nodes, returning (values substitution success-p).

   Uses Prolog-style unification with occurs check.

   Returns:
     - (values updated-substitution T) on success
     - (values NIL NIL) on unification failure

   Examples:
     (type-unify type-int type-int) => (values NIL T)
     (type-unify ?a type-int)       => (values ((?a . type-int)) T)
     (type-unify type-int type-string) => (values NIL NIL)"
  (declare (ignore subst-supplied-p))
  (macrolet ((succeed (s) `(values ,s t))
             (fail () `(values nil nil)))
    (cond
      ;; Same object - success
      ((eq t1 t2) (succeed subst))

      ;; T1 is type variable
      ((type-variable-p t1)
       (multiple-value-bind (binding found-p) (subst-lookup t1 subst)
         (if found-p
             ;; T1 is bound - unify the binding with T2
             (type-unify binding t2 subst)
             ;; T1 is unbound
             (if (and (type-variable-p t2) (type-variable-equal-p t1 t2))
                 ;; Same variable - success
                 (succeed subst)
                 ;; Check occurs and bind
                 (if (type-occurs-p t1 t2 subst)
                     (fail)  ; Occurs check failed - would create infinite type
                     (succeed (extend-subst t1 t2 subst)))))))

      ;; T2 is type variable
      ((type-variable-p t2)
       (multiple-value-bind (binding found-p) (subst-lookup t2 subst)
         (if found-p
             ;; T2 is bound - unify T1 with the binding
             (type-unify t1 binding subst)
             ;; T2 is unbound - check occurs and bind
             (if (type-occurs-p t2 t1 subst)
                 (fail)  ; Occurs check failed
                 (succeed (extend-subst t2 t1 subst))))))

      ;; Both are function types
      ((and (typep t1 'type-function) (typep t2 'type-function))
       (let ((params1 (type-function-params t1))
             (params2 (type-function-params t2)))
         ;; Arity must match
         (unless (= (length params1) (length params2))
           (return-from type-unify (fail)))
         ;; Unify parameters
         (multiple-value-bind (subst-params ok) (type-unify-lists params1 params2 subst)
           (if ok
               ;; Unify return types
               (type-unify (type-function-return t1)
                           (type-function-return t2)
                           subst-params)
               (fail)))))

      ;; Both are tuple types
      ((and (typep t1 'type-tuple) (typep t2 'type-tuple))
       (type-unify-lists (type-tuple-elements t1)
                         (type-tuple-elements t2)
                         subst))

      ;; Both are union types (simplified - require same structure)
      ((and (typep t1 'type-union) (typep t2 'type-union))
       (let ((types1 (type-union-types t1))
             (types2 (type-union-types t2)))
         (unless (= (length types1) (length types2))
           (return-from type-unify (fail)))
         (type-unify-lists types1 types2 subst)))

      ;; Both are intersection types
      ((and (typep t1 'type-intersection) (typep t2 'type-intersection))
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
      ((and (typep t1 'type-primitive) (typep t2 'type-primitive))
       (if (eq (type-primitive-name t1) (type-primitive-name t2))
           (succeed subst)
           (fail)))

      ;; Both are unknown types - always unify
      ((and (typep t1 'type-unknown) (typep t2 'type-unknown))
       (succeed subst))

      ;; One is unknown, other is concrete - unknown unifies with anything
      ((typep t1 'type-unknown)
       (succeed subst))
      ((typep t2 'type-unknown)
       (succeed subst))

      ;; Different type constructors - fail
      (t (fail)))))

(defun type-unify-lists (types1 types2 subst)
  "Unify two lists of types element-wise.

   Returns (values updated-substitution success-p)."
  (cond
    ;; Both empty - success
    ((and (null types1) (null types2)) (values subst t))
    ;; Length mismatch - fail
    ((or (null types1) (null types2)) (values nil nil))
    ;; Unify first pair, then rest
    (t
     (multiple-value-bind (subst1 ok) (type-unify (car types1) (car types2) subst)
       (if ok
           (type-unify-lists (cdr types1) (cdr types2) subst1)
           (values nil nil))))))

;;; Substitution Composition

(defun compose-subst (s1 s2)
  "Compose two substitutions: s1 ∘ s2.
   
   The result applies s2 first, then s1.
   (compose-subst s1 s2) = s1 ∘ s2
   
   This means: apply the result to a type gives the same result as
   applying s2 first, then s1."
  ;; Apply s1 to the range (values) of s2
  (let ((s2-applied (mapcar (lambda (binding)
                              (cons (car binding)
                                    (type-substitute (cdr binding) s1)))
                            s2)))
    ;; Prepend s1 bindings (s1 takes precedence for overlapping domains)
    (append s1 s2-applied)))

;;; Environment Operations

(defun apply-subst (env subst)
  "Apply substitution SUBST to type environment ENV.

   ENV is an alist mapping symbols to types/type-schemes.
   Returns a new alist with types substituted."
  (mapcar (lambda (entry)
            (let ((val (cdr entry)))
              (cons (car entry)
                    (if (typep val 'type-scheme)
                        (make-type-scheme
                         (type-scheme-quantified-vars val)
                         (type-substitute (type-scheme-type val) subst))
                        (type-substitute val subst)))))
          env))

(defun apply-subst-env (env subst)
  "Apply substitution SUBST to type-env object ENV.
   Returns a new type-env with types substituted."
  (make-type-env
                 :bindings (apply-subst (type-env-bindings env) subst)))

;;; Free Type Variables

(defun type-free-vars (type)
  "Get all free type variables in TYPE as a list."
  (cond
    ;; Type variable - singleton list
    ((type-variable-p type) (list type))
    
    ;; Function type - union of param and return variables
    ((typep type 'type-function)
     (remove-duplicates
      (append (mapcan #'type-free-vars (type-function-params type))
              (type-free-vars (type-function-return type)))
      :test #'type-variable-equal-p))
    
    ;; Tuple type - union of element variables
    ((typep type 'type-tuple)
     (remove-duplicates
      (mapcan #'type-free-vars (type-tuple-elements type))
      :test #'type-variable-equal-p))
    
    ;; Union type
    ((typep type 'type-union)
     (remove-duplicates
      (mapcan #'type-free-vars (type-union-types type))
      :test #'type-variable-equal-p))
    
    ;; Intersection type
    ((typep type 'type-intersection)
     (remove-duplicates
      (mapcan #'type-free-vars (type-intersection-types type))
      :test #'type-variable-equal-p))

    ;; Type constructor
    ((typep type 'type-constructor)
     (remove-duplicates
      (mapcan #'type-free-vars (type-constructor-args type))
      :test #'type-variable-equal-p))

    ;; Other types - no free variables
    (t nil)))

(defun environment-free-vars (env)
  "Get all free type variables in environment ENV."
  (remove-duplicates
   (mapcan (lambda (entry) (type-free-vars (cdr entry))) env)
   :test #'type-variable-equal-p))

;;; Type Schemes for Let-Polymorphism

(defstruct (type-scheme (:constructor make-type-scheme-raw))
  (quantified-vars nil :type list)
  (type nil))

(defun make-type-scheme (quantified-vars type)
  "Create a type scheme with given quantified variables and type."
  (make-type-scheme-raw
                 :quantified-vars quantified-vars
                 :type type))

(defun type-to-scheme (type)
  "Convert a monomorphic type to a type scheme with no quantified variables."
  (make-type-scheme nil type))

;;; Generalization

(defun generalize (env type)
  "Generalize TYPE to a type scheme, quantifying free variables.
   
   Implements the rule:
     Γ ⊢ let x = e in ... → ∀α. τ where α ∉ FTV(Γ)
   
   Free variables in TYPE that are not free in ENV become quantified.
   
   Examples:
     (generalize () (function (?a) ?a)) → ∀a. a -> a
     (generalize ((x . ?a)) (function (?b) ?b)) → ∀b. b -> b (a not quantified)"
  (let* ((type-free (type-free-vars type))
         (env-free (environment-free-vars env))
         ;; Variables to quantify = in type but not in environment
         (to-quantify (set-difference type-free env-free
                                      :test #'type-variable-equal-p)))
    (make-type-scheme to-quantify type)))

;;; Instantiation

(defun instantiate (scheme)
  "Instantiate type SCHEME with fresh type variables.
   
   Each quantified variable is replaced with a fresh type variable.
   This allows polymorphic types to be used at multiple different types.
   
   Example:
     (∀a. a -> a) instantiates to (?fresh1 -> ?fresh1)"
  (let* ((quantified (type-scheme-quantified-vars scheme))
         (type (type-scheme-type scheme))
         ;; Create fresh variable for each quantified variable
         (subst (mapcar (lambda (var)
                          (cons var (make-type-variable
                                     (type-variable-name var))))
                        quantified)))
    (type-substitute type subst)))

;;; Normalize Type (for pretty printing)

(defun normalize-type-variables (type)
  "Rename type variables in TYPE to canonical names (?a, ?b, ...).
   Useful for comparing types after inference."
  (let ((mapping (make-hash-table :test #'eql))
        (counter 0))
    (labels ((get-canonical (var)
               (or (gethash (type-variable-id var) mapping)
                   (let ((name (aref "abcdefghijklmnopqrstuvwxyz"
                                     (mod counter 26))))
                     (incf counter)
                     (setf (gethash (type-variable-id var) mapping)
                           (make-type-variable name)))))
             (normalize-rec (ty)
               (cond
                 ((type-variable-p ty)
                  (get-canonical ty))
                 ((typep ty 'type-function)
                  (make-type-function-raw
                                 :params (mapcar #'normalize-rec
                                                 (type-function-params ty))
                                 :return (normalize-rec (type-function-return ty))))
                 ((typep ty 'type-tuple)
                  (make-type-tuple-raw
                                 :elements (mapcar #'normalize-rec
                                                   (type-tuple-elements ty))))
                 ((typep ty 'type-constructor)
                  (make-type-constructor-raw
                                 :name (type-constructor-name ty)
                                 :args (mapcar #'normalize-rec
                                               (type-constructor-args ty))))
                 (t ty))))
      (normalize-rec type))))

;;; Utility: Apply Unification Result

(defun apply-unification (type subst)
  "Apply substitution from unification to TYPE.
   Convenience function that checks for failure."
  (when subst
    (type-substitute type subst)))
