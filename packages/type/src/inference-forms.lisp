(in-package :cl-cc/type)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Type Inference — Predicate Registry + Complex Form Handlers
;;;
;;; Contains: *type-predicate-table* (data), register-type-predicate,
;;; type-predicate-to-type, extract-type-guard, narrow-union-type,
;;; infer-if (type-guard narrowing), syntactic-value-p (FR-1604),
;;; infer-let, infer-lambda, infer-progn, infer-print,
;;; check-qualified-constraints, infer-call, infer-args, infer-body,
;;; infer-with-env, infer-with-constraints, annotate-type,
;;; and condition class definitions (type-error/type-mismatch-error/etc.).
;;;
;;; Basic infer-* handlers (infer-var through infer-binop), the `infer`
;;; dispatcher, and the class/registry setup are in inference.lisp (before).
;;;
;;; Load order: after inference.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Type Predicate Registry (data-driven, extensible)

(defvar *type-predicate-table* (make-hash-table :test #'eq)
  "Maps predicate name symbols to the type-node they test for.
   E.g., 'integerp → type-int.  Extensible via register-type-predicate.")

(defun register-type-predicate (predicate-name type-node)
  "Register PREDICATE-NAME as testing for TYPE-NODE."
  (setf (gethash predicate-name *type-predicate-table*) type-node))

(defun type-predicate-to-type (predicate-name)
  "Map a type predicate name to the type it tests for.
   Looks up *type-predicate-table*; returns nil if not registered."
  (gethash predicate-name *type-predicate-table*))

;;; Built-in predicate→type pairs. This is the data; registration is the logic.
(defparameter *builtin-type-predicates*
  `((numberp    . ,type-int)
    (integerp   . ,type-int)
    (stringp    . ,type-string)
    (symbolp    . ,type-symbol)
    (consp      . ,type-cons)
    (null       . ,type-null)
    (characterp . ,(make-type-primitive :name 'character))
    (functionp  . ,(make-type-primitive :name 'function)))
  "Association list of (predicate-name . type-node) for built-in type predicates.")

(dolist (pair *builtin-type-predicates*)
  (register-type-predicate (car pair) (cdr pair)))

(defun extract-type-guard (cond-ast)
  "Extract type guard info from a condition AST.
   If the condition is (predicate var) where predicate is a type predicate,
   returns (values var-name narrowed-type) or (values nil nil).
   Also handles (typep var 'classname) pattern."
  (when (typep cond-ast 'cl-cc/ast:ast-call)
    (let ((func (cl-cc/ast:ast-call-func cond-ast))
          (args (cl-cc/ast:ast-call-args cond-ast)))
      (cond
        ;; (predicate var) pattern
        ((and (typep func 'cl-cc/ast:ast-var)
              (= (length args) 1)
              (typep (first args) 'cl-cc/ast:ast-var))
         (let ((pred-type (type-predicate-to-type (cl-cc/ast:ast-var-name func))))
           (when pred-type
             (values (cl-cc/ast:ast-var-name (first args)) pred-type))))
        ;; (typep var 'classname) pattern
        ((and (typep func 'cl-cc/ast:ast-var)
              (eq (cl-cc/ast:ast-var-name func) 'typep)
              (= (length args) 2)
              (typep (first args) 'cl-cc/ast:ast-var)
              (typep (second args) 'cl-cc/ast:ast-quote)
              (symbolp (cl-cc/ast:ast-quote-value (second args))))
         (let* ((class-name (cl-cc/ast:ast-quote-value (second args)))
                (prim-type (make-type-primitive :name class-name)))
           (values (cl-cc/ast:ast-var-name (first args)) prim-type)))
        (t (values nil nil))))))

(defun narrow-union-type (union-type keep-type)
  "Remove KEEP-TYPE from UNION-TYPE, returning the remaining types.
   If UNION-TYPE is (or A B C) and KEEP-TYPE is A, returns (or B C)."
  (if (typep union-type 'type-union)
      (let ((remaining (remove-if (lambda (t1) (type-equal-p t1 keep-type))
                                  (type-union-types union-type))))
        (cond
          ((null remaining) +type-unknown+)
          ((= (length remaining) 1) (first remaining))
          (t (make-type-union remaining))))
      union-type))

(defun %infer-if-condition (ast env)
  "Infer the condition type of AST in ENV, returning the substitution.
Returns NIL when the condition check fails (gradual typing: allow failure)."
  (handler-case
      (multiple-value-bind (ct s1) (infer (cl-cc/ast:ast-if-cond ast) env)
        (declare (ignore ct))
        s1)
    (error () nil)))

(defun %narrow-else-env (guard-var guard-type base-env)
  "Return BASE-ENV with GUARD-VAR's union type narrowed to exclude GUARD-TYPE."
  (multiple-value-bind (scheme found-p) (type-env-lookup guard-var base-env)
    (let ((var-type (if found-p (instantiate scheme) nil)))
      (if (and var-type (typep var-type 'type-union))
          (type-env-extend guard-var
                           (make-type-scheme nil (narrow-union-type var-type guard-type))
                           base-env)
          base-env))))

(defun %build-if-branch-envs (ast env cond-subst)
  "Return (values then-env else-env) with narrowed types for each branch of AST."
  (multiple-value-bind (guard-var guard-type)
      (extract-type-guard (cl-cc/ast:ast-if-cond ast))
    (let ((base-env (zonk-env env cond-subst)))
      (if guard-var
          (values (type-env-extend guard-var (make-type-scheme nil guard-type) base-env)
                  (%narrow-else-env guard-var guard-type base-env))
          (values base-env base-env)))))

(defun %unify-if-branches (ast then-env else-env)
  "Infer both branches of AST and unify their types. Returns (values type subst)."
  (multiple-value-bind (then-type subst2) (infer (cl-cc/ast:ast-if-then ast) then-env)
    (multiple-value-bind (else-type subst3)
        (infer (cl-cc/ast:ast-if-else ast) (zonk-env else-env subst2))
      (multiple-value-bind (final-subst _) (type-unify then-type else-type subst3)
        (values (zonk then-type final-subst) final-subst)))))

(defun infer-if (ast env)
  "Infer type for if expression with type-guard narrowing."
  (let ((cond-subst (%infer-if-condition ast env)))
    (multiple-value-bind (then-env else-env)
        (%build-if-branch-envs ast env cond-subst)
      (%unify-if-branches ast then-env else-env))))

;;; FR-1604: Value Restriction
;;; Only syntactic values (lambda, literal, variable, quote, #'fn) may be
;;; generalized.  Applications and other impure expressions are kept monomorphic
;;; to prevent unsound polymorphism with mutable state.

;;; Value restriction (FR-1604): Only syntactic values may be generalized.
;;; Applications and impure expressions stay monomorphic to prevent unsound
;;; polymorphism with mutable state.
(defparameter *syntactic-value-ast-types*
  '(cl-cc/ast:ast-int
    cl-cc/ast:ast-var
    cl-cc/ast:ast-lambda
    cl-cc/ast:ast-quote
    cl-cc/ast:ast-function
    cl-cc/ast:ast-hole)
  "AST node types that qualify as syntactic values under the value restriction.")

(defun syntactic-value-p (ast)
  "Return T if AST is a syntactic value safe to generalize (value restriction)."
  (some (lambda (type) (typep ast type)) *syntactic-value-ast-types*))

(defun infer-let (ast env)
  "Infer type for let with polymorphism (value restriction applied).
   Only syntactic values are generalized; applications stay monomorphic."
  (let ((new-env env))
    (dolist (binding (cl-cc/ast:ast-let-bindings ast))
      (let* ((name (car binding))
             (expr (cdr binding))
             (type (multiple-value-bind (result-type s) (infer expr new-env)
                     (zonk result-type s)))
             ;; Value restriction: generalize only syntactic values.
             (scheme (if (syntactic-value-p expr)
                         (generalize new-env type)
                         (make-type-scheme nil type))))
        (setf new-env (type-env-extend name scheme new-env))))
    (infer-body (cl-cc/ast:ast-let-body ast) new-env)))

(defun %make-lambda-param-env (params env)
  "Assign fresh type vars to PARAMS, extend ENV, return (values types body-env)."
  (when (null params)
    (return-from %make-lambda-param-env (values nil env)))
  (loop for p in params
        for tv = (fresh-type-var)
        collect tv into types
        collect (cons p (make-type-scheme nil tv)) into bindings
        finally (return (values types (type-env-extend* bindings env)))))

(defun infer-lambda (ast env)
  "Infer type for lambda."
  (multiple-value-bind (param-types body-env)
      (%make-lambda-param-env (cl-cc/ast:ast-lambda-params ast) env)
    (multiple-value-bind (body-type subst)
        (infer-body (cl-cc/ast:ast-lambda-body ast) body-env)
      (values (make-type-arrow (mapcar (lambda (p) (zonk p subst)) param-types) body-type)
              subst))))

(defun infer-progn (ast env)
  "Infer type for progn (sequence of expressions)."
  (infer-body (cl-cc/ast:ast-progn-forms ast) env))

(defun infer-print (ast env)
  "Infer type for print (returns type of printed expression)."
  (infer (cl-cc/ast:ast-print-expr ast) env))

