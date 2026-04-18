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

;; Bootstrap built-in predicates at load time via declarative data table.
(dolist (pair `((numberp    . ,type-int)
                (integerp   . ,type-int)
                (stringp    . ,type-string)
                (symbolp    . ,type-symbol)
                (consp      . ,type-cons)
                (null       . ,type-null)
                (characterp . ,(make-type-primitive :name 'character))
                (functionp  . ,(make-type-primitive :name 'function))))
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

(defun infer-if (ast env)
  "Infer type for if expression.
   Supports type narrowing: (if (numberp x) ...) narrows x in each branch."
  (let ((subst1 nil))
    ;; Infer condition type — allow failures for gradual typing
    (handler-case
        (multiple-value-bind (ct s1)
            (infer (cl-cc/ast:ast-if-cond ast) env)
          (declare (ignore ct))
          (setf subst1 s1))
      (error () nil))
    ;; Extract type guard info for narrowing
    (multiple-value-bind (guard-var guard-type)
        (extract-type-guard (cl-cc/ast:ast-if-cond ast))
      (let* ((base-env (apply-subst-env env subst1))
             ;; Narrow environments for each branch
             (then-env (if guard-var
                           (type-env-extend guard-var
                                            (make-type-scheme nil guard-type)
                                            base-env)
                           base-env))
             (else-env (if guard-var
                           (multiple-value-bind (scheme found-p) (type-env-lookup guard-var base-env)
                             (let ((var-type (if found-p (instantiate scheme) nil)))
                               (if (and var-type (typep var-type 'type-union))
                                   (type-env-extend guard-var
                                                    (make-type-scheme nil (narrow-union-type var-type guard-type))
                                                    base-env)
                                   base-env)))
                           base-env)))
        (multiple-value-bind (then-type subst2)
            (infer (cl-cc/ast:ast-if-then ast) then-env)
          (multiple-value-bind (else-type subst3)
              (infer (cl-cc/ast:ast-if-else ast) (apply-subst-env else-env subst2))
            (multiple-value-bind (final-subst final-ok) (type-unify then-type else-type subst3)
              (declare (ignore final-ok))
              (values (type-substitute then-type final-subst) final-subst))))))))

;;; FR-1604: Value Restriction
;;; Only syntactic values (lambda, literal, variable, quote, #'fn) may be
;;; generalized.  Applications and other impure expressions are kept monomorphic
;;; to prevent unsound polymorphism with mutable state.

(defun syntactic-value-p (ast)
  "Return T if AST is a syntactic value safe to generalize (value restriction).
   Values: integer/string literals, variable references, lambdas, quoted data,
   function references, and typed holes.
   Non-values: function calls, binary operations, let/if/progn, etc."
  (typep ast '(or cl-cc/ast:ast-int
                  cl-cc/ast:ast-var
                  cl-cc/ast:ast-lambda
                  cl-cc/ast:ast-quote
                  cl-cc/ast:ast-function
                   cl-cc/ast:ast-hole)))

(defun infer-let (ast env)
  "Infer type for let with polymorphism (value restriction applied).
   Only syntactic values are generalized; applications stay monomorphic."
  (let ((new-env env))
    (dolist (binding (cl-cc/ast:ast-let-bindings ast))
      (let* ((name (car binding))
             (expr (cdr binding))
             (type (multiple-value-bind (result-type s) (infer expr new-env)
                     (type-substitute result-type s)))
             ;; Value restriction: generalize only syntactic values.
             (scheme (if (syntactic-value-p expr)
                         (generalize new-env type)
                         (make-type-scheme nil type))))
        (setf new-env (type-env-extend name scheme new-env))))
    (infer-body (cl-cc/ast:ast-let-body ast) new-env)))

(defun infer-lambda (ast env)
  "Infer type for lambda."
  (let* ((param-types (mapcar (lambda (p)
                                (declare (ignore p))
                                (fresh-type-var))
                              (cl-cc/ast:ast-lambda-params ast)))
         (param-bindings (mapcar (lambda (name type)
                                   (cons name (make-type-scheme nil type)))
                                 (cl-cc/ast:ast-lambda-params ast)
                                 param-types))
         (body-env (type-env-extend* param-bindings env)))
    (multiple-value-bind (body-type subst)
        (infer-body (cl-cc/ast:ast-lambda-body ast) body-env)
      (values (make-type-function-raw
                             :params (mapcar (lambda (p)
                                               (type-substitute p subst))
                                             param-types)
                             :return body-type)
              subst))))

(defun infer-progn (ast env)
  "Infer type for progn (sequence of expressions)."
  (infer-body (cl-cc/ast:ast-progn-forms ast) env))

(defun infer-print (ast env)
  "Infer type for print (returns type of printed expression)."
  (infer (cl-cc/ast:ast-print-expr ast) env))

(defun check-qualified-constraints (func-type subst env)
  "If FUNC-TYPE is a qualified type (Eq a) => ..., verify each constraint
   is satisfied for the instantiated type argument."
  (when (typep func-type 'type-qualified)
    (dolist (constraint (type-qualified-constraints func-type))
      (let* ((class-name (type-class-constraint-class-name constraint))
             (type-arg (type-substitute
                        (type-class-constraint-type-arg constraint)
                        subst)))
        (unless (or (typep type-arg 'type-unknown)
                    (typep type-arg 'type-variable)
                    (and (type-env-p env)
                         (dict-env-lookup class-name type-arg env))
                    (has-typeclass-instance-p class-name type-arg))
          (error 'type-inference-error
                 :message (format nil "No instance of ~A for ~A"
                                  class-name
                                  (type-to-string type-arg))))))))


(defun infer-call (ast env)
  "Infer type for function call with typeclass constraint checking."
  (multiple-value-bind (func-type subst1)
      (infer (cl-cc/ast:ast-call-func ast) env)
    (let* ((result-type (fresh-type-var))
           (arg-types (infer-args (cl-cc/ast:ast-call-args ast)
                                  (apply-subst-env env subst1)))
           (expected-fn (make-type-function-raw
                                       :params arg-types
                                       :return result-type)))
      (multiple-value-bind (subst ok)
          (type-unify func-type expected-fn subst1)
        (if ok
            (progn
              ;; Check typeclass constraints if callee has a qualified type
              (check-qualified-constraints func-type subst env)
              (values (type-substitute result-type subst) subst))
            (error 'type-mismatch-error
                   :expected expected-fn :actual func-type))))))

(defun infer-args (asts env)
  "Infer types for list of ASTs (function arguments), threading substitution."
  (loop with subst = nil
        with current-env = env
        for ast in asts
        collect (multiple-value-bind (type new-subst) (infer ast current-env)
                  (setf subst (compose-subst new-subst subst)
                        current-env (apply-subst-env current-env new-subst))
                  (type-substitute type subst))))

(defun infer-body (asts env)
  "Infer type of sequence (return last type); single-pass over ASTS."
  (if (null asts)
      (values type-null nil)
      (loop with subst = nil
            with current-env = env
            with last-type = type-null
            for ast in asts
            do (multiple-value-bind (type new-subst) (infer ast current-env)
                 (setf subst       (compose-subst new-subst subst)
                       last-type   (type-substitute type subst)
                       current-env (apply-subst-env current-env new-subst)))
            finally (return (values last-type subst)))))


(defun infer-with-env (ast)
  "Infer type of AST in empty environment (convenience function)."
  (infer ast (type-env-empty)))

(defun infer-with-constraints (ast env)
  "Infer type by generating constraints and then solving them.
   Returns (values type substitution residual-constraints)."
  (multiple-value-bind (ty constraints)
      (collect-constraints ast env)
    (multiple-value-bind (subst residual)
        (solve-constraints constraints nil)
      (values (type-substitute ty subst) subst residual))))

;;; Type Annotation

(defun annotate-type (ast env)
  "Annotate AST with inferred types (for debugging).
   Returns (values type annotated-ast)."
  (multiple-value-bind (type subst) (infer ast env)
    (declare (ignore subst))
    (values type ast)))

;;; NOTE: Effect inference, bidirectional checking, constraint solving,
;;;       typeclass registries are in their respective files (loaded after).
;;;
;;; Condition classes (type-inference-error, typed-hole-error, unbound-variable-error,
;;; type-mismatch-error) and the export block are in inference-conditions.lisp (loaded next).
