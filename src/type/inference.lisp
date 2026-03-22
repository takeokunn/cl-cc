;;;; src/type/inference.lisp - Hindley-Milner Type Inference
;;;
;;; Implements Algorithm W with:
;;; - Prolog-style backtracking for constraint solving
;;; - Gradual typing support (? and unknown types)
;;; - Type generalization/instantiation (let-polymorphism)
;;; - Support for Common Lisp type semantics

(in-package :cl-cc/type)

;;; Type Inference State

(defvar *class-type-registry* (make-hash-table :test #'eq)
  "Maps class names to class type descriptors.
   Each entry is an alist of (slot-name . type-node) pairs.")

(defun register-class-type (name slot-types)
  "Register a class with its slot types for type inference.
   SLOT-TYPES is an alist of (slot-name . type-node)."
  (setf (gethash name *class-type-registry*) slot-types))

(defun lookup-class-type (name)
  "Look up registered slot types for class NAME. Returns alist or nil."
  (gethash name *class-type-registry*))

(defun lookup-slot-type (class-name slot-name)
  "Look up the type of SLOT-NAME in CLASS-NAME. Returns type-node or nil."
  (let ((slots (lookup-class-type class-name)))
    (when slots
      (cdr (assoc slot-name slots :test #'eq)))))

(defvar *type-alias-registry* (make-hash-table :test #'eq)
  "Maps type alias names to their expanded type specifiers.
   E.g., integer-or-string → (or fixnum string)")

(defun register-type-alias (name type-spec)
  "Register a type alias NAME expanding to TYPE-SPEC."
  (setf (gethash name *type-alias-registry*) type-spec))

(defun lookup-type-alias (name)
  "Look up a type alias. Returns expanded type-spec or nil."
  (gethash name *type-alias-registry*))

(defvar *type-var-counter* 0)

(defun fresh-type-var (&optional name)
  "Generate fresh type variable with optional NAME."
  (incf *type-var-counter*)
  (make-type-variable (or name (format nil "t~a" *type-var-counter*))))

(defun reset-type-vars! ()
  "Reset type variable counter."
  (setf *type-var-counter* 0))

;;; Type Environment

(defstruct type-env
  (bindings nil :type list))

(defun type-env-empty ()
  "Create empty type environment."
  (make-type-env))

(defun type-env-lookup (name env)
  "Look up type scheme in environment."
  (assoc name (type-env-bindings env) :test #'eq))

(defun type-env-extend (name scheme env)
  "Extend environment with new binding."
  (make-type-env :bindings (cons (cons name scheme)
                               (type-env-bindings env))))

(defun type-env-extend* (bindings env)
  "Extend environment with multiple bindings."
  (reduce (lambda (env binding)
            (type-env-extend (car binding) (cdr binding) env))
          bindings
          :initial-value env))

(defun type-env-to-alist (env)
  "Convert type environment to alist."
  (type-env-bindings env))

(defun type-env-free-vars (env)
  "Get all free type variables in environment."
  (environment-free-vars (type-env-bindings env)))

;;; Type Inference (Algorithm W)

(defun infer (ast env)
  "Infer type of AST in environment.

   Returns (values type substitution) or signals type-error."
  (cond
    ;; Integer literal
    ((typep ast 'cl-cc:ast-int)
     (values type-int nil))

    ;; Variable reference
    ((typep ast 'cl-cc:ast-var)
     (let ((binding (type-env-lookup (cl-cc:ast-var-name ast) env)))
       (if binding
           (values (instantiate (cdr binding)) nil)
           (error 'unbound-variable-error
                  :name (cl-cc:ast-var-name ast)))))

    ;; Binary operation
    ((typep ast 'cl-cc:ast-binop)
     (infer-binop ast env))

    ;; If expression
    ((typep ast 'cl-cc:ast-if)
     (infer-if ast env))

    ;; Let binding
    ((typep ast 'cl-cc:ast-let)
     (infer-let ast env))

    ;; Lambda
    ((typep ast 'cl-cc:ast-lambda)
     (infer-lambda ast env))

    ;; Progn (sequence)
    ((typep ast 'cl-cc:ast-progn)
     (infer-progn ast env))

    ;; Function call
    ((typep ast 'cl-cc:ast-call)
     (infer-call ast env))

    ;; Print (side effect, returns type of expr)
    ((typep ast 'cl-cc:ast-print)
     (infer-print ast env))

    ;; Quote - infer type from quoted value
    ((typep ast 'cl-cc:ast-quote)
     (let ((val (cl-cc:ast-quote-value ast)))
       (values (cond ((integerp val) type-int)
                     ((stringp val) type-string)
                     ((symbolp val) type-symbol)
                     ((consp val) type-cons)
                     (t +type-unknown+))
               nil)))

    ;; The - type assertion
    ((typep ast 'cl-cc:ast-the)
     (multiple-value-bind (body-type subst) (infer (cl-cc:ast-the-value ast) env)
       (let ((declared (parse-type-specifier (cl-cc:ast-the-type ast))))
         (multiple-value-bind (unified ok) (type-unify body-type declared subst)
           (if ok
               (values (type-substitute declared unified) unified)
               (error 'type-mismatch-error
                      :expected declared
                      :actual body-type))))))

    ;; Setq - variable assignment
    ((typep ast 'cl-cc:ast-setq)
     (multiple-value-bind (val-type subst) (infer (cl-cc:ast-setq-value ast) env)
       (let ((binding (type-env-lookup (cl-cc:ast-setq-var ast) env)))
         (if binding
             (multiple-value-bind (unified ok) (type-unify val-type (instantiate (cdr binding)) subst)
               (if ok
                   (values (type-substitute val-type unified) unified)
                   (error 'type-mismatch-error
                          :expected (instantiate (cdr binding))
                          :actual val-type)))
             (values val-type subst)))))

    ;; Defun - top-level function definition
    ((typep ast 'cl-cc:ast-defun)
     (let* ((param-types (mapcar (lambda (p)
                                   (declare (ignore p))
                                   (fresh-type-var))
                                 (cl-cc:ast-defun-params ast)))
            (param-bindings (mapcar (lambda (name type)
                                     (cons name (make-type-scheme nil type)))
                                   (cl-cc:ast-defun-params ast)
                                   param-types))
            (body-env (type-env-extend* param-bindings env)))
       (infer-body (cl-cc:ast-defun-body ast) body-env)
       (values type-symbol nil)))

    ;; Defvar - top-level variable definition
    ((typep ast 'cl-cc:ast-defvar)
     (when (cl-cc:ast-defvar-value ast)
       (infer (cl-cc:ast-defvar-value ast) env))
     (values type-symbol nil))

    ;; Function reference (#'name)
    ((typep ast 'cl-cc:ast-function)
     (let ((binding (type-env-lookup (cl-cc:ast-function-name ast) env)))
       (if binding
           (values (instantiate (cdr binding)) nil)
           (values +type-unknown+ nil))))

    ;; Flet - local non-recursive function bindings
    ((typep ast 'cl-cc:ast-flet)
     (let ((new-env env))
       (dolist (binding (cl-cc:ast-flet-bindings ast))
         (let* ((name (first binding))
                (params (second binding))
                (body (cddr binding))
                (param-types (mapcar (lambda (p)
                                      (declare (ignore p))
                                      (fresh-type-var))
                                    params))
                (param-bindings (mapcar (lambda (pname ptype)
                                         (cons pname (make-type-scheme nil ptype)))
                                       params param-types))
                (fn-env (type-env-extend* param-bindings env)))
           (multiple-value-bind (body-type subst) (infer-body body fn-env)
             (let* ((fn-type (make-type-function-raw
                                            :params (mapcar (lambda (p)
                                                              (type-substitute p subst))
                                                            param-types)
                                            :return body-type))
                    (scheme (generalize (type-env-bindings new-env) fn-type)))
               (setf new-env (type-env-extend name scheme new-env))))))
       (infer-body (cl-cc:ast-flet-body ast) new-env)))

    ;; Labels - local recursive function bindings
    ((typep ast 'cl-cc:ast-labels)
     (let ((new-env env))
       ;; First pass: bind all function names to fresh type vars
       (dolist (binding (cl-cc:ast-labels-bindings ast))
         (let ((name (first binding)))
           (setf new-env (type-env-extend name
                                          (make-type-scheme nil (fresh-type-var))
                                          new-env))))
       ;; Second pass: infer bodies with all names in scope
       (dolist (binding (cl-cc:ast-labels-bindings ast))
         (let* ((name (first binding))
                (params (second binding))
                (body (cddr binding))
                (param-types (mapcar (lambda (p)
                                      (declare (ignore p))
                                      (fresh-type-var))
                                    params))
                (param-bindings (mapcar (lambda (pname ptype)
                                         (cons pname (make-type-scheme nil ptype)))
                                       params param-types))
                (fn-env (type-env-extend* param-bindings new-env)))
           (multiple-value-bind (body-type subst) (infer-body body fn-env)
             (let* ((fn-type (make-type-function-raw
                                            :params (mapcar (lambda (p)
                                                              (type-substitute p subst))
                                                            param-types)
                                            :return body-type))
                    (scheme (generalize (type-env-bindings new-env) fn-type)))
               (setf new-env (type-env-extend name scheme new-env))))))
       (infer-body (cl-cc:ast-labels-body ast) new-env)))

    ;; Block - named block
    ((typep ast 'cl-cc:ast-block)
     (infer-body (cl-cc:ast-block-body ast) env))

    ;; Return-from - return from named block
    ((typep ast 'cl-cc:ast-return-from)
     (infer (cl-cc:ast-return-from-value ast) env))

    ;; Defclass - class definition: register slot types
    ((typep ast 'cl-cc:ast-defclass)
     (let* ((class-name (cl-cc:ast-defclass-name ast))
            (slots (cl-cc:ast-defclass-slots ast))
            (slot-types
              (loop for slot in slots
                    for stype = (cl-cc:ast-slot-type slot)
                    collect (cons (cl-cc:ast-slot-name slot)
                                  (if stype
                                      (parse-type-specifier stype)
                                      +type-unknown+)))))
       (register-class-type class-name slot-types)
       (values type-symbol nil)))

    ;; Make-instance - object creation: return class as named type
    ((typep ast 'cl-cc:ast-make-instance)
     (let ((class-expr (cl-cc:ast-make-instance-class ast)))
       (if (and (typep class-expr 'cl-cc:ast-quote)
                (symbolp (cl-cc:ast-quote-value class-expr)))
           (values (make-type-primitive
                                  :name (cl-cc:ast-quote-value class-expr))
                   nil)
           (values +type-unknown+ nil))))

    ;; Slot-value - slot access: look up slot type from class registry
    ((typep ast 'cl-cc:ast-slot-value)
     (multiple-value-bind (obj-type subst) (infer (cl-cc:ast-slot-value-object ast) env)
       (let* ((slot-name (cl-cc:ast-slot-value-slot ast))
              (slot-type
                (if (typep obj-type 'type-primitive)
                    (or (lookup-slot-type (type-primitive-name obj-type) slot-name)
                        +type-unknown+)
                    +type-unknown+)))
         (values slot-type subst))))

    ;; Unknown AST type - return unknown for gradual typing
    (t (values +type-unknown+ nil))))

(defun infer-binop (ast env)
  "Infer type for binary operations."
  (multiple-value-bind (lhs-type subst1) (infer (cl-cc:ast-binop-lhs ast) env)
    (multiple-value-bind (rhs-type subst2)
        (infer (cl-cc:ast-binop-rhs ast)
               (apply-subst-env env subst1))
      (let* ((subst (compose-subst subst2 subst1))
             (result-type (fresh-type-var))
             (op-type (make-type-function-raw
                                     :params (list lhs-type rhs-type)
                                     :return result-type))
             ;; Primitive op expects (int, int) -> int
             (expected (make-type-function-raw
                                      :params (list type-int type-int)
                                      :return type-int)))
        (multiple-value-bind (unified ok) (type-unify op-type expected subst)
          (if ok
              (values (type-substitute result-type unified) unified)
              (error 'type-mismatch-error
                     :expected expected
                     :actual op-type)))))))

(defun type-predicate-to-type (predicate-name)
  "Map a type predicate name to the type it tests for.
   Returns a type-node or nil if not a recognized predicate."
  (case predicate-name
    ((numberp integerp) type-int)
    ((stringp) type-string)
    ((symbolp) type-symbol)
    ((consp) type-cons)
    ((null) type-null)
    ((characterp) (make-type-primitive :name 'character))
    ((functionp) (make-type-primitive :name 'function))
    (otherwise nil)))

(defun extract-type-guard (cond-ast)
  "Extract type guard info from a condition AST.
   If the condition is (predicate var) where predicate is a type predicate,
   returns (values var-name narrowed-type) or (values nil nil)."
  (when (typep cond-ast 'cl-cc:ast-call)
    (let ((func (cl-cc:ast-call-func cond-ast))
          (args (cl-cc:ast-call-args cond-ast)))
      ;; Check for (predicate var) pattern
      (when (and (typep func 'cl-cc:ast-var)
                 (= (length args) 1)
                 (typep (first args) 'cl-cc:ast-var))
        (let ((pred-type (type-predicate-to-type (cl-cc:ast-var-name func))))
          (when pred-type
            (values (cl-cc:ast-var-name (first args)) pred-type))))))
  ;; Also handle (typep var 'type) pattern
  )

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
            (infer (cl-cc:ast-if-cond ast) env)
          (declare (ignore ct))
          (setf subst1 s1))
      (error () nil))
    ;; Extract type guard info for narrowing
    (multiple-value-bind (guard-var guard-type)
        (extract-type-guard (cl-cc:ast-if-cond ast))
      (let* ((base-env (apply-subst-env env subst1))
             ;; Narrow environments for each branch
             (then-env (if guard-var
                           (type-env-extend guard-var
                                            (make-type-scheme nil guard-type)
                                            base-env)
                           base-env))
             (else-env (if guard-var
                           (let* ((binding (type-env-lookup guard-var base-env))
                                  (var-type (if binding (instantiate (cdr binding)) nil)))
                             (if (and var-type (typep var-type 'type-union))
                                 (type-env-extend guard-var
                                                  (make-type-scheme nil (narrow-union-type var-type guard-type))
                                                  base-env)
                                 base-env))
                           base-env)))
        (multiple-value-bind (then-type subst2)
            (infer (cl-cc:ast-if-then ast) then-env)
          (multiple-value-bind (else-type subst3)
              (infer (cl-cc:ast-if-else ast) (apply-subst-env else-env subst2))
            (multiple-value-bind (final-subst final-ok) (type-unify then-type else-type subst3)
              (declare (ignore final-ok))
              (values (type-substitute then-type final-subst) final-subst))))))))

(defun infer-let (ast env)
  "Infer type for let with polymorphism."
  (let ((new-env env))
    (dolist (binding (cl-cc:ast-let-bindings ast))
      (let* ((name (car binding))
             (expr (cdr binding))
             (type (multiple-value-bind (result-type s) (infer expr new-env)
                     (type-substitute result-type s)))
             (scheme (generalize (type-env-bindings new-env) type)))
        (setf new-env (type-env-extend name scheme new-env))))
    (infer-body (cl-cc:ast-let-body ast) new-env)))

(defun infer-lambda (ast env)
  "Infer type for lambda."
  (let* ((param-types (mapcar (lambda (p)
                                (declare (ignore p))
                                (fresh-type-var))
                              (cl-cc:ast-lambda-params ast)))
         (param-bindings (mapcar (lambda (name type)
                                   (cons name (make-type-scheme nil type)))
                                 (cl-cc:ast-lambda-params ast)
                                 param-types))
         (body-env (type-env-extend* param-bindings env)))
    (multiple-value-bind (body-type subst)
        (infer-body (cl-cc:ast-lambda-body ast) body-env)
      (values (make-type-function-raw
                             :params (mapcar (lambda (p)
                                               (type-substitute p subst))
                                             param-types)
                             :return body-type)
              subst))))

(defun infer-progn (ast env)
  "Infer type for progn (sequence of expressions)."
  (infer-sequence (cl-cc:ast-progn-forms ast) env))

(defun infer-print (ast env)
  "Infer type for print (returns type of printed expression)."
  (infer (cl-cc:ast-print-expr ast) env))

(defun infer-call (ast env)
  "Infer type for function call."
  (multiple-value-bind (func-type subst1)
      (infer (cl-cc:ast-call-func ast) env)
    (let* ((result-type (fresh-type-var))
           (arg-types (infer-args (cl-cc:ast-call-args ast)
                                  (apply-subst-env env subst1)))
           (expected-fn (make-type-function-raw
                                       :params arg-types
                                       :return result-type))
           )
      (multiple-value-bind (subst ok) (type-unify func-type expected-fn subst1)
        (if ok
            (values (type-substitute result-type subst) subst)
            (error 'type-mismatch-error
                   :expected expected-fn :actual func-type))))))

(defun infer-args (asts env)
  "Infer types for list of ASTs (function arguments)."
  (mapcar (lambda (ast)
            (multiple-value-bind (type subst) (infer ast env)
              (type-substitute type subst)))
          asts))

(defun infer-body (asts env)
  "Infer type of sequence (return last type).
   ASTS is a list of AST nodes. ENV must be a type-env object."
  (if (null asts)
      (values type-int nil)
      (let ((subst nil)
            (current-env env))
        (dolist (ast asts)
          (multiple-value-bind (type new-subst) (infer ast current-env)
            (declare (ignore type))
            (setf subst (compose-subst new-subst subst))
            (setf current-env (apply-subst-env current-env subst))))
        ;; Infer last element again to get final type
        (let ((last-ast (car (last asts))))
          (multiple-value-bind (final-type final-subst)
              (infer last-ast current-env)
            (values (type-substitute final-type (compose-subst final-subst subst))
                    (compose-subst final-subst subst)))))))

(defun infer-sequence (asts env)
  "Infer type of sequence (return last type)."
  (infer-body asts env))

(defun infer-top-level (asts &optional (env (type-env-empty)))
  "Infer types for multiple top-level forms.
   Returns list of (type . substitution) pairs."
  (mapcar (lambda (ast)
            (multiple-value-bind (type subst) (infer ast env)
              (cons type subst)))
          asts))

(defun infer-with-env (ast)
  "Infer type of AST in empty environment (convenience function)."
  (infer ast (type-env-empty)))

;;; Type Annotation

(defun annotate-type (ast env)
  "Annotate AST with inferred types (for debugging).
   Returns (values type annotated-ast)."
  (multiple-value-bind (type subst) (infer ast env)
    (declare (ignore subst))
    (values type ast)))

;;; Condition Classes

(define-condition type-inference-error (error)
  ((message :initarg :message :reader type-inference-error-message))
  (:report (lambda (condition stream)
             (format stream "Type inference error: ~A"
                     (type-inference-error-message condition)))))

(define-condition unbound-variable-error (type-inference-error)
  ((name :initarg :name :initform nil :reader unbound-variable-error-name))
  (:report (lambda (condition stream)
             (format stream "Unbound variable: ~A"
                     (unbound-variable-error-name condition)))))

(define-condition type-mismatch-error (type-inference-error)
  ((expected :initarg :expected :initform nil :reader type-mismatch-error-expected)
   (actual :initarg :actual :initform nil :reader type-mismatch-error-actual))
  (:report (lambda (condition stream)
             (format stream "Type mismatch: expected ~A, got ~A"
                     (type-to-string (type-mismatch-error-expected condition))
                     (type-to-string (type-mismatch-error-actual condition))))))

;;; Constraint Solving (for future extension)

(defstruct type-constraint
  (left nil)
  (right nil))

(defun make-constraint (left right)
  "Create a constraint (left = right)."
  (make-type-constraint :left left :right right))

(defun collect-constraints (ast env)
  "Collect type constraints from AST without solving.
   Returns (values constraints type substitution)."
  ;; For now, just use standard inference
  ;; In a bidirectional system, we'd separate constraint collection
  (infer ast env))

(defun solve-constraints (constraints subst)
  "Solve constraints with substitution SUBST.
   Returns updated substitution or NIL on failure."
  (if (null constraints)
      subst
      (let ((c (car constraints)))
        (multiple-value-bind (new-subst ok) (type-unify (type-constraint-left c)
                                                        (type-constraint-right c)
                                                        subst)
          (when ok
            (solve-constraints (cdr constraints) new-subst))))))

;;; Generalization/Instantiation Helpers

(defun generalize-in-env (env type)
  "Generalize TYPE in environment ENV.
   Alias for generalize for backward compatibility."
  (generalize env type))

(defun instantiate-scheme (scheme env)
  "Instantiate type SCHEME in environment ENV.
   Ensures fresh variables don't clash."
  (instantiate scheme))

;;; Exports

(export '(infer
          infer-binop
          infer-if
          infer-let
          infer-lambda
          infer-call
          infer-progn
          infer-sequence
          infer-args
          infer-top-level
          infer-with-env
          annotate-type

          type-inference-error
          type-inference-error-message
          unbound-variable-error
          unbound-variable-error-name
          type-mismatch-error
          type-mismatch-error-expected
          type-mismatch-error-actual))
