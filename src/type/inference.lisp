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

;;; NOTE: *type-var-counter*, fresh-type-var, reset-type-vars! are defined
;;;       in representation.lisp.
;;;       type-env struct and type-env-{empty,lookup,extend,extend*,to-alist,free-vars}
;;;       are defined in representation.lisp.
;;;       dict-env-extend / dict-env-lookup are defined in typeclass.lisp.
;;;       Do NOT redefine any of those here.

;;; Helpers

(defun %infer-fn-binding (binding lookup-env result-env)
  "Infer the type scheme for a single (name params . body) binding and extend RESULT-ENV.
   Parameters are looked up in LOOKUP-ENV (= parent env for flet, = mutual env for labels).
   Returns the new type environment."
  (let* ((name         (first binding))
         (params       (second binding))
         (body         (cddr binding))
         (param-types  (mapcar (lambda (p) (declare (ignore p)) (fresh-type-var)) params))
         (param-env    (type-env-extend* (mapcar (lambda (pname ptype)
                                                   (cons pname (make-type-scheme nil ptype)))
                                                 params param-types)
                                         lookup-env)))
    (multiple-value-bind (body-type subst) (infer-body body param-env)
      (let* ((fn-type (make-type-function-raw
                        :params (mapcar (lambda (p) (type-substitute p subst)) param-types)
                        :return body-type))
             (scheme  (generalize result-env fn-type)))
        (type-env-extend name scheme result-env)))))

;;; FR-004: Polymorphic Recursion Helpers

(defun %find-fn-type-declaration (fn-name declarations)
  "Scan DECLARATIONS for (type type-spec fn-name ...) and return type-spec.
   DECLARATIONS is the list from ast-callable-declarations.
   Returns a type specifier s-expression, or nil if not found."
  (when fn-name
    (loop for decl in declarations
          when (and (consp decl)
                    (eq (car decl) 'type)
                    (>= (length decl) 3)
                    (member fn-name (cddr decl) :test #'eq))
          return (second decl))))

;;; ── infer-* handlers (one per AST node type) ────────────────────────────
;;; Each function handles exactly one AST case; `infer` below is a pure
;;; dispatcher.  New AST node support = add one infer-* function + one arm.

(defun infer-var (ast env)
  "Infer type of a variable reference; signals unbound-variable-error if missing."
  (multiple-value-bind (scheme found-p) (type-env-lookup (cl-cc/ast:ast-var-name ast) env)
    (if found-p
        (values (instantiate scheme) nil)
        (error 'unbound-variable-error :name (cl-cc/ast:ast-var-name ast)))))

(defun infer-quote (ast)
  "Infer type of a quoted literal from its CL type."
  (values (typecase (cl-cc/ast:ast-quote-value ast)
            (integer type-int)
            (string  type-string)
            (symbol  type-symbol)
            (cons    type-cons)
            (t       +type-unknown+))
          nil))

(defun infer-the (ast env)
  "Infer type of a type-assertion form, unifying declared and actual types."
  (multiple-value-bind (body-type subst) (infer (cl-cc/ast:ast-the-value ast) env)
    (let ((declared (parse-type-specifier (cl-cc/ast:ast-the-type ast))))
      (multiple-value-bind (unified ok) (type-unify body-type declared subst)
        (if ok
            (values (type-substitute declared unified) unified)
            (error 'type-mismatch-error :expected declared :actual body-type))))))

(defun infer-setq (ast env)
  "Infer type of a variable assignment; unifies with declared type when present."
  (multiple-value-bind (val-type subst) (infer (cl-cc/ast:ast-setq-value ast) env)
    (multiple-value-bind (scheme found-p) (type-env-lookup (cl-cc/ast:ast-setq-var ast) env)
      (if found-p
          (let ((declared (instantiate scheme)))
            (multiple-value-bind (unified ok) (type-unify val-type declared subst)
              (if ok
                  (values (type-substitute val-type unified) unified)
                  (error 'type-mismatch-error :expected declared :actual val-type))))
          (values val-type subst)))))

(defun infer-defun (ast env)
  "Infer type of a function definition.
FR-004 Polymorphic Recursion: an explicit (declare (type T name)) annotation
seeds the function's own name in body-env so recursive call sites may be
instantiated at different monotypes."
  (let* ((fn-name       (cl-cc/ast:ast-defun-name ast))
         (decls         (cl-cc/ast:ast-defun-declarations ast))
         (declared-spec (%find-fn-type-declaration fn-name decls))
         (declared-type (when declared-spec (parse-type-specifier declared-spec)))
         (param-types   (if (and declared-type (type-function-p declared-type))
                            (type-function-params declared-type)
                            (mapcar (lambda (p) (declare (ignore p)) (fresh-type-var))
                                    (cl-cc/ast:ast-defun-params ast))))
         (param-bindings (mapcar (lambda (name type)
                                   (cons name (make-type-scheme nil type)))
                                 (cl-cc/ast:ast-defun-params ast) param-types))
         (body-env (let ((e (type-env-extend* param-bindings env)))
                     (if (and fn-name declared-type)
                         (type-env-extend fn-name (generalize env declared-type) e)
                         e))))
    (multiple-value-bind (body-type subst) (infer-body (cl-cc/ast:ast-defun-body ast) body-env)
      (values (or declared-type
                  (make-type-function-raw
                   :params (mapcar (lambda (p) (type-substitute p subst)) param-types)
                   :return body-type))
              subst))))

(defun infer-defvar (ast env)
  "Infer type of a global variable definition; always returns type-symbol."
  (when (cl-cc/ast:ast-defvar-value ast)
    (infer (cl-cc/ast:ast-defvar-value ast) env))
  (values type-symbol nil))

(defun infer-function (ast env)
  "Infer type of a #'name function reference; returns its scheme or unknown."
  (multiple-value-bind (scheme found-p) (type-env-lookup (cl-cc/ast:ast-function-name ast) env)
    (if found-p
        (values (instantiate scheme) nil)
        (values +type-unknown+ nil))))

(defun infer-flet (ast env)
  "Infer type of an flet form; each binding is inferred against the parent env."
  (let ((new-env env))
    (dolist (binding (cl-cc/ast:ast-flet-bindings ast))
      (setf new-env (%infer-fn-binding binding env new-env)))
    (infer-body (cl-cc/ast:ast-flet-body ast) new-env)))

(defun infer-labels (ast env)
  "Infer type of a labels form using a two-pass strategy for mutual recursion.
Pass 1: seed each name with a fresh type variable so recursive calls resolve.
Pass 2: infer each binding in the fully-seeded mutual environment."
  (let ((new-env env))
    (dolist (binding (cl-cc/ast:ast-labels-bindings ast))
      (setf new-env (type-env-extend (first binding)
                                     (make-type-scheme nil (fresh-type-var))
                                     new-env)))
    (dolist (binding (cl-cc/ast:ast-labels-bindings ast))
      (setf new-env (%infer-fn-binding binding new-env new-env)))
    (infer-body (cl-cc/ast:ast-labels-body ast) new-env)))

(defun infer-defclass (ast)
  "Register the class's slot types and return type-symbol."
  (let ((slot-types (loop for slot in (cl-cc/ast:ast-defclass-slots ast)
                          for stype = (cl-cc/ast:ast-slot-type slot)
                          collect (cons (cl-cc/ast:ast-slot-name slot)
                                        (if stype
                                            (parse-type-specifier stype)
                                            +type-unknown+)))))
    (register-class-type (cl-cc/ast:ast-defclass-name ast) slot-types)
    (values type-symbol nil)))

(defun infer-make-instance (ast)
  "Infer type of a make-instance call; uses the class name as a primitive type."
  (let ((class-expr (cl-cc/ast:ast-make-instance-class ast)))
    (if (and (typep class-expr 'cl-cc/ast:ast-quote)
             (symbolp (cl-cc/ast:ast-quote-value class-expr)))
        (values (make-type-primitive :name (cl-cc/ast:ast-quote-value class-expr)) nil)
        (values +type-unknown+ nil))))

(defun infer-slot-value (ast env)
  "Infer type of a slot-value access; resolves slot type from class registry."
  (multiple-value-bind (obj-type subst) (infer (cl-cc/ast:ast-slot-value-object ast) env)
    (let ((slot-type (if (typep obj-type 'type-primitive)
                         (or (lookup-slot-type (type-primitive-name obj-type)
                                               (cl-cc/ast:ast-slot-value-slot ast))
                             +type-unknown+)
                         +type-unknown+)))
      (values slot-type subst))))

;;; Type Inference (Algorithm W) — pure dispatcher
;;;
;;; Each arm is a one-liner.  All logic lives in the infer-* helpers above.

(defun infer (ast env)
  "Infer type of AST in environment.
Returns (values type substitution) or signals a type-error condition."
  (typecase ast
    (cl-cc/ast:ast-int           (values type-int nil))
    (cl-cc/ast:ast-var           (infer-var           ast env))
    (cl-cc/ast:ast-hole          (error 'typed-hole-error :message (%typed-hole-message env)))
    (cl-cc/ast:ast-quote         (infer-quote         ast))
    (cl-cc/ast:ast-binop         (infer-binop         ast env))
    (cl-cc/ast:ast-if            (infer-if            ast env))
    (cl-cc/ast:ast-the           (infer-the           ast env))
    (cl-cc/ast:ast-setq          (infer-setq          ast env))
    (cl-cc/ast:ast-let           (infer-let           ast env))
    (cl-cc/ast:ast-lambda        (infer-lambda        ast env))
    (cl-cc/ast:ast-progn         (infer-progn         ast env))
    (cl-cc/ast:ast-call          (infer-call          ast env))
    (cl-cc/ast:ast-print         (infer-print         ast env))
    (cl-cc/ast:ast-defun         (infer-defun         ast env))
    (cl-cc/ast:ast-defvar        (infer-defvar        ast env))
    (cl-cc/ast:ast-function      (infer-function      ast env))
    (cl-cc/ast:ast-flet          (infer-flet          ast env))
    (cl-cc/ast:ast-labels        (infer-labels        ast env))
    (cl-cc/ast:ast-block         (infer-body (cl-cc/ast:ast-block-body ast) env))
    (cl-cc/ast:ast-return-from   (infer (cl-cc/ast:ast-return-from-value ast) env))
    (cl-cc/ast:ast-defclass      (infer-defclass      ast))
    (cl-cc/ast:ast-make-instance (infer-make-instance ast))
    (cl-cc/ast:ast-slot-value    (infer-slot-value    ast env))
    ;; Gradual typing: unknown AST nodes become the unknown type.
    (t (values +type-unknown+ nil))))

(defun %typed-hole-message (env)
  "Build a typed-hole error message with available in-scope variables."
  (let ((bindings (and (type-env-p env)
                       (type-env-bindings env))))
    (if bindings
        (with-output-to-string (out)
          (write-string "Typed hole '_' cannot be inferred; fill the hole with an expression. Available: " out)
          (loop for entry in bindings
                for first-p = t then nil
                do (unless first-p (write-string ", " out))
                   (let* ((name (car entry))
                          (scheme (cdr entry))
                          (ty (if (type-scheme-p scheme)
                                  (type-scheme-type scheme)
                                  scheme)))
                     (format out "~A :: ~A" name (type-to-string ty))))
          (write-char #\. out))
        "Typed hole '_' cannot be inferred; fill the hole with an expression. Available: none.")))

(defun infer-binop (ast env)
  "Infer type for binary operations."
  (multiple-value-bind (lhs-type subst1) (infer (cl-cc/ast:ast-binop-lhs ast) env)
    (multiple-value-bind (rhs-type subst2)
        (infer (cl-cc/ast:ast-binop-rhs ast)
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

