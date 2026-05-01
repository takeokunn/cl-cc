;;;; packages/type/src/inference-handlers.lisp - HM Inference Handlers and Dispatcher
;;;
;;; Contains: infer-var, infer-quote, infer-the, infer-setq, infer-defun,
;;; infer-defvar, infer-function, infer-flet, infer-labels, infer-defclass,
;;; infer-make-instance, infer-slot-value, infer (Algorithm W dispatcher),
;;; %typed-hole-message, infer-binop.
;;;
;;; Registries and helpers (%infer-fn-binding, %find-fn-type-declaration) are in inference.lisp.
;;;
;;; Load order: after inference.lisp, before inference-forms.lisp.

(in-package :cl-cc/type)

;;; ── infer-* handlers (one per AST node type) ────────────────────────────
;;; Each function handles exactly one AST case; `infer` is a pure dispatcher.
;;; New AST node support = add one infer-* function + one arm in `infer`.

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
            (values (zonk declared unified) unified)
            (error 'type-mismatch-error :expected declared :actual body-type))))))

(defun infer-setq (ast env)
  "Infer type of a variable assignment; unifies with declared type when present."
  (multiple-value-bind (val-type subst) (infer (cl-cc/ast:ast-setq-value ast) env)
    (multiple-value-bind (scheme found-p) (type-env-lookup (cl-cc/ast:ast-setq-var ast) env)
      (if found-p
          (let ((declared (instantiate scheme)))
            (multiple-value-bind (unified ok) (type-unify val-type declared subst)
              (if ok
                  (values (zonk val-type unified) unified)
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
         (param-types   (if (and declared-type (type-arrow-p declared-type))
                            (type-arrow-params declared-type)
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
                  (make-type-arrow-raw
                   :params (mapcar (lambda (p) (zonk p subst)) param-types)
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

;;; ── Algorithm W dispatcher ────────────────────────────────────────────────
;;; Each arm is a one-liner. All logic lives in the infer-* helpers above.

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
    (t (values +type-unknown+ nil))))

(defun %typed-hole-message (env)
  "Build a typed-hole error message with available in-scope variables."
  (let ((bindings (and (type-env-p env) (type-env-bindings env))))
    (if bindings
        (format nil "Typed hole '_' cannot be inferred; fill the hole with an expression. Available: ~{~A :: ~A~^, ~}."
                (loop for (name . scheme) in bindings
                      for ty = (if (type-scheme-p scheme) (type-scheme-type scheme) scheme)
                      nconc (list name (type-to-string ty))))
        "Typed hole '_' cannot be inferred; fill the hole with an expression. Available: none.")))

(defun infer-binop (ast env)
  "Infer type for binary operations."
  (multiple-value-bind (lhs-type subst1) (infer (cl-cc/ast:ast-binop-lhs ast) env)
    (multiple-value-bind (rhs-type subst2)
        (infer (cl-cc/ast:ast-binop-rhs ast)
               (zonk-env env subst1))
      (let* ((subst (subst-compose subst2 subst1))
               (result-type (fresh-type-var))
               (op-type (make-type-arrow-raw
                         :params (list lhs-type rhs-type)
                         :return result-type))
              (expected (make-type-arrow-raw
                          :params (list type-int type-int)
                          :return type-int)))
        (multiple-value-bind (unified ok) (type-unify op-type expected subst)
          (if ok
              (values (zonk result-type unified) unified)
              (error 'type-mismatch-error
                     :expected expected
                     :actual op-type)))))))
