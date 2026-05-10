;;;; packages/type/src/solver-collect.lisp — Constraint Collection Pass
;;;
;;; Extracted from solver.lisp.
;;; Contains: collect-constraints — AST walker that generates equality
;;;   constraints for the OutsideIn(X) solver.
;;;
;;; Depends on solver.lisp / constraint.lisp (make-equal-constraint),
;;;   substitution-schemes.lisp (instantiate, generalize, fresh-type-var),
;;;   representation.lisp (make-type-arrow, type-int, type-string, etc.),
;;;   environment.lisp (type-env-lookup, type-env-extend, type-env-extend*,
;;;     type-to-scheme).
;;; Load order: immediately after solver.lisp.

(in-package :cl-cc/type)

;;; ─── collect-constraints ──────────────────────────────────────────────────

(defun collect-constraints (ast env)
  "Generate constraints by walking AST.
Returns (values type constraints) where TYPE is the expected/inferred type
and CONSTRAINTS is a list of constraint structs.

This is a simplified constraint-generation pass: it walks the AST once,
assigns fresh type variables, and emits equality constraints at each node.
The actual solving is done by solve-constraints."
  (let ((constraints nil))
    (labels
        ((emit (c) (push c constraints))
         (emit= (t1 t2) (emit (make-equal-constraint t1 t2)))

         (gen (node env)
           (typecase node
             (cl-cc/ast:ast-int type-int)

             (cl-cc/ast:ast-var
              (multiple-value-bind (scheme found-p)
                  (type-env-lookup (cl-cc/ast:ast-var-name node) env)
                (if found-p
                    (instantiate scheme)
                    (error 'unbound-variable-error
                           :message (format nil "Unbound variable: ~A"
                                            (cl-cc/ast:ast-var-name node))
                           :variable-name (cl-cc/ast:ast-var-name node)))))

             (cl-cc/ast:ast-quote
              (let ((val (cl-cc/ast:ast-quote-value node)))
                (cond ((integerp val) type-int)
                      ((stringp  val) type-string)
                      ((symbolp  val) type-symbol)
                      ((consp    val) type-cons)
                      (t +type-unknown+))))

             (cl-cc/ast:ast-if
              (let* ((then-ty (gen (cl-cc/ast:ast-if-then node) env))
                     (else-ty (gen (cl-cc/ast:ast-if-else node) env))
                     (result  (fresh-type-var :name "if")))
                (gen (cl-cc/ast:ast-if-cond node) env)
                (emit= result then-ty)
                (emit= result else-ty)
                result))

             (cl-cc/ast:ast-let
              (let ((new-env env))
                (dolist (binding (cl-cc/ast:ast-let-bindings node))
                  (let* ((name    (car binding))
                         (rhs     (cdr binding))
                         (rhs-ty  (gen rhs new-env))
                         (scheme  (generalize new-env rhs-ty)))
                    (setf new-env (type-env-extend name scheme new-env))))
                (let ((result type-null))
                  (dolist (form (cl-cc/ast:ast-let-body node))
                    (setf result (gen form new-env)))
                  result)))

             (cl-cc/ast:ast-lambda
              (let* ((params (cl-cc/ast:ast-lambda-params node))
                     (p-types (mapcar (lambda (p)
                                        (declare (ignore p))
                                        (fresh-type-var :name "p"))
                                      params))
                     (body-env (type-env-extend*
                                (mapcar (lambda (name ty)
                                          (cons name (type-to-scheme ty)))
                                        params p-types)
                                env))
                     (body-forms (cl-cc/ast:ast-lambda-body node))
                     (body-ty (if (null body-forms)
                                  type-null
                                  (let ((last-ty type-null))
                                    (dolist (f body-forms)
                                      (setf last-ty (gen f body-env)))
                                    last-ty))))
                (make-type-arrow p-types body-ty)))

             (cl-cc/ast:ast-call
              (let* ((fn-ty  (gen (cl-cc/ast:ast-call-func node) env))
                     (arg-tys (mapcar (lambda (a) (gen a env))
                                      (cl-cc/ast:ast-call-args node)))
                     (ret-ty (fresh-type-var :name "r")))
                (emit= fn-ty (make-type-arrow arg-tys ret-ty))
                ret-ty))

             (cl-cc/ast:ast-progn
              (let ((forms (cl-cc/ast:ast-progn-forms node)))
                (if (null forms)
                    type-null
                    (let ((result type-null))
                      (dolist (f forms)
                        (setf result (gen f env)))
                      result))))

             (cl-cc/ast:ast-defun
              (let* ((params (cl-cc/ast:ast-defun-params node))
                     (p-types (mapcar (lambda (p)
                                        (declare (ignore p))
                                        (fresh-type-var :name "p"))
                                      params))
                     (body-env (type-env-extend*
                                (mapcar (lambda (name ty)
                                          (cons name (type-to-scheme ty)))
                                        params p-types)
                                env))
                     (body-forms (cl-cc/ast:ast-defun-body node)))
                (dolist (f body-forms)
                  (gen f body-env))
                type-symbol))

             (cl-cc/ast:ast-defvar
              (when (cl-cc/ast:ast-defvar-value node)
                (gen (cl-cc/ast:ast-defvar-value node) env))
              type-symbol)

             (cl-cc/ast:ast-setq
              (let ((val-ty (gen (cl-cc/ast:ast-setq-value node) env)))
                (multiple-value-bind (scheme found-p)
                    (type-env-lookup (cl-cc/ast:ast-setq-var node) env)
                  (when found-p
                    (emit= val-ty (instantiate scheme))))
                val-ty))

             (t (fresh-type-var :name "?")))))

      (let ((ty (gen ast env)))
        (values ty (nreverse constraints))))))
