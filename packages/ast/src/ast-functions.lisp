;;;; packages/ast/src/ast-functions.lisp — AST Data Layer, Source Utilities, and Error Reporting
;;;;
;;;; Contains:
;;;;   - ast-children    — flat list of child AST sub-expressions (single source of truth)
;;;;   - ast-bound-names — variable names newly bound by a binding form
;;;;   - ast-location-string — human-readable source location
;;;;   - ast-compilation-error condition type
;;;;   - ast-error — signal a compilation error with location context
;;;;
;;;; Depends on ast.lisp (all AST struct definitions, ast-source-* accessors).
;;;; Load order: immediately after ast.lisp.

(in-package :cl-cc/ast)

;;; ─── AST Data Layer ────────────────────────────────────────────────────────
;;;
;;; ast-children    — returns child sub-expressions (flat list of ast-nodes)
;;; ast-bound-names — returns names newly bound by a binding form
;;;
;;; These functions are the SINGLE SOURCE OF TRUTH for AST structure.
;;; Traversal algorithms (find-free-variables, find-mutated-variables, etc.)
;;; should use these instead of per-node typecase.

(defun ast-children (node)
  "Return a flat list of child AST sub-expressions for NODE.
All structural knowledge about AST shapes lives here."
  (typecase node
    ;; Leaves: no children
    ((or ast-int ast-var ast-hole ast-quote ast-function ast-go) nil)
    ;; Binary operation
    (ast-binop (list (ast-binop-lhs node) (ast-binop-rhs node)))
    ;; Conditional
    (ast-if (list (ast-if-cond node) (ast-if-then node) (ast-if-else node)))
    ;; Sequences
    (ast-progn (ast-progn-forms node))
    (ast-values (ast-values-forms node))
    ;; Single child wrappers
    (ast-print (list (ast-print-expr node)))
    (ast-setq (list (ast-setq-value node)))
    (ast-return-from (list (ast-return-from-value node)))
    (ast-the (list (ast-the-value node)))
    ;; Let: init-exprs + body
    (ast-let (append (mapcar #'cdr (ast-let-bindings node))
                     (ast-let-body node)))
    ;; Callable forms: body + default exprs
    (ast-lambda (append (ast-lambda-body node)
                        (remove nil (mapcar #'second (ast-lambda-optional-params node)))
                        (remove nil (mapcar #'second (ast-lambda-key-params node)))))
    (ast-defun (ast-defun-body node))
    (ast-defmethod (ast-defmethod-body node))
    (ast-defmacro (ast-defmacro-body node))
    ;; Local function bindings: all binding bodies + outer body
    (ast-local-fns (append (loop for b in (ast-local-fns-bindings node)
                                 append (cddr b))
                           (ast-local-fns-body node)))
    ;; Function call: func (if AST) + args
    (ast-call (let ((f (ast-call-func node)))
                (if (typep f 'ast-node) (cons f (ast-call-args node)) (ast-call-args node))))
    (ast-apply (cons (ast-apply-func node) (ast-apply-args node)))
    ;; Block/control flow
    (ast-block (ast-block-body node))
    (ast-tagbody (loop for entry in (ast-tagbody-tags node) append (copy-list (cdr entry))))
    (ast-catch (cons (ast-catch-tag node) (copy-list (ast-catch-body node))))
    (ast-throw (list (ast-throw-tag node) (ast-throw-value node)))
    (ast-unwind-protect (cons (ast-unwind-protected node) (copy-list (ast-unwind-cleanup node))))
    (ast-handler-case (cons (ast-handler-case-form node)
                            (loop for c in (ast-handler-case-clauses node)
                                  append (copy-list (cddr c)))))
    ;; Multiple values
    (ast-multiple-value-call (cons (ast-mv-call-func node) (copy-list (ast-mv-call-args node))))
    (ast-multiple-value-prog1 (cons (ast-mv-prog1-first node) (copy-list (ast-mv-prog1-forms node))))
    (ast-multiple-value-bind (cons (ast-mvb-values-form node) (copy-list (ast-mvb-body node))))
    ;; Defvar: optional init-form
    (ast-defvar (when (ast-defvar-value node) (list (ast-defvar-value node))))
    ;; CLOS
    (ast-defclass (append (remove nil (mapcar #'ast-slot-initform (ast-defclass-slots node)))
                         (mapcar #'cdr (ast-defclass-default-initargs node))))
    (ast-defgeneric nil)
    (ast-make-instance (loop for (k v) on (ast-make-instance-initargs node) by #'cddr collect v))
    (ast-slot-value (list (ast-slot-value-object node)))
    (ast-set-slot-value (list (ast-set-slot-value-object node) (ast-set-slot-value-value node)))
    (ast-set-gethash (list (ast-set-gethash-key node)
                           (ast-set-gethash-table node)
                           (ast-set-gethash-value node)))
    (t nil)))

(defun ast-bound-names (node)
  "Return the list of variable names newly bound by NODE.
Only meaningful for binding forms (let, lambda, defun, flet, labels, mvb)."
  (typecase node
    (ast-let (mapcar #'car (ast-let-bindings node)))
    (ast-lambda (append (ast-lambda-params node)
                        (mapcar #'first (ast-lambda-optional-params node))
                        (when (ast-lambda-rest-param node)
                          (list (ast-lambda-rest-param node)))
                        (mapcar #'first (ast-lambda-key-params node))))
    (ast-defun (ast-defun-params node))
    (ast-local-fns (mapcar #'first (ast-local-fns-bindings node)))
    (ast-multiple-value-bind (ast-mvb-vars node))
    (t nil)))

;;; ─── Source Location Utilities ───────────────────────────────────────────────

(defun ast-location-string (node)
  "Return a human-readable string of the source location for NODE."
  (let ((file (ast-source-file node))
        (line (ast-source-line node))
        (col (ast-source-column node)))
    (cond
      ((and file line col)
       (format nil "~A:~D:~D" file line col))
      ((and file line)
       (format nil "~A:~D" file line))
      (file
       (format nil "~A" file))
      (t "<unknown location>"))))

(define-condition ast-compilation-error (error)
  ((location :initarg :location :reader ast-error-location)
   (format-control :initarg :format-control :reader ast-error-format-control)
   (format-arguments :initarg :format-arguments :reader ast-error-format-arguments))
  (:report (lambda (condition stream)
             (format stream "Compilation error at ~A: ~?"
                     (ast-error-location condition)
                     (ast-error-format-control condition)
                     (ast-error-format-arguments condition)))))

(defun ast-error (node format-control &rest format-args)
  "Signal an error with source location information from NODE."
  (error 'ast-compilation-error
         :location (ast-location-string node)
         :format-control format-control
         :format-arguments format-args))
