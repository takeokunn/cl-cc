;;;; packages/ast/src/ast-roundtrip.lisp - AST → S-expression roundtrip
;;;;
;;;; This module provides ast-to-sexp, converting AST nodes back to
;;;; S-expressions for debugging and pretty-printing purposes.
;;;; It is a pure roundtrip concern, separate from parsing and lowering.

(in-package :cl-cc/ast)

;;; AST Pretty Printing

(defgeneric ast-to-sexp (node)
  (:documentation "Convert an AST node back to an S-expression for debugging."))

(defmethod ast-to-sexp ((node ast-int))
  (ast-int-value node))

(defmethod ast-to-sexp ((node ast-var))
  (ast-var-name node))

(defmethod ast-to-sexp ((node ast-hole))
  (declare (ignore node))
  (intern "_" *package*))

(defmethod ast-to-sexp ((node ast-binop))
  (list (ast-binop-op node)
        (ast-to-sexp (ast-binop-lhs node))
        (ast-to-sexp (ast-binop-rhs node))))

(defmethod ast-to-sexp ((node ast-if))
  (list 'if
        (ast-to-sexp (ast-if-cond node))
        (ast-to-sexp (ast-if-then node))
        (ast-to-sexp (ast-if-else node))))

(defmethod ast-to-sexp ((node ast-progn))
  (cons 'progn (mapcar #'ast-to-sexp (ast-progn-forms node))))

(defmethod ast-to-sexp ((node ast-print))
  (list 'print (ast-to-sexp (ast-print-expr node))))

(defmethod ast-to-sexp ((node ast-let))
  (list* 'let
         (mapcar (lambda (binding)
                   (list (car binding) (ast-to-sexp (cdr binding))))
                 (ast-let-bindings node))
         (mapcar #'ast-to-sexp (ast-let-body node))))

(defun %ast-param-slot-to-sexp (slot)
  "Convert an optional/key param slot (name default-ast supplied-p) to lambda-list form."
  (destructuring-bind (name default supplied-p) slot
    (cond ((and default supplied-p) (list name (ast-to-sexp default) supplied-p))
          (default                  (list name (ast-to-sexp default)))
          (t                        name))))

(defun %ast-callable-lambda-list (params optional-params rest-param key-params)
  "Reconstruct a full lambda list from AST callable slot fields."
  (append params
          (when optional-params
            (cons '&optional (mapcar #'%ast-param-slot-to-sexp optional-params)))
          (when rest-param
            (list '&rest rest-param))
          (when key-params
            (cons '&key (mapcar #'%ast-param-slot-to-sexp key-params)))))

(defmethod ast-to-sexp ((node ast-lambda))
  (list* 'lambda
         (%ast-callable-lambda-list (ast-lambda-params node)
                                    (ast-lambda-optional-params node)
                                    (ast-lambda-rest-param node)
                                    (ast-lambda-key-params node))
         (append (mapcar (lambda (d) (list 'declare d)) (ast-lambda-declarations node))
                 (mapcar #'ast-to-sexp (ast-lambda-body node)))))

(defmethod ast-to-sexp ((node ast-function))
  (list 'function (ast-function-name node)))

(defmethod ast-to-sexp ((node ast-block))
  (list* 'block
         (ast-block-name node)
         (mapcar #'ast-to-sexp (ast-block-body node))))

(defmethod ast-to-sexp ((node ast-return-from))
  (list 'return-from
        (ast-return-from-name node)
        (ast-to-sexp (ast-return-from-value node))))

(defmethod ast-to-sexp ((node ast-tagbody))
  (let ((result (list 'tagbody)))
    (dolist (tag-entry (ast-tagbody-tags node))
      (push (car tag-entry) result)
      (dolist (form (cdr tag-entry))
        (push (ast-to-sexp form) result)))
    (nreverse result)))

(defmethod ast-to-sexp ((node ast-go))
  (list 'go (ast-go-tag node)))

(defmethod ast-to-sexp ((node ast-setq))
  (list 'setq
        (ast-setq-var node)
        (ast-to-sexp (ast-setq-value node))))

(defun %ast-local-fn-binding-to-sexp (binding)
  "Convert an flet/labels binding (name params . body-asts) to sexp form."
  (list* (first binding) (second binding)
         (mapcar #'ast-to-sexp (cddr binding))))

(defmethod ast-to-sexp ((node ast-flet))
  (list* 'flet
         (mapcar #'%ast-local-fn-binding-to-sexp (ast-flet-bindings node))
         (mapcar #'ast-to-sexp (ast-flet-body node))))

(defmethod ast-to-sexp ((node ast-labels))
  (list* 'labels
         (mapcar #'%ast-local-fn-binding-to-sexp (ast-labels-bindings node))
         (mapcar #'ast-to-sexp (ast-labels-body node))))

(defmethod ast-to-sexp ((node ast-defun))
  (list* 'defun
         (ast-defun-name node)
         (%ast-callable-lambda-list (ast-defun-params node)
                                    (ast-defun-optional-params node)
                                    (ast-defun-rest-param node)
                                    (ast-defun-key-params node))
         (append (ast-defun-declarations node)
                 (mapcar #'ast-to-sexp (ast-defun-body node)))))

(defmethod ast-to-sexp ((node ast-defvar))
  (if (ast-defvar-value node)
      (list 'defvar
            (ast-defvar-name node)
            (ast-to-sexp (ast-defvar-value node)))
      (list 'defvar
            (ast-defvar-name node))))

(defmethod ast-to-sexp ((node ast-defmacro))
  (list* 'defmacro
         (ast-defmacro-name node)
         (ast-defmacro-lambda-list node)
         (ast-defmacro-body node)))

(defmethod ast-to-sexp ((node ast-multiple-value-call))
  (list* 'multiple-value-call
         (ast-to-sexp (ast-mv-call-func node))
         (mapcar #'ast-to-sexp (ast-mv-call-args node))))

(defmethod ast-to-sexp ((node ast-multiple-value-prog1))
  (list* 'multiple-value-prog1
         (ast-to-sexp (ast-mv-prog1-first node))
         (mapcar #'ast-to-sexp (ast-mv-prog1-forms node))))

(defmethod ast-to-sexp ((node ast-values))
  (cons 'values (mapcar #'ast-to-sexp (ast-values-forms node))))

(defmethod ast-to-sexp ((node ast-multiple-value-bind))
  (list* 'multiple-value-bind
         (ast-mvb-vars node)
         (ast-to-sexp (ast-mvb-values-form node))
         (mapcar #'ast-to-sexp (ast-mvb-body node))))

(defmethod ast-to-sexp ((node ast-apply))
  (list* 'apply
         (ast-to-sexp (ast-apply-func node))
         (mapcar #'ast-to-sexp (ast-apply-args node))))

(defmethod ast-to-sexp ((node ast-catch))
  (list* 'catch
         (ast-to-sexp (ast-catch-tag node))
         (mapcar #'ast-to-sexp (ast-catch-body node))))

(defmethod ast-to-sexp ((node ast-throw))
  (list 'throw
        (ast-to-sexp (ast-throw-tag node))
        (ast-to-sexp (ast-throw-value node))))

(defmethod ast-to-sexp ((node ast-unwind-protect))
  (list* 'unwind-protect
         (ast-to-sexp (ast-unwind-protected node))
         (mapcar #'ast-to-sexp (ast-unwind-cleanup node))))

(defmethod ast-to-sexp ((node ast-handler-case))
  (list* 'handler-case
         (ast-to-sexp (ast-handler-case-form node))
         (mapcar (lambda (clause)
                   (let ((error-type (first clause))
                         (var (second clause))
                         (body (cddr clause)))
                     (list* error-type
                            (if var (list var) nil)
                            (mapcar #'ast-to-sexp body))))
                 (ast-handler-case-clauses node))))

(defmethod ast-to-sexp ((node ast-call))
  (let ((func (ast-call-func node))
        (args (mapcar #'ast-to-sexp (ast-call-args node))))
    (if (typep func 'ast-node)
        (cons (ast-to-sexp func) args)
        (cons func args))))

(defmethod ast-to-sexp ((node ast-quote))
  (list 'quote (ast-quote-value node)))

(defmethod ast-to-sexp ((node ast-the))
  (list 'the
        (ast-the-type node)
        (ast-to-sexp (ast-the-value node))))

;;; CLOS AST to S-expression roundtrip

(defparameter *slot-option-readers*
  '((:initarg  ast-slot-initarg  nil)
    (:initform ast-slot-initform t)    ; initform needs ast-to-sexp
    (:reader   ast-slot-reader   nil)
    (:writer   ast-slot-writer   nil)
    (:accessor ast-slot-accessor nil)
    (:type     ast-slot-type     nil))
  "Slot option (keyword reader-fn sexp-p) specs for slot-def-to-sexp.")

(defun slot-def-to-sexp (slot)
  "Convert an ast-slot-def back to a slot specification s-expression."
  (let ((opts (loop for (keyword reader sexp-p) in *slot-option-readers*
                    for value = (funcall reader slot)
                    when value
                    nconc (list keyword (if sexp-p (ast-to-sexp value) value)))))
    (when (and (ast-slot-allocation slot)
               (not (eq (ast-slot-allocation slot) :instance)))
      (setf opts (append opts (list :allocation (ast-slot-allocation slot)))))
    (if opts (cons (ast-slot-name slot) opts) (ast-slot-name slot))))

(defmethod ast-to-sexp ((node ast-defclass))
  (list* 'defclass
         (ast-defclass-name node)
         (ast-defclass-superclasses node)
         (list (mapcar #'slot-def-to-sexp (ast-defclass-slots node)))))

(defmethod ast-to-sexp ((node ast-defgeneric))
  (list 'defgeneric
        (ast-defgeneric-name node)
        (ast-defgeneric-params node)))

(defmethod ast-to-sexp ((node ast-defmethod))
  (let ((params (loop for name in (ast-defmethod-params node)
                      for spec in (ast-defmethod-specializers node)
                      collect (if spec (list name (cdr spec)) name))))
    (list* 'defmethod (ast-defmethod-name node)
           (append (when (ast-defmethod-qualifier node)
                     (list (ast-defmethod-qualifier node)))
                   (list params)
                   (mapcar #'ast-to-sexp (ast-defmethod-body node))))))

(defmethod ast-to-sexp ((node ast-make-instance))
  (list* 'make-instance
         (ast-to-sexp (ast-make-instance-class node))
         (loop for (key . val-ast) in (ast-make-instance-initargs node)
               nconc (list key (ast-to-sexp val-ast)))))

(defmethod ast-to-sexp ((node ast-slot-value))
  (list 'slot-value
        (ast-to-sexp (ast-slot-value-object node))
        (list 'quote (ast-slot-value-slot node))))

(defmethod ast-to-sexp ((node ast-set-slot-value))
  (list 'setf
        (list 'slot-value
              (ast-to-sexp (ast-set-slot-value-object node))
              (list 'quote (ast-set-slot-value-slot node)))
        (ast-to-sexp (ast-set-slot-value-value node))))

(defmethod ast-to-sexp ((node ast-set-gethash))
  (list 'setf
        (list 'gethash
              (ast-to-sexp (ast-set-gethash-key node))
              (ast-to-sexp (ast-set-gethash-table node)))
        (ast-to-sexp (ast-set-gethash-value node))))
