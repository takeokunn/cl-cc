;;;; frontend/ast.lisp - AST Node Definitions
;;;;
;;;; This module provides:
;;;; - AST node structs with source location tracking
;;;; - Source location utilities and error reporting
;;;; - AST condition type (ast-compilation-error)

(in-package :cl-cc)

;;; AST Base Struct with Source Location Tracking

(defstruct (ast-node (:conc-name ast-))
  "Base struct for all AST nodes with optional source location tracking."
  (source-file nil)
  (source-line nil)
  (source-column nil))

;;; Intermediate AST Structs (shared slot groups)

(defstruct (ast-callable (:include ast-node))
  "Intermediate struct for callable forms (lambda, defun)."
  (params nil :type list)
  (optional-params nil :type list)
  (rest-param nil)
  (key-params nil :type list)
  (declarations nil :type list)
  (body nil :type list))

(defstruct (ast-local-fns (:include ast-node))
  "Intermediate struct for local function binding forms (flet, labels)."
  (bindings nil :type list)
  (body nil :type list))

;;; Core AST Structs

(defstruct (ast-int (:include ast-node))
  "Integer literal AST node."
  (value nil))

(defstruct (ast-var (:include ast-node))
  "Variable reference AST node."
  (name nil))

(defstruct (ast-hole (:include ast-node))
  "Expression-level typed hole AST node for source symbol _.")

(defstruct (ast-binop (:include ast-node))
  "Binary operation AST node."
  (op nil)
  (lhs nil)
  (rhs nil))

(defstruct (ast-if (:include ast-node))
  "Conditional expression AST node."
  (cond nil)
  (then nil)
  (else nil))

(defstruct (ast-progn (:include ast-node))
  "Sequence of expressions AST node."
  (forms nil :type list))

(defstruct (ast-print (:include ast-node))
  "Print expression AST node."
  (expr nil))

(defstruct (ast-let (:include ast-node))
  "Let binding AST node."
  (bindings nil :type list)
  (declarations nil :type list)
  (body nil :type list))

;;; Function and Lambda AST Structs

(defstruct (ast-lambda (:include ast-callable))
  "Lambda expression AST node."
  (env nil))

(defstruct (ast-function (:include ast-node))
  "Function reference AST node for #'var."
  (name nil))

(defstruct (ast-flet (:include ast-local-fns))
  "Local non-recursive function bindings AST node.")

(defstruct (ast-labels (:include ast-local-fns))
  "Local recursive function bindings AST node.")

(defstruct (ast-defun (:include ast-callable))
  "Top-level function definition AST node."
  (name nil :type symbol))

(defstruct (ast-defvar (:include ast-node))
  "Top-level variable definition AST node (defvar/defparameter)."
  (name nil)
  (value nil)
  (kind 'defparameter)  ; 'defvar or 'defparameter — controls conditional-init semantics
  )

(defstruct (ast-defmacro (:include ast-node))
  "Top-level macro definition AST node."
  (name nil)
  (lambda-list nil)
  (body nil :type list))

;;; Block and Control Flow AST Structs

(defstruct (ast-block (:include ast-node))
  "Named block AST node."
  (name nil)
  (body nil :type list))

(defstruct (ast-return-from (:include ast-node))
  "Return from named block AST node."
  (name nil)
  (value nil))

(defstruct (ast-tagbody (:include ast-node))
  "Tagged body for goto-style control flow AST node."
  (tags nil :type list))

(defstruct (ast-go (:include ast-node))
  "Go to tag AST node."
  (tag nil))

;;; Assignment and Variables AST Structs

(defstruct (ast-setq (:include ast-node))
  "Variable assignment AST node."
  (var nil)
  (value nil))

;;; Multiple Values AST Structs

(defstruct (ast-multiple-value-call (:include ast-node) (:conc-name ast-mv-call-))
  "Multiple-value-call special form AST node."
  (func nil)
  (args nil :type list))

(defstruct (ast-multiple-value-prog1 (:include ast-node) (:conc-name ast-mv-prog1-))
  "Multiple-value-prog1 special form AST node."
  (first nil)
  (forms nil :type list))

(defstruct (ast-values (:include ast-node))
  "Values form AST node."
  (forms nil :type list))

(defstruct (ast-multiple-value-bind (:include ast-node) (:conc-name ast-mvb-))
  "Multiple-value-bind AST node."
  (vars nil :type list)
  (values-form nil)
  (body nil :type list))

(defstruct (ast-apply (:include ast-node))
  "Apply form AST node."
  (func nil)
  (args nil :type list))

;;; Exception Handling AST Structs

(defstruct (ast-catch (:include ast-node))
  "Catch block AST node."
  (tag nil)
  (body nil :type list))

(defstruct (ast-throw (:include ast-node))
  "Throw AST node."
  (tag nil)
  (value nil))

(defstruct (ast-unwind-protect (:include ast-node) (:conc-name ast-unwind-))
  "Unwind-protect AST node."
  (protected nil)
  (cleanup nil :type list))

(defstruct (ast-handler-case (:include ast-node))
  "Handler-case AST node for condition handling."
  (form nil)
  (clauses nil :type list))

;;; Additional AST Structs for Common Operations

(defstruct (ast-call (:include ast-node))
  "Function call AST node (for non-special operator calls)."
  (func nil)
  (args nil :type list))

(defstruct (ast-quote (:include ast-node))
  "Quote special form AST node."
  (value nil))

(defstruct (ast-the (:include ast-node))
  "The type declaration AST node."
  (type nil)
  (value nil))

;;; CLOS AST Structs

(defstruct (ast-slot-def (:include ast-node) (:conc-name ast-slot-))
  "Slot definition for defclass."
  (name nil :type symbol)
  (initarg nil)
  (initform nil)
  (reader nil)
  (writer nil)
  (accessor nil)
  (type nil)
  (allocation :instance))

(defstruct (ast-defclass (:include ast-node))
  "Class definition AST node (defclass)."
  (name nil)
  (superclasses nil :type list)
  (slots nil :type list)
  (default-initargs nil :type list))

(defstruct (ast-defgeneric (:include ast-node))
  "Generic function definition AST node (defgeneric)."
  (name nil)
  (params nil :type list)
  (combination nil))

(defstruct (ast-defmethod (:include ast-node))
  "Method definition AST node (defmethod)."
  (name nil)
  (qualifier nil)
  (specializers nil :type list)
  (params nil :type list)
  (body nil :type list))

(defstruct (ast-make-instance (:include ast-node))
  "Object creation AST node (make-instance)."
  (class nil)
  (initargs nil :type list))

(defstruct (ast-slot-value (:include ast-node))
  "Slot access AST node (slot-value)."
  (object nil)
  (slot nil))

(defstruct (ast-set-slot-value (:include ast-node))
  "Slot mutation AST node (setf (slot-value obj 'slot) val)."
  (object nil)
  (slot nil)
  (value nil))

(defstruct (ast-set-gethash (:include ast-node))
  "Hash table mutation AST node (setf (gethash key table) value)."
  (key nil)
  (table nil)
  (value nil))

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

;;; Source Location Utilities

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
