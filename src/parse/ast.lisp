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
  (value nil))

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
  (type nil))

(defstruct (ast-defclass (:include ast-node))
  "Class definition AST node (defclass)."
  (name nil)
  (superclasses nil :type list)
  (slots nil :type list))

(defstruct (ast-defgeneric (:include ast-node))
  "Generic function definition AST node (defgeneric)."
  (name nil)
  (params nil :type list))

(defstruct (ast-defmethod (:include ast-node))
  "Method definition AST node (defmethod)."
  (name nil)
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
