(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Compile — Pipeline Policy Data
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defparameter *cps-host-eval-unsafe-ast-types*
  '(cl-cc/ast::ast-defun
    cl-cc/ast::ast-defvar
    cl-cc/ast::ast-defclass
    cl-cc/ast::ast-defgeneric
    cl-cc/ast::ast-defmethod
    cl-cc/ast::ast-flet
    cl-cc/ast::ast-labels
    cl-cc/ast::ast-block
    cl-cc/ast::ast-return-from
    cl-cc/ast::ast-catch
    cl-cc/ast::ast-throw
    cl-cc/ast::ast-tagbody
    cl-cc/ast::ast-go
    cl-cc/ast::ast-unwind-protect
    cl-cc/ast::ast-handler-case)
  "AST node types whose side effects require the full compile→VM pipeline.
Definition forms, non-local exits, and condition handling must not short-circuit
through the host CPS fast path.")


;;; NOTE: CPS compile allowlists now live in pipeline-cps.lisp so the shared
;;; VM/native policy surface stays in one place.
