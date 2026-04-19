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


(defparameter *early-selfhost-macro-bridge-entries*
  '(("PARSE-LAMBDA-LIST"            . :cl-cc/expand)
    ("DESTRUCTURE-LAMBDA-LIST"      . :cl-cc/expand)
    ("GENERATE-LAMBDA-BINDINGS"     . :cl-cc/expand)
    ("LAMBDA-LIST-INFO-ENVIRONMENT" . :cl-cc/expand))
  "Bridge helpers required before selfhost macro evaluation switches to OUR-EVAL.")

(defparameter *cps-native-compile-safe-ast-types*
  '(cl-cc/ast:ast-int
    cl-cc/ast:ast-var
    cl-cc/ast:ast-quote
    cl-cc/ast:ast-the
    cl-cc/ast:ast-binop
    cl-cc/ast:ast-if
    cl-cc/ast:ast-progn
    cl-cc/ast:ast-let
    cl-cc/ast:ast-print)
  "AST node types allowed on the narrow CPS-backed native compile path.
The allowlist is intentionally small and limited to forms with existing CPS
semantic coverage so native compilation can consume CPS without widening the
failure surface across the VM/selfhost pipeline.")
