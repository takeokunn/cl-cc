(in-package :cl-cc)
;;;; Pipeline — CPS compile helpers shared by VM/native entrypoints

(defparameter *cps-vm-compile-safe-ast-types*
  '(cl-cc/ast:ast-int
    cl-cc/ast:ast-var
    cl-cc/ast:ast-quote
    cl-cc/ast:ast-the
    cl-cc/ast:ast-binop
    cl-cc/ast:ast-if
    cl-cc/ast:ast-progn
    cl-cc/ast:ast-let
    cl-cc/ast:ast-setq
    cl-cc/ast:ast-print
    cl-cc/ast:ast-function
    cl-cc/ast:ast-lambda
    cl-cc/ast:ast-call
    cl-cc/ast:ast-apply
    cl-cc/ast:ast-values
    cl-cc/ast:ast-multiple-value-bind
    cl-cc/ast:ast-multiple-value-call
    cl-cc/ast:ast-multiple-value-prog1)
  "AST node types allowed on the CPS-backed VM compile path.
This intentionally favors expression forms whose CPS lowering is already backed
by dedicated functional transformers, including direct call-bearing forms.")

(defvar *enable-cps-vm-primary-path* t
  "When true, `compile-expression` prefers a CPS-backed VM compilation path for safe ASTs.")

 (defvar *compile-expression-cps-recursion-guard* nil
  "Internal guard to prevent `compile-expression` from recursively re-entering the CPS primary path.")

(defparameter *cps-native-compile-safe-ast-types*
  '(cl-cc/ast:ast-int
    cl-cc/ast:ast-var
    cl-cc/ast:ast-quote
    cl-cc/ast:ast-the
    cl-cc/ast:ast-binop
    cl-cc/ast:ast-if
    cl-cc/ast:ast-progn
    cl-cc/ast:ast-let
    cl-cc/ast:ast-setq
    cl-cc/ast:ast-print
    cl-cc/ast:ast-function
    cl-cc/ast:ast-lambda
    cl-cc/ast:ast-call
    cl-cc/ast:ast-apply
    cl-cc/ast:ast-values
    cl-cc/ast:ast-multiple-value-bind
    cl-cc/ast:ast-multiple-value-call
    cl-cc/ast:ast-multiple-value-prog1)
  "AST node types allowed on the CPS-backed native compile path.
This mirrors the VM subset today so both entrypoints share one readable policy
surface, while still allowing the two targets to diverge later if needed.")

(defun %cps-compile-safe-ast-p (ast allowed-types)
  "Return T when AST and all descendants stay within ALLOWED-TYPES."
  (and (typep ast 'cl-cc/ast:ast-node)
       (some (lambda (type) (typep ast type)) allowed-types)
       (every (lambda (child)
                (%cps-compile-safe-ast-p child allowed-types))
              (cl-cc/ast:ast-children ast))))

(defun %cps-vm-compile-safe-ast-p (ast)
  "Return T when AST and all descendants stay within the CPS-backed VM allowlist."
  (%cps-compile-safe-ast-p ast *cps-vm-compile-safe-ast-types*))

(defun %cps-native-compile-safe-ast-p (ast)
  "Return T when AST and all descendants stay within the CPS-backed native allowlist."
  (%cps-compile-safe-ast-p ast *cps-native-compile-safe-ast-types*))

(defun %cps-identity-entry-form (cps-form)
  "Wrap CPS-FORM with an identity continuation for VM/native compilation."
  (list cps-form '(lambda (value) value)))

(defun %cps-entry-form (cps-form)
  "Wrap CPS-FORM with an identity continuation for VM/native compilation."
  (%cps-identity-entry-form cps-form))
