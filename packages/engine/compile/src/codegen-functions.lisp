(in-package :cl-cc/compile)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Codegen — Function Compilation
;;;
;;; Contains: defmacro compilation plus a reference to the typed-parameter
;;; machinery (*function-type-registry*, lambda-list-has-typed-p,
;;; strip-typed-params, register-function-type) which is defined in
;;; expander-typed-params.lisp (expand package, loaded before compile).
;;;
;;; Load order: after codegen-clos, before codegen.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

;;; ── defmacro ─────────────────────────────────────────────────────────────

(defmethod compile-ast ((node ast-defmacro) ctx)
  "Compile a top-level macro definition.
Registers the macro expander at compile time so subsequent forms can use it.
At runtime, defmacro evaluates to the macro name."
  (setf (ctx-tail-position ctx) nil)
  (let ((name (ast-defmacro-name node))
        (lambda-list (ast-defmacro-lambda-list node))
        (body (ast-defmacro-body node)))
    (cl-cc/expand:register-macro name (cl-cc/expand:make-macro-expander lambda-list body))
    (let ((dst (make-register ctx)))
      (emit ctx (make-vm-const :dst dst :value name))
      dst)))

;;; Parameter-list helpers (allocate-defaulting-params, allocate-extended-params,
;;; rest-param-stack-alloc-p, emit-supplied-p-checks, emit-non-constant-defaults,
;;; build-all-param-bindings, function-param-type-bindings, compile-function-body)
;;; are in codegen-functions-params.lisp (loads next).


;;; (%emit-closure-body, compile-ast for ast-lambda/ast-defun/ast-defvar
;;;  are in codegen-functions-emit.lisp which loads after this file.)
