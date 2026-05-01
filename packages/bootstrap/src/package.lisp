(cl:in-package :cl-user)

;;;; packages/bootstrap/src/package.lisp — :cl-cc/bootstrap
;;;;
;;;; Phase 2 prerequisite: the 12 symbols that must be interned before
;;;; cl-cc/prolog and cl-cc/compile load.
;;;;
;;;; Why a separate package?
;;;;   cl-cc/prolog uses binop/const/var/cmp/... as Prolog predicate atoms
;;;;   in the solver/query layer and as the DCG token bridge (dcg.lisp).
;;;;   cl-cc/compile defines our-eval; cl-cc/prolog calls it back at runtime.
;;;;   Without a common bootstrap both subsystems would need to import from
;;;;   :cl-cc, which loads *after* prolog — creating a circular dependency.
;;;;
;;;; Consumers:
;;;;   cl-cc/prolog   — (:use :cl :cl-cc/bootstrap)
;;;;   cl-cc/compile  — (:use :cl ... :cl-cc/bootstrap)  [defines our-eval, our-load here]
;;;;   cl-cc/parse    — (:use :cl ... :cl-cc/bootstrap)  [defines lexer-token-* here]
;;;;   cl-cc/expand   — (:use :cl :cl-cc/bootstrap)       [references our-eval, our-load, run-string-repl]
;;;;   cl-cc          — (:use ... :cl-cc/bootstrap)       [re-exports all]

(defpackage :cl-cc/bootstrap
  (:use :cl)
  (:export
   ;; Compiler re-entry point — defined in cl-cc/compile, called by cl-cc/prolog
   #:our-eval
   ;; REPL entry points — defined in cl-cc/compile; referenced in cl-cc/expand macro templates
   ;; Must live in bootstrap so expand can reference them before compile loads.
   #:our-load
   #:run-string-repl
    ;; VM bootstrap installers — defined in cl-cc/vm, consumed by runtime/parse/expand/selfhost
    #:*vm-runtime-callable-installer*
    #:*runtime-vm-callable-register-hook*
    #:*runtime-package-registry-provider*
    #:*runtime-find-package-fn*
    #:*runtime-intern-fn*
    #:*runtime-set-symbol-value-fn*
    #:*vm-eval-hook-installer*
    #:*vm-macroexpand-hook-installer*
    #:*vm-parse-forms-hook-installer*
   ;; Prolog type/relation predicate atoms (keys in *prolog-rules* fact DB)
   #:binop #:const #:var #:cmp
   #:integer-type #:boolean-type #:env-lookup
   ;; CST token bridge — defined in cl-cc/parse, referenced in DCG rules
   #:make-cst-token
   #:lexer-token-p #:lexer-token-type #:lexer-token-value
   ;; Quasiquote reader symbols — produced by cst.lisp, consumed by macro.lisp
   ;; Both cl-cc/parse and cl-cc use bootstrap, so they share the same symbol objects.
   #:backquote #:unquote #:unquote-splicing
   ;; Runtime plist helper — used by cl-cc/parse (lower.lisp) and cl-cc/vm (vm-bridge.lisp)
   ;; Must live in bootstrap so both packages share the same symbol without conflict.
   #:rt-plist-put))

(in-package :cl-cc/bootstrap)

(defvar *vm-runtime-callable-installer* nil)
(defvar *runtime-vm-callable-register-hook* nil)
(defvar *runtime-package-registry-provider* nil)
(defvar *runtime-find-package-fn* nil)
(defvar *runtime-intern-fn* nil)
(defvar *runtime-set-symbol-value-fn* nil)
(defvar *vm-eval-hook-installer* nil)
(defvar *vm-macroexpand-hook-installer* nil)
(defvar *vm-parse-forms-hook-installer* nil)

(defun rt-plist-put (plist indicator value)
  "Return a new plist with INDICATOR set to VALUE. Non-destructive."
  (let ((result nil) (found nil) (p plist))
    (loop while p do
          (let ((k (car p)))
            (if (eq k indicator)
                (progn (push indicator result) (push value result) (setf found t))
                (progn (push k result) (push (cadr p) result)))
            (setf p (cddr p))))
    (unless found
      (push indicator result) (push value result))
    (nreverse result)))
