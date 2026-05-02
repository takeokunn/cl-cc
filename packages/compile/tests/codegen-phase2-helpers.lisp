;;;; tests/unit/compile/codegen-phase2-helpers.lisp — Shared helpers for phase 2 codegen tests

(in-package :cl-cc/test)

(defun codegen-count-inst (ctx type)
  "Count instructions of TYPE emitted into CTX."
  (count-if (lambda (i) (typep i type)) (codegen-instructions ctx)))

(defun make-call (func &rest arg-forms)
  "Build an ast-call node. FUNC is a symbol; ARG-FORMS are already-built AST nodes."
  (make-ast-call :func func :args arg-forms))

(defun make-int (n)
  (make-ast-int :value n))

(defun make-var (s)
  (make-ast-var :name s))

(defun make-quoted (v)
  (make-ast-quote :value v))

(defun make-fn (name)
  (make-ast-function :name name))

(defun make-ctx-with-vars (&rest names)
  "Create a codegen ctx with NAMES pre-bound to fresh registers.
Each name maps to its own unique register so compile-ast on ast-var
nodes referring to these names succeeds without signaling 'unbound variable'."
  (let ((ctx (make-codegen-ctx)))
    (dolist (name names)
      (let ((reg (cl-cc/compile:make-register ctx)))
        (push (cons name reg) (cl-cc/compile:ctx-env ctx))))
    ctx))
