;;;; tests/unit/compile/context-tests.lisp — Compiler Context Unit Tests
;;;;
;;;; Tests for compiler-context: register allocation, label generation,
;;;; emit, variable lookup, builtin special variables initialization.

(in-package :cl-cc/test)

(defsuite context-suite :description "Compiler context unit tests"
  :parent cl-cc-suite)


(in-suite context-suite)
;;; ─── make-register ──────────────────────────────────────────────────────────

(deftest ctx-make-register-behavior
  "make-register: first is :R0 keyword; successive calls increment; counter state tracks."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    ;; first result is :R0, a keyword
    (let ((reg (cl-cc::make-register ctx)))
      (assert-true (keywordp reg))
      (assert-equal :R0 reg))
    ;; subsequent calls yield :R1, :R2
    (assert-equal :R1 (cl-cc::make-register ctx))
    (assert-equal :R2 (cl-cc::make-register ctx)))
  ;; counter state increments on each call
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (assert-equal 0 (cl-cc::ctx-next-register ctx))
    (cl-cc::make-register ctx)
    (assert-equal 1 (cl-cc::ctx-next-register ctx))))

;;; ─── make-label ─────────────────────────────────────────────────────────────

(deftest ctx-make-label-behavior
  "make-label: PREFIX_N format; successive calls increment; all prefixes share one counter."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    ;; format: string with prefix and index
    (let ((lbl (cl-cc::make-label ctx "IF")))
      (assert-true (stringp lbl))
      (assert-equal "IF_0" lbl))
    ;; same prefix increments
    (assert-equal "IF_1" (cl-cc::make-label ctx "IF")))
  ;; different prefixes share counter
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (assert-equal "IF_0"   (cl-cc::make-label ctx "IF"))
    (assert-equal "ELSE_1" (cl-cc::make-label ctx "ELSE"))
    (assert-equal "END_2"  (cl-cc::make-label ctx "END"))))

;;; ─── emit ───────────────────────────────────────────────────────────────────

(deftest ctx-emit-behavior
  "emit: pushes instruction, returns it, and uses LIFO order."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    ;; push + return value
    (assert-eq :test (cl-cc::emit ctx :test))
    (assert-equal 1 (length (cl-cc::ctx-instructions ctx)))
    (assert-eq :test (first (cl-cc::ctx-instructions ctx)))
    ;; LIFO: second emit becomes first in list
    (cl-cc::emit ctx :second)
    (assert-eq :second (first (cl-cc::ctx-instructions ctx)))
    (assert-eq :test   (second (cl-cc::ctx-instructions ctx)))))

;;; ─── lookup-var ─────────────────────────────────────────────────────────────

(deftest ctx-lookup-var-behavior
  "lookup-var: returns register when found; signals error for unbound; returns most recent binding when shadowed."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (push (cons 'x :R0) (cl-cc::ctx-env ctx))
    (assert-eq :R0 (cl-cc::lookup-var ctx 'x)))
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (assert-signals error (cl-cc::lookup-var ctx 'nonexistent)))
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (push (cons 'x :R0) (cl-cc::ctx-env ctx))
    (push (cons 'x :R5) (cl-cc::ctx-env ctx))
    (assert-eq :R5 (cl-cc::lookup-var ctx 'x))))

;;; ─── initialize-instance ────────────────────────────────────────────────────

(deftest ctx-initialization
  "Fresh context: top-level=T; no instructions; empty env; builtin specials pre-registered."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (assert-true (cl-cc::ctx-top-level-p ctx))
    (assert-null (cl-cc::ctx-instructions ctx))
    (assert-null (cl-cc::ctx-env ctx))
    (assert-true (typep (cl-cc::ctx-type-env ctx) 'cl-cc/type:type-env))
    (assert-null (cl-cc/type::type-env-bindings (cl-cc::ctx-type-env ctx)))
    (assert-true (gethash '*standard-output* (cl-cc::ctx-global-variables ctx)))
    (assert-true (gethash '*standard-input* (cl-cc::ctx-global-variables ctx)))
    (assert-true (gethash '*features* (cl-cc::ctx-global-variables ctx)))
    (assert-true (gethash '*package* (cl-cc::ctx-global-variables ctx)))))

;;; ─── REPL state ─────────────────────────────────────────────────────────────

(deftest ctx-repl-state-persistence
  "REPL state: *repl-label-counter* continues label numbering; *repl-global-variables* are merged into context."
  (let ((cl-cc::*repl-label-counter* 100))
    (let ((ctx (make-instance 'cl-cc::compiler-context)))
      (assert-equal 100 (cl-cc::ctx-next-label ctx))
      (assert-equal "L_100" (cl-cc::make-label ctx "L"))))
  (let ((cl-cc::*repl-global-variables* (make-hash-table :test #'eq)))
    (setf (gethash 'my-var cl-cc::*repl-global-variables*) t)
    (let ((ctx (make-instance 'cl-cc::compiler-context)))
      (assert-true (gethash 'my-var (cl-cc::ctx-global-variables ctx))))))
