;;;; tests/unit/compile/context-tests.lisp — Compiler Context Unit Tests
;;;;
;;;; Tests for compiler-context: register allocation, label generation,
;;;; emit, variable lookup, builtin special variables initialization.

(in-package :cl-cc/test)

(defsuite context-suite :description "Compiler context unit tests")

;;; ─── make-register ──────────────────────────────────────────────────────────

(deftest ctx-make-register-keyword
  "make-register returns a keyword like :R0."
  (let* ((ctx (make-instance 'cl-cc::compiler-context))
         (reg (cl-cc::make-register ctx)))
    (assert-true (keywordp reg))
    (assert-equal :R0 reg)))

(deftest ctx-make-register-increments
  "Successive make-register calls yield :R0, :R1, :R2, ..."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (assert-equal :R0 (cl-cc::make-register ctx))
    (assert-equal :R1 (cl-cc::make-register ctx))
    (assert-equal :R2 (cl-cc::make-register ctx))))

(deftest ctx-make-register-counter-state
  "make-register increments ctx-next-register."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (assert-equal 0 (cl-cc::ctx-next-register ctx))
    (cl-cc::make-register ctx)
    (assert-equal 1 (cl-cc::ctx-next-register ctx))))

;;; ─── make-label ─────────────────────────────────────────────────────────────

(deftest ctx-make-label-format
  "make-label returns PREFIX_N string."
  (let* ((ctx (make-instance 'cl-cc::compiler-context))
         (lbl (cl-cc::make-label ctx "IF")))
    (assert-true (stringp lbl))
    (assert-equal "IF_0" lbl)))

(deftest ctx-make-label-increments
  "Successive make-label calls yield incrementing indices."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (assert-equal "L_0" (cl-cc::make-label ctx "L"))
    (assert-equal "L_1" (cl-cc::make-label ctx "L"))))

(deftest ctx-make-label-different-prefixes
  "Different prefixes share the same counter."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (assert-equal "IF_0"   (cl-cc::make-label ctx "IF"))
    (assert-equal "ELSE_1" (cl-cc::make-label ctx "ELSE"))
    (assert-equal "END_2"  (cl-cc::make-label ctx "END"))))

;;; ─── emit ───────────────────────────────────────────────────────────────────

(deftest ctx-emit-pushes-instruction
  "emit pushes an instruction onto ctx-instructions."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (cl-cc::emit ctx :fake-instruction)
    (assert-equal 1 (length (cl-cc::ctx-instructions ctx)))
    (assert-eq :fake-instruction (first (cl-cc::ctx-instructions ctx)))))

(deftest ctx-emit-returns-instruction
  "emit returns the instruction it was given."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (assert-eq :test (cl-cc::emit ctx :test))))

(deftest ctx-emit-stack-order
  "emit uses LIFO order (most recent first)."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (cl-cc::emit ctx :first)
    (cl-cc::emit ctx :second)
    (assert-eq :second (first (cl-cc::ctx-instructions ctx)))
    (assert-eq :first (second (cl-cc::ctx-instructions ctx)))))

;;; ─── lookup-var ─────────────────────────────────────────────────────────────

(deftest ctx-lookup-var-found
  "lookup-var returns register when variable is in env."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (push (cons 'x :R0) (cl-cc::ctx-env ctx))
    (assert-eq :R0 (cl-cc::lookup-var ctx 'x))))

(deftest ctx-lookup-var-unbound-error
  "lookup-var signals error for unbound variable."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (assert-signals error (cl-cc::lookup-var ctx 'nonexistent))))

(deftest ctx-lookup-var-shadowing
  "lookup-var returns most recent binding (shadowing)."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (push (cons 'x :R0) (cl-cc::ctx-env ctx))
    (push (cons 'x :R5) (cl-cc::ctx-env ctx))
    (assert-eq :R5 (cl-cc::lookup-var ctx 'x))))

;;; ─── initialize-instance ────────────────────────────────────────────────────

(deftest ctx-init-builtin-specials
  "*builtin-special-variables* are pre-registered in global-variables."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (assert-true (gethash '*standard-output* (cl-cc::ctx-global-variables ctx)))
    (assert-true (gethash '*standard-input* (cl-cc::ctx-global-variables ctx)))
    (assert-true (gethash '*features* (cl-cc::ctx-global-variables ctx)))))

(deftest ctx-init-starts-top-level
  "Fresh context starts at top-level."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (assert-true (cl-cc::ctx-top-level-p ctx))))

(deftest ctx-init-empty-instructions
  "Fresh context has no instructions."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (assert-null (cl-cc::ctx-instructions ctx))))

(deftest ctx-init-empty-env
  "Fresh context has empty variable environment."
  (let ((ctx (make-instance 'cl-cc::compiler-context)))
    (assert-null (cl-cc::ctx-env ctx))))

;;; ─── REPL state ─────────────────────────────────────────────────────────────

(deftest ctx-repl-label-counter-continues
  "*repl-label-counter* continues label numbering."
  (let ((cl-cc::*repl-label-counter* 100))
    (let ((ctx (make-instance 'cl-cc::compiler-context)))
      (assert-equal 100 (cl-cc::ctx-next-label ctx))
      (assert-equal "L_100" (cl-cc::make-label ctx "L")))))

(deftest ctx-repl-globals-merged
  "*repl-global-variables* are merged into context."
  (let ((cl-cc::*repl-global-variables* (make-hash-table :test #'eq)))
    (setf (gethash 'my-var cl-cc::*repl-global-variables*) t)
    (let ((ctx (make-instance 'cl-cc::compiler-context)))
      (assert-true (gethash 'my-var (cl-cc::ctx-global-variables ctx))))))
