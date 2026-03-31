;;;; tests/unit/compile/codegen-tests.lisp — Codegen Unit Tests
;;;
;;; Direct tests for compile-ast methods and codegen helpers.
;;; Tests compile individual AST nodes to VM instructions without
;;; going through the full pipeline (parser + macro expander).

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── Helpers ────────────────────────────────────────────────────────────

(defun make-codegen-ctx ()
  "Create a fresh compiler context for codegen tests."
  (make-instance 'cl-cc::compiler-context))

(defun codegen-instructions (ctx)
  "Return the emitted instructions from CTX (in order)."
  (nreverse (copy-list (cl-cc::ctx-instructions ctx))))

(defun codegen-find-inst (ctx type)
  "Find the first instruction of TYPE in CTX's emitted instructions."
  (find-if (lambda (i) (typep i type)) (codegen-instructions ctx)))

;;; ─── compile-ast: ast-int ───────────────────────────────────────────────

(deftest codegen-int-returns-register
  "Compiling an integer literal returns a register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-int :value 42) ctx)))
    (assert-true (keywordp reg))))

(deftest-each codegen-int-emits-const-value
  "Compiling an integer literal emits vm-const with the correct value."
  :cases (("positive"  42)
          ("zero"       0)
          ("negative"  -1))
  (n)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-int :value n) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-= n (cl-cc::vm-const-value inst)))))

;;; ─── compile-ast: ast-var ───────────────────────────────────────────────

(deftest-each codegen-var-constant-emits-const
  "Compiling T, NIL, or a keyword emits vm-const with that exact value."
  :cases (("t"       t)
          ("nil"     nil)
          ("keyword" :foo))
  (name)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-var :name name) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-equal name (cl-cc::vm-const-value inst)))))

(deftest codegen-var-local-returns-register
  "Compiling a local variable returns its bound register."
  (let* ((ctx (make-codegen-ctx))
         (reg :R99))
    (setf (cl-cc::ctx-env ctx) (list (cons 'x reg)))
    (let ((result (compile-ast (make-ast-var :name 'x) ctx)))
      (assert-eq reg result))))

(deftest codegen-var-unbound-signals-error
  "Compiling an unbound variable signals an error."
  (let ((ctx (make-codegen-ctx)))
    (assert-signals error
      (compile-ast (make-ast-var :name 'nonexistent-var-xyz) ctx))))

;;; ─── compile-ast: ast-quote ─────────────────────────────────────────────

(deftest-each codegen-quote-forms-emit-const-value
  "Compiling a quoted form emits vm-const with the exact value for all literal types."
  :cases (("symbol"    'hello)
          ("list"      '(1 2 3))
          ("nil"       nil)
          ("string"    "hello")
          ("empty-str" "")
          ("char"      #\a))
  (datum)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-quote :value datum) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-equal datum (cl-cc::vm-const-value inst)))))

;;; ─── compile-ast: ast-binop ─────────────────────────────────────────────

(deftest-each codegen-binop-emits-correct-instruction
  "Each binary operator compiles to its corresponding VM instruction."
  :cases (("add" '+ 'cl-cc::vm-add)
          ("sub" '- 'cl-cc::vm-sub)
          ("mul" '* 'cl-cc::vm-mul)
          ("lt"  '< 'cl-cc::vm-lt)
          ("gt"  '> 'cl-cc::vm-gt)
          ("eq"  '= 'cl-cc::vm-num-eq))
  (op inst-type)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-binop :op op
                                  :lhs (make-ast-int :value 1)
                                  :rhs (make-ast-int :value 2))
                 ctx)
    (assert-true (codegen-find-inst ctx inst-type))))

;;; ─── codegen helpers ─────────────────────────────────────────────────────

(deftest codegen-make-register-increments
  "make-register returns incrementing registers."
  (let* ((ctx (make-codegen-ctx))
         (r1 (cl-cc::make-register ctx))
         (r2 (cl-cc::make-register ctx)))
    (assert-true (keywordp r1))
    (assert-true (keywordp r2))
    (assert-false (eq r1 r2))))

(deftest codegen-emit-appends
  "emit adds instruction to context."
  (let ((ctx (make-codegen-ctx)))
    (cl-cc::emit ctx (cl-cc::make-vm-const :dst :R0 :value 42))
    (assert-= 1 (length (codegen-instructions ctx)))))

(deftest codegen-make-label-unique
  "make-label returns unique labels."
  (let* ((ctx (make-codegen-ctx))
         (l1 (cl-cc::make-label ctx "TEST"))
         (l2 (cl-cc::make-label ctx "TEST")))
    (assert-false (string= l1 l2))))


