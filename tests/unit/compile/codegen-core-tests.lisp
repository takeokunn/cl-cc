;;;; tests/unit/compile/codegen-core-tests.lisp — Codegen core tests

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

(deftest codegen-if-compilation
  "Compiling an if-form emits vm-jump-zero and returns a register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-if :cond (make-ast-var :name t)
                                        :then (make-ast-int :value 1)
                                        :else (make-ast-int :value 2))
                           ctx)))
    (assert-true (keywordp reg))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-jump-zero))))

(deftest codegen-progn-compilation
  "Compiling a progn returns a register and emits instructions for sub-forms."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-progn
                             :forms (list (make-ast-int :value 1)
                                          (make-ast-int :value 2)
                                          (make-ast-int :value 3)))
                           ctx))
         (consts (remove-if-not (lambda (i) (typep i 'cl-cc::vm-const))
                                (codegen-instructions ctx))))
    (assert-true (keywordp reg))
    (assert-= 3 (length consts))))

(deftest codegen-let-compilation
  "Compiling a let returns a register and emits a move for the bound variable."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-let
                             :bindings (list (cons 'x (make-ast-int :value 42)))
                             :body (list (make-ast-var :name 'x)))
                           ctx)))
    (assert-true (keywordp reg))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-const))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-move))))

(deftest codegen-setq-local-emits-move
  "Compiling setq on a local variable emits a vm-move."
  (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc::ctx-env ctx) (list (cons 'x :R0)))
    (compile-ast (make-ast-setq :var 'x :value (make-ast-int :value 99)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-move))))

(deftest codegen-print-emits-vm-print
  "Compiling print emits vm-print."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-print :expr (make-ast-int :value 42)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-print))))

(deftest codegen-the-compiles-inner
  "Compiling (the integer ...) emits the inner assertion path."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc:make-ast-the :type 'integer
                                :value (make-ast-int :value 42))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-typep))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-signal-error))))

(deftest codegen-if-narrows-branch-type-env
  "Type guards narrow the then branch so proven vars skip redundant assertions."
  (let* ((ctx (make-codegen-ctx))
         (ast (cl-cc::lower-sexp-to-ast
               '(if (numberp x) (the fixnum x) 0))))
    (setf (cl-cc::ctx-env ctx) (list (cons 'x :R0)))
    (compile-ast ast ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-typep))
    (assert-null (codegen-find-inst ctx 'cl-cc::vm-signal-error))))
