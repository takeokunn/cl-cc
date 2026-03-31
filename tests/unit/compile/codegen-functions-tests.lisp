;;;; tests/unit/compile/codegen-functions-tests.lisp — Codegen function/call unit tests

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

(deftest codegen-function-ref-returns-register
  "Compiling #'fn returns a register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-function :name 'car) ctx)))
    (assert-true (keywordp reg))))

(deftest-each codegen-closure-form-emits-vm-closure
  "Every closure-creating form (defun, lambda, flet, labels) emits vm-closure."
  :cases (("defun"  (cl-cc::make-ast-defun
                      :name 'my-fn :params '(x)
                      :body (list (make-ast-var :name 'x))))
          ("lambda" (make-ast-lambda
                      :params '(x)
                      :body (list (make-ast-var :name 'x))))
          ("flet"   (cl-cc::make-ast-flet
                      :bindings (list (list 'f '(x) (make-ast-var :name 'x)))
                      :body (list (make-ast-call :func 'f
                                                  :args (list (make-ast-int :value 1))))))
          ("labels" (cl-cc::make-ast-labels
                      :bindings (list (list 'g '(x) (make-ast-var :name 'x)))
                      :body (list (make-ast-call :func 'g
                                                  :args (list (make-ast-int :value 2)))))))
  (ast)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast ast ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-closure))))

(deftest codegen-defun-registers-global
  "Compiling defun registers the function name in global-functions."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-defun :name 'my-fn
                                  :params '(x)
                                  :body (list (make-ast-var :name 'x)))
                 ctx)
    (assert-true (gethash 'my-fn (cl-cc::ctx-global-functions ctx)))))

(deftest codegen-defvar-compilation
  "Compiling defvar registers in global-variables and emits vm-const for the value."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-defvar :name 'test-codegen-var
                                   :value (make-ast-int :value 99))
                 ctx)
    (assert-true (gethash 'test-codegen-var (cl-cc::ctx-global-variables ctx)))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-const))))

(deftest codegen-apply-compilation
  "Compiling ast-apply emits vm-apply and returns a register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc::make-ast-apply
                              :func (make-ast-function :name '+)
                              :args (list (make-ast-quote :value '(1 2 3))))
                            ctx)))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-apply))
    (assert-true (keywordp reg))))

(deftest-each codegen-apply-run
  "apply spreads list arguments to a function."
  :cases (("list-only"     6  "(apply #'+ '(1 2 3))")
          ("leading-args"  10 "(apply #'+ 1 2 '(3 4))"))
  (expected code)
  (assert-run= expected code))

(deftest codegen-lambda-returns-register
  "Compiling lambda returns a register holding the closure."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-lambda :params '(x)
                                             :body (list (make-ast-int :value 1)))
                           ctx)))
    (assert-true (keywordp reg))))

(deftest codegen-call-known-function
  "Compiling a call to a known function emits instructions."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'cons
                                 :args (list (make-ast-int :value 1)
                                             (make-ast-int :value 2)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-cons))))

(deftest-each codegen-call-list-accessor-emits-instruction
  "Calling car or cdr on a list emits the corresponding VM accessor instruction."
  :cases (("car" 'car 'cl-cc::vm-car)
          ("cdr" 'cdr 'cl-cc::vm-cdr))
  (func inst-type)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func func
                                 :args (list (make-ast-quote :value '(1 2))))
                 ctx)
    (assert-true (codegen-find-inst ctx inst-type))))

(deftest resolve-func-sym-global-emits-const
  "%resolve-func-sym-reg for a global symbol emits vm-const with that symbol."
  (let ((ctx (make-codegen-ctx)))
    (cl-cc::%resolve-func-sym-reg 'foo ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-eq 'foo (cl-cc::vm-const-value inst)))))

(deftest resolve-func-sym-local-returns-env-reg
  "%resolve-func-sym-reg returns the local env register when symbol is in env."
  (let* ((ctx (make-codegen-ctx))
         (local-reg :r42))
    (push (cons 'my-fn local-reg) (cl-cc::ctx-env ctx))
    (let ((result (cl-cc::%resolve-func-sym-reg 'my-fn ctx)))
      (assert-eq local-reg result))))

(deftest resolve-func-sym-local-no-const-emitted
  "%resolve-func-sym-reg for local var emits no vm-const."
  (let* ((ctx (make-codegen-ctx))
         (local-reg :r99))
    (push (cons 'loc local-reg) (cl-cc::ctx-env ctx))
    (cl-cc::%resolve-func-sym-reg 'loc ctx)
    (assert-eq nil (codegen-find-inst ctx 'cl-cc::vm-const))))

(deftest resolve-func-sym-global-shadows-local
  "%resolve-func-sym-reg for a global function loads the symbol even if env has entry."
  (let* ((ctx (make-codegen-ctx))
         (local-reg :r77))
    (push (cons 'gfn local-reg) (cl-cc::ctx-env ctx))
    (setf (gethash 'gfn (cl-cc::ctx-global-functions ctx)) t)
    (cl-cc::%resolve-func-sym-reg 'gfn ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-eq 'gfn (cl-cc::vm-const-value inst)))))

(deftest compile-closure-body
  "%compile-closure-body emits vm-ret and binds params in ctx-env."
  (let* ((ctx       (make-codegen-ctx))
         (base-env  (list (cons 'outer :r0)))
         (param-reg (cl-cc::make-register ctx)))
    (setf (cl-cc::ctx-env ctx) base-env)
    (cl-cc::%compile-closure-body ctx '(p) (list param-reg)
                                  (list (make-ast-int :value 1)) base-env)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-ret))
    (assert-true (assoc 'p (cl-cc::ctx-env ctx)))))
