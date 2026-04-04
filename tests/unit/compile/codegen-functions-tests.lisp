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

(deftest codegen-defun-self-tail-call-loops
  "A simple self-tail call in defun compiles to a jump back to the entry label."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast
     (cl-cc::make-ast-defun
      :name 'loop-fn
      :params '(x)
      :body (list (make-ast-if
                   :cond (make-ast-binop :op '=
                                         :lhs (make-ast-var :name 'x)
                                         :rhs (make-ast-int :value 0))
                   :then (make-ast-var :name 'x)
                   :else (make-ast-call
                          :func 'loop-fn
                          :args (list (make-ast-binop :op '-
                                                      :lhs (make-ast-var :name 'x)
                                                      :rhs (make-ast-int :value 1)))))))
     ctx)
    (let ((tail-call (codegen-find-inst ctx 'cl-cc::vm-tail-call))
          (loop-label (format nil "DEFUN_~A_0" 'loop-fn))
          (self-jump nil))
      (dolist (inst (codegen-instructions ctx))
        (when (and (typep inst 'cl-cc::vm-jump)
                   (string= (cl-cc::vm-label-name inst) loop-label))
          (setf self-jump inst)))
       (assert-eq nil tail-call)
       (assert-true self-jump))))

(deftest codegen-defun-self-tail-call-loop-snapshots-args
  "Self-tail loop lowering snapshots argument registers before rewriting params."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast
     (cl-cc::make-ast-defun
      :name 'swap-loop
      :params '(x y)
      :body (list (make-ast-if
                   :cond (make-ast-binop :op '=
                                         :lhs (make-ast-var :name 'x)
                                         :rhs (make-ast-int :value 0))
                   :then (make-ast-var :name 'y)
                   :else (make-ast-call
                          :func 'swap-loop
                          :args (list (make-ast-var :name 'y)
                                      (make-ast-binop :op '-
                                                      :lhs (make-ast-var :name 'x)
                                                      :rhs (make-ast-int :value 1)))))))
     ctx)
    (let ((tail-call (codegen-find-inst ctx 'cl-cc::vm-tail-call))
          (loop-label (format nil "DEFUN_~A_0" 'swap-loop))
          (self-jump nil)
          (move-count 0))
      (dolist (inst (codegen-instructions ctx))
        (when (typep inst 'cl-cc::vm-move)
          (incf move-count))
        (when (and (typep inst 'cl-cc::vm-jump)
                   (string= (cl-cc::vm-label-name inst) loop-label))
          (setf self-jump inst)))
      (assert-eq nil tail-call)
      (assert-true self-jump)
      ;; Two snapshot moves + two parameter rewrite moves.
      (assert-true (>= move-count 4)))))

(deftest codegen-defvar-compilation
  "Compiling defvar registers in global-variables and emits vm-const for the value."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-defvar :name 'test-codegen-var
                                   :value (make-ast-int :value 99))
                 ctx)
    (assert-true (gethash 'test-codegen-var (cl-cc::ctx-global-variables ctx)))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-const))))

(deftest codegen-defconstant-inlines-symbol-reference
  "Compiling a symbol bound by defconstant emits vm-const with the constant value."
  (cl-cc::compiler-macroexpand-all '(defconstant codegen-inline-constant 123))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-var :name 'codegen-inline-constant) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-equal 123 (cl-cc::vm-const-value inst)))
    (assert-eq nil (codegen-find-inst ctx 'cl-cc::vm-get-global))))

(deftest codegen-apply-compilation
  "Compiling ast-apply with a constant spread list emits vm-call and returns a register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc::make-ast-apply
                               :func (make-ast-function :name '+)
                               :args (list (make-ast-quote :value '(1 2 3))))
                             ctx)))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-call))
    (assert-true (null (codegen-find-inst ctx 'cl-cc::vm-apply)))
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

(deftest codegen-let-noescape-lambda-call-bypasses-vm-closure
  "A non-escaping let-bound lambda used only at direct call sites avoids vm-closure."
  (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast
                (make-ast-let
                 :bindings (list (cons 'f (make-ast-lambda
                                           :params '(x)
                                           :body (list (make-ast-var :name 'x)))))
                 :body (list (make-ast-call :func (make-ast-var :name 'f)
                                            :args (list (make-ast-int :value 7)))))
                ctx)))
      (assert-true (keywordp reg))
      (assert-null (codegen-find-inst ctx 'cl-cc::vm-closure))
      (assert-null (codegen-find-inst ctx 'cl-cc::vm-call)))))

(deftest codegen-let-escaped-lambda-call-falls-back-to-vm-closure
  "Captured let-bound lambdas keep the normal vm-closure + vm-call path."
  (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast
                (make-ast-let
                 :bindings (list (cons 'f (make-ast-lambda
                                           :params '(x)
                                           :body (list (make-ast-var :name 'x)))))
                 :body (list (make-ast-lambda :params '() :body (list (make-ast-var :name 'f)))
                             (make-ast-call :func (make-ast-var :name 'f)
                                            :args (list (make-ast-int :value 9)))))
                ctx)))
      (assert-true (keywordp reg))
      (assert-true (codegen-find-inst ctx 'cl-cc::vm-closure))
      (assert-true (codegen-find-inst ctx 'cl-cc::vm-call)))))

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

(deftest codegen-rest-params-mark-stack-safe-closures
  "&rest closures that only consume the list locally are marked stack-safe."
  (let* ((ctx (make-codegen-ctx))
         (ast (make-ast-lambda
                :params '(x)
                :rest-param 'args
                :body (list (make-ast-call
                              :func 'car
                              :args (list (make-ast-var :name 'args)))))))
    (compile-ast ast ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-closure)))
      (assert-true inst)
      (assert-true (cl-cc::vm-closure-rest-stack-alloc-p inst)))))

(deftest codegen-rest-params-mark-escaping-closures
  "&rest closures that return the list are not marked stack-safe."
  (let* ((ctx (make-codegen-ctx))
         (ast (make-ast-lambda
                 :params '(x)
                :rest-param 'args
                :body (list (make-ast-var :name 'args)))))
    (compile-ast ast ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-closure)))
      (assert-true inst)
      (assert-null (cl-cc::vm-closure-rest-stack-alloc-p inst)))))

(deftest codegen-rest-params-dynamic-extent-forces-stack-safe
  "&rest dynamic-extent declarations force stack-safe closures."
  (let* ((ctx (make-codegen-ctx))
         (ast (make-ast-lambda
                 :params '(x)
                :rest-param 'args
                :declarations '((dynamic-extent args))
                :body (list (make-ast-var :name 'args)))))
    (compile-ast ast ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-closure)))
      (assert-true inst)
      (assert-true (cl-cc::vm-closure-rest-stack-alloc-p inst)))))

(deftest codegen-rest-params-captured-by-inner-lambda-escape
  "&rest lists captured by an inner lambda are not marked stack-safe."
  (let* ((ctx (make-codegen-ctx))
         (ast (make-ast-lambda
                :params '(x)
                :rest-param 'args
                :body (list (make-ast-lambda
                             :params '()
                             :body (list (make-ast-var :name 'args)))))))
    (compile-ast ast ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-closure)))
      (assert-true inst)
      (assert-null (cl-cc::vm-closure-rest-stack-alloc-p inst)))))
