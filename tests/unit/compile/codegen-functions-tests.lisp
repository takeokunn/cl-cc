;;;; tests/unit/compile/codegen-functions-tests.lisp — Codegen function/call unit tests

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

(deftest codegen-function-ref-returns-register
  "Compiling #'fn returns a register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-function :name 'car) ctx)))
    (assert-true (keywordp reg))))

(deftest-each codegen-closure-form-emits-vm-closure
  "Closure-creating forms (defun, lambda, labels) emit vm-closure.
NOTE: flet is omitted because the noescape-closure optimizer inlines
flet-bound functions even when #'f is used in the body — the optimizer's
escape analysis treats (function f) as a direct reference to the known
in-scope binding rather than a true escape. Forcing vm-closure emission
from flet would require disabling the optimization specifically for this
test, which isn't worth the test-quality tradeoff."
  :cases (("defun"  (cl-cc::make-ast-defun
                      :name 'my-fn :params '(x)
                      :body (list (make-ast-var :name 'x))))
          ("lambda" (make-ast-lambda
                      :params '(x)
                      :body (list (make-ast-var :name 'x))))
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

(deftest-each codegen-let-lambda-escape-determines-closure
  "Non-escaping let-bound lambda avoids vm-closure; captured lambda keeps the full path."
  :cases (("noescape"
           (make-ast-let
            :bindings (list (cons 'f (make-ast-lambda
                                      :params '(x)
                                      :body (list (make-ast-var :name 'x)))))
            :body (list (make-ast-call :func (make-ast-var :name 'f)
                                       :args (list (make-ast-int :value 7)))))
           nil)
          ("escaped"
           (make-ast-let
            :bindings (list (cons 'f (make-ast-lambda
                                      :params '(x)
                                      :body (list (make-ast-var :name 'x)))))
            :body (list (make-ast-lambda :params '() :body (list (make-ast-var :name 'f)))
                        (make-ast-call :func (make-ast-var :name 'f)
                                       :args (list (make-ast-int :value 9)))))
           t))
  (ast closure-p)
  (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast ast ctx)))
      (assert-true (keywordp reg))
      (if closure-p
          (progn (assert-true (codegen-find-inst ctx 'cl-cc::vm-closure))
                 (assert-true (codegen-find-inst ctx 'cl-cc::vm-call)))
          (progn (assert-null (codegen-find-inst ctx 'cl-cc::vm-closure))
                 (assert-null (codegen-find-inst ctx 'cl-cc::vm-call)))))))

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

(deftest-each resolve-func-sym-dispatch
  "%resolve-func-sym-reg loads globals as vm-const and returns local env registers directly."
  :cases (("global-no-env"     t   'foo  nil   nil  'foo)
          ("global-shadows-env" t   'gfn  :r77  t    'gfn)
          ("local-env"         nil 'my-fn :r42  nil  :r42))
  (emits-const sym local-reg is-global expected-val)
  (let* ((ctx (make-codegen-ctx)))
    (when local-reg
      (push (cons sym local-reg) (cl-cc::ctx-env ctx)))
    (when is-global
      (setf (gethash sym (cl-cc::ctx-global-functions ctx)) t))
    (let ((result (cl-cc::%resolve-func-sym-reg sym ctx))
          (inst   (codegen-find-inst ctx 'cl-cc::vm-const)))
      (if emits-const
          (progn (assert-true inst)
                 (assert-eq expected-val (cl-cc::vm-const-value inst)))
          (progn (assert-null inst)
                 (assert-eq expected-val result))))))

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

(deftest codegen-flet-noescape-function-uses-vm-func-ref
  "Zero-capture flet bindings use vm-func-ref instead of vm-closure." 
  (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast
                (make-ast-flet
                 :bindings (list (list 'f '(x) (make-ast-var :name 'x)))
                 :body (list (make-ast-call :func 'f
                                            :args (list (make-ast-int :value 5)))))
                ctx)))
      (assert-true (keywordp reg))
      (assert-true (codegen-find-inst ctx 'cl-cc::vm-func-ref))
      (assert-null (codegen-find-inst ctx 'cl-cc::vm-closure)))))

(deftest codegen-flet-capturing-function-falls-back-to-vm-closure
  "Flet bindings that capture outer variables still allocate vm-closure." 
  (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast
                (make-ast-let
                 :bindings (list (cons 'y (make-ast-int :value 9)))
                 :body (list (make-ast-flet
                              :bindings (list (list 'f '(x) (make-ast-var :name 'y)))
                              :body (list (make-ast-call :func 'f
                                                         :args (list (make-ast-int :value 5)))))))
                ctx)))
      (assert-true (keywordp reg))
      (assert-true (codegen-find-inst ctx 'cl-cc::vm-closure)))))

(deftest-each codegen-rest-params-stack-alloc-classification
  "&rest param closures are marked stack-safe iff the rest list cannot escape."
  :cases (("local-consumer"
           t   nil
           (list (make-ast-call :func 'car :args (list (make-ast-var :name 'args)))))
          ("direct-return"
           nil nil
           (list (make-ast-var :name 'args)))
          ("dynamic-extent"
           t   '((dynamic-extent args))
           (list (make-ast-var :name 'args)))
          ("inner-capture"
           nil nil
           (list (make-ast-lambda :params '() :body (list (make-ast-var :name 'args))))))
  (expected declarations body)
  (let* ((ctx (make-codegen-ctx))
         (ast (make-ast-lambda
                :params '(x)
                :rest-param 'args
                :declarations declarations
                :body body)))
    (compile-ast ast ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-closure)))
      (assert-true inst)
      (if expected
          (assert-true (cl-cc::vm-closure-rest-stack-alloc-p inst))
          (assert-null (cl-cc::vm-closure-rest-stack-alloc-p inst))))))
