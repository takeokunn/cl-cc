;;;; tests/unit/compile/codegen-functions-callsite-tests.lisp — Call-site codegen tests
;;;;
;;;; Tests for apply/lambda/let/call/flet/rest-param and function resolution.
;;;; Suite: cl-cc-codegen-functions-serial-suite (defined in codegen-functions-tests.lisp).

(in-package :cl-cc/test)

(in-suite cl-cc-codegen-functions-serial-suite)

;;; ─── apply ────────────────────────────────────────────────────────────────

(deftest codegen-apply-compilation
  "Compiling ast-apply with a constant spread list emits vm-call and returns a register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast::make-ast-apply
                               :func (make-ast-function :name '+)
                               :args (list (make-ast-quote :value '(1 2 3))))
                             ctx)))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-call))
    (assert-true (null (codegen-find-inst ctx 'cl-cc/vm::vm-apply)))
    (assert-true (keywordp reg))))

(deftest-each codegen-apply-run
  "apply spreads list arguments to a function."
  :cases (("list-only"     6  "(apply #'+ '(1 2 3))")
          ("leading-args"  10 "(apply #'+ 1 2 '(3 4))"))
  (expected code)
  (assert-run= expected code))

;;; ─── lambda ───────────────────────────────────────────────────────────────

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
          (progn (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-closure))
                 (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-call)))
          (progn (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-closure))
                 (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-call)))))))

;;; ─── call ─────────────────────────────────────────────────────────────────

(deftest codegen-call-known-function
  "Compiling a call to a known function emits instructions."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'cons
                                 :args (list (make-ast-int :value 1)
                                             (make-ast-int :value 2)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-cons))))

(deftest-each codegen-call-list-accessor-emits-instruction
  "Calling car or cdr on a list emits the corresponding VM accessor instruction."
  :cases (("car" 'car 'cl-cc/vm::vm-car)
          ("cdr" 'cdr 'cl-cc/vm::vm-cdr))
  (func inst-type)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func func
                                 :args (list (make-ast-quote :value '(1 2))))
                 ctx)
    (assert-true (codegen-find-inst ctx inst-type))))

;;; ─── function symbol resolution ───────────────────────────────────────────

(deftest-each resolve-func-sym-dispatch
  "%resolve-func-sym-reg loads globals as vm-const and returns local env registers directly."
  :cases (("global-no-env"     t   'foo  nil   nil  'foo)
          ("global-shadows-env" t   'gfn  :r77  t    'gfn)
          ("local-env"         nil 'my-fn :r42  nil  :r42))
  (emits-const sym local-reg is-global expected-val)
  (let* ((ctx (make-codegen-ctx)))
    (when local-reg
      (push (cons sym local-reg) (cl-cc/compile::ctx-env ctx)))
    (when is-global
      (setf (gethash sym (cl-cc/compile::ctx-global-functions ctx)) t))
    (let ((result (cl-cc/compile::%resolve-func-sym-reg sym ctx))
          (inst   (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (if emits-const
          (progn (assert-true inst)
                 (assert-eq expected-val (cl-cc::vm-const-value inst)))
          (progn (assert-null inst)
                 (assert-eq expected-val result))))))

(deftest compile-closure-body
  "%compile-closure-body emits vm-ret and binds params in ctx-env."
  (let* ((ctx       (make-codegen-ctx))
         (base-env  (list (cons 'outer :r0)))
         (param-reg (cl-cc/compile::make-register ctx)))
    (setf (cl-cc/compile::ctx-env ctx) base-env)
    (cl-cc/compile::%compile-closure-body ctx '(p) (list param-reg)
                                  (list (make-ast-int :value 1)) base-env)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-ret))
    (assert-true (assoc 'p (cl-cc/compile::ctx-env ctx)))))

;;; ─── flet ─────────────────────────────────────────────────────────────────

(deftest-each codegen-flet-closure-vs-func-ref
  "flet: zero-capture bindings use vm-func-ref; capturing bindings fall back to vm-closure."
  :cases (("noescape"   :noescape)
          ("capturing"  :capturing))
  (scenario)
  (let ((ctx (make-codegen-ctx)))
    (let ((reg (compile-ast
                (ecase scenario
                  (:noescape
                   (make-ast-flet
                    :bindings (list (list 'f '(x) (make-ast-var :name 'x)))
                    :body (list (make-ast-call :func 'f :args (list (make-ast-int :value 5))))))
                  (:capturing
                   (make-ast-let
                    :bindings (list (cons 'y (make-ast-int :value 9)))
                    :body (list (make-ast-flet
                                 :bindings (list (list 'f '(x) (make-ast-var :name 'y)))
                                 :body (list (make-ast-call :func 'f :args (list (make-ast-int :value 5)))))))))
                ctx)))
      (assert-true (keywordp reg))
      (ecase scenario
        (:noescape
         (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-func-ref))
         (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-closure)))
        (:capturing
         (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-closure)))))))

;;; ─── rest params ──────────────────────────────────────────────────────────

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
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-closure)))
      (assert-true inst)
      (if expected
          (assert-true (cl-cc/vm::vm-closure-rest-stack-alloc-p inst))
          (assert-null (cl-cc/vm::vm-closure-rest-stack-alloc-p inst))))))
