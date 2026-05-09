;;;; tests/unit/compile/codegen-tests.lisp — Codegen Unit Tests
;;;
;;; Direct tests for compile-ast methods and codegen helpers.
;;; Tests compile individual AST nodes to VM instructions without
;;; going through the full pipeline (parser + macro expander).

(in-package :cl-cc/test)

(defsuite cl-cc-codegen-unit-serial-suite
  :description "Serial codegen-heavy unit tests that share mutable compiler state"
  :parent cl-cc-unit-suite
  :parallel nil)

(in-suite cl-cc-codegen-unit-serial-suite)

;;; ─── Helpers ────────────────────────────────────────────────────────────

(defun make-codegen-ctx ()
  "Create a fresh compiler context for codegen tests.

Codegen unit tests assert on exact label names such as `DEFUN_FOO_0`. Those
labels become order-dependent if a prior REPL-oriented test leaked
`*repl-label-counter*` or related REPL globals into `compiler-context`
initialization. Bind the REPL state hooks to NIL here so unit tests always get a
stable, isolated context."
  (let ((cl-cc/compile:*repl-label-counter* nil)
        (cl-cc/compile:*repl-global-variables* nil)
        (cl-cc/compile:*repl-capture-label-counter* nil))
    (make-instance 'cl-cc/compile:compiler-context)))

(defun codegen-instructions (ctx)
  "Return the emitted instructions from CTX (in order)."
  (nreverse (copy-list (cl-cc/compile:ctx-instructions ctx))))

(defun codegen-find-inst (ctx type)
  "Find the first instruction of TYPE in CTX's emitted instructions."
  (find-if (lambda (i) (typep i type)) (codegen-instructions ctx)))

;;; ─── compile-ast: ast-int ───────────────────────────────────────────────

(deftest codegen-integer-literal-returns-keyword-register
  "compile-ast of an ast-int returns a keyword-named register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-int :value 42) ctx)))
    (assert-true (keywordp reg))))

(deftest codegen-local-var-returns-bound-register
  "compile-ast of an ast-var with a bound name returns the associated register."
  (let* ((ctx (make-codegen-ctx))
         (reg :R99))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'x reg)))
    (let ((result (compile-ast (make-ast-var :name 'x) ctx)))
      (assert-eq reg result))))

(deftest codegen-unbound-var-signals-error
  "compile-ast of an ast-var for an unbound name signals an error."
  (let ((ctx (make-codegen-ctx)))
    (assert-signals error
      (compile-ast (make-ast-var :name 'nonexistent-var-xyz) ctx))))

(deftest-each codegen-int-emits-const-value
  "Compiling an integer literal emits vm-const with the correct value."
  :cases (("positive"  42)
          ("zero"       0)
          ("negative"  -1))
  (n)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-int :value n) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
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
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (assert-true inst)
      (assert-equal name (cl-cc::vm-const-value inst)))))


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
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (assert-true inst)
      (assert-equal datum (cl-cc::vm-const-value inst)))))

;;; ─── compile-ast: ast-binop ─────────────────────────────────────────────

(deftest-each codegen-binop-emits-correct-instruction
  "Each binary operator compiles to its corresponding VM instruction."
  :cases (("add" '+ 'cl-cc/vm::vm-add)
          ("sub" '- 'cl-cc/vm::vm-sub)
          ("mul" '* 'cl-cc/vm::vm-mul)
          ("lt"  '< 'cl-cc/vm::vm-lt)
          ("gt"  '> 'cl-cc/vm::vm-gt)
          ("eq"  '= 'cl-cc/vm::vm-num-eq))
  (op inst-type)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-binop :op op
                                  :lhs (make-ast-int :value 1)
                                  :rhs (make-ast-int :value 2))
                 ctx)
    (assert-true (codegen-find-inst ctx inst-type))))

;;; ─── codegen helpers ─────────────────────────────────────────────────────

(deftest codegen-make-register-returns-unique-keywords
  "make-register returns unique keyword registers on each call."
  (let* ((ctx (make-codegen-ctx))
         (r1 (cl-cc/compile:make-register ctx))
         (r2 (cl-cc/compile:make-register ctx)))
    (assert-true (keywordp r1))
    (assert-true (keywordp r2))
    (assert-false (eq r1 r2))))

(deftest codegen-emit-appends-one-instruction
  "emit appends exactly one instruction to the context's instruction list."
  (let ((ctx (make-codegen-ctx)))
    (cl-cc/compile:emit ctx (cl-cc::make-vm-const :dst :R0 :value 42))
    (assert-= 1 (length (codegen-instructions ctx)))))

(deftest codegen-make-label-returns-unique-strings
  "make-label returns distinct strings even for the same prefix."
  (let* ((ctx (make-codegen-ctx))
         (l1 (cl-cc/compile:make-label ctx "TEST"))
         (l2 (cl-cc/compile:make-label ctx "TEST")))
    (assert-false (string= l1 l2))))

;;; ─── compile-ast: ast-call fast paths ────────────────────────────────────

(deftest-each codegen-call-higher-order-fast-path
  "funcall and apply each dispatch to their dedicated VM instructions, never vm-generic-call."
  :cases (("funcall" 'funcall 'cl-cc/vm::vm-call)
          ("apply"   'apply   'cl-cc/vm::vm-apply))
  (fn-sym inst-type)
  (let* ((ctx (make-codegen-ctx))
         (fn-reg (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-env ctx) (list (cons 'fn fn-reg)))
    (compile-ast (make-ast-call :func fn-sym
                                :args (list (make-ast-var :name 'fn)
                                            (make-ast-int :value 1)))
                 ctx)
    (assert-true (codegen-find-inst ctx inst-type))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-generic-call))))

(deftest codegen-call-noescape-array-length-emits-const
  "(array-length arr) on noescape array emits vm-const with the size, not vm-call."
  (let* ((ctx (make-codegen-ctx))
         ;; Register a noescape array binding of size 3
         (arr-reg (cl-cc/compile:make-register ctx))
         (z-reg   (cl-cc/compile:make-register ctx)))
    (setf (cl-cc/compile:ctx-noescape-array-bindings ctx)
          (list (cons 'arr (list 3 z-reg z-reg z-reg))))
    (compile-ast (make-ast-call :func 'array-length
                                :args (list (make-ast-var :name 'arr)))
                 ctx)
    (let ((const-inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (assert-true const-inst)
      (assert-= 3 (cl-cc::vm-const-value const-inst)))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-call))))

;;; ─── optimize-ast / %loc macro ───────────────────────────────────────────

(deftest optimize-ast-preserves-source-location
  "optimize-ast carries source-file, source-line, and source-column through to the result."
  (let* ((src (make-ast-if :cond (make-ast-int :value 1 :source-line 5)
                            :then (make-ast-int :value 1)
                            :else (make-ast-int :value 0)
                            :source-file "test.lisp"
                            :source-line 5
                            :source-column 2))
         (result (cl-cc/compile:optimize-ast src)))
    (assert-equal "test.lisp" (cl-cc::ast-source-file result))
    (assert-=     5           (cl-cc::ast-source-line result))
    (assert-=     2           (cl-cc::ast-source-column result))))

(deftest optimize-ast-recursively-folds-ast-progn
  "optimize-ast constant-folds a binop inside an ast-progn."
  (let* ((node (make-ast-progn :forms (list (make-ast-binop :op '+
                                                             :lhs (make-ast-int :value 2)
                                                             :rhs (make-ast-int :value 3)))))
         (result (cl-cc/compile:optimize-ast node)))
    (assert-true (typep result 'cl-cc::ast-progn))
    (assert-true (typep (first (cl-cc/ast:ast-progn-forms result)) 'cl-cc::ast-int))
    (assert-= 5 (cl-cc/ast:ast-int-value (first (cl-cc/ast:ast-progn-forms result))))))

(deftest optimize-ast-folds-defconstant-backed-binop
  "optimize-ast rewrites defconstant-backed ast-var nodes so surrounding binops can fold."
  (cl-cc/expand::compiler-macroexpand-all '(defconstant +optimize-ast-inline-constant+ 41))
  (let* ((node (make-ast-binop :op '+
                               :lhs (make-ast-var :name '+optimize-ast-inline-constant+)
                               :rhs (make-ast-int :value 1)))
         (result (cl-cc/compile:optimize-ast node)))
    (assert-true (typep result 'cl-cc::ast-int))
    (assert-= 42 (cl-cc/ast:ast-int-value result))))

(deftest-each optimize-ast-keeps-shadowed-defconstant-as-variable
  "optimize-ast must not inline a defconstant when an inner lexical binding shadows the name."
  :cases (("let-body"
           (make-ast-let :bindings (list (cons 'optimize-shadowed-constant
                                               (make-ast-int :value 2)))
                         :declarations nil
                         :body (list (make-ast-var :name 'optimize-shadowed-constant)))
           #'cl-cc/ast:ast-let-body)
          ("lambda-key-param"
           (make-ast-lambda :params nil
                            :optional-params nil
                            :rest-param nil
                            :key-params (list (list 'optimize-shadowed-constant nil nil nil))
                            :declarations nil
                            :body (list (make-ast-var :name 'optimize-shadowed-constant)))
           #'cl-cc/ast:ast-lambda-body)
          ("defun-optional-param"
           (cl-cc/ast:make-ast-defun :name 'optimize-shadowed-constant-user
                                     :params nil
                                     :optional-params (list (list 'optimize-shadowed-constant nil nil))
                                     :rest-param nil
                                     :key-params nil
                                     :declarations nil
                                     :documentation nil
                                     :body (list (make-ast-var :name 'optimize-shadowed-constant)))
           #'cl-cc/ast:ast-defun-body))
  (node body-reader)
  (cl-cc/expand::compiler-macroexpand-all '(defconstant optimize-shadowed-constant 99))
  (let* ((result (cl-cc/compile:optimize-ast node))
         (body-form (first (funcall body-reader result))))
    (assert-true (typep body-form 'cl-cc::ast-var))
    (assert-eq 'optimize-shadowed-constant (cl-cc/ast:ast-var-name body-form))))

;;; ─── %let-binding-special-p ─────────────────────────────────────────────

(deftest-each let-binding-special-p-dispatch
  "%let-binding-special-p returns T only for earmuffed symbols registered as global."
  :cases (("earmuffs-and-global"      '*x*            t    t)
          ("earmuffs-no-registration" '*unregistered* nil  nil)
          ("no-earmuffs-global"       'plain          t    nil))
  (sym register-p expected)
  (let ((ctx (make-codegen-ctx)))
    (when register-p
      (setf (gethash sym (cl-cc/compile:ctx-global-variables ctx)) t))
    (assert-equal expected (cl-cc/compile::%let-binding-special-p sym ctx))))

;;; ─── %let-noescape-closure ──────────────────────────────────────────────

(deftest let-noescape-closure-unmutated-lambda-is-eligible
  "%let-noescape-closure returns the lambda when the binding is not mutated."
  (let* ((lam  (make-ast-lambda :params '(x)
                                :optional-params nil
                                :rest-param nil
                                :key-params nil
                                :body (list (make-ast-var :name 'x))))
         (body (list (make-ast-call :func (make-ast-var :name 'f)
                                    :args (list (make-ast-int :value 1)))))
         (result (cl-cc/compile::%let-noescape-closure 'f lam nil nil nil body)))
    (assert-eq lam result)))

(deftest let-noescape-closure-mutated-binding-returns-nil
  "%let-noescape-closure returns NIL when the binding appears in the mutated set."
  (let* ((lam (make-ast-lambda :params '(x)
                               :optional-params nil
                               :rest-param nil
                               :key-params nil
                               :body (list (make-ast-var :name 'x))))
         (body (list (make-ast-int :value 1))))
    (assert-null (cl-cc/compile::%let-noescape-closure 'f lam nil '(f) nil body))))

;;; ─── %let-noescape-array-size ───────────────────────────────────────────

(deftest let-noescape-array-size-non-make-array-returns-nil
  "%let-noescape-array-size returns NIL when the expression is not a make-array call."
  (let ((expr (make-ast-int :value 5)))
    (assert-null (cl-cc/compile::%let-noescape-array-size 'arr expr nil nil nil nil))))

(deftest let-noescape-cons-p-non-cons-expr-returns-nil
  "%let-noescape-cons-p returns NIL when the expression is not a cons call."
  (let ((expr (make-ast-int :value 5)))
    (assert-null (cl-cc/compile::%let-noescape-cons-p 'c expr nil nil nil nil))))
