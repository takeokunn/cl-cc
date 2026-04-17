;;;; tests/unit/compile/codegen-runtime-tests.lisp — Codegen runtime semantics tests

(in-package :cl-cc/test)
(in-suite cl-cc-integration-suite)

(deftest codegen-values-compilation
  "Compiling ast-values emits vm-values and returns a register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast::make-ast-values
                             :forms (list (make-ast-int :value 1)
                                          (make-ast-int :value 2)
                                          (make-ast-int :value 3)))
                           ctx)))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-values))
    (assert-true (keywordp reg))))

(deftest codegen-values-basic-run
  "values returns all values accessible via multiple-value-list."
  (assert-equal '(1 2 3)
    (run-string "(multiple-value-list (values 1 2 3))")))

(deftest codegen-values-single-run
  "values with one argument returns that argument."
  (assert-run= 42 "(values 42)"))

(deftest codegen-mvb-explicit-values-uses-direct-binding
  "Compiling multiple-value-bind over explicit ast-values skips vm-values/vm-mv-bind."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast::make-ast-multiple-value-bind
                              :vars '(a b)
                              :values-form (cl-cc/ast::make-ast-values
                                            :forms (list (make-ast-int :value 1)
                                                         (make-ast-int :value 2)))
                              :body (list (make-ast-var :name 'a)))
                            ctx)))
    (assert-false (codegen-find-inst ctx 'cl-cc/vm::vm-values))
    (assert-false (codegen-find-inst ctx 'cl-cc/vm::vm-mv-bind))
    (assert-true (keywordp reg))))

(deftest codegen-mvb-non-values-form-still-uses-vm-mv-bind
  "Non-ast-values multiple-value-bind still uses the generic vm-mv-bind path."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast::make-ast-multiple-value-bind
                             :vars '(a b)
                             :values-form (make-ast-call :func 'floor
                                                         :args (list (make-ast-int :value 17)
                                                                     (make-ast-int :value 5)))
                             :body (list (make-ast-var :name 'a)))
                           ctx)))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-mv-bind))
    (assert-true (keywordp reg))))

(deftest-each codegen-mvb-run
  "multiple-value-bind binds values and evaluates body correctly."
  :cases (("sum-values"  3  "(multiple-value-bind (a b) (values 1 2) (+ a b))")
          ("first-value" 10 "(multiple-value-bind (x y) (values 10 20) x)"))
  (expected code)
  (assert-run= expected code))

(deftest-each codegen-mv-call-direct-path
  "multiple-value-call uses vm-call directly for explicit ast-values and zero-arg cases."
  :cases (("explicit-values"
           (cl-cc/ast::make-ast-multiple-value-call
             :func (make-ast-function :name '+)
             :args (list (cl-cc/ast::make-ast-values
                          :forms (list (make-ast-int :value 1)
                                       (make-ast-int :value 2))))))
          ("no-args"
           (cl-cc/ast::make-ast-multiple-value-call
             :func (make-ast-function :name 'list)
             :args nil)))
  (ast)
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast ast ctx)))
    (assert-true  (codegen-find-inst ctx 'cl-cc/vm::vm-call))
    (assert-false (codegen-find-inst ctx 'cl-cc/vm::vm-apply))
    (assert-false (codegen-find-inst ctx 'cl-cc/vm::vm-values-to-list))
    (assert-true  (keywordp reg))))

(deftest codegen-mv-call-mixed-args-still-uses-apply-path
  "Non-ast-values arguments keep the generic multiple-value-call path."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast::make-ast-multiple-value-call
                             :func (make-ast-function :name '+)
                             :args (list (cl-cc/ast::make-ast-values
                                          :forms (list (make-ast-int :value 1)
                                                       (make-ast-int :value 2)))
                                         (make-ast-call :func 'floor
                                                        :args (list (make-ast-int :value 17)
                                                                    (make-ast-int :value 5)))))
                           ctx)))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-apply))
    (assert-true (keywordp reg))))

(deftest codegen-mv-call-basic-run
  "multiple-value-call spreads values as arguments to a user-defined function."
  (assert-run= 3
    "(multiple-value-call (lambda (a b) (+ a b)) (values 1 2))"))

(deftest codegen-mv-call-cons-run
  "multiple-value-call with cons spreads two values into car and cdr."
  (assert-equal '(1 . 2)
    (run-string "(multiple-value-call #'cons (values 1 2))")))

(deftest codegen-mv-prog1-compilation
  "Compiling ast-multiple-value-prog1: returns a register and emits all sub-form constants."
  (let* ((ctx    (make-codegen-ctx))
         (reg    (compile-ast (cl-cc::make-ast-multiple-value-prog1
                                :first (make-ast-int :value 1)
                                :forms (list (make-ast-int :value 2)
                                             (make-ast-int :value 3)))
                              ctx))
         (consts (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-const))
                                (codegen-instructions ctx))))
    (assert-true (keywordp reg))
    (assert-true (>= (length consts) 3))))

(deftest codegen-mv-prog1-preserves-first-run
  "multiple-value-prog1 returns the value of the first form."
  (assert-run= 1
    "(multiple-value-prog1 1 2 3)"))

(deftest codegen-mv-prog1-side-effects-run
  "multiple-value-prog1 evaluates subsequent forms for side effects."
  (let ((output (with-output-to-string (*standard-output*)
                  (run-string "(multiple-value-prog1 42 (print 99))"))))
    (assert-true (search "99" output))))

(deftest-each codegen-exception-form-emits-establish-handler
  "Both unwind-protect and handler-case emit vm-establish-handler."
  :cases (("unwind-protect" (cl-cc/ast::make-ast-unwind-protect
                              :protected (make-ast-int :value 42)
                              :cleanup (list (make-ast-int :value 0))))
          ("handler-case"   (cl-cc/ast::make-ast-handler-case
                              :form (make-ast-int :value 42)
                              :clauses (list (list 'error 'e (make-ast-int :value 0))))))
  (ast)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast ast ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-establish-handler))))

(deftest-each codegen-exception-form-returns-register
  "Both unwind-protect and handler-case compilation return a register keyword."
  :cases (("unwind-protect"
           (cl-cc/ast::make-ast-unwind-protect
             :protected (make-ast-int :value 7)
             :cleanup (list (make-ast-int :value 0))))
          ("handler-case"
           (cl-cc/ast::make-ast-handler-case
             :form (make-ast-int :value 10)
             :clauses (list (list 'error nil (make-ast-int :value 0))))))
  (ast)
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast ast ctx)))
    (assert-true (keywordp reg))))

(deftest codegen-unwind-protect-normal-result-run
  "unwind-protect returns the protected form value on normal exit."
  (assert-run= 42
    "(unwind-protect 42 nil)"))

(deftest codegen-unwind-protect-cleanup-runs-run
  "unwind-protect cleanup form executes on normal exit."
  (let ((output (with-output-to-string (*standard-output*)
                  (run-string "(unwind-protect 1 (print 99))"))))
    (assert-true (search "99" output))))

(deftest codegen-handler-case-normal-result-run
  "handler-case returns the protected form value when no condition is signaled."
  (assert-run= 42
    "(handler-case 42 (error (e) -1))"))

(deftest codegen-handler-case-catches-error-run
  "handler-case invokes the matching handler when a condition is signaled."
  (assert-run= 99
    "(handler-case (error \"boom\") (error (e) 99))"))
