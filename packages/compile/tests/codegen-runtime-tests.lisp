;;;; tests/unit/compile/codegen-runtime-tests.lisp — Codegen runtime semantics tests

(in-package :cl-cc/test)
(in-suite cl-cc-integration-suite)

(deftest codegen-values-compilation
  "Compiling ast-values emits vm-values and returns a register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-values
                             :forms (list (make-ast-int :value 1)
                                          (make-ast-int :value 2)
                                          (make-ast-int :value 3)))
                           ctx)))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-values))
    (assert-true (keywordp reg))))

(deftest-each codegen-values-run
  "values returns correct results for varying argument counts."
  :cases (("multi-values"  '(1 2 3) "(multiple-value-list (values 1 2 3))")
          ("single-value"  42       "(values 42)"))
  (expected form)
  (assert-equal expected (run-string form)))

(deftest codegen-mvb-compilation-cases
  "ast-values mvb skips vm-values/vm-mv-bind; non-ast-values mvb uses generic vm-mv-bind path."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-multiple-value-bind
                              :vars '(a b)
                              :values-form (cl-cc/ast:make-ast-values
                                            :forms (list (make-ast-int :value 1)
                                                         (make-ast-int :value 2)))
                              :body (list (make-ast-var :name 'a)))
                            ctx)))
    (assert-false (codegen-find-inst ctx 'cl-cc/vm::vm-values))
    (assert-false (codegen-find-inst ctx 'cl-cc/vm::vm-mv-bind))
    (assert-true (keywordp reg)))
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc/ast:make-ast-multiple-value-bind
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
           (cl-cc/ast:make-ast-multiple-value-call
             :func (make-ast-function :name '+)
             :args (list (cl-cc/ast:make-ast-values
                          :forms (list (make-ast-int :value 1)
                                       (make-ast-int :value 2))))))
          ("no-args"
           (cl-cc/ast:make-ast-multiple-value-call
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
         (reg (compile-ast (cl-cc/ast:make-ast-multiple-value-call
                             :func (make-ast-function :name '+)
                             :args (list (cl-cc/ast:make-ast-values
                                          :forms (list (make-ast-int :value 1)
                                                       (make-ast-int :value 2)))
                                         (make-ast-call :func 'floor
                                                        :args (list (make-ast-int :value 17)
                                                                    (make-ast-int :value 5)))))
                           ctx)))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-apply))
    (assert-true (keywordp reg))))

(deftest codegen-mv-call-run-cases
  "multiple-value-call spreads values to lambda; spreads to cons producing dotted pair."
  (assert-run= 3
    "(multiple-value-call (lambda (a b) (+ a b)) (values 1 2))")
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

(deftest codegen-mv-prog1-run-cases
  "multiple-value-prog1 returns first form's value; evaluates subsequent forms for side effects."
  (assert-run= 1
    "(multiple-value-prog1 1 2 3)")
  (let ((output (with-output-to-string (*standard-output*)
                  (run-string "(multiple-value-prog1 42 (print 99))"))))
    (assert-true (search "99" output))))

(deftest-each codegen-exception-form-emits-establish-handler
  "Both unwind-protect and handler-case emit vm-establish-handler."
  :cases (("unwind-protect" (cl-cc/ast:make-ast-unwind-protect
                              :protected (make-ast-int :value 42)
                              :cleanup (list (make-ast-int :value 0))))
          ("handler-case"   (cl-cc/ast:make-ast-handler-case
                              :form (make-ast-int :value 42)
                              :clauses (list (list 'error 'e (make-ast-int :value 0))))))
  (ast)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast ast ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-establish-handler))))

(deftest-each codegen-exception-form-returns-register
  "Both unwind-protect and handler-case compilation return a register keyword."
  :cases (("unwind-protect"
           (cl-cc/ast:make-ast-unwind-protect
             :protected (make-ast-int :value 7)
             :cleanup (list (make-ast-int :value 0))))
          ("handler-case"
           (cl-cc/ast:make-ast-handler-case
             :form (make-ast-int :value 10)
             :clauses (list (list 'error nil (make-ast-int :value 0))))))
  (ast)
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast ast ctx)))
    (assert-true (keywordp reg))))

(deftest codegen-unwind-protect-run-cases
  "unwind-protect returns protected value on normal exit; cleanup form executes."
  (assert-run= 42
    "(unwind-protect 42 nil)")
  (let ((output (with-output-to-string (*standard-output*)
                  (run-string "(unwind-protect 1 (print 99))"))))
    (assert-true (search "99" output))))

(deftest codegen-handler-case-run-cases
  "handler-case returns protected value when no condition signaled; catches error correctly."
  (assert-run= 42
    "(handler-case 42 (error (e) -1))")
  (assert-run= 99
    "(handler-case (error \"boom\") (error (e) 99))"))
