;;;; tests/unit/compile/codegen-core-array-sink-tests.lisp — Codegen array non-escape/sink tests

(in-package :cl-cc/test)
(in-suite cl-cc-codegen-unit-serial-suite)

(deftest codegen-let-noescape-array-variable-aset-bypasses-vm-aset
  "A non-escaping fixed-size local array can update variable indices via bounded dispatch."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2))))
                                (cons 'i (make-ast-int :value 1)))
                :body (list (make-ast-call :func 'aset
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-var :name 'i)
                                                       (make-ast-int :value 42)))
                            (make-ast-call :func 'aref
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-var :name 'i)))))
               ctx)))
    (assert-true (keywordp reg))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-make-array))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-aset))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-aref))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-num-eq))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-jump-zero))))

(deftest codegen-let-branch-local-array-use-elides-allocation
  "A fixed-size local array used only in one branch is elided entirely on the optimized path."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2))))
                                (cons 'i (make-ast-int :value 1)))
                :body (list (make-ast-if
                             :cond (make-ast-int :value 1)
                             :then (make-ast-call :func 'aref
                                                  :args (list (make-ast-var :name 'arr)
                                                              (make-ast-var :name 'i)))
                             :else (make-ast-int :value 0))))
                ctx))
         (insts (codegen-instructions ctx))
         (jump-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-jump-zero)) insts))
         (const0-positions (loop for inst in insts
                                 for idx from 0
                                 when (and (typep inst 'cl-cc/vm::vm-const)
                                           (eql (cl-cc/vm::vm-value inst) 0))
                                 collect idx)))
    (assert-true (keywordp reg))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-make-array))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-aref))
    (assert-true jump-pos)
    (assert-true const0-positions)
    (assert-true (every (lambda (idx) (> idx jump-pos)) const0-positions))))

(deftest codegen-let-branch-array-escape-preserves-allocation
  "If a branch returns the array itself, the heap-backed make-array path is preserved."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2))))
                                (cons 'i (make-ast-int :value 1)))
                :body (list (make-ast-if
                             :cond (make-ast-int :value 1)
                             :then (make-ast-var :name 'arr)
                             :else (make-ast-call :func 'aref
                                                  :args (list (make-ast-var :name 'arr)
                                                              (make-ast-var :name 'i))))))
                ctx)))
    (assert-true (keywordp reg))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-make-array))))

(deftest codegen-let-branch-shadowed-array-binding-still-sinks-outer-use
  "A shadowed inner array binding must not block sinking the outer array into its only real branch."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2))))
                                (cons 'i (make-ast-int :value 1)))
                :body (list (make-ast-if
                             :cond (make-ast-int :value 1)
                             :then (make-ast-let
                                    :bindings (list (cons 'arr (make-ast-call
                                                               :func 'make-array
                                                               :args (list (make-ast-int :value 1)))))
                                    :body (list (make-ast-call :func 'aref
                                                               :args (list (make-ast-var :name 'arr)
                                                                           (make-ast-int :value 0)))))
                             :else (make-ast-call :func 'aref
                                                  :args (list (make-ast-var :name 'arr)
                                                              (make-ast-var :name 'i))))))
               ctx))
         (insts (codegen-instructions ctx))
         (jump-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-jump-zero)) insts)))
    (assert-true (keywordp reg))
    (assert-true jump-pos)
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-make-array))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-aref))))

(deftest-each codegen-single-inst-emission
  "Each AST form emits the expected VM instruction type."
  :cases (("setq-emits-move"  :setq  'cl-cc/vm::vm-move)
          ("print-emits-print" :print 'cl-cc/vm::vm-print))
  (scenario expected-inst)
  (let ((ctx (make-codegen-ctx)))
    (ecase scenario
      (:setq
       (setf (cl-cc/compile::ctx-env ctx) (list (cons 'x :R0)))
       (compile-ast (make-ast-setq :var 'x :value (make-ast-int :value 99)) ctx))
      (:print
       (compile-ast (make-ast-print :expr (make-ast-int :value 42)) ctx)))
    (assert-true (codegen-find-inst ctx expected-inst))))

(deftest codegen-the-compiles-inner
  "Compiling (the integer ...) emits the inner assertion path."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc:make-ast-the :type 'integer
                                 :value (make-ast-int :value 42))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-typep))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-signal-error))))

(deftest codegen-hole-signals-typed-hole-message
  "Compiling '_' signals an ast-compilation-error mentioning typed holes."
  (let ((ctx (make-codegen-ctx)))
    (handler-case
        (progn
          (compile-ast (cl-cc/parse::lower-sexp-to-ast '_) ctx)
          (assert-true nil))
      (cl-cc:ast-compilation-error (e)
        (assert-true (search "Typed hole" (format nil "~A" e)))))))

(deftest codegen-if-narrows-branch-type-env
  "Type guards narrow the then branch so proven vars skip redundant assertions."
  (let* ((ctx (make-codegen-ctx))
         (ast (cl-cc/parse::lower-sexp-to-ast
                '(if (numberp x) (the fixnum x) 0))))
    (setf (cl-cc/compile::ctx-env ctx) (list (cons 'x :R0)))
    (compile-ast ast ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-typep))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-signal-error))))

(deftest codegen-if-case-of-case-collapses-redundant-inner-branch
  "Nested ifs with the same condition only emit one branch test."
  (let* ((ctx (make-codegen-ctx))
         (ast (cl-cc/parse::lower-sexp-to-ast
               '(if (numberp x)
                    (if (numberp x) 1 2)
                    3))))
    (setf (cl-cc/compile::ctx-env ctx) (list (cons 'x :R0)))
    (compile-ast ast ctx)
    (assert-= 1 (count-if (lambda (inst)
                            (typep inst 'cl-cc/vm::vm-jump-zero))
                          (codegen-instructions ctx)))))
