;;;; tests/unit/compile/codegen-core-tests.lisp — Codegen core tests

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

(deftest codegen-if-compilation
  "Compiling a pure if-form emits a vm-jump-zero + labels sequence and returns
a register. The codegen backend doesn't use vm-select for ordinary IF forms;
it branches via jump-zero/jump/label. vm-select only appears in specialized
sink/fold paths."
  (let* ((ctx (make-codegen-ctx))
         (x-reg (cl-cc/compile::make-register ctx)))
    (setf (cl-cc/compile::ctx-env ctx) (list (cons 'x x-reg)))
    (let ((reg (compile-ast (make-ast-if :cond (make-ast-var :name 'x)
                                          :then (make-ast-int :value 1)
                                          :else (make-ast-int :value 2))
                             ctx)))
      (assert-true (keywordp reg))
      (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-jump-zero))
      (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-jump)))))

(deftest codegen-progn-compilation
  "Compiling a progn returns a register and emits instructions for sub-forms."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-progn
                              :forms (list (make-ast-int :value 1)
                                          (make-ast-int :value 2)
                                          (make-ast-int :value 3)))
                           ctx))
         (consts (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-const))
                                (codegen-instructions ctx))))
    (assert-true (keywordp reg))
    (assert-= 3 (length consts))))

(deftest ast-constant-folding-before-codegen
  "optimize-ast folds literal arithmetic and pure string-length calls."
  (let ((arith (cl-cc/compile::optimize-ast (cl-cc/parse::lower-sexp-to-ast '(+ 1 2 3))))
        (strlen (cl-cc/compile::optimize-ast (cl-cc/parse::lower-sexp-to-ast '(string-length "hello")))))
    (assert-true (cl-cc/ast::ast-int-p arith))
    (assert-= 6 (cl-cc/ast::ast-int-value arith))
    (assert-true (cl-cc/ast::ast-int-p strlen))
    (assert-= 5 (cl-cc/ast::ast-int-value strlen))))

(deftest ast-partial-eval-known-defun-call
  "optimize-ast folds a known top-level defun call to an AST integer."
  (let ((result (cl-cc/compile::compile-toplevel-forms
                 '((defun add1 (x) (+ x 1))
                   (add1 41)))))
    (let ((asts (cl-cc/compile::compilation-result-ast result)))
      (assert-true (cl-cc/ast::ast-int-p (second asts)))
      (assert-= 42 (cl-cc/ast::ast-int-value (second asts))))))

(deftest codegen-let-compilation
  "Compiling a let returns a register and emits a move for the bound variable."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-let
                              :bindings (list (cons 'x (make-ast-int :value 42)))
                              :body (list (make-ast-var :name 'x)))
                            ctx)))
    (assert-true (keywordp reg))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-const))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-move))))

(deftest codegen-let-ignore-binding-skips-own-move
  "Compiling let with (declare (ignore x)) avoids the extra binding move."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-let
                             :bindings (list (cons 'x (make-ast-int :value 42)))
                             :declarations '((ignore x))
                             :body (list (make-ast-int :value 0)))
                           ctx))
         (moves (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-move))
                               (codegen-instructions ctx))))
    (assert-true (keywordp reg))
    (assert-= 0 (length moves))))

(deftest codegen-let-noescape-cons-car-bypasses-vm-car
  "A non-escaping let-bound cons lets car read the original component register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-let
                             :bindings (list (cons 'p (make-ast-call
                                                      :func 'cons
                                                      :args (list (make-ast-int :value 1)
                                                                  (make-ast-int :value 2)))))
                             :body (list (make-ast-call :func 'car
                                                        :args (list (make-ast-var :name 'p)))))
                           ctx))
         (cars (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-car))
                              (codegen-instructions ctx)))
         (moves (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-move))
                               (codegen-instructions ctx))))
    (assert-true (keywordp reg))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-cons))
    (assert-= 0 (length cars))
    (assert-true (> (length moves) 0))))

(deftest codegen-let-escaped-cons-car-falls-back-to-vm-car
  "Captured cons bindings are not treated as non-escaping car/cdr shortcuts."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'p (make-ast-call
                                         :func 'cons
                                         :args (list (make-ast-int :value 1)
                                                     (make-ast-int :value 2)))))
                :body (list (make-ast-lambda :params '() :body (list (make-ast-var :name 'p)))
                            (make-ast-call :func 'car :args (list (make-ast-var :name 'p)))))
               ctx))
         (cars (remove-if-not (lambda (i) (typep i 'cl-cc/vm::vm-car))
                              (codegen-instructions ctx))))
    (assert-true (keywordp reg))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-cons))
    (assert-true (> (length cars) 0))))

(deftest-each codegen-let-branch-local-cons-sinks-allocation
  "Branch-local non-escaping cons delays component evaluation past the branch test (cons-pos > jump-pos)."
  :cases (("simple-cond"
           (make-ast-let
            :bindings (list (cons 'p (make-ast-call
                                     :func 'cons
                                     :args (list (make-ast-call :func 'cons
                                                                :args (list (make-ast-int :value 1)
                                                                            (make-ast-int :value 2)))
                                                 (make-ast-int :value 3)))))
            :body (list (make-ast-if
                         :cond (make-ast-int :value 1)
                         :then (make-ast-call :func 'car :args (list (make-ast-var :name 'p)))
                         :else (make-ast-int :value 0)))))
          ("multi-binding-cond"
           (make-ast-let
            :bindings (list (cons 'p (make-ast-call
                                     :func 'cons
                                     :args (list (make-ast-call :func 'cons
                                                                :args (list (make-ast-int :value 1)
                                                                            (make-ast-int :value 2)))
                                                 (make-ast-int :value 3))))
                            (cons 'flag (make-ast-int :value 1)))
            :body (list (make-ast-if
                         :cond (make-ast-var :name 'flag)
                         :then (make-ast-call :func 'car :args (list (make-ast-var :name 'p)))
                         :else (make-ast-int :value 0))))))
  (ast)
  (let* ((ctx   (make-codegen-ctx))
         (reg   (compile-ast ast ctx))
         (insts (codegen-instructions ctx))
         (jump-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-jump-zero)) insts))
         (cons-pos (position-if (lambda (inst) (typep inst 'cl-cc/vm::vm-cons)) insts)))
    (assert-true (keywordp reg))
    (assert-true jump-pos)
    (assert-true cons-pos)
    (assert-true (> cons-pos jump-pos))))

(deftest codegen-let-noescape-array-aref-bypasses-vm-make-array-and-vm-aref
  "A non-escaping fixed-size local array can serve constant-index aref from split registers."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-let
                             :bindings (list (cons 'arr (make-ast-call
                                                        :func 'make-array
                                                        :args (list (make-ast-int :value 3)))))
                             :body (list (make-ast-call :func 'aref
                                                        :args (list (make-ast-var :name 'arr)
                                                                    (make-ast-int :value 1)))))
                           ctx)))
    (assert-true (keywordp reg))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-make-array))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-aref))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-move))))

(deftest codegen-let-noescape-array-length-bypasses-vm-make-array
  "A non-escaping fixed-size local array returns array-length without heap-backed array ops."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-let
                             :bindings (list (cons 'arr (make-ast-call
                                                        :func 'make-array
                                                        :args (list (make-ast-int :value 3)))))
                             :body (list (make-ast-call :func 'array-length
                                                        :args (list (make-ast-var :name 'arr)))))
                           ctx)))
    (assert-true (keywordp reg))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-make-array))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-const))))

(deftest codegen-let-noescape-array-variable-aref-bypasses-vm-aref
  "A non-escaping fixed-size local array can serve variable-index aref via bounded dispatch."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2))))
                                (cons 'i (make-ast-int :value 1)))
                :body (list (make-ast-call :func 'aref
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-var :name 'i)))))
               ctx)))
    (assert-true (keywordp reg))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-make-array))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-aref))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-num-eq))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-jump-zero))))

(deftest codegen-let-escaped-array-aref-falls-back-to-vm-aref
  "Captured array bindings keep the normal heap-backed make-array/aref path."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2)))))
                :body (list (make-ast-lambda :params '() :body (list (make-ast-var :name 'arr)))
                            (make-ast-call :func 'aref
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-int :value 0)))))
               ctx)))
    (assert-true (keywordp reg))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-make-array))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-aref))))

(deftest codegen-let-noescape-array-aset-bypasses-vm-make-array-and-vm-aset
  "A non-escaping fixed-size local array can update constant indices without heap-backed array ops."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast
               (make-ast-let
                :bindings (list (cons 'arr (make-ast-call
                                           :func 'make-array
                                           :args (list (make-ast-int :value 2)))))
                :body (list (make-ast-call :func 'aset
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-int :value 1)
                                                       (make-ast-int :value 42)))
                            (make-ast-call :func 'aref
                                           :args (list (make-ast-var :name 'arr)
                                                       (make-ast-int :value 1)))))
               ctx)))
    (assert-true (keywordp reg))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-make-array))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-aset))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-aref))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-move))))

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

(deftest codegen-setq-local-emits-move
  "Compiling setq on a local variable emits a vm-move."
  (let ((ctx (make-codegen-ctx)))
    (setf (cl-cc/compile::ctx-env ctx) (list (cons 'x :R0)))
    (compile-ast (make-ast-setq :var 'x :value (make-ast-int :value 99)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-move))))

(deftest codegen-print-emits-vm-print
  "Compiling print emits vm-print."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-print :expr (make-ast-int :value 42)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-print))))

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
