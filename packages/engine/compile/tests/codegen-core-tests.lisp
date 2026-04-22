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

(deftest-each ast-constant-folding-before-codegen
  "optimize-ast folds literal arithmetic and pure string-length calls to an integer node."
  :cases (("arithmetic"    6 '(+ 1 2 3))
          ("string-length" 5 '(string-length "hello")))
  (expected form)
  (let ((result (cl-cc/compile::optimize-ast (cl-cc/parse::lower-sexp-to-ast form))))
    (assert-true (cl-cc/ast::ast-int-p result))
    (assert-= expected (cl-cc/ast::ast-int-value result))))

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

(deftest codegen-result-vm-instructions-without-halt-strips-terminal-halt
  "%result-vm-instructions-without-halt removes only the final vm-halt instruction."
  (let* ((move (cl-cc:make-vm-move :dst :R1 :src :R0))
         (halt (cl-cc:make-vm-halt :reg :R1))
         (result (cl-cc/compile::make-compilation-result
                  :program (cl-cc:make-vm-program :instructions (list move halt) :result-register :R1)
                  :vm-instructions (list move halt))))
    (assert-equal (list move)
                  (cl-cc/compile::%result-vm-instructions-without-halt result))))

(deftest-compile-each codegen-toplevel-cps-semantic-preservation
  "Top-level CPS routing preserves common supported forms."
  :cases (("two-safe-forms" 7 "(+ 1 2) (+ 3 4)")
          ("defvar-then-use" 3 "(defvar *ulw-cps* 1) (+ *ulw-cps* 2)")
          ("call-bearing-form" 6 "(defun add1 (x) (+ x 1)) (add1 5)")
          ("apply" 6 "(apply + (list 1 2 3))")
          ("values-primary" 1 "(values 1 2 3)")
          ("multiple-value-bind" 3 "(multiple-value-bind (a b) (values 1 2) (+ a b))"))
  :stdlib nil)
