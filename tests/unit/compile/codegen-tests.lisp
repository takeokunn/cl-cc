;;;; tests/unit/compile/codegen-tests.lisp — Codegen Unit Tests
;;;
;;; Direct tests for compile-ast methods and codegen helpers.
;;; Tests compile individual AST nodes to VM instructions without
;;; going through the full pipeline (parser + macro expander).

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── Helpers ────────────────────────────────────────────────────────────

(defun make-codegen-ctx ()
  "Create a fresh compiler context for codegen tests."
  (make-instance 'cl-cc::compiler-context))

(defun codegen-instructions (ctx)
  "Return the emitted instructions from CTX (in order)."
  (nreverse (copy-list (cl-cc::ctx-instructions ctx))))

(defun codegen-find-inst (ctx type)
  "Find the first instruction of TYPE in CTX's emitted instructions."
  (find-if (lambda (i) (typep i type)) (codegen-instructions ctx)))

;;; ─── compile-ast: ast-int ───────────────────────────────────────────────

(deftest codegen-int-returns-register
  "Compiling an integer literal returns a register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-int :value 42) ctx)))
    (assert-true (keywordp reg))))

(deftest-each codegen-int-emits-const-value
  "Compiling an integer literal emits vm-const with the correct value."
  :cases (("positive"  42)
          ("zero"       0)
          ("negative"  -1))
  (n)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-int :value n) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
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
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-equal name (cl-cc::vm-const-value inst)))))

(deftest codegen-var-local-returns-register
  "Compiling a local variable returns its bound register."
  (let* ((ctx (make-codegen-ctx))
         (reg :R99))
    (setf (cl-cc::ctx-env ctx) (list (cons 'x reg)))
    (let ((result (compile-ast (make-ast-var :name 'x) ctx)))
      (assert-eq reg result))))

(deftest codegen-var-unbound-signals-error
  "Compiling an unbound variable signals an error."
  (let ((ctx (make-codegen-ctx)))
    (assert-signals error
      (compile-ast (make-ast-var :name 'nonexistent-var-xyz) ctx))))

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
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-equal datum (cl-cc::vm-const-value inst)))))

;;; ─── compile-ast: ast-binop ─────────────────────────────────────────────

(deftest-each codegen-binop-emits-correct-instruction
  "Each binary operator compiles to its corresponding VM instruction."
  :cases (("add" '+ 'cl-cc::vm-add)
          ("sub" '- 'cl-cc::vm-sub)
          ("mul" '* 'cl-cc::vm-mul)
          ("lt"  '< 'cl-cc::vm-lt)
          ("gt"  '> 'cl-cc::vm-gt)
          ("eq"  '= 'cl-cc::vm-num-eq))
  (op inst-type)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-binop :op op
                                  :lhs (make-ast-int :value 1)
                                  :rhs (make-ast-int :value 2))
                 ctx)
    (assert-true (codegen-find-inst ctx inst-type))))

;;; ─── compile-ast: ast-if ────────────────────────────────────────────────

(deftest codegen-if-compilation
  "Compiling an if-form: emits vm-jump-zero, 2 branch labels, returns a register."
  (let* ((ctx  (make-codegen-ctx))
         (reg  (compile-ast (make-ast-if :cond (make-ast-var :name t)
                                          :then (make-ast-int :value 1)
                                          :else (make-ast-int :value 2))
                            ctx))
         (insts  (codegen-instructions ctx))
         (labels (remove-if-not (lambda (i) (typep i 'cl-cc::vm-label)) insts)))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-jump-zero))
    (assert-=    2 (length labels))
    (assert-true (keywordp reg))))

;;; ─── compile-ast: ast-progn ─────────────────────────────────────────────

(deftest codegen-progn-compilation
  "Compiling a progn returns a register and emits instructions for all sub-forms."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-progn
                             :forms (list (make-ast-int :value 1)
                                          (make-ast-int :value 2)
                                          (make-ast-int :value 3)))
                           ctx))
         (consts (remove-if-not (lambda (i) (typep i 'cl-cc::vm-const))
                                (codegen-instructions ctx))))
    (assert-true (keywordp reg))
    (assert-=    3 (length consts))))

;;; ─── compile-ast: ast-let ───────────────────────────────────────────────

(deftest codegen-let-compilation
  "Compiling a let returns a register; body can reference the bound variable."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-let
                             :bindings (list (cons 'x (make-ast-int :value 42)))
                             :body (list (make-ast-var :name 'x)))
                           ctx)))
    (assert-true (keywordp reg))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-const))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-move))))

;;; ─── compile-ast: ast-setq ─────────────────────────────────────────────

(deftest codegen-setq-local-emits-move
  "Compiling setq on a local variable emits a vm-move."
  (let* ((ctx (make-codegen-ctx)))
    (setf (cl-cc::ctx-env ctx) (list (cons 'x :R0)))
    (compile-ast (make-ast-setq :var 'x :value (make-ast-int :value 99)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-move))))

;;; ─── binop-ctor ─────────────────────────────────────────────────────────

(deftest codegen-binop-ctor
  "binop-ctor returns constructors for all standard ops; signals error for unknown op."
  (dolist (op '(+ - * = < > <= >=))
    (assert-true (functionp (cl-cc::binop-ctor op))))
  (assert-signals error
    (cl-cc::binop-ctor 'unknown-op-xyz)))

;;; ─── compile-ast: ast-print ─────────────────────────────────────────────

(deftest codegen-print-emits-vm-print
  "Compiling a print form emits vm-print."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-print :expr (make-ast-int :value 42)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-print))))

;;; ─── compile-ast: ast-the ────────────────────────────────────────────────

(deftest codegen-the-compiles-inner
  "Compiling (the integer 42) compiles the inner form and returns a register."
  (let* ((ctx (make-codegen-ctx))
         ;; ast-the without typed-fn mode just compiles the inner form
         (reg (compile-ast (cl-cc:make-ast-the :type 'integer
                                          :value (make-ast-int :value 42))
                           ctx)))
    (assert-true (keywordp reg))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-const))))

;;; ─── compile-ast: ast-function ───────────────────────────────────────────

(deftest codegen-function-ref-returns-register
  "Compiling #'fn returns a register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-function :name 'car) ctx)))
    (assert-true (keywordp reg))))

;;; ─── compile-ast: ast-defun ──────────────────────────────────────────────

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

;;; ─── compile-ast: ast-lambda ─────────────────────────────────────────────

(deftest codegen-lambda-returns-register
  "Compiling lambda returns a register holding the closure."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-lambda :params '(x)
                                             :body (list (make-ast-int :value 1)))
                           ctx)))
    (assert-true (keywordp reg))))

;;; ─── compile-ast: ast-defvar ─────────────────────────────────────────────

(deftest codegen-defvar-compilation
  "Compiling defvar registers in global-variables and emits vm-const for the value."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-defvar :name 'test-codegen-var
                                   :value (make-ast-int :value 99))
                 ctx)
    (assert-true (gethash 'test-codegen-var (cl-cc::ctx-global-variables ctx)))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-const))))

;;; ─── compile-ast: ast-call ───────────────────────────────────────────────

(deftest codegen-call-known-function
  "Compiling a call to a known function emits instructions."
  (let ((ctx (make-codegen-ctx)))
    ;; cons is a known builtin
    (compile-ast (make-ast-call :func 'cons
                                 :args (list (make-ast-int :value 1)
                                             (make-ast-int :value 2)))
                 ctx)
    ;; Should emit vm-cons
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

;;; ─── compile-ast: ast-block / ast-return-from ────────────────────────────

(deftest codegen-block-compilation
  "Compiling a block returns a register and emits an exit label."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-block :name 'my-block
                                            :body (list (make-ast-int :value 42)))
                           ctx)))
    (assert-true (keywordp reg))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-label))))

;;; ─── compile-ast: ast-tagbody / ast-go ───────────────────────────────────

(deftest codegen-tagbody-compilation
  "Compiling a tagbody emits a label per tag and returns a register."
  (let* ((ctx  (make-codegen-ctx))
         (reg  (compile-ast (make-ast-tagbody
                              :tags (list (cons 'tag1 (list (make-ast-int :value 1)))
                                          (cons 'tag2 (list (make-ast-int :value 2)))))
                            ctx))
         (labels (remove-if-not (lambda (i) (typep i 'cl-cc::vm-label))
                                (codegen-instructions ctx))))
    (assert-true (>= (length labels) 2))
    (assert-true (keywordp reg))))

;;; ─── compile-ast: ast-catch / ast-throw ──────────────────────────────────

(deftest codegen-catch-compilation
  "Compiling catch emits establish-catch, labels, and returns a register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-catch
                             :tag  (make-ast-quote :value 'my-tag)
                             :body (list (make-ast-int :value 42)))
                           ctx)))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-establish-catch))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-label))
    (assert-true (keywordp reg))))

(deftest codegen-throw-compiles-tag-and-value
  "Compiling throw emits vm-throw with tag and value registers."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-throw
                   :tag (make-ast-quote :value 'my-tag)
                   :value (make-ast-int :value 42))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-throw))
    ;; Should have at least 2 vm-const: one for 'my-tag, one for 42
    (let* ((insts (codegen-instructions ctx))
           (consts (remove-if-not (lambda (i) (typep i 'cl-cc::vm-const)) insts)))
      (assert-true (>= (length consts) 2)))))

;;; ─── codegen helpers ─────────────────────────────────────────────────────

(deftest codegen-make-register-increments
  "make-register returns incrementing registers."
  (let* ((ctx (make-codegen-ctx))
         (r1 (cl-cc::make-register ctx))
         (r2 (cl-cc::make-register ctx)))
    (assert-true (keywordp r1))
    (assert-true (keywordp r2))
    (assert-false (eq r1 r2))))

(deftest codegen-emit-appends
  "emit adds instruction to context."
  (let ((ctx (make-codegen-ctx)))
    (cl-cc::emit ctx (cl-cc::make-vm-const :dst :R0 :value 42))
    (assert-= 1 (length (codegen-instructions ctx)))))

(deftest codegen-make-label-unique
  "make-label returns unique labels."
  (let* ((ctx (make-codegen-ctx))
         (l1 (cl-cc::make-label ctx "TEST"))
         (l2 (cl-cc::make-label ctx "TEST")))
    (assert-false (string= l1 l2))))

;;; ─── compile-ast: ast-defclass ───────────────────────────────────────────────

(defun make-test-slot (name &key initarg)
  "Build a minimal ast-slot-def for use in codegen tests."
  (cl-cc::make-ast-slot-def :name name :initarg initarg))

(deftest codegen-defclass-compilation
  "Compiling defclass: emits vm-class-def with correct slot names, registers globally, returns register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc::make-ast-defclass
                             :name 'my-rect
                             :superclasses nil
                             :slots (list (make-test-slot 'w :initarg :w)
                                          (make-test-slot 'h :initarg :h)))
                           ctx))
         (inst (codegen-find-inst ctx 'cl-cc::vm-class-def)))
    (assert-true inst)
    (assert-equal '(w h) (cl-cc::vm-slot-names inst))
    (assert-true (gethash 'my-rect (cl-cc::ctx-global-classes ctx)))
    (assert-true (keywordp reg))))

;;; ─── compile-ast: ast-defgeneric ─────────────────────────────────────────────

(deftest codegen-defgeneric-compilation
  "Compiling defgeneric emits vm-class-def dispatch table and registers in global-generics."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-defgeneric :name 'my-speak :params '(animal))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-class-def))
    (assert-true (gethash 'my-speak (cl-cc::ctx-global-generics ctx)))))

(deftest codegen-defgeneric-idempotent
  "Compiling the same defgeneric twice reuses the existing dispatch register."
  (let ((ctx (make-codegen-ctx)))
    (let ((r1 (compile-ast (cl-cc::make-ast-defgeneric :name 'my-compute :params '(x)) ctx))
          (r2 (compile-ast (cl-cc::make-ast-defgeneric :name 'my-compute :params '(x)) ctx)))
      (assert-eq r1 r2))))

;;; ─── compile-ast: ast-defmethod ──────────────────────────────────────────────

(deftest codegen-defmethod-compilation
  "Compiling defmethod emits vm-register-method (with correct specializer) and vm-closure."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-defgeneric :name 'my-greet :params '(obj)) ctx)
    (compile-ast (cl-cc::make-ast-defmethod
                  :name 'my-greet
                  :specializers (list '(obj . dog))
                  :params '(obj)
                  :body (list (cl-cc::make-ast-int :value 99)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-register-method)))
      (assert-true inst)
      (assert-equal '(dog) (cl-cc::vm-method-specializer inst)))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-closure))))

;;; ─── compile-ast: ast-make-instance ─────────────────────────────────────────

(deftest-each codegen-make-instance-emits-vm-make-obj
  "make-instance emits vm-make-obj regardless of static vs dynamic class reference."
  :cases (("static"  (cl-cc::make-ast-make-instance
                       :class (cl-cc::make-ast-quote :value 'my-dog)
                       :initargs (list (cons :name (cl-cc::make-ast-quote :value 'rex))))
                     nil)
          ("dynamic" (cl-cc::make-ast-make-instance
                       :class (cl-cc::make-ast-var :name 'cls)
                       :initargs nil)
                     (list (cons 'cls :R50))))
  (ast env-setup)
  (let ((ctx (make-codegen-ctx)))
    (when env-setup (setf (cl-cc::ctx-env ctx) env-setup))
    (compile-ast ast ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-obj))))

(deftest codegen-make-instance-static-loads-class-globally
  "Static make-instance emits vm-get-global to load the class descriptor."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-make-instance
                  :class (cl-cc::make-ast-quote :value 'my-cat)
                  :initargs nil)
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-get-global))))

;;; ─── compile-ast: ast-slot-value ─────────────────────────────────────────────

(deftest codegen-slot-value
  "slot-value emits vm-slot-read with correct slot name and returns a register."
  (let* ((ctx (make-codegen-ctx)))
    (setf (cl-cc::ctx-env ctx) (list (cons 'obj :R42)))
    (let* ((reg  (compile-ast (cl-cc::make-ast-slot-value
                                :object (cl-cc::make-ast-var :name 'obj)
                                :slot 'radius)
                               ctx))
           (inst (codegen-find-inst ctx 'cl-cc::vm-slot-read)))
      (assert-true inst)
      (assert-eq 'radius (cl-cc::vm-slot-name-sym inst))
      (assert-true (keywordp reg)))))

;;; ─── compile-ast: ast-set-slot-value ─────────────────────────────────────────

(deftest codegen-set-slot-value
  "set-slot-value emits vm-slot-write with correct slot name and returns a register."
  (let* ((ctx (make-codegen-ctx)))
    (setf (cl-cc::ctx-env ctx) (list (cons 'obj :R60)))
    (let* ((reg  (compile-ast (cl-cc::make-ast-set-slot-value
                                :object (cl-cc::make-ast-var :name 'obj)
                                :slot 'weight
                                :value (cl-cc::make-ast-int :value 42))
                               ctx))
           (inst (codegen-find-inst ctx 'cl-cc::vm-slot-write)))
      (assert-true inst)
      (assert-eq 'weight (cl-cc::vm-slot-name-sym inst))
      (assert-true (keywordp reg)))))

;;; ─── compile-ast: ast-values ─────────────────────────────────────────────

(deftest codegen-values-compilation
  "Compiling ast-values emits vm-values and returns a register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc::make-ast-values
                             :forms (list (make-ast-int :value 1)
                                          (make-ast-int :value 2)
                                          (make-ast-int :value 3)))
                           ctx)))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-values))
    (assert-true (keywordp reg))))

(deftest codegen-values-basic-run
  "values returns all values accessible via multiple-value-list."
  (assert-equal '(1 2 3)
    (run-string "(multiple-value-list (values 1 2 3))")))

(deftest codegen-values-single-run
  "values with one argument returns that argument."
  (assert-run= 42 "(values 42)"))

;;; ─── compile-ast: ast-multiple-value-bind ───────────────────────────────

(deftest codegen-mvb-compilation
  "Compiling multiple-value-bind emits vm-mv-bind and returns a register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc::make-ast-multiple-value-bind
                             :vars '(a b)
                             :values-form (cl-cc::make-ast-values
                                           :forms (list (make-ast-int :value 1)
                                                        (make-ast-int :value 2)))
                             :body (list (make-ast-var :name 'a)))
                           ctx)))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-mv-bind))
    (assert-true (keywordp reg))))

(deftest-each codegen-mvb-run
  "multiple-value-bind binds values and evaluates body correctly."
  :cases (("sum-values"  3  "(multiple-value-bind (a b) (values 1 2) (+ a b))")
          ("first-value" 10 "(multiple-value-bind (x y) (values 10 20) x)"))
  (expected code)
  (assert-run= expected code))

;;; ─── compile-ast: ast-multiple-value-call ───────────────────────────────

(deftest codegen-mv-call-compilation
  "Compiling ast-multiple-value-call emits vm-apply and returns a register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc::make-ast-multiple-value-call
                             :func (make-ast-function :name '+)
                             :args (list (cl-cc::make-ast-values
                                          :forms (list (make-ast-int :value 1)
                                                       (make-ast-int :value 2)))))
                           ctx)))
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-apply))
    (assert-true (keywordp reg))))

(deftest codegen-mv-call-basic-run
  "multiple-value-call spreads values as arguments to a user-defined function."
  (assert-run= 3
    "(multiple-value-call (lambda (a b) (+ a b)) (values 1 2))"))

(deftest codegen-mv-call-cons-run
  "multiple-value-call with cons spreads two values into car and cdr."
  (assert-equal '(1 . 2)
    (run-string "(multiple-value-call #'cons (values 1 2))")))

;;; ─── compile-ast: ast-multiple-value-prog1 ──────────────────────────────

(deftest codegen-mv-prog1-compilation
  "Compiling ast-multiple-value-prog1: returns a register and emits all sub-form constants."
  (let* ((ctx    (make-codegen-ctx))
         (reg    (compile-ast (cl-cc::make-ast-multiple-value-prog1
                                :first (make-ast-int :value 1)
                                :forms (list (make-ast-int :value 2)
                                             (make-ast-int :value 3)))
                              ctx))
         (consts (remove-if-not (lambda (i) (typep i 'cl-cc::vm-const))
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

;;; ─── compile-ast: ast-unwind-protect ────────────────────────────────────

(deftest-each codegen-exception-form-emits-establish-handler
  "Both unwind-protect and handler-case emit vm-establish-handler."
  :cases (("unwind-protect" (cl-cc::make-ast-unwind-protect
                              :protected (make-ast-int :value 42)
                              :cleanup (list (make-ast-int :value 0))))
          ("handler-case"   (cl-cc::make-ast-handler-case
                              :form (make-ast-int :value 42)
                              :clauses (list (list 'error 'e (make-ast-int :value 0))))))
  (ast)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast ast ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-establish-handler))))

(deftest codegen-unwind-protect-returns-register
  "Compiling ast-unwind-protect returns a register keyword."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc::make-ast-unwind-protect
                             :protected (make-ast-int :value 7)
                             :cleanup (list (make-ast-int :value 0)))
                           ctx)))
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

;;; ─── compile-ast: ast-handler-case ──────────────────────────────────────

(deftest codegen-handler-case-returns-register
  "Compiling ast-handler-case returns a register keyword."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc::make-ast-handler-case
                             :form (make-ast-int :value 10)
                             :clauses (list (list 'error nil (make-ast-int :value 0))))
                           ctx)))
    (assert-true (keywordp reg))))

(deftest codegen-handler-case-normal-result-run
  "handler-case returns the protected form value when no condition is signaled."
  (assert-run= 42
    "(handler-case 42 (error (e) -1))"))

(deftest codegen-handler-case-catches-error-run
  "handler-case invokes the matching handler when a condition is signaled."
  (assert-run= 99
    "(handler-case (error \"boom\") (error (e) 99))"))

;;; ─── compile-ast: ast-flet ───────────────────────────────────────────────

(deftest codegen-flet-basic-run
  "flet defines and calls a local function in the body."
  (assert-run= 10
    "(flet ((double (x) (+ x x))) (double 5))"))

(deftest codegen-flet-multiple-fns-run
  "flet can define multiple independent local functions."
  (assert-run= 9
    "(flet ((add1 (x) (+ x 1))
            (mul2 (x) (* x 2)))
       (add1 (mul2 4)))"))

(deftest codegen-flet-captures-outer-binding-run
  "flet closures capture variables from the enclosing lexical scope."
  (assert-run= 15
    "(let ((base 10))
       (flet ((add-base (x) (+ x base)))
         (add-base 5)))"))

;;; ─── compile-ast: ast-labels ─────────────────────────────────────────────

(deftest-each codegen-labels-run
  "labels defines local and recursive functions accessible in the body."
  :cases (("basic"     5   "(labels ((f (x) x)) (f 5))")
          ("recursive" 120 "(labels ((fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (fact 5))"))
  (expected code)
  (assert-run= expected code))

(deftest codegen-labels-mutual-recursion-run
  "labels supports mutually-recursive local functions."
  (assert-equal t
    (run-string "(labels ((even? (n) (if (= n 0) t (odd? (- n 1))))
                           (odd?  (n) (if (= n 0) nil (even? (- n 1)))))
                   (even? 4))")))

;;; ─── compile-ast: ast-apply ──────────────────────────────────────────────

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

;;; ─── %resolve-func-sym-reg ──────────────────────────────────────────────

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
    ;; Mark gfn as global
    (setf (gethash 'gfn (cl-cc::ctx-global-functions ctx)) t)
    (cl-cc::%resolve-func-sym-reg 'gfn ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-eq 'gfn (cl-cc::vm-const-value inst)))))

;;; ─── %compile-closure-body (flet/labels helper) ─────────────────────────

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
