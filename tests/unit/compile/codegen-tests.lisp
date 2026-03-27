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

(deftest codegen-int-emits-const
  "Compiling an integer literal emits a vm-const instruction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-int :value 42) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-= 42 (cl-cc::vm-const-value inst)))))

(deftest codegen-int-zero
  "Compiling integer 0 works."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-int :value 0) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-= 0 (cl-cc::vm-const-value inst)))))

(deftest codegen-int-negative
  "Compiling negative integer works."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-int :value -1) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-= -1 (cl-cc::vm-const-value inst)))))

;;; ─── compile-ast: ast-var ───────────────────────────────────────────────

(deftest codegen-var-t-is-const
  "Compiling variable T emits vm-const with value T."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-var :name t) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-eq t (cl-cc::vm-const-value inst)))))

(deftest codegen-var-nil-is-const
  "Compiling variable NIL emits vm-const with value NIL."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-var :name nil) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-null (cl-cc::vm-const-value inst)))))

(deftest codegen-var-keyword-is-const
  "Compiling a keyword emits vm-const."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-var :name :foo) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-eq :foo (cl-cc::vm-const-value inst)))))

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

(deftest codegen-quote-symbol
  "Compiling a quoted symbol emits vm-const."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-quote :value 'hello) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-eq 'hello (cl-cc::vm-const-value inst)))))

(deftest codegen-quote-list
  "Compiling a quoted list emits vm-const."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-quote :value '(1 2 3)) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-equal '(1 2 3) (cl-cc::vm-const-value inst)))))

(deftest codegen-quote-nil
  "Compiling (quote nil) emits vm-const nil."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-quote :value nil) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-null (cl-cc::vm-const-value inst)))))

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

(deftest codegen-if-emits-jump-zero
  "Compiling an if-form emits a vm-jump-zero for the condition."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-if :cond (make-ast-var :name t)
                               :then (make-ast-int :value 1)
                               :else (make-ast-int :value 2))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-jump-zero))))

(deftest codegen-if-emits-labels
  "Compiling an if-form emits else and end labels."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-if :cond (make-ast-var :name t)
                               :then (make-ast-int :value 1)
                               :else (make-ast-int :value 2))
                 ctx)
    (let* ((insts (codegen-instructions ctx))
           (labels (remove-if-not (lambda (i) (typep i 'cl-cc::vm-label)) insts)))
      (assert-= 2 (length labels)))))

(deftest codegen-if-returns-register
  "Compiling an if-form returns a destination register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-if :cond (make-ast-var :name t)
                                         :then (make-ast-int :value 1)
                                         :else (make-ast-int :value 2))
                           ctx)))
    (assert-true (keywordp reg))))

;;; ─── compile-ast: ast-progn ─────────────────────────────────────────────

(deftest codegen-progn-returns-last
  "Compiling a progn returns the register of the last form."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-progn
                            :forms (list (make-ast-int :value 1)
                                         (make-ast-int :value 42)))
                           ctx)))
    (assert-true (keywordp reg))))

(deftest codegen-progn-emits-all
  "Compiling a progn emits instructions for all sub-forms."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-progn
                  :forms (list (make-ast-int :value 1)
                               (make-ast-int :value 2)
                               (make-ast-int :value 3)))
                 ctx)
    (let* ((insts (codegen-instructions ctx))
           (consts (remove-if-not (lambda (i) (typep i 'cl-cc::vm-const)) insts)))
      (assert-= 3 (length consts)))))

;;; ─── compile-ast: ast-let ───────────────────────────────────────────────

(deftest codegen-let-binds-and-compiles-body
  "Compiling a let emits instructions for bindings and body."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-let
                            :bindings (list (cons 'x (make-ast-int :value 10)))
                            :body (list (make-ast-var :name 'x)))
                           ctx)))
    (assert-true (keywordp reg))))

(deftest codegen-let-body-sees-binding
  "Let body can reference the bound variable."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-let
                  :bindings (list (cons 'x (make-ast-int :value 42)))
                  :body (list (make-ast-var :name 'x)))
                 ctx)
    ;; Should have at least a vm-const for 42 and a vm-move for the binding
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

(deftest codegen-binop-ctor-add
  "binop-ctor returns make-vm-add for '+"
  (let ((ctor (cl-cc::binop-ctor '+)))
    (assert-true (functionp ctor))))

(deftest codegen-binop-ctor-generic
  "binop-ctor for + returns a callable function constructor."
  (let ((ctor (cl-cc::binop-ctor '+)))
    (assert-true (functionp ctor))))

(deftest codegen-binop-ctor-unknown-signals-error
  "binop-ctor signals error for unknown operator."
  (assert-signals error
    (cl-cc::binop-ctor 'unknown-op-xyz)))

(deftest codegen-binop-ctor-all-ops
  "All standard binop operators have constructors."
  (dolist (op '(+ - * = < > <= >=))
    (assert-true (functionp (cl-cc::binop-ctor op)))))

;;; ─── compile-ast: ast-print ─────────────────────────────────────────────

(deftest codegen-print-emits-vm-print
  "Compiling a print form emits vm-print."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-print :expr (make-ast-int :value 42)) ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-print))))

;;; ─── compile-ast: ast-quote (string values) ─────────────────────────────

(deftest codegen-quote-string-emits-const
  "Compiling a quoted string emits vm-const with the string value."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-quote :value "hello") ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-string= "hello" (cl-cc::vm-const-value inst)))))

(deftest codegen-quote-empty-string
  "Compiling a quoted empty string works."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-quote :value "") ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-string= "" (cl-cc::vm-const-value inst)))))

(deftest codegen-quote-char
  "Compiling a quoted character emits vm-const."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-quote :value #\a) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-const)))
      (assert-true inst)
      (assert-true (char= #\a (cl-cc::vm-const-value inst))))))

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

(deftest codegen-defun-emits-closure
  "Compiling defun emits a vm-closure instruction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-defun :name 'my-fn
                                  :params '(x)
                                  :body (list (make-ast-var :name 'x)))
                 ctx)
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

(deftest codegen-lambda-emits-closure
  "Compiling lambda emits a vm-closure instruction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-lambda :params '(x)
                                   :body (list (make-ast-var :name 'x)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-closure))))

(deftest codegen-lambda-returns-register
  "Compiling lambda returns a register holding the closure."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-lambda :params '(x)
                                             :body (list (make-ast-int :value 1)))
                           ctx)))
    (assert-true (keywordp reg))))

;;; ─── compile-ast: ast-defvar ─────────────────────────────────────────────

(deftest codegen-defvar-registers-global
  "Compiling defvar registers the variable in global-variables."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-defvar :name 'test-codegen-var
                                   :value (make-ast-int :value 42))
                 ctx)
    (assert-true (gethash 'test-codegen-var (cl-cc::ctx-global-variables ctx)))))

(deftest codegen-defvar-emits-const-for-value
  "Compiling defvar with an initial value emits vm-const for that value."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-defvar :name 'test-codegen-var2
                                   :value (make-ast-int :value 99))
                 ctx)
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

(deftest codegen-call-car
  "Compiling (car x) emits vm-car."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'car
                                 :args (list (make-ast-quote :value '(1 2))))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-car))))

(deftest codegen-call-cdr
  "Compiling (cdr x) emits vm-cdr."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-call :func 'cdr
                                 :args (list (make-ast-quote :value '(1 2))))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-cdr))))

;;; ─── compile-ast: ast-block / ast-return-from ────────────────────────────

(deftest codegen-block-compiles-body
  "Compiling a block compiles its body and returns a register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-block :name 'my-block
                                            :body (list (make-ast-int :value 42)))
                           ctx)))
    (assert-true (keywordp reg))))

(deftest codegen-block-emits-label
  "Compiling a block emits a label for the block exit."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-block :name 'my-block
                                  :body (list (make-ast-int :value 42)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-label))))

;;; ─── compile-ast: ast-tagbody / ast-go ───────────────────────────────────

(deftest codegen-tagbody-emits-labels
  "Compiling a tagbody emits labels for each tag."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-tagbody
                   :tags (list (cons 'tag1 (list (make-ast-int :value 1)))
                               (cons 'tag2 (list (make-ast-int :value 2)))))
                 ctx)
    (let* ((insts (codegen-instructions ctx))
           (labels (remove-if-not (lambda (i) (typep i 'cl-cc::vm-label)) insts)))
      ;; At least 2 labels for tag1 and tag2
      (assert-true (>= (length labels) 2)))))

(deftest codegen-tagbody-returns-nil-register
  "Compiling a tagbody returns a register (value is nil)."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-tagbody
                             :tags (list (cons 'tag1 (list (make-ast-int :value 1)))))
                           ctx)))
    (assert-true (keywordp reg))))

;;; ─── compile-ast: ast-catch / ast-throw ──────────────────────────────────

(deftest codegen-catch-emits-labels
  "Compiling catch emits labels for the catch body."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-catch
                   :tag (make-ast-quote :value 'my-tag)
                   :body (list (make-ast-int :value 42)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-label))))

(deftest codegen-catch-returns-register
  "Compiling catch returns a result register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-catch
                             :tag (make-ast-quote :value 'my-tag)
                             :body (list (make-ast-int :value 42)))
                           ctx)))
    (assert-true (keywordp reg))))

(deftest codegen-throw-compiles-tag-and-value
  "Compiling throw compiles both tag and value expressions."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-throw
                   :tag (make-ast-quote :value 'my-tag)
                   :value (make-ast-int :value 42))
                 ctx)
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

(deftest codegen-defclass-emits-vm-class-def
  "Compiling defclass emits a vm-class-def instruction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-defclass
                  :name 'my-point
                  :superclasses nil
                  :slots (list (make-test-slot 'x :initarg :x)
                               (make-test-slot 'y :initarg :y)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-class-def))))

(deftest codegen-defclass-slot-names-correct
  "Compiled vm-class-def carries the declared slot names."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-defclass
                  :name 'my-rect
                  :superclasses nil
                  :slots (list (make-test-slot 'w :initarg :w)
                               (make-test-slot 'h :initarg :h)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-class-def)))
      (assert-true inst)
      (assert-equal '(w h) (cl-cc::vm-slot-names inst)))))

(deftest codegen-defclass-registers-global-class
  "Compiling defclass registers the name in ctx-global-classes."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-defclass
                  :name 'my-box
                  :superclasses nil
                  :slots (list (make-test-slot 'val :initarg :val)))
                 ctx)
    (assert-true (gethash 'my-box (cl-cc::ctx-global-classes ctx)))))

(deftest codegen-defclass-returns-register
  "Compiling defclass returns a register keyword."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc::make-ast-defclass
                             :name 'my-circle
                             :superclasses nil
                             :slots (list (make-test-slot 'radius :initarg :radius)))
                            ctx)))
    (assert-true (keywordp reg))))

;;; ─── compile-ast: ast-defgeneric ─────────────────────────────────────────────

(deftest codegen-defgeneric-emits-vm-class-def
  "Compiling defgeneric emits a vm-class-def (used as dispatch table)."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-defgeneric :name 'my-area :params '(shape))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-class-def))))

(deftest codegen-defgeneric-registers-in-global-generics
  "Compiling defgeneric registers the name in ctx-global-generics."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-defgeneric :name 'my-speak :params '(animal))
                 ctx)
    (assert-true (gethash 'my-speak (cl-cc::ctx-global-generics ctx)))))

(deftest codegen-defgeneric-idempotent
  "Compiling the same defgeneric twice reuses the existing dispatch register."
  (let ((ctx (make-codegen-ctx)))
    (let ((r1 (compile-ast (cl-cc::make-ast-defgeneric :name 'my-compute :params '(x)) ctx))
          (r2 (compile-ast (cl-cc::make-ast-defgeneric :name 'my-compute :params '(x)) ctx)))
      (assert-eq r1 r2))))

;;; ─── compile-ast: ast-defmethod ──────────────────────────────────────────────

(deftest codegen-defmethod-emits-vm-register-method
  "Compiling defmethod emits a vm-register-method instruction."
  (let ((ctx (make-codegen-ctx)))
    ;; Pre-register the generic so defmethod finds it
    (compile-ast (cl-cc::make-ast-defgeneric :name 'my-leg-count :params '(a)) ctx)
    (compile-ast (cl-cc::make-ast-defmethod
                  :name 'my-leg-count
                  :specializers (list '(a . animal))
                  :params '(a)
                  :body (list (cl-cc::make-ast-int :value 4)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-register-method))))

(deftest codegen-defmethod-specializer-in-dispatch-key
  "vm-register-method specializer matches the declared class name."
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
      (assert-equal '(dog) (cl-cc::vm-method-specializer inst)))))

(deftest codegen-defmethod-emits-closure
  "Compiling defmethod emits a vm-closure for the method body."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-defgeneric :name 'my-info :params '(s)) ctx)
    (compile-ast (cl-cc::make-ast-defmethod
                  :name 'my-info
                  :specializers (list '(s . shape))
                  :params '(s)
                  :body (list (cl-cc::make-ast-int :value 1)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-closure))))

;;; ─── compile-ast: ast-make-instance ─────────────────────────────────────────

(deftest codegen-make-instance-static-emits-vm-make-obj
  "Compiling (make-instance 'cls :k v) emits vm-make-obj (static class path)."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-make-instance
                  :class (cl-cc::make-ast-quote :value 'my-dog)
                  :initargs (list (cons :name (cl-cc::make-ast-quote :value 'rex))))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-obj))))

(deftest codegen-make-instance-static-loads-class-globally
  "Static make-instance emits vm-get-global to load the class descriptor."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-make-instance
                  :class (cl-cc::make-ast-quote :value 'my-cat)
                  :initargs nil)
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-get-global))))

(deftest codegen-make-instance-dynamic-emits-vm-make-obj
  "Compiling make-instance with dynamic class variable also emits vm-make-obj."
  (let* ((ctx (make-codegen-ctx))
         (cls-reg :R50))
    ;; Pre-bind a local variable to simulate a dynamic class reference
    (setf (cl-cc::ctx-env ctx) (list (cons 'cls cls-reg)))
    (compile-ast (cl-cc::make-ast-make-instance
                  :class (cl-cc::make-ast-var :name 'cls)
                  :initargs nil)
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-make-obj))))

;;; ─── compile-ast: ast-slot-value ─────────────────────────────────────────────

(deftest codegen-slot-value-emits-vm-slot-read
  "Compiling slot-value emits a vm-slot-read instruction."
  (let* ((ctx (make-codegen-ctx))
         (obj-reg :R42))
    (setf (cl-cc::ctx-env ctx) (list (cons 'obj obj-reg)))
    (compile-ast (cl-cc::make-ast-slot-value
                  :object (cl-cc::make-ast-var :name 'obj)
                  :slot 'x)
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-slot-read))))

(deftest codegen-slot-value-slot-name-correct
  "vm-slot-read carries the correct slot name."
  (let* ((ctx (make-codegen-ctx))
         (obj-reg :R43))
    (setf (cl-cc::ctx-env ctx) (list (cons 'p obj-reg)))
    (compile-ast (cl-cc::make-ast-slot-value
                  :object (cl-cc::make-ast-var :name 'p)
                  :slot 'radius)
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-slot-read)))
      (assert-true inst)
      (assert-eq 'radius (cl-cc::vm-slot-name-sym inst)))))

(deftest codegen-slot-value-returns-register
  "Compiling slot-value returns a register keyword."
  (let* ((ctx (make-codegen-ctx))
         (obj-reg :R44))
    (setf (cl-cc::ctx-env ctx) (list (cons 'q obj-reg)))
    (let ((reg (compile-ast (cl-cc::make-ast-slot-value
                              :object (cl-cc::make-ast-var :name 'q)
                              :slot 'color)
                             ctx)))
      (assert-true (keywordp reg)))))

;;; ─── compile-ast: ast-set-slot-value ─────────────────────────────────────────

(deftest codegen-set-slot-value-emits-vm-slot-write
  "Compiling (setf (slot-value obj 'slot) val) emits vm-slot-write."
  (let* ((ctx (make-codegen-ctx))
         (obj-reg :R60))
    (setf (cl-cc::ctx-env ctx) (list (cons 'b obj-reg)))
    (compile-ast (cl-cc::make-ast-set-slot-value
                  :object (cl-cc::make-ast-var :name 'b)
                  :slot 'content
                  :value (cl-cc::make-ast-int :value 77))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-slot-write))))

(deftest codegen-set-slot-value-slot-name-correct
  "vm-slot-write carries the correct slot name."
  (let* ((ctx (make-codegen-ctx))
         (obj-reg :R61))
    (setf (cl-cc::ctx-env ctx) (list (cons 'c obj-reg)))
    (compile-ast (cl-cc::make-ast-set-slot-value
                  :object (cl-cc::make-ast-var :name 'c)
                  :slot 'weight
                  :value (cl-cc::make-ast-int :value 5))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc::vm-slot-write)))
      (assert-true inst)
      (assert-eq 'weight (cl-cc::vm-slot-name-sym inst)))))

(deftest codegen-set-slot-value-returns-value-register
  "Compiling set-slot-value returns a register (the assigned value)."
  (let* ((ctx (make-codegen-ctx))
         (obj-reg :R62))
    (setf (cl-cc::ctx-env ctx) (list (cons 'd obj-reg)))
    (let ((reg (compile-ast (cl-cc::make-ast-set-slot-value
                              :object (cl-cc::make-ast-var :name 'd)
                              :slot 'flag
                              :value (cl-cc::make-ast-int :value 1))
                             ctx)))
      (assert-true (keywordp reg)))))

;;; ─── compile-ast: ast-values ─────────────────────────────────────────────

(deftest codegen-values-emits-vm-values
  "Compiling ast-values emits a vm-values instruction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-values
                  :forms (list (make-ast-int :value 1)
                               (make-ast-int :value 2)
                               (make-ast-int :value 3)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-values))))

(deftest codegen-values-returns-register
  "Compiling ast-values returns a register keyword."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc::make-ast-values
                             :forms (list (make-ast-int :value 1)
                                          (make-ast-int :value 2)))
                           ctx)))
    (assert-true (keywordp reg))))

(deftest codegen-values-basic-run
  "values returns all values accessible via multiple-value-list."
  (assert-equal '(1 2 3)
    (run-string "(multiple-value-list (values 1 2 3))")))

(deftest codegen-values-single-run
  "values with one argument returns that argument."
  (assert-run= 42 "(values 42)"))

;;; ─── compile-ast: ast-multiple-value-bind ───────────────────────────────

(deftest codegen-mvb-emits-vm-mv-bind
  "Compiling multiple-value-bind emits a vm-mv-bind instruction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-multiple-value-bind
                  :vars '(a b)
                  :values-form (cl-cc::make-ast-values
                                :forms (list (make-ast-int :value 1)
                                             (make-ast-int :value 2)))
                  :body (list (make-ast-var :name 'a)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-mv-bind))))

(deftest codegen-mvb-returns-register
  "Compiling multiple-value-bind returns a register keyword."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc::make-ast-multiple-value-bind
                             :vars '(a b)
                             :values-form (cl-cc::make-ast-values
                                           :forms (list (make-ast-int :value 10)
                                                        (make-ast-int :value 20)))
                             :body (list (make-ast-var :name 'a)))
                           ctx)))
    (assert-true (keywordp reg))))

(deftest codegen-mvb-basic-run
  "multiple-value-bind binds all variables and evaluates body."
  (assert-run= 3
    "(multiple-value-bind (a b) (values 1 2) (+ a b))"))

(deftest codegen-mvb-first-value-run
  "multiple-value-bind first variable holds the primary value."
  (assert-run= 10
    "(multiple-value-bind (x y) (values 10 20) x)"))

;;; ─── compile-ast: ast-multiple-value-call ───────────────────────────────

(deftest codegen-mv-call-emits-apply
  "Compiling ast-multiple-value-call emits a vm-apply instruction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-multiple-value-call
                  :func (make-ast-function :name '+)
                  :args (list (cl-cc::make-ast-values
                               :forms (list (make-ast-int :value 1)
                                            (make-ast-int :value 2)))))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-apply))))

(deftest codegen-mv-call-returns-register
  "Compiling ast-multiple-value-call returns a register keyword."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc::make-ast-multiple-value-call
                             :func (make-ast-function :name '+)
                             :args (list (cl-cc::make-ast-values
                                          :forms (list (make-ast-int :value 5)))))
                           ctx)))
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

(deftest codegen-mv-prog1-returns-register
  "Compiling ast-multiple-value-prog1 returns a register keyword."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc::make-ast-multiple-value-prog1
                             :first (make-ast-int :value 42)
                             :forms (list (make-ast-int :value 99)))
                           ctx)))
    (assert-true (keywordp reg))))

(deftest codegen-mv-prog1-compiles-all-sub-forms
  "Compiling ast-multiple-value-prog1 emits constants for every sub-form."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-multiple-value-prog1
                  :first (make-ast-int :value 1)
                  :forms (list (make-ast-int :value 2)
                               (make-ast-int :value 3)))
                 ctx)
    (let* ((insts (codegen-instructions ctx))
           (consts (remove-if-not (lambda (i) (typep i 'cl-cc::vm-const)) insts)))
      (assert-true (>= (length consts) 3)))))

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

(deftest codegen-unwind-protect-emits-handler
  "Compiling ast-unwind-protect emits a vm-establish-handler instruction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-unwind-protect
                  :protected (make-ast-int :value 42)
                  :cleanup (list (make-ast-int :value 0)))
                 ctx)
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

(deftest codegen-handler-case-emits-establish-handler
  "Compiling ast-handler-case emits vm-establish-handler for each clause."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-handler-case
                  :form (make-ast-int :value 42)
                  :clauses (list (list 'error 'e (make-ast-int :value 0))))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-establish-handler))))

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

(deftest codegen-flet-emits-closure
  "Compiling ast-flet emits a vm-closure for the local function."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-flet
                  :bindings (list (list 'double '(x)
                                        (make-ast-binop
                                          :op '+
                                          :lhs (make-ast-var :name 'x)
                                          :rhs (make-ast-var :name 'x))))
                  :body (list (make-ast-call :func 'double
                                              :args (list (make-ast-int :value 5)))))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-closure))))

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

(deftest codegen-labels-emits-closure
  "Compiling ast-labels emits a vm-closure for the local function."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-labels
                  :bindings (list (list 'id '(x) (make-ast-var :name 'x)))
                  :body (list (make-ast-call :func 'id
                                              :args (list (make-ast-int :value 1)))))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-closure))))

(deftest codegen-labels-basic-run
  "labels defines a local function accessible in the body."
  (assert-run= 5
    "(labels ((f (x) x)) (f 5))"))

(deftest codegen-labels-recursive-run
  "labels supports self-recursive local functions."
  (assert-run= 120
    "(labels ((fact (n)
                (if (= n 0)
                    1
                    (* n (fact (- n 1))))))
       (fact 5))"))

(deftest codegen-labels-mutual-recursion-run
  "labels supports mutually-recursive local functions."
  (assert-equal t
    (run-string "(labels ((even? (n) (if (= n 0) t (odd? (- n 1))))
                           (odd?  (n) (if (= n 0) nil (even? (- n 1)))))
                   (even? 4))")))

;;; ─── compile-ast: ast-apply ──────────────────────────────────────────────

(deftest codegen-apply-emits-vm-apply
  "Compiling ast-apply emits a vm-apply instruction."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc::make-ast-apply
                  :func (make-ast-function :name '+)
                  :args (list (make-ast-quote :value '(1 2 3))))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-apply))))

(deftest codegen-apply-returns-register
  "Compiling ast-apply returns a register keyword."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (cl-cc::make-ast-apply
                             :func (make-ast-function :name '+)
                             :args (list (make-ast-quote :value '(1 2))))
                           ctx)))
    (assert-true (keywordp reg))))

(deftest codegen-apply-basic-run
  "apply calls a function with arguments spread from a list."
  (assert-run= 6
    "(apply #'+ '(1 2 3))"))

(deftest codegen-apply-with-leading-args-run
  "apply accepts leading fixed arguments before the final list argument."
  (assert-run= 10
    "(apply #'+ 1 2 '(3 4))"))

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

(deftest compile-closure-body-emits-ret
  "%compile-closure-body emits a vm-ret instruction."
  (let* ((ctx (make-codegen-ctx))
         (param-reg (cl-cc::make-register ctx))
         (old-env (cl-cc::ctx-env ctx)))
    (cl-cc::%compile-closure-body ctx '(x) (list param-reg)
                                  (list (make-ast-int :value 5)) old-env)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-ret))))

(deftest compile-closure-body-binds-params-in-env
  "%compile-closure-body adds param bindings to ctx-env during compilation.
   (Caller is responsible for restoration via unwind-protect.)"
  (let* ((ctx (make-codegen-ctx))
         (saved-reg :r0)
         (base-env (list (cons 'outer saved-reg)))
         (param-reg (cl-cc::make-register ctx)))
    (setf (cl-cc::ctx-env ctx) base-env)
    ;; Use an ast-var that references 'p so we can confirm env was extended
    (cl-cc::%compile-closure-body ctx '(p) (list param-reg)
                                  (list (make-ast-int :value 1)) base-env)
    ;; After the call, ctx-env has param binding prepended (caller restores)
    (assert-true (assoc 'p (cl-cc::ctx-env ctx)))))
