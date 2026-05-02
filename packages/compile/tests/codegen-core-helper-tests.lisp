;;;; tests/unit/compile/codegen-core-helper-tests.lisp — Codegen helper and CPS routing tests

(in-package :cl-cc/test)
(in-suite cl-cc-codegen-unit-serial-suite)

;;; ─── %case-of-case-collapse-node (extracted IF-collapse helper) ─────────────

(deftest case-of-case-collapse-node-non-if-passthrough
  "%case-of-case-collapse-node returns non-IF nodes unchanged."
  (let* ((outer-cond (make-ast-var :name 'x))
         (leaf       (make-ast-int :value 42))
         (result     (cl-cc/compile::%case-of-case-collapse-node leaf outer-cond t)))
    (assert-eq leaf result)))

(deftest case-of-case-collapse-node-different-cond-passthrough
  "%case-of-case-collapse-node returns an ast-if unchanged when its condition differs."
  (let* ((outer-cond (make-ast-var :name 'x))
         (inner-cond (make-ast-var :name 'y))
         (inner-if   (make-ast-if :cond inner-cond
                                  :then (make-ast-int :value 1)
                                  :else (make-ast-int :value 2)))
         (result     (cl-cc/compile::%case-of-case-collapse-node inner-if outer-cond t)))
    (assert-eq inner-if result)))

(deftest case-of-case-collapse-node-same-cond-extracts-then
  "%case-of-case-collapse-node extracts THEN when condition matches and THENP is true."
  (let* ((cond       (make-ast-var :name 'p))
         (then-node  (make-ast-int :value 10))
         (else-node  (make-ast-int :value 20))
         (inner-if   (make-ast-if :cond cond :then then-node :else else-node))
         (result     (cl-cc/compile::%case-of-case-collapse-node inner-if cond t)))
    (assert-eq then-node result)))

(deftest case-of-case-collapse-node-same-cond-extracts-else
  "%case-of-case-collapse-node extracts ELSE when condition matches and THENP is false."
  (let* ((cond      (make-ast-var :name 'p))
         (then-node (make-ast-int :value 10))
         (else-node (make-ast-int :value 20))
         (inner-if  (make-ast-if :cond cond :then then-node :else else-node))
         (result    (cl-cc/compile::%case-of-case-collapse-node inner-if cond nil)))
    (assert-eq else-node result)))

(deftest-compile codegen-toplevel-cps-semantic-preservation
  "Top-level CPS routing preserves common supported forms."
  :cases (("two-safe-forms" 7 "(+ 1 2) (+ 3 4)")
          ("defvar-then-use" 3 "(defvar *ulw-cps* 1) (+ *ulw-cps* 2)")
          ("call-bearing-form" 6 "(defun add1 (x) (+ x 1)) (add1 5)")
          ("apply" 6 "(apply #'+ (list 1 2 3))")
          ("values-primary" 1 "(values 1 2 3)")
          ("multiple-value-bind" 3 "(multiple-value-bind (a b) (values 1 2) (+ a b))"))
  :stdlib nil)

;;; ─── Numeric constructor helpers ────────────────────────────────────────────

(deftest lookup-numeric-binop-ctor-symbol-known-pair
  "%lookup-numeric-binop-ctor-symbol returns the constructor symbol for a known op/kind."
  (let ((sym (cl-cc/compile::%lookup-numeric-binop-ctor-symbol '+ :generic)))
    (assert-true (symbolp sym))
    (assert-true (fboundp sym))))

(deftest lookup-numeric-binop-ctor-symbol-unknown-op
  "%lookup-numeric-binop-ctor-symbol returns NIL for an unknown operator."
  (assert-false (cl-cc/compile::%lookup-numeric-binop-ctor-symbol 'nonexistent-op :generic)))

(deftest lookup-numeric-binop-ctor-symbol-missing-kind
  "%lookup-numeric-binop-ctor-symbol returns NIL when the kind is absent for a known op."
  (assert-false (cl-cc/compile::%lookup-numeric-binop-ctor-symbol '/ :fixnum)))

(deftest numeric-binop-ctor-function-known-pair-returns-function
  "%numeric-binop-ctor-function returns a callable function for a known op/kind."
  (let ((fn (cl-cc/compile::%numeric-binop-ctor-function '+ :generic)))
    (assert-true (functionp fn))))

(deftest numeric-binop-ctor-function-unknown-op-returns-nil
  "%numeric-binop-ctor-function returns NIL when the operator is unknown."
  (assert-false (cl-cc/compile::%numeric-binop-ctor-function 'bogus-op :generic)))

(deftest binop-ctor-returns-callable-function
  "binop-ctor returns a callable constructor function for a known operator."
  (let ((fn (cl-cc/compile::binop-ctor '+)))
    (assert-true (functionp fn))
    (let ((inst (funcall fn :dst :r0 :lhs :r1 :rhs :r2)))
      (assert-true (typep inst 'cl-cc/vm::vm-add)))))

(deftest binop-ctor-signals-error-for-unknown-op
  "binop-ctor signals an error when given an operator not in the dispatch table."
  (assert-signals error (cl-cc/compile::binop-ctor 'no-such-op)))

(deftest ast-proven-type-fixnum-int-literal
  "%ast-proven-type returns the fixnum type for an ast-int holding a fixnum value."
  (let* ((ctx  (make-codegen-ctx))
         (node (make-ast-int :value 42))
         (ty   (cl-cc/compile::%ast-proven-type ctx node)))
    (assert-true ty)
    (assert-true (cl-cc/compile::%proven-fixnum-type-p ty))))

(deftest ast-proven-type-nil-for-bignum
  "%ast-proven-type returns NIL for an ast-int whose value is a bignum."
  (let* ((ctx  (make-codegen-ctx))
         (node (make-ast-int :value (1+ most-positive-fixnum))))
    (assert-false (cl-cc/compile::%ast-proven-type ctx node))))

(deftest ast-proven-type-ast-the-returns-declared
  "%ast-proven-type returns the declared type for an ast-the node with a type annotation."
  (let* ((ctx  (make-codegen-ctx))
         (node (make-ast-the :type 'fixnum :value (make-ast-int :value 10)))
         (ty   (cl-cc/compile::%ast-proven-type ctx node)))
    (assert-true (cl-cc/compile::%proven-fixnum-type-p ty))))

(deftest ast-proven-type-nil-for-non-literal-node
  "%ast-proven-type returns NIL for AST forms without type evidence."
  (let* ((ctx  (make-codegen-ctx))
         (node (make-ast-progn :forms (list (make-ast-int :value 1)))))
    (assert-false (cl-cc/compile::%ast-proven-type ctx node))))

(deftest numeric-binop-constructor-fixnum-path
  "%numeric-binop-constructor picks a fixnum specialization for two integer literal operands."
  (let* ((ctx (make-codegen-ctx))
         (lhs (make-ast-int :value 3))
         (rhs (make-ast-int :value 5))
         (fn  (cl-cc/compile::%numeric-binop-constructor '+ lhs rhs ctx)))
    (assert-true (functionp fn))
    (let ((inst (funcall fn :dst :r0 :lhs :r1 :rhs :r2)))
      (assert-true (typep inst 'cl-cc/vm::vm-integer-add)))))

(deftest numeric-binop-constructor-float-path
  "%numeric-binop-constructor picks a float specialization for two float-literal operands."
  (let* ((ctx (make-codegen-ctx))
         (lhs (make-ast-quote :value 3.0))
         (rhs (make-ast-quote :value 2.0))
         (fn  (cl-cc/compile::%numeric-binop-constructor '+ lhs rhs ctx)))
    (assert-true (functionp fn))
    (let ((inst (funcall fn :dst :r0 :lhs :r1 :rhs :r2)))
      (assert-true (typep inst 'cl-cc/vm::vm-float-add)))))

(deftest numeric-binop-constructor-generic-fallback
  "%numeric-binop-constructor falls back to the generic constructor for unknown operand types."
  (let* ((ctx (make-codegen-ctx))
         (lhs (make-ast-var :name 'x))
         (rhs (make-ast-var :name 'y))
         (fn  (cl-cc/compile::%numeric-binop-constructor '+ lhs rhs ctx)))
    (assert-true (functionp fn))
    (let ((inst (funcall fn :dst :r0 :lhs :r1 :rhs :r2)))
      (assert-true (typep inst 'cl-cc/vm::vm-add)))))

;;; ─── Branch typing / collapse helpers ──────────────────────────────────────

(deftest branch-type-env-no-guard-var-returns-base
  "%branch-type-env returns the base type-env unchanged when guard-var is NIL."
  (let* ((ctx (make-codegen-ctx))
         (env (cl-cc/compile:ctx-type-env ctx))
         (result (cl-cc/compile::%branch-type-env ctx nil nil :then)))
    (assert-eq env result)))

(deftest branch-type-env-then-extends-with-guard-type
  "%branch-type-env :then extends the env so guard-var maps to guard-type."
  (let* ((ctx        (make-codegen-ctx))
         (guard-type (cl-cc/type:parse-type-specifier 'fixnum))
         (new-env    (cl-cc/compile::%branch-type-env ctx 'x guard-type :then)))
    (multiple-value-bind (scheme found-p)
        (cl-cc/type:type-env-lookup 'x new-env)
      (assert-true found-p)
      (assert-true (not (null scheme))))))

(deftest branch-type-env-else-no-prior-binding-returns-base
  "%branch-type-env :else returns base env when guard-var has no prior union binding."
  (let* ((ctx      (make-codegen-ctx))
         (base     (cl-cc/compile:ctx-type-env ctx))
         (guard-ty (cl-cc/type:parse-type-specifier 'fixnum))
         (result   (cl-cc/compile::%branch-type-env ctx 'z guard-ty :else)))
    (assert-eq base result)))

(deftest branch-type-env-unknown-branch-returns-base
  "%branch-type-env returns the base type-env for an unknown branch keyword."
  (let* ((ctx        (make-codegen-ctx))
         (base       (cl-cc/compile:ctx-type-env ctx))
         (guard-type (cl-cc/type:parse-type-specifier 'fixnum))
         (result     (cl-cc/compile::%branch-type-env ctx 'x guard-type :bogus)))
    (assert-eq base result)))

(deftest case-of-case-collapse-branch-delegates-to-node
  "%case-of-case-collapse-branch is a thin wrapper: same behavior as %case-of-case-collapse-node."
  (let* ((cond  (make-ast-var :name 'p))
         (then  (make-ast-int :value 1))
         (else  (make-ast-int :value 2))
         (inner (make-ast-if :cond cond :then then :else else)))
    (assert-eq then (cl-cc/compile::%case-of-case-collapse-branch cond inner t))
    (assert-eq else (cl-cc/compile::%case-of-case-collapse-branch cond inner nil))))

;;; ─── %compile-if-branch ─────────────────────────────────────────────────────

(deftest compile-if-branch-emits-move-to-dst
  "%compile-if-branch compiles the AST into CTX and emits a vm-move from result to DST."
  (let* ((ctx  (make-codegen-ctx))
         (dst  (cl-cc/compile:make-register ctx)))
    (cl-cc/compile::%compile-if-branch
     (make-ast-int :value 99) ctx dst nil nil nil :then)
    (let ((insts (codegen-instructions ctx)))
      (assert-true (some (lambda (i) (and (typep i 'cl-cc/vm::vm-move)
                                          (eq dst (cl-cc/vm::vm-dst i))))
                         insts)))))

(deftest compile-if-branch-emits-jump-when-label-supplied
  "%compile-if-branch emits a vm-jump to JUMP-LABEL when the optional argument is provided."
  (let* ((ctx  (make-codegen-ctx))
         (dst  (cl-cc/compile:make-register ctx)))
    (cl-cc/compile::%compile-if-branch
     (make-ast-int :value 7) ctx dst nil nil nil :then "end_label_0")
    (let ((insts (codegen-instructions ctx)))
      (assert-true (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) insts)))))

(deftest compile-if-branch-no-jump-without-label
  "%compile-if-branch does not emit a vm-jump when JUMP-LABEL is absent."
  (let* ((ctx  (make-codegen-ctx))
         (dst  (cl-cc/compile:make-register ctx)))
    (cl-cc/compile::%compile-if-branch
     (make-ast-int :value 5) ctx dst nil nil nil :else)
    (let ((insts (codegen-instructions ctx)))
      (assert-false (some (lambda (i) (typep i 'cl-cc/vm::vm-jump)) insts)))))
