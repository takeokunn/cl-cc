;;;; tests/unit/compile/codegen-calls-tests.lisp
;;;; Unit tests for src/compile/codegen-calls.lisp
;;;;
;;;; Covers: %try-compile-funcall, %try-compile-apply,
;;;;   %try-compile-noescape-cons, %try-compile-noescape-array (direct),
;;;;   %compile-normal-call (GF path + normal path),
;;;;   and compile-ast(ast-call) dispatch — tail call and GF paths.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── helper functions extracted for readability ────────────────────────────

(deftest compile-call-arg-registers-preserves-argument-order
  "%compile-call-arg-registers compiles arguments left-to-right and returns one register per arg."
  (let* ((ctx (make-codegen-ctx))
         (regs (cl-cc/compile::%compile-call-arg-registers
                (list (make-ast-int :value 10)
                      (make-ast-int :value 20))
                ctx))
         (instructions (cl-cc/compile::ctx-instructions ctx)))
    (assert-= 2 (length regs))
    (assert-= 2 (length instructions))
    (let ((first (first instructions))
          (second (second instructions)))
      (assert-type first 'cl-cc/vm::vm-const)
      (assert-type second 'cl-cc/vm::vm-const)
      (assert-= 10 (cl-cc::vm-const-value first))
      (assert-= 20 (cl-cc::vm-const-value second))
      (assert-eq (cl-cc/vm::vm-dst first) (first regs))
      (assert-eq (cl-cc/vm::vm-dst second) (second regs)))))

(deftest emit-call-like-instruction-selects-call-variant
  "%emit-call-like-instruction emits vm-call normally and vm-tail-call in tail position."
  (let* ((ctx (make-codegen-ctx))
         (func-reg (cl-cc/compile::make-register ctx))
         (arg-reg (cl-cc/compile::make-register ctx))
         (result-reg (cl-cc/compile::make-register ctx)))
    (assert-eq result-reg
               (cl-cc/compile::%emit-call-like-instruction nil result-reg func-reg (list arg-reg) ctx))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-call))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-tail-call)))
  (let* ((ctx (make-codegen-ctx))
         (func-reg (cl-cc/compile::make-register ctx))
         (arg-reg (cl-cc/compile::make-register ctx))
         (result-reg (cl-cc/compile::make-register ctx)))
    (assert-eq result-reg
               (cl-cc/compile::%emit-call-like-instruction t result-reg func-reg (list arg-reg) ctx))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-tail-call))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-call))))

;;; ─── %try-compile-funcall ─────────────────────────────────────────────────

(deftest-each try-compile-funcall-returns-nil-for-non-funcall-sym
  "%try-compile-funcall returns nil when func-sym is not 'funcall."
  :cases (("apply"  'apply)
          ("car"    'car)
          ("nil"    nil))
  (sym)
  (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc/compile::make-register ctx))
         (ret (cl-cc/compile::%try-compile-funcall sym (list (make-ast-int :value 1)) result-reg nil ctx)))
    (assert-null ret)))

(deftest try-compile-funcall-cases
  "%try-compile-funcall: nil args→nil; success→result-reg+vm-call; tail=t→vm-tail-call not vm-call."
  (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc/compile::make-register ctx))
         (ret (cl-cc/compile::%try-compile-funcall 'funcall nil result-reg nil ctx)))
    (assert-null ret))
  (let* ((ctx (make-codegen-ctx))
         (fn-reg (cl-cc/compile::make-register ctx))
         (result-reg (cl-cc/compile::make-register ctx)))
    (setf (cl-cc/compile::ctx-env ctx) (list (cons 'fn fn-reg)))
    (let ((ret (cl-cc/compile::%try-compile-funcall
                'funcall
                (list (make-ast-var :name 'fn) (make-ast-int :value 1))
                result-reg nil ctx)))
      (assert-eq result-reg ret)
      (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-call))))
  (let* ((ctx (make-codegen-ctx))
         (fn-reg (cl-cc/compile::make-register ctx))
         (result-reg (cl-cc/compile::make-register ctx)))
    (setf (cl-cc/compile::ctx-env ctx) (list (cons 'fn fn-reg)))
    (cl-cc/compile::%try-compile-funcall
     'funcall
     (list (make-ast-var :name 'fn) (make-ast-int :value 99))
     result-reg t ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-tail-call))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-call))))

;;; ─── %try-compile-apply ──────────────────────────────────────────────────

(deftest-each try-compile-apply-returns-nil-for-non-apply-sym
  "%try-compile-apply returns nil when func-sym is not 'apply."
  :cases (("funcall" 'funcall)
          ("list"    'list)
          ("nil"     nil))
  (sym)
  (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc/compile::make-register ctx))
         (ret (cl-cc/compile::%try-compile-apply sym (list (make-ast-int :value 1)) result-reg ctx)))
    (assert-null ret)))

(deftest try-compile-apply-emits-vm-apply-and-returns-result-reg
  "%try-compile-apply emits vm-apply and returns result-reg on success."
  (let* ((ctx (make-codegen-ctx))
         (fn-reg (cl-cc/compile::make-register ctx))
         (result-reg (cl-cc/compile::make-register ctx)))
    (setf (cl-cc/compile::ctx-env ctx) (list (cons 'f fn-reg)))
    (let ((ret (cl-cc/compile::%try-compile-apply
                'apply
                (list (make-ast-var :name 'f) (make-ast-int :value 1))
                result-reg ctx)))
      (assert-eq result-reg ret)
      (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-apply)))))

;;; ─── %try-compile-noescape-cons ──────────────────────────────────────────

(deftest try-compile-noescape-cons-car-cdr-cases
  "%try-compile-noescape-cons: 'car emits vm-move from car-reg; 'cdr emits vm-move from cdr-reg."
  (let* ((ctx (make-codegen-ctx))
         (car-reg    (cl-cc/compile::make-register ctx))
         (cdr-reg    (cl-cc/compile::make-register ctx))
         (result-reg (cl-cc/compile::make-register ctx)))
    (setf (cl-cc/compile::ctx-noescape-cons-bindings ctx)
          (list (cons 'p (cons car-reg cdr-reg))))
    (let ((ret (cl-cc/compile::%try-compile-noescape-cons
                'car
                (list (make-ast-var :name 'p))
                result-reg ctx)))
      (assert-eq result-reg ret)
      (let ((move (codegen-find-inst ctx 'cl-cc/vm::vm-move)))
        (assert-true move)
        (assert-eq car-reg (cl-cc/vm::vm-src move)))))
  (let* ((ctx (make-codegen-ctx))
         (car-reg    (cl-cc/compile::make-register ctx))
         (cdr-reg    (cl-cc/compile::make-register ctx))
         (result-reg (cl-cc/compile::make-register ctx)))
    (setf (cl-cc/compile::ctx-noescape-cons-bindings ctx)
          (list (cons 'p (cons car-reg cdr-reg))))
    (cl-cc/compile::%try-compile-noescape-cons
     'cdr (list (make-ast-var :name 'p)) result-reg ctx)
    (let ((move (codegen-find-inst ctx 'cl-cc/vm::vm-move)))
      (assert-true move)
      (assert-eq cdr-reg (cl-cc/vm::vm-src move)))))

(deftest try-compile-noescape-cons-nil-cases
  "%try-compile-noescape-cons returns nil: no binding registered; func-sym not car/cdr."
  (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc/compile::make-register ctx))
         (ret (cl-cc/compile::%try-compile-noescape-cons
               'car (list (make-ast-var :name 'unregistered)) result-reg ctx)))
    (assert-null ret))
  (let* ((ctx (make-codegen-ctx))
         (car-reg    (cl-cc/compile::make-register ctx))
         (cdr-reg    (cl-cc/compile::make-register ctx))
         (result-reg (cl-cc/compile::make-register ctx)))
    (setf (cl-cc/compile::ctx-noescape-cons-bindings ctx)
          (list (cons 'p (cons car-reg cdr-reg))))
    (let ((ret (cl-cc/compile::%try-compile-noescape-cons
                'first (list (make-ast-var :name 'p)) result-reg ctx)))
      (assert-null ret))))

;;; ─── %try-compile-noescape-array ─────────────────────────────────────────

(deftest try-compile-noescape-array-cases
  "%try-compile-noescape-array: array-length→vm-const with size; aref static index→vm-move to element."
  (let* ((ctx (make-codegen-ctx))
         (r0 (cl-cc/compile::make-register ctx))
         (r1 (cl-cc/compile::make-register ctx))
         (result-reg (cl-cc/compile::make-register ctx)))
    (setf (cl-cc/compile::ctx-noescape-array-bindings ctx)
          (list (cons 'arr (list 2 r0 r1))))
    (let ((ret (cl-cc/compile::%try-compile-noescape-array
                'array-length
                (list (make-ast-var :name 'arr))
                result-reg ctx)))
      (assert-eq result-reg ret)
      (let ((const (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
        (assert-true const)
        (assert-= 2 (cl-cc::vm-const-value const)))))
  (let* ((ctx (make-codegen-ctx))
         (r0 (cl-cc/compile::make-register ctx))
         (r1 (cl-cc/compile::make-register ctx))
         (r2 (cl-cc/compile::make-register ctx))
         (result-reg (cl-cc/compile::make-register ctx)))
    (setf (cl-cc/compile::ctx-noescape-array-bindings ctx)
          (list (cons 'arr (list 3 r0 r1 r2))))
    (let ((ret (cl-cc/compile::%try-compile-noescape-array
                'aref
                (list (make-ast-var :name 'arr) (make-ast-int :value 1))
                result-reg ctx)))
      (assert-eq result-reg ret)
      (let ((move (codegen-find-inst ctx 'cl-cc/vm::vm-move)))
        (assert-true move)
        (assert-eq r1 (cl-cc/vm::vm-src move))))))

;;; ─── %compile-normal-call ─────────────────────────────────────────────────

(deftest compile-normal-call-cases
  "%compile-normal-call: GF→vm-generic-call not vm-call; ordinary fn→vm-call."
  (let* ((ctx (make-codegen-ctx))
         (gf-reg     (cl-cc/compile::make-register ctx))
         (result-reg (cl-cc/compile::make-register ctx)))
    (setf (gethash 'my-gf (cl-cc/compile::ctx-global-generics ctx)) gf-reg)
    (cl-cc/compile::%compile-normal-call
     'my-gf 'my-gf (list (make-ast-int :value 1)) result-reg nil ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-generic-call))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-call)))
  (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc/compile::make-register ctx)))
    (cl-cc/compile::%compile-normal-call
     'ordinary-fn 'ordinary-fn (list (make-ast-int :value 1)) result-reg nil ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-call))))

;;; ─── compile-ast(ast-call) — dispatch integration ────────────────────────

(deftest codegen-call-dispatch-cases
  "compile-ast dispatch: funcall in tail position→vm-tail-call; GF call→vm-generic-call."
  (let* ((ctx (make-codegen-ctx))
         (fn-reg (cl-cc/compile::make-register ctx)))
    (setf (cl-cc/compile::ctx-env ctx) (list (cons 'fn fn-reg)))
    (setf (cl-cc/compile::ctx-tail-position ctx) t)
    (compile-ast (make-ast-call :func 'funcall
                                :args (list (make-ast-var :name 'fn)
                                            (make-ast-int :value 1)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-tail-call))
    (assert-null (codegen-find-inst ctx 'cl-cc/vm::vm-call)))
  (let* ((ctx (make-codegen-ctx))
         (gf-reg (cl-cc/compile::make-register ctx)))
    (setf (gethash 'my-speak (cl-cc/compile::ctx-global-generics ctx)) gf-reg)
    (compile-ast (make-ast-call :func 'my-speak
                                :args (list (make-ast-int :value 1)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-generic-call))))
