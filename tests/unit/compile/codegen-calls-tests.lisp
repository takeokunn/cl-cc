;;;; tests/unit/compile/codegen-calls-tests.lisp
;;;; Unit tests for src/compile/codegen-calls.lisp
;;;;
;;;; Covers: %try-compile-funcall, %try-compile-apply,
;;;;   %try-compile-noescape-cons, %try-compile-noescape-array (direct),
;;;;   %compile-normal-call (GF path + normal path),
;;;;   and compile-ast(ast-call) dispatch — tail call and GF paths.

(in-package :cl-cc/test)
(in-suite cl-cc-suite)

;;; ─── %try-compile-funcall ─────────────────────────────────────────────────

(deftest-each try-compile-funcall-returns-nil-for-non-funcall-sym
  "%try-compile-funcall returns nil when func-sym is not 'funcall."
  :cases (("apply"  'apply)
          ("car"    'car)
          ("nil"    nil))
  (sym)
  (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc::make-register ctx))
         (ret (cl-cc::%try-compile-funcall sym (list (make-ast-int :value 1)) result-reg nil ctx)))
    (assert-null ret)))

(deftest try-compile-funcall-returns-nil-when-args-empty
  "%try-compile-funcall returns nil when the args list is nil (guard: (and ... args))."
  (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc::make-register ctx))
         (ret (cl-cc::%try-compile-funcall 'funcall nil result-reg nil ctx)))
    (assert-null ret)))

(deftest try-compile-funcall-emits-vm-call-and-returns-result-reg
  "%try-compile-funcall emits vm-call and returns result-reg on success."
  (let* ((ctx (make-codegen-ctx))
         (fn-reg (cl-cc::make-register ctx))
         (result-reg (cl-cc::make-register ctx)))
    (setf (cl-cc::ctx-env ctx) (list (cons 'fn fn-reg)))
    (let ((ret (cl-cc::%try-compile-funcall
                'funcall
                (list (make-ast-var :name 'fn) (make-ast-int :value 1))
                result-reg nil ctx)))
      (assert-eq result-reg ret)
      (assert-true (codegen-find-inst ctx 'cl-cc::vm-call)))))

(deftest try-compile-funcall-tail-emits-vm-tail-call
  "%try-compile-funcall with tail=t emits vm-tail-call, not vm-call."
  (let* ((ctx (make-codegen-ctx))
         (fn-reg (cl-cc::make-register ctx))
         (result-reg (cl-cc::make-register ctx)))
    (setf (cl-cc::ctx-env ctx) (list (cons 'fn fn-reg)))
    (cl-cc::%try-compile-funcall
     'funcall
     (list (make-ast-var :name 'fn) (make-ast-int :value 99))
     result-reg t ctx)
    (assert-true  (codegen-find-inst ctx 'cl-cc::vm-tail-call))
    (assert-null  (codegen-find-inst ctx 'cl-cc::vm-call))))

;;; ─── %try-compile-apply ──────────────────────────────────────────────────

(deftest-each try-compile-apply-returns-nil-for-non-apply-sym
  "%try-compile-apply returns nil when func-sym is not 'apply."
  :cases (("funcall" 'funcall)
          ("list"    'list)
          ("nil"     nil))
  (sym)
  (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc::make-register ctx))
         (ret (cl-cc::%try-compile-apply sym (list (make-ast-int :value 1)) result-reg ctx)))
    (assert-null ret)))

(deftest try-compile-apply-emits-vm-apply-and-returns-result-reg
  "%try-compile-apply emits vm-apply and returns result-reg on success."
  (let* ((ctx (make-codegen-ctx))
         (fn-reg (cl-cc::make-register ctx))
         (result-reg (cl-cc::make-register ctx)))
    (setf (cl-cc::ctx-env ctx) (list (cons 'f fn-reg)))
    (let ((ret (cl-cc::%try-compile-apply
                'apply
                (list (make-ast-var :name 'f) (make-ast-int :value 1))
                result-reg ctx)))
      (assert-eq result-reg ret)
      (assert-true (codegen-find-inst ctx 'cl-cc::vm-apply)))))

;;; ─── %try-compile-noescape-cons ──────────────────────────────────────────

(deftest try-compile-noescape-cons-car-emits-move-from-car-reg
  "%try-compile-noescape-cons for 'car emits vm-move using the car register."
  (let* ((ctx (make-codegen-ctx))
         (car-reg    (cl-cc::make-register ctx))
         (cdr-reg    (cl-cc::make-register ctx))
         (result-reg (cl-cc::make-register ctx)))
    ;; ctx-noescape-cons-bindings entry: (name car-reg . cdr-reg)
    (setf (cl-cc::ctx-noescape-cons-bindings ctx)
          (list (cons 'p (cons car-reg cdr-reg))))
    (let ((ret (cl-cc::%try-compile-noescape-cons
                'car
                (list (make-ast-var :name 'p))
                result-reg ctx)))
      (assert-eq result-reg ret)
      (let ((move (codegen-find-inst ctx 'cl-cc::vm-move)))
        (assert-true move)
        (assert-eq car-reg (cl-cc::vm-src move))))))

(deftest try-compile-noescape-cons-cdr-emits-move-from-cdr-reg
  "%try-compile-noescape-cons for 'cdr emits vm-move using the cdr register."
  (let* ((ctx (make-codegen-ctx))
         (car-reg    (cl-cc::make-register ctx))
         (cdr-reg    (cl-cc::make-register ctx))
         (result-reg (cl-cc::make-register ctx)))
    (setf (cl-cc::ctx-noescape-cons-bindings ctx)
          (list (cons 'p (cons car-reg cdr-reg))))
    (cl-cc::%try-compile-noescape-cons
     'cdr (list (make-ast-var :name 'p)) result-reg ctx)
    (let ((move (codegen-find-inst ctx 'cl-cc::vm-move)))
      (assert-true move)
      (assert-eq cdr-reg (cl-cc::vm-src move)))))

(deftest try-compile-noescape-cons-returns-nil-when-no-binding
  "%try-compile-noescape-cons returns nil when the variable has no noescape entry."
  (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc::make-register ctx))
         (ret (cl-cc::%try-compile-noescape-cons
               'car (list (make-ast-var :name 'unregistered)) result-reg ctx)))
    (assert-null ret)))

(deftest try-compile-noescape-cons-returns-nil-for-non-car-cdr
  "%try-compile-noescape-cons returns nil when func-sym is neither 'car nor 'cdr."
  (let* ((ctx (make-codegen-ctx))
         (car-reg    (cl-cc::make-register ctx))
         (cdr-reg    (cl-cc::make-register ctx))
         (result-reg (cl-cc::make-register ctx)))
    (setf (cl-cc::ctx-noescape-cons-bindings ctx)
          (list (cons 'p (cons car-reg cdr-reg))))
    (let ((ret (cl-cc::%try-compile-noescape-cons
                'first (list (make-ast-var :name 'p)) result-reg ctx)))
      (assert-null ret))))

;;; ─── %try-compile-noescape-array ─────────────────────────────────────────

(deftest try-compile-noescape-array-length-emits-const-with-size
  "%try-compile-noescape-array for array-length emits vm-const with the array's size."
  (let* ((ctx (make-codegen-ctx))
         (r0 (cl-cc::make-register ctx))
         (r1 (cl-cc::make-register ctx))
         (result-reg (cl-cc::make-register ctx)))
    ;; Entry: (name size . element-regs) — 2-element array
    (setf (cl-cc::ctx-noescape-array-bindings ctx)
          (list (cons 'arr (list 2 r0 r1))))
    (let ((ret (cl-cc::%try-compile-noescape-array
                'array-length
                (list (make-ast-var :name 'arr))
                result-reg ctx)))
      (assert-eq result-reg ret)
      (let ((const (codegen-find-inst ctx 'cl-cc::vm-const)))
        (assert-true const)
        (assert-= 2 (cl-cc::vm-const-value const))))))

(deftest try-compile-noescape-array-aref-static-index-emits-move-to-element-reg
  "%try-compile-noescape-array for aref with static index emits vm-move to element register."
  (let* ((ctx (make-codegen-ctx))
         (r0 (cl-cc::make-register ctx))
         (r1 (cl-cc::make-register ctx))
         (r2 (cl-cc::make-register ctx))
         (result-reg (cl-cc::make-register ctx)))
    ;; 3-element array; element 1 is r1
    (setf (cl-cc::ctx-noescape-array-bindings ctx)
          (list (cons 'arr (list 3 r0 r1 r2))))
    (let ((ret (cl-cc::%try-compile-noescape-array
                'aref
                (list (make-ast-var :name 'arr) (make-ast-int :value 1))
                result-reg ctx)))
      (assert-eq result-reg ret)
      (let ((move (codegen-find-inst ctx 'cl-cc::vm-move)))
        (assert-true move)
        (assert-eq r1 (cl-cc::vm-src move))))))

;;; ─── %compile-normal-call ─────────────────────────────────────────────────

(deftest compile-normal-call-emits-vm-generic-call-for-gf
  "%compile-normal-call emits vm-generic-call when function is in ctx-global-generics."
  (let* ((ctx (make-codegen-ctx))
         (gf-reg     (cl-cc::make-register ctx))
         (result-reg (cl-cc::make-register ctx)))
    (setf (gethash 'my-gf (cl-cc::ctx-global-generics ctx)) gf-reg)
    (cl-cc::%compile-normal-call
     'my-gf 'my-gf (list (make-ast-int :value 1)) result-reg nil ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-generic-call))
    (assert-null (codegen-find-inst ctx 'cl-cc::vm-call))))

(deftest compile-normal-call-emits-vm-call-for-ordinary-function
  "%compile-normal-call emits vm-call for a non-GF function."
  (let* ((ctx (make-codegen-ctx))
         (result-reg (cl-cc::make-register ctx)))
    (cl-cc::%compile-normal-call
     'ordinary-fn 'ordinary-fn (list (make-ast-int :value 1)) result-reg nil ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-call))))

;;; ─── compile-ast(ast-call) — dispatch integration ────────────────────────

(deftest codegen-call-funcall-tail-position-emits-tail-call
  "compile-ast for funcall in tail position (ctx-tail-position=t) emits vm-tail-call."
  (let* ((ctx (make-codegen-ctx))
         (fn-reg (cl-cc::make-register ctx)))
    (setf (cl-cc::ctx-env ctx) (list (cons 'fn fn-reg)))
    (setf (cl-cc::ctx-tail-position ctx) t)
    (compile-ast (make-ast-call :func 'funcall
                                :args (list (make-ast-var :name 'fn)
                                            (make-ast-int :value 1)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-tail-call))
    (assert-null (codegen-find-inst ctx 'cl-cc::vm-call))))

(deftest codegen-call-to-generic-function-emits-vm-generic-call
  "compile-ast for a call to a function in ctx-global-generics emits vm-generic-call."
  (let* ((ctx (make-codegen-ctx))
         (gf-reg (cl-cc::make-register ctx)))
    (setf (gethash 'my-speak (cl-cc::ctx-global-generics ctx)) gf-reg)
    (compile-ast (make-ast-call :func 'my-speak
                                :args (list (make-ast-int :value 1)))
                 ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc::vm-generic-call))))
