;;;; tests/unit/compile/codegen-functions-tests.lisp — Codegen function/call unit tests

(in-package :cl-cc/test)

;; These tests exercise self-tail-loop lowering and closure/defun registration
;; paths that consult process-global compiler state during compilation. They
;; pass reliably in isolation but have shown parallel-only flakes in the full
;; suite, so keep this file on a dedicated serial child suite.
(defsuite cl-cc-codegen-functions-serial-suite
  :description "Serial codegen function tests"
  :parent cl-cc-unit-suite
  :parallel nil)

(defbefore :each (cl-cc-codegen-functions-serial-suite)
  (setf cl-cc/compile:*labels-boxed-fns* nil
         cl-cc/compile:*compiling-typed-fn* nil)
  (clrhash cl-cc/expand:*function-type-registry*)
  (clrhash cl-cc/expand:*declaim-inline-registry*))

(in-suite cl-cc-codegen-functions-serial-suite)

(deftest codegen-function-ref-returns-register
  "Compiling #'fn returns a register."
  (let* ((ctx (make-codegen-ctx))
         (reg (compile-ast (make-ast-function :name 'car) ctx)))
    (assert-true (keywordp reg))))

(deftest-each codegen-closure-form-emits-vm-closure
  "Closure-creating forms (defun, lambda, labels) emit vm-closure.
NOTE: flet is omitted because the noescape-closure optimizer inlines
flet-bound functions even when #'f is used in the body — the optimizer's
escape analysis treats (function f) as a direct reference to the known
in-scope binding rather than a true escape. Forcing vm-closure emission
from flet would require disabling the optimization specifically for this
test, which isn't worth the test-quality tradeoff."
  :cases (("defun"  (cl-cc/ast:make-ast-defun
                      :name 'my-fn :params '(x)
                      :body (list (make-ast-var :name 'x))))
          ("lambda" (make-ast-lambda
                      :params '(x)
                      :body (list (make-ast-var :name 'x))))
          ("labels" (cl-cc/ast:make-ast-labels
                      :bindings (list (list 'g '(x) (make-ast-var :name 'x)))
                      :body (list (make-ast-call :func 'g
                                                  :args (list (make-ast-int :value 2)))))))
  (ast)
  (let ((ctx (make-codegen-ctx)))
    (compile-ast ast ctx)
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-closure))))

(deftest codegen-defun-registers-global
  "Compiling defun registers the function name in global-functions."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc/ast:make-ast-defun :name 'my-fn
                                   :params '(x)
                                   :body (list (make-ast-var :name 'x)))
                  ctx)
    (assert-true (gethash 'my-fn (cl-cc/compile:ctx-global-functions ctx)))))

(deftest-each codegen-callable-inline-policy-merge-cases
  "Inline policy helper merges pending/local/global declarations conservatively."
  :cases (("local-inline" '((inline f)) nil nil :inline)
          ("global-inline" nil :inline nil :inline)
          ("pending-inline" nil nil :inline :inline)
          ("notinline-wins-local-over-global" '((notinline f)) :inline nil :notinline)
          ("notinline-wins-global-over-pending" nil :notinline :inline :notinline))
  (declarations global-policy pending-policy expected)
  (let ((cl-cc/expand:*declaim-inline-registry* (make-hash-table :test #'eq)))
    (when global-policy
      (setf (gethash 'f cl-cc/expand:*declaim-inline-registry*) global-policy))
    (assert-eq expected
               (cl-cc/compile::%callable-inline-policy declarations
                                                       :name 'f
                                                       :pending-policy pending-policy))))

(deftest codegen-let-inline-declaration-propagates-to-lambda-closure
  "(declare (inline f)) on a let-bound lambda annotates the emitted vm-closure."
  (let* ((ctx (make-codegen-ctx))
         (ast (make-ast-let
               :bindings (list (cons 'f (make-ast-lambda
                                        :params '(x)
                                        :body (list (make-ast-var :name 'x)))))
               :declarations '((inline f))
               :body (list (make-ast-var :name 'f)))))
    (compile-ast ast ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-closure)))
      (assert-true inst)
      (assert-eq :inline (cl-cc/vm:vm-closure-inline-policy inst)))))

(deftest codegen-defun-global-inline-policy-propagates-to-closure
  "Global declaim inline policy is attached to the emitted defun closure metadata."
  (let ((ctx (make-codegen-ctx))
        (cl-cc/expand:*declaim-inline-registry* (make-hash-table :test #'eq)))
    (setf (gethash 'my-fn cl-cc/expand:*declaim-inline-registry*) :inline)
    (compile-ast (cl-cc/ast:make-ast-defun :name 'my-fn
                                           :params '(x)
                                           :body (list (make-ast-var :name 'x)))
                 ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-closure)))
      (assert-true inst)
      (assert-eq :inline (cl-cc/vm:vm-closure-inline-policy inst)))))

(deftest codegen-defun-self-tail-call-loops
  "A simple self-tail call in defun compiles to a jump back to the entry label."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast
     (cl-cc/ast:make-ast-defun
      :name 'loop-fn
      :params '(x)
      :body (list (make-ast-if
                   :cond (make-ast-binop :op '=
                                         :lhs (make-ast-var :name 'x)
                                         :rhs (make-ast-int :value 0))
                   :then (make-ast-var :name 'x)
                   :else (make-ast-call
                          :func 'loop-fn
                          :args (list (make-ast-binop :op '-
                                                      :lhs (make-ast-var :name 'x)
                                                      :rhs (make-ast-int :value 1)))))))
     ctx)
    (let ((tail-call (codegen-find-inst ctx 'cl-cc/vm::vm-tail-call))
          (loop-label (format nil "DEFUN_~A_0" 'loop-fn))
          (self-jump nil))
      (dolist (inst (codegen-instructions ctx))
        (when (and (typep inst 'cl-cc/vm::vm-jump)
                   (string= (cl-cc/vm::vm-label-name inst) loop-label))
          (setf self-jump inst)))
       (assert-eq nil tail-call)
       (assert-true self-jump))))

(deftest codegen-defun-self-tail-call-loop-snapshots-args
  "Self-tail loop lowering snapshots argument registers before rewriting params."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast
     (cl-cc/ast:make-ast-defun
      :name 'swap-loop
      :params '(x y)
      :body (list (make-ast-if
                   :cond (make-ast-binop :op '=
                                         :lhs (make-ast-var :name 'x)
                                         :rhs (make-ast-int :value 0))
                   :then (make-ast-var :name 'y)
                   :else (make-ast-call
                          :func 'swap-loop
                          :args (list (make-ast-var :name 'y)
                                      (make-ast-binop :op '-
                                                      :lhs (make-ast-var :name 'x)
                                                      :rhs (make-ast-int :value 1)))))))
     ctx)
    (let ((tail-call (codegen-find-inst ctx 'cl-cc/vm::vm-tail-call))
          (loop-label (format nil "DEFUN_~A_0" 'swap-loop))
          (self-jump nil)
          (move-count 0))
      (dolist (inst (codegen-instructions ctx))
        (when (typep inst 'cl-cc/vm::vm-move)
          (incf move-count))
        (when (and (typep inst 'cl-cc/vm::vm-jump)
                   (string= (cl-cc/vm::vm-label-name inst) loop-label))
          (setf self-jump inst)))
      (assert-eq nil tail-call)
      (assert-true self-jump)
      ;; Two snapshot moves + two parameter rewrite moves.
      (assert-true (>= move-count 4)))))

(deftest codegen-defvar-compilation
  "Compiling defvar registers in global-variables and emits vm-const for the value."
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (cl-cc/ast:make-ast-defvar :name 'test-codegen-var
                                   :value (make-ast-int :value 99))
                 ctx)
    (assert-true (gethash 'test-codegen-var (cl-cc/compile:ctx-global-variables ctx)))
    (assert-true (codegen-find-inst ctx 'cl-cc/vm::vm-const))))

(deftest codegen-defconstant-inlines-symbol-reference
  "Compiling a symbol bound by defconstant emits vm-const with the constant value."
  (cl-cc/expand::compiler-macroexpand-all '(defconstant codegen-inline-constant 123))
  (let ((ctx (make-codegen-ctx)))
    (compile-ast (make-ast-var :name 'codegen-inline-constant) ctx)
    (let ((inst (codegen-find-inst ctx 'cl-cc/vm::vm-const)))
      (assert-true inst)
      (assert-equal 123 (cl-cc::vm-const-value inst)))
    (assert-eq nil (codegen-find-inst ctx 'cl-cc/vm::vm-get-global))))
