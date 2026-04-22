(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest codegen-result-vm-instructions-without-halt-strips-terminal-halt
  "%result-vm-instructions-without-halt removes only the final vm-halt instruction."
  (let* ((move (cl-cc:make-vm-move :dst :R1 :src :R0))
         (halt (cl-cc:make-vm-halt :reg :R1))
         (result (cl-cc/compile::make-compilation-result
                  :program (cl-cc:make-vm-program :instructions (list move halt) :result-register :R1)
                  :vm-instructions (list move halt))))
    (assert-equal (list move)
                  (cl-cc/compile::%result-vm-instructions-without-halt result))))

(deftest codegen-toplevel-safe-form-prefers-cps-primary-path
  "compile-toplevel-forms routes VM-safe top-level forms through the CPS entry path."
  (let ((captured-expr nil)
        (compile-ast-called nil))
    (with-replaced-function (cl-cc/compile:compile-expression
                             (lambda (expr &rest args)
                               (declare (ignore args))
                               (setf captured-expr expr)
                               (cl-cc/compile::make-compilation-result
                                :program (cl-cc:make-vm-program :instructions nil :result-register :R-CPS)
                                :vm-instructions (list (cl-cc:make-vm-halt :reg :R-CPS))
                                :cps '(lambda (k) (funcall k 3)))))
      (with-replaced-function (cl-cc/compile:compile-ast
                               (lambda (&rest args)
                                 (declare (ignore args))
                                 (setf compile-ast-called t)
                                 :R-DIRECT))
        (cl-cc/compile::compile-toplevel-forms '((+ 1 2)) :target :vm)))
    (assert-true captured-expr)
    (assert-eq 'lambda (car captured-expr))
    (assert-false compile-ast-called)))

(deftest codegen-toplevel-unsafe-form-stays-on-direct-path
  "compile-toplevel-forms keeps CPS-unsafe forms on the direct compile-ast path."
  (let ((compile-expression-called nil)
        (compile-ast-called nil))
    (with-replaced-function (cl-cc/compile:compile-expression
                             (lambda (&rest args)
                               (declare (ignore args))
                               (setf compile-expression-called t)
                               (cl-cc/compile::make-compilation-result
                                :program (cl-cc:make-vm-program :instructions nil :result-register :R-CPS)
                                :vm-instructions (list (cl-cc:make-vm-halt :reg :R-CPS)))))
      (with-replaced-function (cl-cc/compile:compile-ast
                               (lambda (&rest args)
                                 (declare (ignore args))
                                 (setf compile-ast-called t)
                                 :R-DIRECT))
        (cl-cc/compile::compile-toplevel-forms '((make-instance 'point)) :target :vm)))
    (assert-false compile-expression-called)
    (assert-true compile-ast-called)))

(deftest-compile-each codegen-toplevel-cps-semantic-preservation
  "Multi-form Lisp sources still evaluate to the final value after the top-level CPS routing change."
  :cases (("two-safe-forms" 7 "(+ 1 2) (+ 3 4)")
          ("defvar-then-use" 3 "(defvar *ulw-cps* 1) (+ *ulw-cps* 2)")
          ("call-bearing-form" 6 "(defun add1 (x) (+ x 1)) (add1 5)"))
  :stdlib nil)

(deftest-compile-each codegen-toplevel-cps-multi-value-semantics
  "Top-level CPS routing preserves apply/values/multiple-value-bind behavior for supported forms."
  :cases (("apply" 6 "(apply + (list 1 2 3))")
          ("values-primary" 1 "(values 1 2 3)")
          ("multiple-value-bind" 3 "(multiple-value-bind (a b) (values 1 2) (+ a b))"))
  :stdlib nil)
