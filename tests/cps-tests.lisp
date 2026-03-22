;;;; cps-tests.lisp - Tests for CPS Transformation
;;;;
;;;; This module provides:
;;;; - Tests for S-expression based CPS transformation
;;;; - Tests for AST-based CPS transformation with all special forms

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; ----------------------------------------------------------------------------
;;; S-expression CPS transformation tests
;;; ----------------------------------------------------------------------------

(test cps-transform-int
  (let ((fn (cps-transform-eval '42)))
    (is (equal 42 (funcall fn #'identity)))))

(test cps-transform-if
  (let ((fn (cps-transform-eval '(if 1 10 20))))
    (is (equal 10 (funcall fn #'identity)))))

(test cps-transform-if-false
  (let ((fn (cps-transform-eval '(if 0 10 20))))
    (is (equal 20 (funcall fn #'identity)))))

(test cps-transform-progn
  (let ((fn (cps-transform-eval '(progn 1 2 3))))
    (is (equal 3 (funcall fn #'identity)))))

(test cps-transform-let
  (let ((fn (cps-transform-eval '(let ((x 1) (y 2)) (+ x y)))))
    (is (equal 3 (funcall fn #'identity)))))

(test cps-transform-nested-let
  (let ((fn (cps-transform-eval '(let ((x 1) (y (+ x 1))) (let ((z 3)) (+ y z))))))
    (is (equal 5 (funcall fn #'identity)))))

;;; ----------------------------------------------------------------------------
;;; AST-based CPS transformation tests
;;; ----------------------------------------------------------------------------

(defun eval-cps-ast (ast)
  "CPS-transform an AST node and evaluate the result.
Returns a function that takes a continuation."
  (eval (cps-transform-ast* ast)))

(test cps-transform-ast-int
  (let* ((ast (make-instance 'cl-cc:ast-int :value 42))
         (fn (eval-cps-ast ast)))
    (is (equal 42 (funcall fn #'identity)))))

(test cps-transform-ast-var
  (let* ((ast (make-instance 'cl-cc:ast-var :name 'x))
         (result (cps-transform-ast* ast)))
    (is (listp result))
    (is (eq 'lambda (car result)))))

(test cps-transform-ast-binop
  (let* ((lhs (make-instance 'cl-cc:ast-int :value 1))
         (rhs (make-instance 'cl-cc:ast-int :value 2))
         (ast (make-instance 'cl-cc:ast-binop :op '+ :lhs lhs :rhs rhs))
         (fn (eval-cps-ast ast)))
    (is (equal 3 (funcall fn #'identity)))))

(test cps-transform-ast-if
  (let* ((cond-node (make-instance 'cl-cc:ast-int :value 1))
         (then-node (make-instance 'cl-cc:ast-int :value 10))
         (else-node (make-instance 'cl-cc:ast-int :value 20))
         (ast (make-instance 'cl-cc:ast-if :cond cond-node :then then-node :else else-node))
         (fn (eval-cps-ast ast)))
    (is (equal 10 (funcall fn #'identity)))))

(test cps-transform-ast-progn
  (let* ((forms (list (make-instance 'cl-cc:ast-int :value 1)
                      (make-instance 'cl-cc:ast-int :value 2)
                      (make-instance 'cl-cc:ast-int :value 3)))
         (ast (make-instance 'cl-cc:ast-progn :forms forms))
         (fn (eval-cps-ast ast)))
    (is (equal 3 (funcall fn #'identity)))))

(test cps-transform-ast-let
  (let* ((bindings (list (cons 'x (make-instance 'cl-cc:ast-int :value 1))
                         (cons 'y (make-instance 'cl-cc:ast-int :value 2))))
         (body (list (make-instance 'cl-cc:ast-binop
                                    :op '+
                                    :lhs (make-instance 'cl-cc:ast-var :name 'x)
                                    :rhs (make-instance 'cl-cc:ast-var :name 'y))))
         (ast (make-instance 'cl-cc:ast-let :bindings bindings :body body))
         (fn (eval-cps-ast ast)))
    (is (equal 3 (funcall fn #'identity)))))

(test cps-transform-ast-print
  (let* ((expr (make-instance 'cl-cc:ast-int :value 42))
         (ast (make-instance 'cl-cc:ast-print :expr expr))
         (fn (eval-cps-ast ast)))
    (is (equal 42 (funcall fn #'identity)))))

;;; ----------------------------------------------------------------------------
;;; Extended CPS Form Tests (structural tests)
;;; ----------------------------------------------------------------------------

(test cps-transform-ast-lambda
  (let* ((body (list (make-instance 'cl-cc:ast-int :value 42)))
         (ast (make-instance 'cl-cc:ast-lambda :params '(x) :body body))
         (result (cps-transform-ast* ast)))
    (is (listp result))
    (is (eq 'lambda (car result)))))

(test cps-transform-ast-block
  (let* ((body (list (make-instance 'cl-cc:ast-int :value 42)))
         (ast (make-instance 'cl-cc:ast-block :name 'my-block :body body))
         (result (cps-transform-ast* ast)))
    (is (listp result))
    (is (eq 'lambda (car result)))))

(test cps-transform-ast-return-from
  (let* ((value (make-instance 'cl-cc:ast-int :value 42))
         (ast (make-instance 'cl-cc:ast-return-from :name 'my-block :value value))
         (result (cps-transform-ast* ast)))
    (is (listp result))
    (is (eq 'lambda (car result)))))

(test cps-transform-ast-tagbody
  (let* ((tags (list (cons 'start (list (make-instance 'cl-cc:ast-int :value 1)))
                    (cons 'end (list (make-instance 'cl-cc:ast-int :value 2)))))
         (ast (make-instance 'cl-cc:ast-tagbody :tags tags))
         (result (cps-transform-ast* ast)))
    (is (listp result))
    (is (eq 'lambda (car result)))))

(test cps-transform-ast-go
  (let* ((ast (make-instance 'cl-cc:ast-go :tag 'start))
         (result (cps-transform-ast* ast)))
    (is (listp result))
    (is (eq 'lambda (car result)))))

(test cps-transform-ast-catch
  (let* ((tag (make-instance 'cl-cc:ast-var :name 'my-tag))
         (body (list (make-instance 'cl-cc:ast-int :value 42)))
         (ast (make-instance 'cl-cc:ast-catch :tag tag :body body))
         (result (cps-transform-ast* ast)))
    (is (listp result))
    (is (eq 'lambda (car result)))))

(test cps-transform-ast-throw
  (let* ((tag (make-instance 'cl-cc:ast-var :name 'my-tag))
         (value (make-instance 'cl-cc:ast-int :value 42))
         (ast (make-instance 'cl-cc:ast-throw :tag tag :value value))
         (result (cps-transform-ast* ast)))
    (is (listp result))
    (is (eq 'lambda (car result)))))

(test cps-transform-ast-unwind-protect
  (let* ((protected (make-instance 'cl-cc:ast-int :value 42))
         (cleanup (list (make-instance 'cl-cc:ast-int :value 0)))
         (ast (make-instance 'cl-cc:ast-unwind-protect :protected protected :cleanup cleanup))
         (result (cps-transform-ast* ast)))
    (is (listp result))
    (is (eq 'lambda (car result)))))

(test cps-transform-ast-flet
  (let* ((binding (list 'square '(x)
                        (make-instance 'cl-cc:ast-binop
                                       :op '*
                                       :lhs (make-instance 'cl-cc:ast-var :name 'x)
                                       :rhs (make-instance 'cl-cc:ast-var :name 'x))))
         (body (list (make-instance 'cl-cc:ast-call
                                    :func 'square
                                    :args (list (make-instance 'cl-cc:ast-int :value 5)))))
         (ast (make-instance 'cl-cc:ast-flet :bindings (list binding) :body body))
         (result (cps-transform-ast* ast)))
    (is (listp result))
    (is (eq 'lambda (car result)))))

(test cps-transform-ast-labels
  (let* ((binding (list 'factorial '(x)
                        (make-instance 'cl-cc:ast-if
                                       :cond (make-instance 'cl-cc:ast-binop
                                                            :op '=
                                                            :lhs (make-instance 'cl-cc:ast-var :name 'x)
                                                            :rhs (make-instance 'cl-cc:ast-int :value 1))
                                       :then (make-instance 'cl-cc:ast-int :value 1)
                                       :else (make-instance 'cl-cc:ast-call
                                                            :func 'factorial
                                                            :args (list (make-instance 'cl-cc:ast-binop
                                                                                       :op '-
                                                                                       :lhs (make-instance 'cl-cc:ast-var :name 'x)
                                                                                       :rhs (make-instance 'cl-cc:ast-int :value 1)))))))
          (body (list (make-instance 'cl-cc:ast-call
                                     :func 'factorial
                                     :args (list (make-instance 'cl-cc:ast-int :value 5)))))
          (ast (make-instance 'cl-cc:ast-labels :bindings (list binding) :body body))
          (result (cps-transform-ast* ast)))
    (is (listp result))
    (is (eq 'lambda (car result)))))
