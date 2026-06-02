;;;; packages/cps/tests/cps-coverage-matrix-tests.lisp
;;;; Table-driven coverage for every AST node type with a CPS transformer.

(in-package :cl-cc/test)

(defsuite cps-coverage-suite
  :description "CPS AST transformation coverage matrix"
  :parent cl-cc-unit-suite)

(in-suite cps-coverage-suite)

(defun %cps-coverage-int (&optional (value 1))
  "Build a minimal integer AST node for CPS coverage fixtures."
  (cl-cc/ast:make-ast-int :value value))

(defun %cps-transform-succeeds-p (node)
  "Return true when NODE can be transformed by cps-transform-ast*."
  (handler-case
      (progn
        (cl-cc/cps:cps-transform-ast* node)
        t)
    (error () nil)))

(deftest-each cps-ast-coverage
  "Verify cps-transform-ast* succeeds for every AST special form."
  :cases
  (("ast-int" "ast-int"
    (cl-cc/ast:make-ast-int :value 42))

   ("ast-var" "ast-var"
    (cl-cc/ast:make-ast-var :name 'x))

   ("ast-binop" "ast-binop"
    (cl-cc/ast:make-ast-binop
     :op '+
     :lhs (%cps-coverage-int 1)
     :rhs (%cps-coverage-int 2)))

   ("ast-if" "ast-if"
    (cl-cc/ast:make-ast-if
     :cond (%cps-coverage-int 1)
     :then (%cps-coverage-int 2)
     :else (%cps-coverage-int 3)))

   ("ast-progn" "ast-progn"
    (cl-cc/ast:make-ast-progn
     :forms (list (%cps-coverage-int 1)
                  (%cps-coverage-int 2))))

   ("ast-print" "ast-print"
    (cl-cc/ast:make-ast-print
     :expr (%cps-coverage-int 1)))

   ("ast-let" "ast-let"
    (cl-cc/ast:make-ast-let
     :bindings (list (cons 'x (%cps-coverage-int 1)))
     :body (list (cl-cc/ast:make-ast-var :name 'x))))

   ("ast-lambda" "ast-lambda"
    (cl-cc/ast:make-ast-lambda
     :params '(x)
     :body (list (cl-cc/ast:make-ast-var :name 'x))))

   ("ast-function" "ast-function"
    (cl-cc/ast:make-ast-function :name 'identity))

   ("ast-block" "ast-block"
    (cl-cc/ast:make-ast-block
     :name 'done
     :body (list (%cps-coverage-int 1))))

   ("ast-return-from" "ast-return-from"
    (cl-cc/ast:make-ast-return-from
     :name 'done
     :value (%cps-coverage-int 1)))

   ("ast-tagbody" "ast-tagbody"
    (cl-cc/ast:make-ast-tagbody
     :tags (list (cons 'start (list (%cps-coverage-int 1))))))

   ("ast-go" "ast-go"
    (cl-cc/ast:make-ast-go :tag 'start))

   ("ast-catch" "ast-catch"
    (cl-cc/ast:make-ast-catch
     :tag (cl-cc/ast:make-ast-quote :value 'tag)
     :body (list (%cps-coverage-int 1))))

   ("ast-throw" "ast-throw"
    (cl-cc/ast:make-ast-throw
     :tag (cl-cc/ast:make-ast-quote :value 'tag)
     :value (%cps-coverage-int 1)))

   ("ast-unwind-protect" "ast-unwind-protect"
    (cl-cc/ast:make-ast-unwind-protect
     :protected (%cps-coverage-int 1)
     :cleanup (list (%cps-coverage-int 0))))

   ("ast-flet" "ast-flet"
    (cl-cc/ast:make-ast-flet
     :bindings (list (list 'local-id '(x) (cl-cc/ast:make-ast-var :name 'x)))
     :body (list (%cps-coverage-int 1))))

   ("ast-labels" "ast-labels"
    (cl-cc/ast:make-ast-labels
     :bindings (list (list 'local-id '(x) (cl-cc/ast:make-ast-var :name 'x)))
     :body (list (%cps-coverage-int 1))))

   ("ast-setq" "ast-setq"
    (cl-cc/ast:make-ast-setq
     :var 'x
     :value (%cps-coverage-int 1)))

   ("ast-defvar" "ast-defvar"
    (cl-cc/ast:make-ast-defvar
     :name '*coverage-var*
     :kind 'defparameter
     :value (%cps-coverage-int 1)))

   ("ast-defun" "ast-defun"
    (cl-cc/ast:make-ast-defun
     :name 'coverage-function
     :params '(x)
     :body (list (cl-cc/ast:make-ast-var :name 'x))))

   ("ast-defmacro" "ast-defmacro"
    (cl-cc/ast:make-ast-defmacro
     :name 'coverage-macro
     :lambda-list '(x)
     :body '((list 'quote x))))

   ("ast-handler-case" "ast-handler-case"
    (cl-cc/ast:make-ast-handler-case
     :form (%cps-coverage-int 1)
     :clauses (list (list 'error 'e (%cps-coverage-int 0)))))

   ("ast-make-instance" "ast-make-instance"
    (cl-cc/ast:make-ast-make-instance
     :class (cl-cc/ast:make-ast-quote :value 'coverage-class)
     :initargs (list :x (%cps-coverage-int 1))))

   ("ast-slot-value" "ast-slot-value"
    (cl-cc/ast:make-ast-slot-value
     :object (cl-cc/ast:make-ast-var :name 'object)
     :slot 'x))

   ("ast-set-slot-value" "ast-set-slot-value"
    (cl-cc/ast:make-ast-set-slot-value
     :object (cl-cc/ast:make-ast-var :name 'object)
     :slot 'x
     :value (%cps-coverage-int 1)))

   ("ast-defclass" "ast-defclass"
    (cl-cc/ast:make-ast-defclass
     :name 'coverage-class
     :superclasses nil
     :slots nil))

   ("ast-defgeneric" "ast-defgeneric"
    (cl-cc/ast:make-ast-defgeneric
     :name 'coverage-generic
     :params '(object)))

   ("ast-defmethod" "ast-defmethod"
    (cl-cc/ast:make-ast-defmethod
     :name 'coverage-generic
     :params '(object)
     :specializers '(t)
     :body (list (cl-cc/ast:make-ast-var :name 'object))))

   ("ast-quote" "ast-quote"
    (cl-cc/ast:make-ast-quote :value '(a b c)))

   ("ast-the" "ast-the"
    (cl-cc/ast:make-ast-the
     :type 'integer
     :value (%cps-coverage-int 1)))

   ("ast-values" "ast-values"
    (cl-cc/ast:make-ast-values
     :forms (list (%cps-coverage-int 1)
                  (%cps-coverage-int 2))))

   ("ast-multiple-value-bind" "ast-multiple-value-bind"
    (cl-cc/ast:make-ast-multiple-value-bind
     :vars '(a b)
     :values-form (cl-cc/ast:make-ast-values
                   :forms (list (%cps-coverage-int 1)
                                (%cps-coverage-int 2)))
     :body (list (cl-cc/ast:make-ast-var :name 'a))))

   ("ast-multiple-value-prog1" "ast-multiple-value-prog1"
    (cl-cc/ast:make-ast-multiple-value-prog1
     :first (%cps-coverage-int 1)
     :forms (list (%cps-coverage-int 2))))

   ("ast-multiple-value-call" "ast-multiple-value-call"
    (cl-cc/ast:make-ast-multiple-value-call
     :func (cl-cc/ast:make-ast-function :name 'list)
     :args (list (%cps-coverage-int 1)
                 (%cps-coverage-int 2))))

   ("ast-apply" "ast-apply"
    (cl-cc/ast:make-ast-apply
     :func (cl-cc/ast:make-ast-function :name 'list)
     :args (list (%cps-coverage-int 1)
                 (cl-cc/ast:make-ast-quote :value '(2 3)))))

   ("ast-call" "ast-call"
    (cl-cc/ast:make-ast-call
     :func 'list
     :args (list (%cps-coverage-int 1)
                 (%cps-coverage-int 2))))

   ("ast-set-gethash" "ast-set-gethash"
    (cl-cc/ast:make-ast-set-gethash
     :key (cl-cc/ast:make-ast-quote :value 'key)
     :table (cl-cc/ast:make-ast-var :name 'table)
     :value (%cps-coverage-int 1))))
  (label node)
  (declare (ignore label))
  (assert-true (%cps-transform-succeeds-p node)))
