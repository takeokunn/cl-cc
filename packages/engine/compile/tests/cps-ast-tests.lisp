;;;; tests/unit/compile/cps-ast-tests.lisp — CPS AST transformer tests

(in-package :cl-cc/test)

(defsuite cps-ast-suite
  :description "AST CPS transformer unit tests"
  :parent cl-cc-unit-suite)

(in-suite cps-ast-suite)

(deftest cps-ast-int-expands-to-funcall
  "ast-int CPS expansion passes the literal to the continuation."
  (let ((result (cl-cc/compile::cps-transform-ast (cl-cc:make-ast-int :value 42) 'k)))
    (assert-eq 'funcall (car result))
    (assert-eq 'k (second result))
    (assert-equal 42 (third result))))

(deftest cps-ast-binop-uses-two-continuations
  "ast-binop CPS expansion nests two lambda continuations."
  (let ((result (format nil "~S"
                        (cl-cc/compile::cps-transform-ast
                         (cl-cc:make-ast-binop
                          :op '+
                          :lhs (cl-cc:make-ast-int :value 1)
                          :rhs (cl-cc:make-ast-int :value 2))
                         'k))))
    (assert-true (search "LAMBDA" result))
    (assert-true (search "FUNCALL" result))))

(deftest cps-ast-sequence-and-if-are-recursive
  "Sequence recurses through cps-transform-sequence and if deduplicates its continuation."
  (let ((seq (cl-cc/compile::cps-transform-sequence (list (cl-cc:make-ast-int :value 1)
                                                  (cl-cc:make-ast-int :value 2)) 'k))
        (iff (cl-cc/compile::cps-transform-ast
              (cl-cc:make-ast-if
               :cond (cl-cc:make-ast-int :value 1)
               :then (cl-cc:make-ast-int :value 10)
               :else (cl-cc:make-ast-int :value 20))
              'k)))
    (assert-eq 'funcall (car seq))
    (assert-eq 'let (car iff))
    (assert-true (search "IF" (format nil "~S" iff)))))

(deftest-each cps-ast-conservative-coverage
  "Representative extended AST nodes lower to CPS forms with the expected keyword markers."
  :cases (("values"
           (cl-cc/ast::make-ast-values
             :forms (list (cl-cc:make-ast-int :value 1) (cl-cc:make-ast-int :value 2)))
           "MULTIPLE-VALUE-CALL")
          ("mvb"
           (cl-cc/ast::make-ast-multiple-value-bind
             :vars '(a b)
             :values-form (cl-cc/ast::make-ast-values
                            :forms (list (cl-cc:make-ast-int :value 1)
                                         (cl-cc:make-ast-int :value 2)))
             :body (list (cl-cc:make-ast-var :name 'a)))
           "MULTIPLE-VALUE-BIND")
          ("apply"
           (cl-cc/ast::make-ast-apply
             :func (cl-cc:make-ast-function :name 'list)
             :args (list (cl-cc:make-ast-quote :value '(1 2))))
           "APPLY")
          ("defvar"
           (cl-cc/ast::make-ast-defvar :name '*x :kind 'defparameter
                                   :value (cl-cc:make-ast-int :value 1))
           "DEFPARAMETER")
          ("handler-case"
           (cl-cc/ast::make-ast-handler-case
             :form (cl-cc:make-ast-int :value 1)
             :clauses (list (list 'error 'e (cl-cc:make-ast-int :value 0))))
           "HANDLER-CASE")
          ("make-instance"
           (cl-cc/ast::make-ast-make-instance
             :class (cl-cc:make-ast-quote :value 'foo)
             :initargs (list :x (cl-cc:make-ast-int :value 1)))
           "MAKE-INSTANCE")
          ("slot-value"
           (cl-cc/ast::make-ast-slot-value :object (cl-cc:make-ast-var :name 'obj) :slot 'bar)
           "SLOT-VALUE")
          ("set-slot-value"
           (cl-cc/ast::make-ast-set-slot-value
             :object (cl-cc:make-ast-var :name 'obj) :slot 'bar
             :value (cl-cc:make-ast-int :value 2))
           "SETF")
          ("defclass"
           (cl-cc/ast::make-ast-defclass :name 'foo :superclasses nil :slots nil)
           "DEFCLASS")
          ("defgeneric"
           (cl-cc/ast::make-ast-defgeneric :name 'gf :params '(x) :combination nil)
           "DEFGENERIC")
          ("defmethod"
           (cl-cc/ast::make-ast-defmethod :name 'gf :qualifier nil :specializers '(t)
                                      :params '(x) :body (list (cl-cc:make-ast-var :name 'x)))
           "DEFMETHOD")
          ("set-gethash"
           (cl-cc/ast::make-ast-set-gethash
             :key (cl-cc:make-ast-quote :value 'a)
             :table (cl-cc:make-ast-var :name 'tbl)
             :value (cl-cc:make-ast-int :value 1))
           "GETHASH"))
  (ast expected-keyword)
  (assert-true (search expected-keyword (format nil "~S" (cl-cc/compile::cps-transform-ast ast 'k)))))

;;; ─── %cps-expand-let-bindings (extracted recursive helper) ──────────────

(deftest cps-expand-let-bindings-empty-bindings
  "%cps-expand-let-bindings with no bindings delegates to cps-transform-sequence."
  (let* ((body (list (cl-cc:make-ast-int :value 99)))
         (result (cl-cc/compile::%cps-expand-let-bindings nil body 'k)))
    (assert-true (consp result))
    (assert-eq 'funcall (car result))
    (assert-eq 'k (second result))
    (assert-= 99 (third result))))

(deftest cps-expand-let-bindings-single-binding
  "%cps-expand-let-bindings with one binding wraps in a lambda continuation."
  (let* ((binding (cons 'x (cl-cc:make-ast-int :value 1)))
         (body    (list (cl-cc:make-ast-var :name 'x)))
         (result  (format nil "~S"
                          (cl-cc/compile::%cps-expand-let-bindings
                           (list binding) body 'k))))
    (assert-true (search "LAMBDA" result))
    (assert-true (search "LET"    result))))

(deftest cps-expand-let-bindings-two-bindings-nest
  "%cps-expand-let-bindings with two bindings produces doubly-nested lambda."
  (let* ((b1     (cons 'x (cl-cc:make-ast-int :value 1)))
         (b2     (cons 'y (cl-cc:make-ast-int :value 2)))
         (body   (list (cl-cc:make-ast-var :name 'x)))
         (result (format nil "~S"
                         (cl-cc/compile::%cps-expand-let-bindings
                          (list b1 b2) body 'k)))
         (lambda-count (let ((count 0) (pos 0))
                         (loop
                           (let ((found (search "LAMBDA" result :start2 pos)))
                             (if found
                                 (progn (incf count) (setf pos (1+ found)))
                                 (return count)))))))
    (assert-true (>= lambda-count 2))))
