;;;; tests/unit/compile/cps-ast-tests.lisp — CPS AST transformer tests

(in-package :cl-cc/test)

(defsuite cps-ast-suite
  :description "AST CPS transformer unit tests"
  :parent cl-cc-suite)

(in-suite cps-ast-suite)

(deftest cps-ast-int-expands-to-funcall
  "ast-int CPS expansion passes the literal to the continuation."
  (let ((result (cl-cc::cps-transform-ast (cl-cc:make-ast-int :value 42) 'k)))
    (assert-eq 'funcall (car result))
    (assert-eq 'k (second result))
    (assert-equal 42 (third result))))

(deftest cps-ast-binop-uses-two-continuations
  "ast-binop CPS expansion nests two lambda continuations."
  (let ((result (format nil "~S"
                        (cl-cc::cps-transform-ast
                         (cl-cc:make-ast-binop
                          :op '+
                          :lhs (cl-cc:make-ast-int :value 1)
                          :rhs (cl-cc:make-ast-int :value 2))
                         'k))))
    (assert-true (search "LAMBDA" result))
    (assert-true (search "FUNCALL" result))))

(deftest cps-ast-sequence-and-if-are-recursive
  "Sequence recurses through cps-transform-sequence and if deduplicates its continuation."
  (let ((seq (cl-cc::cps-transform-sequence (list (cl-cc:make-ast-int :value 1)
                                                  (cl-cc:make-ast-int :value 2)) 'k))
        (iff (cl-cc::cps-transform-ast
              (cl-cc:make-ast-if
               :cond (cl-cc:make-ast-int :value 1)
               :then (cl-cc:make-ast-int :value 10)
               :else (cl-cc:make-ast-int :value 20))
              'k)))
    (assert-eq 'funcall (car seq))
    (assert-eq 'let (car iff))
    (assert-true (search "IF" (format nil "~S" iff)))))

(deftest-each cps-ast-conservative-coverage
  "All previously uncovered AST nodes produce host-backed CPS forms with the expected keyword."
  :cases (("values"
           (cl-cc::make-ast-values
             :forms (list (cl-cc:make-ast-int :value 1) (cl-cc:make-ast-int :value 2)))
           "MULTIPLE-VALUE-CALL")
          ("mvb"
           (cl-cc::make-ast-multiple-value-bind
             :vars '(a b)
             :values-form (cl-cc::make-ast-values
                            :forms (list (cl-cc:make-ast-int :value 1)
                                         (cl-cc:make-ast-int :value 2)))
             :body (list (cl-cc:make-ast-var :name 'a)))
           "MULTIPLE-VALUE-BIND")
          ("apply"
           (cl-cc::make-ast-apply
             :func (cl-cc:make-ast-function :name 'list)
             :args (list (cl-cc:make-ast-quote :value '(1 2))))
           "APPLY")
          ("defvar"
           (cl-cc::make-ast-defvar :name '*x :kind 'defparameter
                                   :value (cl-cc:make-ast-int :value 1))
           "DEFPARAMETER")
          ("handler-case"
           (cl-cc::make-ast-handler-case
             :form (cl-cc:make-ast-int :value 1)
             :clauses (list (list 'error 'e (cl-cc:make-ast-int :value 0))))
           "HANDLER-CASE")
          ("make-instance"
           (cl-cc::make-ast-make-instance
             :class (cl-cc:make-ast-quote :value 'foo)
             :initargs (list :x (cl-cc:make-ast-int :value 1)))
           "MAKE-INSTANCE")
          ("slot-value"
           (cl-cc::make-ast-slot-value :object (cl-cc:make-ast-var :name 'obj) :slot 'bar)
           "SLOT-VALUE")
          ("set-slot-value"
           (cl-cc::make-ast-set-slot-value
             :object (cl-cc:make-ast-var :name 'obj) :slot 'bar
             :value (cl-cc:make-ast-int :value 2))
           "SETF")
          ("defclass"
           (cl-cc::make-ast-defclass :name 'foo :superclasses nil :slots nil)
           "DEFCLASS")
          ("defgeneric"
           (cl-cc::make-ast-defgeneric :name 'gf :params '(x) :combination nil)
           "DEFGENERIC")
          ("defmethod"
           (cl-cc::make-ast-defmethod :name 'gf :qualifier nil :specializers '(t)
                                      :params '(x) :body (list (cl-cc:make-ast-var :name 'x)))
           "DEFMETHOD")
          ("set-gethash"
           (cl-cc::make-ast-set-gethash
             :key (cl-cc:make-ast-quote :value 'a)
             :table (cl-cc:make-ast-var :name 'tbl)
             :value (cl-cc:make-ast-int :value 1))
           "GETHASH"))
  (ast expected-keyword)
  (assert-true (search expected-keyword (format nil "~S" (cl-cc::cps-transform-ast ast 'k)))))
