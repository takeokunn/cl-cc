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

(deftest cps-ast-values/mvb/apply-have-conservative-coverage
  "Previously uncovered multiple-value/apply AST nodes now produce CPS forms instead of method errors."
  (let ((values-form (format nil "~S"
                             (cl-cc::cps-transform-ast
                              (cl-cc::make-ast-values
                               :forms (list (cl-cc:make-ast-int :value 1)
                                            (cl-cc:make-ast-int :value 2)))
                              'k)))
        (mvb-form (format nil "~S"
                          (cl-cc::cps-transform-ast
                           (cl-cc::make-ast-multiple-value-bind
                            :vars '(a b)
                            :values-form (cl-cc::make-ast-values
                                          :forms (list (cl-cc:make-ast-int :value 1)
                                                       (cl-cc:make-ast-int :value 2)))
                            :body (list (cl-cc:make-ast-var :name 'a)))
                           'k)))
        (apply-form (format nil "~S"
                            (cl-cc::cps-transform-ast
                             (cl-cc::make-ast-apply
                              :func (cl-cc:make-ast-function :name 'list)
                              :args (list (cl-cc:make-ast-quote :value '(1 2))))
                             'k))))
    (assert-true (search "MULTIPLE-VALUE-CALL" values-form))
    (assert-true (search "MULTIPLE-VALUE-BIND" mvb-form))
    (assert-true (search "APPLY" apply-form))))

(deftest cps-ast-defvar-and-handler-case-have-conservative-coverage
  "defvar and handler-case now produce host-backed CPS forms instead of method errors."
  (let ((defvar-form (format nil "~S"
                             (cl-cc::cps-transform-ast
                              (cl-cc::make-ast-defvar :name '*x :kind 'defparameter
                                                      :value (cl-cc:make-ast-int :value 1))
                              'k)))
        (handler-form (format nil "~S"
                              (cl-cc::cps-transform-ast
                               (cl-cc::make-ast-handler-case
                                :form (cl-cc:make-ast-int :value 1)
                               :clauses (list (list 'error 'e (cl-cc:make-ast-int :value 0))))
                               'k))))
    (assert-true (search "DEFPARAMETER" defvar-form))
    (assert-true (search "HANDLER-CASE" handler-form))))

(deftest cps-ast-clos-helpers-have-conservative-coverage
  "make-instance/slot-value/set-slot-value now produce host-backed CPS forms."
  (let ((make-form (format nil "~S"
                           (cl-cc::cps-transform-ast
                            (cl-cc::make-ast-make-instance
                             :class (cl-cc:make-ast-quote :value 'foo)
                             :initargs (list :x (cl-cc:make-ast-int :value 1)))
                            'k)))
        (slot-form (format nil "~S"
                           (cl-cc::cps-transform-ast
                            (cl-cc::make-ast-slot-value
                             :object (cl-cc:make-ast-var :name 'obj)
                             :slot 'bar)
                            'k)))
        (set-slot-form (format nil "~S"
                               (cl-cc::cps-transform-ast
                                (cl-cc::make-ast-set-slot-value
                                 :object (cl-cc:make-ast-var :name 'obj)
                                 :slot 'bar
                                 :value (cl-cc:make-ast-int :value 2))
                                'k))))
    (assert-true (search "MAKE-INSTANCE" make-form))
    (assert-true (search "SLOT-VALUE" slot-form))
    (assert-true (search "SETF" set-slot-form))))

(deftest cps-ast-more-uncovered-nodes-have-conservative-coverage
  "defclass/defgeneric/defmethod/set-gethash now produce host-backed CPS forms."
  (let ((defclass-form (format nil "~S"
                               (cl-cc::cps-transform-ast
                                (cl-cc::make-ast-defclass :name 'foo :superclasses nil :slots nil)
                                'k)))
        (defgeneric-form (format nil "~S"
                                 (cl-cc::cps-transform-ast
                                  (cl-cc::make-ast-defgeneric :name 'gf :params '(x) :combination nil)
                                  'k)))
        (defmethod-form (format nil "~S"
                                (cl-cc::cps-transform-ast
                                 (cl-cc::make-ast-defmethod :name 'gf :qualifier nil :specializers '(t) :params '(x) :body (list (cl-cc:make-ast-var :name 'x)))
                                 'k)))
        (set-gethash-form (format nil "~S"
                                  (cl-cc::cps-transform-ast
                                   (cl-cc::make-ast-set-gethash :key (cl-cc:make-ast-quote :value 'a) :table (cl-cc:make-ast-var :name 'tbl) :value (cl-cc:make-ast-int :value 1))
                                   'k))))
    (assert-true (search "DEFCLASS" defclass-form))
    (assert-true (search "DEFGENERIC" defgeneric-form))
    (assert-true (search "DEFMETHOD" defmethod-form))
    (assert-true (search "GETHASH" set-gethash-form))))
