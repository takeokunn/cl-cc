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
  "Sequence and if nodes recurse through cps-transform-sequence and cps-transform-ast."
  (let ((seq (cl-cc::cps-transform-sequence (list (cl-cc:make-ast-int :value 1)
                                                 (cl-cc:make-ast-int :value 2)) 'k))
        (iff (cl-cc::cps-transform-ast
              (cl-cc:make-ast-if
               :cond (cl-cc:make-ast-int :value 1)
               :then (cl-cc:make-ast-int :value 10)
               :else (cl-cc:make-ast-int :value 20))
              'k)))
    (assert-eq 'funcall (car seq))
    (assert-true (search "IF" (format nil "~S" iff)))))
