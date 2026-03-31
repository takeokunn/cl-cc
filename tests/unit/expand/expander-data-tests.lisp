;;;; tests/unit/expand/expander-data-tests.lisp — Expander data table tests

(in-package :cl-cc/test)

(defsuite expander-data-suite :description "Expander data table unit tests")

(deftest variadic-fold-builtins-contents
  "*variadic-fold-builtins* includes + * append nconc."
  (assert-true (member '+ cl-cc::*variadic-fold-builtins*))
  (assert-true (member '* cl-cc::*variadic-fold-builtins*))
  (assert-true (member 'append cl-cc::*variadic-fold-builtins*))
  (assert-true (member 'nconc cl-cc::*variadic-fold-builtins*)))

(deftest binary-builtins-contents
  "*binary-builtins* includes key entries."
  (assert-true (member 'cons cl-cc::*binary-builtins*))
  (assert-true (member '= cl-cc::*binary-builtins*))
  (assert-true (member 'mod cl-cc::*binary-builtins*))
  (assert-true (member 'ash cl-cc::*binary-builtins*)))

(deftest unary-builtins-contents
  "*unary-builtins* includes key entries."
  (assert-true (member 'car cl-cc::*unary-builtins*))
  (assert-true (member 'cdr cl-cc::*unary-builtins*))
  (assert-true (member 'not cl-cc::*unary-builtins*))
  (assert-true (member 'length cl-cc::*unary-builtins*)))

(deftest cxr-builtins-completeness
  "*cxr-builtins* has all 28 compositions."
  (assert-equal 28 (length cl-cc::*cxr-builtins*))
  (assert-true (member 'caar cl-cc::*cxr-builtins*))
  (assert-true (member 'cddddr cl-cc::*cxr-builtins*)))

(deftest all-builtin-names-union
  "*all-builtin-names* is the union of all sub-tables."
  (assert-true (member '+ cl-cc::*all-builtin-names*))
  (assert-true (member 'cons cl-cc::*all-builtin-names*))
  (assert-true (member 'car cl-cc::*all-builtin-names*))
  (assert-true (member 'caar cl-cc::*all-builtin-names*))
  (assert-true (member 'list cl-cc::*all-builtin-names*)))
