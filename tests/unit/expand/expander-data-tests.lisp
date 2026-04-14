;;;; tests/unit/expand/expander-data-tests.lisp — Expander data table tests

(in-package :cl-cc/test)

(defsuite expander-data-suite :description "Expander data table unit tests"
  :parent cl-cc-unit-suite)


(in-suite expander-data-suite)
(deftest-each builtin-table-members
  "Key symbols are present in their respective builtin tables."
  :cases (("variadic-+"      '+      cl-cc::*variadic-fold-builtins*)
          ("variadic-*"      '*      cl-cc::*variadic-fold-builtins*)
          ("variadic-append" 'append cl-cc::*variadic-fold-builtins*)
          ("variadic-nconc"  'nconc  cl-cc::*variadic-fold-builtins*)
          ("binary-cons"     'cons   cl-cc::*binary-builtins*)
          ("binary-="        '=      cl-cc::*binary-builtins*)
          ("binary-mod"      'mod    cl-cc::*binary-builtins*)
          ("binary-ash"      'ash    cl-cc::*binary-builtins*)
          ("unary-car"       'car    cl-cc::*unary-builtins*)
          ("unary-cdr"       'cdr    cl-cc::*unary-builtins*)
          ("unary-not"       'not    cl-cc::*unary-builtins*)
          ("unary-length"    'length cl-cc::*unary-builtins*))
  (sym table)
  (assert-true (member sym table)))

(deftest cxr-builtins-completeness
  "*cxr-builtins* has all 28 compositions."
  (assert-equal 28 (length cl-cc::*cxr-builtins*))
  (assert-true (member 'caar cl-cc::*cxr-builtins*))
  (assert-true (member 'cddddr cl-cc::*cxr-builtins*)))

(deftest-each all-builtin-names-members
  "*all-builtin-names* is the union of all sub-tables and contains representative symbols."
  :cases (("variadic-+"   '+    cl-cc::*all-builtin-names*)
          ("binary-cons"  'cons cl-cc::*all-builtin-names*)
          ("unary-car"    'car  cl-cc::*all-builtin-names*)
          ("cxr-caar"     'caar cl-cc::*all-builtin-names*)
          ("special-list" 'list cl-cc::*all-builtin-names*))
  (sym table)
  (assert-true (member sym table)))
