;;;; tests/unit/expand/expander-data-tests.lisp — Expander data table tests

(in-package :cl-cc/test)

(defsuite expander-data-suite :description "Expander data table unit tests"
  :parent cl-cc-unit-suite)


(in-suite expander-data-suite)
(deftest-each builtin-table-members
  "Key symbols are present in their respective builtin tables."
  :cases (("variadic-+"      '+      cl-cc/expand::*variadic-fold-builtins*)
          ("variadic-*"      '*      cl-cc/expand::*variadic-fold-builtins*)
          ("variadic-append" 'append cl-cc/expand::*variadic-fold-builtins*)
          ("variadic-nconc"  'nconc  cl-cc/expand::*variadic-fold-builtins*)
          ("binary-cons"     'cons   cl-cc/expand::*binary-builtins*)
          ("binary-="        '=      cl-cc/expand::*binary-builtins*)
          ("binary-mod"      'mod    cl-cc/expand::*binary-builtins*)
          ("binary-ash"      'ash    cl-cc/expand::*binary-builtins*)
          ("unary-car"       'car    cl-cc/expand::*unary-builtins*)
          ("unary-cdr"       'cdr    cl-cc/expand::*unary-builtins*)
          ("unary-not"       'not    cl-cc/expand::*unary-builtins*)
          ("unary-length"    'length cl-cc/expand::*unary-builtins*))
  (sym table)
  (assert-true (member sym table)))

(deftest cxr-builtins-completeness
  "*cxr-builtins* has all 28 compositions."
  (assert-equal 28 (length cl-cc/expand::*cxr-builtins*))
  (assert-true (member 'caar cl-cc/expand::*cxr-builtins*))
  (assert-true (member 'cddddr cl-cc/expand::*cxr-builtins*)))

(deftest-each all-builtin-names-members
  "*all-builtin-names* is the union of all sub-tables and contains representative symbols."
  :cases (("variadic-+"   '+    cl-cc/expand::*all-builtin-names*)
          ("binary-cons"  'cons cl-cc/expand::*all-builtin-names*)
          ("unary-car"    'car  cl-cc/expand::*all-builtin-names*)
          ("cxr-caar"     'caar cl-cc/expand::*all-builtin-names*)
          ("special-list" 'list cl-cc/expand::*all-builtin-names*))
  (sym table)
  (assert-true (member sym table)))

(deftest expander-data-registry-sanity
  "Core data registries and query helpers in expander-data.lisp are wired and readable."
  (assert-true (hash-table-p cl-cc/expand:*accessor-slot-map*))
  (assert-true (hash-table-p cl-cc/expand:*defstruct-slot-registry*))
  (assert-true (hash-table-p cl-cc/expand:*defstruct-type-registry*))
  (assert-true (hash-table-p cl-cc/expand:*symbol-macro-table*))
  (assert-true (hash-table-p cl-cc/expand:*constant-table*))
  (assert-true (hash-table-p cl-cc/expand:*compiler-macro-table*))
  (assert-true (hash-table-p cl-cc/expand::*setf-compound-place-handlers*))
  (assert-true (functionp cl-cc/expand:*macro-eval-fn*))
  (assert-true (cl-cc/expand::compiler-special-form-p 'if))
  (assert-false (cl-cc/expand::compiler-special-form-p 'not-a-special-form))
  (assert-true (cl-cc/expand::builtin-name-p 'append))
  (assert-false (cl-cc/expand::builtin-name-p 'not-a-builtin))
   (assert-equal 0 (cl-cc/expand::variadic-fold-identity '+))
   (assert-equal 1 (cl-cc/expand::variadic-fold-identity '*))
   (assert-equal nil (cl-cc/expand::variadic-fold-identity 'length)))

(deftest bootstrap-macro-eval-errors-without-our-eval
  "%bootstrap-macro-eval rejects host fallback when OUR-EVAL is unavailable."
  (let ((original (when (fboundp 'cl-cc/expand::our-eval)
                    (symbol-function 'cl-cc/expand::our-eval))))
    (unwind-protect
         (progn
           (when original
             (fmakunbound 'cl-cc/expand::our-eval))
           (assert-signals error
             (cl-cc/expand::%bootstrap-macro-eval '(+ 1 2))))
      (when original
        (setf (symbol-function 'cl-cc/expand::our-eval) original)))))
