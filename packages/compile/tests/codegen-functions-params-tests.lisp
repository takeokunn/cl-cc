;;;; tests/unit/compile/codegen-functions-params-tests.lisp
;;;; Unit tests for src/compile/codegen-functions-params.lisp
;;;;
;;;; Covers: extract-constant-value, dynamic-extent-declared-p,
;;;;   rest-param-stack-alloc-p, build-all-param-bindings.
;;;;
;;;; All tested functions are pure (no ctx/emit side effects), so no
;;;; compilation context is needed.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── extract-constant-value ──────────────────────────────────────────────

(deftest-each extract-constant-value-constants
  "extract-constant-value returns the constant and T for compile-time literals."
  :cases (("int-42"       (cl-cc/ast:make-ast-int   :value 42)    42  t)
          ("int-0"        (cl-cc/ast:make-ast-int   :value 0)      0  t)
          ("int-neg"      (cl-cc/ast:make-ast-int   :value -7)    -7  t)
          ("quote-symbol" (cl-cc/ast:make-ast-quote :value 'hello) 'hello t)
          ("quote-list"   (cl-cc/ast:make-ast-quote :value '(1 2)) '(1 2) t)
          ("var-t"        (cl-cc/ast:make-ast-var   :name 't)      t  t)
          ("var-nil"      (cl-cc/ast:make-ast-var   :name 'nil)    nil t))
  (ast expected-val expected-const)
  (multiple-value-bind (val is-const)
      (cl-cc/compile::extract-constant-value ast)
    (assert-equal expected-val val)
    (assert-equal expected-const is-const)))

(deftest-each extract-constant-value-non-constants
  "extract-constant-value returns (nil nil) for non-constant AST nodes."
  :cases (("var-x"    (cl-cc/ast:make-ast-var  :name 'x))
          ("call"     (cl-cc/ast:make-ast-call  :func 'f :args nil))
          ("binop"    (cl-cc/ast:make-ast-binop :op '+ :lhs (cl-cc/ast:make-ast-int :value 1)
                                                         :rhs (cl-cc/ast:make-ast-int :value 2)))
          ("lambda"   (cl-cc/ast:make-ast-lambda :params '(x) :body nil)))
  (ast)
  (multiple-value-bind (val is-const)
      (cl-cc/compile::extract-constant-value ast)
    (assert-null val)
    (assert-false is-const)))

;;; ─── dynamic-extent-declared-p ───────────────────────────────────────────

(deftest-each dynamic-extent-declared-p-truthy
  "dynamic-extent-declared-p returns T when name appears in (dynamic-extent ...)."
  :cases (("single"   '((dynamic-extent rest))          'rest)
          ("multi"    '((dynamic-extent a b rest))       'rest)
          ("first"    '((dynamic-extent args))           'args)
          ("mixed-decls" '((ignore x) (dynamic-extent r) (type integer n)) 'r))
  (declarations name)
  (assert-true (cl-cc/compile::dynamic-extent-declared-p declarations name)))

(deftest-each dynamic-extent-declared-p-falsy
  "dynamic-extent-declared-p returns NIL when name is absent or declarations empty."
  :cases (("empty"        '()                            'rest)
          ("other-decl"   '((ignore rest))               'rest)
          ("wrong-name"   '((dynamic-extent other))      'rest)
          ("nil-decls"    nil                            'args))
  (declarations name)
  (assert-false (cl-cc/compile::dynamic-extent-declared-p declarations name)))

;;; ─── rest-param-stack-alloc-p ────────────────────────────────────────────

(deftest rest-param-stack-alloc-p-non-list-body
  "rest-param-stack-alloc-p returns NIL for non-list body (conservative)."
  (assert-false (cl-cc/compile::rest-param-stack-alloc-p nil 'rest))
  (assert-false (cl-cc/compile::rest-param-stack-alloc-p 'x  'rest)))

(deftest rest-param-stack-alloc-p-safe-consumers
  "rest-param-stack-alloc-p returns T when rest is only used by safe consumers."
  (let ((body-forms (list `(car rest))))
    (assert-true (cl-cc/compile::rest-param-stack-alloc-p body-forms 'rest))))

;;; ─── build-all-param-bindings ────────────────────────────────────────────

(deftest build-all-param-bindings-required-only
  "build-all-param-bindings with only required params returns correct alist."
  (let ((result (cl-cc/compile::build-all-param-bindings
                 '(a b c) '(:r0 :r1 :r2) nil nil nil)))
    (assert-equal '((a . :r0) (b . :r1) (c . :r2)) result)))

(deftest build-all-param-bindings-with-optional
  "build-all-param-bindings appends optional bindings after required."
  (let ((result (cl-cc/compile::build-all-param-bindings
                 '(x) '(:r0)
                 '((opt . :r1))
                 nil nil)))
    (assert-equal '((x . :r0) (opt . :r1)) result)))

(deftest build-all-param-bindings-with-rest
  "build-all-param-bindings includes rest binding after required and optional."
  (let ((result (cl-cc/compile::build-all-param-bindings
                 '(x) '(:r0) nil '(rest . :r2) nil)))
    (assert-equal '((x . :r0) (rest . :r2)) result)))

(deftest build-all-param-bindings-with-key
  "build-all-param-bindings appends key bindings last."
  (let ((result (cl-cc/compile::build-all-param-bindings
                 '(a) '(:r0) nil nil '((kw . :r3)))))
    (assert-equal '((a . :r0) (kw . :r3)) result)))

(deftest-each build-all-param-bindings-combined
  "build-all-param-bindings ordering: required → optional → rest → key."
  :cases (("all-present"
           '(a) '(:r0) '((b . :r1)) '(c . :r2) '((d . :r3))
           '((a . :r0) (b . :r1) (c . :r2) (d . :r3)))
          ("empty"
           nil nil nil nil nil
           nil))
  (params param-regs opt-bindings rest-binding key-bindings expected)
  (assert-equal expected
                (cl-cc/compile::build-all-param-bindings
                 params param-regs opt-bindings rest-binding key-bindings)))

(deftest build-all-param-bindings-preserves-order
  "build-all-param-bindings preserves left-to-right parameter order."
  (let* ((params '(x y z))
         (regs   '(:ra :rb :rc))
         (result (cl-cc/compile::build-all-param-bindings params regs nil nil nil)))
    (assert-equal 'x (car (first result)))
    (assert-equal :ra (cdr (first result)))
    (assert-equal 'y (car (second result)))
    (assert-equal 'z (car (third result)))))
