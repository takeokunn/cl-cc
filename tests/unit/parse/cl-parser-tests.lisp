;;;; tests/unit/parse/cl-parser-tests.lisp — CL parser and grammar unit tests
;;;;
;;;; Tests: parse-source, parse-all-forms, parse-cl-source,
;;;; lower-sexp-to-ast, ast-to-sexp roundtrip, token-stream operations,
;;;; parse-compiler-lambda-list, lambda-list-has-extended-p,
;;;; sexp-head-to-kind, specialized form parsers.

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun parse-one (source)
  "Parse SOURCE and return the first s-expression."
  (cl-cc:parse-source source))

(defun parse-many (source)
  "Parse SOURCE and return all s-expressions."
  (cl-cc:parse-all-forms source))

(defun lower (sexp)
  "Lower an s-expression to an AST node."
  (cl-cc::lower-sexp-to-ast sexp))

;;; ─── parse-source ───────────────────────────────────────────────────────────

(deftest parser-parse-source-integer
  "parse-source: integer literal"
  (assert-= 42 (parse-one "42")))

(deftest parser-parse-source-negative
  "parse-source: negative integer"
  (assert-= -7 (parse-one "-7")))

(deftest parser-parse-source-string
  "parse-source: string literal"
  (assert-string= "hello" (parse-one "\"hello\"")))

(deftest parser-parse-source-symbol
  "parse-source: symbol is interned and uppercased"
  (assert-equal "FOO" (symbol-name (parse-one "foo"))))

(deftest parser-parse-source-nil
  "parse-source: nil literal"
  (assert-null (parse-one "nil")))

(deftest parser-parse-source-t
  "parse-source: t literal"
  (assert-true (eq t (parse-one "t"))))

(deftest parser-parse-source-empty-list
  "parse-source: empty list"
  (assert-null (parse-one "()")))

(deftest parser-parse-source-list
  "parse-source: simple list"
  (let ((result (parse-one "(+ 1 2)")))
    (assert-equal '(+ 1 2) result)))

(deftest parser-parse-source-nested-list
  "parse-source: nested list"
  (let ((result (parse-one "(if (> x 0) x (- x))")))
    (assert-true (consp result))
    (assert-eq 'if (first result))))

(deftest parser-parse-source-quote-sugar
  "parse-source: 'x expands to (quote x)"
  (let ((result (parse-one "'foo")))
    (assert-true (consp result))
    (assert-eq 'quote (first result))
    (assert-equal "FOO" (symbol-name (second result)))))

(deftest parser-parse-source-empty-error
  "parse-source: empty input signals error"
  (assert-signals error (parse-one "")))

;;; ─── parse-all-forms ────────────────────────────────────────────────────────

(deftest parser-parse-all-forms-multiple
  "parse-all-forms: parses multiple top-level forms"
  (let ((forms (parse-many "(defun f (x) x) (defun g (y) y)")))
    (assert-= 2 (length forms))
    (assert-eq 'defun (first (first forms)))
    (assert-eq 'defun (first (second forms)))))

(deftest parser-parse-all-forms-single
  "parse-all-forms: single form returns one-element list"
  (let ((forms (parse-many "(+ 1 2)")))
    (assert-= 1 (length forms))
    (assert-equal '(+ 1 2) (first forms))))

(deftest parser-parse-all-forms-empty
  "parse-all-forms: empty source returns nil"
  (assert-null (parse-many "")))

(deftest parser-parse-all-forms-atoms
  "parse-all-forms: sequence of atoms"
  (let ((forms (parse-many "1 2 3")))
    (assert-= 3 (length forms))
    (assert-= 1 (first forms))
    (assert-= 2 (second forms))
    (assert-= 3 (third forms))))

;;; ─── parse-cl-source ────────────────────────────────────────────────────────

(deftest grammar-parse-cl-source-returns-cst
  "parse-cl-source: returns CST nodes"
  (multiple-value-bind (cst-list diagnostics)
      (cl-cc::parse-cl-source "(+ 1 2)")
    (declare (ignore diagnostics))
    (assert-= 1 (length cst-list))
    (assert-true (cl-cc:cst-interior-p (first cst-list)))))

(deftest grammar-parse-cl-source-empty
  "parse-cl-source: empty string returns no forms"
  (multiple-value-bind (cst-list diagnostics)
      (cl-cc::parse-cl-source "")
    (declare (ignore diagnostics))
    (assert-null cst-list)))

(deftest grammar-parse-cl-source-diagnostics
  "parse-cl-source: returns second value for diagnostics"
  (multiple-value-bind (cst-list diagnostics)
      (cl-cc::parse-cl-source "(+ 1 2)")
    (declare (ignore cst-list))
    ;; Valid source: diagnostics list exists (may be empty)
    (assert-true (listp diagnostics))))

(deftest grammar-parse-cl-source-multiple-forms
  "parse-cl-source: multiple forms produce multiple CST nodes"
  (multiple-value-bind (cst-list diagnostics)
      (cl-cc::parse-cl-source "1 2 3")
    (declare (ignore diagnostics))
    (assert-= 3 (length cst-list))))

;;; ─── token-stream ────────────────────────────────────────────────────────────

(deftest grammar-token-stream-creation
  "token-stream: make-token-stream creates struct with fields"
  (let ((ts (cl-cc::make-token-stream :tokens nil :source "test")))
    (assert-true (cl-cc::token-stream-p ts))
    (assert-null (cl-cc::token-stream-tokens ts))
    (assert-string= "test" (cl-cc::token-stream-source ts))))

(deftest grammar-ts-peek-empty
  "ts-peek: returns nil on empty stream"
  (let ((ts (cl-cc::make-token-stream :tokens nil :source "")))
    (assert-null (cl-cc::ts-peek ts))))

(deftest grammar-ts-at-end-p-empty
  "ts-at-end-p: returns true on empty stream"
  (let ((ts (cl-cc::make-token-stream :tokens nil :source "")))
    (assert-true (cl-cc::ts-at-end-p ts))))

(deftest grammar-ts-advance-consumes-token
  "ts-advance: consumes and returns the first token"
  (let* ((tokens (cl-cc:lex-all "42"))
         (ts (cl-cc::make-token-stream :tokens tokens :source "42"))
         (first-tok (cl-cc::ts-advance ts)))
    (assert-eq :T-INT (cl-cc:lexer-token-type first-tok))))

;;; ─── parse-compiler-lambda-list ─────────────────────────────────────────────

(deftest parser-lambda-list-required-only
  "parse-compiler-lambda-list: required params only"
  (multiple-value-bind (required optional rest-param key-params)
      (cl-cc::parse-compiler-lambda-list '(x y z))
    (assert-equal '(x y z) required)
    (assert-null optional)
    (assert-null rest-param)
    (assert-null key-params)))

(deftest parser-lambda-list-with-optional
  "parse-compiler-lambda-list: &optional params"
  (multiple-value-bind (required optional rest-param key-params)
      (cl-cc::parse-compiler-lambda-list '(x &optional (y 10)))
    (assert-equal '(x) required)
    (assert-= 1 (length optional))
    (assert-eq 'y (first (first optional)))
    (assert-= 10 (second (first optional)))
    (assert-null rest-param)
    (assert-null key-params)))

(deftest parser-lambda-list-with-rest
  "parse-compiler-lambda-list: &rest param"
  (multiple-value-bind (required optional rest-param key-params)
      (cl-cc::parse-compiler-lambda-list '(x &rest args))
    (assert-equal '(x) required)
    (assert-null optional)
    (assert-eq 'args rest-param)
    (assert-null key-params)))

(deftest parser-lambda-list-with-key
  "parse-compiler-lambda-list: &key params"
  (multiple-value-bind (required optional rest-param key-params)
      (cl-cc::parse-compiler-lambda-list '(x &key (size 0)))
    (assert-equal '(x) required)
    (assert-null optional)
    (assert-null rest-param)
    (assert-= 1 (length key-params))
    (assert-eq 'size (first (first key-params)))))

(deftest parser-lambda-list-empty
  "parse-compiler-lambda-list: empty lambda list"
  (multiple-value-bind (required optional rest-param key-params)
      (cl-cc::parse-compiler-lambda-list '())
    (assert-null required)
    (assert-null optional)
    (assert-null rest-param)
    (assert-null key-params)))

;;; ─── lambda-list-has-extended-p ──────────────────────────────────────────────

(deftest parser-lambda-list-has-extended-optional
  "lambda-list-has-extended-p: true for &optional"
  (assert-true (cl-cc::lambda-list-has-extended-p '(x &optional y))))

(deftest parser-lambda-list-has-extended-rest
  "lambda-list-has-extended-p: true for &rest"
  (assert-true (cl-cc::lambda-list-has-extended-p '(x &rest args))))

(deftest parser-lambda-list-has-extended-key
  "lambda-list-has-extended-p: true for &key"
  (assert-true (cl-cc::lambda-list-has-extended-p '(x &key y))))

(deftest parser-lambda-list-has-extended-false
  "lambda-list-has-extended-p: false for simple params"
  (assert-false (cl-cc::lambda-list-has-extended-p '(x y z))))

(deftest parser-lambda-list-has-extended-empty
  "lambda-list-has-extended-p: false for empty list"
  (assert-false (cl-cc::lambda-list-has-extended-p '())))

;;; ─── lower-sexp-to-ast: atoms ────────────────────────────────────────────────

(deftest lower-integer-produces-ast-int
  "lower-sexp-to-ast: integer -> ast-int"
  (let ((node (lower 42)))
    (assert-true (cl-cc::ast-int-p node))
    (assert-= 42 (cl-cc::ast-int-value node))))

(deftest lower-symbol-produces-ast-var
  "lower-sexp-to-ast: symbol -> ast-var"
  (let ((node (lower 'x)))
    (assert-true (cl-cc::ast-var-p node))
    (assert-eq 'x (cl-cc::ast-var-name node))))

(deftest lower-nil-produces-ast-quote
  "lower-sexp-to-ast: nil -> ast-quote wrapping nil"
  (let ((node (lower nil)))
    (assert-true (cl-cc::ast-quote-p node))
    (assert-null (cl-cc::ast-quote-value node))))

(deftest lower-t-produces-ast-quote
  "lower-sexp-to-ast: t -> ast-quote wrapping t"
  (let ((node (lower t)))
    (assert-true (cl-cc::ast-quote-p node))
    (assert-eq t (cl-cc::ast-quote-value node))))

(deftest lower-string-produces-ast-quote
  "lower-sexp-to-ast: string -> ast-quote"
  (let ((node (lower "hello")))
    (assert-true (cl-cc::ast-quote-p node))
    (assert-string= "hello" (cl-cc::ast-quote-value node))))

(deftest lower-float-produces-ast-quote
  "lower-sexp-to-ast: float -> ast-quote"
  (let ((node (lower 3.14)))
    (assert-true (cl-cc::ast-quote-p node))))

(deftest lower-character-produces-ast-quote
  "lower-sexp-to-ast: character -> ast-quote"
  (let ((node (lower #\a)))
    (assert-true (cl-cc::ast-quote-p node))
    (assert-true (char= #\a (cl-cc::ast-quote-value node)))))

;;; ─── lower-sexp-to-ast: special forms ───────────────────────────────────────

(deftest lower-if-form
  "lower-sexp-to-ast: if form -> ast-if"
  (let ((node (lower '(if x 1 2))))
    (assert-true (cl-cc::ast-if-p node))
    (assert-true (cl-cc::ast-var-p (cl-cc::ast-if-cond node)))
    (assert-= 1 (cl-cc::ast-int-value (cl-cc::ast-if-then node)))))

(deftest lower-if-without-else
  "lower-sexp-to-ast: (if cond then) inserts nil else"
  (let ((node (lower '(if x 1))))
    (assert-true (cl-cc::ast-if-p node))
    (assert-true (cl-cc::ast-quote-p (cl-cc::ast-if-else node)))))

(deftest lower-progn-form
  "lower-sexp-to-ast: progn form -> ast-progn"
  (let ((node (lower '(progn 1 2 3))))
    (assert-true (cl-cc::ast-progn-p node))
    (assert-= 3 (length (cl-cc::ast-progn-forms node)))))

(deftest lower-let-form
  "lower-sexp-to-ast: let form -> ast-let with bindings"
  (let ((node (lower '(let ((x 1) (y 2)) x))))
    (assert-true (cl-cc::ast-let-p node))
    (assert-= 2 (length (cl-cc::ast-let-bindings node)))
    (assert-= 1 (length (cl-cc::ast-let-body node)))))

(deftest lower-let-bare-symbol
  "lower-sexp-to-ast: (let (x) body) binds x to nil"
  (let ((node (lower '(let (x) x))))
    (assert-true (cl-cc::ast-let-p node))
    (let ((binding (first (cl-cc::ast-let-bindings node))))
      (assert-eq 'x (car binding))
      (assert-true (cl-cc::ast-quote-p (cdr binding))))))

(deftest lower-lambda-form
  "lower-sexp-to-ast: lambda form -> ast-lambda"
  (let ((node (lower '(lambda (x y) (+ x y)))))
    (assert-true (cl-cc::ast-lambda-p node))
    (assert-equal '(x y) (cl-cc::ast-lambda-params node))
    (assert-= 1 (length (cl-cc::ast-lambda-body node)))))

(deftest lower-defun-form
  "lower-sexp-to-ast: defun form -> ast-defun"
  (let ((node (lower '(defun my-func (a b) (+ a b)))))
    (assert-true (cl-cc::ast-defun-p node))
    (assert-eq 'my-func (cl-cc::ast-defun-name node))
    (assert-equal '(a b) (cl-cc::ast-defun-params node))
    (assert-= 1 (length (cl-cc::ast-defun-body node)))))

(deftest lower-defvar-form
  "lower-sexp-to-ast: defvar form -> ast-defvar"
  (let ((node (lower '(defvar *x* 42))))
    (assert-true (cl-cc::ast-defvar-p node))
    (assert-eq '*x* (cl-cc::ast-defvar-name node))
    (assert-true (cl-cc::ast-int-p (cl-cc::ast-defvar-value node)))))

(deftest lower-defvar-no-value
  "lower-sexp-to-ast: (defvar *x*) with no initial value"
  (let ((node (lower '(defvar *x*))))
    (assert-true (cl-cc::ast-defvar-p node))
    (assert-null (cl-cc::ast-defvar-value node))))

(deftest lower-setq-form
  "lower-sexp-to-ast: setq form -> ast-setq"
  (let ((node (lower '(setq x 10))))
    (assert-true (cl-cc::ast-setq-p node))
    (assert-eq 'x (cl-cc::ast-setq-var node))
    (assert-= 10 (cl-cc::ast-int-value (cl-cc::ast-setq-value node)))))

(deftest lower-setq-multi-var
  "lower-sexp-to-ast: multi-var setq -> ast-progn of setq"
  (let ((node (lower '(setq a 1 b 2))))
    (assert-true (cl-cc::ast-progn-p node))
    (assert-= 2 (length (cl-cc::ast-progn-forms node)))))

(deftest lower-quote-form
  "lower-sexp-to-ast: (quote x) -> ast-quote"
  (let ((node (lower '(quote hello))))
    (assert-true (cl-cc::ast-quote-p node))
    (assert-eq 'hello (cl-cc::ast-quote-value node))))

(deftest lower-block-form
  "lower-sexp-to-ast: block form -> ast-block"
  (let ((node (lower '(block my-block 1 2))))
    (assert-true (cl-cc::ast-block-p node))
    (assert-eq 'my-block (cl-cc::ast-block-name node))
    (assert-= 2 (length (cl-cc::ast-block-body node)))))

(deftest lower-return-from-form
  "lower-sexp-to-ast: return-from form -> ast-return-from"
  (let ((node (lower '(return-from my-block 42))))
    (assert-true (cl-cc::ast-return-from-p node))
    (assert-eq 'my-block (cl-cc::ast-return-from-name node))))

(deftest lower-function-ref
  "lower-sexp-to-ast: (function foo) -> ast-function"
  (let ((node (lower '(function foo))))
    (assert-true (cl-cc::ast-function-p node))
    (assert-eq 'foo (cl-cc::ast-function-name node))))

(deftest lower-function-lambda
  "lower-sexp-to-ast: (function (lambda ...)) signals error (not yet supported)"
  (assert-signals error (lower '(function (lambda (x) x)))))

(deftest lower-values-form
  "lower-sexp-to-ast: values form -> ast-values"
  (let ((node (lower '(values 1 2 3))))
    (assert-true (cl-cc::ast-values-p node))
    (assert-= 3 (length (cl-cc::ast-values-forms node)))))

(deftest lower-multiple-value-bind
  "lower-sexp-to-ast: multiple-value-bind -> ast-multiple-value-bind"
  (let ((node (lower '(multiple-value-bind (a b) (values 1 2) (+ a b)))))
    (assert-true (cl-cc::ast-multiple-value-bind-p node))
    (assert-equal '(a b) (cl-cc::ast-mvb-vars node))
    (assert-= 1 (length (cl-cc::ast-mvb-body node)))))

(deftest lower-go-form
  "lower-sexp-to-ast: go form -> ast-go"
  (let ((node (lower '(go my-tag))))
    (assert-true (cl-cc::ast-go-p node))
    (assert-eq 'my-tag (cl-cc::ast-go-tag node))))

(deftest lower-tagbody-form
  "lower-sexp-to-ast: tagbody form -> ast-tagbody"
  (let ((node (lower '(tagbody start (print 1) end (print 2)))))
    (assert-true (cl-cc::ast-tagbody-p node))))

(deftest lower-catch-form
  "lower-sexp-to-ast: catch form -> ast-catch"
  (let ((node (lower '(catch 'my-tag 1 2))))
    (assert-true (cl-cc::ast-catch-p node))
    (assert-= 2 (length (cl-cc::ast-catch-body node)))))

(deftest lower-throw-form
  "lower-sexp-to-ast: throw form -> ast-throw"
  (let ((node (lower '(throw 'my-tag 42))))
    (assert-true (cl-cc::ast-throw-p node))))

(deftest lower-unwind-protect-form
  "lower-sexp-to-ast: unwind-protect form -> ast-unwind-protect"
  (let ((node (lower '(unwind-protect (risky) (cleanup)))))
    (assert-true (cl-cc::ast-unwind-protect-p node))
    (assert-= 1 (length (cl-cc::ast-unwind-cleanup node)))))

(deftest lower-apply-form
  "lower-sexp-to-ast: apply form -> ast-apply"
  (let ((node (lower '(apply #'foo '(1 2)))))
    (assert-true (cl-cc::ast-apply-p node))))

(deftest lower-funcall-form
  "lower-sexp-to-ast: funcall form -> ast-call"
  (let ((node (lower '(funcall #'foo 1 2))))
    (assert-true (cl-cc::ast-call-p node))))

(deftest lower-generic-call
  "lower-sexp-to-ast: generic call -> ast-call"
  (let ((node (lower '(my-func 1 2 3))))
    (assert-true (cl-cc::ast-call-p node))
    (assert-= 3 (length (cl-cc::ast-call-args node)))))

(deftest lower-the-form
  "lower-sexp-to-ast: (the type expr) -> ast-the"
  (let ((node (lower '(the fixnum x))))
    (assert-true (cl-cc::ast-the-p node))
    (assert-eq 'fixnum (cl-cc::ast-the-type node))))

(deftest lower-flet-form
  "lower-sexp-to-ast: flet form -> ast-flet"
  (let ((node (lower '(flet ((helper (x) (* x 2))) (helper 5)))))
    (assert-true (cl-cc::ast-flet-p node))
    (assert-= 1 (length (cl-cc::ast-flet-bindings node)))
    (assert-= 1 (length (cl-cc::ast-flet-body node)))))

(deftest lower-labels-form
  "lower-sexp-to-ast: labels form -> ast-labels"
  (let ((node (lower '(labels ((fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (fact 5)))))
    (assert-true (cl-cc::ast-labels-p node))
    (assert-= 1 (length (cl-cc::ast-labels-bindings node)))))

(deftest lower-handler-case-form
  "lower-sexp-to-ast: handler-case form -> ast-handler-case"
  (let ((node (lower '(handler-case (risky) (error (e) (print e))))))
    (assert-true (cl-cc::ast-handler-case-p node))
    (assert-= 1 (length (cl-cc::ast-handler-case-clauses node)))))

;;; ─── lower-sexp-to-ast: error cases ────────────────────────────────────────

(deftest lower-if-error-wrong-arity
  "lower-sexp-to-ast: (if) with missing args signals error"
  (assert-signals error (lower '(if))))

(deftest lower-if-error-too-many-args
  "lower-sexp-to-ast: (if a b c d) signals error"
  (assert-signals error (lower '(if a b c d))))

(deftest lower-let-error-no-bindings
  "lower-sexp-to-ast: (let) with no bindings signals error"
  (assert-signals error (lower '(let))))

(deftest lower-defun-error-no-params
  "lower-sexp-to-ast: (defun f) without params signals error"
  (assert-signals error (lower '(defun f))))

(deftest lower-setq-error-odd-args
  "lower-sexp-to-ast: (setq x) with odd number of args signals error"
  (assert-signals error (lower '(setq x))))

(deftest lower-quote-error-wrong-arity
  "lower-sexp-to-ast: (quote) with no arg signals error"
  (assert-signals error (lower '(quote))))

(deftest lower-function-error-no-arg
  "lower-sexp-to-ast: (function) with no arg signals error"
  (assert-signals error (lower '(function))))

;;; ─── ast-to-sexp roundtrip ──────────────────────────────────────────────────

(defun ast-roundtrip (sexp)
  "Lower sexp to AST then convert back to sexp."
  (cl-cc::ast-to-sexp (lower sexp)))

(deftest ast-roundtrip-integer
  "ast-to-sexp roundtrip: integer"
  (assert-= 42 (ast-roundtrip 42)))

(deftest ast-roundtrip-symbol
  "ast-to-sexp roundtrip: symbol"
  (assert-eq 'x (ast-roundtrip 'x)))

(deftest ast-roundtrip-if
  "ast-to-sexp roundtrip: if form"
  (let ((result (ast-roundtrip '(if x 1 2))))
    (assert-eq 'if (first result))
    (assert-eq 'x (second result))
    (assert-= 1 (third result))
    (assert-= 2 (fourth result))))

(deftest ast-roundtrip-let
  "ast-to-sexp roundtrip: let form"
  (let ((result (ast-roundtrip '(let ((x 10)) x))))
    (assert-eq 'let (first result))))

(deftest ast-roundtrip-lambda
  "ast-to-sexp roundtrip: lambda form"
  (let ((result (ast-roundtrip '(lambda (x y) (+ x y)))))
    (assert-eq 'lambda (first result))
    (assert-equal '(x y) (second result))))

(deftest ast-roundtrip-defun
  "ast-to-sexp roundtrip: defun form"
  (let ((result (ast-roundtrip '(defun add (a b) (+ a b)))))
    (assert-eq 'defun (first result))
    (assert-eq 'add (second result))
    (assert-equal '(a b) (third result))))

(deftest ast-roundtrip-defvar-with-value
  "ast-to-sexp roundtrip: defvar with value"
  (let ((result (ast-roundtrip '(defvar *count* 0))))
    (assert-eq 'defvar (first result))
    (assert-eq '*count* (second result))
    (assert-= 0 (third result))))

(deftest ast-roundtrip-quote
  "ast-to-sexp roundtrip: quoted form"
  (let ((result (ast-roundtrip '(quote hello))))
    (assert-equal '(quote hello) result)))

(deftest ast-roundtrip-progn
  "ast-to-sexp roundtrip: progn form"
  (let ((result (ast-roundtrip '(progn 1 2 3))))
    (assert-eq 'progn (first result))
    (assert-= 3 (length (rest result)))))

(deftest ast-roundtrip-setq
  "ast-to-sexp roundtrip: setq form"
  (let ((result (ast-roundtrip '(setq x 42))))
    (assert-equal '(setq x 42) result)))

(deftest ast-roundtrip-block
  "ast-to-sexp roundtrip: block form"
  (let ((result (ast-roundtrip '(block outer 1 2))))
    (assert-eq 'block (first result))
    (assert-eq 'outer (second result))))

;;; ─── sexp-head-to-kind ───────────────────────────────────────────────────────

(deftest grammar-sexp-head-to-kind-defun
  "sexp-head-to-kind: defun -> :defun"
  (assert-eq :defun (cl-cc::sexp-head-to-kind 'defun)))

(deftest grammar-sexp-head-to-kind-let
  "sexp-head-to-kind: let -> :let"
  (assert-eq :let (cl-cc::sexp-head-to-kind 'let)))

(deftest grammar-sexp-head-to-kind-if
  "sexp-head-to-kind: if -> :if"
  (assert-eq :if (cl-cc::sexp-head-to-kind 'if)))

(deftest grammar-sexp-head-to-kind-lambda
  "sexp-head-to-kind: lambda -> :lambda"
  (assert-eq :lambda (cl-cc::sexp-head-to-kind 'lambda)))

(deftest grammar-sexp-head-to-kind-setq
  "sexp-head-to-kind: setq -> :setq"
  (assert-eq :setq (cl-cc::sexp-head-to-kind 'setq)))

(deftest grammar-sexp-head-to-kind-progn
  "sexp-head-to-kind: progn -> :progn"
  (assert-eq :progn (cl-cc::sexp-head-to-kind 'progn)))

(deftest grammar-sexp-head-to-kind-defclass
  "sexp-head-to-kind: defclass -> :defclass"
  (assert-eq :defclass (cl-cc::sexp-head-to-kind 'defclass)))

(deftest grammar-sexp-head-to-kind-unknown
  "sexp-head-to-kind: unknown symbol -> :call"
  (assert-eq :call (cl-cc::sexp-head-to-kind 'completely-unknown-symbol)))

;;; ─── Grammar specialized parsers ────────────────────────────────────────────

(deftest grammar-parse-cl-form-integer
  "parse-cl-form: integer token -> cst-token of kind :T-INT"
  (let* ((tokens (cl-cc:lex-all "42"))
         (ts (cl-cc::make-token-stream :tokens tokens :source "42"))
         (form (cl-cc::parse-cl-form ts)))
    (assert-true (cl-cc:cst-token-p form))
    (assert-= 42 (cl-cc:cst-token-value form))))

(deftest grammar-parse-cl-form-string
  "parse-cl-form: string token -> cst-token"
  (let* ((tokens (cl-cc:lex-all "\"hello\""))
         (ts (cl-cc::make-token-stream :tokens tokens :source "\"hello\""))
         (form (cl-cc::parse-cl-form ts)))
    (assert-true (cl-cc:cst-token-p form))
    (assert-string= "hello" (cl-cc:cst-token-value form))))

(deftest grammar-parse-cl-form-list
  "parse-cl-form: list -> cst-interior node"
  (let* ((tokens (cl-cc:lex-all "(1 2 3)"))
         (ts (cl-cc::make-token-stream :tokens tokens :source "(1 2 3)"))
         (form (cl-cc::parse-cl-form ts)))
    (assert-true (cl-cc:cst-interior-p form))
    (assert-= 3 (length (cl-cc:cst-children form)))))

(deftest grammar-parse-cl-form-empty-list
  "parse-cl-form: empty list -> empty cst-interior"
  (let* ((tokens (cl-cc:lex-all "()"))
         (ts (cl-cc::make-token-stream :tokens tokens :source "()"))
         (form (cl-cc::parse-cl-form ts)))
    (assert-true (cl-cc:cst-interior-p form))
    (assert-null (cl-cc:cst-children form))))

(deftest grammar-parse-cl-form-quote-sugar
  "parse-cl-form: 'x -> :quote interior node"
  (let* ((tokens (cl-cc:lex-all "'foo"))
         (ts (cl-cc::make-token-stream :tokens tokens :source "'foo"))
         (form (cl-cc::parse-cl-form ts)))
    (assert-true (cl-cc:cst-interior-p form))
    (assert-eq :quote (cl-cc:cst-node-kind form))))

(deftest grammar-parse-cl-form-backquote
  "parse-cl-form: `x -> :quasiquote interior node"
  (let* ((tokens (cl-cc:lex-all "`foo"))
         (ts (cl-cc::make-token-stream :tokens tokens :source "`foo"))
         (form (cl-cc::parse-cl-form ts)))
    (assert-true (cl-cc:cst-interior-p form))
    (assert-eq :quasiquote (cl-cc:cst-node-kind form))))

(deftest grammar-parse-cl-form-function-ref
  "parse-cl-form: #'fn -> :function interior node"
  (let* ((tokens (cl-cc:lex-all "#'foo"))
         (ts (cl-cc::make-token-stream :tokens tokens :source "#'foo"))
         (form (cl-cc::parse-cl-form ts)))
    (assert-true (cl-cc:cst-interior-p form))
    (assert-eq :function (cl-cc:cst-node-kind form))))

(deftest grammar-parse-cl-form-at-end
  "parse-cl-form: returns nil at end of stream"
  (let* ((ts (cl-cc::make-token-stream :tokens nil :source "")))
    (assert-null (cl-cc::parse-cl-form ts))))

;;; ─── defmacro lowering ───────────────────────────────────────────────────────

(deftest lower-defmacro-form
  "lower-sexp-to-ast: defmacro form -> ast-defmacro"
  (let ((node (lower '(defmacro my-mac (x) `(list ,x)))))
    (assert-true (cl-cc::ast-defmacro-p node))
    (assert-eq 'my-mac (cl-cc::ast-defmacro-name node))
    (assert-equal '(x) (cl-cc::ast-defmacro-lambda-list node))))

;;; ─── defclass lowering ───────────────────────────────────────────────────────

(deftest lower-defclass-form
  "lower-sexp-to-ast: defclass form -> ast-defclass"
  (let ((node (lower '(defclass point () (x y)))))
    (assert-true (cl-cc::ast-defclass-p node))
    (assert-eq 'point (cl-cc::ast-defclass-name node))
    (assert-null (cl-cc::ast-defclass-superclasses node))
    (assert-= 2 (length (cl-cc::ast-defclass-slots node)))))

(deftest lower-defclass-with-superclass
  "lower-sexp-to-ast: defclass with superclasses"
  (let ((node (lower '(defclass colored-point (point) (color)))))
    (assert-true (cl-cc::ast-defclass-p node))
    (assert-equal '(point) (cl-cc::ast-defclass-superclasses node))))

;;; ─── defgeneric / defmethod lowering ────────────────────────────────────────

(deftest lower-defgeneric-form
  "lower-sexp-to-ast: defgeneric form -> ast-defgeneric"
  (let ((node (lower '(defgeneric area (shape)))))
    (assert-true (cl-cc::ast-defgeneric-p node))
    (assert-eq 'area (cl-cc::ast-defgeneric-name node))
    (assert-equal '(shape) (cl-cc::ast-defgeneric-params node))))

(deftest lower-defmethod-form
  "lower-sexp-to-ast: defmethod form -> ast-defmethod"
  (let ((node (lower '(defmethod area ((s circle)) (* pi (expt (slot-value s 'radius) 2))))))
    (assert-true (cl-cc::ast-defmethod-p node))
    (assert-eq 'area (cl-cc::ast-defmethod-name node))
    (assert-equal '(s) (cl-cc::ast-defmethod-params node))))

;;; ─── make-instance lowering ──────────────────────────────────────────────────

(deftest lower-make-instance-form
  "lower-sexp-to-ast: make-instance -> ast-make-instance"
  (let ((node (lower '(make-instance 'point :x 1 :y 2))))
    (assert-true (cl-cc::ast-make-instance-p node))
    (assert-= 2 (length (cl-cc::ast-make-instance-initargs node)))))

;;; ─── slot-value lowering ─────────────────────────────────────────────────────

(deftest lower-slot-value-form
  "lower-sexp-to-ast: slot-value -> ast-slot-value"
  (let ((node (lower '(slot-value obj 'name))))
    (assert-true (cl-cc::ast-slot-value-p node))
    (assert-eq 'name (cl-cc::ast-slot-value-slot node))))

;;; ─── NEW: parse-source atom parsing ────────────────────────────────────────

(deftest-each parser-atom-types
  "parse-source: various atom types parsed correctly"
  :cases (("float"          "3.14"        3.14)
          ("zero"           "0"           0)
          ("large-int"      "999999"      999999)
          ("keyword"        ":foo"        :foo)
          ("keyword-upper"  ":BAR"        :bar)
          ("char-a"         "#\\a"        #\a)
          ("char-space"     "#\\Space"    #\Space)
          ("char-newline"   "#\\Newline"  #\Newline))
  (source expected)
  (let ((result (parse-one source)))
    (assert-true (eql expected result))))

(deftest parser-parse-source-dotted-pair
  "parse-source: dotted pair (a . b)"
  (let ((result (parse-one "(a . b)")))
    (assert-true (consp result))
    (assert-equal "A" (symbol-name (car result)))
    (assert-equal "B" (symbol-name (cdr result)))))

(deftest parser-parse-source-vector
  "parse-source: #(1 2 3) parses to vector"
  (let ((result (parse-one "#(1 2 3)")))
    (assert-true (vectorp result))
    (assert-= 3 (length result))
    (assert-= 1 (aref result 0))
    (assert-= 2 (aref result 1))
    (assert-= 3 (aref result 2))))

(deftest parser-parse-source-empty-vector
  "parse-source: #() parses to empty vector"
  (let ((result (parse-one "#()")))
    (assert-true (vectorp result))
    (assert-= 0 (length result))))

(deftest parser-parse-source-backquote
  "parse-source: backquote expands to backquote form"
  (let ((result (parse-one "`(a b)")))
    (assert-true (consp result))
    ;; backquote produces (cl-cc::backquote ...)
    (assert-true (symbolp (first result)))))

(deftest parser-parse-source-unquote
  "parse-source: ,x inside backquote produces unquote form"
  (let ((result (parse-one "`,x")))
    (assert-true (consp result))))

(deftest parser-parse-source-nested-deep
  "parse-source: deeply nested list"
  (let ((result (parse-one "((((((1))))))")))
    (assert-true (consp result))
    ;; Traverse 5 levels of nesting to reach (1)
    (assert-= 1 (first (first (first (first (first (first result)))))))))

(deftest parser-parse-source-long-symbol
  "parse-source: very long symbol name"
  (let* ((long-name (make-string 200 :initial-element #\A))
         (result (parse-one long-name)))
    (assert-true (symbolp result))
    (assert-= 200 (length (symbol-name result)))))

;;; ─── NEW: parse-all-forms additional cases ─────────────────────────────────

(deftest parser-parse-all-forms-mixed
  "parse-all-forms: mixed atoms and lists"
  (let ((forms (parse-many "42 \"hello\" (+ 1 2)")))
    (assert-= 3 (length forms))
    (assert-= 42 (first forms))
    (assert-string= "hello" (second forms))
    (assert-true (consp (third forms)))))

(deftest parser-parse-all-forms-with-comments
  "parse-all-forms: comments are skipped"
  (let ((forms (parse-many "; this is a comment
1 2")))
    (assert-= 2 (length forms))
    (assert-= 1 (first forms))
    (assert-= 2 (second forms))))

(deftest parser-parse-all-forms-whitespace-only
  "parse-all-forms: whitespace-only input returns nil"
  (assert-null (parse-many "
  ")))

(deftest parser-parse-all-forms-many-forms
  "parse-all-forms: many top-level forms"
  (let ((forms (parse-many "1 2 3 4 5 6 7 8 9 10")))
    (assert-= 10 (length forms))))

;;; ─── NEW: AST node constructors ────────────────────────────────────────────

(deftest-each ast-constructor-basic
  "AST node constructors create correct struct types"
  :cases (("ast-int"       (cl-cc::make-ast-int :value 5)              #'cl-cc::ast-int-p)
          ("ast-var"       (cl-cc::make-ast-var :name 'x)              #'cl-cc::ast-var-p)
          ("ast-quote"     (cl-cc::make-ast-quote :value 'hello)       #'cl-cc::ast-quote-p)
          ("ast-progn"     (cl-cc::make-ast-progn :forms nil)          #'cl-cc::ast-progn-p)
          ("ast-print"     (cl-cc::make-ast-print :expr nil)           #'cl-cc::ast-print-p)
          ("ast-block"     (cl-cc::make-ast-block :name 'b :body nil)  #'cl-cc::ast-block-p)
          ("ast-go"        (cl-cc::make-ast-go :tag 'done)             #'cl-cc::ast-go-p)
          ("ast-setq"      (cl-cc::make-ast-setq :var 'x :value nil)   #'cl-cc::ast-setq-p)
          ("ast-function"  (cl-cc::make-ast-function :name 'foo)       #'cl-cc::ast-function-p)
          ("ast-the"       (cl-cc::make-ast-the :type 'fixnum :value nil) #'cl-cc::ast-the-p)
          ("ast-values"    (cl-cc::make-ast-values :forms nil)         #'cl-cc::ast-values-p))
  (node pred)
  (assert-true (funcall pred node)))

(deftest ast-node-source-location
  "AST nodes store source location fields"
  (let ((node (cl-cc::make-ast-int :value 42
                                    :source-file "test.lisp"
                                    :source-line 10
                                    :source-column 5)))
    (assert-string= "test.lisp" (cl-cc::ast-source-file node))
    (assert-= 10 (cl-cc::ast-source-line node))
    (assert-= 5 (cl-cc::ast-source-column node))))

(deftest ast-location-string-full
  "ast-location-string: formats file:line:col"
  (let ((node (cl-cc::make-ast-int :value 1
                                    :source-file "foo.lisp"
                                    :source-line 3
                                    :source-column 7)))
    (assert-string= "foo.lisp:3:7" (cl-cc::ast-location-string node))))

(deftest ast-location-string-unknown
  "ast-location-string: returns <unknown location> when no source info"
  (let ((node (cl-cc::make-ast-int :value 1)))
    (assert-string= "<unknown location>" (cl-cc::ast-location-string node))))

(deftest ast-callable-slots
  "ast-callable derived structs have optional/rest/key param slots"
  (let ((node (cl-cc::make-ast-lambda :params '(x)
                                       :optional-params '((y nil))
                                       :rest-param 'args
                                       :key-params '((z nil))
                                       :body nil)))
    (assert-equal '(x) (cl-cc::ast-lambda-params node))
    (assert-equal '((y nil)) (cl-cc::ast-lambda-optional-params node))
    (assert-eq 'args (cl-cc::ast-lambda-rest-param node))
    (assert-equal '((z nil)) (cl-cc::ast-lambda-key-params node))))

(deftest ast-slot-def-full
  "ast-slot-def: all slot options stored correctly"
  (let ((slot (cl-cc::make-ast-slot-def :name 'x
                                         :initarg :x
                                         :reader 'get-x
                                         :writer 'set-x
                                         :accessor 'x-accessor
                                         :type 'integer)))
    (assert-eq 'x (cl-cc::ast-slot-name slot))
    (assert-eq :x (cl-cc::ast-slot-initarg slot))
    (assert-eq 'get-x (cl-cc::ast-slot-reader slot))
    (assert-eq 'set-x (cl-cc::ast-slot-writer slot))
    (assert-eq 'x-accessor (cl-cc::ast-slot-accessor slot))
    (assert-eq 'integer (cl-cc::ast-slot-type slot))))

;;; ─── NEW: lower-sexp-to-ast additional forms ──────────────────────────────

(deftest lower-binop-operators
  "lower-sexp-to-ast: all binary operators produce ast-binop"
  (dolist (op '(+ - * = < > <= >=))
    (let ((node (lower (list op 1 2))))
      (assert-true (cl-cc::ast-binop-p node))
      (assert-eq op (cl-cc::ast-binop-op node)))))

(deftest lower-print-form
  "lower-sexp-to-ast: print form -> ast-print"
  (let ((node (lower '(print 42))))
    (assert-true (cl-cc::ast-print-p node))
    (assert-true (cl-cc::ast-int-p (cl-cc::ast-print-expr node)))))

(deftest lower-let-single-element-binding
  "lower-sexp-to-ast: (let ((x)) body) binds x to nil"
  (let ((node (lower '(let ((x)) x))))
    (assert-true (cl-cc::ast-let-p node))
    (let ((binding (first (cl-cc::ast-let-bindings node))))
      (assert-eq 'x (car binding))
      (assert-true (cl-cc::ast-quote-p (cdr binding)))
      (assert-null (cl-cc::ast-quote-value (cdr binding))))))

(deftest lower-lambda-with-optional
  "lower-sexp-to-ast: lambda with &optional -> ast-lambda with optional-params"
  (let ((node (lower '(lambda (x &optional (y 0)) (+ x y)))))
    (assert-true (cl-cc::ast-lambda-p node))
    (assert-equal '(x) (cl-cc::ast-lambda-params node))
    (assert-= 1 (length (cl-cc::ast-lambda-optional-params node)))))

(deftest lower-lambda-with-rest
  "lower-sexp-to-ast: lambda with &rest -> ast-lambda with rest-param"
  (let ((node (lower '(lambda (x &rest args) args))))
    (assert-true (cl-cc::ast-lambda-p node))
    (assert-eq 'args (cl-cc::ast-lambda-rest-param node))))

(deftest lower-defun-with-key-params
  "lower-sexp-to-ast: defun with &key -> ast-defun with key-params"
  (let ((node (lower '(defun f (x &key (size 10)) x))))
    (assert-true (cl-cc::ast-defun-p node))
    (assert-= 1 (length (cl-cc::ast-defun-key-params node)))))

(deftest lower-defparameter-form
  "lower-sexp-to-ast: defparameter -> ast-defvar (same as defvar)"
  (let ((node (lower '(defparameter *x* 99))))
    (assert-true (cl-cc::ast-defvar-p node))
    (assert-eq '*x* (cl-cc::ast-defvar-name node))))

(deftest lower-return-from-no-value
  "lower-sexp-to-ast: (return-from blk) with no value signals error (not yet supported)"
  (assert-signals error (lower '(return-from blk))))

(deftest lower-function-setf
  "lower-sexp-to-ast: (function (setf foo)) -> ast-function"
  (let ((node (lower '(function (setf foo)))))
    (assert-true (cl-cc::ast-function-p node))
    (assert-equal '(setf foo) (cl-cc::ast-function-name node))))

(deftest lower-multiple-value-call-form
  "lower-sexp-to-ast: multiple-value-call -> ast-multiple-value-call"
  (let ((node (lower '(multiple-value-call #'list (values 1 2) (values 3 4)))))
    (assert-true (cl-cc::ast-multiple-value-call-p node))
    (assert-= 2 (length (cl-cc::ast-mv-call-args node)))))

(deftest lower-multiple-value-prog1-form
  "lower-sexp-to-ast: multiple-value-prog1 -> ast-multiple-value-prog1"
  (let ((node (lower '(multiple-value-prog1 (values 1 2) (print 3)))))
    (assert-true (cl-cc::ast-multiple-value-prog1-p node))))

(deftest lower-setf-gethash
  "lower-sexp-to-ast: (setf (gethash k tbl) v) -> ast-set-gethash"
  (let ((node (lower '(setf (gethash 'k tbl) 42))))
    (assert-true (cl-cc::ast-set-gethash-p node))))

(deftest lower-setf-slot-value
  "lower-sexp-to-ast: (setf (slot-value obj 'x) v) -> ast-set-slot-value"
  (let ((node (lower '(setf (slot-value obj 'x) 10))))
    (assert-true (cl-cc::ast-set-slot-value-p node))
    (assert-eq 'x (cl-cc::ast-set-slot-value-slot node))))

(deftest lower-setf-symbol-place
  "lower-sexp-to-ast: (setf x 10) -> ast-setq (same as setq)"
  (let ((node (lower '(setf x 10))))
    (assert-true (cl-cc::ast-setq-p node))
    (assert-eq 'x (cl-cc::ast-setq-var node))))

;;; ─── NEW: lower-sexp-to-ast error cases ───────────────────────────────────

(deftest lower-error-progn-no-body
  "lower-sexp-to-ast: (progn) signals error"
  (assert-signals error (lower '(progn))))

(deftest lower-error-let-no-body
  "lower-sexp-to-ast: (let ()) signals error"
  (assert-signals error (lower '(let ()))))

(deftest lower-error-lambda-no-body
  "lower-sexp-to-ast: (lambda ()) signals error"
  (assert-signals error (lower '(lambda ()))))

(deftest lower-error-defun-no-body
  "lower-sexp-to-ast: (defun f ()) signals error"
  (assert-signals error (lower '(defun f ()))))

(deftest lower-error-block-no-body
  "lower-sexp-to-ast: (block b) signals error"
  (assert-signals error (lower '(block b))))

(deftest lower-error-binop-wrong-arity
  "lower-sexp-to-ast: (+ 1) signals error"
  (assert-signals error (lower '(+ 1))))

(deftest lower-error-go-no-tag
  "lower-sexp-to-ast: (go) signals error"
  (assert-signals error (lower '(go))))

(deftest lower-error-catch-no-body
  "lower-sexp-to-ast: (catch 'tag) signals error"
  (assert-signals error (lower '(catch 'tag))))

(deftest lower-error-throw-wrong-arity
  "lower-sexp-to-ast: (throw 'tag) signals error"
  (assert-signals error (lower '(throw 'tag))))

(deftest lower-error-unwind-no-cleanup
  "lower-sexp-to-ast: (unwind-protect (x)) signals error"
  (assert-signals error (lower '(unwind-protect (x)))))

(deftest lower-error-handler-no-clause
  "lower-sexp-to-ast: (handler-case (x)) signals error"
  (assert-signals error (lower '(handler-case (x)))))

(deftest lower-error-the-wrong-arity
  "lower-sexp-to-ast: (the fixnum) signals error"
  (assert-signals error (lower '(the fixnum))))

(deftest lower-error-defclass-no-slots
  "lower-sexp-to-ast: (defclass c ()) signals error"
  (assert-signals error (lower '(defclass c ()))))

(deftest lower-error-defgeneric-no-ll
  "lower-sexp-to-ast: (defgeneric g) signals error"
  (assert-signals error (lower '(defgeneric g))))

(deftest lower-error-defmethod-no-body
  "lower-sexp-to-ast: (defmethod m ()) signals error"
  (assert-signals error (lower '(defmethod m ()))))

(deftest lower-error-defmacro-no-body
  "lower-sexp-to-ast: (defmacro m ()) signals error"
  (assert-signals error (lower '(defmacro m ()))))

;;; ─── NEW: ast-to-sexp roundtrip additional forms ──────────────────────────

(deftest ast-roundtrip-return-from
  "ast-to-sexp roundtrip: return-from preserves head"
  (let ((result (ast-roundtrip '(return-from blk 42))))
    (assert-eq 'return-from (first result))))

(deftest ast-roundtrip-go
  "ast-to-sexp roundtrip: go preserves head"
  (let ((result (ast-roundtrip '(go my-tag))))
    (assert-eq 'go (first result))))

(deftest ast-roundtrip-catch
  "ast-to-sexp roundtrip: catch preserves head"
  (let ((result (ast-roundtrip '(catch 'tag 1 2))))
    (assert-eq 'catch (first result))))

(deftest ast-roundtrip-throw
  "ast-to-sexp roundtrip: throw preserves head"
  (let ((result (ast-roundtrip '(throw 'tag 99))))
    (assert-eq 'throw (first result))))

(deftest ast-roundtrip-the
  "ast-to-sexp roundtrip: the preserves head"
  (let ((result (ast-roundtrip '(the fixnum x))))
    (assert-eq 'the (first result))))

(deftest ast-roundtrip-values
  "ast-to-sexp roundtrip: values preserves head"
  (let ((result (ast-roundtrip '(values 1 2 3))))
    (assert-eq 'values (first result))))

(deftest ast-roundtrip-print
  "ast-to-sexp roundtrip: print preserves head"
  (let ((result (ast-roundtrip '(print 42))))
    (assert-eq 'print (first result))))

(deftest ast-roundtrip-flet
  "ast-to-sexp roundtrip: flet preserves structure"
  (let ((result (ast-roundtrip '(flet ((f (x) x)) (f 1)))))
    (assert-eq 'flet (first result))
    (assert-= 1 (length (second result)))))

(deftest ast-roundtrip-labels
  "ast-to-sexp roundtrip: labels preserves structure"
  (let ((result (ast-roundtrip '(labels ((f (n) (if (= n 0) 1 (f (- n 1))))) (f 5)))))
    (assert-eq 'labels (first result))
    (assert-= 1 (length (second result)))))

(deftest ast-roundtrip-handler-case
  "ast-to-sexp roundtrip: handler-case preserves clauses"
  (let ((result (ast-roundtrip '(handler-case (risky) (error (e) (print e))))))
    (assert-eq 'handler-case (first result))
    ;; form + at least one clause
    (assert-true (>= (length result) 3))))

(deftest ast-roundtrip-unwind-protect
  "ast-to-sexp roundtrip: unwind-protect preserves structure"
  (let ((result (ast-roundtrip '(unwind-protect (risky) (cleanup1) (cleanup2)))))
    (assert-eq 'unwind-protect (first result))
    ;; protected + 2 cleanup forms
    (assert-= 4 (length result))))

(deftest ast-roundtrip-defvar-no-value
  "ast-to-sexp roundtrip: defvar without value"
  (let ((result (ast-roundtrip '(defvar *x*))))
    (assert-equal '(defvar *x*) result)))

(deftest ast-roundtrip-defclass
  "ast-to-sexp roundtrip: defclass preserves name and supers"
  (let ((result (ast-roundtrip '(defclass point (shape) (x y)))))
    (assert-eq 'defclass (first result))
    (assert-eq 'point (second result))
    (assert-equal '(shape) (third result))))

(deftest ast-roundtrip-defmethod
  "ast-to-sexp roundtrip: defmethod preserves specializers"
  (let ((result (ast-roundtrip '(defmethod area ((s circle)) (* 3 (slot-value s 'r))))))
    (assert-eq 'defmethod (first result))
    (assert-eq 'area (second result))))

(deftest ast-roundtrip-make-instance
  "ast-to-sexp roundtrip: make-instance preserves initargs"
  (let ((result (ast-roundtrip '(make-instance 'point :x 1 :y 2))))
    (assert-eq 'make-instance (first result))))

;;; ─── NEW: parse-slot-spec ──────────────────────────────────────────────────

(deftest parse-slot-spec-simple
  "parse-slot-spec: bare symbol -> ast-slot-def with name only"
  (let ((slot (cl-cc::parse-slot-spec 'x)))
    (assert-eq 'x (cl-cc::ast-slot-name slot))
    (assert-null (cl-cc::ast-slot-initarg slot))
    (assert-null (cl-cc::ast-slot-reader slot))))

(deftest parse-slot-spec-full
  "parse-slot-spec: full slot spec with all options"
  (let ((slot (cl-cc::parse-slot-spec '(x :initarg :x :reader get-x :writer set-x :accessor x-acc :type integer))))
    (assert-eq 'x (cl-cc::ast-slot-name slot))
    (assert-eq :x (cl-cc::ast-slot-initarg slot))
    (assert-eq 'get-x (cl-cc::ast-slot-reader slot))
    (assert-eq 'set-x (cl-cc::ast-slot-writer slot))
    (assert-eq 'x-acc (cl-cc::ast-slot-accessor slot))
    (assert-eq 'integer (cl-cc::ast-slot-type slot))))

(deftest parse-slot-spec-initform
  "parse-slot-spec: slot with :initform produces AST initform"
  (let ((slot (cl-cc::parse-slot-spec '(count :initform 0))))
    (assert-eq 'count (cl-cc::ast-slot-name slot))
    (assert-true (cl-cc::ast-int-p (cl-cc::ast-slot-initform slot)))
    (assert-= 0 (cl-cc::ast-int-value (cl-cc::ast-slot-initform slot)))))

;;; ─── NEW: parse-compiler-lambda-list edge cases ────────────────────────────

(deftest parser-lambda-list-rest-then-key
  "parse-compiler-lambda-list: &rest followed by &key"
  (multiple-value-bind (required optional rest-param key-params)
      (cl-cc::parse-compiler-lambda-list '(x &rest args &key verbose))
    (assert-equal '(x) required)
    (assert-null optional)
    (assert-eq 'args rest-param)
    (assert-= 1 (length key-params))
    (assert-eq 'verbose (first (first key-params)))))

(deftest parser-lambda-list-allow-other-keys
  "parse-compiler-lambda-list: &allow-other-keys is accepted"
  (multiple-value-bind (required optional rest-param key-params)
      (cl-cc::parse-compiler-lambda-list '(&key x &allow-other-keys))
    (assert-null required)
    (assert-null optional)
    (assert-null rest-param)
    (assert-= 1 (length key-params))))

(deftest parser-lambda-list-body
  "parse-compiler-lambda-list: &body treated as &rest"
  (multiple-value-bind (required optional rest-param key-params)
      (cl-cc::parse-compiler-lambda-list '(x &body forms))
    (assert-equal '(x) required)
    (assert-eq 'forms rest-param)
    (assert-null key-params)))

(deftest parser-lambda-list-optional-bare
  "parse-compiler-lambda-list: bare &optional symbol gets nil default"
  (multiple-value-bind (required optional rest-param key-params)
      (cl-cc::parse-compiler-lambda-list '(&optional x))
    (declare (ignore rest-param key-params))
    (assert-null required)
    (assert-= 1 (length optional))
    (assert-eq 'x (first (first optional)))
    (assert-null (second (first optional)))))

;;; ─── NEW: full parse-then-lower pipeline ───────────────────────────────────

(deftest-each parse-lower-pipeline
  "parse-source then lower-sexp-to-ast: end-to-end for various forms"
  :cases (("integer"     "42"                    #'cl-cc::ast-int-p)
          ("string"      "\"hi\""                #'cl-cc::ast-quote-p)
          ("nil"         "nil"                    #'cl-cc::ast-quote-p)
          ("t"           "t"                      #'cl-cc::ast-quote-p)
          ("symbol"      "foo"                    #'cl-cc::ast-var-p)
          ("if"          "(if x 1 2)"             #'cl-cc::ast-if-p)
          ("let"         "(let ((x 1)) x)"        #'cl-cc::ast-let-p)
          ("lambda"      "(lambda (x) x)"         #'cl-cc::ast-lambda-p)
          ("defun"       "(defun f (x) x)"        #'cl-cc::ast-defun-p)
          ("quote"       "'hello"                  #'cl-cc::ast-quote-p)
          ("progn"       "(progn 1 2)"             #'cl-cc::ast-progn-p)
          ("setq"        "(setq x 1)"              #'cl-cc::ast-setq-p)
          ("block"       "(block b 1)"             #'cl-cc::ast-block-p)
          ("call"        "(foo 1 2)"               #'cl-cc::ast-call-p))
  (source pred)
  (let* ((sexp (parse-one source))
         (node (lower sexp)))
    (assert-true (funcall pred node))))
