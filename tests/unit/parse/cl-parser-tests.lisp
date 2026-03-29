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

(deftest-each parser-parse-source-integer
  "parse-source: integer literals (positive and negative)."
  :cases (("positive" 42 "42")
          ("negative" -7 "-7"))
  (expected source)
  (assert-= expected (parse-one source)))

(deftest parser-parse-source-string
  "parse-source: string literal"
  (assert-string= "hello" (parse-one "\"hello\"")))

(deftest parser-parse-source-symbol
  "parse-source: symbol is interned and uppercased"
  (assert-equal "FOO" (symbol-name (parse-one "foo"))))

(deftest-each parser-parse-source-null-values
  "parse-source: forms that evaluate to nil/null."
  :cases (("nil"        "nil")
          ("empty-list" "()"))
  (source)
  (assert-null (parse-one source)))

(deftest parser-parse-source-t
  "parse-source: t literal"
  (assert-true (eq t (parse-one "t"))))

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

(deftest-each parser-lambda-list-has-extended-p
  "lambda-list-has-extended-p returns correct boolean for each lambda list."
  :cases (("optional" '(x &optional y) t)
          ("rest"     '(x &rest args)  t)
          ("key"      '(x &key y)      t)
          ("simple"   '(x y z)         nil)
          ("empty"    '()              nil))
  (lambda-list expected)
  (assert-equal expected (if (cl-cc::lambda-list-has-extended-p lambda-list) t nil)))

;;; ─── lower-sexp-to-ast: atoms ────────────────────────────────────────────────

(deftest lower-integer-produces-ast-int
  "lower-sexp-to-ast: integer -> ast-int with correct value"
  (let ((node (lower 42)))
    (assert-true (cl-cc::ast-int-p node))
    (assert-= 42 (cl-cc::ast-int-value node))))

(deftest lower-symbol-produces-ast-var
  "lower-sexp-to-ast: symbol -> ast-var with correct name"
  (let ((node (lower 'x)))
    (assert-true (cl-cc::ast-var-p node))
    (assert-eq 'x (cl-cc::ast-var-name node))))

(deftest-each lower-self-eval-produces-ast-quote
  "lower-sexp-to-ast: nil/t/float all produce ast-quote."
  :cases (("nil"   nil)
          ("t"     t)
          ("float" 3.14))
  (input)
  (assert-true (cl-cc::ast-quote-p (lower input))))

(deftest-each lower-literal-values-in-ast-quote
  "lower-sexp-to-ast: string and character literals produce ast-quote preserving their value."
  :cases (("string"    "hello")
          ("character" #\a))
  (input)
  (let ((node (lower input)))
    (assert-true (cl-cc::ast-quote-p node))
    (assert-equal input (cl-cc::ast-quote-value node))))

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

(deftest-each lower-function-name-forms
  "lower-sexp-to-ast: (function <name>) produces ast-function with correct name."
  :cases (("symbol-ref" '(function foo)        'foo)
          ("setf-name"  '(function (setf foo))  '(setf foo)))
  (form expected-name)
  (let ((node (lower form)))
    (assert-true (cl-cc::ast-function-p node))
    (assert-equal expected-name (cl-cc::ast-function-name node))))

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

(deftest lower-catch-form
  "lower-sexp-to-ast: catch form -> ast-catch"
  (let ((node (lower '(catch 'my-tag 1 2))))
    (assert-true (cl-cc::ast-catch-p node))
    (assert-= 2 (length (cl-cc::ast-catch-body node)))))

(deftest-each lower-type-only
  "lower-sexp-to-ast: forms whose only check is the AST node type predicate."
  :cases (("throw"       '(throw 'my-tag 42)                          #'cl-cc::ast-throw-p)
          ("apply"       '(apply #'foo '(1 2))                         #'cl-cc::ast-apply-p)
          ("funcall"     '(funcall #'foo 1 2)                          #'cl-cc::ast-call-p)
          ("tagbody"     '(tagbody start (print 1) end (print 2))      #'cl-cc::ast-tagbody-p)
          ("setf-gethash"  '(setf (gethash 'k tbl) 42)                #'cl-cc::ast-set-gethash-p)
          ("mv-prog1"    '(multiple-value-prog1 (values 1 2) (print 3)) #'cl-cc::ast-multiple-value-prog1-p))
  (form pred)
  (assert-true (funcall pred (lower form))))

(deftest lower-unwind-protect-form
  "lower-sexp-to-ast: unwind-protect form -> ast-unwind-protect"
  (let ((node (lower '(unwind-protect (risky) (cleanup)))))
    (assert-true (cl-cc::ast-unwind-protect-p node))
    (assert-= 1 (length (cl-cc::ast-unwind-cleanup node)))))

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

(deftest lower-defun-with-declare-type
  "lower-sexp-to-ast: leading (declare (type ...)) becomes typed params."
  (let ((node (lower '(defun add1 (x)
                        (declare (type fixnum x))
                        (+ x 1)))))
    (assert-true (cl-cc::ast-defun-p node))
    (assert-equal '((x fixnum)) (cl-cc::ast-defun-params node))))

(deftest lower-lambda-with-declare-type
  "lower-sexp-to-ast: lambda leading type declaration becomes typed params."
  (let ((node (lower '(lambda (x)
                        (declare (type fixnum x))
                        (+ x 1)))))
    (assert-true (cl-cc::ast-lambda-p node))
    (assert-equal '((x fixnum)) (cl-cc::ast-lambda-params node))))

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

(deftest-each lower-sexp-arity-errors
  "lower-sexp-to-ast signals error for malformed special forms."
  :cases (("if-missing-args"   '(if))
          ("if-too-many-args"  '(if a b c d))
          ("let-no-bindings"   '(let))
          ("defun-no-params"   '(defun f))
          ("setq-odd-args"     '(setq x))
          ("quote-wrong-arity" '(quote))
          ("function-no-arg"   '(function)))
  (form)
  (assert-signals error (lower form)))

;;; ─── ast-to-sexp roundtrip ──────────────────────────────────────────────────

(defun ast-roundtrip (sexp)
  "Lower sexp to AST then convert back to sexp."
  (cl-cc::ast-to-sexp (lower sexp)))

(deftest ast-roundtrip
  "ast-to-sexp roundtrip: atoms, control flow, bindings, definitions, and data."
  ;; Atoms
  (assert-= 42 (ast-roundtrip 42))
  (assert-eq 'x (ast-roundtrip 'x))
  ;; Control flow
  (let ((result (ast-roundtrip '(if x 1 2))))
    (assert-eq 'if (first result))
    (assert-eq 'x (second result))
    (assert-= 1 (third result))
    (assert-= 2 (fourth result)))
  (let ((result (ast-roundtrip '(progn 1 2 3))))
    (assert-eq 'progn (first result))
    (assert-= 3 (length (rest result))))
  (let ((result (ast-roundtrip '(block outer 1 2))))
    (assert-eq 'block (first result))
    (assert-eq 'outer (second result)))
  ;; Bindings
  (let ((result (ast-roundtrip '(let ((x 10)) x))))
    (assert-eq 'let (first result)))
  (let ((result (ast-roundtrip '(setq x 42))))
    (assert-equal '(setq x 42) result))
  ;; Functions
  (let ((result (ast-roundtrip '(lambda (x y) (+ x y)))))
    (assert-eq 'lambda (first result))
    (assert-equal '(x y) (second result)))
  (let ((result (ast-roundtrip '(defun add (a b) (+ a b)))))
    (assert-eq 'defun (first result))
    (assert-eq 'add (second result))
    (assert-equal '(a b) (third result)))
  ;; Definitions and data
  (let ((result (ast-roundtrip '(defvar *count* 0))))
    (assert-eq 'defvar (first result))
    (assert-eq '*count* (second result))
    (assert-= 0 (third result)))
  (let ((result (ast-roundtrip '(quote hello))))
    (assert-equal '(quote hello) result)))

;;; ─── sexp-head-to-kind ───────────────────────────────────────────────────────

(deftest-each grammar-sexp-head-to-kind
  "sexp-head-to-kind maps each special form head to its kind keyword."
  :cases (("defun"   'defun                   :defun)
          ("let"     'let                     :let)
          ("if"      'if                      :if)
          ("lambda"  'lambda                  :lambda)
          ("setq"    'setq                    :setq)
          ("progn"   'progn                   :progn)
          ("defclass" 'defclass               :defclass)
          ("unknown" 'completely-unknown-symbol :call))
  (sym expected)
  (assert-eq expected (cl-cc::sexp-head-to-kind sym)))

;;; ─── Grammar specialized parsers ────────────────────────────────────────────

(deftest grammar-parse-cl-form-atoms
  "parse-cl-form: scalar tokens produce cst-token with correct value."
  (let* ((tokens (cl-cc:lex-all "42"))
         (ts (cl-cc::make-token-stream :tokens tokens :source "42"))
         (form (cl-cc::parse-cl-form ts)))
    (assert-true (cl-cc:cst-token-p form))
    (assert-= 42 (cl-cc:cst-token-value form)))
  (let* ((tokens (cl-cc:lex-all "\"hello\""))
         (ts (cl-cc::make-token-stream :tokens tokens :source "\"hello\""))
         (form (cl-cc::parse-cl-form ts)))
    (assert-true (cl-cc:cst-token-p form))
    (assert-string= "hello" (cl-cc:cst-token-value form))))

(deftest grammar-parse-cl-form-lists
  "parse-cl-form: list -> cst-interior; empty list -> nil children."
  (let* ((tokens (cl-cc:lex-all "(1 2 3)"))
         (ts (cl-cc::make-token-stream :tokens tokens :source "(1 2 3)"))
         (form (cl-cc::parse-cl-form ts)))
    (assert-true (cl-cc:cst-interior-p form))
    (assert-= 3 (length (cl-cc:cst-children form))))
  (let* ((tokens (cl-cc:lex-all "()"))
         (ts (cl-cc::make-token-stream :tokens tokens :source "()"))
         (form (cl-cc::parse-cl-form ts)))
    (assert-true (cl-cc:cst-interior-p form))
    (assert-null (cl-cc:cst-children form))))

(deftest-each grammar-parse-cl-form-reader-macros
  "parse-cl-form: reader-macro sugar -> cst-interior node with correct kind."
  :cases (("quote"    "'foo"  :quote)
          ("backquote" "`foo" :quasiquote)
          ("function" "#'foo" :function))
  (source expected-kind)
  (let* ((tokens (cl-cc:lex-all source))
         (ts (cl-cc::make-token-stream :tokens tokens :source source))
         (form (cl-cc::parse-cl-form ts)))
    (assert-true (cl-cc:cst-interior-p form))
    (assert-eq expected-kind (cl-cc:cst-node-kind form))))

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

(deftest-each lower-defclass-form
  "lower-sexp-to-ast: defclass -> ast-defclass with correct name, superclasses, and slot count."
  :cases (("no-superclass"   '(defclass point () (x y))              'point '()      2)
          ("with-superclass" '(defclass colored-point (point) (color)) 'colored-point '(point) 1))
  (form expected-name expected-supers expected-slots)
  (let ((node (lower form)))
    (assert-true (cl-cc::ast-defclass-p node))
    (assert-eq expected-name (cl-cc::ast-defclass-name node))
    (assert-equal expected-supers (cl-cc::ast-defclass-superclasses node))
    (assert-= expected-slots (length (cl-cc::ast-defclass-slots node)))))

;;; ─── defgeneric / defmethod lowering ────────────────────────────────────────

(deftest-each lower-generic-dispatch-forms
  "lower-sexp-to-ast: defgeneric and defmethod produce correct AST nodes."
  :cases (("defgeneric" '(defgeneric area (shape))
           #'cl-cc::ast-defgeneric-p #'cl-cc::ast-defgeneric-name #'cl-cc::ast-defgeneric-params
           'area '(shape))
          ("defmethod"  '(defmethod area ((s circle)) (* pi (expt (slot-value s 'radius) 2)))
           #'cl-cc::ast-defmethod-p #'cl-cc::ast-defmethod-name #'cl-cc::ast-defmethod-params
           'area '(s)))
  (form pred-p get-name get-params expected-name expected-params)
  (let ((node (lower form)))
    (assert-true (funcall pred-p node))
    (assert-eq expected-name (funcall get-name node))
    (assert-equal expected-params (funcall get-params node))))

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

(deftest-each parser-parse-source-vector
  "parse-source: vector literals parse to vectors with the expected length."
  :cases (("empty" "#()"      0)
          ("three" "#(1 2 3)" 3))
  (source expected-len)
  (let ((result (parse-one source)))
    (assert-true (vectorp result))
    (assert-= expected-len (length result))))

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

(deftest-each ast-location-string-formats
  "ast-location-string formats file:line:col when present; falls back to <unknown location>."
  :cases (("with-location"    (cl-cc::make-ast-int :value 1
                                :source-file "foo.lisp" :source-line 3 :source-column 7)
           "foo.lisp:3:7")
          ("unknown-location" (cl-cc::make-ast-int :value 1)
           "<unknown location>"))
  (node expected)
  (assert-string= expected (cl-cc::ast-location-string node)))

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

(deftest-each lower-extended-lambda-list-params
  "lower-sexp-to-ast: &optional, &rest, &key all populate the correct extended-params slot."
  :cases (("optional" '(lambda (x &optional (y 0)) (+ x y))
           #'cl-cc::ast-lambda-p #'cl-cc::ast-lambda-optional-params 1)
          ("rest"     '(lambda (x &rest args) args)
           #'cl-cc::ast-lambda-p #'cl-cc::ast-lambda-rest-param :rest)
          ("key"      '(defun f (x &key (size 10)) x)
           #'cl-cc::ast-defun-p #'cl-cc::ast-defun-key-params 1))
  (form pred-p get-slot expected)
  (let ((node (lower form)))
    (assert-true (funcall pred-p node))
    (let ((slot-val (funcall get-slot node)))
      (if (eq expected :rest)
          (assert-eq 'args slot-val)
          (assert-= expected (length slot-val))))))

(deftest lower-defparameter-form
  "lower-sexp-to-ast: defparameter -> ast-defvar (same as defvar)"
  (let ((node (lower '(defparameter *x* 99))))
    (assert-true (cl-cc::ast-defvar-p node))
    (assert-eq '*x* (cl-cc::ast-defvar-name node))))

(deftest lower-return-from-no-value
  "lower-sexp-to-ast: (return-from blk) with no value signals error (not yet supported)"
  (assert-signals error (lower '(return-from blk))))

(deftest lower-multiple-value-call-form
  "lower-sexp-to-ast: multiple-value-call -> ast-multiple-value-call"
  (let ((node (lower '(multiple-value-call #'list (values 1 2) (values 3 4)))))
    (assert-true (cl-cc::ast-multiple-value-call-p node))
    (assert-= 2 (length (cl-cc::ast-mv-call-args node)))))

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

(deftest-each lower-sexp-body-errors
  "lower-sexp-to-ast signals error for forms missing required body or cleanup."
  :cases (("progn-no-body"     '(progn))
          ("let-no-body"       '(let ()))
          ("lambda-no-body"    '(lambda ()))
          ("defun-no-body"     '(defun f ()))
          ("block-no-body"     '(block b))
          ("binop-wrong-arity" '(+ 1))
          ("go-no-tag"         '(go))
          ("catch-no-body"     '(catch 'tag))
          ("throw-wrong-arity" '(throw 'tag))
          ("unwind-no-cleanup" '(unwind-protect (x)))
          ("handler-no-clause" '(handler-case (x)))
          ("the-wrong-arity"   '(the fixnum))
          ("defclass-no-slots" '(defclass c ()))
          ("defgeneric-no-ll"  '(defgeneric g))
          ("defmethod-no-body" '(defmethod m ()))
          ("defmacro-no-body"  '(defmacro m ())))
  (form)
  (assert-signals error (lower form)))

;;; ─── NEW: ast-to-sexp roundtrip additional forms ──────────────────────────

(deftest-each ast-roundtrip-head-preserved
  "ast-to-sexp roundtrip: each form round-trips with its head symbol intact."
  :cases (("return-from"   '(return-from blk 42)          'return-from)
          ("go"            '(go my-tag)                   'go)
          ("catch"         '(catch 'tag 1 2)              'catch)
          ("throw"         '(throw 'tag 99)               'throw)
          ("the"           '(the fixnum x)                'the)
          ("values"        '(values 1 2 3)                'values)
          ("print"         '(print 42)                    'print)
          ("make-instance" '(make-instance 'point :x 1 :y 2) 'make-instance))
  (form expected-head)
  (assert-eq expected-head (first (ast-roundtrip form))))

(deftest-each ast-roundtrip-local-fn-forms
  "ast-to-sexp roundtrip: flet/labels preserve structure with one binding."
  :cases (("flet"   'flet   '(flet ((f (x) x)) (f 1)))
          ("labels" 'labels '(labels ((f (n) (if (= n 0) 1 (f (- n 1))))) (f 5))))
  (expected-head form)
  (let ((result (ast-roundtrip form)))
    (assert-eq expected-head (first result))
    (assert-= 1 (length (second result)))))

(deftest-each ast-roundtrip-condition-control
  "ast-to-sexp roundtrip: handler-case and unwind-protect preserve head and structure length."
  :cases (("handler-case"   '(handler-case (risky) (error (e) (print e)))       'handler-case   3)
          ("unwind-protect" '(unwind-protect (risky) (cleanup1) (cleanup2))     'unwind-protect 4))
  (form expected-head expected-min-len)
  (let ((result (ast-roundtrip form)))
    (assert-eq expected-head (first result))
    (assert-true (>= (length result) expected-min-len))))

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
