;;;; tests/unit/parse/cl-parser-tests.lisp — CL parser and grammar unit tests
;;;;
;;;; Tests: parse-source, parse-all-forms, parse-cl-source,
;;;; lower-sexp-to-ast, token-stream operations,
;;;; parse-compiler-lambda-list, lambda-list-has-extended-p.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun parse-one (source)
  "Parse SOURCE and return the first s-expression."
  (cl-cc:parse-source source))

(defun parse-many (source)
  "Parse SOURCE and return all s-expressions."
  (cl-cc:parse-all-forms source))

(defun lower (sexp)
  "Lower an s-expression to an AST node."
  (cl-cc/parse::lower-sexp-to-ast sexp))

;;; ─── parse-source ───────────────────────────────────────────────────────────

(deftest-each parser-parse-source-integer
  "parse-source: integer literals (positive and negative)."
  :cases (("positive" 42 "42")
          ("negative" -7 "-7"))
  (expected source)
  (assert-= expected (parse-one source)))

(deftest-each parser-parse-source-atom-cases
  "parse-source: string literals, symbols, t, nil, lists, and quote sugar."
  :cases (("string"       "\"hello\"" (lambda (r) (string= "hello" r)))
          ("symbol"       "foo"       (lambda (r) (string= "FOO" (symbol-name r))))
          ("t-literal"    "t"         (lambda (r) (eq t r)))
          ("nil-atom"     "nil"       (lambda (r) (null r)))
          ("empty-list"   "()"        (lambda (r) (null r)))
          ("simple-list"  "(+ 1 2)"   (lambda (r) (equal '(+ 1 2) r)))
          ("nested-list"  "(if (> x 0) x (- x))"
                          (lambda (r) (and (consp r) (eq 'if (first r)))))
          ("quote-sugar"  "'foo"
                          (lambda (r) (and (consp r) (eq 'quote (first r))
                                           (string= "FOO" (symbol-name (second r)))))))
  (source pred)
  (assert-true (funcall pred (parse-one source))))

;;; ─── parse-all-forms ────────────────────────────────────────────────────────

(deftest-each parser-parse-all-forms-cases
  "parse-all-forms: multiple forms, single form, empty source, atom sequence."
  :cases (("multiple"  "(defun f (x) x) (defun g (y) y)"
                        (lambda (forms) (and (= 2 (length forms))
                                             (eq 'defun (first (first forms)))
                                             (eq 'defun (first (second forms))))))
          ("single"    "(+ 1 2)"
                        (lambda (forms) (and (= 1 (length forms))
                                             (equal '(+ 1 2) (first forms)))))
          ("empty"     ""
                        (lambda (forms) (null forms)))
          ("atoms-seq" "1 2 3"
                        (lambda (forms) (and (= 3 (length forms))
                                             (= 1 (first forms)) (= 2 (second forms))
                                             (= 3 (third forms))))))
  (source pred)
  (assert-true (funcall pred (parse-many source))))

;;; ─── parse-cl-source ────────────────────────────────────────────────────────

(deftest-each grammar-parse-cl-source-cases
  "parse-cl-source: single form yields 1 CST node; empty yields nil; diagnostics is a list; count matches."
  :cases (("single-form-cst"
           (lambda ()
             (multiple-value-bind (cst-list diagnostics)
                 (cl-cc/parse::parse-cl-source "(+ 1 2)")
               (declare (ignore diagnostics))
               (assert-= 1 (length cst-list))
               (assert-true (cl-cc:cst-interior-p (first cst-list))))))
          ("empty-yields-nil"
           (lambda ()
             (multiple-value-bind (cst-list diagnostics)
                 (cl-cc/parse::parse-cl-source "")
               (declare (ignore diagnostics))
               (assert-null cst-list))))
          ("diagnostics-list"
           (lambda ()
             (multiple-value-bind (cst-list diagnostics)
                 (cl-cc/parse::parse-cl-source "(+ 1 2)")
               (declare (ignore cst-list))
               (assert-true (listp diagnostics)))))
          ("multi-form-count"
           (lambda ()
             (multiple-value-bind (cst-list diagnostics)
                 (cl-cc/parse::parse-cl-source "1 2 3")
               (declare (ignore diagnostics))
               (assert-= 3 (length cst-list))))))
  (verify)
  (funcall verify))

;;; ─── token-stream ────────────────────────────────────────────────────────────

(deftest-each grammar-token-stream-cases
  "Token stream: make-token-stream creates struct; empty stream peeks nil and is at end; nil returned at EOF."
  :cases (("struct-fields"
           (lambda ()
             (let ((ts (cl-cc/parse::make-token-stream :tokens nil :source "test")))
               (assert-true (cl-cc/parse::token-stream-p ts))
               (assert-null (cl-cc/parse::token-stream-tokens ts))
               (assert-string= "test" (cl-cc/parse::token-stream-source ts)))))
          ("empty-peek"
           (lambda ()
             (let ((ts (cl-cc/parse::make-token-stream :tokens nil :source "")))
               (assert-null (cl-cc/parse::ts-peek ts))
               (assert-true (cl-cc/parse::ts-at-end-p ts)))))
          ("eof-returns-nil"
           (lambda ()
             (let ((ts (cl-cc/parse::make-token-stream :tokens nil :source "")))
               (assert-null (cl-cc/parse::parse-cl-form ts))))))
  (verify)
  (funcall verify))

;;; ─── parse-compiler-lambda-list ─────────────────────────────────────────────

(deftest-each parser-lambda-list-parsing
  "parse-compiler-lambda-list: required-only, &optional, &rest, &key, and empty lists."
  :cases (("required-only"
           (lambda ()
             (multiple-value-bind (required optional rest-param key-params)
                 (cl-cc/parse::parse-compiler-lambda-list '(x y z))
               (assert-equal '(x y z) required)
               (assert-null optional)
               (assert-null rest-param)
               (assert-null key-params))))
          ("optional"
           (lambda ()
             (multiple-value-bind (required optional rest-param key-params)
                 (cl-cc/parse::parse-compiler-lambda-list '(x &optional (y 10)))
               (assert-equal '(x) required)
               (assert-= 1 (length optional))
               (assert-eq 'y (first (first optional)))
               (assert-= 10 (second (first optional)))
               (assert-null rest-param)
               (assert-null key-params))))
          ("rest"
           (lambda ()
             (multiple-value-bind (required optional rest-param key-params)
                 (cl-cc/parse::parse-compiler-lambda-list '(x &rest args))
               (assert-equal '(x) required)
               (assert-null optional)
               (assert-eq 'args rest-param)
               (assert-null key-params))))
          ("key"
           (lambda ()
             (multiple-value-bind (required optional rest-param key-params)
                 (cl-cc/parse::parse-compiler-lambda-list '(x &key (size 0)))
               (assert-equal '(x) required)
               (assert-null optional)
               (assert-null rest-param)
               (assert-= 1 (length key-params))
               (assert-eq 'size (first (first key-params))))))
          ("empty"
           (lambda ()
             (multiple-value-bind (required optional rest-param key-params)
                 (cl-cc/parse::parse-compiler-lambda-list '())
               (assert-null required)
               (assert-null optional)
               (assert-null rest-param)
               (assert-null key-params)))))
  (verify)
  (funcall verify))

;;; ─── lambda-list-has-extended-p ──────────────────────────────────────────────

(deftest-each parser-lambda-list-has-extended-p
  "lambda-list-has-extended-p returns correct boolean for each lambda list."
  :cases (("optional" '(x &optional y) t)
          ("rest"     '(x &rest args)  t)
          ("key"      '(x &key y)      t)
          ("simple"   '(x y z)         nil)
          ("empty"    '()              nil))
  (lambda-list expected)
  (assert-equal expected (if (cl-cc/parse::lambda-list-has-extended-p lambda-list) t nil)))

;;; ─── lower-sexp-to-ast: atoms ────────────────────────────────────────────────

(deftest-each lower-symbol-and-hole-atoms
  "lower-sexp-to-ast: symbol → ast-var with name; _ → ast-hole."
  :cases (("symbol"
           'x
           (lambda (node)
             (assert-true (cl-cc/ast::ast-var-p node))
             (assert-eq 'x (cl-cc/ast::ast-var-name node))))
          ("underscore"
           '_
           (lambda (node)
             (assert-true (cl-cc/ast::ast-hole-p node)))))
  (input verify)
  (let ((node (lower input)))
    (funcall verify node)))

(deftest-each lower-arithmetic-cases
  "lower-sexp-to-ast: n-ary arithmetic folds left; unary minus becomes (0 - x)."
  :cases (("nary-left-fold"
           '(+ 1 2 3)
           (lambda (node)
             (assert-true (cl-cc/ast::ast-binop-p node))
             (assert-eq '+ (cl-cc/ast::ast-binop-op node))
             (assert-true (cl-cc/ast::ast-binop-p (cl-cc/ast::ast-binop-lhs node)))
             (assert-= 3 (cl-cc/ast::ast-int-value (cl-cc/ast::ast-binop-rhs node)))))
          ("unary-minus"
           '(- 7)
           (lambda (node)
             (assert-true (cl-cc/ast::ast-binop-p node))
             (assert-eq '- (cl-cc/ast::ast-binop-op node))
             (assert-= 0 (cl-cc/ast::ast-int-value (cl-cc/ast::ast-binop-lhs node)))
             (assert-= 7 (cl-cc/ast::ast-int-value (cl-cc/ast::ast-binop-rhs node))))))
  (form verify)
  (funcall verify (lower form)))

(deftest-each lower-self-eval-produces-ast-quote
  "lower-sexp-to-ast: nil/t/float all produce ast-quote."
  :cases (("nil"   nil)
          ("t"     t)
          ("float" 3.14))
  (input)
  (assert-true (cl-cc/ast::ast-quote-p (lower input))))

(deftest-each lower-literal-values-in-ast-quote
  "lower-sexp-to-ast: string and character literals produce ast-quote preserving their value."
  :cases (("string"    "hello")
          ("character" #\a))
  (input)
  (let ((node (lower input)))
    (assert-true (cl-cc/ast::ast-quote-p node))
    (assert-equal input (cl-cc/ast::ast-quote-value node))))

