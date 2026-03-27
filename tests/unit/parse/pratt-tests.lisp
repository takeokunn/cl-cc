;;;; tests/unit/parse/pratt-tests.lisp — Pratt parser engine unit tests
;;;;
;;;; Tests: pratt-context stream ops, NUD dispatch, CL grammar round-trips,
;;;; parse-cl-source CST structure, parse-all-forms s-expression output,
;;;; error recovery via cst-error-node, and parse-and-lower pipeline.

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; ─── Helpers ─────────────────────────────────────────────────────────────────

(defun make-test-ctx (source)
  "Create a CL pratt-context from SOURCE string."
  (let ((tokens (cl-cc:lex-all source)))
    (cl-cc::make-cl-pratt-context tokens source nil)))

(defun parse-one-cst (source)
  "Parse SOURCE and return the first CST node."
  (first (nth-value 0 (cl-cc:parse-cl-source source))))

(defun parse-one-sexp (source)
  "Parse SOURCE and return the first s-expression."
  (first (cl-cc:parse-all-forms source)))

;;; ─── Stream Operations ───────────────────────────────────────────────────────

(deftest pratt-peek-behavior
  "pratt-peek returns the current token without advancing, and is idempotent"
  (let ((ctx (make-test-ctx "42")))
    (let ((tok (cl-cc::pratt-peek ctx)))
      (assert-true (not (null tok)))
      (assert-eq :T-INT (cl-cc:lexer-token-type tok))))
  (let ((ctx (make-test-ctx "42")))
    (let ((tok1 (cl-cc::pratt-peek ctx))
          (tok2 (cl-cc::pratt-peek ctx)))
      (assert-eq tok1 tok2))))

(deftest pratt-advance-consumes-token
  "pratt-advance returns current token and moves forward"
  (let ((ctx (make-test-ctx "1 2")))
    (let ((tok1 (cl-cc::pratt-advance ctx)))
      (assert-eq :T-INT (cl-cc:lexer-token-type tok1))
      (assert-= 1 (cl-cc:lexer-token-value tok1)))
    (let ((tok2 (cl-cc::pratt-advance ctx)))
      (assert-eq :T-INT (cl-cc:lexer-token-type tok2))
      (assert-= 2 (cl-cc:lexer-token-value tok2)))))

(deftest-each pratt-at-end-p-states
  "pratt-at-end-p returns the correct value in each parser state"
  :cases (("empty-source"  "" t   nil)
          ("non-empty"     "42" nil nil)
          ("after-advance" "x" t   t))
  (source expected advance-first)
  (let ((ctx (make-test-ctx source)))
    (when advance-first
      (cl-cc::pratt-advance ctx))
    (if expected
        (assert-true (cl-cc::pratt-at-end-p ctx))
        (assert-false (cl-cc::pratt-at-end-p ctx)))))

;;; ─── Token Accessors ─────────────────────────────────────────────────────────

(deftest pratt-tok-type-via-context
  "pratt-tok-type returns type using context's accessor function"
  (let* ((ctx (make-test-ctx "hello"))
         (tok (cl-cc::pratt-peek ctx)))
    (assert-eq :T-IDENT (cl-cc::pratt-tok-type ctx tok))))

(deftest pratt-tok-value-via-context
  "pratt-tok-value returns value using context's accessor function"
  (let* ((ctx (make-test-ctx "hello"))
         (tok (cl-cc::pratt-peek ctx)))
    (assert-string= "HELLO" (string (cl-cc::pratt-tok-value ctx tok)))))

(deftest pratt-tok-type-nil-safe
  "pratt-tok-type returns nil when tok is nil"
  (let ((ctx (make-test-ctx "")))
    (assert-eq nil (cl-cc::pratt-tok-type ctx nil))))

;;; ─── Diagnostics: pratt-expect ───────────────────────────────────────────────

(deftest pratt-expect-matching-type
  "pratt-expect consumes token when type matches"
  (let ((ctx (make-test-ctx "42")))
    (let ((tok (cl-cc::pratt-expect ctx :T-INT "test")))
      (assert-true (not (null tok)))
      (assert-eq :T-INT (cl-cc:lexer-token-type tok)))))

(deftest pratt-expect-mismatch-adds-diagnostic
  "pratt-expect pushes diagnostic on type mismatch"
  (let ((ctx (make-test-ctx "42")))
    (cl-cc::pratt-expect ctx :T-STRING "test")
    (assert-true (not (null (cl-cc::pratt-context-diagnostics ctx))))))

(deftest pratt-expect-mismatch-returns-nil
  "pratt-expect returns nil on type mismatch"
  (let ((ctx (make-test-ctx "42")))
    (let ((result (cl-cc::pratt-expect ctx :T-STRING)))
      (assert-eq nil result))))

(deftest pratt-expect-eof-adds-diagnostic
  "pratt-expect on empty stream pushes end-of-input diagnostic"
  (let ((ctx (make-test-ctx "")))
    (cl-cc::pratt-expect ctx :T-INT "eof-test")
    (assert-true (not (null (cl-cc::pratt-context-diagnostics ctx))))))

;;; ─── NUD Table Registration ──────────────────────────────────────────────────

(deftest cl-nud-table-not-nil
  "CL NUD table is populated"
  (assert-true (not (null cl-cc::*cl-nud-table*)))
  (assert-true (> (hash-table-count cl-cc::*cl-nud-table*) 0)))

(deftest cl-led-table-empty
  "CL LED table is empty (CL has no infix operators)"
  (assert-= 0 (hash-table-count cl-cc::*cl-led-table*)))

(deftest-each cl-nud-table-has-handler
  "NUD table has handler for each required token type"
  :cases (("int"    :T-INT)
          ("lparen" :T-LPAREN)
          ("quote"  :T-QUOTE))
  (token-type)
  (assert-true (not (null (gethash token-type cl-cc::*cl-nud-table*)))))

;;; ─── parse-cl-source: CST Structure ─────────────────────────────────────────

(deftest parse-integer-produces-cst-token
  "Parsing an integer produces a cst-token"
  (let ((node (parse-one-cst "42")))
    (assert-true (cl-cc:cst-token-p node))
    (assert-eq :T-INT (cl-cc:cst-node-kind node))
    (assert-= 42 (cl-cc:cst-token-value node))))

(deftest-each parse-literal-kind
  "Parsing each literal type produces a cst-token with the correct kind"
  :cases (("float"   "3.14" :T-FLOAT)
          ("symbol"  "foo"  :T-IDENT)
          ("keyword" ":foo" :T-KEYWORD))
  (source expected-kind)
  (let ((node (parse-one-cst source)))
    (assert-true (cl-cc:cst-token-p node))
    (assert-eq expected-kind (cl-cc:cst-node-kind node))))

(deftest parse-string-produces-cst-token
  "Parsing a string produces a cst-token with the correct value"
  (let ((node (parse-one-cst "\"hello\"")))
    (assert-true (cl-cc:cst-token-p node))
    (assert-eq :T-STRING (cl-cc:cst-node-kind node))
    (assert-string= "hello" (cl-cc:cst-token-value node))))

(deftest parse-empty-list-produces-interior
  "Parsing () produces a cst-interior with :list kind"
  (let ((node (parse-one-cst "()")))
    (assert-true (cl-cc:cst-interior-p node))
    (assert-eq :list (cl-cc:cst-node-kind node))
    (assert-= 0 (length (cl-cc:cst-children node)))))

(deftest parse-simple-list-produces-children
  "Parsing (a b c) produces a 3-child cst-interior"
  (let ((node (parse-one-cst "(a b c)")))
    (assert-true (cl-cc:cst-interior-p node))
    (assert-= 3 (length (cl-cc:cst-children node)))))

(deftest parse-nested-list
  "Parsing (a (b c)) produces nested cst-interiors"
  (let ((node (parse-one-cst "(a (b c))")))
    (assert-true (cl-cc:cst-interior-p node))
    (assert-= 2 (length (cl-cc:cst-children node)))
    (let ((inner (second (cl-cc:cst-children node))))
      (assert-true (cl-cc:cst-interior-p inner))
      (assert-= 2 (length (cl-cc:cst-children inner))))))

(deftest parse-quote-produces-quote-kind
  "Parsing 'x produces a cst-interior with :quote kind"
  (let ((node (parse-one-cst "'x")))
    (assert-true (cl-cc:cst-interior-p node))
    (assert-eq :quote (cl-cc:cst-node-kind node))))

(deftest parse-quasiquote-produces-quasiquote-kind
  "Parsing `x produces a cst-interior with :quasiquote kind"
  (let ((node (parse-one-cst "`x")))
    (assert-true (cl-cc:cst-interior-p node))
    (assert-eq :quasiquote (cl-cc:cst-node-kind node))))

(deftest parse-function-dispatch
  "Parsing #'foo produces a cst-interior with :function kind"
  (let ((node (parse-one-cst "#'foo")))
    (assert-true (cl-cc:cst-interior-p node))
    (assert-eq :function (cl-cc:cst-node-kind node))))

(deftest parse-vector-dispatch
  "Parsing #(1 2 3) produces a cst-interior with :vector kind"
  (let ((node (parse-one-cst "#(1 2 3)")))
    (assert-true (cl-cc:cst-interior-p node))
    (assert-eq :vector (cl-cc:cst-node-kind node))
    (assert-= 3 (length (cl-cc:cst-children node)))))

;;; ─── CST Node Source Positions ───────────────────────────────────────────────

(deftest parse-integer-byte-positions
  "Integer CST token records correct byte positions"
  (let ((node (parse-one-cst "42")))
    (assert-= 0 (cl-cc:cst-node-start-byte node))
    (assert-= 2 (cl-cc:cst-node-end-byte node))))

(deftest parse-offset-byte-positions
  "Second token in stream has correct start offset"
  (multiple-value-bind (nodes _diag)
      (cl-cc:parse-cl-source "1 99")
    (declare (ignore _diag))
    (let ((second-node (second nodes)))
      (assert-= 2 (cl-cc:cst-node-start-byte second-node)))))

;;; ─── parse-cl-source: Multiple Forms ────────────────────────────────────────

(deftest parse-cl-source-multiple-forms
  "parse-cl-source returns all top-level forms"
  (multiple-value-bind (nodes diag)
      (cl-cc:parse-cl-source "1 2 3")
    (declare (ignore diag))
    (assert-= 3 (length nodes))))

(deftest parse-cl-source-returns-diagnostics
  "parse-cl-source returns second value as diagnostics list"
  (multiple-value-bind (nodes diag)
      (cl-cc:parse-cl-source "42")
    (declare (ignore nodes))
    (assert-true (listp diag))))

(deftest parse-cl-source-empty-string
  "parse-cl-source on empty string returns empty list"
  (multiple-value-bind (nodes _)
      (cl-cc:parse-cl-source "")
    (assert-= 0 (length nodes))))

;;; ─── parse-all-forms: S-expression Output ───────────────────────────────────

(deftest-each parse-all-forms-literal-types
  "parse-all-forms returns the correct value for each literal type"
  :cases (("integer"  "42"       42)
          ("string"   "\"hello\"" "hello")
          ("nil-list" "()"       nil))
  (source expected)
  (assert-equal expected (parse-one-sexp source)))

(deftest parse-all-forms-literal-types-symbol
  "parse-all-forms returns a symbol named FOO for the source \"foo\""
  (assert-string= "FOO" (symbol-name (parse-one-sexp "foo"))))

(deftest parse-all-forms-simple-call
  "parse-all-forms returns proper list for (f x)"
  (let ((form (parse-one-sexp "(+ 1 2)")))
    (assert-eq '+ (car form))
    (assert-= 1 (cadr form))
    (assert-= 2 (caddr form))))

(deftest parse-all-forms-quoted-symbol
  "parse-all-forms returns (quote x) for 'x"
  (let ((form (parse-one-sexp "'foo")))
    (assert-eq 'quote (car form))
    (assert-string= "FOO" (symbol-name (cadr form)))))

(deftest parse-all-forms-multiple
  "parse-all-forms returns all top-level forms"
  (let ((forms (cl-cc:parse-all-forms "(defun f (x) x) (f 1)")))
    (assert-= 2 (length forms))
    (assert-eq 'defun (caar forms))
    (assert-string= "F" (symbol-name (cadar forms)))))

(deftest parse-all-forms-nested
  "parse-all-forms handles deeply nested forms"
  (let ((form (parse-one-sexp "(let ((x (+ 1 2))) (* x x))")))
    (assert-eq 'let (car form))
    (assert-string= "X" (symbol-name (caaadr form)))))

;;; ─── parse-source: Single Form ───────────────────────────────────────────────

(deftest parse-source-returns-one-form
  "parse-source returns the first s-expression"
  (assert-= 99 (cl-cc:parse-source "99")))

(deftest parse-source-errors-on-empty
  "parse-source signals an error on empty source"
  (assert-signals error (cl-cc:parse-source "")))

;;; ─── Error Recovery via cst-error-node ──────────────────────────────────────

(deftest parse-unmatched-paren-adds-diagnostic
  "Unmatched open paren produces a diagnostic"
  (multiple-value-bind (nodes diag)
      (cl-cc:parse-cl-source "(+ 1")
    (declare (ignore nodes))
    (assert-true (not (null diag)))))

;;; ─── Special Forms: sexp-head-to-kind Dispatch ───────────────────────────────

(deftest-each parse-special-form-head-kind
  "Parser assigns correct CST kind to special form heads"
  :cases (("defun"   "(defun f (x) x)"  :defun)
          ("let"     "(let ((x 1)) x)"  :let)
          ("if"      "(if t 1 2)"       :if)
          ("lambda"  "(lambda (x) x)"   :lambda)
          ("unknown" "(my-fn a b)"      :call))
  (source expected-kind)
  (let ((node (parse-one-cst source)))
    (assert-eq expected-kind (cl-cc:cst-node-kind node))))

;;; ─── parse-and-lower: Full Pipeline ─────────────────────────────────────────

(deftest parse-and-lower-returns-ast-list
  "parse-and-lower returns a list of AST nodes"
  (let ((asts (cl-cc:parse-and-lower "(+ 1 2)")))
    (assert-true (not (null asts)))
    (assert-true (listp asts))))

(deftest parse-and-lower-integer-ast
  "parse-and-lower on a number returns an ast-int node"
  (let ((asts (cl-cc:parse-and-lower "42")))
    (let ((node (first asts)))
      (assert-true (cl-cc::ast-int-p node)))))

(deftest parse-and-lower-multiple-forms
  "parse-and-lower on multiple forms returns list of equal length"
  (let ((asts (cl-cc:parse-and-lower "1 2 3")))
    (assert-= 3 (length asts))))

;;; ─── pratt-parse-expr: Direct Tests ──────────────────────────────────────────

(deftest pratt-parse-expr-integer
  "pratt-parse-expr returns cst-token for integer."
  (let ((ctx (make-test-ctx "42")))
    (let ((node (cl-cc::pratt-parse-expr ctx)))
      (assert-true (cl-cc:cst-token-p node))
      (assert-equal 42 (cl-cc:cst-token-value node)))))

(deftest pratt-parse-expr-symbol
  "pratt-parse-expr returns cst-token for symbol."
  (let ((ctx (make-test-ctx "foo")))
    (let ((node (cl-cc::pratt-parse-expr ctx)))
      (assert-true (cl-cc:cst-token-p node)))))

(deftest pratt-parse-expr-list
  "pratt-parse-expr returns cst-interior for list."
  (let ((ctx (make-test-ctx "(+ 1 2)")))
    (let ((node (cl-cc::pratt-parse-expr ctx)))
      (assert-true (cl-cc:cst-interior-p node)))))

(deftest pratt-parse-expr-error-on-eof
  "pratt-parse-expr on empty stream returns error node."
  (let ((ctx (make-test-ctx "")))
    (let ((node (cl-cc::pratt-parse-expr ctx)))
      (assert-true (cl-cc::cst-error-node-p node)))))

(deftest pratt-parse-expr-quoted
  "pratt-parse-expr handles quote syntax."
  (let ((ctx (make-test-ctx "'x")))
    (let ((node (cl-cc::pratt-parse-expr ctx)))
      (assert-true (cl-cc:cst-interior-p node))
      (assert-eq :quote (cl-cc:cst-node-kind node)))))

(deftest pratt-parse-expr-nested-list
  "pratt-parse-expr handles nested lists returning cst-interior."
  (let ((ctx (make-test-ctx "(let ((x 1)) x)")))
    (let ((node (cl-cc::pratt-parse-expr ctx)))
      (assert-true (cl-cc:cst-interior-p node)))))

;;; ─── pratt-add-diagnostic: Direct Tests ─────────────────────────────────────

(deftest pratt-add-diagnostic-pushes
  "pratt-add-diagnostic pushes a diagnostic to the context."
  (let ((ctx (make-test-ctx "42")))
    (cl-cc::pratt-add-diagnostic ctx "test error" (cons 0 2))
    (assert-equal 1 (length (cl-cc::pratt-context-diagnostics ctx)))))

(deftest pratt-add-diagnostic-accumulates
  "pratt-add-diagnostic accumulates multiple diagnostics."
  (let ((ctx (make-test-ctx "42")))
    (cl-cc::pratt-add-diagnostic ctx "error 1" (cons 0 1))
    (cl-cc::pratt-add-diagnostic ctx "error 2" (cons 1 2))
    (assert-equal 2 (length (cl-cc::pratt-context-diagnostics ctx)))))

(deftest pratt-add-diagnostic-records-message
  "pratt-add-diagnostic stores the error message."
  (let ((ctx (make-test-ctx "42")))
    (cl-cc::pratt-add-diagnostic ctx "unexpected token" (cons 0 2))
    (let ((diag (first (cl-cc::pratt-context-diagnostics ctx))))
      (assert-true (not (null diag))))))

;;; ─── pratt-parse-list-until: Direct Tests ───────────────────────────────────

(deftest pratt-parse-list-until-empty
  "pratt-parse-list-until on () returns empty list."
  (let ((ctx (make-test-ctx "()")))
    ;; Advance past the LPAREN
    (cl-cc::pratt-advance ctx)
    (let ((items (cl-cc::pratt-parse-list-until ctx :T-RPAREN
                   (lambda (c) (cl-cc::pratt-parse-expr c)))))
      (assert-equal 0 (length items)))))

(deftest pratt-parse-list-until-elements
  "pratt-parse-list-until collects parsed elements."
  (let ((ctx (make-test-ctx "(1 2 3)")))
    ;; Advance past the LPAREN
    (cl-cc::pratt-advance ctx)
    (let ((items (cl-cc::pratt-parse-list-until ctx :T-RPAREN
                   (lambda (c) (cl-cc::pratt-parse-expr c)))))
      (assert-equal 3 (length items)))))

(deftest pratt-parse-list-until-consumes-terminator
  "pratt-parse-list-until consumes the end token."
  (let ((ctx (make-test-ctx "(1) 42")))
    (cl-cc::pratt-advance ctx) ; consume LPAREN
    (cl-cc::pratt-parse-list-until ctx :T-RPAREN
      (lambda (c) (cl-cc::pratt-parse-expr c)))
    ;; Next token should be 42, not RPAREN
    (let ((tok (cl-cc::pratt-peek ctx)))
      (assert-eq :T-INT (cl-cc:lexer-token-type tok))
      (assert-equal 42 (cl-cc:lexer-token-value tok)))))
