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

(deftest pratt-peek-returns-first-token
  "pratt-peek returns the current token without advancing"
  (let ((ctx (make-test-ctx "42")))
    (let ((tok (cl-cc::pratt-peek ctx)))
      (assert-true (not (null tok)))
      (assert-eq :T-INT (cl-cc:lexer-token-type tok)))))

(deftest pratt-peek-does-not-consume
  "pratt-peek called twice returns the same token"
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

(deftest pratt-at-end-p-empty-source
  "pratt-at-end-p returns true when only EOF token remains"
  (let ((ctx (make-test-ctx "")))
    (assert-true (cl-cc::pratt-at-end-p ctx))))

(deftest pratt-at-end-p-non-empty
  "pratt-at-end-p returns false when tokens remain"
  (let ((ctx (make-test-ctx "42")))
    (assert-false (cl-cc::pratt-at-end-p ctx))))

(deftest pratt-at-end-p-after-advance
  "pratt-at-end-p returns true after consuming all tokens"
  (let ((ctx (make-test-ctx "x")))
    (cl-cc::pratt-advance ctx) ; consume :T-IDENT
    (assert-true (cl-cc::pratt-at-end-p ctx))))

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
    (assert-string= "hello" (string (cl-cc::pratt-tok-value ctx tok)))))

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

(deftest cl-nud-table-has-int-handler
  "NUD table has handler for :T-INT"
  (assert-true (not (null (gethash :T-INT cl-cc::*cl-nud-table*)))))

(deftest cl-nud-table-has-lparen-handler
  "NUD table has handler for :T-LPAREN"
  (assert-true (not (null (gethash :T-LPAREN cl-cc::*cl-nud-table*)))))

(deftest cl-nud-table-has-quote-handler
  "NUD table has handler for :T-QUOTE"
  (assert-true (not (null (gethash :T-QUOTE cl-cc::*cl-nud-table*)))))

;;; ─── parse-cl-source: CST Structure ─────────────────────────────────────────

(deftest parse-integer-produces-cst-token
  "Parsing an integer produces a cst-token"
  (let ((node (parse-one-cst "42")))
    (assert-true (cl-cc:cst-token-p node))
    (assert-eq :T-INT (cl-cc:cst-node-kind node))
    (assert-= 42 (cl-cc:cst-token-value node))))

(deftest parse-float-produces-cst-token
  "Parsing a float produces a cst-token"
  (let ((node (parse-one-cst "3.14")))
    (assert-true (cl-cc:cst-token-p node))
    (assert-eq :T-FLOAT (cl-cc:cst-node-kind node))))

(deftest parse-string-produces-cst-token
  "Parsing a string produces a cst-token"
  (let ((node (parse-one-cst "\"hello\"")))
    (assert-true (cl-cc:cst-token-p node))
    (assert-eq :T-STRING (cl-cc:cst-node-kind node))
    (assert-string= "hello" (cl-cc:cst-token-value node))))

(deftest parse-symbol-produces-cst-token
  "Parsing a symbol produces a :T-IDENT cst-token"
  (let ((node (parse-one-cst "foo")))
    (assert-true (cl-cc:cst-token-p node))
    (assert-eq :T-IDENT (cl-cc:cst-node-kind node))))

(deftest parse-keyword-produces-cst-token
  "Parsing a keyword produces a :T-KEYWORD cst-token"
  (let ((node (parse-one-cst ":foo")))
    (assert-true (cl-cc:cst-token-p node))
    (assert-eq :T-KEYWORD (cl-cc:cst-node-kind node))))

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

(deftest parse-all-forms-integer
  "parse-all-forms returns integer as number"
  (assert-= 42 (parse-one-sexp "42")))

(deftest parse-all-forms-symbol
  "parse-all-forms returns symbol for identifier"
  (assert-eq 'foo (parse-one-sexp "foo")))

(deftest parse-all-forms-string
  "parse-all-forms returns string for string literal"
  (assert-string= "hello" (parse-one-sexp "\"hello\"")))

(deftest parse-all-forms-nil-list
  "parse-all-forms returns nil for empty list"
  (assert-eq nil (parse-one-sexp "()")))

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
    (assert-eq 'foo (cadr form))))

(deftest parse-all-forms-multiple
  "parse-all-forms returns all top-level forms"
  (let ((forms (cl-cc:parse-all-forms "(defun f (x) x) (f 1)")))
    (assert-= 2 (length forms))
    (assert-eq 'defun (caar forms))
    (assert-eq 'f (cadar forms))))

(deftest parse-all-forms-nested
  "parse-all-forms handles deeply nested forms"
  (let ((form (parse-one-sexp "(let ((x (+ 1 2))) (* x x))")))
    (assert-eq 'let (car form))
    (assert-eq 'x (caadr form))))

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

(deftest parse-defun-head-kind
  "Parser recognizes defun head → :defun kind"
  (let ((node (parse-one-cst "(defun f (x) x)")))
    (assert-eq :defun (cl-cc:cst-node-kind node))))

(deftest parse-let-head-kind
  "Parser recognizes let head → :let kind"
  (let ((node (parse-one-cst "(let ((x 1)) x)")))
    (assert-eq :let (cl-cc:cst-node-kind node))))

(deftest parse-if-head-kind
  "Parser recognizes if head → :if kind"
  (let ((node (parse-one-cst "(if t 1 2)")))
    (assert-eq :if (cl-cc:cst-node-kind node))))

(deftest parse-lambda-head-kind
  "Parser recognizes lambda head → :lambda kind"
  (let ((node (parse-one-cst "(lambda (x) x)")))
    (assert-eq :lambda (cl-cc:cst-node-kind node))))

(deftest parse-unknown-head-call-kind
  "Unknown head defaults to :call kind"
  (let ((node (parse-one-cst "(my-fn a b)")))
    (assert-eq :call (cl-cc:cst-node-kind node))))

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
