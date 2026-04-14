;;;; tests/unit/parse/pratt-tests.lisp — Pratt parser engine unit tests
;;;;
;;;; Tests: pratt-context stream ops, NUD dispatch, CL grammar round-trips,
;;;; parse-cl-source CST structure, parse-all-forms s-expression output,
;;;; error recovery via cst-error-node, and parse-and-lower pipeline.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

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

(deftest-each pratt-expect-adds-diagnostic-on-failure
  "pratt-expect adds a diagnostic when the expected token type doesn't match or input is empty."
  :cases (("type-mismatch" "42" :T-STRING "test")
          ("eof"           ""   :T-INT    "eof-test"))
  (source expected-type label)
  (let ((ctx (make-test-ctx source)))
    (cl-cc::pratt-expect ctx expected-type label)
    (assert-true (not (null (cl-cc::pratt-context-diagnostics ctx))))))

(deftest pratt-expect-mismatch-returns-nil
  "pratt-expect returns nil on type mismatch"
  (let ((ctx (make-test-ctx "42")))
    (let ((result (cl-cc::pratt-expect ctx :T-STRING)))
      (assert-eq nil result))))

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

(deftest-each parse-literal-kind
  "Parsing each literal type produces a cst-token with the correct kind and value."
  :cases (("integer"  "42"        :T-INT     42        nil)
          ("float"    "3.14"      :T-FLOAT   nil       nil)
          ("symbol"   "foo"       :T-IDENT   nil       nil)
          ("keyword"  ":foo"      :T-KEYWORD nil       nil)
          ("string"   "\"hello\"" :T-STRING  nil       "hello"))
  (source expected-kind expected-num expected-str)
  (let ((node (parse-one-cst source)))
    (assert-true (cl-cc:cst-token-p node))
    (assert-eq expected-kind (cl-cc:cst-node-kind node))
    (when expected-num
      (assert-= expected-num (cl-cc:cst-token-value node)))
    (when expected-str
      (assert-string= expected-str (cl-cc:cst-token-value node)))))

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

(deftest-each parse-dispatch-interior-kind
  "Reader dispatch shorthands produce cst-interior nodes with the correct kind."
  :cases (("quote"       "'x"      :quote)
          ("quasiquote"  "`x"      :quasiquote)
          ("function"    "#'foo"   :function)
          ("vector"      "#(1 2 3)" :vector))
  (source expected-kind)
  (let ((node (parse-one-cst source)))
    (assert-true (cl-cc:cst-interior-p node))
    (assert-eq expected-kind (cl-cc:cst-node-kind node))))

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

(deftest-each parse-cl-source-basic-behavior
  "parse-cl-source: multiple forms, diagnostics return, and empty-string handling."
  :cases (("multiple-forms"     "1 2 3" 3  nil)
          ("returns-diagnostics" "42"   1  t)
          ("empty-string"        ""     0  t))
  (source expected-count check-diagnostics-listp)
  (multiple-value-bind (nodes diag)
      (cl-cc:parse-cl-source source)
    (assert-= expected-count (length nodes))
    (when check-diagnostics-listp
      (assert-true (listp diag)))))

;;; ─── parse-all-forms: S-expression Output ───────────────────────────────────

(deftest-each parse-all-forms-value-cases
  "parse-all-forms returns the correct s-expression for various input forms."
  :cases (("integer"    "42"        (lambda (f) (= f 42)))
          ("string"     "\"hello\"" (lambda (f) (string= f "hello")))
          ("nil-list"   "()"        (lambda (f) (null f)))
          ("symbol"     "foo"       (lambda (f) (string= "FOO" (symbol-name f))))
          ("call"       "(+ 1 2)"   (lambda (f) (and (eq '+ (car f)) (= 1 (cadr f)) (= 2 (caddr f)))))
          ("quote"      "'foo"      (lambda (f) (and (eq 'quote (car f)) (string= "FOO" (symbol-name (cadr f))))))
          ("nested"     "(let ((x (+ 1 2))) (* x x))"
                        (lambda (f) (and (eq 'let (car f)) (string= "X" (symbol-name (caaadr f)))))))
  (source pred)
  (assert-true (funcall pred (parse-one-sexp source))))

(deftest parse-all-forms-multiple
  "parse-all-forms returns all top-level forms from multi-form input."
  (let ((forms (cl-cc:parse-all-forms "(defun f (x) x) (f 1)")))
    (assert-= 2 (length forms))
    (assert-eq 'defun (caar forms))
    (assert-string= "F" (symbol-name (cadar forms)))))

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

(deftest-each parse-and-lower-cases
  "parse-and-lower: returns list, produces ast-int for integers, handles multiple forms."
  :cases (("returns-list"      "(+ 1 2)" (lambda (asts) (and (listp asts) (not (null asts)))))
          ("integer-ast-int"   "42"      (lambda (asts) (cl-cc::ast-int-p (first asts))))
          ("multiple-3-forms"  "1 2 3"   (lambda (asts) (= 3 (length asts)))))
  (source pred)
  (assert-true (funcall pred (cl-cc:parse-and-lower source))))

;;; ─── pratt-parse-expr: Direct Tests ──────────────────────────────────────────

(deftest-each pratt-parse-expr-node-type
  "pratt-parse-expr returns the correct CST node type for each input shape."
  :cases (("integer"     "42"               #'cl-cc:cst-token-p    nil)
          ("symbol"      "foo"              #'cl-cc:cst-token-p    nil)
          ("list"        "(+ 1 2)"          #'cl-cc:cst-interior-p nil)
          ("eof-error"   ""                 #'cl-cc::cst-error-node-p nil)
          ("quote"       "'x"               #'cl-cc:cst-interior-p :quote)
          ("nested-list" "(let ((x 1)) x)"  #'cl-cc:cst-interior-p nil))
  (source pred expected-kind)
  (let* ((ctx  (make-test-ctx source))
         (node (cl-cc::pratt-parse-expr ctx)))
    (assert-true (funcall pred node))
    (when expected-kind
      (assert-eq expected-kind (cl-cc:cst-node-kind node)))))

;;; ─── pratt-add-diagnostic: Direct Tests ─────────────────────────────────────

(deftest-each pratt-add-diagnostic-count
  "pratt-add-diagnostic accumulates the correct number of diagnostics."
  :cases (("one"  1)
          ("two"  2))
  (n)
  (let ((ctx (make-test-ctx "42")))
    (dotimes (i n)
      (cl-cc::pratt-add-diagnostic ctx (format nil "error ~a" i) (cons i (1+ i))))
    (assert-equal n (length (cl-cc::pratt-context-diagnostics ctx)))))

(deftest pratt-add-diagnostic-records-message
  "pratt-add-diagnostic stores the error message."
  (let ((ctx (make-test-ctx "42")))
    (cl-cc::pratt-add-diagnostic ctx "unexpected token" (cons 0 2))
    (let ((diag (first (cl-cc::pratt-context-diagnostics ctx))))
      (assert-true (not (null diag))))))

;;; ─── pratt-parse-list-until: Direct Tests ───────────────────────────────────

(deftest-each pratt-parse-list-until-length
  "pratt-parse-list-until returns the correct number of parsed elements."
  :cases (("empty"    "()"      0)
          ("elements" "(1 2 3)" 3))
  (source expected-len)
  (let ((ctx (make-test-ctx source)))
    (cl-cc::pratt-advance ctx) ; consume LPAREN
    (let ((items (cl-cc::pratt-parse-list-until ctx :T-RPAREN
                   (lambda (c) (cl-cc::pratt-parse-expr c)))))
      (assert-equal expected-len (length items)))))

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
