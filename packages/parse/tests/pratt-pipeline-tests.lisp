;;;; tests/unit/parse/pratt-pipeline-tests.lisp — Pratt parser pipeline tests
;;;;
;;;; Tests: CST node source positions, parse-cl-source multiple forms,
;;;; parse-all-forms s-expression output, parse-source single form,
;;;; error recovery via cst-error, special form dispatch,
;;;; parse-and-lower pipeline, pratt-parse-expr, pratt-add-diagnostic,
;;;; and pratt-parse-list-until.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

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

;;; ─── Error Recovery via cst-error ───────────────────────────────────────────

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
          ("integer-ast-int"   "42"      (lambda (asts) (cl-cc/ast:ast-int-p (first asts))))
          ("multiple-3-forms"  "1 2 3"   (lambda (asts) (= 3 (length asts)))))
  (source pred)
  (assert-true (funcall pred (cl-cc:parse-and-lower source))))

;;; ─── pratt-parse-expr: Direct Tests ──────────────────────────────────────────

(deftest pratt-parse-expr-empty-nud-table-returns-error
  "pratt-parse-expr with an empty NUD table returns cst-error for any non-empty input."
  (let* ((ctx  (make-test-ctx "42"))
         (node (cl-cc/parse::pratt-parse-expr ctx)))
    (assert-true (cl-cc/parse:cst-error-p node))))

(deftest pratt-parse-expr-eof-returns-error
  "pratt-parse-expr on empty input returns cst-error even with empty NUD table."
  (let* ((ctx  (make-test-ctx ""))
         (node (cl-cc/parse::pratt-parse-expr ctx)))
    (assert-true (cl-cc/parse:cst-error-p node))))

(deftest-each pratt-parse-expr-node-type
  "parse-cl-source returns the correct CST node type for each CL grammar input shape."
  :cases (("integer"     "42"               #'cl-cc:cst-token-p    nil)
          ("symbol"      "foo"              #'cl-cc:cst-token-p    nil)
          ("list"        "(+ 1 2)"          #'cl-cc:cst-interior-p nil)
          ("quote"       "'x"               #'cl-cc:cst-interior-p :quote)
          ("nested-list" "(let ((x 1)) x)"  #'cl-cc:cst-interior-p nil))
  (source pred expected-kind)
  (let ((node (parse-one-cst source)))
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
      (cl-cc/parse::pratt-add-diagnostic ctx (format nil "error ~a" i) (cons i (1+ i))))
    (assert-equal n (length (cl-cc/parse::pratt-context-diagnostics ctx)))))

(deftest pratt-add-diagnostic-records-message
  "pratt-add-diagnostic stores the error message."
  (let ((ctx (make-test-ctx "42")))
    (cl-cc/parse::pratt-add-diagnostic ctx "unexpected token" (cons 0 2))
    (let ((diag (first (cl-cc/parse::pratt-context-diagnostics ctx))))
      (assert-true (not (null diag))))))

;;; ─── pratt-parse-list-until: Direct Tests ───────────────────────────────────

(deftest-each pratt-parse-list-until-length
  "pratt-parse-list-until returns the correct number of parsed elements."
  :cases (("empty"    "()"      0)
          ("elements" "(1 2 3)" 3))
  (source expected-len)
  (let ((ctx (make-test-ctx source)))
    (cl-cc/parse::pratt-advance ctx) ; consume LPAREN
    (let ((items (cl-cc/parse::pratt-parse-list-until ctx :T-RPAREN
                   (lambda (c) (cl-cc/parse::pratt-parse-expr c)))))
      (assert-equal expected-len (length items)))))

(deftest pratt-parse-list-until-consumes-terminator
  "pratt-parse-list-until consumes the end token."
  (let ((ctx (make-test-ctx "(1) 42")))
    (cl-cc/parse::pratt-advance ctx) ; consume LPAREN
    (cl-cc/parse::pratt-parse-list-until ctx :T-RPAREN
      (lambda (c) (cl-cc/parse::pratt-parse-expr c)))
    ;; Next token should be 42, not RPAREN
    (let ((tok (cl-cc/parse::pratt-peek ctx)))
      (assert-eq :T-INT (cl-cc:lexer-token-type tok))
      (assert-equal 42 (cl-cc:lexer-token-value tok)))))
