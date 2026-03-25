;;;; tests/unit/frontend/php-tests.lisp
;;;; Unit tests for PHP frontend: lexer and parser

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; Lexer helpers (local, not imported from cl-cc)

(defun php-tok-type  (tok) (getf tok :type))
(defun php-tok-value (tok) (getf tok :value))

;;; ─── Lexer Tests ────────────────────────────────────────────────────────────

(deftest php-lex-empty
  (let ((tokens (tokenize-php-source "<?php ")))
    (assert-= 1 (length tokens))
    (assert-eq :T-EOF (php-tok-type (first tokens)))))

(deftest php-lex-integer
  (let ((tokens (tokenize-php-source "<?php 42;")))
    (assert-eq :T-INT (php-tok-type (first tokens)))
    (assert-= 42 (php-tok-value (first tokens)))))

(deftest php-lex-float
  (let ((tokens (tokenize-php-source "<?php 3.14;")))
    (assert-eq :T-FLOAT (php-tok-type (first tokens)))
    (assert-true (< (abs (- 3.14 (php-tok-value (first tokens)))) 1e-6))))

(deftest-each php-lex-string-literals
  "Both single- and double-quoted strings lex as :T-STRING"
  :cases (("double-quote" "<?php \"hello\";" "hello")
          ("single-quote" "<?php 'world';"  "world"))
  (source expected-val)
  (let ((tok (first (tokenize-php-source source))))
    (assert-eq :T-STRING (php-tok-type tok))
    (assert-equal expected-val (php-tok-value tok))))

(deftest php-lex-variable
  (let ((tokens (tokenize-php-source "<?php $x;")))
    (assert-eq :T-VAR (php-tok-type (first tokens)))
    (assert-true (string-equal "x" (symbol-name (php-tok-value (first tokens)))))))

(deftest-each php-lex-keywords
  "PHP reserved words lex as :T-KEYWORD with the correct value"
  :cases (("if"       "<?php if"       :if)
          ("function" "<?php function" :function))
  (source expected-kw)
  (let ((tok (first (tokenize-php-source source))))
    (assert-eq :T-KEYWORD (php-tok-type tok))
    (assert-eq expected-kw (php-tok-value tok))))

(deftest php-lex-type-int
  (let ((tokens (tokenize-php-source "<?php int")))
    (assert-eq :T-TYPE (php-tok-type (first tokens)))
    (assert-eq :int (php-tok-value (first tokens)))))

(deftest-each php-lex-punctuation
  "PHP punctuation tokens lex to the correct token type"
  :cases (("arrow"         "<?php ->"  :T-ARROW)
          ("nullsafe-arrow" "<?php ?->" :T-NULLSAFE-ARROW)
          ("semicolon"     "<?php ;"   :T-SEMI))
  (source expected-type)
  (assert-eq expected-type (php-tok-type (first (tokenize-php-source source)))))

(deftest php-lex-comment-line
  "Comments should be skipped."
  (let ((tokens (tokenize-php-source "<?php // this is a comment
42;")))
    (assert-eq :T-INT (php-tok-type (first tokens)))
    (assert-= 42 (php-tok-value (first tokens)))))

(deftest php-lex-operators
  (let ((tokens (tokenize-php-source "<?php + - * /")))
    (assert-true (every (lambda (tok) (eq :T-OP (php-tok-type tok)))
                        (butlast tokens)))))  ; exclude :T-EOF

;;; ─── Parser Tests ───────────────────────────────────────────────────────────

(deftest php-parse-integer-literal
  (let ((ast (first (parse-php-source "<?php 42;"))))
    (assert-true (ast-int-p ast))
    (assert-= 42 (ast-int-value ast))))

(deftest php-parse-echo
  "echo expr produces ast-print"
  (let ((ast (first (parse-php-source "<?php echo 42;"))))
    (assert-true (ast-print-p ast))
    (assert-true (ast-int-p (ast-print-expr ast)))))

(deftest php-parse-variable-assignment
  "$x = 42 produces ast-let or ast-setq"
  (let ((ast (first (parse-php-source "<?php $x = 42;"))))
    (assert-true (or (ast-let-p ast) (ast-setq-p ast)))))

(deftest php-parse-if-statement
  "if ($c) { echo 1; } produces ast-if"
  (let ((ast (first (parse-php-source "<?php if ($x) { echo 1; }"))))
    (assert-true (ast-if-p ast))))

(deftest php-parse-function-def
  "function f($x) { return $x; } produces ast-defun"
  (let ((ast (first (parse-php-source "<?php function f($x) { return $x; }"))))
    (assert-true (ast-defun-p ast))
    (assert-string= "F" (symbol-name (ast-defun-name ast)))))

(deftest php-parse-multiple-statements
  (let ((asts (parse-php-source "<?php echo 1; echo 2;")))
    (assert-= 2 (length asts))))

(deftest php-parse-binary-op
  "$a + $b produces ast-binop or ast-call"
  (let ((ast (first (parse-php-source "<?php $a + $b;"))))
    (assert-true (or (ast-binop-p ast) (ast-call-p ast)))))

(deftest php-parse-string-literal
  (let ((ast (first (parse-php-source "<?php \"hello\";"))))
    (assert-true (ast-quote-p ast))
    (assert-equal "hello" (ast-quote-value ast))))

(deftest php-parse-class-def
  "class Foo {} produces ast-defclass"
  (let ((ast (first (parse-php-source "<?php class Foo {}"))))
    (assert-true (ast-defclass-p ast))
    (assert-string= "FOO" (symbol-name (ast-defclass-name ast)))))
