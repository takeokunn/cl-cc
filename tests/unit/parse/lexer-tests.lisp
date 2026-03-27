;;;; tests/unit/parse/lexer-tests.lisp — CL lexer unit tests
;;;;
;;;; Tests: integers, floats, ratios, strings, symbols, keywords,
;;;; parens, quote macros, hash dispatch, radix, comments, position
;;;; tracking, lex-all for full forms.

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; ─── Helper ──────────────────────────────────────────────────────────────────

(defun first-token-type (source)
  "Lex SOURCE and return the type of the first token."
  (let ((tokens (cl-cc:lex-all source)))
    (cl-cc:lexer-token-type (first tokens))))

(defun first-token-value (source)
  "Lex SOURCE and return the value of the first token."
  (let ((tokens (cl-cc:lex-all source)))
    (cl-cc:lexer-token-value (first tokens))))

(defun token-types (source)
  "Lex SOURCE and return list of token types (excluding :T-EOF)."
  (let ((tokens (cl-cc:lex-all source)))
    (remove :T-EOF (mapcar #'cl-cc:lexer-token-type tokens))))

;;; ─── Integer Tokens ──────────────────────────────────────────────────────────

(deftest lexer-integer-simple
  "Lexer: simple integer"
  (assert-eq :T-INT (first-token-type "42"))
  (assert-= 42 (first-token-value "42")))

(deftest lexer-integer-zero
  "Lexer: zero"
  (assert-= 0 (first-token-value "0")))

(deftest lexer-integer-negative
  "Lexer: negative integer"
  (assert-= -7 (first-token-value "-7")))

;;; ─── Float Tokens ────────────────────────────────────────────────────────────

(deftest lexer-float-simple
  "Lexer: simple float"
  (assert-eq :T-FLOAT (first-token-type "3.14"))
  (let ((val (first-token-value "3.14")))
    (assert-true (< (abs (- val 3.14d0)) 0.001))))

(deftest lexer-float-exponent
  "Lexer: float with exponent"
  (assert-eq :T-FLOAT (first-token-type "1.0e5")))

;;; ─── Ratio Tokens ────────────────────────────────────────────────────────────

(deftest lexer-ratio
  "Lexer: ratio literal"
  (assert-eq :T-RATIO (first-token-type "3/4"))
  (assert-eql 3/4 (first-token-value "3/4")))

;;; ─── String Tokens ──────────────────────────────────────────────────────────

(deftest lexer-string-simple
  "Lexer: simple string"
  (assert-eq :T-STRING (first-token-type "\"hello\""))
  (assert-string= "hello" (first-token-value "\"hello\"")))

(deftest lexer-string-escapes
  "Lexer: string with escape sequences"
  (let ((val (first-token-value "\"a\\nb\"")))
    (assert-= 3 (length val))
    (assert-true (char= (char val 1) #\Newline))))

;;; ─── Symbol Tokens ──────────────────────────────────────────────────────────

(deftest lexer-symbol-simple
  "Lexer: simple symbol is uppercased"
  (assert-eq :T-IDENT (first-token-type "foo"))
  (assert-string= "FOO" (symbol-name (first-token-value "foo"))))

(deftest lexer-symbol-pipe-escaped
  "Lexer: pipe-escaped symbol preserves case"
  (let ((val (first-token-value "|MixedCase|")))
    (assert-string= "MixedCase" (symbol-name val))))

(deftest lexer-bool-true
  "Lexer: T is recognized as :T-BOOL-TRUE"
  (assert-eq :T-BOOL-TRUE (first-token-type "t")))

(deftest lexer-bool-false-nil
  "Lexer: NIL is recognized as :T-BOOL-FALSE"
  (assert-eq :T-BOOL-FALSE (first-token-type "nil")))

;;; ─── Keyword Tokens ─────────────────────────────────────────────────────────

(deftest lexer-keyword
  "Lexer: keyword symbol"
  (assert-eq :T-KEYWORD (first-token-type ":foo"))
  (assert-eq :FOO (first-token-value ":foo")))

;;; ─── Parens ─────────────────────────────────────────────────────────────────

(deftest lexer-parens
  "Lexer: parentheses"
  (let ((types (token-types "()")))
    (assert-equal '(:T-LPAREN :T-RPAREN) types)))

;;; ─── Quote Macros ───────────────────────────────────────────────────────────

(deftest-each lexer-quote-macros
  "Lexer: quote macro characters produce correct token types"
  ((input expected-token-type)
   ("'x"  :T-QUOTE)
   ("`x"  :T-BACKQUOTE)
   (",x"  :T-UNQUOTE)
   (",@x" :T-UNQUOTE-SPLICING))
  (assert-eq expected-token-type (first-token-type input)))

;;; ─── Hash Dispatch ──────────────────────────────────────────────────────────

(deftest lexer-hash-function
  "Lexer: #' function dispatch"
  (assert-eq :T-FUNCTION (first-token-type "#'foo")))

(deftest lexer-hash-char
  "Lexer: #\\a character literal"
  (assert-eq :T-CHAR (first-token-type "#\\a"))
  (assert-true (char= #\a (first-token-value "#\\a"))))

(deftest lexer-hash-char-named
  "Lexer: #\\Space named character"
  (assert-true (char= #\Space (first-token-value "#\\Space"))))

(deftest lexer-hash-vector
  "Lexer: #( vector start"
  (assert-eq :T-VECTOR-START (first-token-type "#(1 2)")))

;;; ─── Radix Dispatch ─────────────────────────────────────────────────────────

(deftest-each lexer-radix-dispatch
  "Lexer: radix dispatch produces correct integer values"
  ((input expected-value)
   ("#b101" 5)
   ("#o10"  8)
   ("#xFF"  255))
  (assert-= expected-value (first-token-value input)))

;;; ─── Comments ───────────────────────────────────────────────────────────────

(deftest lexer-line-comment
  "Lexer: line comment is trivia, not a token"
  (let ((types (token-types (format nil "; comment~%42"))))
    (assert-equal '(:T-INT) types)))

(deftest lexer-block-comment
  "Lexer: block comment #|...|# is trivia"
  (let ((types (token-types "#| block |# 99")))
    (assert-equal '(:T-INT) types)))

;;; ─── Position Tracking ──────────────────────────────────────────────────────

(deftest lexer-position-start-byte
  "Lexer: start-byte tracks position"
  (let* ((tokens (cl-cc:lex-all "  42"))
         (tok (first tokens)))
    (assert-= 2 (cl-cc:lexer-token-start-byte tok))))

;;; ─── Dot Token ──────────────────────────────────────────────────────────────

(deftest lexer-dot
  "Lexer: dot token for dotted pairs"
  (let ((types (token-types "(a . b)")))
    (assert-equal '(:T-LPAREN :T-IDENT :T-DOT :T-IDENT :T-RPAREN) types)))

;;; ─── lex-all Full Forms ─────────────────────────────────────────────────────

(deftest lexer-full-form-simple-list
  "Lexer: lex-all on (+ 1 2) produces correct token types"
  (let ((types (token-types "(+ 1 2)")))
    (assert-equal '(:T-LPAREN :T-IDENT :T-INT :T-INT :T-RPAREN) types)))

(deftest lexer-full-form-defun
  "Lexer: lex-all on (defun f (x) x) produces correct tokens"
  (let ((types (token-types "(defun f (x) x)")))
    (assert-= 8 (length types))
    (assert-eq :T-LPAREN (first types))
    (assert-eq :T-RPAREN (car (last types)))))

(deftest lexer-full-form-quoted
  "Lexer: lex-all on '(a b) produces quote + list tokens"
  (let ((types (token-types "'(a b)")))
    (assert-eq :T-QUOTE (first types))))

(deftest lexer-eof-token
  "Lexer: lex-all always ends with :T-EOF"
  (let* ((tokens (cl-cc:lex-all "42"))
         (last-tok (car (last tokens))))
    (assert-eq :T-EOF (cl-cc:lexer-token-type last-tok))))

(deftest lexer-empty-input
  "Lexer: empty input produces just :T-EOF"
  (let ((tokens (cl-cc:lex-all "")))
    (assert-= 1 (length tokens))
    (assert-eq :T-EOF (cl-cc:lexer-token-type (first tokens)))))
