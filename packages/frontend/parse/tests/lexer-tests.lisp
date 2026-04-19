;;;; tests/unit/parse/lexer-tests.lisp — CL lexer unit tests
;;;;
;;;; Tests: integers, floats, ratios, strings, symbols, keywords,
;;;; parens, quote macros, hash dispatch, radix, comments, position
;;;; tracking, lex-all for full forms.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

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

(deftest-each lexer-integer-literals
  "Lexer correctly tokenizes integer literals: type :T-INT and correct numeric value."
  :cases (("simple"   "42"  42)
          ("zero"     "0"   0)
          ("negative" "-7"  -7))
  (source expected-value)
  (assert-eq :T-INT (first-token-type source))
  (assert-= expected-value (first-token-value source)))

;;; ─── Float Tokens ────────────────────────────────────────────────────────────

(deftest lexer-float-cases
  "Lexer: simple float token type and value; float with exponent token type."
  (assert-eq :T-FLOAT (first-token-type "3.14"))
  (let ((val (first-token-value "3.14")))
    (assert-true (< (abs (- val 3.14d0)) 0.001)))
  (assert-eq :T-FLOAT (first-token-type "1.0e5")))

;;; ─── Ratio Tokens ────────────────────────────────────────────────────────────

(deftest lexer-ratio
  "Lexer: ratio literal"
  (assert-eq :T-RATIO (first-token-type "3/4"))
  (assert-eql 3/4 (first-token-value "3/4")))

;;; ─── String Tokens ──────────────────────────────────────────────────────────

(deftest lexer-string-cases
  "Lexer strings: simple string type/value; escape sequences produce correct chars."
  (assert-eq :T-STRING (first-token-type "\"hello\""))
  (assert-string= "hello" (first-token-value "\"hello\""))
  (let ((val (first-token-value "\"a\\nb\"")))
    (assert-= 3 (length val))
    (assert-true (char= (char val 1) #\Newline))))

;;; ─── Symbol Tokens ──────────────────────────────────────────────────────────

(deftest lexer-symbol-cases
  "Lexer symbols: plain symbol uppercased; pipe-escaped symbol preserves case."
  (assert-eq :T-IDENT (first-token-type "foo"))
  (assert-string= "FOO" (symbol-name (first-token-value "foo")))
  (let ((val (first-token-value "|MixedCase|")))
    (assert-string= "MixedCase" (symbol-name val))))

(deftest-each lexer-bool-tokens
  "Lexer: T is :T-BOOL-TRUE and NIL is :T-BOOL-FALSE."
  :cases (("true"  "t"   :T-BOOL-TRUE)
          ("false" "nil" :T-BOOL-FALSE))
  (source expected-type)
  (assert-eq expected-type (first-token-type source)))

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

(deftest lexer-hash-misc-cases
  "Lexer hash dispatch: #' → :T-FUNCTION; #( → :T-VECTOR-START."
  (assert-eq :T-FUNCTION    (first-token-type "#'foo"))
  (assert-eq :T-VECTOR-START (first-token-type "#(1 2)")))

(deftest-each lexer-hash-char-dispatch
  "Lexer: #\\char produces :T-CHAR tokens for regular and named character forms."
  :cases (("letter" "#\\a"     #\a)
          ("space"  "#\\Space" #\Space))
  (source expected-char)
  (assert-eq :T-CHAR (first-token-type source))
  (assert-true (char= expected-char (first-token-value source))))


;;; ─── Radix Dispatch ─────────────────────────────────────────────────────────

(deftest-each lexer-radix-dispatch
  "Lexer: radix dispatch produces correct integer values"
  ((input expected-value)
   ("#b101" 5)
   ("#o10"  8)
   ("#xFF"  255))
  (assert-= expected-value (first-token-value input)))

;;; ─── Comments ───────────────────────────────────────────────────────────────

(deftest-each lexer-comment-forms
  "Both line and block comments are stripped from the token stream."
  :cases (("line"  (format nil "; comment~%42"))
          ("block" "#| block |# 99"))
  (source)
  (assert-equal '(:T-INT) (token-types source)))

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

(deftest-each lexer-full-form-token-types
  "lex-all produces the correct token type sequence for complete forms."
  :cases (("simple-list" "(+ 1 2)"  '(:T-LPAREN :T-IDENT :T-INT :T-INT :T-RPAREN))
          ("quoted-list" "'(a b)"   '(:T-QUOTE :T-LPAREN :T-IDENT :T-IDENT :T-RPAREN)))
  (source expected-types)
  (assert-equal expected-types (token-types source)))

(deftest lexer-full-form-defun
  "Lexer: lex-all on (defun f (x) x) produces 8 tokens wrapped in parens."
  (let ((types (token-types "(defun f (x) x)")))
    (assert-= 8 (length types))
    (assert-eq :T-LPAREN (first types))
    (assert-eq :T-RPAREN (car (last types)))))

(deftest-each lexer-eof-always-present
  "lex-all always terminates with a :T-EOF token."
  :cases (("non-empty" "42" nil)
          ("empty"     ""   1))
  (source expected-count)
  (let ((tokens (cl-cc:lex-all source)))
    (assert-eq :T-EOF (cl-cc:lexer-token-type (car (last tokens))))
    (when expected-count
      (assert-= expected-count (length tokens)))))
