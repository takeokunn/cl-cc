;;;; packages/javascript/tests/js-lexer-tests.lisp — ES2026 JavaScript Lexer Tests
;;;;
;;;; 35 FiveAM tests covering tokenize-js-source for all major token categories.
;;;; Token format: (:type :T-XXX :value val)

(in-package :cl-user)
(defpackage :cl-cc/javascript-test
  (:use :cl :fiveam :cl-cc/javascript))
(in-package :cl-cc/javascript-test)

;;; ─── Helpers ──────────────────────────────────────────────────────────────────

(defun %js-lex (src)
  "Tokenize SRC and return the token list (including :T-EOF)."
  (cl-cc/javascript:tokenize-js-source src))

(defun %js-lex-types (src)
  "Return only the :type field of each token produced from SRC."
  (mapcar (lambda (tok) (getf tok :type)) (%js-lex src)))

(defun %js-lex-values (src)
  "Return only the :value field of each token produced from SRC."
  (mapcar (lambda (tok) (getf tok :value)) (%js-lex src)))

(defun %js-first-token (src)
  "Return the first token produced from SRC."
  (first (%js-lex src)))

(defun %js-first-type (src)
  "Return the :type of the first token produced from SRC."
  (getf (%js-first-token src) :type))

(defun %js-first-value (src)
  "Return the :value of the first token produced from SRC."
  (getf (%js-first-token src) :value))

;;; ─── Integer literals ─────────────────────────────────────────────────────────

(test lex-integers-42
  "42 is lexed as :T-NUMBER with numeric value 42."
  (is (eq :T-NUMBER (%js-first-type "42")))
  (is (= 42 (%js-first-value "42"))))

(test lex-integers-zero
  "0 is lexed as :T-NUMBER with numeric value 0."
  (is (eq :T-NUMBER (%js-first-type "0")))
  (is (= 0 (%js-first-value "0"))))

(test lex-integers-255
  "255 is lexed as :T-NUMBER with numeric value 255."
  (is (eq :T-NUMBER (%js-first-type "255")))
  (is (= 255 (%js-first-value "255"))))

;;; ─── Float literals ───────────────────────────────────────────────────────────

(test lex-floats-pi
  "3.14 is lexed as :T-NUMBER with a double-float value."
  (is (eq :T-NUMBER (%js-first-type "3.14")))
  (is (typep (%js-first-value "3.14") 'double-float))
  (is (< (abs (- 3.14d0 (%js-first-value "3.14"))) 1.0d-10)))

(test lex-floats-scientific
  "1.5e-3 is lexed as :T-NUMBER with approximate float value 0.0015."
  (is (eq :T-NUMBER (%js-first-type "1.5e-3")))
  (is (< (abs (- 1.5d-3 (%js-first-value "1.5e-3"))) 1.0d-15)))

;;; ─── Hex literals ─────────────────────────────────────────────────────────────

(test lex-hex-0xff
  "0xFF is lexed as :T-NUMBER with integer value 255."
  (is (eq :T-NUMBER (%js-first-type "0xFF")))
  (is (= 255 (%js-first-value "0xFF"))))

;;; ─── Octal literals ───────────────────────────────────────────────────────────

(test lex-octal-0o777
  "0o777 is lexed as :T-NUMBER with integer value 511."
  (is (eq :T-NUMBER (%js-first-type "0o777")))
  (is (= 511 (%js-first-value "0o777"))))

;;; ─── Binary literals ──────────────────────────────────────────────────────────

(test lex-binary-0b1010
  "0b1010 is lexed as :T-NUMBER with integer value 10."
  (is (eq :T-NUMBER (%js-first-type "0b1010")))
  (is (= 10 (%js-first-value "0b1010"))))

;;; ─── BigInt literals ──────────────────────────────────────────────────────────

(test lex-bigint-42n
  "42n is lexed as :T-BIGINT with integer value 42."
  (is (eq :T-BIGINT (%js-first-type "42n")))
  (is (= 42 (%js-first-value "42n"))))

;;; ─── Numeric separator ────────────────────────────────────────────────────────

(test lex-numeric-separator-1_000_000
  "1_000_000 (numeric separator) is lexed as :T-NUMBER with value 1000000."
  (is (eq :T-NUMBER (%js-first-type "1_000_000")))
  (is (= 1000000 (%js-first-value "1_000_000"))))

;;; ─── Single-quoted string ─────────────────────────────────────────────────────

(test lex-string-single-quoted
  "Single-quoted 'hello' is lexed as :T-STRING with value \"hello\"."
  (is (eq :T-STRING (%js-first-type "'hello'")))
  (is (string= "hello" (%js-first-value "'hello'"))))

;;; ─── Double-quoted string ─────────────────────────────────────────────────────

(test lex-string-double-quoted
  "Double-quoted \"world\" is lexed as :T-STRING with value \"world\"."
  (is (eq :T-STRING (%js-first-type "\"world\"")))
  (is (string= "world" (%js-first-value "\"world\""))))

;;; ─── Keyword tokens ───────────────────────────────────────────────────────────

(test lex-keyword-if
  "if is lexed as :T-IF."
  (is (eq :T-IF (%js-first-type "if"))))

(test lex-keyword-for
  "for is lexed as :T-FOR."
  (is (eq :T-FOR (%js-first-type "for"))))

(test lex-keyword-class
  "class is lexed as :T-CLASS."
  (is (eq :T-CLASS (%js-first-type "class"))))

(test lex-keyword-async
  "async is lexed as :T-ASYNC."
  (is (eq :T-ASYNC (%js-first-type "async"))))

(test lex-keyword-await
  "await is lexed as :T-AWAIT."
  (is (eq :T-AWAIT (%js-first-type "await"))))

(test lex-keyword-using
  "using is lexed as :T-USING (ES2025 explicit resource management)."
  (is (eq :T-USING (%js-first-type "using"))))

;;; ─── Boolean / null / undefined ──────────────────────────────────────────────

(test lex-boolean-null-true
  "true is lexed as :T-TRUE."
  (is (eq :T-TRUE (%js-first-type "true"))))

(test lex-boolean-null-false
  "false is lexed as :T-FALSE."
  (is (eq :T-FALSE (%js-first-type "false"))))

(test lex-boolean-null-null
  "null is lexed as :T-NULL."
  (is (eq :T-NULL (%js-first-type "null"))))

(test lex-boolean-null-undefined
  "undefined is lexed as :T-UNDEFINED."
  (is (eq :T-UNDEFINED (%js-first-type "undefined"))))

;;; ─── Identifiers ──────────────────────────────────────────────────────────────

(test lex-identifier-foo
  "foo is lexed as :T-IDENT with value \"foo\"."
  (is (eq :T-IDENT (%js-first-type "foo")))
  (is (string= "foo" (%js-first-value "foo"))))

(test lex-identifier-underscore-bar
  "_bar is lexed as :T-IDENT."
  (is (eq :T-IDENT (%js-first-type "_bar")))
  (is (string= "_bar" (%js-first-value "_bar"))))

(test lex-identifier-dollar-baz
  "$baz is lexed as :T-IDENT."
  (is (eq :T-IDENT (%js-first-type "$baz")))
  (is (string= "$baz" (%js-first-value "$baz"))))

(test lex-identifier-camel-case
  "camelCase is lexed as :T-IDENT."
  (is (eq :T-IDENT (%js-first-type "camelCase")))
  (is (string= "camelCase" (%js-first-value "camelCase"))))

;;; ─── Private identifiers ──────────────────────────────────────────────────────

(test lex-private-ident-field
  "#field is lexed as :T-PRIVATE-IDENT with value \"field\" (no hash prefix)."
  (is (eq :T-PRIVATE-IDENT (%js-first-type "#field")))
  (is (string= "field" (%js-first-value "#field"))))

(test lex-private-ident-method
  "#privateMethod is lexed as :T-PRIVATE-IDENT."
  (is (eq :T-PRIVATE-IDENT (%js-first-type "#privateMethod")))
  (is (string= "privateMethod" (%js-first-value "#privateMethod"))))

;;; ─── Decorator ────────────────────────────────────────────────────────────────

(test lex-decorator-at
  "@decorator starts with a :T-AT token followed by an :T-IDENT token."
  (let ((types (%js-lex-types "@decorator")))
    (is (eq :T-AT (first types)))
    (is (eq :T-IDENT (second types)))))

;;; ─── Multi-character operators ────────────────────────────────────────────────

(test lex-operators-strict-eq
  "=== is lexed as :T-OP with value \"===\"."
  (is (eq :T-OP (%js-first-type "===")))
  (is (string= "===" (%js-first-value "==="))))

(test lex-operators-strict-neq
  "!== is lexed as :T-OP with value \"!==\"."
  (is (eq :T-OP (%js-first-type "!==")))
  (is (string= "!==" (%js-first-value "!=="))))

(test lex-operators-nullish-coalesce
  "?? is lexed as :T-OP with value \"??\"."
  (is (eq :T-OP (%js-first-type "??")))
  (is (string= "??" (%js-first-value "??"))))

(test lex-operators-optional-chain
  "?. is lexed as :T-OP with value \"?.\"."
  (is (eq :T-OP (%js-first-type "?.")))
  (is (string= "?." (%js-first-value "?."))))

(test lex-operators-logical-and-assign
  "&&= is lexed as :T-OP with value \"&&=\"."
  (is (eq :T-OP (%js-first-type "&&=")))
  (is (string= "&&=" (%js-first-value "&&="))))

(test lex-operators-logical-or-assign
  "||= is lexed as :T-OP with value \"||=\"."
  (is (eq :T-OP (%js-first-type "||=")))
  (is (string= "||=" (%js-first-value "||="))))

(test lex-operators-nullish-assign
  "??= is lexed as :T-OP with value \"??=\"."
  (is (eq :T-OP (%js-first-type "??=")))
  (is (string= "??=" (%js-first-value "??="))))

(test lex-operators-exponent-assign
  "**= is lexed as :T-OP with value \"**=\"."
  (is (eq :T-OP (%js-first-type "**=")))
  (is (string= "**=" (%js-first-value "**="))))

;;; ─── Ellipsis ─────────────────────────────────────────────────────────────────

(test lex-ellipsis
  "... is lexed as :T-ELLIPSIS with value \"...\"."
  (is (eq :T-ELLIPSIS (%js-first-type "...")))
  (is (string= "..." (%js-first-value "..."))))

;;; ─── Arrow ────────────────────────────────────────────────────────────────────

(test lex-arrow
  "=> is lexed as :T-ARROW with value \"=>\"."
  (is (eq :T-ARROW (%js-first-type "=>")))
  (is (string= "=>" (%js-first-value "=>"))))

;;; ─── Comments are skipped ─────────────────────────────────────────────────────

(test lex-line-comment-skipped
  "// comment is skipped; only :T-EOF follows."
  (let ((types (%js-lex-types "// this is a comment")))
    (is (= 1 (length types)))
    (is (eq :T-EOF (first types)))))

(test lex-block-comment-skipped
  "/* block comment */ is skipped; identifier after it is first real token."
  (let ((tokens (%js-lex "/* skip me */ foo")))
    (is (eq :T-IDENT (getf (first tokens) :type)))
    (is (string= "foo" (getf (first tokens) :value)))))

;;; ─── Multi-token sequence ─────────────────────────────────────────────────────

(test lex-multiple-tokens-const-stmt
  "\"const x = 42 + y;\" produces const/ident/=/42/+/y/semi/eof in order."
  (let ((types (%js-lex-types "const x = 42 + y;")))
    (is (equal '(:T-CONST :T-IDENT :T-OP :T-NUMBER :T-OP :T-IDENT :T-SEMI :T-EOF)
               types))))
