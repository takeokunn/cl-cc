;;;; packages/javascript/tests/js-lexer-tests.lisp — ES2026 JavaScript Lexer Tests
;;;;
;;;; 35 FiveAM tests covering cl-cc/javascript:tokenize-js-source for all major token categories.
;;;; Token format: (:type :T-XXX :value val)

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

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

(deftest lex-integers-42
  "42 is lexed as :T-NUMBER with numeric value 42."
  (assert-true (eq :T-NUMBER (%js-first-type "42")))
  (assert-true (= 42 (%js-first-value "42"))))

(deftest lex-integers-zero
  "0 is lexed as :T-NUMBER with numeric value 0."
  (assert-true (eq :T-NUMBER (%js-first-type "0")))
  (assert-true (= 0 (%js-first-value "0"))))

(deftest lex-integers-255
  "255 is lexed as :T-NUMBER with numeric value 255."
  (assert-true (eq :T-NUMBER (%js-first-type "255")))
  (assert-true (= 255 (%js-first-value "255"))))

;;; ─── Float literals ───────────────────────────────────────────────────────────

(deftest lex-floats-pi
  "3.14 is lexed as :T-NUMBER with a double-float value."
  (assert-true (eq :T-NUMBER (%js-first-type "3.14")))
  (assert-true (typep (%js-first-value "3.14") 'double-float))
  (assert-true (< (abs (- 3.14d0 (%js-first-value "3.14"))) 1.0d-10)))

(deftest lex-floats-scientific
  "1.5e-3 is lexed as :T-NUMBER with approximate float value 0.0015."
  (assert-true (eq :T-NUMBER (%js-first-type "1.5e-3")))
  (assert-true (< (abs (- 1.5d-3 (%js-first-value "1.5e-3"))) 1.0d-15)))

;;; ─── Hex literals ─────────────────────────────────────────────────────────────

(deftest lex-hex-0xff
  "0xFF is lexed as :T-NUMBER with integer value 255."
  (assert-true (eq :T-NUMBER (%js-first-type "0xFF")))
  (assert-true (= 255 (%js-first-value "0xFF"))))

;;; ─── Octal literals ───────────────────────────────────────────────────────────

(deftest lex-octal-0o777
  "0o777 is lexed as :T-NUMBER with integer value 511."
  (assert-true (eq :T-NUMBER (%js-first-type "0o777")))
  (assert-true (= 511 (%js-first-value "0o777"))))

;;; ─── Binary literals ──────────────────────────────────────────────────────────

(deftest lex-binary-0b1010
  "0b1010 is lexed as :T-NUMBER with integer value 10."
  (assert-true (eq :T-NUMBER (%js-first-type "0b1010")))
  (assert-true (= 10 (%js-first-value "0b1010"))))

;;; ─── BigInt literals ──────────────────────────────────────────────────────────

(deftest lex-bigint-42n
  "42n is lexed as :T-BIGINT with integer value 42."
  (assert-true (eq :T-BIGINT (%js-first-type "42n")))
  (assert-true (= 42 (%js-first-value "42n"))))

;;; ─── Numeric separator ────────────────────────────────────────────────────────

(deftest lex-numeric-separator-1_000_000
  "1_000_000 (numeric separator) is lexed as :T-NUMBER with value 1000000."
  (assert-true (eq :T-NUMBER (%js-first-type "1_000_000")))
  (assert-true (= 1000000 (%js-first-value "1_000_000"))))

;;; ─── Single-quoted string ─────────────────────────────────────────────────────

(deftest lex-string-single-quoted
  "Single-quoted 'hello' is lexed as :T-STRING with value \"hello\"."
  (assert-true (eq :T-STRING (%js-first-type "'hello'")))
  (assert-true (string= "hello" (%js-first-value "'hello'"))))

;;; ─── Double-quoted string ─────────────────────────────────────────────────────

(deftest lex-string-double-quoted
  "Double-quoted \"world\" is lexed as :T-STRING with value \"world\"."
  (assert-true (eq :T-STRING (%js-first-type "\"world\"")))
  (assert-true (string= "world" (%js-first-value "\"world\""))))

;;; ─── Keyword tokens ───────────────────────────────────────────────────────────

(deftest lex-keyword-if
  "if is lexed as :T-IF."
  (assert-true (eq :T-IF (%js-first-type "if"))))

(deftest lex-keyword-for
  "for is lexed as :T-FOR."
  (assert-true (eq :T-FOR (%js-first-type "for"))))

(deftest lex-keyword-class
  "class is lexed as :T-CLASS."
  (assert-true (eq :T-CLASS (%js-first-type "class"))))

(deftest lex-keyword-async
  "async is lexed as :T-ASYNC."
  (assert-true (eq :T-ASYNC (%js-first-type "async"))))

(deftest lex-keyword-await
  "await is lexed as :T-AWAIT."
  (assert-true (eq :T-AWAIT (%js-first-type "await"))))

(deftest lex-keyword-using
  "using is lexed as :T-USING (ES2025 explicit resource management)."
  (assert-true (eq :T-USING (%js-first-type "using"))))

;;; ─── Boolean / null / undefined ──────────────────────────────────────────────

(deftest lex-boolean-null-true
  "true is lexed as :T-TRUE."
  (assert-true (eq :T-TRUE (%js-first-type "true"))))

(deftest lex-boolean-null-false
  "false is lexed as :T-FALSE."
  (assert-true (eq :T-FALSE (%js-first-type "false"))))

(deftest lex-boolean-null-null
  "null is lexed as :T-NULL."
  (assert-true (eq :T-NULL (%js-first-type "null"))))

(deftest lex-boolean-null-undefined
  "undefined is lexed as :T-UNDEFINED."
  (assert-true (eq :T-UNDEFINED (%js-first-type "undefined"))))

;;; ─── Identifiers ──────────────────────────────────────────────────────────────

(deftest lex-identifier-foo
  "foo is lexed as :T-IDENT with value \"foo\"."
  (assert-true (eq :T-IDENT (%js-first-type "foo")))
  (assert-true (string= "foo" (%js-first-value "foo"))))

(deftest lex-identifier-underscore-bar
  "_bar is lexed as :T-IDENT."
  (assert-true (eq :T-IDENT (%js-first-type "_bar")))
  (assert-true (string= "_bar" (%js-first-value "_bar"))))

(deftest lex-identifier-dollar-baz
  "$baz is lexed as :T-IDENT."
  (assert-true (eq :T-IDENT (%js-first-type "$baz")))
  (assert-true (string= "$baz" (%js-first-value "$baz"))))

(deftest lex-identifier-camel-case
  "camelCase is lexed as :T-IDENT."
  (assert-true (eq :T-IDENT (%js-first-type "camelCase")))
  (assert-true (string= "camelCase" (%js-first-value "camelCase"))))

;;; ─── Private identifiers ──────────────────────────────────────────────────────

(deftest lex-private-ident-field
  "#field is lexed as :T-PRIVATE-IDENT with value \"field\" (no hash prefix)."
  (assert-true (eq :T-PRIVATE-IDENT (%js-first-type "#field")))
  (assert-true (string= "field" (%js-first-value "#field"))))

(deftest lex-private-ident-method
  "#privateMethod is lexed as :T-PRIVATE-IDENT."
  (assert-true (eq :T-PRIVATE-IDENT (%js-first-type "#privateMethod")))
  (assert-true (string= "privateMethod" (%js-first-value "#privateMethod"))))

;;; ─── Decorator ────────────────────────────────────────────────────────────────

(deftest lex-decorator-at
  "@decorator starts with a :T-AT token followed by an :T-IDENT token."
  (let ((types (%js-lex-types "@decorator")))
    (assert-true (eq :T-AT (first types)))
    (assert-true (eq :T-IDENT (second types)))))

;;; ─── Multi-character operators ────────────────────────────────────────────────

(deftest lex-operators-strict-eq
  "=== is lexed as :T-OP with value \"===\"."
  (assert-true (eq :T-OP (%js-first-type "===")))
  (assert-true (string= "===" (%js-first-value "==="))))

(deftest lex-operators-strict-neq
  "!== is lexed as :T-OP with value \"!==\"."
  (assert-true (eq :T-OP (%js-first-type "!==")))
  (assert-true (string= "!==" (%js-first-value "!=="))))

(deftest lex-operators-nullish-coalesce
  "?? is lexed as :T-OP with value \"??\"."
  (assert-true (eq :T-OP (%js-first-type "??")))
  (assert-true (string= "??" (%js-first-value "??"))))

(deftest lex-operators-optional-chain
  "?. is lexed as :T-OP with value \"?.\"."
  (assert-true (eq :T-OP (%js-first-type "?.")))
  (assert-true (string= "?." (%js-first-value "?."))))

(deftest lex-operators-logical-and-assign
  "&&= is lexed as :T-OP with value \"&&=\"."
  (assert-true (eq :T-OP (%js-first-type "&&=")))
  (assert-true (string= "&&=" (%js-first-value "&&="))))

(deftest lex-operators-logical-or-assign
  "||= is lexed as :T-OP with value \"||=\"."
  (assert-true (eq :T-OP (%js-first-type "||=")))
  (assert-true (string= "||=" (%js-first-value "||="))))

(deftest lex-operators-nullish-assign
  "??= is lexed as :T-OP with value \"??=\"."
  (assert-true (eq :T-OP (%js-first-type "??=")))
  (assert-true (string= "??=" (%js-first-value "??="))))

(deftest lex-operators-exponent-assign
  "**= is lexed as :T-OP with value \"**=\"."
  (assert-true (eq :T-OP (%js-first-type "**=")))
  (assert-true (string= "**=" (%js-first-value "**="))))

;;; ─── Ellipsis ─────────────────────────────────────────────────────────────────

(deftest lex-ellipsis
  "... is lexed as :T-ELLIPSIS with value \"...\"."
  (assert-true (eq :T-ELLIPSIS (%js-first-type "...")))
  (assert-true (string= "..." (%js-first-value "..."))))

;;; ─── Arrow ────────────────────────────────────────────────────────────────────

(deftest lex-arrow
  "=> is lexed as :T-ARROW with value \"=>\"."
  (assert-true (eq :T-ARROW (%js-first-type "=>")))
  (assert-true (string= "=>" (%js-first-value "=>"))))

;;; ─── Comments are skipped ─────────────────────────────────────────────────────

(deftest lex-line-comment-skipped
  "// comment is skipped; only :T-EOF follows."
  (let ((types (%js-lex-types "// this is a comment")))
    (assert-true (= 1 (length types)))
    (assert-true (eq :T-EOF (first types)))))

(deftest lex-block-comment-skipped
  "/* block comment */ is skipped; identifier after it is first real token."
  (let ((tokens (%js-lex "/* skip me */ foo")))
    (assert-true (eq :T-IDENT (getf (first tokens) :type)))
    (assert-true (string= "foo" (getf (first tokens) :value)))))

;;; ─── Multi-token sequence ─────────────────────────────────────────────────────

(deftest lex-multiple-tokens-const-stmt
  "\"const x = 42 + y;\" produces const/ident/=/42/+/y/semi/eof in order."
  (let ((types (%js-lex-types "const x = 42 + y;")))
    (assert-true (equal '(:T-CONST :T-IDENT :T-OP :T-NUMBER :T-OP :T-IDENT :T-SEMI :T-EOF)
               types))))

;;; ─── Regex literals ─────────────────────────────────────────────────────────

(deftest lex-regex-with-flags
  "/ab+c/gi lexes to a :T-REGEX token with pattern \"ab+c\" and flags \"gi\"."
  (let ((tok (%js-first-token "/ab+c/gi")))
    (assert-eq :T-REGEX (getf tok :type))
    (let ((val (getf tok :value)))
      (assert-string= "ab+c" (second val))
      (assert-string= "gi"   (third val)))))

(deftest lex-regex-plain
  "/hello/ lexes as a regex with empty flags."
  (let ((tok (%js-first-token "/hello/")))
    (assert-eq :T-REGEX (getf tok :type))
    (assert-string= "hello" (second (getf tok :value)))))

(deftest lex-regex-char-class
  "/[a-z]+/ keeps the character class in the pattern."
  (let ((tok (%js-first-token "/[a-z]+/")))
    (assert-eq :T-REGEX (getf tok :type))
    (assert-string= "[a-z]+" (second (getf tok :value)))))

(deftest lex-regex-all-flags
  "/x/dgimsuy accepts every ES2026 flag including u (Unicode)."
  (let ((tok (%js-first-token "/x/dgimsuy")))
    (assert-eq :T-REGEX (getf tok :type))
    (assert-string= "dgimsuy" (third (getf tok :value)))))

(deftest lex-division-not-regex
  "After an identifier, / is division (:T-OP), not a regex."
  (let ((types (%js-lex-types "a / b")))
    (assert-true (member :T-OP types))
    (assert-false (member :T-REGEX types))))
