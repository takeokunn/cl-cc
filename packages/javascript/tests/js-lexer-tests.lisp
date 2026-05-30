;;;; packages/javascript/tests/js-lexer-tests.lisp — ES2026 JavaScript Lexer Tests
;;;;
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

;;; ─── Numeric literals ─────────────────────────────────────────────────────────

(deftest-each lex-integer-literal
  "Integer literals in decimal, hex, octal, binary, and with numeric separators
   all lex to :T-NUMBER with the expected integer value."
  :cases (("decimal-42"    "42"        42)
          ("decimal-0"     "0"         0)
          ("decimal-255"   "255"       255)
          ("hex-0xff"      "0xFF"      255)
          ("octal-0o777"   "0o777"     511)
          ("binary-0b1010" "0b1010"    10)
          ("separator"     "1_000_000" 1000000))
  (src expected)
  (assert-eq :T-NUMBER (%js-first-type src))
  (assert-= expected (%js-first-value src)))

(deftest lex-float-pi
  "3.14 lexes to :T-NUMBER with a double-float value close to pi."
  (assert-eq :T-NUMBER (%js-first-type "3.14"))
  (assert-true (typep (%js-first-value "3.14") 'double-float))
  (assert-true (< (abs (- 3.14d0 (%js-first-value "3.14"))) 1.0d-10)))

(deftest lex-float-scientific
  "1.5e-3 lexes to :T-NUMBER within 1e-15 of the expected double value."
  (assert-eq :T-NUMBER (%js-first-type "1.5e-3"))
  (assert-true (< (abs (- 1.5d-3 (%js-first-value "1.5e-3"))) 1.0d-15)))

(deftest lex-bigint-literal
  "42n lexes to :T-BIGINT with integer value 42."
  (assert-eq :T-BIGINT (%js-first-type "42n"))
  (assert-= 42 (%js-first-value "42n")))

;;; ─── String literals ──────────────────────────────────────────────────────────

(deftest-each lex-string-literal
  "Single and double quoted string literals lex to :T-STRING."
  :cases (("single" "'hello'" "hello")
          ("double" "\"world\"" "world"))
  (src expected)
  (assert-eq :T-STRING (%js-first-type src))
  (assert-string= expected (%js-first-value src)))

;;; ─── Keyword tokens ───────────────────────────────────────────────────────────

(deftest-each lex-keyword
  "JavaScript keywords each lex to their canonical token type."
  :cases (("if"        "if"        :T-IF)
          ("for"       "for"       :T-FOR)
          ("class"     "class"     :T-CLASS)
          ("async"     "async"     :T-ASYNC)
          ("await"     "await"     :T-AWAIT)
          ("using"     "using"     :T-USING)
          ("true"      "true"      :T-TRUE)
          ("false"     "false"     :T-FALSE)
          ("null"      "null"      :T-NULL)
          ("undefined" "undefined" :T-UNDEFINED))
  (src expected-type)
  (assert-eq expected-type (%js-first-type src)))

;;; ─── Identifiers ──────────────────────────────────────────────────────────────

(deftest-each lex-identifier
  "Valid JS identifiers lex to :T-IDENT with their source text as the value."
  :cases (("simple"     "foo"       "foo")
          ("underscore" "_bar"      "_bar")
          ("dollar"     "$baz"      "$baz")
          ("camel"      "camelCase" "camelCase"))
  (src expected)
  (assert-eq :T-IDENT (%js-first-type src))
  (assert-string= expected (%js-first-value src)))

;;; ─── Private identifiers ──────────────────────────────────────────────────────

(deftest-each lex-private-identifier
  "Private identifiers (#name) lex to :T-PRIVATE-IDENT; the # is stripped from value."
  :cases (("field"  "#field"         "field")
          ("method" "#privateMethod" "privateMethod"))
  (src expected)
  (assert-eq :T-PRIVATE-IDENT (%js-first-type src))
  (assert-string= expected (%js-first-value src)))

;;; ─── Decorator ────────────────────────────────────────────────────────────────

(deftest lex-decorator-at
  "@decorator opens with :T-AT followed by :T-IDENT."
  (let ((types (%js-lex-types "@decorator")))
    (assert-eq :T-AT    (first  types))
    (assert-eq :T-IDENT (second types))))

;;; ─── Multi-character operators ────────────────────────────────────────────────

(deftest-each lex-operator
  "Multi-character operators lex to :T-OP with their literal text as value."
  :cases (("strict-eq"      "==="  "===")
          ("strict-neq"     "!=="  "!==")
          ("nullish"        "??"   "??")
          ("optional-chain" "?."   "?.")
          ("logic-and-asgn" "&&="  "&&=")
          ("logic-or-asgn"  "||="  "||=")
          ("nullish-asgn"   "??="  "??=")
          ("exp-asgn"       "**="  "**="))
  (src expected)
  (assert-eq :T-OP (%js-first-type src))
  (assert-string= expected (%js-first-value src)))

;;; ─── Special punctuation ──────────────────────────────────────────────────────

(deftest lex-ellipsis
  "... lexes to :T-ELLIPSIS."
  (assert-eq :T-ELLIPSIS (%js-first-type "..."))
  (assert-string= "..." (%js-first-value "...")))

(deftest lex-arrow
  "=> lexes to :T-ARROW."
  (assert-eq :T-ARROW (%js-first-type "=>"))
  (assert-string= "=>" (%js-first-value "=>")))

;;; ─── Comments ─────────────────────────────────────────────────────────────────

(deftest lex-line-comment-skipped
  "// line comment produces only :T-EOF."
  (let ((types (%js-lex-types "// this is a comment")))
    (assert-= 1 (length types))
    (assert-eq :T-EOF (first types))))

(deftest lex-block-comment-skipped
  "/* block comment */ is skipped; the identifier after it is the first real token."
  (let ((tokens (%js-lex "/* skip me */ foo")))
    (assert-eq :T-IDENT (getf (first tokens) :type))
    (assert-string= "foo" (getf (first tokens) :value))))

;;; ─── Multi-token sequence ─────────────────────────────────────────────────────

(deftest lex-const-statement-sequence
  "\"const x = 42 + y;\" yields the full expected token-type sequence."
  (assert-equal '(:T-CONST :T-IDENT :T-OP :T-NUMBER :T-OP :T-IDENT :T-SEMI :T-EOF)
                (%js-lex-types "const x = 42 + y;")))

;;; ─── Regex literals ───────────────────────────────────────────────────────────

(deftest-each lex-regex-pattern
  "Regex literals lex to :T-REGEX; value is (list :regex pattern flags)."
  :cases (("with-flags"   "/ab+c/gi"  "ab+c"  "gi")
          ("no-flags"     "/hello/"   "hello" "")
          ("char-class"   "/[a-z]+/"  "[a-z]+" "")
          ("all-flags"    "/x/dgimsuy" "x"    "dgimsuy"))
  (src expected-pattern expected-flags)
  (let* ((tok (first (%js-lex src)))
         (val (getf tok :value)))
    (assert-eq :T-REGEX (getf tok :type))
    (assert-string= expected-pattern (second val))
    (assert-string= expected-flags   (third  val))))

(deftest lex-division-not-regex
  "After an identifier, / is :T-OP (division), not :T-REGEX."
  (let ((types (%js-lex-types "a / b")))
    (assert-true  (member :T-OP    types))
    (assert-false (member :T-REGEX types))))
