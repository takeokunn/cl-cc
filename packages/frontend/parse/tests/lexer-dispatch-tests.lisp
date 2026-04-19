;;;; tests/unit/parse/lexer-dispatch-tests.lisp
;;;; Unit tests for src/parse/lexer-dispatch.lisp
;;;;
;;;; Covers:
;;;;   Skip helpers    — %lex-skip-string-chars, %lex-skip-list-body, %lex-skip-atom
;;;;                     (via lex-read-form-text and lex-skip-form)
;;;;   Feature logic   — lex-feature-present-p (pure boolean combinators)
;;;;   Hash dispatch   — #b/#o/#x radix, #' function, #\\ char, #( vector,
;;;;                     #* bit-vector, #+ / #- feature conditionals, #| block comment
;;;;   lex-read-form-text — reads balanced forms as raw strings

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Helpers ────────────────────────────────────────────────────────────────

(defun lexer-dispatch-lex-types (source)
  "Tokenize SOURCE and return list of token types (excluding :T-EOF)."
  (remove :t-eof (mapcar #'cl-cc:lexer-token-type (cl-cc:lex-all source))))

(defun lexer-dispatch-first-value (source)
  "Tokenize SOURCE and return the value of the first non-EOF token."
  (cl-cc:lexer-token-value (first (cl-cc:lex-all source))))

(defun lexer-dispatch-read-form-text (source)
  "Use internal lex-read-form-text to extract the raw form text from SOURCE."
  (let ((state (cl-cc/parse::make-lexer source)))
    (cl-cc/parse::lex-read-form-text state)))

;;; ─── lex-feature-present-p ───────────────────────────────────────────────────

(deftest-each lex-feature-present-p-keyword-lookup
  "lex-feature-present-p returns T for features in *features*, NIL otherwise."
  :cases (("present"  :sbcl   t)
          ("absent"   :no-such-feature nil))
  (feature expected)
  (let ((*features* '(:sbcl :common-lisp)))
    (assert-equal expected (if (cl-cc/parse::lex-feature-present-p feature) t nil))))

(deftest lex-feature-present-p-combinator-cases
  "lex-feature-present-p: :or any-present; :and all-present; :not negates; unknown → nil."
  (let ((*features* '(:sbcl)))
    (assert-true (cl-cc/parse::lex-feature-present-p '(:or :sbcl :ccl)))
    (assert-null (cl-cc/parse::lex-feature-present-p '(:or :ccl :ecl))))
  (let ((*features* '(:sbcl :common-lisp)))
    (assert-true (cl-cc/parse::lex-feature-present-p '(:and :sbcl :common-lisp)))
    (assert-null (cl-cc/parse::lex-feature-present-p '(:and :sbcl :ccl))))
  (let ((*features* '(:sbcl)))
    (assert-null (cl-cc/parse::lex-feature-present-p '(:not :sbcl)))
    (assert-true (cl-cc/parse::lex-feature-present-p '(:not :ccl))))
  (assert-null (cl-cc/parse::lex-feature-present-p 42))
  (assert-null (cl-cc/parse::lex-feature-present-p '(:unknown :foo))))

;;; ─── lex-read-form-text ──────────────────────────────────────────────────────

(deftest-each lex-read-form-text-cases
  "lex-read-form-text captures atoms, strings, and balanced lists verbatim."
  :cases (("integer"     "42"          "42")
          ("symbol"      "foo"         "foo")
          ("keyword"     ":bar"        ":bar")
          ("string"      "\"hello\""   "\"hello\"")
          ("list"        "(+ 1 2)"     "(+ 1 2)")
          ("nested-list" "(a (b c) d)" "(a (b c) d)"))
  (source expected)
  (assert-string= expected (lexer-dispatch-read-form-text source)))

(deftest lex-read-form-text-list-with-string
  "lex-read-form-text handles lists containing strings with parens inside."
  (let ((text (lexer-dispatch-read-form-text "(f \")\")"))  )
    ;; The ) inside the string must not close the list
    (assert-string= "(f \")\")" text)))

;;; ─── Hash Dispatch: Radix Integers ──────────────────────────────────────────

(deftest-each lexer-dispatch-radix-integers
  "Lexer correctly parses binary, octal, and hexadecimal integer literals."
  :cases (("binary"  "#b1010"  10)
          ("octal"   "#o17"    15)
          ("hex"     "#xff"    255)
          ("hex-cap" "#xFF"    255))
  (source expected)
  (assert-eq :t-int (first (lexer-dispatch-lex-types source)))
  (assert-= expected (lexer-dispatch-first-value source)))

;;; ─── Hash Dispatch: Function Reference ──────────────────────────────────────

(deftest lexer-dispatch-function-reference
  "Lexer tokenizes #'foo as :T-FUNCTION + :T-IDENT."
  (let ((types (lexer-dispatch-lex-types "#'foo")))
    (assert-equal '(:t-function :t-ident) types)))

;;; ─── Hash Dispatch: Bit Vector ───────────────────────────────────────────────

(deftest lexer-dispatch-bit-vector-cases
  "Lexer: #*101 → bit-vector with bits 1,0,1; #* → empty bit-vector."
  (let ((val (lexer-dispatch-first-value "#*101")))
    (assert-true (bit-vector-p val))
    (assert-= 3 (length val))
    (assert-= 1 (sbit val 0))
    (assert-= 0 (sbit val 1))
    (assert-= 1 (sbit val 2)))
  (let ((val (lexer-dispatch-first-value "#*")))
    (assert-true (bit-vector-p val))
    (assert-= 0 (length val))))

;;; ─── Hash Dispatch: Block Comment ───────────────────────────────────────────

(deftest lexer-dispatch-block-comment-cases
  "Lexer skips #| ... |# block comments; token after comment is returned."
  (assert-equal '(:t-int)   (lexer-dispatch-lex-types "#| this is a comment |# 42"))
  (assert-equal '(:t-ident) (lexer-dispatch-lex-types "#| comment |# symbol")))

;;; ─── Hash Dispatch: Feature Conditionals #+/# ──────────────────────────────

(deftest lexer-dispatch-feature-conditional-cases
  "#+/-: include when present; skip when absent (#+); skip when present (#-); include when absent (#-)."
  (let ((*features* '(:sbcl)))
    (assert-equal '(:t-int) (lexer-dispatch-lex-types "#+sbcl 42")))
  (let ((*features* '()))
    (assert-equal '(:t-int) (lexer-dispatch-lex-types "#+no-such-feature 42 99"))
    (assert-= 99 (lexer-dispatch-first-value "#+no-such-feature 42 99")))
  (let ((*features* '(:sbcl)))
    (assert-equal '(:t-int) (lexer-dispatch-lex-types "#-sbcl 42 99"))
    (assert-= 99 (lexer-dispatch-first-value "#-sbcl 42 99")))
  (let ((*features* '()))
    (assert-equal '(:t-int) (lexer-dispatch-lex-types "#-no-such-feature 42"))))

;;; ─── Hash Dispatch: Arbitrary Radix #nR ─────────────────────────────────────

(deftest lexer-dispatch-arbitrary-radix
  "Lexer parses #nR arbitrary-radix integers."
  (assert-= 255 (lexer-dispatch-first-value "#16rFF"))
  (assert-= 10  (lexer-dispatch-first-value "#2r1010")))

;;; ─── Hash Dispatch: Vector #( ───────────────────────────────────────────────

(deftest lexer-dispatch-vector-start
  "Lexer tokenizes #( as :T-VECTOR-START."
  (let ((types (lexer-dispatch-lex-types "#(1 2)")))
    (assert-true (member :t-vector-start types))))

;;; ─── Skip helpers via feature skip (indirect) ───────────────────────────────

;; For #- skip tests: #-feature skips the form when feature IS PRESENT.
;; Bind *features* to contain :skip-me so #-skip-me actually skips.

(deftest-each lex-skip-form-cases
  "lex-skip-form (via #-skip-me) correctly skips lists, strings, and atoms."
  :cases (("list"   "#-skip-me (+ 1 2) 99"           99)
          ("string" "#-skip-me \"hello (world)\" 42"  42)
          ("atom"   "#-skip-me some-symbol 77"        77))
  (source expected)
  (let ((*features* '(:skip-me)))
    (assert-= expected (lexer-dispatch-first-value source))))

(deftest lex-skip-list-with-comment
  "lex-skip-form handles lists containing ; line comments (body paren after comment)."
  (let ((*features* '(:skip-me)))
    ;; The ; comment inside the list must not prevent the closing ) from being found
    (let ((val (lexer-dispatch-first-value
                "#-skip-me (foo ; comment
                 bar) 55")))
      (assert-= 55 val))))
