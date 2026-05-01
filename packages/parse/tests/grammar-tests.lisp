;;;; tests/unit/parse/grammar-tests.lisp — CL grammar parser unit tests
;;;;
;;;; Tests: token-stream operations, parse-cl-atom, parse-cl-form via
;;;; parse-cl-source, multi-form parsing, error recovery, and Pratt bridge.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── Helper ──────────────────────────────────────────────────────────────────

(defun make-grammar-token (type value &key (start 0) (end 1))
  "Create a lexer-token for grammar token-stream tests."
  (cl-cc/parse::make-lexer-token :type type :value value
                           :start-byte start :end-byte end
                           :line 1 :column 0))

(defun make-grammar-ts (&rest token-specs)
  "Create a token-stream from TOKEN-SPECS: each is (type value) or (type value :start s :end e)."
  (let ((tokens (mapcar (lambda (spec)
                          (apply #'make-grammar-token spec))
                        token-specs)))
    (cl-cc/parse::make-token-stream :tokens tokens :source "" :source-file nil)))

(defun parse-first-form (source)
  "Parse SOURCE and return the first CST node."
  (multiple-value-bind (forms diags)
      (cl-cc:parse-cl-source source)
    (declare (ignore diags))
    (first forms)))

(defun parse-first-kind (source)
  "Parse SOURCE and return the kind of the first CST node."
  (let ((node (parse-first-form source)))
    (when node (cl-cc:cst-node-kind node))))

(defun parse-first-value (source)
  "Parse SOURCE and return the value of the first CST token node."
  (let ((node (parse-first-form source)))
    (when (and node (cl-cc:cst-token-p node))
      (cl-cc:cst-token-value node))))

;;; ─── Token Stream: ts-peek ─────────────────────────────────────────────────

(deftest grammar-ts-peek-non-empty-stream
  "ts-peek returns the first token without consuming; repeated peek returns same token."
  (let* ((ts (make-grammar-ts '(:T-INT 42) '(:T-INT 99)))
         (tok (cl-cc/parse::ts-peek ts)))
    (assert-eq :T-INT (cl-cc/parse::lexer-token-type tok))
    (assert-= 42 (cl-cc/parse::lexer-token-value tok))
    (assert-= 42 (cl-cc/parse::lexer-token-value (cl-cc/parse::ts-peek ts)))))

(deftest grammar-ts-peek-empty-stream-returns-nil
  "ts-peek returns nil on an empty token stream."
  (let ((ts (cl-cc/parse::make-token-stream :tokens nil :source "")))
    (assert-null (cl-cc/parse::ts-peek ts))))

;;; ─── Token Stream: ts-advance ──────────────────────────────────────────────

(deftest grammar-ts-advance-consumes-token
  "ts-advance consumes and returns current token, advances to next"
  (let ((ts (make-grammar-ts '(:T-INT 1) '(:T-INT 2) '(:T-INT 3))))
    (let ((tok1 (cl-cc/parse::ts-advance ts)))
      (assert-= 1 (cl-cc/parse::lexer-token-value tok1)))
    (let ((tok2 (cl-cc/parse::ts-advance ts)))
      (assert-= 2 (cl-cc/parse::lexer-token-value tok2)))
    (let ((tok3 (cl-cc/parse::ts-advance ts)))
      (assert-= 3 (cl-cc/parse::lexer-token-value tok3)))
    ;; Stream now exhausted
    (assert-null (cl-cc/parse::ts-advance ts))))

;;; ─── Token Stream: ts-peek-type ────────────────────────────────────────────

(deftest-each grammar-ts-peek-type-cases
  "ts-peek-type: non-empty stream returns type keyword; empty stream returns nil."
  :cases (("non-empty" :non-empty :T-STRING)
          ("empty"     :empty     nil))
  (scenario expected)
  (let ((ts (ecase scenario
               (:non-empty (make-grammar-ts '(:T-STRING "hello")))
               (:empty     (cl-cc/parse::make-token-stream :tokens nil :source "")))))
    (assert-equal expected (cl-cc/parse::ts-peek-type ts))))

;;; ─── Token Stream: ts-expect ───────────────────────────────────────────────

(deftest grammar-ts-expect-behavior
  "ts-expect: matching consumes token; mismatch and end-of-input add diagnostics"
  ;; Matching type consumes and returns the token
  (let* ((ts (make-grammar-ts '(:T-INT 42)))
         (tok (cl-cc/parse::ts-expect ts :T-INT)))
    (assert-true (not (null tok)))
    (assert-= 42 (cl-cc/parse::lexer-token-value tok))
    ;; Stream is empty after consuming the token
    (assert-null (cl-cc/parse::ts-peek ts)))
  ;; Mismatch adds a diagnostic and returns nil without consuming
  (let* ((ts (make-grammar-ts '(:T-INT 42)))
         (result (cl-cc/parse::ts-expect ts :T-STRING)))
    (assert-null result)
    (assert-true (not (null (cl-cc/parse::token-stream-diagnostics ts))))
    ;; Token was NOT consumed
    (assert-= 42 (cl-cc/parse::lexer-token-value (cl-cc/parse::ts-peek ts))))
  ;; At end of input adds a diagnostic
  (let* ((ts (cl-cc/parse::make-token-stream :tokens nil :source ""))
         (result (cl-cc/parse::ts-expect ts :T-RPAREN)))
    (assert-null result)
    (assert-true (not (null (cl-cc/parse::token-stream-diagnostics ts))))))

;;; ─── Token Stream: ts-at-end-p ─────────────────────────────────────────────

(deftest-each grammar-ts-at-end-p-cases
  "ts-at-end-p: empty list and :T-EOF return true; non-empty returns false"
  :cases (("empty"     :empty     t)
          ("non-empty" :non-empty nil)
          ("eof-token" :eof       t))
  (scenario expected)
  (let ((ts (ecase scenario
               (:empty     (cl-cc/parse::make-token-stream :tokens nil :source ""))
               (:non-empty (make-grammar-ts '(:T-INT 42)))
               (:eof       (make-grammar-ts '(:T-EOF nil))))))
    (if expected
        (assert-true  (cl-cc/parse::ts-at-end-p ts))
        (assert-false (cl-cc/parse::ts-at-end-p ts)))))

;;; ─── Token Stream: ts-token-value ──────────────────────────────────────────

(deftest-each grammar-ts-token-value-cases
  "ts-token-value: non-empty stream returns token value; empty stream returns nil."
  :cases (("non-empty" :non-empty 42)
          ("empty"     :empty     nil))
  (scenario expected)
  (let ((ts (ecase scenario
               (:non-empty (make-grammar-ts '(:T-INT 42)))
               (:empty     (cl-cc/parse::make-token-stream :tokens nil :source "")))))
    (assert-equal expected (cl-cc/parse::ts-token-value ts))))

;;; ─── parse-cl-atom ─────────────────────────────────────────────────────────

(deftest-each grammar-parse-cl-atom-value-types
  "parse-cl-atom produces cst-token nodes with correct values for integer, string, and keyword."
  :cases (("integer" :T-INT     42        42)
          ("string"  :T-STRING  "hello"   "hello")
          ("keyword" :T-KEYWORD :foo      :foo))
  (tok-type tok-value expected)
  (let* ((ts (make-grammar-ts (list tok-type tok-value)))
         (node (cl-cc/parse::parse-cl-atom ts)))
    (assert-true (cl-cc:cst-token-p node))
    (assert-equal expected (cl-cc:cst-token-value node))))

(deftest-each grammar-parse-cl-atom-nil-cases
  "parse-cl-atom returns nil for a non-atom token and for an empty stream."
  :cases (("non-atom" :non-atom)
          ("empty"    :empty))
  (scenario)
  (let* ((ts (ecase scenario
               (:non-atom (make-grammar-ts (list :T-LPAREN nil)))
               (:empty    (cl-cc/parse::make-token-stream :tokens nil :source ""))))
         (node (cl-cc/parse::parse-cl-atom ts)))
    (assert-null node)))

;;; ─── parse-cl-source: atoms ────────────────────────────────────────────────

(deftest-each grammar-parse-atomic-tokens
  "parse-cl-source: integer and string literals produce cst-token nodes with correct values."
  :cases (("integer" "42"       42)
          ("string"  "\"hello\"" "hello"))
  (source expected-value)
  (let ((node (parse-first-form source)))
    (assert-true (cl-cc:cst-token-p node))
    (assert-equal expected-value (cl-cc:cst-token-value node))))

;;; ─── parse-cl-source: quote sugar ──────────────────────────────────────────

(deftest-each grammar-quote-sugar-forms
  "parse-cl-source: quote sugar and vector reader macros produce correct interior CST nodes"
  :cases (("quote"      "'x"       :quote       1)
          ("backquote"  "`x"       :quasiquote  1)
          ("unquote"    ",x"       :unquote     1)
          ("function"   "#'foo"    :function    1)
          ("vector"     "#(1 2 3)" :vector      3))
  (source expected-kind expected-children)
  (let ((node (parse-first-form source)))
    (assert-true (cl-cc:cst-interior-p node))
    (assert-eq expected-kind (cl-cc:cst-node-kind node))
    (assert-= expected-children (length (cl-cc:cst-interior-children node)))))

;;; ─── parse-cl-source: list forms ───────────────────────────────────────────

(deftest-each grammar-list-forms
  "parse-cl-source: empty list, simple list, and dotted list produce correct CST interior nodes"
  :cases (("empty-list"   "()"      :list        0)
          ("simple-list"  "(a b c)" nil          3)
          ("dotted-list"  "(a . b)" :dotted-list 2))
  (source expected-kind expected-children)
  (let ((node (parse-first-form source)))
    (assert-true (cl-cc:cst-interior-p node))
    (when expected-kind
      (assert-eq expected-kind (cl-cc:cst-node-kind node)))
    (assert-= expected-children (length (cl-cc:cst-interior-children node)))))

;;; ─── parse-cl-source: multi-form ───────────────────────────────────────────

(deftest-each grammar-multi-form-cases
  "parse-cl-source: three atoms, defun+call, and empty input return correct form counts"
  :cases (("three-atoms"    "1 2 3"                3   nil)
          ("defun-and-call" "(defun f () 1) (f)"   2   :defun)
          ("empty-input"    ""                     0   nil))
  (source expected-count expected-first-kind)
  (multiple-value-bind (forms diags)
      (cl-cc:parse-cl-source source)
    (declare (ignore diags))
    (assert-= expected-count (length forms))
    (when expected-first-kind
      (assert-eq expected-first-kind (cl-cc:cst-node-kind (first forms))))))

;;; ─── Error Recovery ────────────────────────────────────────────────────────

(deftest grammar-error-recovery-behavior
  "parse-cl-source: missing close paren adds diagnostic; diagnostics are a list; valid input has none"
  ;; Missing close paren adds a diagnostic
  (multiple-value-bind (forms diags)
      (cl-cc:parse-cl-source "(a b c")
    (declare (ignore forms))
    (assert-true (> (length diags) 0)))
  ;; Diagnostics are returned as a list (second value)
  (multiple-value-bind (forms diags)
      (cl-cc:parse-cl-source "(a b")
    (declare (ignore forms))
    (assert-true (listp diags))
    (assert-true (> (length diags) 0)))
  ;; Valid input returns empty diagnostics
  (multiple-value-bind (forms diags)
      (cl-cc:parse-cl-source "(+ 1 2)")
    (declare (ignore forms))
    (assert-= 0 (length diags))))

;;; ─── CST byte positions ────────────────────────────────────────────────────

(deftest-each grammar-byte-positions-cases
  "parse-cl-source: CST nodes carry correct start/end byte offsets"
  :cases (("atom" "  42"    2 4)
          ("list" "(+ 1 2)" 0 7))
  (source expected-start expected-end)
  (let ((node (parse-first-form source)))
    (assert-= expected-start (cl-cc:cst-node-start-byte node))
    (assert-= expected-end   (cl-cc:cst-node-end-byte   node))))
