;;;; tests/unit/parse/pratt-tests.lisp — Pratt parser engine unit tests
;;;;
;;;; Tests: pratt-context stream ops, NUD dispatch, CL grammar round-trips,
;;;; parse-cl-source CST structure, parse-all-forms s-expression output,
;;;; error recovery via cst-error-node, and parse-and-lower pipeline.

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── Helpers ─────────────────────────────────────────────────────────────────

(defun make-test-ctx (source)
  "Create a generic pratt-context from SOURCE string for parser-engine tests."
  (let ((tokens (cl-cc:lex-all source)))
    (cl-cc/parse::make-pratt-context
     :tokens tokens
     :source source
     :source-file nil
     :tok-type-fn #'cl-cc:lexer-token-type
     :tok-value-fn #'cl-cc:lexer-token-value
     :tok-start-fn #'cl-cc:lexer-token-start-byte
     :tok-end-fn #'cl-cc:lexer-token-end-byte
     :nud-table (make-hash-table :test #'eq)
     :led-table (make-hash-table :test #'eq))))

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
    (let ((tok (cl-cc/parse::pratt-peek ctx)))
      (assert-true (not (null tok)))
      (assert-eq :T-INT (cl-cc:lexer-token-type tok))))
  (let ((ctx (make-test-ctx "42")))
    (let ((tok1 (cl-cc/parse::pratt-peek ctx))
          (tok2 (cl-cc/parse::pratt-peek ctx)))
      (assert-eq tok1 tok2))))

(deftest pratt-advance-consumes-token
  "pratt-advance returns current token and moves forward"
  (let ((ctx (make-test-ctx "1 2")))
    (let ((tok1 (cl-cc/parse::pratt-advance ctx)))
      (assert-eq :T-INT (cl-cc:lexer-token-type tok1))
      (assert-= 1 (cl-cc:lexer-token-value tok1)))
    (let ((tok2 (cl-cc/parse::pratt-advance ctx)))
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
      (cl-cc/parse::pratt-advance ctx))
    (if expected
        (assert-true (cl-cc/parse::pratt-at-end-p ctx))
        (assert-false (cl-cc/parse::pratt-at-end-p ctx)))))

;;; ─── Token Accessors ─────────────────────────────────────────────────────────

(deftest pratt-tok-type-via-context
  "pratt-tok-type returns type using context's accessor function"
  (let* ((ctx (make-test-ctx "hello"))
         (tok (cl-cc/parse::pratt-peek ctx)))
    (assert-eq :T-IDENT (cl-cc/parse::pratt-tok-type ctx tok))))

(deftest pratt-tok-value-via-context
  "pratt-tok-value returns value using context's accessor function"
  (let* ((ctx (make-test-ctx "hello"))
         (tok (cl-cc/parse::pratt-peek ctx)))
    (assert-string= "HELLO" (string (cl-cc/parse::pratt-tok-value ctx tok)))))

(deftest pratt-tok-type-nil-safe
  "pratt-tok-type returns nil when tok is nil"
  (let ((ctx (make-test-ctx "")))
    (assert-eq nil (cl-cc/parse::pratt-tok-type ctx nil))))

;;; ─── Diagnostics: pratt-expect ───────────────────────────────────────────────

(deftest pratt-expect-matching-type
  "pratt-expect consumes token when type matches"
  (let ((ctx (make-test-ctx "42")))
    (let ((tok (cl-cc/parse::pratt-expect ctx :T-INT "test")))
      (assert-true (not (null tok)))
      (assert-eq :T-INT (cl-cc:lexer-token-type tok)))))

(deftest-each pratt-expect-adds-diagnostic-on-failure
  "pratt-expect adds a diagnostic when the expected token type doesn't match or input is empty."
  :cases (("type-mismatch" "42" :T-STRING "test")
          ("eof"           ""   :T-INT    "eof-test"))
  (source expected-type label)
  (let ((ctx (make-test-ctx source)))
    (cl-cc/parse::pratt-expect ctx expected-type label)
    (assert-true (not (null (cl-cc/parse::pratt-context-diagnostics ctx))))))

(deftest pratt-expect-mismatch-returns-nil
  "pratt-expect returns nil on type mismatch"
  (let ((ctx (make-test-ctx "42")))
    (let ((result (cl-cc/parse::pratt-expect ctx :T-STRING)))
      (assert-eq nil result))))

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
