;;;; tests/unit/parse/combinator-tests.lisp — Parser Combinator Engine Tests
;;;;
;;;; Tests for the grammar-driven parser combinator engine: token stream
;;;; protocol, grammar rule database, and all combinator primitives.

(in-package :cl-cc/test)

(defsuite combinator-suite :description "Parser combinator engine tests"
  :parent cl-cc-unit-suite)


(in-suite combinator-suite)
;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun make-tok (type value)
  "Create a plist token."
  (list :type type :value value))

(defun toks (&rest specs)
  "Build a token stream from (type value) pairs.
   Example: (toks :T-INT 1 :T-PLUS \"+\" :T-INT 2)"
  (loop for (type val) on specs by #'cddr
        collect (make-tok type val)))

;;; ─── Token Stream Protocol ──────────────────────────────────────────────────

(deftest-each comb-stream-empty-p-behavior
  "stream-empty-p: true for nil; false for non-empty stream."
  :cases (("nil-stream"      nil          t)
          ("non-empty-stream" (toks :T-INT 1) nil))
  (stream expected)
  (if expected
      (assert-true  (cl-cc/parse::stream-empty-p stream))
      (assert-false (cl-cc/parse::stream-empty-p stream))))

(deftest comb-stream-peek
  "Peek returns first token without consuming."
  (let ((s (toks :T-INT 42)))
    (let ((tok (cl-cc/parse::stream-peek s)))
      (assert-equal :T-INT (cl-cc/parse::tok-type tok))
      (assert-equal 42 (cl-cc/parse::tok-value tok))
      ;; Stream unchanged
      (assert-equal 1 (length s)))))

(deftest comb-stream-consume
  "Consume returns token and rest of stream."
  (let ((s (toks :T-INT 1 :T-INT 2)))
    (multiple-value-bind (tok rest) (cl-cc/parse::stream-consume s)
      (assert-equal 1 (cl-cc/parse::tok-value tok))
      (assert-equal 1 (length rest)))))

(deftest comb-stream-consume-empty
  "Consume on empty stream returns nil, nil."
  (multiple-value-bind (tok rest) (cl-cc/parse::stream-consume nil)
    (assert-null tok)
    (assert-null rest)))

;;; ─── Grammar Rule Database ──────────────────────────────────────────────────

(deftest comb-grammar-rule-lifecycle
  "Grammar rule database: store-and-query, missing returns nil, clear empties DB."
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    (setf (gethash :test-rule cl-cc/parse:*grammar-rules*) '(token :T-INT))
    (let ((rule (cl-cc/parse:query-grammar :test-rule)))
      (assert-true (consp rule))
      (assert-equal 'token (first rule))))
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    (assert-null (cl-cc/parse:query-grammar :nonexistent)))
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    (setf (gethash :foo cl-cc/parse:*grammar-rules*) '(token :T-INT))
    (cl-cc/parse:clear-grammar-rules)
    (assert-null (cl-cc/parse:query-grammar :foo))))

;;; ─── parse-ok-p ─────────────────────────────────────────────────────────────

(deftest-each comb-parse-ok-p-behavior
  "parse-ok-p: true for non-:fail values (integer, nil, list); false for :fail."
  :cases (("integer" 42       t)
          ("nil"     nil      t)
          ("list"    '(1 2 3) t)
          ("fail"    :fail    nil))
  (value expected)
  (if expected
      (assert-true  (cl-cc/parse:parse-ok-p value))
      (assert-false (cl-cc/parse:parse-ok-p value))))

;;; ─── parse-token* ───────────────────────────────────────────────────────────

(deftest comb-token-match-type
  "parse-token* matches by type."
  (let ((s (toks :T-INT 42)))
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-token* :T-INT nil s)
      (assert-equal 42 ast)
      (assert-null rest))))

(deftest comb-token-match-type-and-value
  "parse-token* matches by type and specific value."
  (let ((s (toks :T-IDENT "foo")))
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-token* :T-IDENT "foo" s)
      (assert-equal "foo" ast)
      (assert-null rest))))

(deftest-each comb-token-mismatch-cases
  "parse-token* returns :fail on type mismatch, value mismatch, and empty stream."
  :cases (("type-mismatch"  (toks :T-INT 1)      :T-IDENT nil)
          ("value-mismatch" (toks :T-IDENT "bar") :T-IDENT "foo")
          ("empty-stream"   nil                    :T-INT   nil))
  (stream tok-type tok-val)
  (multiple-value-bind (ast rest) (cl-cc/parse::parse-token* tok-type tok-val stream)
    (declare (ignore rest))
    (assert-equal :fail ast)))

;;; ─── parse-literal* ─────────────────────────────────────────────────────────

(deftest comb-literal-match
  "parse-literal* matches :T-IDENT with exact string."
  (let ((s (toks :T-IDENT "if")))
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-literal* "if" s)
      (assert-equal "if" ast)
      (assert-null rest))))

(deftest comb-literal-mismatch
  "parse-literal* fails on wrong string."
  (let ((s (toks :T-IDENT "else")))
    (multiple-value-bind (ast _rest) (cl-cc/parse::parse-literal* "if" s)
      (declare (ignore _rest))
      (assert-equal :fail ast))))

;;; ─── parse-seq* ─────────────────────────────────────────────────────────────

(deftest comb-seq-all-match
  "parse-seq* succeeds when all sub-expressions match."
  (let ((s (toks :T-INT 1 :T-IDENT "+" :T-INT 2)))
    (multiple-value-bind (ast rest)
        (cl-cc/parse::parse-seq* '((token :T-INT) (token :T-IDENT) (token :T-INT)) s)
      (assert-equal '(1 "+" 2) ast)
      (assert-null rest))))

(deftest comb-seq-partial-fail
  "parse-seq* fails if any sub-expression fails (backtracks)."
  (let ((s (toks :T-INT 1 :T-INT 2)))
    (multiple-value-bind (ast rest)
        (cl-cc/parse::parse-seq* '((token :T-INT) (token :T-IDENT)) s)
      (assert-equal :fail ast)
      (assert-null rest))))

(deftest comb-seq-empty
  "parse-seq* with no expressions returns empty list."
  (let ((s (toks :T-INT 1)))
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-seq* nil s)
      (assert-equal nil ast)
      (assert-equal 1 (length rest)))))

;;; ─── parse-alt* ─────────────────────────────────────────────────────────────

(deftest-each comb-alt-matching-behavior
  "parse-alt*: first-alternative match, second-alternative match, no-match (fail)."
  :cases (("first-matches"  (toks :T-INT 42)    '((token :T-INT) (token :T-IDENT))  42    t)
          ("second-matches" (toks :T-IDENT "x") '((token :T-INT) (token :T-IDENT)) "x"   t)
          ("none-match"     (toks :T-IDENT "x") '((token :T-INT) (token :T-PLUS))   :fail nil))
  (tokens alts expected-ast success-p)
  (multiple-value-bind (ast rest) (cl-cc/parse::parse-alt* alts tokens)
    (assert-equal expected-ast ast)
    (when success-p
      (assert-null rest))))

;;; ─── parse-many* ────────────────────────────────────────────────────────────

(deftest-each comb-many-behavior
  "parse-many*: zero matches leaves stream intact; all-INT collects list; stops at mismatch."
  :cases (("zero"    (toks :T-IDENT "x")                    nil      1)
          ("all"     (toks :T-INT 1 :T-INT 2 :T-INT 3)      '(1 2 3) 0)
          ("partial" (toks :T-INT 1 :T-INT 2 :T-IDENT "x")  '(1 2)   1))
  (tokens expected-ast expected-rest-len)
  (multiple-value-bind (ast rest) (cl-cc/parse::parse-many* '(token :T-INT) tokens)
    (assert-equal expected-ast ast)
    (assert-= expected-rest-len (length rest))))

;;; ─── parse-many1* ───────────────────────────────────────────────────────────

(deftest-each comb-many1-behavior
  "parse-many1*: one match, multiple matches, and zero matches (fail)."
  :cases (("one"      (toks :T-INT 1 :T-IDENT "x")  '(1)   1   t)
          ("multiple" (toks :T-INT 1 :T-INT 2)        '(1 2) 0   t)
          ("zero"     (toks :T-IDENT "x")             :fail  nil nil))
  (tokens expected-ast expected-rest-len success-p)
  (multiple-value-bind (ast rest) (cl-cc/parse::parse-many1* '(token :T-INT) tokens)
    (if success-p
        (progn
          (assert-equal expected-ast ast)
          (assert-= expected-rest-len (length rest)))
        (assert-equal :fail ast))))

