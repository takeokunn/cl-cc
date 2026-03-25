;;;; tests/unit/parse/combinator-tests.lisp — Parser Combinator Engine Tests
;;;;
;;;; Tests for the grammar-driven parser combinator engine: token stream
;;;; protocol, grammar rule database, and all combinator primitives.

(in-package :cl-cc/test)

(defsuite combinator-suite :description "Parser combinator engine tests")

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

(deftest comb-stream-empty-p-nil
  "Empty stream is empty."
  (assert-true (cl-cc::stream-empty-p nil)))

(deftest comb-stream-empty-p-nonempty
  "Non-empty stream is not empty."
  (assert-false (cl-cc::stream-empty-p (toks :T-INT 1))))

(deftest comb-stream-peek
  "Peek returns first token without consuming."
  (let ((s (toks :T-INT 42)))
    (let ((tok (cl-cc::stream-peek s)))
      (assert-equal :T-INT (cl-cc::tok-type tok))
      (assert-equal 42 (cl-cc::tok-value tok))
      ;; Stream unchanged
      (assert-equal 1 (length s)))))

(deftest comb-stream-consume
  "Consume returns token and rest of stream."
  (let ((s (toks :T-INT 1 :T-INT 2)))
    (multiple-value-bind (tok rest) (cl-cc::stream-consume s)
      (assert-equal 1 (cl-cc::tok-value tok))
      (assert-equal 1 (length rest)))))

(deftest comb-stream-consume-empty
  "Consume on empty stream returns nil, nil."
  (multiple-value-bind (tok rest) (cl-cc::stream-consume nil)
    (assert-null tok)
    (assert-null rest)))

;;; ─── Grammar Rule Database ──────────────────────────────────────────────────

(deftest comb-grammar-rule-store-and-query
  "def-grammar-rule stores, query-grammar retrieves."
  (let ((cl-cc::*grammar-rules* (make-hash-table)))
    (setf (gethash :test-rule cl-cc::*grammar-rules*) '(token :T-INT))
    (let ((rule (cl-cc::query-grammar :test-rule)))
      (assert-true (consp rule))
      (assert-equal 'token (first rule)))))

(deftest comb-query-grammar-missing
  "query-grammar returns nil for undefined rule."
  (let ((cl-cc::*grammar-rules* (make-hash-table)))
    (assert-null (cl-cc::query-grammar :nonexistent))))

(deftest comb-clear-grammar-rules
  "clear-grammar-rules empties the database."
  (let ((cl-cc::*grammar-rules* (make-hash-table)))
    (setf (gethash :foo cl-cc::*grammar-rules*) '(token :T-INT))
    (cl-cc::clear-grammar-rules)
    (assert-null (cl-cc::query-grammar :foo))))

;;; ─── parse-ok-p ─────────────────────────────────────────────────────────────

(deftest comb-parse-ok-p-success
  "parse-ok-p returns true for non-:fail values."
  (assert-true (cl-cc::parse-ok-p 42))
  (assert-true (cl-cc::parse-ok-p nil))
  (assert-true (cl-cc::parse-ok-p '(1 2 3))))

(deftest comb-parse-ok-p-fail
  "parse-ok-p returns false for :fail."
  (assert-false (cl-cc::parse-ok-p :fail)))

;;; ─── parse-token* ───────────────────────────────────────────────────────────

(deftest comb-token-match-type
  "parse-token* matches by type."
  (let ((s (toks :T-INT 42)))
    (multiple-value-bind (ast rest) (cl-cc::parse-token* :T-INT nil s)
      (assert-equal 42 ast)
      (assert-null rest))))

(deftest comb-token-match-type-and-value
  "parse-token* matches by type and specific value."
  (let ((s (toks :T-IDENT "foo")))
    (multiple-value-bind (ast rest) (cl-cc::parse-token* :T-IDENT "foo" s)
      (assert-equal "foo" ast)
      (assert-null rest))))

(deftest comb-token-mismatch-type
  "parse-token* fails on type mismatch."
  (let ((s (toks :T-INT 1)))
    (multiple-value-bind (ast rest) (cl-cc::parse-token* :T-IDENT nil s)
      (assert-equal :fail ast)
      (assert-null rest))))

(deftest comb-token-mismatch-value
  "parse-token* fails on value mismatch."
  (let ((s (toks :T-IDENT "bar")))
    (multiple-value-bind (ast rest) (cl-cc::parse-token* :T-IDENT "foo" s)
      (assert-equal :fail ast))))

(deftest comb-token-empty-stream
  "parse-token* fails on empty stream."
  (multiple-value-bind (ast rest) (cl-cc::parse-token* :T-INT nil nil)
    (assert-equal :fail ast)
    (assert-null rest)))

;;; ─── parse-literal* ─────────────────────────────────────────────────────────

(deftest comb-literal-match
  "parse-literal* matches :T-IDENT with exact string."
  (let ((s (toks :T-IDENT "if")))
    (multiple-value-bind (ast rest) (cl-cc::parse-literal* "if" s)
      (assert-equal "if" ast)
      (assert-null rest))))

(deftest comb-literal-mismatch
  "parse-literal* fails on wrong string."
  (let ((s (toks :T-IDENT "else")))
    (multiple-value-bind (ast _rest) (cl-cc::parse-literal* "if" s)
      (declare (ignore _rest))
      (assert-equal :fail ast))))

;;; ─── parse-seq* ─────────────────────────────────────────────────────────────

(deftest comb-seq-all-match
  "parse-seq* succeeds when all sub-expressions match."
  (let ((s (toks :T-INT 1 :T-IDENT "+" :T-INT 2)))
    (multiple-value-bind (ast rest)
        (cl-cc::parse-seq* '((token :T-INT) (token :T-IDENT) (token :T-INT)) s)
      (assert-equal '(1 "+" 2) ast)
      (assert-null rest))))

(deftest comb-seq-partial-fail
  "parse-seq* fails if any sub-expression fails (backtracks)."
  (let ((s (toks :T-INT 1 :T-INT 2)))
    (multiple-value-bind (ast rest)
        (cl-cc::parse-seq* '((token :T-INT) (token :T-IDENT)) s)
      (assert-equal :fail ast)
      (assert-null rest))))

(deftest comb-seq-empty
  "parse-seq* with no expressions returns empty list."
  (let ((s (toks :T-INT 1)))
    (multiple-value-bind (ast rest) (cl-cc::parse-seq* nil s)
      (assert-equal nil ast)
      (assert-equal 1 (length rest)))))

;;; ─── parse-alt* ─────────────────────────────────────────────────────────────

(deftest comb-alt-first-matches
  "parse-alt* returns first matching alternative."
  (let ((s (toks :T-INT 42)))
    (multiple-value-bind (ast rest)
        (cl-cc::parse-alt* '((token :T-INT) (token :T-IDENT)) s)
      (assert-equal 42 ast)
      (assert-null rest))))

(deftest comb-alt-second-matches
  "parse-alt* falls through to second alternative."
  (let ((s (toks :T-IDENT "x")))
    (multiple-value-bind (ast rest)
        (cl-cc::parse-alt* '((token :T-INT) (token :T-IDENT)) s)
      (assert-equal "x" ast)
      (assert-null rest))))

(deftest comb-alt-none-match
  "parse-alt* fails if no alternative matches."
  (let ((s (toks :T-IDENT "x")))
    (multiple-value-bind (ast _rest)
        (cl-cc::parse-alt* '((token :T-INT) (token :T-PLUS)) s)
      (declare (ignore _rest))
      (assert-equal :fail ast))))

;;; ─── parse-many* ────────────────────────────────────────────────────────────

(deftest comb-many-zero
  "parse-many* succeeds with zero matches."
  (let ((s (toks :T-IDENT "x")))
    (multiple-value-bind (ast rest) (cl-cc::parse-many* '(token :T-INT) s)
      (assert-equal nil ast)
      (assert-equal 1 (length rest)))))

(deftest comb-many-multiple
  "parse-many* collects all matches."
  (let ((s (toks :T-INT 1 :T-INT 2 :T-INT 3)))
    (multiple-value-bind (ast rest) (cl-cc::parse-many* '(token :T-INT) s)
      (assert-equal '(1 2 3) ast)
      (assert-null rest))))

(deftest comb-many-stops-at-mismatch
  "parse-many* stops at first non-match, leaving rest."
  (let ((s (toks :T-INT 1 :T-INT 2 :T-IDENT "x")))
    (multiple-value-bind (ast rest) (cl-cc::parse-many* '(token :T-INT) s)
      (assert-equal '(1 2) ast)
      (assert-equal 1 (length rest)))))

;;; ─── parse-many1* ───────────────────────────────────────────────────────────

(deftest comb-many1-one
  "parse-many1* succeeds with one match."
  (let ((s (toks :T-INT 1 :T-IDENT "x")))
    (multiple-value-bind (ast rest) (cl-cc::parse-many1* '(token :T-INT) s)
      (assert-equal '(1) ast)
      (assert-equal 1 (length rest)))))

(deftest comb-many1-multiple
  "parse-many1* collects multiple matches."
  (let ((s (toks :T-INT 1 :T-INT 2)))
    (multiple-value-bind (ast rest) (cl-cc::parse-many1* '(token :T-INT) s)
      (assert-equal '(1 2) ast)
      (assert-null rest))))

(deftest comb-many1-zero-fails
  "parse-many1* fails with zero matches."
  (let ((s (toks :T-IDENT "x")))
    (multiple-value-bind (ast _rest) (cl-cc::parse-many1* '(token :T-INT) s)
      (declare (ignore _rest))
      (assert-equal :fail ast))))

;;; ─── parse-opt* ─────────────────────────────────────────────────────────────

(deftest comb-opt-match
  "parse-opt* returns matched value on success."
  (let ((s (toks :T-INT 42)))
    (multiple-value-bind (ast rest) (cl-cc::parse-opt* '(token :T-INT) s)
      (assert-equal 42 ast)
      (assert-null rest))))

(deftest comb-opt-absent
  "parse-opt* returns :opt-absent on no match (not :fail)."
  (let ((s (toks :T-IDENT "x")))
    (multiple-value-bind (ast rest) (cl-cc::parse-opt* '(token :T-INT) s)
      (assert-equal :opt-absent ast)
      (assert-equal 1 (length rest)))))

;;; ─── parse-rule ─────────────────────────────────────────────────────────────

(deftest comb-parse-rule-defined
  "parse-rule looks up and applies a named grammar rule."
  (let ((cl-cc::*grammar-rules* (make-hash-table)))
    (setf (gethash :my-int cl-cc::*grammar-rules*) '(token :T-INT))
    (let ((s (toks :T-INT 99)))
      (multiple-value-bind (ast rest) (cl-cc::parse-rule :my-int s)
        (assert-equal 99 ast)
        (assert-null rest)))))

(deftest comb-parse-rule-undefined-error
  "parse-rule errors on undefined rule."
  (let ((cl-cc::*grammar-rules* (make-hash-table)))
    (assert-signals error (cl-cc::parse-rule :no-such-rule nil))))

;;; ─── parse-combinator (dispatcher) ─────────────────────────────────────────

(deftest comb-dispatch-keyword-shorthand
  "Bare keyword dispatches to parse-rule."
  (let ((cl-cc::*grammar-rules* (make-hash-table)))
    (setf (gethash :num cl-cc::*grammar-rules*) '(token :T-INT))
    (let ((s (toks :T-INT 7)))
      (multiple-value-bind (ast rest) (cl-cc::parse-combinator :num s)
        (assert-equal 7 ast)
        (assert-null rest)))))

(deftest comb-dispatch-token
  "parse-combinator dispatches (token ...) form."
  (let ((s (toks :T-INT 5)))
    (multiple-value-bind (ast rest) (cl-cc::parse-combinator '(token :T-INT) s)
      (assert-equal 5 ast)
      (assert-null rest))))

(deftest comb-dispatch-unknown-operator-error
  "parse-combinator errors on unknown operator."
  (assert-signals error (cl-cc::parse-combinator '(bogus :T-INT) nil)))

(deftest comb-dispatch-non-list-non-keyword-error
  "parse-combinator errors on invalid expression."
  (assert-signals error (cl-cc::parse-combinator 42 nil)))

;;; ─── parse-with-grammar (top-level) ────────────────────────────────────────

(deftest comb-parse-with-grammar-success
  "parse-with-grammar returns AST on successful parse."
  (let ((cl-cc::*grammar-rules* (make-hash-table)))
    (setf (gethash :expr cl-cc::*grammar-rules*) '(token :T-INT))
    (let ((s (toks :T-INT 100)))
      (multiple-value-bind (ast rest) (cl-cc::parse-with-grammar :expr s)
        (assert-equal 100 ast)
        (assert-null rest)))))

(deftest comb-parse-with-grammar-failure-error
  "parse-with-grammar signals error on parse failure."
  (let ((cl-cc::*grammar-rules* (make-hash-table)))
    (setf (gethash :expr cl-cc::*grammar-rules*) '(token :T-INT))
    (let ((s (toks :T-IDENT "x")))
      (assert-signals error (cl-cc::parse-with-grammar :expr s)))))

;;; ─── Integration: multi-rule grammar ────────────────────────────────────────

(deftest comb-integration-arithmetic
  "Multi-rule grammar parses simple arithmetic token sequence."
  (let ((cl-cc::*grammar-rules* (make-hash-table)))
    ;; Grammar: expr → INT (PLUS INT)*
    (setf (gethash :add-pair cl-cc::*grammar-rules*)
          '(seq (token :T-PLUS) (token :T-INT)))
    (setf (gethash :expr cl-cc::*grammar-rules*)
          '(seq (token :T-INT) (many :add-pair)))
    (let ((s (toks :T-INT 1 :T-PLUS "+" :T-INT 2 :T-PLUS "+" :T-INT 3)))
      (multiple-value-bind (ast rest) (cl-cc::parse-with-grammar :expr s)
        ;; ast = (1 (("+" 2) ("+" 3)))
        (assert-true (listp ast))
        (assert-equal 1 (first ast))
        (assert-equal 2 (length (second ast)))
        (assert-null rest)))))

(deftest comb-integration-optional-semicolon
  "Grammar with optional trailing token."
  (let ((cl-cc::*grammar-rules* (make-hash-table)))
    (setf (gethash :stmt cl-cc::*grammar-rules*)
          '(seq (token :T-INT) (opt (token :T-SEMI))))
    ;; With semicolon
    (let ((s (toks :T-INT 1 :T-SEMI ";")))
      (multiple-value-bind (ast rest) (cl-cc::parse-with-grammar :stmt s)
        (assert-equal 1 (first ast))
        (assert-equal ";" (second ast))
        (assert-null rest)))
    ;; Without semicolon
    (let ((s (toks :T-INT 1)))
      (multiple-value-bind (ast rest) (cl-cc::parse-with-grammar :stmt s)
        (assert-equal 1 (first ast))
        (assert-equal :opt-absent (second ast))
        (assert-null rest)))))
