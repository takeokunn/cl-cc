;;;; combinator-tests-2.lisp — Parser Combinator Engine Tests (continued)
;;;;
;;;; Tests for parse-opt*, parse-rule, parse-combinator dispatcher,
;;;; parse-with-grammar, and multi-rule integration scenarios.

(in-package :cl-cc/test)

(in-suite combinator-suite)

;;; ─── parse-opt* ─────────────────────────────────────────────────────────────

(deftest comb-opt-match
  "parse-opt* returns matched value on success."
  (let ((s (toks :T-INT 42)))
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-opt* '(token :T-INT) s)
      (assert-equal 42 ast)
      (assert-null rest))))

(deftest comb-opt-absent
  "parse-opt* returns :opt-absent on no match (not :fail)."
  (let ((s (toks :T-IDENT "x")))
    (multiple-value-bind (ast rest) (cl-cc/parse::parse-opt* '(token :T-INT) s)
      (assert-equal :opt-absent ast)
      (assert-equal 1 (length rest)))))

;;; ─── parse-rule ─────────────────────────────────────────────────────────────

(deftest comb-parse-rule-defined
  "parse-rule looks up and applies a named grammar rule."
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    (setf (gethash :my-int cl-cc/parse:*grammar-rules*) '(token :T-INT))
    (let ((s (toks :T-INT 99)))
      (multiple-value-bind (ast rest) (cl-cc/parse::parse-rule :my-int s)
        (assert-equal 99 ast)
        (assert-null rest)))))

(deftest comb-parse-rule-undefined-error
  "parse-rule errors on undefined rule."
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    (assert-signals error (cl-cc/parse::parse-rule :no-such-rule nil))))

;;; ─── parse-combinator (dispatcher) ─────────────────────────────────────────

(deftest comb-dispatch-keyword-shorthand
  "Bare keyword dispatches to parse-rule."
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    (setf (gethash :num cl-cc/parse:*grammar-rules*) '(token :T-INT))
    (let ((s (toks :T-INT 7)))
      (multiple-value-bind (ast rest) (cl-cc/parse:parse-combinator :num s)
        (assert-equal 7 ast)
        (assert-null rest)))))

(deftest comb-dispatch-token
  "parse-combinator dispatches (token ...) form."
  (let ((s (toks :T-INT 5)))
    (multiple-value-bind (ast rest) (cl-cc/parse:parse-combinator '(token :T-INT) s)
      (assert-equal 5 ast)
      (assert-null rest))))

(deftest comb-dispatch-unknown-operator-error
  "parse-combinator errors on unknown operator."
  (assert-signals error (cl-cc/parse:parse-combinator '(bogus :T-INT) nil)))

(deftest comb-dispatch-non-list-non-keyword-error
  "parse-combinator errors on invalid expression."
  (assert-signals error (cl-cc/parse:parse-combinator 42 nil)))

;;; ─── parse-with-grammar (top-level) ────────────────────────────────────────

(deftest comb-parse-with-grammar-success
  "parse-with-grammar returns AST on successful parse."
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    (setf (gethash :expr cl-cc/parse:*grammar-rules*) '(token :T-INT))
    (let ((s (toks :T-INT 100)))
      (multiple-value-bind (ast rest) (cl-cc/parse:parse-with-grammar :expr s)
        (assert-equal 100 ast)
        (assert-null rest)))))

(deftest comb-parse-with-grammar-failure-error
  "parse-with-grammar signals error on parse failure."
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    (setf (gethash :expr cl-cc/parse:*grammar-rules*) '(token :T-INT))
    (let ((s (toks :T-IDENT "x")))
      (assert-signals error (cl-cc/parse:parse-with-grammar :expr s)))))

;;; ─── Integration: multi-rule grammar ────────────────────────────────────────

(deftest comb-integration-arithmetic
  "Multi-rule grammar parses simple arithmetic token sequence."
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    ;; Grammar: expr → INT (PLUS INT)*
    (setf (gethash :add-pair cl-cc/parse:*grammar-rules*)
          '(seq (token :T-PLUS) (token :T-INT)))
    (setf (gethash :expr cl-cc/parse:*grammar-rules*)
          '(seq (token :T-INT) (many :add-pair)))
    (let ((s (toks :T-INT 1 :T-PLUS "+" :T-INT 2 :T-PLUS "+" :T-INT 3)))
      (multiple-value-bind (ast rest) (cl-cc/parse:parse-with-grammar :expr s)
        ;; ast = (1 (("+" 2) ("+" 3)))
        (assert-true (listp ast))
        (assert-equal 1 (first ast))
        (assert-equal 2 (length (second ast)))
        (assert-null rest)))))

(deftest comb-integration-optional-semicolon
  "Grammar with optional trailing token."
  (let ((cl-cc/parse:*grammar-rules* (make-hash-table)))
    (setf (gethash :stmt cl-cc/parse:*grammar-rules*)
          '(seq (token :T-INT) (opt (token :T-SEMI))))
    ;; With semicolon
    (let ((s (toks :T-INT 1 :T-SEMI ";")))
      (multiple-value-bind (ast rest) (cl-cc/parse:parse-with-grammar :stmt s)
        (assert-equal 1 (first ast))
        (assert-equal ";" (second ast))
        (assert-null rest)))
    ;; Without semicolon
    (let ((s (toks :T-INT 1)))
      (multiple-value-bind (ast rest) (cl-cc/parse:parse-with-grammar :stmt s)
        (assert-equal 1 (first ast))
        (assert-equal :opt-absent (second ast))
        (assert-null rest)))))
