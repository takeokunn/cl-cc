;;;; tests/unit/frontend/parser-combinator-tests.lisp
;;;; Unit tests for the grammar-driven parser combinator engine

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; Helpers

(defun make-tok (type value)
  "Create a test token plist."
  (list :type type :value value))

;;; Grammar rule management

(deftest grammar-rule-register-and-query
  (clear-grammar-rules)
  (def-grammar-rule :test-rule (token :T-INT))
  (assert-equal '(token :T-INT) (query-grammar :test-rule))
  (clear-grammar-rules))

(deftest grammar-rule-unknown-returns-nil
  (clear-grammar-rules)
  (assert-null (query-grammar :no-such-rule)))

;;; Token matching

(deftest parse-token-match
  (let ((stream (list (make-tok :T-INT 42))))
    (multiple-value-bind (ast rest) (parse-combinator '(token :T-INT) stream)
      (assert-true (parse-ok-p ast))
      (assert-= 42 ast)
      (assert-null rest))))

(deftest parse-token-no-match
  (let ((stream (list (make-tok :T-IDENT "foo"))))
    (multiple-value-bind (ast rest) (parse-combinator '(token :T-INT) stream)
      (declare (ignore rest))
      (assert-false (parse-ok-p ast)))))

(deftest parse-token-empty-stream
  (multiple-value-bind (ast rest) (parse-combinator '(token :T-INT) nil)
    (declare (ignore rest))
    (assert-false (parse-ok-p ast))))

(deftest parse-token-with-value
  (let ((stream (list (make-tok :T-OP "+") (make-tok :T-INT 1))))
    (multiple-value-bind (ast rest) (parse-combinator '(token :T-OP "+") stream)
      (assert-true (parse-ok-p ast))
      (assert-equal "+" ast)
      (assert-= 1 (length rest)))))

;;; Sequence

(deftest parse-seq-success
  (let ((stream (list (make-tok :T-INT 1) (make-tok :T-OP "+") (make-tok :T-INT 2))))
    (multiple-value-bind (ast rest)
        (parse-combinator '(seq (token :T-INT) (token :T-OP "+") (token :T-INT)) stream)
      (assert-true (parse-ok-p ast))
      (assert-equal '(1 "+" 2) ast)
      (assert-null rest))))

(deftest parse-seq-partial-failure
  (let ((stream (list (make-tok :T-INT 1) (make-tok :T-IDENT "foo"))))
    (multiple-value-bind (ast rest)
        (parse-combinator '(seq (token :T-INT) (token :T-OP "+")) stream)
      (declare (ignore rest))
      (assert-false (parse-ok-p ast)))))

;;; Alternation

(deftest parse-alt-first-succeeds
  (let ((stream (list (make-tok :T-INT 42))))
    (multiple-value-bind (ast rest)
        (parse-combinator '(alt (token :T-INT) (token :T-IDENT)) stream)
      (declare (ignore rest))
      (assert-true (parse-ok-p ast))
      (assert-= 42 ast))))

(deftest parse-alt-second-succeeds
  (let ((stream (list (make-tok :T-IDENT "foo"))))
    (multiple-value-bind (ast rest)
        (parse-combinator '(alt (token :T-INT) (token :T-IDENT)) stream)
      (declare (ignore rest))
      (assert-true (parse-ok-p ast))
      (assert-equal "foo" ast))))

(deftest parse-alt-both-fail
  (let ((stream (list (make-tok :T-OP "+"))))
    (multiple-value-bind (ast rest)
        (parse-combinator '(alt (token :T-INT) (token :T-IDENT)) stream)
      (declare (ignore rest))
      (assert-false (parse-ok-p ast)))))

;;; Repetition

(deftest parse-many-zero
  (let ((stream (list (make-tok :T-IDENT "x"))))
    (multiple-value-bind (ast rest) (parse-combinator '(many (token :T-INT)) stream)
      (assert-true (parse-ok-p ast))
      (assert-null ast)
      (assert-= 1 (length rest)))))

(deftest parse-many-multiple
  (let ((stream (list (make-tok :T-INT 1) (make-tok :T-INT 2) (make-tok :T-INT 3)
                      (make-tok :T-EOF nil))))
    (multiple-value-bind (ast rest) (parse-combinator '(many (token :T-INT)) stream)
      (assert-true (parse-ok-p ast))
      (assert-equal '(1 2 3) ast)
      (assert-= 1 (length rest)))))

(deftest parse-many1-requires-one
  (let ((stream (list (make-tok :T-IDENT "x"))))
    (multiple-value-bind (ast rest) (parse-combinator '(many1 (token :T-INT)) stream)
      (declare (ignore rest))
      (assert-false (parse-ok-p ast)))))

(deftest parse-many1-one-match
  (let ((stream (list (make-tok :T-INT 7) (make-tok :T-IDENT "x"))))
    (multiple-value-bind (ast rest) (parse-combinator '(many1 (token :T-INT)) stream)
      (declare (ignore rest))
      (assert-true (parse-ok-p ast))
      (assert-equal '(7) ast))))

;;; Optional

(deftest parse-opt-present
  (let ((stream (list (make-tok :T-INT 5))))
    (multiple-value-bind (ast rest) (parse-combinator '(opt (token :T-INT)) stream)
      (assert-true (parse-ok-p ast))
      (assert-= 5 ast)
      (assert-null rest))))

(deftest parse-opt-absent
  (let ((stream (list (make-tok :T-IDENT "x"))))
    (multiple-value-bind (ast rest) (parse-combinator '(opt (token :T-INT)) stream)
      (assert-eq :opt-absent ast)
      (assert-= 1 (length rest)))))

;;; Named rule reference

(deftest parse-named-rule
  (clear-grammar-rules)
  (def-grammar-rule :my-int (token :T-INT))
  (let ((stream (list (make-tok :T-INT 99))))
    (multiple-value-bind (ast rest) (parse-combinator '(rule :my-int) stream)
      (declare (ignore rest))
      (assert-true (parse-ok-p ast))
      (assert-= 99 ast)))
  (clear-grammar-rules))

(deftest parse-keyword-shorthand
  (clear-grammar-rules)
  (def-grammar-rule :my-ident (token :T-IDENT))
  (let ((stream (list (make-tok :T-IDENT "hello"))))
    (multiple-value-bind (ast rest) (parse-combinator :my-ident stream)
      (declare (ignore rest))
      (assert-true (parse-ok-p ast))
      (assert-equal "hello" ast)))
  (clear-grammar-rules))

;;; Integration: arithmetic expression grammar

(deftest grammar-arithmetic-expression
  (clear-grammar-rules)
  ;; atom = integer | identifier
  (def-grammar-rule :atom (alt (token :T-INT) (token :T-IDENT)))
  ;; addop = "+" | "-"
  (def-grammar-rule :addop (alt (token :T-OP "+") (token :T-OP "-")))
  ;; expr = atom (addop atom)*
  (def-grammar-rule :expr (seq (rule :atom) (many (seq (rule :addop) (rule :atom)))))
  (let ((stream (list (make-tok :T-INT 1) (make-tok :T-OP "+")
                      (make-tok :T-INT 2) (make-tok :T-OP "-")
                      (make-tok :T-INT 3))))
    (multiple-value-bind (ast rest) (parse-with-grammar :expr stream)
      (assert-true (parse-ok-p ast))
      (assert-null rest)))
  (clear-grammar-rules))
