;;;; tests/unit/frontend/parser-combinator-tests.lisp
;;;; Unit tests for the grammar-driven parser combinator engine

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; Helpers

(defun make-tok (type value)
  "Create a test token plist."
  (list :type type :value value))

;;; Grammar rule management

(deftest grammar-rule-management
  "def-grammar-rule registers rules; unknown keys return nil."
  (clear-grammar-rules)
  (def-grammar-rule :test-rule (token :T-INT))
  (assert-equal '(token :T-INT) (query-grammar :test-rule))
  (assert-null (query-grammar :no-such-rule))
  (clear-grammar-rules))

;;; Token matching

(deftest parse-token-success-cases
  "Token combinator succeeds on matching type and on value-constrained type."
  (let ((stream (list (make-tok :T-INT 42))))
    (multiple-value-bind (ast rest) (parse-combinator '(token :T-INT) stream)
      (assert-true (parse-ok-p ast))
      (assert-= 42 ast)
      (assert-null rest)))
  (let ((stream (list (make-tok :T-OP "+") (make-tok :T-INT 1))))
    (multiple-value-bind (ast rest) (parse-combinator '(token :T-OP "+") stream)
      (assert-true (parse-ok-p ast))
      (assert-equal "+" ast)
      (assert-= 1 (length rest)))))

(deftest parse-token-failure-cases
  "Token combinator fails on wrong type and on empty stream."
  (let ((stream (list (make-tok :T-IDENT "foo"))))
    (multiple-value-bind (ast rest) (parse-combinator '(token :T-INT) stream)
      (declare (ignore rest))
      (assert-false (parse-ok-p ast))))
  (multiple-value-bind (ast rest) (parse-combinator '(token :T-INT) nil)
    (declare (ignore rest))
    (assert-false (parse-ok-p ast))))

;;; Sequence

(deftest parse-seq-cases
  (let ((stream-ok (list (make-tok :T-INT 1) (make-tok :T-OP "+") (make-tok :T-INT 2))))
    (multiple-value-bind (ast rest)
        (parse-combinator '(seq (token :T-INT) (token :T-OP "+") (token :T-INT)) stream-ok)
      (assert-true (parse-ok-p ast))
      (assert-equal '(1 "+" 2) ast)
      (assert-null rest)))
  (let ((stream-bad (list (make-tok :T-INT 1) (make-tok :T-IDENT "foo"))))
    (multiple-value-bind (ast rest)
        (parse-combinator '(seq (token :T-INT) (token :T-OP "+")) stream-bad)
      (declare (ignore rest))
      (assert-false (parse-ok-p ast)))))

;;; Alternation

(deftest parse-alt-cases
  "alt returns matching branch or fails when neither branch matches."
  (multiple-value-bind (ast rest)
      (parse-combinator '(alt (token :T-INT) (token :T-IDENT)) (list (make-tok :T-INT 42)))
    (declare (ignore rest))
    (assert-true (parse-ok-p ast))
    (assert-= 42 ast))
  (multiple-value-bind (ast rest)
      (parse-combinator '(alt (token :T-INT) (token :T-IDENT)) (list (make-tok :T-IDENT "foo")))
    (declare (ignore rest))
    (assert-true (parse-ok-p ast))
    (assert-equal "foo" ast))
  (multiple-value-bind (ast rest)
      (parse-combinator '(alt (token :T-INT) (token :T-IDENT)) (list (make-tok :T-OP "+")))
    (declare (ignore rest))
    (assert-false (parse-ok-p ast))))

;;; Repetition

(deftest parse-many-cases
  "many succeeds with nil on zero matches; many1 fails on zero matches."
  (let ((stream (list (make-tok :T-IDENT "x"))))
    (multiple-value-bind (ast rest) (parse-combinator '(many (token :T-INT)) stream)
      (assert-true (parse-ok-p ast))
      (assert-null ast)
      (assert-= 1 (length rest))))
  (multiple-value-bind (ast rest)
      (parse-combinator '(many1 (token :T-INT)) (list (make-tok :T-IDENT "x")))
    (declare (ignore rest))
    (assert-false (parse-ok-p ast))))

(deftest parse-many-collects-all-matches
  "many collects all consecutive matching tokens into a list."
  (let ((stream (list (make-tok :T-INT 1) (make-tok :T-INT 2) (make-tok :T-INT 3)
                      (make-tok :T-EOF nil))))
    (multiple-value-bind (ast rest) (parse-combinator '(many (token :T-INT)) stream)
      (assert-true (parse-ok-p ast))
      (assert-equal '(1 2 3) ast)
      (assert-= 1 (length rest))))
  (multiple-value-bind (ast rest)
      (parse-combinator '(many1 (token :T-INT)) (list (make-tok :T-INT 7) (make-tok :T-IDENT "x")))
    (declare (ignore rest))
    (assert-true (parse-ok-p ast))
    (assert-equal '(7) ast)))

;;; Optional

(deftest parse-opt-cases
  "opt returns parsed value when present; :opt-absent and unchanged stream when absent."
  (multiple-value-bind (ast rest)
      (parse-combinator '(opt (token :T-INT)) (list (make-tok :T-INT 5)))
    (assert-true (parse-ok-p ast))
    (assert-= 5 ast)
    (assert-null rest))
  (multiple-value-bind (ast rest)
      (parse-combinator '(opt (token :T-INT)) (list (make-tok :T-IDENT "x")))
    (assert-eq :opt-absent ast)
    (assert-= 1 (length rest))))

;;; Named rule reference

(deftest parse-named-rule-and-keyword-shorthand
  "Named rule and keyword shorthand both resolve via the grammar registry."
  (clear-grammar-rules)
  (def-grammar-rule :my-int (token :T-INT))
  (let ((stream (list (make-tok :T-INT 99))))
    (multiple-value-bind (ast rest) (parse-combinator '(rule :my-int) stream)
      (declare (ignore rest))
      (assert-true (parse-ok-p ast))
      (assert-= 99 ast)))
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
