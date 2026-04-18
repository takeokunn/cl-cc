;;;; tests/unit/frontend/parser-combinator-tests.lisp
;;;; Unit tests for the grammar-driven parser combinator engine

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

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

(deftest parse-token-matches-correct-type
  "Token combinator succeeds and returns value when type matches."
  (let ((stream (list (make-tok :T-INT 42))))
    (multiple-value-bind (ast rest) (parse-combinator '(token :T-INT) stream)
      (assert-true (parse-ok-p ast))
      (assert-= 42 ast)
      (assert-null rest))))

(deftest parse-token-fails-on-wrong-type
  "Token combinator fails when stream head is the wrong type."
  (let ((stream (list (make-tok :T-IDENT "foo"))))
    (multiple-value-bind (ast rest) (parse-combinator '(token :T-INT) stream)
      (declare (ignore rest))
      (assert-false (parse-ok-p ast)))))

(deftest parse-token-fails-on-empty-stream
  "Token combinator fails on an empty stream."
  (multiple-value-bind (ast rest) (parse-combinator '(token :T-INT) nil)
    (declare (ignore rest))
    (assert-false (parse-ok-p ast))))

(deftest parse-token-with-value-constraint
  "Token combinator with type and value constraint matches and leaves rest."
  (let ((stream (list (make-tok :T-OP "+") (make-tok :T-INT 1))))
    (multiple-value-bind (ast rest) (parse-combinator '(token :T-OP "+") stream)
      (assert-true (parse-ok-p ast))
      (assert-equal "+" ast)
      (assert-= 1 (length rest)))))

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

(deftest parse-alt-first-branch-wins
  "alt returns the first branch's result when first token matches."
  (multiple-value-bind (ast rest)
      (parse-combinator '(alt (token :T-INT) (token :T-IDENT)) (list (make-tok :T-INT 42)))
    (declare (ignore rest))
    (assert-true (parse-ok-p ast))
    (assert-= 42 ast)))

(deftest parse-alt-second-branch-wins
  "alt falls through to second branch when first fails."
  (multiple-value-bind (ast rest)
      (parse-combinator '(alt (token :T-INT) (token :T-IDENT)) (list (make-tok :T-IDENT "foo")))
    (declare (ignore rest))
    (assert-true (parse-ok-p ast))
    (assert-equal "foo" ast)))

(deftest parse-alt-fails-when-both-branches-fail
  "alt fails when neither branch matches."
  (multiple-value-bind (ast rest)
      (parse-combinator '(alt (token :T-INT) (token :T-IDENT)) (list (make-tok :T-OP "+")))
    (declare (ignore rest))
    (assert-false (parse-ok-p ast))))

;;; Repetition

(deftest parse-many-zero-matches-succeed-with-nil
  "many succeeds with nil result when no tokens match."
  (let ((stream (list (make-tok :T-IDENT "x"))))
    (multiple-value-bind (ast rest) (parse-combinator '(many (token :T-INT)) stream)
      (assert-true (parse-ok-p ast))
      (assert-null ast)
      (assert-= 1 (length rest)))))

(deftest parse-many-collects-all-matches
  "many collects all consecutive matching tokens into a list."
  (let ((stream (list (make-tok :T-INT 1) (make-tok :T-INT 2) (make-tok :T-INT 3)
                      (make-tok :T-EOF nil))))
    (multiple-value-bind (ast rest) (parse-combinator '(many (token :T-INT)) stream)
      (assert-true (parse-ok-p ast))
      (assert-equal '(1 2 3) ast)
      (assert-= 1 (length rest)))))

(deftest parse-many1-fails-when-no-match
  "many1 fails when no tokens match the pattern."
  (multiple-value-bind (ast rest)
      (parse-combinator '(many1 (token :T-INT)) (list (make-tok :T-IDENT "x")))
    (declare (ignore rest))
    (assert-false (parse-ok-p ast))))

(deftest parse-many1-succeeds-with-at-least-one
  "many1 succeeds and collects the one matching token."
  (multiple-value-bind (ast rest)
      (parse-combinator '(many1 (token :T-INT)) (list (make-tok :T-INT 7) (make-tok :T-IDENT "x")))
    (declare (ignore rest))
    (assert-true (parse-ok-p ast))
    (assert-equal '(7) ast)))

;;; Optional

(deftest parse-opt-returns-value-when-present
  "opt returns the parsed value when the token is present."
  (multiple-value-bind (ast rest)
      (parse-combinator '(opt (token :T-INT)) (list (make-tok :T-INT 5)))
    (assert-true (parse-ok-p ast))
    (assert-= 5 ast)
    (assert-null rest)))

(deftest parse-opt-returns-absent-sentinel-when-missing
  "opt returns :opt-absent and leaves the stream unchanged when token is absent."
  (multiple-value-bind (ast rest)
      (parse-combinator '(opt (token :T-INT)) (list (make-tok :T-IDENT "x")))
    (assert-eq :opt-absent ast)
    (assert-= 1 (length rest))))

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
