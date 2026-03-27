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

(deftest-each parse-token-cases
  "Token matching: hit, miss, empty stream, value-constrained match"
  :cases (("match"      (list (make-tok :T-INT 42))                          :match)
          ("no-match"   (list (make-tok :T-IDENT "foo"))                     :no-match)
          ("empty"      nil                                                   :no-match)
          ("with-value" (list (make-tok :T-OP "+") (make-tok :T-INT 1))      :with-value))
  (stream kind)
  (ecase kind
    (:match
     (multiple-value-bind (ast rest) (parse-combinator '(token :T-INT) stream)
       (assert-true (parse-ok-p ast))
       (assert-= 42 ast)
       (assert-null rest)))
    (:no-match
     (multiple-value-bind (ast rest) (parse-combinator '(token :T-INT) stream)
       (declare (ignore rest))
       (assert-false (parse-ok-p ast))))
    (:with-value
     (multiple-value-bind (ast rest) (parse-combinator '(token :T-OP "+") stream)
       (assert-true (parse-ok-p ast))
       (assert-equal "+" ast)
       (assert-= 1 (length rest))))))

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

(deftest-each parse-alt-cases
  "Alternation: first branch wins, second branch wins, both branches fail"
  :cases (("first-succeeds"  (list (make-tok :T-INT 42))        :first)
          ("second-succeeds" (list (make-tok :T-IDENT "foo"))   :second)
          ("both-fail"       (list (make-tok :T-OP "+"))        :fail))
  (stream kind)
  (multiple-value-bind (ast rest)
      (parse-combinator '(alt (token :T-INT) (token :T-IDENT)) stream)
    (declare (ignore rest))
    (ecase kind
      (:first
       (assert-true (parse-ok-p ast))
       (assert-= 42 ast))
      (:second
       (assert-true (parse-ok-p ast))
       (assert-equal "foo" ast))
      (:fail
       (assert-false (parse-ok-p ast))))))

;;; Repetition

(deftest-each parse-many-cases
  "many: zero matches succeed with nil, multiple matches collect all"
  :cases (("zero"     (list (make-tok :T-IDENT "x"))                                          :zero)
          ("multiple" (list (make-tok :T-INT 1) (make-tok :T-INT 2) (make-tok :T-INT 3)
                            (make-tok :T-EOF nil))                                             :multiple))
  (stream kind)
  (multiple-value-bind (ast rest) (parse-combinator '(many (token :T-INT)) stream)
    (assert-true (parse-ok-p ast))
    (ecase kind
      (:zero
       (assert-null ast)
       (assert-= 1 (length rest)))
      (:multiple
       (assert-equal '(1 2 3) ast)
       (assert-= 1 (length rest))))))

(deftest-each parse-many1-cases
  "many1: fails when no tokens match, succeeds with at least one"
  :cases (("requires-one" (list (make-tok :T-IDENT "x"))                        :fail)
          ("one-match"    (list (make-tok :T-INT 7) (make-tok :T-IDENT "x"))    :ok))
  (stream kind)
  (multiple-value-bind (ast rest) (parse-combinator '(many1 (token :T-INT)) stream)
    (declare (ignore rest))
    (ecase kind
      (:fail  (assert-false (parse-ok-p ast)))
      (:ok
       (assert-true (parse-ok-p ast))
       (assert-equal '(7) ast)))))

;;; Optional

(deftest-each parse-opt-cases
  "opt: returns value when token present, :opt-absent when absent"
  :cases (("present" (list (make-tok :T-INT 5))      :present)
          ("absent"  (list (make-tok :T-IDENT "x"))  :absent))
  (stream kind)
  (multiple-value-bind (ast rest) (parse-combinator '(opt (token :T-INT)) stream)
    (ecase kind
      (:present
       (assert-true (parse-ok-p ast))
       (assert-= 5 ast)
       (assert-null rest))
      (:absent
       (assert-eq :opt-absent ast)
       (assert-= 1 (length rest))))))

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
