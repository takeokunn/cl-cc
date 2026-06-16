;;;; tests/unit/prolog/dcg-tests.lisp — DCG Engine Unit Tests
;;;;
;;;; Tests for the DCG (Definite Clause Grammar) parsing engine:
;;;; builtin predicates and public entry points (phrase, phrase-all).

(in-package :cl-cc/test)

(in-suite cl-cc-serial-suite)

(defsuite dcg-suite :description "DCG engine unit tests"
  :parent cl-cc-serial-suite)


(in-suite dcg-suite)

(deftest dcg-token-match-value-binds-scalar-token-values
  "dcg-token-match-value binds the token value and preserves the remaining stream."
  (with-fresh-prolog
    (let ((envs nil))
      (cl-cc:solve-goal
       (list 'cl-cc/prolog::dcg-token-match-value
             :T-INT '?value
             (dcg-input :T-INT 42 :T-EOF nil)
             '?out)
       nil
       (lambda (env)
         (push env envs)))
      (assert-true envs)
      (let ((env (first envs)))
        (assert-equal 42 (cl-cc:logic-substitute '?value env))
        (assert-equal (list (cons :T-EOF nil))
                      (cl-cc:logic-substitute '?out env))))))

(deftest dcg-builtins-registered-via-shared-table
  "DCG builtins are registered through the shared builtin predicate table."
  (dolist (spec cl-cc/prolog::*dcg-builtin-specs*)
    (destructuring-bind (predicate handler) spec
      (assert-eq (symbol-function handler)
                 (gethash predicate cl-cc/prolog::*builtin-predicates*)))))

;;; ─── phrase ─────────────────────────────────────────────────────────────────

(deftest phrase-returns-remaining-input-after-match
  "phrase returns the remaining token list after a successful rule match."
  (with-prolog-facts ((consume-one (?h . ?rest) ?rest))
    (let* ((input  (dcg-input :T-INT 1 :T-INT 2))
           (result (cl-cc/prolog:phrase 'consume-one input)))
      (assert-equal 1 (length result))
      (assert-equal :T-INT (caar result))
      (assert-equal 2     (cdar result)))))

(deftest phrase-returns-nil-when-no-rule-matches
  "phrase returns nil when no rule in the database matches the input."
  (with-prolog-facts ((never-match impossible-sentinel ?rest))
    (assert-null (cl-cc/prolog:phrase 'never-match (dcg-input :T-INT 1)))))

(deftest phrase-all-multiple
  "phrase-all returns all possible parse results."
  (with-prolog-facts ((flexible (?h . ?rest) ?rest)
                      (flexible (?a ?b . ?rest) ?rest))
    (let* ((input (dcg-input :T-INT 1 :T-INT 2 :T-INT 3))
           (results (cl-cc/prolog:phrase-all 'flexible input)))
      ;; Should get at least 2 results (consume-1 and consume-2)
      (assert-true (>= (length results) 2)))))

(deftest dcg-rule-helpers-roundtrip
  "def-dcg-rule, phrase, and phrase-all agree on a simple rule."
  (with-dcg-rules ((accept-all))
    (let ((input (dcg-input :T-INT 1 :T-EOF nil)))
      (assert-equal input (cl-cc:phrase 'accept-all input))
      (assert-equal (list input)
                    (cl-cc:phrase-all 'accept-all input)))))

(deftest dcg-rule-terminal-and-brace-forms
  "def-dcg-rule compiles terminal and brace bodies into a working parser."
  (with-dcg-rules ((accept-token-and-check
                    (terminal :T-INT)
                    (brace t)
                    (terminal :T-IDENT)))
    (let ((input (dcg-input :T-INT 1 :T-IDENT "x" :T-EOF nil)))
      (assert-equal (list (cons :T-EOF nil))
                    (cl-cc:phrase 'accept-token-and-check input)))))

(deftest dcg-fresh-counter-resets
  "dcg-fresh-var is deterministic after dcg-reset-counter."
  (cl-cc/prolog::dcg-reset-counter)
  (let ((first (cl-cc/prolog::dcg-fresh-var))
        (second (cl-cc/prolog::dcg-fresh-var)))
    (assert-false (eq first second))
    (cl-cc/prolog::dcg-reset-counter)
    (assert-eq first (cl-cc/prolog::dcg-fresh-var))))

;;; ─── DCG Builtins via solve-goal ───────────────────────────────────────────

(deftest dcg-alt-matches-first-rule
  "dcg-alt succeeds via the first rule when input matches it; remaining stream is nil."
  (with-dcg-token-rules ((rule-a :T-INT) (rule-b :T-IDENT))
    (with-dcg-results (results
                       (list 'cl-cc/prolog::dcg-alt 'rule-a 'rule-b (dcg-input :T-INT 42) '?out)
                       '?out)
      (assert-true results)
      (assert-null (first results)))))

(deftest dcg-alt-matches-second-rule
  "dcg-alt succeeds via the second rule when only that rule matches the input."
  (with-dcg-token-rules ((rule-a :T-INT) (rule-b :T-IDENT))
    (assert-dcg-solves (list 'cl-cc/prolog::dcg-alt 'rule-a 'rule-b (dcg-input :T-IDENT "x") '?out)
                       '?out)))

(deftest dcg-opt-succeeds-with-match
  "dcg-opt includes nil (empty remaining) in results when the sub-rule matches."
  (with-dcg-token-rules ((tok-int :T-INT))
    (with-dcg-results (results
                       (list 'cl-cc/prolog::dcg-opt 'tok-int (dcg-input :T-INT 1) '?out)
                       '?out)
      (assert-true (member nil results :test #'equal)))))

(deftest dcg-opt-provides-epsilon-on-mismatch
  "dcg-opt succeeds with epsilon (unchanged stream) when the sub-rule does not match."
  (with-dcg-token-rules ((tok-int :T-INT))
    (assert-dcg-solves (list 'cl-cc/prolog::dcg-opt 'tok-int (dcg-input :T-IDENT "x") '?out)
                       '?out)))

(deftest dcg-star-succeeds-with-zero-matches
  "dcg-star (zero-or-more) succeeds even when the sub-rule does not match at all."
  (with-dcg-token-rules ((tok-int :T-INT))
    (assert-dcg-solves (list 'cl-cc/prolog::dcg-star 'tok-int (dcg-input :T-IDENT "x") '?out)
                       '?out)))

(deftest dcg-star-collects-multiple-matches
  "dcg-star collects all consecutive matches; nil (empty stream) appears in results."
  (with-dcg-token-rules ((tok-int :T-INT))
    (with-dcg-results (results
                       (list 'cl-cc/prolog::dcg-star 'tok-int
                             (dcg-input :T-INT 1 :T-INT 2 :T-INT 3) '?out)
                       '?out)
      (assert-true (member nil results :test #'equal)))))

(deftest dcg-plus-succeeds-with-one-match
  "dcg-plus (one-or-more) succeeds when there is at least one matching token."
  (with-dcg-token-rules ((tok-int :T-INT))
    (assert-dcg-solves (list 'cl-cc/prolog::dcg-plus 'tok-int (dcg-input :T-INT 42) '?out)
                       '?out)))

(deftest dcg-plus-fails-on-zero-matches
  "dcg-plus returns no solutions when the sub-rule does not match at all."
  (with-dcg-token-rules ((tok-int :T-INT))
    (with-dcg-results (results
                       (list 'cl-cc/prolog::dcg-plus 'tok-int (dcg-input :T-IDENT "x") '?out)
                       '?out)
      (assert-null results))))

;;; ─── dcg-error-recovery ─────────────────────────────────────────────────────

(deftest dcg-error-recovery-skips-to-sync-token
  "dcg-error-recovery skips tokens until it finds a sync token (:T-RPAREN)."
  (with-dcg-results (results
                     (list 'cl-cc/prolog::dcg-error-recovery
                           (dcg-input :T-INT 1 :T-IDENT "x" :T-RPAREN ")")
                           '?out)
                     '?out)
    (assert-true results)
    (let ((remaining (first results)))
      (assert-equal :T-RPAREN (caar remaining)))))

(deftest dcg-error-recovery-handles-empty-input
  "dcg-error-recovery succeeds on empty input, returning nil as the remaining stream."
  (with-dcg-results (results
                     (list 'cl-cc/prolog::dcg-error-recovery nil '?out)
                     '?out)
    (assert-true (member nil results :test #'equal))))
