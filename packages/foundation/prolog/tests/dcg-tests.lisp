;;;; tests/unit/prolog/dcg-tests.lisp — DCG Engine Unit Tests
;;;;
;;;; Tests for the DCG (Definite Clause Grammar) parsing engine:
;;;; input conversion, builtin predicates (dcg-alt, dcg-opt, dcg-star,
;;;; dcg-plus, dcg-error-recovery), and entry points (dcg-parse, phrase-all).

(in-package :cl-cc/test)

(in-suite cl-cc-serial-suite)

(defsuite dcg-suite :description "DCG engine unit tests"
  :parent cl-cc-serial-suite)


(in-suite dcg-suite)
;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun dcg-input (&rest type-value-pairs)
  "Build a DCG input list from (type value) pairs.
   Example: (dcg-input :T-INT 1 :T-PLUS \"+\") → ((:T-INT . 1) (:T-PLUS . \"+\"))"
  (loop for (type val) on type-value-pairs by #'cddr
        collect (cons type val)))

(defun collect-dcg-outcomes (goal &optional (output-var '?out))
  "Run GOAL through solve-goal and collect the substituted OUTPUT-VAR values."
  (let ((results nil))
    (handler-case
        (cl-cc/prolog::solve-goal goal nil
                           (lambda (env)
                             (push (cl-cc/prolog::logic-substitute output-var env)
                                   results)))
      (cl-cc/prolog::prolog-cut ()))
    (nreverse results)))

;;; ─── lexer-tokens-to-dcg-input ─────────────────────────────────────────────

(deftest dcg-token-conversion-cases
  "lexer-tokens-to-dcg-input handles plist/struct/other tokens; dcg-token-to-cst creates cst-token."
  (let ((tokens (list (list :type :T-INT :value 42)
                      (list :type :T-IDENT :value "x"))))
    (let ((result (cl-cc/prolog::lexer-tokens-to-dcg-input tokens)))
      (assert-equal 2 (length result))
      (assert-equal :T-INT (caar result))
      (assert-equal 42 (cdar result))
      (assert-equal :T-IDENT (caadr result))
      (assert-equal "x" (cdadr result))))
  (let ((tokens (list (cl-cc/parse::make-lexer-token :type :T-INT :value 99))))
    (let ((result (cl-cc/prolog::lexer-tokens-to-dcg-input tokens)))
      (assert-equal 1 (length result))
      (assert-equal :T-INT (caar result))
      (assert-equal 99 (cdar result))))
  (let ((tokens (list 42)))
    (let ((result (cl-cc/prolog::lexer-tokens-to-dcg-input tokens)))
      (assert-equal 1 (length result))
      (assert-equal 42 (first result))))
  (let ((tok (cons :T-INT 42)))
    (let ((cst (cl-cc/prolog::dcg-token-to-cst tok 10)))
      (assert-true (cl-cc/parse::cst-token-p cst))
      (assert-equal :T-INT (cl-cc:cst-node-kind cst))
      (assert-equal 42 (cl-cc:cst-token-value cst))
      (assert-equal 10 (cl-cc:cst-node-start-byte cst)))))

;;; ─── dcg-parse ───────────────────────────────────────────────────────────────

(deftest dcg-parse-cases
  "dcg-parse returns remaining input after match; returns nil when no rule matches."
  (with-fresh-prolog
    (cl-cc/prolog::def-fact (consume-one (?h . ?rest) ?rest))
    (let* ((input (dcg-input :T-INT 1 :T-INT 2))
           (result (cl-cc/prolog::dcg-parse 'consume-one input)))
      (assert-equal 1 (length result))
      (assert-equal :T-INT (caar result))
      (assert-equal 2 (cdar result))))
  (with-fresh-prolog
    (cl-cc/prolog::def-fact (never-match impossible-sentinel ?rest))
    (let ((result (cl-cc/prolog::dcg-parse 'never-match (dcg-input :T-INT 1))))
      (assert-null result))))

(deftest phrase-all-multiple
  "phrase-all returns all possible parse results."
  (with-fresh-prolog
    ;; Two overlapping rules
    (cl-cc/prolog::def-fact (flexible (?h . ?rest) ?rest))       ;; consume 1
    (cl-cc/prolog::def-fact (flexible (?a ?b . ?rest) ?rest))    ;; consume 2
   (let* ((input (dcg-input :T-INT 1 :T-INT 2 :T-INT 3))
           (results (cl-cc/prolog::phrase-all 'flexible input)))
      ;; Should get at least 2 results (consume-1 and consume-2)
      (assert-true (>= (length results) 2)))))

(deftest dcg-rule-helpers-roundtrip
  "def-dcg-rule, phrase, phrase-rest, and phrase-all agree on a simple rule."
  (with-fresh-prolog
    (cl-cc:def-dcg-rule accept-all)
    (let ((input (dcg-input :T-INT 1 :T-EOF nil)))
      (assert-equal input (cl-cc:phrase 'accept-all input))
      (multiple-value-bind (matched remaining)
          (cl-cc:phrase-rest 'accept-all input)
        (assert-true matched)
        (assert-equal input remaining))
      (assert-equal (list input)
                    (cl-cc:phrase-all 'accept-all input)))))

(deftest dcg-fresh-counter-resets
  "dcg-fresh-var is deterministic after dcg-reset-counter."
  (cl-cc:dcg-reset-counter)
  (let ((first (cl-cc:dcg-fresh-var))
        (second (cl-cc:dcg-fresh-var)))
    (assert-false (eq first second))
    (cl-cc:dcg-reset-counter)
    (assert-eq first (cl-cc:dcg-fresh-var))))

;;; ─── DCG Builtins via solve-goal ───────────────────────────────────────────

(deftest dcg-alternation-cases
  "dcg-alt tries both alternatives; dcg-opt succeeds with epsilon when sub-rule fails."
  (with-fresh-prolog
    (cl-cc/prolog::def-fact (rule-a ((:T-INT . ?v) . ?rest) ?rest))
    (cl-cc/prolog::def-fact (rule-b ((:T-IDENT . ?v) . ?rest) ?rest))
    (let ((results (collect-dcg-outcomes
                    (list 'cl-cc/prolog::dcg-alt 'rule-a 'rule-b (dcg-input :T-INT 42) '?out))))
      (assert-true (>= (length results) 1))
      (assert-null (first results))))
  (with-fresh-prolog
    (cl-cc/prolog::def-fact (rule-a ((:T-INT . ?v) . ?rest) ?rest))
    (cl-cc/prolog::def-fact (rule-b ((:T-IDENT . ?v) . ?rest) ?rest))
    (let ((results (collect-dcg-outcomes
                    (list 'cl-cc/prolog::dcg-alt 'rule-a 'rule-b (dcg-input :T-IDENT "x") '?out))))
      (assert-true (>= (length results) 1))))
  (with-fresh-prolog
    (cl-cc/prolog::def-fact (tok-int ((:T-INT . ?v) . ?rest) ?rest))
    (let ((results (collect-dcg-outcomes
                    (list 'cl-cc/prolog::dcg-opt 'tok-int (dcg-input :T-INT 1) '?out))))
      (assert-true (member nil results :test #'equal))))
  (with-fresh-prolog
    (cl-cc/prolog::def-fact (tok-int ((:T-INT . ?v) . ?rest) ?rest))
    (let ((results (collect-dcg-outcomes
                    (list 'cl-cc/prolog::dcg-opt 'tok-int (dcg-input :T-IDENT "x") '?out))))
      (assert-true (>= (length results) 1)))))

(deftest dcg-repetition-cases
  "dcg-star succeeds with zero/multiple matches; dcg-plus requires at least one match."
  (with-fresh-prolog
    (cl-cc/prolog::def-fact (tok-int ((:T-INT . ?v) . ?rest) ?rest))
    (let ((results (collect-dcg-outcomes
                    (list 'cl-cc/prolog::dcg-star 'tok-int (dcg-input :T-IDENT "x") '?out))))
      (assert-true (>= (length results) 1))))
  (with-fresh-prolog
    (cl-cc/prolog::def-fact (tok-int ((:T-INT . ?v) . ?rest) ?rest))
    (let ((results (collect-dcg-outcomes
                    (list 'cl-cc/prolog::dcg-star 'tok-int
                          (dcg-input :T-INT 1 :T-INT 2 :T-INT 3) '?out))))
      (assert-true (member nil results :test #'equal))))
  (with-fresh-prolog
    (cl-cc/prolog::def-fact (tok-int ((:T-INT . ?v) . ?rest) ?rest))
    (let ((results (collect-dcg-outcomes
                    (list 'cl-cc/prolog::dcg-plus 'tok-int (dcg-input :T-INT 42) '?out))))
      (assert-true (>= (length results) 1))))
  (with-fresh-prolog
    (cl-cc/prolog::def-fact (tok-int ((:T-INT . ?v) . ?rest) ?rest))
    (let ((results (collect-dcg-outcomes
                    (list 'cl-cc/prolog::dcg-plus 'tok-int (dcg-input :T-IDENT "x") '?out))))
      (assert-null results))))

;;; ─── dcg-error-recovery ─────────────────────────────────────────────────────

(deftest dcg-error-recovery-cases
  "dcg-error-recovery skips tokens until sync token; handles empty input."
  (with-fresh-prolog
    (let ((results nil)
          (input (dcg-input :T-INT 1 :T-IDENT "x" :T-RPAREN ")")))
      (handler-case
          (cl-cc/prolog::solve-goal (list 'cl-cc/prolog::dcg-error-recovery input '?out)
                            nil
                            (lambda (env)
                              (push (cl-cc/prolog::logic-substitute '?out env) results)))
        (cl-cc/prolog::prolog-cut ()))
      (assert-true (>= (length results) 1))
      (let ((remaining (first results)))
        (assert-true (consp remaining))
        (assert-equal :T-RPAREN (caar remaining)))))
  (with-fresh-prolog
    (let ((results nil))
      (handler-case
          (cl-cc/prolog::solve-goal (list 'cl-cc/prolog::dcg-error-recovery nil '?out)
                            nil
                            (lambda (env)
                              (push (cl-cc/prolog::logic-substitute '?out env) results)))
        (cl-cc/prolog::prolog-cut ()))
      (assert-true (member nil results :test #'equal)))))

;;; ─── *dcg-sync-tokens* ─────────────────────────────────────────────────────

(deftest-each dcg-sync-tokens-default
  "Default sync tokens include :T-RPAREN, :T-SEMI, :T-EOF."
  :cases (("rparen" :T-RPAREN)
          ("semi"   :T-SEMI)
          ("eof"    :T-EOF))
  (tok)
  (assert-true (member tok cl-cc/prolog::*dcg-sync-tokens*)))

;;; ─── %dcg-skip-loop unit tests ──────────────────────────────────────────────

(deftest-each dcg-skip-loop-cases
  "%dcg-skip-loop: nil remaining yields nil; stops at sync token; skips non-sync."
  :cases (("nil-remaining"    nil                                              nil)
          ("at-sync-token"    (list (cons :T-RPAREN ")"))                     (list (cons :T-RPAREN ")")))
          ("skip-to-sync"     (list (cons :T-INT 1) (cons :T-RPAREN ")"))     (list (cons :T-RPAREN ")"))))
  (remaining expected)
  (let ((collected nil))
    (cl-cc/prolog::%dcg-skip-loop '?out nil
                                   (lambda (env)
                                     (push (cl-cc/prolog::logic-substitute '?out env) collected))
                                   remaining)
    (assert-equal expected (first (last collected)))))

(deftest dcg-skip-loop-multiple-non-sync
  "%dcg-skip-loop: skips all non-sync tokens before stopping at sync."
  (let* ((input (list (cons :T-INT 1) (cons :T-PLUS "+") (cons :T-EOF nil)))
         (collected nil))
    (cl-cc/prolog::%dcg-skip-loop '?out nil
                                   (lambda (env)
                                     (push (cl-cc/prolog::logic-substitute '?out env) collected))
                                   input)
    (assert-= 1 (length collected))
    (assert-equal (list (cons :T-EOF nil)) (first collected))))

;;; ─── %dcg-star-loop unit tests ──────────────────────────────────────────────

(deftest dcg-star-loop-epsilon-offered-immediately
  "%dcg-star-loop: offers epsilon (s-out = s-in) before any consumption."
  (with-fresh-prolog
    (cl-cc/prolog::def-fact (tok-int ((:T-INT . ?v) . ?rest) ?rest))
    (let* ((input (dcg-input :T-INT 1 :T-INT 2))
           (solutions nil))
      (cl-cc/prolog::%dcg-star-loop 'tok-int '?s-out
                                    (lambda (env)
                                      (push (cl-cc/prolog::logic-substitute '?s-out env) solutions))
                                    input nil)
      (assert-true (member input solutions :test #'equal)))))

(deftest dcg-star-loop-terminates-on-no-progress
  "%dcg-star-loop: does not loop infinitely when rule makes no progress."
  (with-fresh-prolog
    (cl-cc/prolog::def-fact (always-nil () ()))
    (let ((solutions nil))
      (cl-cc/prolog::%dcg-star-loop 'always-nil '?s-out
                                    (lambda (env)
                                      (push (cl-cc/prolog::logic-substitute '?s-out env) solutions))
                                    '((:T-INT . 1)) nil)
      (assert-true (>= (length solutions) 1)))))

;;; ─── dcg-transform-body-element ──────────────────────────────────────────────

(deftest-each dcg-transform-body-element-cases
  "dcg-transform-body-element: terminal generates match goals; symbol generates call goal."
  :cases (("symbol"        'expr          "s0" "s1" 1)
          ("symbol-list"   '(expr arg1)   "s0" "s1" 1)
          ("empty-terminal" '(terminal)   "s0" "s1" 1)
          ("one-terminal"  '(terminal :A) "s0" "s1" 2))
  (element s-in s-out expected-goal-count)
  (cl-cc/prolog::dcg-reset-counter)
  (let ((goals (cl-cc/prolog::dcg-transform-body-element element s-in s-out)))
    (assert-true (listp goals))
    (assert-= expected-goal-count (length goals))))

(deftest dcg-transform-body-element-brace
  "dcg-transform-body-element brace case: emits goal + (= s-in s-out)."
  (cl-cc/prolog::dcg-reset-counter)
  (let ((goals (cl-cc/prolog::dcg-transform-body-element '(brace (integer-p ?x)) "s0" "s1")))
    (assert-= 2 (length goals))
    (assert-equal '(integer-p ?x) (first goals))
    (assert-equal '= (first (second goals)))))

;;; ─── dcg-transform-body ──────────────────────────────────────────────────────

(deftest dcg-transform-body-empty-body
  "dcg-transform-body: empty body unifies s-in and s-out."
  (cl-cc/prolog::dcg-reset-counter)
  (let ((goals (cl-cc/prolog::dcg-transform-body nil "s0" "s1")))
    (assert-= 1 (length goals))
    (assert-equal '= (first (first goals)))))

(deftest dcg-transform-body-single-element
  "dcg-transform-body: single symbol element produces exactly one call goal."
  (cl-cc/prolog::dcg-reset-counter)
  (let ((goals (cl-cc/prolog::dcg-transform-body '(expr) "s0" "s1")))
    (assert-= 1 (length goals))
    (assert-equal 'expr (first (first goals)))))

(deftest dcg-transform-body-two-elements-chains-states
  "dcg-transform-body: two elements share a fresh intermediate state variable."
  (cl-cc/prolog::dcg-reset-counter)
  (let ((goals (cl-cc/prolog::dcg-transform-body '(expr term) "s0" "s1")))
    (assert-= 2 (length goals))
    ;; intermediate state: third arg of first goal = second arg of second goal
    (let ((mid1 (third (first goals)))
          (mid2 (second (second goals))))
      (assert-equal mid1 mid2))))
