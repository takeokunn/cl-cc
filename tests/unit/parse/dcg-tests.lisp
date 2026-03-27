;;;; tests/unit/parse/dcg-tests.lisp — DCG Engine Unit Tests
;;;;
;;;; Tests for the DCG (Definite Clause Grammar) parsing engine:
;;;; input conversion, builtin predicates (dcg-alt, dcg-opt, dcg-star,
;;;; dcg-plus, dcg-error-recovery), and entry points (dcg-parse, dcg-parse-all).

(in-package :cl-cc/test)

(defsuite dcg-suite :description "DCG engine unit tests")

;;; ─── Helpers ──────────────────────────────────────────────────────────────

(defun dcg-input (&rest type-value-pairs)
  "Build a DCG input list from (type value) pairs.
   Example: (dcg-input :T-INT 1 :T-PLUS \"+\") → ((:T-INT . 1) (:T-PLUS . \"+\"))"
  (loop for (type val) on type-value-pairs by #'cddr
        collect (cons type val)))

;;; ─── lexer-tokens-to-dcg-input ─────────────────────────────────────────────

(deftest dcg-convert-cases
  "lexer-tokens-to-dcg-input converts plist tokens, struct tokens, and passes through other values"
  (let ((tokens (list (list :type :T-INT :value 42)
                      (list :type :T-IDENT :value "x"))))
    (let ((result (cl-cc::lexer-tokens-to-dcg-input tokens)))
      (assert-equal 2 (length result))
      (assert-equal :T-INT (caar result))
      (assert-equal 42 (cdar result))
      (assert-equal :T-IDENT (caadr result))
      (assert-equal "x" (cdadr result))))
  (let ((tokens (list (cl-cc::make-lexer-token :type :T-INT :value 99))))
    (let ((result (cl-cc::lexer-tokens-to-dcg-input tokens)))
      (assert-equal 1 (length result))
      (assert-equal :T-INT (caar result))
      (assert-equal 99 (cdar result))))
  (let ((tokens (list 42)))
    (let ((result (cl-cc::lexer-tokens-to-dcg-input tokens)))
      (assert-equal 1 (length result))
      (assert-equal 42 (first result)))))

;;; ─── dcg-token-to-cst ──────────────────────────────────────────────────────

(deftest dcg-token-to-cst-basic
  "dcg-token-to-cst creates a cst-token from a cons pair."
  (let ((tok (cons :T-INT 42)))
    (let ((cst (cl-cc::dcg-token-to-cst tok 10)))
      (assert-true (cl-cc::cst-token-p cst))
      (assert-equal :T-INT (cl-cc::cst-token-kind cst))
      (assert-equal 42 (cl-cc::cst-token-value cst))
      (assert-equal 10 (cl-cc::cst-token-start-byte cst)))))

;;; ─── dcg-parse / dcg-parse-all ─────────────────────────────────────────────

(deftest dcg-parse-with-fact
  "dcg-parse returns remaining input after matching."
  (with-fresh-prolog
    ;; A trivial DCG rule: consume-one(In, Out) where In = [_ | Out]
    (cl-cc::def-fact (consume-one (?h . ?rest) ?rest))
    (let* ((input (dcg-input :T-INT 1 :T-INT 2))
           (result (cl-cc::dcg-parse 'consume-one input)))
      ;; Should have consumed first token, leaving second
      (assert-equal 1 (length result))
      (assert-equal :T-INT (caar result))
      (assert-equal 2 (cdar result)))))

(deftest dcg-parse-no-match
  "dcg-parse returns nil when no rule matches."
  (with-fresh-prolog
    ;; Rule that never matches (requires input to start with impossible pattern)
    (cl-cc::def-fact (never-match impossible-sentinel ?rest))
    (let ((result (cl-cc::dcg-parse 'never-match (dcg-input :T-INT 1))))
      (assert-null result))))

(deftest dcg-parse-all-multiple
  "dcg-parse-all returns all possible parse results."
  (with-fresh-prolog
    ;; Two overlapping rules
    (cl-cc::def-fact (flexible (?h . ?rest) ?rest))       ;; consume 1
    (cl-cc::def-fact (flexible (?a ?b . ?rest) ?rest))    ;; consume 2
    (let* ((input (dcg-input :T-INT 1 :T-INT 2 :T-INT 3))
           (results (cl-cc::dcg-parse-all 'flexible input)))
      ;; Should get at least 2 results (consume-1 and consume-2)
      (assert-true (>= (length results) 2)))))

;;; ─── DCG Builtins via solve-goal ───────────────────────────────────────────

(deftest dcg-alt-behavior
  "dcg-alt matches the first alternative and falls through to the second"
  (with-fresh-prolog
    (cl-cc::def-fact (rule-a ((:T-INT . ?v) . ?rest) ?rest))
    (cl-cc::def-fact (rule-b ((:T-IDENT . ?v) . ?rest) ?rest))
    (let ((results nil)
          (input (dcg-input :T-INT 42)))
      (handler-case
          (cl-cc::solve-goal (list 'cl-cc::dcg-alt 'rule-a 'rule-b input '?out)
                            nil
                            (lambda (env)
                              (push (cl-cc::logic-substitute '?out env) results)))
        (cl-cc::prolog-cut ()))
      (assert-true (>= (length results) 1))
      ;; After matching rule-a, remaining should be nil
      (assert-null (first results))))
  (with-fresh-prolog
    (cl-cc::def-fact (rule-a ((:T-INT . ?v) . ?rest) ?rest))
    (cl-cc::def-fact (rule-b ((:T-IDENT . ?v) . ?rest) ?rest))
    (let ((results nil)
          (input (dcg-input :T-IDENT "x")))
      (handler-case
          (cl-cc::solve-goal (list 'cl-cc::dcg-alt 'rule-a 'rule-b input '?out)
                            nil
                            (lambda (env)
                              (push (cl-cc::logic-substitute '?out env) results)))
        (cl-cc::prolog-cut ()))
      (assert-true (>= (length results) 1)))))

(deftest dcg-opt-behavior
  "dcg-opt matches when sub-rule matches and succeeds with epsilon when sub-rule fails"
  (with-fresh-prolog
    (cl-cc::def-fact (tok-int ((:T-INT . ?v) . ?rest) ?rest))
    (let ((results nil)
          (input (dcg-input :T-INT 1)))
      (handler-case
          (cl-cc::solve-goal (list 'cl-cc::dcg-opt 'tok-int input '?out)
                            nil
                            (lambda (env)
                              (push (cl-cc::logic-substitute '?out env) results)))
        (cl-cc::prolog-cut ()))
      ;; Should have at least the successful match (empty remaining)
      (assert-true (member nil results :test #'equal))))
  (with-fresh-prolog
    (cl-cc::def-fact (tok-int ((:T-INT . ?v) . ?rest) ?rest))
    (let ((results nil)
          (input (dcg-input :T-IDENT "x")))
      (handler-case
          (cl-cc::solve-goal (list 'cl-cc::dcg-opt 'tok-int input '?out)
                            nil
                            (lambda (env)
                              (push (cl-cc::logic-substitute '?out env) results)))
        (cl-cc::prolog-cut ()))
      ;; Epsilon match: out = input (unconsumed)
      (assert-true (>= (length results) 1)))))

(deftest dcg-star-behavior
  "dcg-star succeeds with zero matches and matches multiple tokens"
  (with-fresh-prolog
    (cl-cc::def-fact (tok-int ((:T-INT . ?v) . ?rest) ?rest))
    (let ((results nil)
          (input (dcg-input :T-IDENT "x")))
      (handler-case
          (cl-cc::solve-goal (list 'cl-cc::dcg-star 'tok-int input '?out)
                            nil
                            (lambda (env)
                              (push (cl-cc::logic-substitute '?out env) results)))
        (cl-cc::prolog-cut ()))
      ;; Zero matches: out = input
      (assert-true (>= (length results) 1))))
  (with-fresh-prolog
    (cl-cc::def-fact (tok-int ((:T-INT . ?v) . ?rest) ?rest))
    (let ((results nil)
          (input (dcg-input :T-INT 1 :T-INT 2 :T-INT 3)))
      (handler-case
          (cl-cc::solve-goal (list 'cl-cc::dcg-star 'tok-int input '?out)
                            nil
                            (lambda (env)
                              (push (cl-cc::logic-substitute '?out env) results)))
        (cl-cc::prolog-cut ()))
      ;; Should include the fully-consumed result (nil)
      (assert-true (member nil results :test #'equal)))))

(deftest dcg-plus-behavior
  "dcg-plus matches one or more tokens and fails with zero matches"
  (with-fresh-prolog
    (cl-cc::def-fact (tok-int ((:T-INT . ?v) . ?rest) ?rest))
    (let ((results nil)
          (input (dcg-input :T-INT 42)))
      (handler-case
          (cl-cc::solve-goal (list 'cl-cc::dcg-plus 'tok-int input '?out)
                            nil
                            (lambda (env)
                              (push (cl-cc::logic-substitute '?out env) results)))
        (cl-cc::prolog-cut ()))
      (assert-true (>= (length results) 1))))
  (with-fresh-prolog
    (cl-cc::def-fact (tok-int ((:T-INT . ?v) . ?rest) ?rest))
    (let ((results nil)
          (input (dcg-input :T-IDENT "x")))
      (handler-case
          (cl-cc::solve-goal (list 'cl-cc::dcg-plus 'tok-int input '?out)
                            nil
                            (lambda (env)
                              (push (cl-cc::logic-substitute '?out env) results)))
        (cl-cc::prolog-cut ()))
      (assert-null results))))

;;; ─── dcg-error-recovery ─────────────────────────────────────────────────────

(deftest dcg-error-recovery-skips-to-sync
  "dcg-error-recovery skips tokens until sync token found."
  (with-fresh-prolog
    (let ((results nil)
          (input (dcg-input :T-INT 1 :T-IDENT "x" :T-RPAREN ")")))
      (handler-case
          (cl-cc::solve-goal (list 'cl-cc::dcg-error-recovery input '?out)
                            nil
                            (lambda (env)
                              (push (cl-cc::logic-substitute '?out env) results)))
        (cl-cc::prolog-cut ()))
      ;; Should stop at :T-RPAREN (a sync token)
      (assert-true (>= (length results) 1))
      (let ((remaining (first results)))
        (assert-true (consp remaining))
        (assert-equal :T-RPAREN (caar remaining))))))

(deftest dcg-error-recovery-empty-input
  "dcg-error-recovery handles empty input."
  (with-fresh-prolog
    (let ((results nil))
      (handler-case
          (cl-cc::solve-goal (list 'cl-cc::dcg-error-recovery nil '?out)
                            nil
                            (lambda (env)
                              (push (cl-cc::logic-substitute '?out env) results)))
        (cl-cc::prolog-cut ()))
      ;; Should succeed with out = nil
      (assert-true (member nil results :test #'equal)))))

;;; ─── *dcg-sync-tokens* ─────────────────────────────────────────────────────

(deftest dcg-sync-tokens-default
  "Default sync tokens include :T-RPAREN, :T-SEMI, :T-EOF."
  (assert-true (member :T-RPAREN cl-cc::*dcg-sync-tokens*))
  (assert-true (member :T-SEMI cl-cc::*dcg-sync-tokens*))
  (assert-true (member :T-EOF cl-cc::*dcg-sync-tokens*)))
