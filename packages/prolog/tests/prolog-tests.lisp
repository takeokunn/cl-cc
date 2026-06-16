;;;; tests/integration/prolog-tests.lisp — Prolog integration tests

(in-package :cl-cc/test)

;; These tests mutate or depend on the global Prolog DB, so they run under the
;; serial integration tree with explicit DB reset/restore around each example.
(defsuite cl-cc-prolog-integration-suite
  :description "Serial Prolog integration tests with isolated rule DB state"
  :parent cl-cc-integration-serial-suite)

(defparameter *prolog-integration-db-snapshot* nil)

(defbefore :each (cl-cc-prolog-integration-suite)
  (setf *prolog-integration-db-snapshot*
        (%snapshot-hash-table cl-cc/prolog:*prolog-rules*))
  (clrhash cl-cc/prolog:*prolog-rules*))

(defafter :each (cl-cc-prolog-integration-suite)
  (clrhash cl-cc/prolog:*prolog-rules*)
  (%restore-hash-table cl-cc/prolog:*prolog-rules*
                       *prolog-integration-db-snapshot*)
  (setf *prolog-integration-db-snapshot* nil))

(in-suite cl-cc-prolog-integration-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; solve-goal — built-in predicates
;;; ─────────────────────────────────────────────────────────────────────────

(deftest prolog-builtin-unification-succeeds
  "Built-in = unifies and calls continuation once"
  (assert-prolog-binding= '(= ?x 42) '?x 42))

(deftest-each prolog-non-unification-cases
  "Built-in /=: succeeds when terms differ, fails when equal"
  :cases (("succeeds" '(/= 1 2) 1)
          ("fails"    '(/= 1 1) 0))
  (goal expected-count)
  (assert-prolog-query-count= goal expected-count))

(deftest prolog-builtin-conjunction
  "Built-in 'and' chains goals and accumulates bindings"
  (with-prolog-single-solution (env '(and (= ?x 1) (= ?y 2)))
    (assert-= 1 (cl-cc:logic-substitute '?x env))
    (assert-= 2 (cl-cc:logic-substitute '?y env))))

(deftest prolog-builtin-disjunction
  "Built-in 'or' tries each alternative"
  (assert-prolog-query-count= '(or (= ?x 1) (= ?x 2)) 2))

(deftest prolog-unknown-predicate-fails-cleanly
  "An unknown predicate yields no solutions instead of erroring."
  (assert-prolog-query-count= '(nonexistent-predicate-xyz) 0))

(deftest-each prolog-builtin-when
  "Built-in :when: succeeds for truthy, fails for falsy"
  :cases (("true"  '(:when t)   1)
          ("false" '(:when nil) 0))
  (goal expected-count)
  (assert-prolog-query-count= goal expected-count))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Cut operator
;;; ─────────────────────────────────────────────────────────────────────────

(deftest prolog-cut-stops-backtracking
  "Cut (!) stops further alternatives in the parent clause"
  (with-fresh-prolog
    (cl-cc/prolog:def-fact (color red))
    (cl-cc/prolog:def-fact (color green))
    (cl-cc/prolog:def-fact (color blue))
    (cl-cc:def-rule (first-color ?c) (color ?c) !)
    (assert-prolog-query-count= '(first-color ?c) 1)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Standard database predicates — cons-functor notation
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each prolog-member-behavior
  "member/2: hit, miss, and full enumeration"
  :cases (("hit"      1 '(member 2  (cons 1 (cons 2 (cons 3 nil)))))
          ("miss"     0 '(member 99 (cons 1 (cons 2 nil))))
          ("enumerate" 3 '(member ?x (cons 1 (cons 2 (cons 3 nil))))))
  (expected goal)
  (assert-prolog-query-count= goal expected))

(deftest-each prolog-append-known-inputs
  "append/3: concatenating two cons-lists yields the expected result"
  :cases (("nil+3"    nil                          '(cons 1 (cons 2 (cons 3 nil))))
          ("one+two"  '(cons a nil)                '(cons b (cons c nil))))
  (l1 l2)
  (assert-prolog-query-count= `(append ,l1 ,l2 ?r) 1))

(deftest-each prolog-reverse-queries
  "reverse/2: produces exactly one solution"
  :cases (("empty"     '(reverse nil ?r))
          ("singleton" '(reverse (cons 1 nil) ?r)))
  (goal)
  (assert-prolog-query-count= goal 1))

(deftest-each prolog-length-queries
  "length/2: produces exactly one solution"
  :cases (("empty" '(length nil ?n))
          ("three" '(length (cons a (cons b (cons c nil))) ?n)))
  (goal)
  (assert-prolog-query-count= goal 1))

;;; ─────────────────────────────────────────────────────────────────────────
;;; solve-conjunction
;;; ─────────────────────────────────────────────────────────────────────────

(deftest prolog-solve-conjunction-empty-succeeds-once
  "solve-conjunction with no goals calls the continuation exactly once"
  (let ((count 0))
    (cl-cc/prolog::solve-conjunction nil nil (lambda (env) (declare (ignore env)) (incf count)))
    (assert-= 1 count)))

(deftest prolog-solve-conjunction-accumulates-bindings
  "solve-conjunction chains goals, accumulating bindings"
  (let ((envs nil))
    (cl-cc/prolog::solve-conjunction '((= ?x 10) (= ?y 20)) nil (lambda (env) (push env envs)))
    (assert-= 1 (length envs))
    (assert-= 10 (cl-cc:logic-substitute '?x (car envs)))
    (assert-= 20 (cl-cc:logic-substitute '?y (car envs)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; query-one / query-all / query-first-n
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each prolog-query-one-result
  "query-one returns the first substitution on success, nil when no solution exists."
  :cases (("success"  '(member ?x (cons a (cons b nil)))  t)
          ("failure"  '(member z  (cons a (cons b nil)))  nil))
  (goal expected-truthy)
  (let ((result (cl-cc:query-one goal)))
    (assert-equal expected-truthy (not (null result)))))

(deftest prolog-query-all-count
  "query-all returns every solution"
  (assert-prolog-query-count= '(member ?x (cons 1 (cons 2 (cons 3 (cons 4 nil))))) 4))

(deftest-each prolog-query-first-n
  "query-first-n returns exactly N solutions"
  :cases (("n=1" 1) ("n=2" 2) ("n=3" 3))
  (n)
  (let ((results (cl-cc:query-first-n '(member ?x (cons a (cons b (cons c (cons d (cons e nil)))))) n)))
    (assert-= n (length results))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; User-defined predicates via def-fact / def-rule
;;; ─────────────────────────────────────────────────────────────────────────

(deftest prolog-def-fact-queryable
  "def-fact registers a ground fact that query-all can find"
  (with-prolog-facts ((likes alice bob) (likes bob carol))
    (assert-prolog-query-count= '(likes ?who bob) 1)))

(deftest prolog-def-rule-one-level
  "def-rule resolves one inference step"
  (with-fresh-prolog
    (cl-cc/prolog:def-fact (parent tom mary))
    (cl-cc/prolog:def-fact (parent tom john))
    (cl-cc:def-rule (child ?c ?p) (parent ?p ?c))
    (assert-prolog-query-count= '(child ?c tom) 2)))

(deftest prolog-def-rule-transitive
  "def-rule supports multi-hop inference"
  (with-fresh-prolog
    (cl-cc/prolog:def-fact (parent tom mary))
    (cl-cc/prolog:def-fact (parent mary ann))
    (cl-cc:def-rule (ancestor ?a ?d) (parent ?a ?d))
    (cl-cc:def-rule (ancestor ?a ?d) (parent ?a ?m) (ancestor ?m ?d))
    (assert-prolog-query-count= '(ancestor tom ?d) 2)))
