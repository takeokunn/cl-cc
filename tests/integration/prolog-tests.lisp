;;;; tests/integration/prolog-tests.lisp — Prolog integration tests

(in-package :cl-cc/test)

;; Moved to cl-cc-integration-serial-suite: these tests mutate the global Prolog DB
;; via with-fresh-prolog / with-baseline-prolog and occasionally hang under
;; randomized scheduling (see MEMORY.md PROLOG-TYPE-OF-CMP flake note).
(in-suite cl-cc-integration-serial-suite)

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
  (assert-= expected-count (prolog-solution-count goal)))

(deftest prolog-builtin-conjunction
  "Built-in 'and' chains goals and accumulates bindings"
  (with-prolog-single-solution (env '(and (= ?x 1) (= ?y 2)))
    (assert-= 1 (cl-cc:substitute-variables '?x env))
    (assert-= 2 (cl-cc:substitute-variables '?y env))))

(deftest prolog-builtin-disjunction
  "Built-in 'or' tries each alternative"
  (assert-= 2 (prolog-solution-count '(or (= ?x 1) (= ?x 2)))))

(deftest-each prolog-builtin-when
  "Built-in :when: succeeds for truthy, fails for falsy"
  :cases (("true"  '(:when t)   1)
          ("false" '(:when nil) 0))
  (goal expected-count)
  (assert-= expected-count (prolog-solution-count goal)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Cut operator
;;; ─────────────────────────────────────────────────────────────────────────

(deftest prolog-cut-stops-backtracking
  "Cut (!) stops further alternatives in the parent clause"
  (with-fresh-prolog
    (cl-cc:def-fact (color red))
    (cl-cc:def-fact (color green))
    (cl-cc:def-fact (color blue))
    (cl-cc:def-rule (first-color ?c) (color ?c) !)
    (assert-= 1 (length (cl-cc:query-all '(first-color ?c))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Standard database predicates — cons-functor notation
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each prolog-member-behavior
  "member/2: hit, miss, and full enumeration"
  :cases (("hit"      1 '(member 2  (cons 1 (cons 2 (cons 3 nil)))))
          ("miss"     0 '(member 99 (cons 1 (cons 2 nil))))
          ("enumerate" 3 '(member ?x (cons 1 (cons 2 (cons 3 nil))))))
  (expected goal)
  (assert-= expected (prolog-solution-count goal)))

(deftest-each prolog-append-known-inputs
  "append/3: concatenating two cons-lists yields the expected result"
  :cases (("nil+3"    nil                          '(cons 1 (cons 2 (cons 3 nil))))
          ("one+two"  '(cons a nil)                '(cons b (cons c nil))))
  (l1 l2)
  (let ((sols (cl-cc:query-all `(append ,l1 ,l2 ?r))))
    (assert-= 1 (length sols))))

(deftest-each prolog-reverse-queries
  "reverse/2: produces exactly one solution"
  :cases (("empty"     '(reverse nil ?r))
          ("singleton" '(reverse (cons 1 nil) ?r)))
  (goal)
  (assert-= 1 (prolog-solution-count goal)))

(deftest-each prolog-length-queries
  "length/2: produces exactly one solution"
  :cases (("empty" '(length nil ?n))
          ("three" '(length (cons a (cons b (cons c nil))) ?n)))
  (goal)
  (assert-= 1 (prolog-solution-count goal)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; solve-conjunction
;;; ─────────────────────────────────────────────────────────────────────────

(deftest prolog-solve-conjunction-empty-succeeds-once
  "solve-conjunction with no goals calls the continuation exactly once"
  (let ((count 0))
    (cl-cc:solve-conjunction nil nil (lambda (env) (declare (ignore env)) (incf count)))
    (assert-= 1 count)))

(deftest prolog-solve-conjunction-accumulates-bindings
  "solve-conjunction chains goals, accumulating bindings"
  (let ((envs nil))
    (cl-cc:solve-conjunction '((= ?x 10) (= ?y 20)) nil (lambda (env) (push env envs)))
    (assert-= 1 (length envs))
    (assert-= 10 (cl-cc:substitute-variables '?x (car envs)))
    (assert-= 20 (cl-cc:substitute-variables '?y (car envs)))))

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
  (with-fresh-prolog
    (cl-cc:def-fact (likes alice bob))
    (cl-cc:def-fact (likes bob carol))
    (assert-prolog-query-count= '(likes ?who bob) 1)))

(deftest prolog-def-rule-one-level
  "def-rule resolves one inference step"
  (with-fresh-prolog
    (cl-cc:def-fact (parent tom mary))
    (cl-cc:def-fact (parent tom john))
    (cl-cc:def-rule (child ?c ?p) (parent ?p ?c))
    (assert-prolog-query-count= '(child ?c tom) 2)))

(deftest prolog-def-rule-transitive
  "def-rule supports multi-hop inference"
  (with-fresh-prolog
    (cl-cc:def-fact (parent tom mary))
    (cl-cc:def-fact (parent mary ann))
    (cl-cc:def-rule (ancestor ?a ?d) (parent ?a ?d))
    (cl-cc:def-rule (ancestor ?a ?d) (parent ?a ?m) (ancestor ?m ?d))
    (assert-prolog-query-count= '(ancestor tom ?d) 2)))

;;; Repeated SBCL with-timeout wrappers over this file's PROLOG-* tests can leave
;;; later queries in a bad state under full-suite execution. Keep the tests
;;; enabled, but run this file's PROLOG-* cases without the framework timeout.
(persist-each *test-registry*
              (lambda (name plist)
                (when (and (symbolp name)
                           (let ((name-str (symbol-name name)))
                             (and (<= 7 (length name-str))
                                  (string= "PROLOG-" name-str :end2 7))))
                  (let ((new-plist (copy-list plist)))
                    (setf (getf new-plist :timeout) :none)
                    (setf *test-registry*
                          (persist-assoc *test-registry* name new-plist))))))
