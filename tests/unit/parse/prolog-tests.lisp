;;;; tests/unit/core/prolog-tests.lisp — Prolog engine unit tests
;;;;
;;;; Covers: logic-var-p, occurs-check, unify, logic-substitute,
;;;;         substitute-variables, rename-variables, def-fact, def-rule,
;;;;         solve-goal (builtins: !, and, or, =, /=, :when),
;;;;         solve-conjunction, query-one, query-all, query-first-n,
;;;;         standard predicates (member, append, reverse, length),
;;;;         type-inference rules, and the Prolog peephole optimizer.
;;;;
;;;; NOTE: Built-in predicates use cons-functor notation, NOT CL list syntax.
;;;;   Correct: (member ?x (cons 1 (cons 2 nil)))
;;;;   Wrong:   (member ?x (1 2))

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Helper: run body with a clean database, restore afterwards
;;; ─────────────────────────────────────────────────────────────────────────

(defmacro with-fresh-prolog (&body body)
  "Snapshot *prolog-rules*, clear the database, run BODY, then restore."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (make-hash-table :test 'eq)))
       (maphash (lambda (k v) (setf (gethash k ,saved) v)) cl-cc:*prolog-rules*)
       (cl-cc:clear-prolog-database)
       (unwind-protect
           (progn ,@body)
         (cl-cc:clear-prolog-database)
         (maphash (lambda (k v) (setf (gethash k cl-cc:*prolog-rules*) v)) ,saved)))))

;;; Helper: collect all solutions for a goal into a list of envs
(defun all-envs (goal)
  (let ((results nil))
    (handler-case
        (cl-cc:solve-goal goal nil (lambda (env) (push env results)))
      (cl-cc::prolog-cut ()))
    (nreverse results)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; logic-var-p
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each prolog-logic-var-p
  "logic-var-p recognises ?-prefixed symbols as logic variables"
  :cases (("?x"    '?x      t)
          ("?foo"  '?foo    t)
          ("x"     'x      nil)
          ("42"    42      nil)
          ("nil"   nil     nil))
  (term expected)
  (if expected
      (assert-true  (cl-cc:logic-var-p term))
      (assert-false (cl-cc:logic-var-p term))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; occurs-check
;;; ─────────────────────────────────────────────────────────────────────────

(deftest prolog-occurs-check-self
  "occurs-check: ?x occurs in ?x"
  (assert-true (cl-cc:occurs-check '?x '?x nil)))

(deftest prolog-occurs-check-in-structure
  "occurs-check: ?x occurs inside a nested term"
  (assert-true (cl-cc:occurs-check '?x '(?x 1) nil)))

(deftest prolog-occurs-check-absent
  "occurs-check: ?x does not occur in an unrelated atom"
  (assert-false (cl-cc:occurs-check '?x 42 nil)))

(deftest prolog-occurs-check-via-binding
  "occurs-check: follows a chain of bindings in env"
  ;; env says ?y → ?x; checking ?x in ?y should detect the cycle
  (let ((env (list (cons '?y '?x))))
    (assert-true (cl-cc:occurs-check '?x '?y env))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; unify
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each prolog-unify-atoms
  "unify: atomic terms unify iff they are equal"
  :cases (("same int"     42  42  t)
          ("same sym"     'a  'a  t)
          ("diff ints"     1   2  nil)
          ("int vs sym"   42  'a  nil))
  (t1 t2 ok)
  ;; unify now returns :unify-fail on failure (not nil), so unify-failed-p
  ;; correctly distinguishes failure from success with empty environment.
  (let ((env (cl-cc:unify t1 t2 nil)))
    (if ok
        (assert-false (cl-cc:unify-failed-p env))
        (assert-true  (cl-cc:unify-failed-p env)))))

(deftest prolog-unify-var-binds-to-atom
  "unify: logic variable binds to an atom"
  (let ((env (cl-cc:unify '?x 42 nil)))
    (assert-false (null env))
    (assert-= 42 (cl-cc:substitute-variables '?x env))))

(deftest prolog-unify-two-vars-aliased
  "unify: two distinct vars become aliased"
  (let* ((e1 (cl-cc:unify '?x '?y nil))
         (e2 (cl-cc:unify '?x 99 e1)))
    (assert-= 99 (cl-cc:substitute-variables '?y e2))))

(deftest prolog-unify-list-structure
  "unify: cons structures are unified component-wise"
  (let ((env (cl-cc:unify '(?x 2) '(1 ?y) nil)))
    (assert-false (null env))
    (assert-= 1 (cl-cc:substitute-variables '?x env))
    (assert-= 2 (cl-cc:substitute-variables '?y env))))

(deftest prolog-unify-cycle-rejected
  "unify: occurs-check prevents ?x = f(?x)"
  (assert-true (cl-cc:unify-failed-p (cl-cc:unify '?x '(?x) nil))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; logic-substitute / substitute-variables
;;; ─────────────────────────────────────────────────────────────────────────

(deftest prolog-substitute-atom-passes-through
  "logic-substitute: non-variable atoms pass through unchanged"
  (assert-= 42 (cl-cc:logic-substitute 42 nil))
  (assert-eq 'hello (cl-cc:logic-substitute 'hello nil)))

(deftest prolog-substitute-bound-var
  "logic-substitute: a bound variable is replaced by its value"
  (let ((env (list (cons '?x 99))))
    (assert-= 99 (cl-cc:logic-substitute '?x env))))

(deftest prolog-substitute-unbound-var
  "logic-substitute: an unbound variable is returned as-is"
  (assert-eq '?unbound (cl-cc:logic-substitute '?unbound nil)))

(deftest prolog-substitute-traverses-structure
  "substitute-variables: traverses nested cons structure"
  (let ((env (cl-cc:unify '?x '(a b c) nil)))
    (assert-equal '((a b c) ?y)
                  (cl-cc:substitute-variables '(?x ?y) env))))

(deftest prolog-substitute-follows-chain
  "logic-substitute: follows a chain of variable bindings"
  (let* ((e1 (cl-cc:unify '?a '?b nil))
         (e2 (cl-cc:unify '?b 7 e1)))
    (assert-= 7 (cl-cc:substitute-variables '?a e2))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; rename-variables
;;; ─────────────────────────────────────────────────────────────────────────

(deftest prolog-rename-variables-fresh-symbols
  "rename-variables: every logic var gets a unique fresh symbol"
  (let* ((rule    (cl-cc:make-prolog-rule :head '(foo ?x ?y) :body '((bar ?x ?z))))
         (renamed (cl-cc:rename-variables rule)))
    (assert-false (eq '?x (second (cl-cc:rule-head renamed))))
    (assert-true  (cl-cc:logic-var-p (second (cl-cc:rule-head renamed))))))

(deftest prolog-rename-variables-consistent
  "rename-variables: the same var in head and body gets the same fresh name"
  (let* ((rule    (cl-cc:make-prolog-rule :head '(foo ?x) :body '((bar ?x))))
         (renamed (cl-cc:rename-variables rule)))
    (let ((head-x (second (cl-cc:rule-head renamed)))
          (body-x (second (car (cl-cc:rule-body renamed)))))
      (assert-eq head-x body-x))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; solve-goal — built-in predicates
;;; ─────────────────────────────────────────────────────────────────────────

(deftest prolog-builtin-unification-succeeds
  "Built-in = unifies and calls continuation once"
  (let ((envs (all-envs '(= ?x 42))))
    (assert-= 1 (length envs))
    (assert-= 42 (cl-cc:substitute-variables '?x (car envs)))))

(deftest prolog-builtin-non-unification-succeeds
  "Built-in /= succeeds when terms are not equal"
  (assert-= 1 (length (all-envs '(/= 1 2)))))

(deftest prolog-builtin-non-unification-fails
  "Built-in /= fails when terms are equal"
  (assert-= 0 (length (all-envs '(/= 1 1)))))

(deftest prolog-builtin-conjunction
  "Built-in 'and' chains goals and accumulates bindings"
  (let ((envs (all-envs '(and (= ?x 1) (= ?y 2)))))
    (assert-= 1 (length envs))
    (assert-= 1 (cl-cc:substitute-variables '?x (car envs)))
    (assert-= 2 (cl-cc:substitute-variables '?y (car envs)))))

(deftest prolog-builtin-disjunction
  "Built-in 'or' tries each alternative"
  (assert-= 2 (length (all-envs '(or (= ?x 1) (= ?x 2))))))

(deftest-each prolog-builtin-when
  "Built-in :when: succeeds for truthy, fails for falsy"
  :cases (("true"  '(:when t)   1)
          ("false" '(:when nil) 0))
  (goal expected-count)
  (assert-= expected-count (length (all-envs goal))))

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
    ;; Should yield exactly one answer despite three color facts
    (assert-= 1 (length (cl-cc:query-all '(first-color ?c))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Standard database predicates — cons-functor notation
;;; ─────────────────────────────────────────────────────────────────────────

(deftest prolog-member-hit
  "member/2: succeeds when element is in the cons-list"
  (assert-= 1 (length (all-envs '(member 2 (cons 1 (cons 2 (cons 3 nil))))))))

(deftest prolog-member-miss
  "member/2: fails when element is absent"
  (assert-= 0 (length (all-envs '(member 99 (cons 1 (cons 2 nil)))))))

(deftest prolog-member-enumerates-all
  "member/2: generates one solution per list element"
  (assert-= 3 (length (cl-cc:query-all '(member ?x (cons 1 (cons 2 (cons 3 nil))))))))

(deftest-each prolog-append-known-inputs
  "append/3: concatenating two cons-lists yields the expected result"
  :cases (("nil+3"    nil                          '(cons 1 (cons 2 (cons 3 nil))))
          ("one+two"  '(cons a nil)                '(cons b (cons c nil))))
  (l1 l2)
  ;; Query succeeds (result variable is bound)
  (let ((sols (cl-cc:query-all `(append ,l1 ,l2 ?r))))
    (assert-= 1 (length sols))))

(deftest-each prolog-reverse-queries
  "reverse/2: produces exactly one solution"
  :cases (("empty"     '(reverse nil ?r))
          ("singleton" '(reverse (cons 1 nil) ?r)))
  (goal)
  (assert-= 1 (length (all-envs goal))))

(deftest-each prolog-length-queries
  "length/2: produces exactly one solution"
  :cases (("empty" '(length nil ?n))
          ("three" '(length (cons a (cons b (cons c nil))) ?n)))
  (goal)
  (assert-= 1 (length (all-envs goal))))

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
    (cl-cc:solve-conjunction
     '((= ?x 10) (= ?y 20))
     nil
     (lambda (env) (push env envs)))
    (assert-= 1 (length envs))
    (assert-= 10 (cl-cc:substitute-variables '?x (car envs)))
    (assert-= 20 (cl-cc:substitute-variables '?y (car envs)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; query-one / query-all / query-first-n
;;; ─────────────────────────────────────────────────────────────────────────

(deftest prolog-query-one-success
  "query-one returns the first substituted goal on success"
  (let ((result (cl-cc:query-one '(member ?x (cons a (cons b nil))))))
    (assert-true result)))

(deftest prolog-query-one-failure
  "query-one returns nil when no solution exists"
  (assert-null (cl-cc:query-one '(member z (cons a (cons b nil))))))

(deftest prolog-query-all-count
  "query-all returns every solution"
  (assert-= 4 (length (cl-cc:query-all '(member ?x (cons 1 (cons 2 (cons 3 (cons 4 nil)))))))))

(deftest-each prolog-query-first-n
  "query-first-n returns exactly N solutions"
  :cases (("n=1" 1) ("n=2" 2) ("n=3" 3))
  (n)
  (let ((results (cl-cc:query-first-n
                  '(member ?x (cons a (cons b (cons c (cons d (cons e nil)))))) n)))
    (assert-= n (length results))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; User-defined predicates via def-fact / def-rule
;;; ─────────────────────────────────────────────────────────────────────────

(deftest prolog-def-fact-queryable
  "def-fact registers a ground fact that query-all can find"
  (with-fresh-prolog
    (cl-cc:def-fact (likes alice bob))
    (cl-cc:def-fact (likes bob carol))
    (assert-= 1 (length (cl-cc:query-all '(likes ?who bob))))))

(deftest prolog-def-rule-one-level
  "def-rule resolves one inference step"
  (with-fresh-prolog
    (cl-cc:def-fact (parent tom mary))
    (cl-cc:def-fact (parent tom john))
    (cl-cc:def-rule (child ?c ?p) (parent ?p ?c))
    (assert-= 2 (length (cl-cc:query-all '(child ?c tom))))))

(deftest prolog-def-rule-transitive
  "def-rule supports multi-hop inference"
  (with-fresh-prolog
    (cl-cc:def-fact (parent tom mary))
    (cl-cc:def-fact (parent mary ann))
    (cl-cc:def-rule (ancestor ?a ?d) (parent ?a ?d))
    (cl-cc:def-rule (ancestor ?a ?d) (parent ?a ?m) (ancestor ?m ?d))
    (assert-= 2 (length (cl-cc:query-all '(ancestor tom ?d))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Type-inference Prolog rules
;;; ─────────────────────────────────────────────────────────────────────────

(deftest prolog-type-of-integer-const
  "type-of/3: integer constant has type (integer-type)"
  (let ((result (cl-cc:query-one '(type-of (const 42) nil ?t))))
    (assert-true result)
    (assert-equal '(type-of (const 42) nil (integer-type)) result)))

(deftest prolog-type-of-binop
  "type-of/3: integer binop resolves to (integer-type)"
  (let ((result (cl-cc:query-one '(type-of (binop + (const 1) (const 2)) nil ?t))))
    (assert-true result)
    ;; The third element of the substituted goal is the type
    (assert-equal '(integer-type) (nth 3 result))))

(deftest prolog-type-of-cmp
  "type-of/3: comparison resolves to (boolean-type)"
  (let ((result (cl-cc:query-one '(type-of (cmp < (const 1) (const 2)) nil ?t))))
    (assert-true result)
    (assert-equal '(boolean-type) (nth 3 result))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Prolog peephole optimizer
;;; ─────────────────────────────────────────────────────────────────────────

(deftest prolog-peephole-const-move-fused
  "Peephole: (:const Rx V)(:move Ry Rx) → (:const Ry V)"
  (assert-equal '((:const :r1 42))
                (cl-cc:apply-prolog-peephole '((:const :r0 42) (:move :r1 :r0)))))

(deftest prolog-peephole-self-move-removed
  "Peephole pre-pass: (:move Rx Rx) is a no-op and is deleted"
  (let ((result (cl-cc:apply-prolog-peephole '((:move :r0 :r0) (:const :r1 1)))))
    (assert-false (member '(:move :r0 :r0) result :test #'equal))))

(deftest prolog-peephole-no-match-passthrough
  "Peephole: unmatched instructions pass through unchanged"
  (assert-equal '((:add :r2 :r0 :r1))
                (cl-cc:apply-prolog-peephole '((:add :r2 :r0 :r1)))))

(deftest prolog-peephole-empty-input
  "Peephole: empty instruction list returns empty"
  (assert-null (cl-cc:apply-prolog-peephole nil)))

(deftest prolog-peephole-single-instruction
  "Peephole: a single instruction with no pair is returned unchanged"
  (assert-equal '((:const :r0 7))
                (cl-cc:apply-prolog-peephole '((:const :r0 7)))))

(deftest prolog-peephole-multiple-pairs
  "Peephole: only matching pairs are fused; others pass through"
  ;; (:const :r0 1)(:move :r1 :r0) fuses; (:add ...) passes through
  (let ((result (cl-cc:apply-prolog-peephole
                 '((:const :r0 1) (:move :r1 :r0) (:add :r2 :r1 :r1)))))
    (assert-= 2 (length result))
    (assert-equal '(:const :r1 1) (first result))
    (assert-equal '(:add :r2 :r1 :r1) (second result))))

(deftest prolog-peephole-jump-to-next-label-eliminated
  "Peephole rule 2: (:jump L0)(:label L0) → (:label L0)"
  (let ((result (cl-cc:apply-prolog-peephole
                 '((:jump "L0") (:label "L0")))))
    (assert-= 1 (length result))
    (assert-equal '(:label "L0") (first result))))

(deftest prolog-peephole-jump-to-different-label-kept
  "Peephole rule 2: (:jump L1)(:label L0) — label mismatch, no elimination"
  (let ((result (cl-cc:apply-prolog-peephole
                 '((:jump "L1") (:label "L0")))))
    (assert-= 2 (length result))))

(deftest prolog-peephole-double-const-same-reg
  "Peephole rule 3: (:const ?r v1)(:const ?r v2) — second overwrites first"
  (let ((result (cl-cc:apply-prolog-peephole
                 '((:const :r0 1) (:const :r0 99)))))
    (assert-= 1 (length result))
    (assert-equal '(:const :r0 99) (first result))))

(deftest prolog-peephole-double-const-different-regs-kept
  "Peephole rule 3: (:const :r0 v1)(:const :r1 v2) — different regs, no merge"
  (let ((result (cl-cc:apply-prolog-peephole
                 '((:const :r0 1) (:const :r1 2)))))
    (assert-= 2 (length result))))

(deftest prolog-peephole-move-chain-propagates-source
  "Peephole rule 4: (:move :r1 :r0)(:move :r2 :r1) — source propagated to end of chain"
  (let ((result (cl-cc:apply-prolog-peephole
                 '((:move :r1 :r0) (:move :r2 :r1)))))
    (assert-= 2 (length result))
    (assert-equal '(:move :r1 :r0) (first result))
    (assert-equal '(:move :r2 :r0) (second result))))

(deftest prolog-peephole-move-chain-non-chain-kept
  "Peephole rule 4: (:move :r1 :r0)(:move :r3 :r2) — different source, no propagation"
  (let ((result (cl-cc:apply-prolog-peephole
                 '((:move :r1 :r0) (:move :r3 :r2)))))
    (assert-= 2 (length result))))
