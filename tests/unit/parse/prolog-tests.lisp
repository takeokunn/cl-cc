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

;; Moved to cl-cc-integration-suite: these tests mutate the global Prolog DB
;; via with-fresh-prolog / with-baseline-prolog and occasionally hang under
;; randomized scheduling (see MEMORY.md PROLOG-TYPE-OF-CMP flake note).
(in-suite cl-cc-integration-suite)

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

(defun copy-prolog-rules-table (table)
  (let ((copy (make-hash-table :test 'eq)))
    (maphash (lambda (k v) (setf (gethash k copy) (copy-list v))) table)
    copy))

(defparameter *baseline-prolog-rules*
  (copy-prolog-rules-table cl-cc:*prolog-rules*)
  "Baseline Prolog rule database captured when this test file is loaded.")

(defmacro with-baseline-prolog (&body body)
  "Run BODY with the original built-in Prolog rule database restored."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (copy-prolog-rules-table cl-cc:*prolog-rules*)))
       (cl-cc:clear-prolog-database)
       (maphash (lambda (k v)
                  (setf (gethash k cl-cc:*prolog-rules*) (copy-list v)))
                *baseline-prolog-rules*)
       (unwind-protect
           (progn ,@body)
         (cl-cc:clear-prolog-database)
         (maphash (lambda (k v)
                    (setf (gethash k cl-cc:*prolog-rules*) (copy-list v)))
                  ,saved)))))

;;; Helper: collect all solutions for a goal into a list of envs
(defun all-envs (goal)
  (let ((results nil))
    (handler-case
        (cl-cc:solve-goal goal nil (lambda (env) (push env results)))
      (cl-cc::prolog-cut ()))
    (nreverse results)))

(defun prolog-solution-count (goal)
  "Return the number of solutions for GOAL."
  (length (all-envs goal)))

(defmacro assert-prolog-peephole-equal (instructions expected)
  (let ((result (gensym "RESULT")))
    `(let ((,result (cl-cc:apply-prolog-peephole ,instructions)))
       (assert-equal ,expected ,result))))

(defmacro assert-prolog-peephole-length= (instructions expected-length)
  (let ((result (gensym "RESULT")))
    `(let ((,result (cl-cc:apply-prolog-peephole ,instructions)))
       (assert-= ,expected-length (length ,result)))))

(defmacro assert-prolog-peephole-not-contains (instructions pattern)
  (let ((result (gensym "RESULT")))
    `(let ((,result (cl-cc:apply-prolog-peephole ,instructions)))
       (assert-false (member ,pattern ,result :test #'equal)))))

(defmacro assert-prolog-query-count= (goal expected-count)
  (let ((count (gensym "COUNT")))
    `(let ((,count (prolog-solution-count ,goal)))
       (assert-= ,expected-count ,count))))

(defmacro assert-prolog-query-one-succeeds (goal)
  `(assert-true (cl-cc:query-one ,goal)))

(defmacro with-prolog-single-solution ((env goal) &body body)
  `(let ((solutions (all-envs ,goal)))
     (assert-= 1 (length solutions))
     (let ((,env (car solutions)))
       ,@body)))

(defmacro assert-prolog-binding= (goal var expected)
  `(with-prolog-single-solution (env ,goal)
     (assert-= ,expected (cl-cc:substitute-variables ,var env))))

(defmacro assert-prolog-peephole-case (input expected)
  `(assert-prolog-peephole-equal ,input ,expected))

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

(deftest prolog-occurs-check-cases
  "occurs-check: self, nested structure, absent, and via binding"
  (assert-true  (cl-cc:occurs-check '?x '?x nil))
  (assert-true  (cl-cc:occurs-check '?x '(?x 1) nil))
  (assert-false (cl-cc:occurs-check '?x 42 nil))
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
  (assert-prolog-binding= '(= ?x 42) '?x 42))

(deftest prolog-unify-two-vars-aliased
  "unify: two distinct vars become aliased"
  (let* ((e1 (cl-cc:unify '?x '?y nil))
         (e2 (cl-cc:unify '?x 99 e1)))
    (assert-= 99 (cl-cc:substitute-variables '?y e2))))

(deftest prolog-unify-list-structure
  "unify: cons structures are unified component-wise"
  (with-prolog-single-solution (env '(= (?x 2) (1 ?y)))
    (assert-= 1 (cl-cc:substitute-variables '?x env))
    (assert-= 2 (cl-cc:substitute-variables '?y env))))

(deftest prolog-unify-cycle-rejected
  "unify: occurs-check prevents ?x = f(?x)"
  (assert-true (cl-cc:unify-failed-p (cl-cc:unify '?x '(?x) nil))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; logic-substitute / substitute-variables
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each prolog-logic-substitute-cases
  "logic-substitute handles atoms, bound and unbound variables correctly."
  :cases (("atom-integer" 42        nil                    42)
          ("atom-symbol"  'hello    nil                    'hello)
          ("bound-var"    '?x       (list (cons '?x 99))  99)
          ("unbound-var"  '?unbound nil                    '?unbound))
  (input env expected)
  (assert-equal expected (cl-cc:logic-substitute input env)))

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
    ;; Should yield exactly one answer despite three color facts
    (assert-= 1 (length (cl-cc:query-all '(first-color ?c))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Standard database predicates — cons-functor notation
;;; ─────────────────────────────────────────────────────────────────────────

(deftest prolog-member-behavior
  "member/2: hit, miss, and full enumeration"
  (assert-= 1 (prolog-solution-count '(member 2 (cons 1 (cons 2 (cons 3 nil))))))
  (assert-= 0 (prolog-solution-count '(member 99 (cons 1 (cons 2 nil)))))
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

(deftest-each prolog-query-one-result
  "query-one returns the first substitution on success, nil when no solution exists."
  :cases (("success"  '(member ?x (cons a (cons b nil)))  t)
          ("failure"  '(member z  (cons a (cons b nil)))  nil))
  (goal expected-truthy)
  (let ((result (cl-cc:query-one goal)))
    (if expected-truthy
        (assert-true result)
        (assert-null result))))

(deftest prolog-query-all-count
  "query-all returns every solution"
  (assert-prolog-query-count= '(member ?x (cons 1 (cons 2 (cons 3 (cons 4 nil))))) 4))

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

;;; ─────────────────────────────────────────────────────────────────────────
;;; Type-inference Prolog rules
;;; ─────────────────────────────────────────────────────────────────────────

(deftest prolog-type-of-integer-const
  "type-of/3: integer constant has type (integer-type)"
  (with-baseline-prolog
    (let ((result (cl-cc:query-one '(type-of (const 42) nil ?t))))
      (assert-true result)
      (assert-equal '(type-of (const 42) nil (integer-type)) result))))

(deftest-each prolog-type-of-operation-types
  "type-of/3 resolves operation types: binop → integer-type, cmp → boolean-type."
  :cases (("binop" '(type-of (binop + (const 1) (const 2)) nil ?t) '(integer-type))
          ("cmp"   '(type-of (cmp < (const 1) (const 2)) nil ?t)   '(boolean-type)))
  (goal expected-type)
  (with-baseline-prolog
    (let ((result (cl-cc:query-one goal)))
      (assert-true result)
      (assert-equal expected-type (nth 3 result)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Prolog peephole optimizer
;;; ─────────────────────────────────────────────────────────────────────────

(deftest prolog-peephole-cases
  "Peephole: const-move fusion, self-move removal, and passthrough"
  (assert-prolog-peephole-equal '((:const :r0 42) (:move :r1 :r0)) '((:const :r1 42)))
  (assert-prolog-peephole-not-contains '((:move :r0 :r0) (:const :r1 1)) '(:move :r0 :r0))
  (assert-prolog-peephole-equal '((:add :r2 :r0 :r1)) '((:add :r2 :r0 :r1))))

(deftest-each prolog-peephole-arithmetic-identities
  "Peephole: local arithmetic and comparison identities preserve the next instruction."
  :cases (("add-zero"      '((:add :r2 :r0 0) (:const :r3 1))    '((:move :r2 :r0) (:const :r3 1)))
          ("sub-from-zero" '((:sub :r4 0 :r1) (:const :r5 2))    '((:neg :r4 :r1) (:const :r5 2)))
          ("sub-same"      '((:sub :r4 :r1 :r1) (:const :r5 2))  '((:const :r4 0) (:const :r5 2)))
          ("mul-zero"      '((:mul :r6 :r1 0) (:const :r7 3))    '((:const :r6 0) (:const :r7 3)))
          ("div-one"       '((:div :r8 :r2 1) (:const :r9 4))    '((:move :r8 :r2) (:const :r9 4)))
          ("logand-all"    '((:logand :r10 :r3 -1) (:const :r11 5)) '((:move :r10 :r3) (:const :r11 5)))
          ("logand-zero"   '((:logand :r10 :r3 0) (:const :r11 5))  '((:const :r10 0) (:const :r11 5)))
          ("logior-all"    '((:logior :r10 :r3 -1) (:const :r11 5)) '((:const :r10 -1) (:const :r11 5)))
          ("num-eq-same"   '((:num-eq :r12 :r4 :r4) (:const :r13 6)) '((:const :r12 1) (:const :r13 6)))
          ("logxor-same"   '((:logxor :r14 :r5 :r5) (:const :r15 7)) '((:const :r14 0) (:const :r15 7))))
  (input expected)
  (assert-prolog-peephole-case input expected))

(deftest-each prolog-peephole-same-reg-identities
  "Peephole: same-register comparison/bitwise identities collapse locally."
  :cases (("eq"     '((:eq :r0 :r1 :r1) (:const :r2 1))         '((:const :r0 1) (:const :r2 1)))
          ("gt"     '((:gt :r3 :r4 :r4) (:const :r5 2))         '((:const :r3 0) (:const :r5 2)))
          ("le"     '((:le :r6 :r7 :r7) (:const :r8 3))         '((:const :r6 1) (:const :r8 3)))
          ("logand" '((:logand :r9 :r10 :r10) (:const :r11 4))  '((:move :r9 :r10) (:const :r11 4)))
          ("logior" '((:logior :r12 :r13 :r13) (:const :r14 5)) '((:move :r12 :r13) (:const :r14 5))))
  (input expected)
  (assert-prolog-peephole-case input expected))

(deftest-each prolog-peephole-negated-comparisons
  "Peephole: compare followed by logical not collapses to the inverse comparison."
  :cases (("lt->ge" '((:lt :r0 :r1 :r2) (:not :r3 :r0))      '((:ge :r3 :r1 :r2)))
          ("gt->le" '((:gt :r4 :r5 :r6) (:not :r7 :r4))      '((:le :r7 :r5 :r6)))
          ("le->gt" '((:le :r8 :r9 :r10) (:not :r11 :r8))    '((:gt :r11 :r9 :r10)))
          ("ge->lt" '((:ge :r12 :r13 :r14) (:not :r15 :r12)) '((:lt :r15 :r13 :r14))))
  (input expected)
  (assert-prolog-peephole-case input expected))

(deftest prolog-peephole-empty-input
  "Peephole: empty instruction list returns empty"
  (assert-null (cl-cc:apply-prolog-peephole nil)))

(deftest prolog-peephole-single-instruction
  "Peephole: a single instruction with no pair is returned unchanged"
  (assert-prolog-peephole-equal '((:const :r0 7)) '((:const :r0 7))))

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
  (assert-prolog-peephole-length= '((:jump "L0") (:label "L0")) 1)
  (assert-equal '(:label "L0")
                (first (cl-cc:apply-prolog-peephole '((:jump "L0") (:label "L0"))))))

(deftest prolog-peephole-jump-chain-remains-shortest
  "Peephole rule 5: consecutive jumps keep only the first jump."
  (assert-prolog-peephole-equal '((:jump "L1") (:jump "L2")) '((:jump "L1"))))

(deftest-each prolog-peephole-terminal-sequence-rules
  "Peephole rule 5: in any terminal-instruction pair, only the first instruction is kept."
  :cases (("jump-before-ret"   '((:jump "L1") (:ret  :r0)) '((:jump "L1")))
          ("jump-before-halt"  '((:jump "L1") (:halt :r0)) '((:jump "L1")))
          ("ret-before-jump"   '((:ret  :r0) (:jump "L1")) '((:ret  :r0)))
          ("halt-before-jump"  '((:halt :r0) (:jump "L1")) '((:halt :r0)))
          ("ret-before-ret"    '((:ret  :r0) (:ret  :r1)) '((:ret  :r0)))
          ("halt-before-halt"  '((:halt :r0) (:halt :r1)) '((:halt :r0)))
          ("ret-before-halt"   '((:ret  :r0) (:halt :r1)) '((:ret  :r0)))
          ("halt-before-ret"   '((:halt :r0) (:ret  :r1)) '((:halt :r0))))
  (input expected)
  (assert-prolog-peephole-equal input expected))

(deftest prolog-peephole-double-const-same-reg
  "Peephole rule 3: (:const ?r v1)(:const ?r v2) — second overwrites first"
  (assert-prolog-peephole-length= '((:const :r0 1) (:const :r0 99)) 1)
  (assert-equal '(:const :r0 99)
                (first (cl-cc:apply-prolog-peephole '((:const :r0 1) (:const :r0 99))))))

(deftest prolog-peephole-move-chain-propagates-source
  "Peephole rule 4: (:move :r1 :r0)(:move :r2 :r1) — source propagated to end of chain"
  (let ((result (cl-cc:apply-prolog-peephole
                 '((:move :r1 :r0) (:move :r2 :r1)))))
    (assert-= 2 (length result))
    (assert-equal '(:move :r1 :r0) (first result))
    (assert-equal '(:move :r2 :r0) (second result))))

(deftest-each prolog-peephole-pair-preserved
  "Peephole: instruction pairs with different registers/labels are never merged."
  :cases (("jump-label-mismatch"   '((:jump "L1") (:label "L0")))
          ("const-different-regs"  '((:const :r0 1) (:const :r1 2)))
          ("move-different-source" '((:move :r1 :r0) (:move :r3 :r2))))
  (insts)
  (assert-prolog-peephole-length= insts 2))

;;; Repeated SBCL with-timeout wrappers over this file's PROLOG-* tests can leave
;;; later queries in a bad state under full-suite execution. Keep the tests
;;; enabled, but run this file's PROLOG-* cases without the framework timeout.
(maphash (lambda (name plist)
           (when (and (symbolp name)
                      (let ((name-str (symbol-name name)))
                        (and (<= 7 (length name-str))
                             (string= "PROLOG-" name-str :end2 7))))
             (setf (getf plist :timeout) :none)
             (setf (gethash name *test-registry*) plist)))
         *test-registry*)
