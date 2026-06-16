(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

(deftest prolog-data-built-in-handler-specs
  "The built-in predicate table stays data-only and exposes the expected handlers."
  (assert-equal `((cl-cc/prolog::! cl-cc/prolog::prolog-cut-handler)
                  (,(find-symbol "AND" :cl) cl-cc/prolog::prolog-and-handler)
                  (,(find-symbol "OR" :cl) cl-cc/prolog::prolog-or-handler)
                  (,(find-symbol "=" :cl) cl-cc/prolog::prolog-unify-handler)
                  (,(find-symbol "/=" :cl) cl-cc/prolog::prolog-not-unify-handler)
                  (:when cl-cc/prolog::prolog-when-handler)
                  (,(find-symbol "WHEN" :cl) cl-cc/prolog::prolog-when-handler))
                cl-cc/prolog:*builtin-predicate-specs*))

(deftest prolog-builtins-builds-dispatch-table-from-specs
  "The built-in table constructor resolves handler symbols into callable functions."
  (let* ((specs '((foo cl-cc/prolog::prolog-cut-handler)))
         (table (cl-cc/prolog::make-builtin-predicate-table specs)))
    (let ((handler (gethash 'foo table)))
      (assert-true handler)
      (assert-eq (symbol-function 'cl-cc/prolog::prolog-cut-handler) handler))))

(deftest prolog-unify-handler-behavior
  "prolog-unify-handler succeeds only when terms can be unified."
  (let ((successes nil))
    (cl-cc/prolog::prolog-unify-handler '(?x 1) nil (lambda (env) (push env successes)))
    (assert-= 1 (length successes))
    (assert-= 1 (cl-cc:logic-substitute '?x (first successes))))
  (let ((failures nil))
    (cl-cc/prolog::prolog-unify-handler '(?x (?x)) nil (lambda (env) (push env failures)))
    (assert-= 0 (length failures))))

(deftest prolog-data-peephole-rules-present
  "The peephole rule data remains available after the data/logic split."
  (assert-true (listp cl-cc/prolog:*peephole-rules*))
  (assert-true (>= (length cl-cc/prolog:*peephole-rules*) 30))
  (assert-equal '((:const cl-cc/prolog::?src cl-cc/prolog::?val)
                  (:move cl-cc/prolog::?dst cl-cc/prolog::?src)
                  ((:const cl-cc/prolog::?dst cl-cc/prolog::?val)))
                (first cl-cc/prolog:*peephole-rules*)))

(deftest prolog-solve-goal-accepts-raw-list-goals
  "solve-goal handles raw list goals through the public query path."
  (with-fresh-prolog
    (cl-cc/prolog:def-fact (goal-shape 1 1))
    (assert-prolog-query-count= '(goal-shape 1 1) 1)))

(deftest-each prolog-logic-var-p
  "logic-var-p recognises ?-prefixed symbols as logic variables."
  :cases (("?x"    '?x      t)
          ("?foo"  '?foo    t)
          ("x"     'x      nil)
          ("42"    42      nil)
          ("nil"   nil     nil))
  (term expected)
  (assert-prolog-boolean-case expected (cl-cc:logic-var-p term)))

(deftest-each prolog-occurs-check-simple-cases
  "occurs-check: same-variable, nested structure, and absent cases with empty env."
  :cases (("self"    t   '?x '?x    nil)
          ("nested"  t   '?x '(?x 1) nil)
          ("absent"  nil '?x 42      nil))
  (expected var term env)
  (assert-prolog-boolean-case expected (cl-cc/prolog::occurs-check var term env)))

(deftest prolog-occurs-check-via-binding
  "occurs-check: follows binding chain — ?x occurs in ?y when ?y is bound to ?x."
  (let ((env (list (cons '?y '?x))))
    (assert-true (cl-cc/prolog::occurs-check '?x '?y env))))

(deftest-each prolog-unify-atoms
  "unify: atomic terms unify iff they are equal."
  :cases (("same int"     42  42  t)
          ("same sym"     'a  'a  t)
          ("diff ints"     1   2  nil)
          ("int vs sym"   42  'a  nil))
  (t1 t2 ok)
  (let ((env (cl-cc:unify t1 t2 nil)))
    (assert-prolog-boolean-case (not ok) (cl-cc:unify-failed-p env))))

(deftest prolog-unify-variable-binding
  "unify: logic variable binds to atom; occurs-check prevents circular unification."
  (assert-= 42 (cl-cc:logic-substitute '?x (cl-cc:unify '?x 42 nil)))
  (assert-true (cl-cc:unify-failed-p (cl-cc:unify '?x '(?x) nil))))

(deftest prolog-unify-two-vars-aliased
  "unify: two distinct vars become aliased."
  (let* ((e1 (cl-cc:unify '?x '?y nil))
         (e2 (cl-cc:unify '?x 99 e1)))
    (assert-= 99 (cl-cc:logic-substitute '?y e2))))

(deftest prolog-unify-list-structure
  "unify: cons structures are unified component-wise."
  (let ((solutions nil))
    (cl-cc:solve-goal '(= (?x 2) (1 ?y)) nil (lambda (env) (push env solutions)))
    (assert-= 1 (length solutions))
    (assert-= 1 (cl-cc:logic-substitute '?x (car solutions)))
    (assert-= 2 (cl-cc:logic-substitute '?y (car solutions)))))

(deftest-each prolog-logic-substitute-cases
  "logic-substitute handles atoms, bound and unbound variables correctly."
  :cases (("atom-integer" 42        nil                    42)
          ("atom-symbol"  'hello    nil                    'hello)
          ("bound-var"    '?x       (list (cons '?x 99))  99)
          ("unbound-var"  '?unbound nil                    '?unbound))
  (input env expected)
  (assert-equal expected (cl-cc:logic-substitute input env)))

(deftest prolog-logic-substitute-traverses-structure-and-chains
  "logic-substitute traverses nested cons structure and follows variable chains."
  (let ((env (cl-cc:unify '?x '(a b c) nil)))
    (assert-equal '((a b c) ?y) (cl-cc:logic-substitute '(?x ?y) env)))
  (let* ((e1 (cl-cc:unify '?a '?b nil))
         (e2 (cl-cc:unify '?b 7 e1)))
    (assert-= 7 (cl-cc:logic-substitute '?a e2))))

(deftest prolog-rename-variables-behavior
  "rename-variables produces fresh symbols distinct from originals and consistent across head/body."
  (let* ((rule1  (cl-cc/prolog::make-prolog-rule :head '(foo ?x ?y) :body '((bar ?x ?z))))
         (fresh1 (cl-cc/prolog::rename-variables rule1)))
    (assert-false (eq '?x (second (cl-cc/prolog::rule-head fresh1))))
    (assert-true  (cl-cc:logic-var-p (second (cl-cc/prolog::rule-head fresh1)))))
  (let* ((rule2  (cl-cc/prolog::make-prolog-rule :head '(foo ?x) :body '((bar ?x))))
         (fresh2 (cl-cc/prolog::rename-variables rule2)))
    (assert-eq (second (cl-cc/prolog::rule-head fresh2))
               (second (car (cl-cc/prolog::rule-body fresh2))))))
