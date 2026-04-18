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
                cl-cc/prolog::*builtin-predicate-specs*))

(deftest prolog-data-builds-dispatch-table-from-specs
  "The symbol dispatch helper resolves handler symbols into callable functions."
  (let* ((table (cl-cc/prolog::%make-symbol-dispatch-table
                 '((foo cl-cc/prolog::prolog-cut-handler))))
         (handler (gethash 'foo table)))
    (assert-true handler)
    (assert-eq (symbol-function 'cl-cc/prolog::prolog-cut-handler) handler)))

(deftest prolog-data-peephole-rules-present
  "The peephole rule data remains available after the data/logic split."
  (assert-true (listp cl-cc/prolog::*peephole-rules*))
  (assert-true (>= (length cl-cc/prolog::*peephole-rules*) 30))
  (assert-equal '((:const cl-cc/prolog::?src cl-cc/prolog::?val)
                  (:move cl-cc/prolog::?dst cl-cc/prolog::?src)
                  ((:const cl-cc/prolog::?dst cl-cc/prolog::?val)))
                (first cl-cc/prolog::*peephole-rules*)))

;;; P5: %atomic-cut-goal-p

(deftest-each prolog-builtin-atomic-cut-goal-p
  "%atomic-cut-goal-p: true only for the bare symbol !."
  :cases (("cut-symbol"  '!                                                         t)
          ("non-cut-sym" 'foo                                                       nil)
          ("goal-struct" (cl-cc/prolog::make-prolog-goal :predicate '! :args nil)   nil)
          ("integer"     42                                                          nil)
          ("list"        '(list)                                                     nil)
          ("nil"         nil                                                         nil))
  (term expected)
  (if expected
      (assert-true  (cl-cc/prolog::%atomic-cut-goal-p term))
      (assert-false (cl-cc/prolog::%atomic-cut-goal-p term))))

;;; P6a: %goal-predicate-and-args — struct branch

(deftest prolog-builtin-goal-predicate-and-args-list
  "For a raw list goal, returns (car . cdr)."
  (multiple-value-bind (pred args)
      (cl-cc/prolog::%goal-predicate-and-args '(member ?x (1 2)))
    (assert-eq 'member pred)
    (assert-equal '(?x (1 2)) args)))

(deftest prolog-builtin-goal-predicate-and-args-struct
  "For a prolog-goal struct, returns (goal-predicate . goal-args)."
  (let ((goal (cl-cc/prolog::make-prolog-goal :predicate 'foo :args '(1 2))))
    (multiple-value-bind (pred args)
        (cl-cc/prolog::%goal-predicate-and-args goal)
      (assert-eq 'foo pred)
      (assert-equal '(1 2) args))))

;;; P6b: %invoke-builtin-goal — predicate miss

(deftest prolog-builtin-invoke-missing-predicate
  "Invoking a non-existent builtin returns NIL (no handler found)."
  (assert-null (cl-cc/prolog::%invoke-builtin-goal 'nonexistent-predicate-xyz nil nil (lambda (env) env))))

(deftest-each prolog-logic-var-p
  "logic-var-p recognises ?-prefixed symbols as logic variables."
  :cases (("?x"    '?x      t)
          ("?foo"  '?foo    t)
          ("x"     'x      nil)
          ("42"    42      nil)
          ("nil"   nil     nil))
  (term expected)
  (if expected
      (assert-true  (cl-cc:logic-var-p term))
      (assert-false (cl-cc:logic-var-p term))))

(deftest-each prolog-occurs-check-simple-cases
  "occurs-check: same-variable, nested structure, and absent cases with empty env."
  :cases (("self"    t   '?x '?x    nil)
          ("nested"  t   '?x '(?x 1) nil)
          ("absent"  nil '?x 42      nil))
  (expected var term env)
  (if expected
      (assert-true  (cl-cc:occurs-check var term env))
      (assert-false (cl-cc:occurs-check var term env))))

(deftest prolog-occurs-check-via-binding
  "occurs-check: follows binding chain — ?x occurs in ?y when ?y is bound to ?x."
  (let ((env (list (cons '?y '?x))))
    (assert-true (cl-cc:occurs-check '?x '?y env))))

(deftest-each prolog-unify-atoms
  "unify: atomic terms unify iff they are equal."
  :cases (("same int"     42  42  t)
          ("same sym"     'a  'a  t)
          ("diff ints"     1   2  nil)
          ("int vs sym"   42  'a  nil))
  (t1 t2 ok)
  (let ((env (cl-cc:unify t1 t2 nil)))
    (if ok
        (assert-false (cl-cc:unify-failed-p env))
        (assert-true  (cl-cc:unify-failed-p env)))))

(deftest prolog-unify-var-binds-to-atom
  "unify: logic variable binds to an atom."
  (assert-= 42 (cl-cc:substitute-variables '?x (cl-cc:unify '?x 42 nil))))

(deftest prolog-unify-two-vars-aliased
  "unify: two distinct vars become aliased."
  (let* ((e1 (cl-cc:unify '?x '?y nil))
         (e2 (cl-cc:unify '?x 99 e1)))
    (assert-= 99 (cl-cc:substitute-variables '?y e2))))

(deftest prolog-unify-list-structure
  "unify: cons structures are unified component-wise."
  (let ((solutions nil))
    (cl-cc:solve-goal '(= (?x 2) (1 ?y)) nil (lambda (env) (push env solutions)))
    (assert-= 1 (length solutions))
    (assert-= 1 (cl-cc:substitute-variables '?x (car solutions)))
    (assert-= 2 (cl-cc:substitute-variables '?y (car solutions)))))

(deftest prolog-unify-cycle-rejected
  "unify: occurs-check prevents ?x = f(?x)."
  (assert-true (cl-cc:unify-failed-p (cl-cc:unify '?x '(?x) nil))))

(deftest-each prolog-logic-substitute-cases
  "logic-substitute handles atoms, bound and unbound variables correctly."
  :cases (("atom-integer" 42        nil                    42)
          ("atom-symbol"  'hello    nil                    'hello)
          ("bound-var"    '?x       (list (cons '?x 99))  99)
          ("unbound-var"  '?unbound nil                    '?unbound))
  (input env expected)
  (assert-equal expected (cl-cc:logic-substitute input env)))

(deftest prolog-substitute-traverses-structure
  "substitute-variables traverses nested cons structure."
  (let ((env (cl-cc:unify '?x '(a b c) nil)))
    (assert-equal '((a b c) ?y)
                  (cl-cc:substitute-variables '(?x ?y) env))))

(deftest prolog-substitute-follows-chain
  "logic-substitute follows a chain of variable bindings."
  (let* ((e1 (cl-cc:unify '?a '?b nil))
         (e2 (cl-cc:unify '?b 7 e1)))
    (assert-= 7 (cl-cc:substitute-variables '?a e2))))

(deftest prolog-rename-variables-fresh-symbols
  "rename-variables gives each logic var a fresh symbol."
  (let* ((rule    (cl-cc:make-prolog-rule :head '(foo ?x ?y) :body '((bar ?x ?z))))
         (renamed (cl-cc:rename-variables rule)))
    (assert-false (eq '?x (second (cl-cc:rule-head renamed))))
    (assert-true  (cl-cc:logic-var-p (second (cl-cc:rule-head renamed))))))

(deftest prolog-rename-variables-consistent
  "rename-variables keeps the same fresh name across head/body references."
  (let* ((rule    (cl-cc:make-prolog-rule :head '(foo ?x) :body '((bar ?x))))
         (renamed (cl-cc:rename-variables rule)))
    (let ((head-x (second (cl-cc:rule-head renamed)))
          (body-x (second (car (cl-cc:rule-body renamed)))))
      (assert-eq head-x body-x))))

