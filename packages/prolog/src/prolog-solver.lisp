(in-package :cl-cc/prolog)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Prolog — Solver and Query Interface
;;;
;;; Contains: solve-goal, solve-conjunction, %with-prolog-cut-handler,
;;; %solve-goal-with-cut, %collect-query-solutions,
;;; query-all/one/first-n.
;;;
;;; Core terms/unification live in prolog-unification.lisp. Built-in predicate
;;; dispatch and Lisp-condition evaluation live in prolog-builtins.lisp. Rule
;;; application helpers live here.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(declaim (ftype function solve-goal solve-conjunction
                %solve-prolog-cut-handler
                %solve-prolog-rule
                %goal-operator-and-args
                %bare-cut-goal-p
                %solve-registered-rules))

(defun %solve-prolog-cut-handler (thunk)
  "Call THUNK and convert PROLOG-CUT into the keyword :CUT."
  (handler-case
      (funcall thunk)
    (prolog-cut ()
      :cut)))

(defun %goal-operator-and-args (goal)
  "Return GOAL's predicate symbol and argument list."
  (values (car goal) (cdr goal)))

(defun %bare-cut-goal-p (goal)
  "Return true when GOAL is the bare cut atom !."
  (and (symbolp goal)
       (string= (symbol-name goal) "!")))

(defun %solve-registered-rules (predicate args env k)
  "Try every registered rule for PREDICATE with ARGS."
  (dolist (rule (gethash predicate *prolog-rules*))
    (when (eq (%solve-prolog-rule rule args env k) :cut)
      (return-from %solve-registered-rules :cut))))

(defun solve-goal (goal env k)
  "Solve GOAL in environment ENV, call continuation K with each solution.
   GOAL should be a list (predicate arg1 arg2 ...) or a bare atom like ! (cut).
   K is a continuation function that receives the new environment."
  (cond
    ((%bare-cut-goal-p goal)
     (funcall k env)
     (signal 'prolog-cut)
     (return-from solve-goal))
    (t
     (multiple-value-bind (predicate args) (%goal-operator-and-args goal)
       (when (%invoke-builtin-goal predicate args env k)
         (return-from solve-goal))
       (when (eq (%solve-registered-rules predicate args env k) :cut)
         (return-from solve-goal))))))

(defun solve-conjunction (goals env k)
  "Solve a conjunction of goals (AND). Call K when all goals succeed."
  (if (null goals)
      (funcall k env)
      (%solve-prolog-cut-handler
       (lambda ()
         (solve-goal (car goals) env
                     (lambda (new-env)
                       (solve-conjunction (cdr goals) new-env k)))))))

(defun %solve-prolog-rule (rule args env k)
  "Attempt to solve RULE against ARGS and call K for each matching environment."
  (let* ((fresh-rule (rename-variables rule))
         (head (rule-head fresh-rule))
         (body (rule-body fresh-rule))
         (new-env (unify args (cdr head) env)))
    (unless (unify-failed-p new-env)
      (%solve-prolog-cut-handler
       (lambda ()
         (if body
             (solve-conjunction body new-env k)
             (funcall k new-env)))))))

(defun %solve-goal-with-cut (goal continuation)
  "Run GOAL with CONTINUATION and swallow PROLOG-CUT exits."
  (%solve-prolog-cut-handler (lambda () (solve-goal goal nil continuation))))

(defun %collect-query-solutions (goal &optional limit)
  "Collect substituted solutions for GOAL, optionally stopping after LIMIT."
  (let ((solutions nil)
        (count 0))
    (%solve-goal-with-cut
     goal
     (lambda (env)
       (when (or (null limit) (< count limit))
         (push (logic-substitute goal env) solutions)
         (incf count)
         (when (and limit (>= count limit))
           (signal 'prolog-cut)))))
    (nreverse solutions)))

(defun query-all (goal)
  "Return all solutions for GOAL as a list of substituted goals.
   GOAL should be a list like (predicate arg1 arg2 ...)."
  (%collect-query-solutions goal))

(defun query-one (goal)
  "Return first solution for GOAL, or NIL if no solution exists."
  (first (%collect-query-solutions goal 1)))

(defun query-first-n (goal n)
  "Return the first N solutions for GOAL."
  (%collect-query-solutions goal n))
