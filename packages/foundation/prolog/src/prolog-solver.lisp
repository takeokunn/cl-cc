(in-package :cl-cc/prolog)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Prolog — Solver and Query Interface
;;;
;;; Contains: solve-goal, solve-conjunction, subst-for-eval,
;;; eval-lisp-condition, %solve-goal-with-cut, %collect-query-solutions,
;;; query-all/one/first-n.
;;;
;;; Core terms/unification live in prolog.lisp. Built-in predicate dispatch and
;;; rule application helpers live in prolog-builtins.lisp. Peephole rewriting
;;; lives in prolog-peephole.lisp.
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

(defun solve-goal (goal env k)
  "Solve GOAL in environment ENV, call continuation K with each solution.
   GOAL can be a prolog-goal object, a list (predicate arg1 arg2 ...), or a
   bare atom like ! (cut).
   K is a continuation function that receives the new environment."
  (when (%atomic-cut-goal-p goal)
    (funcall k env)
    (signal 'prolog-cut)
    (return-from solve-goal))
  (multiple-value-bind (predicate args)
      (%goal-predicate-and-args goal)
    (when (%invoke-builtin-goal predicate args env k)
      (return-from solve-goal))
    (dolist (rule (gethash predicate *prolog-rules*))
      (when (eq (%solve-prolog-rule rule args env k) :cut)
        (return-from solve-goal)))))

(defun solve-conjunction (goals env k)
  "Solve a conjunction of goals (AND). Call K when all goals succeed."
  (if (null goals)
      (funcall k env)
      (handler-case
          (let ((result (solve-goal (car goals) env
                                    (lambda (new-env)
                                      (solve-conjunction (cdr goals) new-env k)))))
            (when (eq result :cut)
              (return-from solve-conjunction :cut)))
        (prolog-cut ()
          (return-from solve-conjunction :cut)))))

(defun subst-for-eval (form env)
  "Like substitute-variables, but wraps substituted non-self-evaluating
   symbols in (quote ...) so they survive CL eval, and skips (quote ...) forms."
  (cond
    ((logic-var-p form)
     (let ((val (logic-substitute form env)))
       (if (logic-var-p val)
           val
           (if (and (symbolp val) val (not (keywordp val)))
               `(quote ,val)
               val))))
    ((and (consp form) (eq (car form) 'quote))
     form)
    ((consp form)
     (cons (subst-for-eval (car form) env)
           (subst-for-eval (cdr form) env)))
    (t form)))

(defun eval-lisp-condition (condition env)
  "Evaluate a Lisp condition embedded in Prolog rules."
  (handler-case
      (let ((substituted (subst-for-eval condition env)))
        (typecase substituted
          (cons (our-eval substituted))
          (t substituted)))
    (error () nil)))

(defun %solve-goal-with-cut (goal continuation)
  "Run GOAL with CONTINUATION and swallow PROLOG-CUT exits."
  (handler-case
      (solve-goal goal nil continuation)
    (prolog-cut ())))

(defun %collect-query-solutions (goal &optional limit)
  "Collect substituted solutions for GOAL, optionally stopping after LIMIT."
  (let ((solutions nil)
        (count 0))
    (%solve-goal-with-cut
     goal
     (lambda (env)
       (when (or (null limit) (< count limit))
         (push (substitute-variables goal env) solutions)
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
