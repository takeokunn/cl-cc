(in-package :cl-cc/prolog)

;;; CL-CC Prolog Implementation with Full Backtracking

;;; Logic Variables

(defun logic-var-p (x)
  "Check if X is a logic variable (symbols starting with ?)."
  (and (symbolp x)
       (> (length (symbol-name x)) 0)
       (char= (char (symbol-name x) 0) #\?)))

;;; Enhanced Unification with Occurs Check

(defun occurs-check (var term env)
  "Check if VAR occurs in TERM (prevents infinite structures like ?X = f(?X))."
  (cond ((logic-var-p term)
         (let ((binding (assoc term env)))
           (if binding
               (occurs-check var (cdr binding) env)
               (eq var term))))
        ((consp term)
         (or (occurs-check var (car term) env)
             (occurs-check var (cdr term) env)))
        (t nil)))

(defun unify-failed-p (result)
  "Return T if RESULT represents a unification failure (distinguished from empty env)."
  (eq result :unify-fail))

(defun %logic-binding (var env)
  "Return VAR's binding pair in ENV, or NIL when VAR is unbound."
  (assoc var env))

(defun %bind-logic-var (var term env)
  "Extend ENV with VAR bound to TERM, unless that would create a cycle."
  (if (occurs-check var term env)
      :unify-fail
      (acons var term env)))

(defun %unify-bound-logic-var (var term env)
  "Unify TERM with VAR's current binding in ENV, or bind VAR if unbound."
  (let ((binding (%logic-binding var env)))
    (if binding
        (unify (cdr binding) term env)
        (%bind-logic-var var term env))))

(defun %unify-two-logic-vars (term1 term2 env)
  "Unify two logic variables, preserving aliases already present in ENV."
  (let ((v1 (%logic-binding term1 env))
        (v2 (%logic-binding term2 env)))
    (cond ((and v1 v2) (unify (cdr v1) (cdr v2) env))
          (v1 (unify (cdr v1) term2 env))
          (v2 (unify term1 (cdr v2) env))
          (t (acons term1 term2 env)))))

(defun %unify-cons-terms (term1 term2 env)
  "Unify TERM1 and TERM2 component-wise when both are cons cells."
  (let ((env1 (unify (car term1) (car term2) env)))
    (if (unify-failed-p env1)
        :unify-fail
        (unify (cdr term1) (cdr term2) env1))))

(defun unify (term1 term2 &optional (env nil))
  "Unify two terms, returning updated environment or :UNIFY-FAIL on failure.
   TERM1 and TERM2 can be atoms, logic variables (?x), or cons cells.
   NOTE: Returns NIL for a successful empty environment (not failure — use
   unify-failed-p to distinguish failure from an empty environment)."
  (cond
    ;; Both are logic variables
    ((and (logic-var-p term1) (logic-var-p term2))
     (%unify-two-logic-vars term1 term2 env))
    ;; term1 is logic variable
    ((logic-var-p term1)
     (%unify-bound-logic-var term1 term2 env))
    ;; term2 is logic variable
    ((logic-var-p term2)
     (unify term2 term1 env))
    ;; Both are cons cells — use unify-failed-p to distinguish nil-env from failure
    ((and (consp term1) (consp term2))
     (%unify-cons-terms term1 term2 env))
    ;; Both are equal atoms
    ((equal term1 term2) env)
    ;; Unification failure
    (t :unify-fail)))

;;; Variable Substitution

(defun logic-substitute (template env)
  "Substitute logic variables in TEMPLATE using bindings from ENV."
  (cond
    ((logic-var-p template)
     (let ((entry (assoc template env)))
       (if entry 
           (logic-substitute (cdr entry) env)
           template)))
    ((consp template)
     (cons (logic-substitute (car template) env)
           (logic-substitute (cdr template) env)))
    (t template)))

(defun substitute-variables (term env)
  "Substitute all bound logic variables in TERM (alias for logic-substitute)."
  (logic-substitute term env))

;;; Goal and Rule Representation

(defstruct (prolog-goal (:conc-name goal-))
  "Represents a Prolog goal to be solved."
  predicate   ; the predicate symbol (e.g., 'member, 'append)
  args)       ; list of arguments

(defstruct (prolog-rule (:conc-name rule-))
  "Represents a Prolog rule or fact."
  head        ; head of the rule (predicate with arguments)
  (body nil)) ; body (list of goals), nil for facts

;;; Variable Renaming for Recursion

(defun rename-variables (rule)
  "Rename all logic variables in RULE to fresh ones (for recursive calls)."
  (let ((renaming (make-hash-table :test 'eq)))
    (labels ((rename-term (term)
               (cond ((logic-var-p term)
                      (or (gethash term renaming)
                          (setf (gethash term renaming)
                              (gensym (symbol-name term)))))
                     ((consp term)
                      (cons (rename-term (car term)) (rename-term (cdr term))))
                     (t term))))
      (make-prolog-rule :head (rename-term (rule-head rule))
                        :body (mapcar #'rename-term (rule-body rule))))))

(declaim (ftype function solve-goal solve-conjunction eval-lisp-condition))

;;; Cut Operator Support

(define-condition prolog-cut (condition)
  ()
  (:documentation "Condition signaled when cut (!) is encountered."))

;;; Built-in predicate dispatch moved to prolog-builtins.lisp.

;;; Backtracking Solver using Continuations
