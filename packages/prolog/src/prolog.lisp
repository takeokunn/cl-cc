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

(defun %bind-logic-var (var term env)
  "Extend ENV with VAR bound to TERM, unless that would create a cycle."
  (if (occurs-check var term env)
      :unify-fail
      (acons var term env)))

(defun %unify-bound-logic-var (var term env)
  "Unify TERM with VAR's current binding in ENV, or bind VAR if unbound."
  (let ((binding (assoc var env)))
    (if binding
        (unify (cdr binding) term env)
        (%bind-logic-var var term env))))

(defun %unify-two-logic-vars (term1 term2 env)
  "Unify two logic variables, preserving aliases already present in ENV."
  (let ((v1 (assoc term1 env))
        (v2 (assoc term2 env)))
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
  "Substitute all bound logic variables in TERM using ENV."
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

;;; Prolog database state

(defvar *prolog-rules* (make-hash-table :test 'eq)
  "Hash table mapping predicate symbols to lists of rules.")

(defun clear-prolog-database ()
  "Clear all rules from the Prolog database."
  (clrhash *prolog-rules*))

(defun add-rule (predicate rule)
  "Add RULE to the database under PREDICATE."
  (setf (gethash predicate *prolog-rules*)
        (cons rule (gethash predicate *prolog-rules*))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro %def-prolog-clause (head &key body)
    (list 'add-rule
          (list 'quote (car head))
          (append (list 'make-prolog-rule :head (list 'quote head))
                  (when body
                    (list :body (list 'quote body))))))

  (defmacro define-prolog-type-rules-from-spec ()
    "Generate all type inference rules from *PROLOG-TYPE-RULE-SPECS*.

Each spec entry has the form (RESULT-TYPE EXPR-KIND OPERATOR-LIST) and expands
to one `def-rule` per operator, keeping rule data and rule expansion separate."
    (cons 'progn
          (loop for (result-type expr-kind operators) in *prolog-type-rule-specs*
                append (mapcar (lambda (op)
                                 (list 'def-rule
                                       (list 'type-of (list expr-kind op '?a '?b) '?env (list result-type))
                                       (list 'type-of '?a '?env '(integer-type))
                                       (list 'type-of '?b '?env '(integer-type))))
                               operators))))

  (defmacro define-prolog-declarative-rules ()
    "Emit `def-rule` forms from *PROLOG-DECLARATIVE-RULE-SPECS*."
    (cons 'progn
          (mapcar (lambda (spec)
                    (destructuring-bind (head &optional body) spec
                      (append (list 'def-rule) head (or body '()))))
                  *prolog-declarative-rule-specs*)))

  (defmacro def-fact (head)
    "Define a Prolog fact. Usage: (def-fact (parent tom mary))"
    (list '%def-prolog-clause head))

  (defmacro def-rule (head &body body)
    "Define a Prolog rule. Usage: (def-rule (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))"
    (list '%def-prolog-clause head :body body)))

;;; Variable Renaming for Recursion

(defun %rename-prolog-term (term renaming)
  "Rename TERM's logic variables via RENAMING hash table (creating new gensyms as needed)."
  (cond ((logic-var-p term)
         (or (gethash term renaming)
             (setf (gethash term renaming) (gensym (symbol-name term)))))
        ((consp term)
         (cons (%rename-prolog-term (car term) renaming)
               (%rename-prolog-term (cdr term) renaming)))
        (t term)))

(defun rename-variables (rule)
  "Rename all logic variables in RULE to fresh ones (for recursive calls)."
  (let ((renaming (make-hash-table :test 'eq)))
    (make-prolog-rule :head (%rename-prolog-term (rule-head rule) renaming)
                      :body (mapcar (lambda (b) (%rename-prolog-term b renaming))
                                    (rule-body rule)))))

(declaim (ftype function solve-goal solve-conjunction eval-lisp-condition))

;;; Cut Operator Support

(define-condition prolog-cut (condition)
  ()
  (:documentation "Condition signaled when cut (!) is encountered."))

;;; Backtracking Solver using Continuations
