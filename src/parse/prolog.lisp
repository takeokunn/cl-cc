(in-package :cl-cc)

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

;;; Prolog Database

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
    `(add-rule ',(car head)
               (make-prolog-rule :head ',head
                                 ,@(when body `(:body ',body)))))

  (defmacro def-fact (head)
    "Define a Prolog fact. Usage: (def-fact (parent tom mary))"
    `(%def-prolog-clause ,head))

  (defmacro def-rule (head &body body)
    "Define a Prolog rule. Usage: (def-rule (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))"
    `(%def-prolog-clause ,head :body ,body)))

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

;;; Built-in Predicate Dispatch Table (data layer)
;;;
;;; Each handler is (lambda (args env k)) — continuation-passing style:
;;; args = predicate arguments, env = current bindings, k = success continuation.

(defun prolog-cut-handler (args env k)
  (declare (ignore args))
  (funcall k env)
  (signal 'prolog-cut))

(defun prolog-and-handler (args env k)
  (solve-conjunction args env k))

(defun prolog-or-handler (args env k)
  (dolist (alt args)
    (solve-goal alt env k)))

(defun prolog-unify-handler (args env k)
  (let ((new-env (unify (first args) (second args) env)))
    (unless (unify-failed-p new-env)
      (funcall k new-env))))

(defun prolog-not-unify-handler (args env k)
  (let ((v1 (logic-substitute (first args) env))
        (v2 (logic-substitute (second args) env)))
    (when (not (equal v1 v2))
      (funcall k env))))

(defun prolog-when-handler (args env k)
  (when (eval-lisp-condition (first args) env)
    (funcall k env)))

(defun %goal-predicate-and-args (goal)
  "Return GOAL's predicate and arg list.
GOAL may be a prolog-goal object or a raw list form."
  (if (prolog-goal-p goal)
      (values (goal-predicate goal) (goal-args goal))
      (values (car goal) (cdr goal))))

(defun %atomic-cut-goal-p (goal)
  "Return true when GOAL denotes the cut operator as a bare atom."
  (and (symbolp goal)
       (not (prolog-goal-p goal))
       (string= (symbol-name goal) "!")))

(defun %invoke-builtin-goal (predicate args env k)
  "Invoke the built-in predicate handler for PREDICATE, if one exists."
  (let ((builtin (gethash predicate *builtin-predicates*)))
    (when builtin
      (funcall builtin args env k)
      t)))

(defun %solve-prolog-rule (rule args env k)
  "Attempt to solve RULE against ARGS and call K for each matching environment."
  (let* ((fresh-rule (rename-variables rule))
         (head (rule-head fresh-rule))
         (body (rule-body fresh-rule))
         (new-env (unify args (cdr head) env)))
    (unless (unify-failed-p new-env)
      (handler-case
          (let ((result (if body
                            (solve-conjunction body new-env k)
                            (funcall k new-env))))
            (when (eq result :cut)
              (return-from %solve-prolog-rule :cut)))
        (prolog-cut ()
          (return-from %solve-prolog-rule :cut))))))

(defun %make-builtin-predicates ()
  "Build the built-in predicate dispatch hash table from *builtin-predicate-specs*.
*builtin-predicate-specs* is defined in prolog-data.lisp, which loads before this file."
  (let ((ht (make-hash-table :test 'eq)))
    (dolist (spec *builtin-predicate-specs* ht)
      (destructuring-bind (predicate handler) spec
        (setf (gethash predicate ht) (symbol-function handler))))))

(defvar *builtin-predicates* nil
  "Hash table mapping built-in predicate symbols to CPS handler functions.")

(eval-when (:load-toplevel :execute)
  (setf *builtin-predicates* (%make-builtin-predicates)))

;;; Backtracking Solver using Continuations
