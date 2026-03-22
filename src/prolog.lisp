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

(defun unify (term1 term2 &optional (env nil))
  "Unify two terms, returning updated environment or NIL on failure.
   TERM1 and TERM2 can be atoms, logic variables (?x), or cons cells."
  (cond
    ;; Both are logic variables
    ((and (logic-var-p term1) (logic-var-p term2))
     (let ((v1 (assoc term1 env))
           (v2 (assoc term2 env)))
       (cond ((and v1 v2) (unify (cdr v1) (cdr v2) env))
             (v1 (unify (cdr v1) term2 env))
             (v2 (unify term1 (cdr v2) env))
             (t (acons term1 term2 env)))))
    ;; term1 is logic variable
    ((logic-var-p term1)
     (let ((binding (assoc term1 env)))
       (if binding
           (unify (cdr binding) term2 env)
           (if (occurs-check term1 term2 env)
               nil  ; Cycle detected
               (acons term1 term2 env)))))
    ;; term2 is logic variable
    ((logic-var-p term2)
     (unify term2 term1 env))
    ;; Both are cons cells
    ((and (consp term1) (consp term2))
     (let ((env1 (unify (car term1) (car term2) env)))
       (and env1 (unify (cdr term1) (cdr term2) env1))))
    ;; Both are equal atoms
    ((equal term1 term2) env)
    ;; Unification failure
    (t nil)))

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

;;; Goal and Rule Representation (CLOS)

(defclass prolog-goal ()
  ((predicate :initarg :predicate :reader goal-predicate
              :documentation "The predicate symbol (e.g., 'member, 'append)")
   (args :initarg :args :reader goal-args
         :documentation "List of arguments to the predicate"))
  (:documentation "Represents a Prolog goal to be solved."))

(defclass prolog-rule ()
  ((head :initarg :head :reader rule-head
         :documentation "Head of the rule (predicate with arguments)")
   (body :initarg :body :initform nil :reader rule-body
         :documentation "Body of the rule (list of goals), nil for facts"))
  (:documentation "Represents a Prolog rule or fact."))

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

(defmacro def-fact (head)
  "Define a Prolog fact. Usage: (def-fact (parent tom mary))"
  `(setf (gethash ',(car head) *prolog-rules*)
         (cons (make-instance 'prolog-rule :head ',head)
               (gethash ',(car head) *prolog-rules*))))

(defmacro def-rule (head &body body)
  "Define a Prolog rule. Usage: (def-rule (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))"
  `(setf (gethash ',(car head) *prolog-rules*)
         (cons (make-instance 'prolog-rule :head ',head :body ',body)
               (gethash ',(car head) *prolog-rules*))))

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
      (make-instance 'prolog-rule
                     :head (rename-term (rule-head rule))
                     :body (mapcar #'rename-term (rule-body rule))))))

;;; Cut Operator Support

(defvar *cut-occurred* nil
  "Flag to signal cut across the call stack.")

(define-condition prolog-cut (condition)
  ()
  (:documentation "Condition signaled when cut (!) is encountered."))

;;; Backtracking Solver using Continuations

(defun solve-goal (goal env k)
  "Solve GOAL in environment ENV, call continuation K with each solution.
   GOAL can be a prolog-goal object or a list (predicate arg1 arg2 ...).
   K is a continuation function that receives the new environment."
  (let* ((predicate (if (typep goal 'prolog-goal)
                        (goal-predicate goal)
                        (car goal)))
         (args (if (typep goal 'prolog-goal)
                   (goal-args goal)
                   (cdr goal))))
    ;; Handle cut operator
    (when (eq predicate '!)
      (funcall k env)
      (signal 'prolog-cut)
      (return-from solve-goal))
    ;; Handle conjunction (and) in goal position
    (when (eq predicate 'and)
      (solve-conjunction args env k)
      (return-from solve-goal))
    ;; Handle disjunction (or) in goal position
    (when (eq predicate 'or)
      (dolist (alternative args)
        (solve-goal alternative env k))
      (return-from solve-goal))
    ;; Handle built-in equality
    (when (eq predicate '=)
      (let ((new-env (unify (first args) (second args) env)))
        (when new-env
          (funcall k new-env)))
      (return-from solve-goal))
    ;; Handle built-in not-equal
    (when (eq predicate '/=)
      (let ((v1 (logic-substitute (first args) env))
            (v2 (logic-substitute (second args) env)))
        (when (not (equal v1 v2))
          (funcall k env)))
      (return-from solve-goal))
    ;; Handle built-in when/:when
    (when (member predicate '(:when when))
      (let ((condition (first args)))
        (when (eval-lisp-condition condition env)
          (funcall k env)))
      (return-from solve-goal))
    ;; Regular predicate lookup
    (dolist (rule (gethash predicate *prolog-rules*))
      (let* ((fresh-rule (rename-variables rule))
             (head (rule-head fresh-rule))
             (body (rule-body fresh-rule))
             (new-env (unify args (cdr head) env)))
        (when new-env
          (handler-case
              (if body
                  (solve-conjunction body new-env k)
                  (funcall k new-env))
            (prolog-cut ()
              ;; Cut stops further alternatives for this goal
              (return-from solve-goal))))))))

(defun solve-conjunction (goals env k)
  "Solve a conjunction of goals (AND). Call K when all goals succeed."
  (if (null goals)
      (funcall k env)
      (solve-goal (car goals) env
                  (lambda (new-env)
                    (solve-conjunction (cdr goals) new-env k)))))

(defun eval-lisp-condition (condition env)
  "Evaluate a Lisp condition embedded in Prolog rules."
  (declare (ignore env))
  ;; Substitute variables and evaluate as Lisp
  (handler-case
      (let ((substituted (substitute-variables condition nil)))
        (typecase substituted
          (cons (eval substituted))
          (t substituted)))
    (error () nil)))

;;; Query Interface

(defun query-all (goal)
  "Return all solutions for GOAL as a list of substituted goals.
   GOAL should be a list like (predicate arg1 arg2 ...)."
  (let ((solutions nil))
    (handler-case
        (solve-goal goal nil
                    (lambda (env)
                      (push (substitute-variables goal env) solutions)))
      (prolog-cut ()))
    (nreverse solutions)))

(defun query-one (goal)
  "Return first solution for GOAL, or NIL if no solution exists."
  (block done
    (handler-case
        (solve-goal goal nil
                    (lambda (env)
                      (return-from done (substitute-variables goal env))))
      (prolog-cut ()))
    nil))

(defun query-first-n (goal n)
  "Return the first N solutions for GOAL."
  (let ((solutions nil)
        (count 0))
    (handler-case
        (solve-goal goal nil
                    (lambda (env)
                      (when (< count n)
                        (push (substitute-variables goal env) solutions)
                        (incf count))
                      (when (>= count n)
                        (signal 'prolog-cut))))
      (prolog-cut ()))
    (nreverse solutions)))

;;; Built-in Predicates

;; Member predicate: (member ?x (list 1 2 3))
(def-rule ((member ?x (cons ?x ?rest))))
(def-rule ((member ?x (cons ?y ?rest))
           (member ?x ?rest)))

;; Append predicate: (append (list 1 2) (list 3 4) ?result)
(def-rule ((append nil ?l ?l)))
(def-rule ((append (cons ?x ?l1) ?l2 (cons ?x ?l3))
           (append ?l1 ?l2 ?l3)))

;; Reverse predicate
(def-rule ((reverse nil nil)))
(def-rule ((reverse (cons ?x ?xs) ?result)
           (append ?rev-xs (cons ?x nil) ?result)
           (reverse ?xs ?rev-xs)))

;; Length predicate
(def-rule ((length nil 0)))
(def-rule ((length (cons ?x ?rest) (+ 1 ?n))
           (length ?rest ?n)))

;;; Type Inference Rules for the Compiler

;; Integer constant
(def-rule ((type-of (const ?val) ?env (integer-type))
           (when (integerp ?val))))

;; Variable lookup
(def-rule ((type-of (var ?name) ?env ?type)
           (env-lookup ?env ?name ?type)))

;; Binary operations
(def-rule ((type-of (binop ?op ?a ?b) ?env (integer-type))
           (type-of ?a ?env (integer-type))
           (type-of ?b ?env (integer-type))
           (member ?op (+ - * / mod))))

;; Comparison operations
(def-rule ((type-of (cmp ?op ?a ?b) ?env (boolean-type))
           (type-of ?a ?env (integer-type))
           (type-of ?b ?env (integer-type))
           (member ?op (< > <= >= = /=))))

;; If expression
(def-rule ((type-of (if ?cond ?then ?else) ?env ?type)
           (type-of ?cond ?env (boolean-type))
           (type-of ?then ?env ?type)
           (type-of ?else ?env ?type)))

;; Environment lookup base case
(def-rule ((env-lookup (cons (cons ?name ?type) ?rest) ?name ?type)))
(def-rule ((env-lookup (cons ?binding ?rest) ?name ?type)
           (env-lookup ?rest ?name ?type)))

;;; Peephole Optimizer (Original Implementation Preserved)

(defparameter *peephole-rules*
  '(((:add ?dst ?a ?z)
     (:when (:const ?z 0))
     (:rewrite (:move ?dst ?a)))
    ((:sub ?dst ?a ?z)
     (:when (:const ?z 0))
     (:rewrite (:move ?dst ?a)))
    ((:mul ?dst ?a ?o)
     (:when (:const ?o 1))
     (:rewrite (:move ?dst ?a)))))

(defparameter *enable-prolog-peephole* nil)

(defun apply-prolog-peephole (instructions)
  "Apply tiny prolog-like optimization rules over two-instruction windows."
  (labels ((maybe-rewrite (current next)
             (dolist (rule *peephole-rules* nil)
               (destructuring-bind (pattern (when-clause witness) (rewrite-clause result))
                   rule
                 (declare (ignore when-clause rewrite-clause))
                 (let ((env (unify pattern current nil)))
                   (when env
                     (let ((env2 (unify witness next env)))
                       (when env2
                         (return (logic-substitute result env2)))))))))
           (walk (rest out)
             (cond
               ((null rest) (nreverse out))
               ((null (cdr rest)) (nreverse (cons (car rest) out)))
               (t
                (let* ((curr (car rest))
                       (next (cadr rest))
                       (rewritten (maybe-rewrite curr next)))
                  (if rewritten
                      (walk (cdr rest) (cons rewritten out))
                      (walk (cdr rest) (cons curr out))))))))
    (walk instructions nil)))
