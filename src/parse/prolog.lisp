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
          (if body
              (solve-conjunction body new-env k)
              (funcall k new-env))
        (prolog-cut ()
          (return-from %solve-prolog-rule :cut))))))

(defun %make-builtin-predicates ()
  (let ((ht (make-hash-table :test 'eq)))
    (dolist (spec (if (boundp '*builtin-predicate-specs*)
                      *builtin-predicate-specs*
                      '((! prolog-cut-handler)
                        (and prolog-and-handler)
                        (or prolog-or-handler)
                        (= prolog-unify-handler)
                        (/= prolog-not-unify-handler)
                        (:when prolog-when-handler)
                        (when prolog-when-handler)))
                  ht)
      (destructuring-bind (predicate handler) spec
        (setf (gethash predicate ht) (symbol-function handler))))))

(defvar *builtin-predicates* nil
  "Hash table mapping built-in predicate symbols to CPS handler functions.")

(eval-when (:load-toplevel :execute)
  (setf *builtin-predicates* (%make-builtin-predicates)))

;;; Backtracking Solver using Continuations

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
        (return-from solve-goal)))));; solve-conjunction is already CPS — passes accumulated env to k

(defun solve-conjunction (goals env k)
  "Solve a conjunction of goals (AND). Call K when all goals succeed."
  (if (null goals)
      (funcall k env)
      (solve-goal (car goals) env
                  (lambda (new-env)
                    (solve-conjunction (cdr goals) new-env k)))))

(defun subst-for-eval (form env)
  "Like substitute-variables, but wraps substituted non-self-evaluating
   symbols in (quote ...) so they survive CL eval, and skips (quote ...) forms."
  (cond
    ((logic-var-p form)
     (let ((val (logic-substitute form env)))
       (if (logic-var-p val)
           val  ; still unbound — leave as symbol (will likely cause eval error)
           (if (and (symbolp val) val (not (keywordp val)))
               `(quote ,val)
               val))))
    ((and (consp form) (eq (car form) 'quote))
     form)  ; don't recurse into quoted forms
    ((consp form)
     (cons (subst-for-eval (car form) env)
           (subst-for-eval (cdr form) env)))
    (t form)))

(defun eval-lisp-condition (condition env)
  "Evaluate a Lisp condition embedded in Prolog rules."
  ;; Substitute logic variables using current bindings before evaluating.
  ;; subst-for-eval quotes substituted symbols so they are treated as literals.
  (handler-case
      (let ((substituted (subst-for-eval condition env)))
        (typecase substituted
          (cons (our-eval substituted))
          (t substituted)))
    (error () nil)))

;;; Query Interface

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

(defun %remove-self-move-p (instruction)
  "Return true when INSTRUCTION is a redundant self move."
  (and (consp instruction)
       (eq (car instruction) :move)
       (eql (cadr instruction) (caddr instruction))))

(defun %match-peephole-rule (rule current next)
  "Return replacement instructions when RULE matches CURRENT and NEXT."
  (destructuring-bind (cur-pat next-pat result-list) rule
    (let ((env (unify cur-pat current nil)))
      (unless (unify-failed-p env)
        (let ((env2 (unify next-pat next env)))
          (unless (unify-failed-p env2)
            (mapcar (lambda (tmpl)
                      (logic-substitute tmpl env2))
                    result-list)))))))

(defun %maybe-peephole-rewrite (current next)
  "Try all peephole rules for CURRENT/NEXT and return replacements if one matches."
  (dolist (rule *peephole-rules*)
    (let ((replacements (%match-peephole-rule rule current next)))
      (when replacements
        (return replacements)))))

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

;;; Built-in Predicates
;;;
;;; NOTE: These predicates use the Prolog cons-functor term representation:
;;;   (cons head tail) — NOT CL list notation.
;;; To query member, pass e.g. (member ?x (cons 1 (cons 2 nil))).

;; member/2: (member ?elem (cons head tail))
(def-rule (member ?x (cons ?x ?rest)))
(def-rule (member ?x (cons ?y ?rest))
          (member ?x ?rest))

;; append/3: (append list1 list2 ?result)
(def-rule (append nil ?l ?l))
(def-rule (append (cons ?x ?l1) ?l2 (cons ?x ?l3))
          (append ?l1 ?l2 ?l3))

;; reverse/2: (reverse list ?result)
(def-rule (reverse nil nil))
(def-rule (reverse (cons ?x ?xs) ?result)
          (reverse ?xs ?rev-xs)
          (append ?rev-xs (cons ?x nil) ?result))

;; length/2: (length list ?n)
(def-rule (length nil 0))
(def-rule (length (cons ?x ?rest) (+ 1 ?n))
          (length ?rest ?n))

;;; Type Inference Rules for the Compiler

;; Integer constant
(def-rule (type-of (const ?val) ?env (integer-type))
          (when (integerp ?val)))

;; Variable lookup
(def-rule (type-of (var ?name) ?env ?type)
          (env-lookup ?env ?name ?type))

;; Binary operations
(def-rule (type-of (binop ?op ?a ?b) ?env (integer-type))
          (type-of ?a ?env (integer-type))
          (type-of ?b ?env (integer-type))
          (:when (cl:member ?op '(+ - * / mod))))

;; Comparison operations
(def-rule (type-of (cmp ?op ?a ?b) ?env (boolean-type))
          (type-of ?a ?env (integer-type))
          (type-of ?b ?env (integer-type))
          (:when (cl:member ?op '(< > <= >= = /=))))

;; If expression
(def-rule (type-of (if ?cond ?then ?else) ?env ?type)
          (type-of ?cond ?env (boolean-type))
          (type-of ?then ?env ?type)
          (type-of ?else ?env ?type))

;; Environment lookup
(def-rule (env-lookup (cons (cons ?name ?type) ?rest) ?name ?type))
(def-rule (env-lookup (cons ?binding ?rest) ?name ?type)
          (env-lookup ?rest ?name ?type))

(defun apply-prolog-peephole (instructions)
  "Apply Prolog-unification peephole rules over two-instruction windows.

   Rule format: each rule in *peephole-rules* is a three-element list
     (CURRENT-PATTERN NEXT-PATTERN REPLACEMENT-LIST)
   On a match, both instructions are consumed and REPLACEMENT-LIST sexps emitted.
   Self-moves (:move :Rx :Rx) are removed in a pre-pass."
  (labels ((walk (rest out)
              (cond
                ((null rest) (nreverse out))
                ((null (cdr rest)) (nreverse (cons (car rest) out)))
                (t
                 (let ((curr (car rest))
                       (next (cadr rest)))
                   (let ((replacements (%maybe-peephole-rewrite curr next)))
                     (if replacements
                         (walk (cddr rest)
                               (let ((acc out))
                                 (dolist (r replacements acc)
                                   (push r acc))
                                 acc))
                         (walk (cdr rest) (cons curr out)))))))))
    (walk (remove-if #'%remove-self-move-p instructions) nil)))
