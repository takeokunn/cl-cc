(in-package :cl-cc)
;;; ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
;;; Prolog — Query Interface, Built-in Predicates, and Peephole
;;;
;;; Contains: solve-goal, solve-conjunction, subst-for-eval, eval-lisp-condition,
;;; %solve-goal-with-cut, %collect-query-solutions, peephole helpers
;;; (%remove-self-move-p, %match-peephole-rule, %maybe-peephole-rewrite),
;;; query-all/one/first-n, built-in predicates (member/append/length/nth/last
;;; /reverse/assoc/atomic/compound/functor/arg/=..), type inference Prolog rules
;;; (infer-type, infer-expr-type, apply-prolog-peephole).
;;;
;;; Core engine (logic-var-p, unify, *prolog-rules*, add-rule, rename-variables,
;;; solve-conjunction internals, *builtin-predicates*) is in prolog.lisp (loads before).
;;;
;;; Load order: after prolog.lisp.
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
        (return-from solve-goal)))));; solve-conjunction is already CPS — passes accumulated env to k

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
(def-rule (type-of (binop + ?a ?b) ?env (integer-type))
          (type-of ?a ?env (integer-type))
          (type-of ?b ?env (integer-type)))

(def-rule (type-of (binop - ?a ?b) ?env (integer-type))
          (type-of ?a ?env (integer-type))
          (type-of ?b ?env (integer-type)))

(def-rule (type-of (binop * ?a ?b) ?env (integer-type))
          (type-of ?a ?env (integer-type))
          (type-of ?b ?env (integer-type)))

(def-rule (type-of (binop / ?a ?b) ?env (integer-type))
          (type-of ?a ?env (integer-type))
          (type-of ?b ?env (integer-type)))

(def-rule (type-of (binop mod ?a ?b) ?env (integer-type))
          (type-of ?a ?env (integer-type))
          (type-of ?b ?env (integer-type)))

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
