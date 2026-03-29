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

(defun unify (term1 term2 &optional (env nil))
  "Unify two terms, returning updated environment or :UNIFY-FAIL on failure.
   TERM1 and TERM2 can be atoms, logic variables (?x), or cons cells.
   NOTE: Returns NIL for a successful empty environment (not failure — use
   unify-failed-p to distinguish failure from an empty environment)."
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
               :unify-fail  ; Cycle detected
               (acons term1 term2 env)))))
    ;; term2 is logic variable
    ((logic-var-p term2)
     (unify term2 term1 env))
    ;; Both are cons cells — use unify-failed-p to distinguish nil-env from failure
    ((and (consp term1) (consp term2))
     (let ((env1 (unify (car term1) (car term2) env)))
       (if (unify-failed-p env1)
           :unify-fail
           (unify (cdr term1) (cdr term2) env1))))
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

(defmacro def-fact (head)
  "Define a Prolog fact. Usage: (def-fact (parent tom mary))"
  `(add-rule ',(car head) (make-prolog-rule :head ',head)))

(defmacro def-rule (head &body body)
  "Define a Prolog rule. Usage: (def-rule (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))"
  `(add-rule ',(car head) (make-prolog-rule :head ',head :body ',body)))

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

(defvar *builtin-predicates*
  (let ((ht (make-hash-table :test 'eq)))
    (setf (gethash '! ht) #'prolog-cut-handler)
    (setf (gethash 'and ht) #'prolog-and-handler)
    (setf (gethash 'or ht) #'prolog-or-handler)
    (setf (gethash '= ht) #'prolog-unify-handler)
    (setf (gethash '/= ht) #'prolog-not-unify-handler)
    (setf (gethash ':when ht) #'prolog-when-handler)
    (setf (gethash 'when ht) #'prolog-when-handler)
    ht)
  "Hash table mapping built-in predicate symbols to CPS handler functions.")

;;; Backtracking Solver using Continuations

(defun solve-goal (goal env k)
  "Solve GOAL in environment ENV, call continuation K with each solution.
   GOAL can be a prolog-goal object, a list (predicate arg1 arg2 ...), or a
   bare atom like ! (cut).
   K is a continuation function that receives the new environment."
  ;; Handle bare atomic goals (e.g. ! used directly as a body element).
  ;; Compare by symbol-name to be package-agnostic (rules may come from any package).
  (when (and (symbolp goal) (not (prolog-goal-p goal)))
    (when (string= (symbol-name goal) "!")
      (funcall k env)
      (signal 'prolog-cut))
    (return-from solve-goal))
  (let* ((predicate (if (prolog-goal-p goal) (goal-predicate goal) (car goal)))
         (args      (if (prolog-goal-p goal) (goal-args      goal) (cdr goal))))
    ;; Dispatch to built-in handler first
    (let ((builtin (gethash predicate *builtin-predicates*)))
      (when builtin
        (funcall builtin args env k)
        (return-from solve-goal)))
    ;; Regular predicate: try each matching rule in the database
    (dolist (rule (gethash predicate *prolog-rules*))
      (let* ((fresh-rule (rename-variables rule))
             (head       (rule-head fresh-rule))
             (body       (rule-body fresh-rule))
             (new-env    (unify args (cdr head) env)))
        (unless (unify-failed-p new-env)
          (handler-case
              (if body
                  (solve-conjunction body new-env k)
                  (funcall k new-env))
            (prolog-cut ()
              ;; Cut stops further alternatives for this goal
              (return-from solve-goal))))))));; solve-conjunction is already CPS — passes accumulated env to k

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

(defun query-all (goal)
  "Return all solutions for GOAL as a list of substituted goals.
   GOAL should be a list like (predicate arg1 arg2 ...)."
  (let ((solutions nil))
    (%solve-goal-with-cut
     goal
     (lambda (env)
       (push (substitute-variables goal env) solutions)))
    (nreverse solutions)))

(defun query-one (goal)
  "Return first solution for GOAL, or NIL if no solution exists."
  (block done
    (%solve-goal-with-cut
     goal
     (lambda (env)
       (return-from done (substitute-variables goal env))))
    nil))

(defun query-first-n (goal n)
  "Return the first N solutions for GOAL."
  (let ((solutions nil)
        (count 0))
    (%solve-goal-with-cut
     goal
     (lambda (env)
       (when (< count n)
         (push (substitute-variables goal env) solutions)
         (incf count))
       (when (>= count n)
         (signal 'prolog-cut))))
    (nreverse solutions)))

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
          (:when (member ?op '(+ - * / mod))))

;; Comparison operations
(def-rule (type-of (cmp ?op ?a ?b) ?env (boolean-type))
          (type-of ?a ?env (integer-type))
          (type-of ?b ?env (integer-type))
          (:when (member ?op '(< > <= >= = /=))))

;; If expression
(def-rule (type-of (if ?cond ?then ?else) ?env ?type)
          (type-of ?cond ?env (boolean-type))
          (type-of ?then ?env ?type)
          (type-of ?else ?env ?type))

;; Environment lookup
(def-rule (env-lookup (cons (cons ?name ?type) ?rest) ?name ?type))
(def-rule (env-lookup (cons ?binding ?rest) ?name ?type)
          (env-lookup ?rest ?name ?type))

;;; Peephole Optimizer
;;;
;;; Three bugs fixed from the original:
;;;   Bug 1: Wrong instruction order — consts come BEFORE binops in the stream.
;;;   Bug 2: Walker advanced by 1 on match; must advance by 2 (consume both).
;;;   Bug 3: Old rules duplicated opt-simplify-binop (runs first); replaced
;;;          with patterns that survive the structural optimizer.
;;;
;;; Rule format: (current-pattern next-pattern result-list)
;;; result-list is a list of replacement sexps (empty=delete both, one sexp=replace both).

(defparameter *peephole-rules*
  '(;; (:const :R1 42)(:move :R2 :R1) → (:const :R2 42)
    ;; Fires when copy-prop is blocked by a label reset but DCE kept the const alive.
    ((:const ?src ?val) (:move ?dst ?src) ((:const ?dst ?val)))

    ;; (:jump "L0")(:label "L0") → (:label "L0")
    ;; Eliminates a jump to the immediately following label (dead branch after threading).
    ((:jump ?lbl) (:label ?lbl) ((:label ?lbl)))

    ;; (:const ?r ?v1)(:const ?r ?v2) → (:const ?r ?v2)
    ;; Second const-load to the same register makes the first dead.
    ;; Safe in a 2-window because no instruction can read ?r between adjacent instructions.
    ((:const ?r ?_v1) (:const ?r ?v2) ((:const ?r ?v2)))

    ;; (:move ?mid ?src)(:move ?dst ?mid) → (:move ?mid ?src)(:move ?dst ?src)
    ;; Copy-propagation through a move chain: ?mid still gets ?src (in case it
    ;; is read elsewhere), but ?dst now reads directly from ?src, enabling DCE
    ;; to later eliminate ?mid if it has no remaining readers.
    ((:move ?mid ?src) (:move ?dst ?mid) ((:move ?mid ?src) (:move ?dst ?src)))))

(defparameter *enable-prolog-peephole* t)

(defun apply-prolog-peephole (instructions)
  "Apply Prolog-unification peephole rules over two-instruction windows.

   Rule format: each rule in *peephole-rules* is a three-element list
     (CURRENT-PATTERN NEXT-PATTERN REPLACEMENT-LIST)
   On a match, both instructions are consumed and REPLACEMENT-LIST sexps emitted.
   Self-moves (:move :Rx :Rx) are removed in a pre-pass."
  (labels ((filter-self-moves (insns)
             (remove-if (lambda (s)
                          (and (consp s)
                               (eq (car s) :move)
                               (eql (cadr s) (caddr s))))
                        insns))
           (maybe-rewrite (current next)
             (dolist (rule *peephole-rules* (values nil nil))
               (destructuring-bind (cur-pat next-pat result-list) rule
                 (let ((env (unify cur-pat current nil)))
                   (unless (unify-failed-p env)
                     (let ((env2 (unify next-pat next env)))
                       (unless (unify-failed-p env2)
                         (return (values (mapcar (lambda (tmpl)
                                                   (logic-substitute tmpl env2))
                                                 result-list)
                                         t)))))))))
           (walk (rest out)
             (cond
               ((null rest) (nreverse out))
               ((null (cdr rest)) (nreverse (cons (car rest) out)))
               (t
                (let ((curr (car rest))
                      (next (cadr rest)))
                  (multiple-value-bind (replacements consumed-two)
                      (maybe-rewrite curr next)
                    (if consumed-two
                        (walk (cddr rest)
                              (let ((acc out))
                                (dolist (r replacements acc)
                                  (push r acc))
                                acc))
                        (walk (cdr rest) (cons curr out)))))))))
    (walk (filter-self-moves instructions) nil)))

;;; ─── DCG (Definite Clause Grammar) Support ──────────────────────────────────
;;;
;;; DCG rules transform into difference-list Prolog rules.
;;; (name --> body...) becomes a rule with two extra state args.

(defvar *dcg-counter* 0
  "Counter for generating fresh DCG state variables.")

(defun dcg-fresh-var ()
  "Generate a fresh DCG state variable."
  (intern (format nil "?S~D" (incf *dcg-counter*))))

(defun dcg-reset-counter ()
  "Reset the DCG variable counter (for testing)."
  (setf *dcg-counter* 0))

(defun dcg-transform-body-element (element s-in s-out)
  "Transform a single DCG body element into a Prolog goal.
   - A list of terminals: match each token in sequence.
   - A symbol: call as a non-terminal with state threading.
   - A list starting with a symbol: call as a non-terminal with args + state."
  (cond
    ;; Terminal list: [tok1, tok2, ...] — match tokens in order
    ((and (consp element) (eq (car element) 'terminal))
     (let ((terminals (cdr element)))
       (if (null terminals)
           ;; Empty terminal list: s-in = s-out
           (list `(= ,s-in ,s-out))
           ;; Chain terminal matches
           (let ((goals nil)
                 (current s-in))
             (dolist (term terminals)
               (let ((next (dcg-fresh-var)))
                 (push `(= ,current (cons ,term ,next)) goals)
                 (setf current next)))
             ;; Unify final state with s-out
             (push `(= ,current ,s-out) goals)
             (nreverse goals)))))
    ;; Non-terminal symbol
    ((symbolp element)
     (list (list element s-in s-out)))
    ;; Non-terminal with arguments: (nt arg1 arg2 ...)
    ((and (consp element) (symbolp (car element)))
     (list (append element (list s-in s-out))))
    ;; Prolog goal in braces: {goal} — execute without consuming input
    ((and (consp element) (eq (car element) 'brace))
     (let ((goal (cadr element)))
       (list goal `(= ,s-in ,s-out))))
    (t (error "DCG: unknown body element ~S" element))))

(defun dcg-transform-body (body s-in s-out)
  "Transform a DCG body (list of elements) into a list of Prolog goals,
   chaining fresh state variables between elements."
  (if (null body)
      (list `(= ,s-in ,s-out))
      (let ((goals nil)
            (current s-in))
        (dolist (element (butlast body))
          (let ((next (dcg-fresh-var)))
            (setf goals (append goals (dcg-transform-body-element element current next)))
            (setf current next)))
        ;; Last element connects to s-out
        (setf goals (append goals (dcg-transform-body-element (car (last body)) current s-out)))
        goals)))

(defmacro def-dcg-rule (name &body body)
  "Define a DCG rule. Transforms (name --> body...) into a Prolog rule
   with difference-list state threading.
   Usage: (def-dcg-rule expr term (terminal (+)) term)"
  (let ((s-in (gensym "?S-IN"))
        (s-out (gensym "?S-OUT")))
    (let ((transformed-body (dcg-transform-body body s-in s-out)))
      `(def-rule (,name ,s-in ,s-out) ,@transformed-body))))

;;; DCG token matching builtins

;; dcg-token-match: match a token by type (cons of (type . value))
(setf (gethash 'dcg-token-match *builtin-predicates*)
      (lambda (args env k)
        (let* ((expected-type (first args))
               (s-in (second args))
               (s-out (third args))
               (input (logic-substitute s-in env)))
          (when (and (consp input)
                     (consp (car input))
                     (eq (caar input) expected-type))
            (let ((new-env (unify s-out (cdr input) env)))
              (unless (unify-failed-p new-env)
                (funcall k new-env)))))))

;; dcg-token-match-value: match token by type AND bind its value
(setf (gethash 'dcg-token-match-value *builtin-predicates*)
      (lambda (args env k)
        (let* ((expected-type (first args))
               (value-var (second args))
               (s-in (third args))
               (s-out (fourth args))
               (input (logic-substitute s-in env)))
          (when (and (consp input)
                     (consp (car input))
                     (eq (caar input) expected-type))
            (let ((val-env (unify value-var (cdar input) env)))
              (unless (unify-failed-p val-env)
                (let ((new-env (unify s-out (cdr input) val-env)))
                  (unless (unify-failed-p new-env)
                    (funcall k new-env)))))))))

;;; DCG query interface

(defun phrase (rule-name input)
  "Parse INPUT (a list of tokens) with DCG RULE-NAME.
   Returns the first solution's remaining input, or NIL on failure."
  (let ((result nil))
    (%solve-goal-with-cut
     (list rule-name input '?dcg-rest)
     (lambda (env)
       (setf result (logic-substitute '?dcg-rest env))
       (signal 'prolog-cut)))
    result))

(defun phrase-rest (rule-name input)
  "Parse INPUT with DCG RULE-NAME. Returns (values matched-p remaining).
   MATCHED-P is T if parsing succeeded."
  (let ((result nil)
        (matched nil))
    (%solve-goal-with-cut
     (list rule-name input '?dcg-rest)
     (lambda (env)
       (setf result (logic-substitute '?dcg-rest env)
             matched t)
       (signal 'prolog-cut)))
    (values matched result)))

(defun phrase-all (rule-name input)
  "Parse INPUT with DCG RULE-NAME, returning all possible remaining inputs."
  (let ((results nil))
    (%solve-goal-with-cut
     (list rule-name input '?dcg-rest)
     (lambda (env)
       (push (logic-substitute '?dcg-rest env) results)))
    (nreverse results)))
