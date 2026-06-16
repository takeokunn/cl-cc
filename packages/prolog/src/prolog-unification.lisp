;;; packages/prolog/src/prolog-unification.lisp
;;;
;;; Logic variable representation and unification/substitution helpers.

(in-package :cl-cc/prolog)

;;; Logic Variables

(defun logic-var-p (x)
  "Check if X is a logic variable (symbols starting with ?)."
  (and (symbolp x)
       (> (length (symbol-name x)) 0)
       (char= (char (symbol-name x) 0) #\?)))

(defun %walk-prolog-term (term on-variable on-atom on-cons)
  "Walk TERM and dispatch logic variables, atoms, and cons cells through callbacks."
  (cond ((logic-var-p term)
         (funcall on-variable term))
        ((consp term)
         (funcall on-cons term))
        (t
         (funcall on-atom term))))

;;; Enhanced Unification with Occurs Check

(defun %resolve-logic-binding (term env)
  "Return TERM's binding in ENV, or TERM itself when unbound."
  (let ((binding (assoc term env)))
    (if binding (cdr binding) term)))

(declaim (ftype (function (t t &optional t) t) unify))

(defun occurs-check (var term env)
  "Check if VAR occurs in TERM (prevents infinite structures like ?X = f(?X))."
  (labels ((walk (node)
             (%walk-prolog-term node
                                 (lambda (logic-var)
                                   (let ((resolved (%resolve-logic-binding logic-var env)))
                                     (if (eq resolved logic-var)
                                         (eq var logic-var)
                                         (walk resolved))))
                                 (lambda (_atom)
                                   (declare (ignore _atom))
                                   nil)
                                 (lambda (cons-node)
                                   (or (walk (car cons-node))
                                       (walk (cdr cons-node)))))))
    (walk term)))

(defun unify-failed-p (result)
  "Return T if RESULT represents a unification failure (distinguished from empty env)."
  (eq result :unify-fail))

(defun %bind-logic-var (var term env)
  "Extend ENV with VAR bound to TERM, unless that would create a cycle."
  (if (occurs-check var term env)
      :unify-fail
      (acons var term env)))

(defun %unify-bound-logic-var (var term env)
  "Unify X with VAR's current binding in ENV, or bind VAR if unbound."
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
  (%walk-prolog-term template
                     (lambda (var)
                       (let ((entry (assoc var env)))
                         (if entry
                             (logic-substitute (cdr entry) env)
                             var)))
                     #'identity
                     (lambda (cons-node)
                       (cons (logic-substitute (car cons-node) env)
                             (logic-substitute (cdr cons-node) env)))))
