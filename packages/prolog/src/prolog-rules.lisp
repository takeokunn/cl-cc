;;; packages/prolog/src/prolog-rules.lisp
;;;
;;; Goal and rule representation, database primitives, and cut support for
;;; CL-CC Prolog.

(in-package :cl-cc/prolog)

;;; Goal and Rule Representation

(defstruct (prolog-rule (:conc-name rule-))
  "Represents a Prolog rule or fact."
  head
  (body nil))

(defun %rename-prolog-term (term renaming)
  "Rename TERM's logic variables via RENAMING hash table."
  (%walk-prolog-term term
                     (lambda (node)
                       (or (gethash node renaming)
                           (setf (gethash node renaming)
                                 (gensym (symbol-name node)))))
                     #'identity
                     (lambda (node)
                       (cons (%rename-prolog-term (car node) renaming)
                             (%rename-prolog-term (cdr node) renaming)))))

(defun rename-variables (rule)
  "Rename all logic variables in RULE to fresh ones for recursive calls."
  (let ((renaming (make-hash-table :test 'eq)))
    (make-prolog-rule :head (%rename-prolog-term (rule-head rule) renaming)
                      :body (mapcar (lambda (b)
                                      (%rename-prolog-term b renaming))
                                    (rule-body rule)))))

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

(defun register-prolog-rule (head &optional body)
  "Create a rule from HEAD and BODY, then register it under HEAD's predicate."
  (let ((rule (make-prolog-rule :head head :body body)))
    (add-rule (car head) rule)
    rule))

(defun %register-declarative-rule-spec (spec)
  "Register a declarative rule SPEC in the Prolog database."
  (destructuring-bind (head &optional body) spec
    (register-prolog-rule head body)))

(defun %register-type-rule-spec (spec)
  "Register one or more type inference rules from SPEC."
  (destructuring-bind (result-type expr-kind operators) spec
    (dolist (op operators)
      (register-prolog-rule
       `(type-of (,expr-kind ,op ?a ?b) ?env (,result-type))
       '((type-of ?a ?env (integer-type))
         (type-of ?b ?env (integer-type)))))))

(defun %register-rule-specs (specs registrar)
  "Apply REGISTRAR to each SPEC in SPECS."
  (dolist (spec specs)
    (funcall registrar spec)))

(defmacro def-fact (head)
  "Define a Prolog fact. Usage: (def-fact (parent tom mary))"
  `(register-prolog-rule ',head))

(defmacro def-rule (head &body body)
  "Define a Prolog rule. Usage: (def-rule (grandparent ?x ?z) (parent ?x ?y) (parent ?y ?z))"
  `(register-prolog-rule ',head ',body))

;;; Cut Operator Support

(define-condition prolog-cut (condition)
  ()
  (:documentation "Condition signaled when cut (!) is encountered."))

;;; Declarative built-in predicates and type rules.
;;; Register them directly so this file stays data-driven without one-off
;;; expansion macros.

(%register-rule-specs *prolog-declarative-rule-specs*
                      #'%register-declarative-rule-spec)
(%register-rule-specs *prolog-type-rule-specs*
                      #'%register-type-rule-spec)
