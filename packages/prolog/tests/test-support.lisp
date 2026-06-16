(in-package :cl-cc/test)

(defmacro assert-prolog-boolean-case (expected form)
  `(if ,expected
       (assert-true ,form)
       (assert-false ,form)))

(defun dcg-input (&rest type-value-pairs)
  "Build a DCG input list from (type value) pairs."
  (loop for (type val) on type-value-pairs by #'cddr
        collect (cons type val)))

(defmacro with-dcg-results ((results goal &optional env) &body body)
  "Bind RESULTS to the substitution list for GOAL and run BODY."
  `(let ((,results (all-prolog-substitutions ,goal ,env)))
     (declare (ignorable ,results))
     ,@body))

(defmacro %with-prolog-fixture (setup-forms &body body)
  `(with-fresh-prolog
     ,@setup-forms
     ,@body))

(defmacro with-prolog-facts (facts &body body)
  "Run BODY in a fresh Prolog DB after installing FACTS."
  `(%with-prolog-fixture
       ,(mapcar (lambda (fact)
                  `(cl-cc/prolog:def-fact ,fact))
                facts)
     ,@body))

(defmacro with-dcg-token-rules (rules &body body)
  "Run BODY in a fresh Prolog DB after installing token-consuming DCG rules.

RULES is a list of (NAME TOKEN-TYPE) pairs; each pair becomes a fact of the
form (NAME ((TOKEN-TYPE . ?V) . ?REST) ?REST)."
  `(%with-prolog-fixture
       ,(mapcar (lambda (rule)
                  (destructuring-bind (name token-type) rule
                    `(,name ((,token-type . ?v) . ?rest) ?rest)))
                rules)
     ,@body))

(defmacro with-dcg-rules (rules &body body)
  "Run BODY in a fresh Prolog DB after defining DCG rules from RULES.

RULES is a list of (NAME &rest BODY-FORMS) entries that expand to
cl-cc:def-dcg-rule forms."
  `(%with-prolog-fixture
       ,(mapcar (lambda (rule)
                  (destructuring-bind (name &rest rule-body) rule
                    `(cl-cc:def-dcg-rule ,name ,@rule-body)))
                rules)
     ,@body))

(defmacro assert-dcg-solves (goal &optional env)
  "Assert that GOAL has at least one substitution."
  `(assert-true (all-prolog-substitutions ,goal ,env)))
