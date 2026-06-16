;;;; dcg-query.lisp — DCG public query API

(in-package :cl-cc/prolog)

(defun %phrase-solutions (rule-name input)
  "Return every remaining-input solution for RULE-NAME applied to INPUT."
  (let ((results nil))
    (%solve-goal-with-cut
     (list rule-name input '?dcg-rest)
     (lambda (env)
       (push (logic-substitute '?dcg-rest env) results)))
    (nreverse results)))

(defun phrase (rule-name input)
  "Parse INPUT (a list of tokens) with DCG RULE-NAME.
   Returns the first solution's remaining input, or NIL on failure."
  (first (%phrase-solutions rule-name input)))

(defun phrase-all (rule-name input)
  "Return all parse results for DCG RULE-NAME applied to INPUT."
  (%phrase-solutions rule-name input))
