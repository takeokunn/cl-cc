;;;; dcg-builtins.lisp — DCG combinator builtins and token matching

(in-package :cl-cc/prolog)

(defvar *dcg-sync-tokens* '(:T-RPAREN :T-SEMI :T-EOF)
  "Token types used as synchronization points for error recovery.")

(defun %dcg-alt (args env k)
  "Try each alternative rule; Args: (alt1 alt2 ... s-in s-out)."
  (let ((s-in (car (last args 2)))
        (s-out (car (last args 1)))
        (alts (butlast args 2)))
    (dolist (alt alts)
      (solve-goal (list alt s-in s-out) env k))))

(defun %dcg-opt (args env k)
  "Optional match (0 or 1); Args: (rule s-in s-out)."
  (let ((rule (first args))
        (s-in (second args))
        (s-out (third args)))
    (solve-goal (list rule s-in s-out) env k)
    (let ((eps-env (unify s-in s-out env)))
      (unless (unify-failed-p eps-env)
        (funcall k eps-env)))))

(defun %dcg-star-loop (rule s-out k current-in current-env)
  "Recursive helper for dcg-star: consume input with RULE until fixed point."
  (let ((eps-env (unify current-in s-out current-env)))
    (unless (unify-failed-p eps-env)
      (funcall k eps-env)))
  (let ((mid (gensym "?MID")))
    (solve-goal (list rule current-in mid) current-env
                (lambda (new-env)
                  (let ((resolved-mid (logic-substitute mid new-env))
                        (resolved-in  (logic-substitute current-in new-env)))
                    (unless (equal resolved-mid resolved-in)
                      (%dcg-star-loop rule s-out k mid new-env)))))))

(defun %dcg-star (args env k)
  "Kleene star (zero or more); Args: (rule s-in s-out)."
  (let ((rule (first args))
        (s-in (second args))
        (s-out (third args)))
    (%dcg-star-loop rule s-out k s-in env)))

(defun %dcg-plus (args env k)
  "One or more matches; Args: (rule s-in s-out)."
  (let ((rule (first args))
        (s-in (second args))
        (s-out (third args))
        (mid  (gensym "?MID")))
    (solve-goal (list rule s-in mid) env
                (lambda (env1)
                  (solve-goal (list 'dcg-star rule mid s-out) env1 k)))))

(defun %dcg-skip-loop (s-out env k remaining)
  "Recursive helper for dcg-error-recovery: skip until a sync token."
  (cond
    ((null remaining)
     (let ((new-env (unify s-out nil env)))
       (unless (unify-failed-p new-env) (funcall k new-env))))
    ((and (consp (car remaining))
          (member (caar remaining) *dcg-sync-tokens*))
     (let ((new-env (unify s-out remaining env)))
       (unless (unify-failed-p new-env) (funcall k new-env))))
    (t (%dcg-skip-loop s-out env k (cdr remaining)))))

(defun %dcg-error-recovery (args env k)
  "Skip tokens until a sync point; Args: (s-in s-out)."
  (let ((s-in  (first args))
        (s-out (second args)))
    (%dcg-skip-loop s-out env k (logic-substitute s-in env))))

(defun %dcg-match-next-token (expected-type stream env)
  "Return the next input token cons and remaining stream when STREAM begins with EXPECTED-TYPE.
Returns NIL when no token is present or the type does not match."
  (let ((input (logic-substitute stream env)))
    (when (and (consp input)
               (consp (car input))
               (eq (caar input) expected-type))
      (values (car input) (cdr input)))))

(defun %dcg-with-matched-token (expected-type stream env on-match)
  "Invoke ON-MATCH with the matched token and rest stream when the next token type matches."
  (multiple-value-bind (token rest)
      (%dcg-match-next-token expected-type stream env)
    (when token
      (funcall on-match token rest))))

(defun %dcg-token-match (args env k)
  "Match a token by type; Args: (expected-type s-in s-out)."
  (let* ((expected-type (first args))
         (s-in  (second args))
         (s-out (third args)))
    (%dcg-with-matched-token
     expected-type s-in env
     (lambda (token rest)
       (declare (ignore token))
       (let ((new-env (unify s-out rest env)))
         (unless (unify-failed-p new-env)
           (funcall k new-env)))))))

(defun %dcg-token-match-value (args env k)
  "Match a token by type and bind its value; Args: (expected-type value-var s-in s-out)."
  (let* ((expected-type (first args))
         (value-var (second args))
         (s-in  (third args))
         (s-out (fourth args)))
    (%dcg-with-matched-token
     expected-type s-in env
     (lambda (token rest)
       (let ((val-env (unify value-var (cdr token) env)))
         (unless (unify-failed-p val-env)
           (let ((new-env (unify s-out rest val-env)))
             (unless (unify-failed-p new-env)
               (funcall k new-env)))))))))

(defparameter *dcg-builtin-specs*
  '((dcg-alt %dcg-alt)
    (dcg-opt %dcg-opt)
    (dcg-star %dcg-star)
    (dcg-plus %dcg-plus)
    (dcg-error-recovery %dcg-error-recovery)
    (dcg-token-match %dcg-token-match)
    (dcg-token-match-value %dcg-token-match-value))
  "DCG builtin predicate registrations.")

(eval-when (:load-toplevel :execute)
  (make-builtin-predicate-table *dcg-builtin-specs* *builtin-predicates*))
