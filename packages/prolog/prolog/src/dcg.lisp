;;;; dcg.lisp — DCG (Definite Clause Grammar) engine with combinators
;;;;
;;;; Provides a DCG parsing engine built on the Prolog infrastructure.
;;;; Builtins registered in *builtin-predicates* for integration with
;;;; the Prolog solver. Entry points: dcg-parse, dcg-parse-all.

(in-package :cl-cc/prolog)

;;; ─── DCG Sync Tokens ─────────────────────────────────────────────────────────

(defvar *dcg-sync-tokens* '(:T-RPAREN :T-SEMI :T-EOF)
  "Token types used as synchronization points for error recovery.")

;;; ─── DCG Input Conversion ────────────────────────────────────────────────────

(defun lexer-tokens-to-dcg-input (tokens)
  "Convert a list of lexer-token structs (or plists) to a DCG input list.
   Each element is a cons (TYPE . VALUE) suitable for DCG matching."
  (mapcar (lambda (tok)
            (cond
              ((and (consp tok) (getf tok :type))
               ;; plist token
               (cons (getf tok :type) (getf tok :value)))
              ((lexer-token-p tok)
               (cons (lexer-token-type tok) (lexer-token-value tok)))
              (t tok)))
          tokens))

(defun dcg-token-to-cst (tok-cons start-byte)
  "Convert a DCG token cons (TYPE . VALUE) into a cst-token node."
  (make-cst-token :kind (car tok-cons)
                  :value (cdr tok-cons)
                  :start-byte start-byte))

;;; ─── DCG Builtin Predicates ──────────────────────────────────────────────────

;; dcg-alt: try alternatives
;; Args: (alt1 alt2 ... s-in s-out)
;; Each altN is a predicate to try; s-in/s-out are difference list vars.
(setf (gethash 'dcg-alt *builtin-predicates*)
      (lambda (args env k)
        (let ((s-in (car (last args 2)))
              (s-out (car (last args 1)))
              (alts (butlast args 2)))
          (dolist (alt alts)
            (let ((goal (list alt s-in s-out)))
              (solve-goal goal env k))))))

;; dcg-opt: optional (0 or 1 match)
;; Args: (rule s-in s-out)
;; Try rule first; if it fails, unify s-in = s-out (epsilon).
(setf (gethash 'dcg-opt *builtin-predicates*)
      (lambda (args env k)
        (let ((rule (first args))
              (s-in (second args))
              (s-out (third args)))
          ;; Try the rule
          (solve-goal (list rule s-in s-out) env k)
          ;; Epsilon: s-in = s-out
          (let ((eps-env (unify s-in s-out env)))
            (unless (unify-failed-p eps-env)
              (funcall k eps-env))))))

;; dcg-star: Kleene star (zero or more)
;; Args: (rule s-in s-out)
;; Uses a loop guard to prevent infinite loops on epsilon matches.
(setf (gethash 'dcg-star *builtin-predicates*)
      (lambda (args env k)
        (let ((rule (first args))
              (s-in (second args))
              (s-out (third args)))
          (labels ((star-loop (current-in current-env)
                     ;; First try epsilon (base case)
                     (let ((eps-env (unify current-in s-out current-env)))
                       (unless (unify-failed-p eps-env)
                         (funcall k eps-env)))
                     ;; Then try one more match + recurse
                     (let ((mid (gensym "?MID")))
                       (solve-goal (list rule current-in mid) current-env
                                   (lambda (new-env)
                                     (let ((resolved-mid (logic-substitute mid new-env)))
                                       ;; Guard: must have consumed input
                                       (let ((resolved-in (logic-substitute current-in new-env)))
                                         (unless (equal resolved-mid resolved-in)
                                           (star-loop mid new-env)))))))))
            (star-loop s-in env)))))

;; dcg-plus: one or more matches
;; Args: (rule s-in s-out)
;; Equivalent to: one match, then star.
(setf (gethash 'dcg-plus *builtin-predicates*)
      (lambda (args env k)
        (let ((rule (first args))
              (s-in (second args))
              (s-out (third args)))
          (let ((mid (gensym "?MID")))
            ;; At least one match
            (solve-goal (list rule s-in mid) env
                        (lambda (env1)
                          ;; Then zero or more
                          (solve-goal (list 'dcg-star rule mid s-out) env1 k)))))))

;; dcg-error-recovery: skip tokens until a sync point is found
;; Args: (s-in s-out)
;; Skips tokens in s-in until one of *dcg-sync-tokens* is encountered.
(setf (gethash 'dcg-error-recovery *builtin-predicates*)
      (lambda (args env k)
        (let ((s-in (first args))
              (s-out (second args)))
          (let ((input (logic-substitute s-in env)))
            (labels ((skip-loop (remaining)
                       (cond
                         ;; Empty input: unify with s-out
                         ((null remaining)
                          (let ((new-env (unify s-out nil env)))
                            (unless (unify-failed-p new-env) (funcall k new-env))))
                         ;; Sync token found: unify s-out with remaining
                         ((and (consp (car remaining))
                               (member (caar remaining) *dcg-sync-tokens*))
                          (let ((new-env (unify s-out remaining env)))
                            (unless (unify-failed-p new-env) (funcall k new-env))))
                         ;; Skip and continue
                         (t (skip-loop (cdr remaining))))))
              (skip-loop input))))))

;;; ─── DCG Parse Entry Points ──────────────────────────────────────────────────

(defun dcg-parse (rule-name input)
  "Parse INPUT (a list of token conses) using DCG rule RULE-NAME.
   Returns the first successful parse result as remaining input, or NIL on failure.
   Uses Prolog query interface internally."
  (nth-value 1 (phrase-rest rule-name input)))

(defun dcg-parse-all (rule-name input)
  "Parse INPUT using DCG rule RULE-NAME, returning all possible parses.
   Each result is the remaining input after a successful parse."
  (phrase-all rule-name input))

;;; ─── DCG Rule Construction (moved from prolog.lisp) ─────────────────────────

(defvar *dcg-counter* 0
  "Counter for generating fresh DCG state variables.")

(defun dcg-fresh-var ()
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
    ((and (consp element) (eq (car element) 'terminal))
     (let ((terminals (cdr element)))
       (if (null terminals)
           (list `(= ,s-in ,s-out))
           (let ((goals nil)
                 (current s-in))
             (dolist (term terminals)
               (let ((next (dcg-fresh-var)))
                 (push `(= ,current (cons ,term ,next)) goals)
                 (setf current next)))
             (push `(= ,current ,s-out) goals)
             (nreverse goals)))))
    ((symbolp element)
     (list (list element s-in s-out)))
    ((and (consp element) (symbolp (car element)))
     (list (append element (list s-in s-out))))
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
        (setf goals (append goals (dcg-transform-body-element (car (last body)) current s-out)))
        goals)))

(defmacro def-dcg-rule (name &body body)
  "Define a DCG rule. Transforms (name --> body...) into a Prolog rule
   with difference-list state threading.
   Usage: (def-dcg-rule expr term (terminal (+)) term)"
  (let ((s-in (gensym "?S-IN"))
        (s-out (gensym "?S-OUT")))
    `(def-rule (,name ,s-in ,s-out)
       ,@(dcg-transform-body body s-in s-out))))

;;; DCG token matching builtins

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
  (nth-value 1 (phrase-rest rule-name input)))

(defun %phrase-solutions (rule-name input)
  "Return every remaining-input solution for RULE-NAME applied to INPUT."
  (let ((results nil))
    (%solve-goal-with-cut
     (list rule-name input '?dcg-rest)
     (lambda (env)
       (push (logic-substitute '?dcg-rest env) results)))
    (nreverse results)))

(defun phrase-rest (rule-name input)
  "Parse INPUT with DCG RULE-NAME. Returns (values matched-p remaining).
    MATCHED-P is T if parsing succeeded."
  (let ((results (%phrase-solutions rule-name input)))
    (values (not (null results)) (first results))))

(defun phrase-all (rule-name input)
  "Parse INPUT with DCG RULE-NAME, returning all possible remaining inputs."
  (%phrase-solutions rule-name input))
