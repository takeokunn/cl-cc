;;;; core/combinators.lisp - Grammar-driven Parser Combinator Engine
;;;;
;;;; Grammar rules are declared via def-grammar-rule (stored in *grammar-rules*).
;;;; The interpreter (parse-combinator) queries this table at parse time and
;;;; executes combinator expressions against an immutable token stream.
;;;;
;;;; Token stream: a plain CL list of tokens. Each token is a plist with
;;;;   :type  - keyword token type (e.g. :T-INT, :T-VAR)
;;;;   :value - the token's parsed value

(in-package :cl-cc/parse)

;;; Grammar Rule Database

(defvar *grammar-rules* (make-hash-table)
  "Grammar rule database mapping keyword names to combinator expressions.")

(defmacro def-grammar-rule (name combinator-expr)
  "Declare grammar rule NAME (a keyword) with combinator expression COMBINATOR-EXPR.
   Example: (def-grammar-rule :php-expr (seq (rule :php-term) (many (alt :plus-term :minus-term))))"
  `(setf (gethash ,name *grammar-rules*) ',combinator-expr))

(defun query-grammar (name)
  "Retrieve the combinator expression for grammar rule NAME.
   Returns NIL if not found."
  (gethash name *grammar-rules*))

(defun clear-grammar-rules ()
  "Clear all grammar rules from the database."
  (clrhash *grammar-rules*))

;;; Token Stream Protocol
;;;
;;; Token stream = immutable CL list. Backtracking is free:
;;; just keep the old list reference (no copying needed).

(defun stream-empty-p (stream) (null stream))
(defun stream-peek    (stream) (car stream))
(defun stream-consume (stream)
  (if stream
      (values (car stream) (cdr stream))
      (values nil nil)))

;;; Token Accessors
;;; Tokens are plists: (:type :T-INT :value 42)

(defun tok-type  (tok) (getf tok :type))
(defun tok-value (tok) (getf tok :value))

;;; Core Combinator Interpreter

(defun parse-combinator (expr stream)
  "Interpret combinator expression EXPR against token STREAM.
   Returns (values ast-value remaining-stream) on success.
   Returns (values :fail nil) on failure (use parse-ok-p to check).

   Combinator forms:
     (token :TYPE)          - match token of given :type
     (token :TYPE value)    - match token of :type with specific value
     (literal string)       - match :T-IDENT with exact string value
     (seq  e1 e2 ...)       - all must match in order; returns list of results
     (alt  e1 e2 ...)       - try each, return first success
     (many expr)            - zero or more; always succeeds; returns list
     (many1 expr)           - one or more
     (opt  expr)            - optional; returns :opt-absent if no match (not failure)
     (rule :name)           - reference a named grammar rule
     :name                  - shorthand for (rule :name)"
  (cond
    ((keywordp expr)   (parse-rule expr stream))
    ((not (consp expr)) (error "Invalid combinator expression: ~S" expr))
    (t
     ;; Normalize the operator to a keyword so that combinator expressions
     ;; work regardless of which package the caller read them from.
     ;; e.g. cl-cc/test::seq and cl-cc::seq both normalize to :SEQ.
     (let ((op (and (symbolp (car expr))
                    (intern (symbol-name (car expr)) :keyword))))
       (case op
         (:token   (parse-token*   (cadr expr) (caddr expr) stream))
         (:literal (parse-literal* (cadr expr) stream))
         (:seq     (parse-seq*     (cdr expr)  stream))
         (:alt     (parse-alt*     (cdr expr)  stream))
         (:many    (parse-many*    (cadr expr) stream))
         (:many1   (parse-many1*   (cadr expr) stream))
         (:opt     (parse-opt*     (cadr expr) stream))
         (:rule    (parse-rule     (cadr expr) stream))
         (t        (error "Unknown combinator operator: ~S" (car expr))))))))

(defun parse-ok-p (ast) (not (eq ast :fail)))

(defun parse-token* (type value stream)
  "Match a token of TYPE, optionally with specific VALUE."
  (if (stream-empty-p stream)
      (values :fail nil)
      (multiple-value-bind (tok rest) (stream-consume stream)
        (if (and (eq (tok-type tok) type)
                 (or (null value) (equal (tok-value tok) value)))
            (values (tok-value tok) rest)
            (values :fail nil)))))

(defun parse-literal* (string stream)
  "Match a :T-IDENT token whose value equals STRING."
  (parse-token* :T-IDENT string stream))

(defun parse-seq* (exprs stream)
  "Match a sequence of combinators. Returns list of results."
  (let ((results nil)
        (current stream))
    (dolist (expr exprs (values (nreverse results) current))
      (multiple-value-bind (ast next) (parse-combinator expr current)
        (unless (parse-ok-p ast)
          (return (values :fail nil)))
        (push ast results)
        (setf current next)))))

(defun parse-alt* (exprs stream)
  "Try each alternative, return first success."
  (dolist (expr exprs (values :fail nil))
    (multiple-value-bind (ast rest) (parse-combinator expr stream)
      (when (parse-ok-p ast)
        (return (values ast rest))))))

(defun parse-many* (expr stream)
  "Match zero or more occurrences. Always succeeds.
   Stops if the sub-expression matches without advancing the stream (prevents
   infinite loops from combinators like (many (opt X)) when X always fails)."
  (let ((results nil)
        (current stream))
    (loop
      (multiple-value-bind (ast next) (parse-combinator expr current)
        (if (and (parse-ok-p ast) (not (eq next current)))
            (progn (push ast results) (setf current next))
            (return (values (nreverse results) current)))))))

(defun parse-many1* (expr stream)
  "Match one or more occurrences."
  (multiple-value-bind (first rest) (parse-combinator expr stream)
    (if (parse-ok-p first)
        (multiple-value-bind (more final) (parse-many* expr rest)
          (values (cons first more) final))
        (values :fail nil))))

(defun parse-opt* (expr stream)
  "Optionally match EXPR. Returns :opt-absent (not :fail) if no match."
  (multiple-value-bind (ast rest) (parse-combinator expr stream)
    (if (parse-ok-p ast)
        (values ast rest)
        (values :opt-absent stream))))

(defun parse-rule (name stream)
  "Look up grammar rule NAME in *grammar-rules* and parse with it."
  (let ((rule-expr (query-grammar name)))
    (if rule-expr
        (parse-combinator rule-expr stream)
        (error "No grammar rule defined for: ~S" name))))

;;; Top-Level Entry Point

(defun parse-with-grammar (start-rule token-stream)
  "Parse TOKEN-STREAM using grammar rule START-RULE.
   Returns (values ast remaining-tokens) on success.
   Signals syntax-error on failure."
  (multiple-value-bind (ast remaining)
      (parse-rule start-rule token-stream)
    (if (parse-ok-p ast)
        (values ast remaining)
        (error "Grammar parse failure starting from rule ~S" start-rule))))
