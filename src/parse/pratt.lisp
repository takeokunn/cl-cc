;;;; pratt.lisp — Language-agnostic Pratt + Recursive Descent engine
;;;;
;;;; Used by cl/grammar.lisp (CL) and php/grammar.lisp (PHP).
;;;; The pratt-context struct stores token-accessor functions so the same
;;;; engine works with lexer-token structs (CL) and token plists (PHP).

(in-package :cl-cc/parse)

;;; ─── Pratt Context ───────────────────────────────────────────────────────────

(defstruct pratt-context
  "Unified mutable parser context for Pratt+RecDescent parsing."
  (tokens      nil  :type list)
  (source      ""   :type string)
  (source-file nil  :type (or string null))
  (diagnostics nil  :type list)
  ;; Per-context NUD table: token-type keyword -> (lambda (ctx tok) -> cst-node)
  (nud-table   nil  :type (or hash-table null))
  ;; Per-context LED table: token-type keyword -> (cons binding-power (lambda (ctx left tok) -> cst-node))
  (led-table   nil  :type (or hash-table null))
  ;; Token accessor functions -- swap these for PHP vs CL
  (tok-type-fn  #'lexer-token-type       :type function)
  (tok-value-fn #'lexer-token-value      :type function)
  (tok-start-fn #'lexer-token-start-byte :type function)
  (tok-end-fn   #'lexer-token-end-byte   :type function))

;;; ─── Token Accessors ─────────────────────────────────────────────────────────

(defun pratt-tok-type (ctx tok)
  (when tok (funcall (pratt-context-tok-type-fn ctx) tok)))

(defun pratt-tok-value (ctx tok)
  (when tok (funcall (pratt-context-tok-value-fn ctx) tok)))

(defun pratt-tok-start (ctx tok)
  (when tok (funcall (pratt-context-tok-start-fn ctx) tok)))

(defun pratt-tok-end (ctx tok)
  (when tok (funcall (pratt-context-tok-end-fn ctx) tok)))

;;; ─── Stream Operations ──────────────────────────────────────────────────────

(defun pratt-peek (ctx)
  "Return current token without consuming."
  (car (pratt-context-tokens ctx)))

(defun pratt-peek-type (ctx)
  (pratt-tok-type ctx (pratt-peek ctx)))


(defun pratt-advance (ctx)
  "Consume and return current token."
  (let ((tok (car (pratt-context-tokens ctx))))
    (setf (pratt-context-tokens ctx) (cdr (pratt-context-tokens ctx)))
    tok))

(defun pratt-at-end-p (ctx)
  (let ((tok (pratt-peek ctx)))
    (or (null tok)
        (eq (pratt-tok-type ctx tok) :T-EOF))))

;;; ─── Diagnostics ────────────────────────────────────────────────────────────

(defun pratt-add-diagnostic (ctx message span &key (severity :error))
  "Push a diagnostic. Span is (start-byte . end-byte)."
  (declare (ignore severity))
  (push (make-parse-error message span
                          :source-file (pratt-context-source-file ctx))
        (pratt-context-diagnostics ctx)))

(defun pratt-expect (ctx expected-type &optional context)
  "Consume a token of EXPECTED-TYPE. Push diagnostic and return NIL on mismatch."
  (let ((tok (pratt-peek ctx)))
    (cond
      ((null tok)
       (pratt-add-diagnostic ctx
         (format nil "~@[~A: ~]unexpected end of input, expected ~A"
                 context expected-type)
         (cons 0 0))
       nil)
      ((eq (pratt-tok-type ctx tok) expected-type)
       (pratt-advance ctx))
      (t
       (pratt-add-diagnostic ctx
         (format nil "~@[~A: ~]expected ~A but got ~A"
                 context expected-type (pratt-tok-type ctx tok))
         (cons (pratt-tok-start ctx tok) (pratt-tok-end ctx tok)))
       nil))))

;;; ─── Core Pratt Loop ────────────────────────────────────────────────────────

(defun pratt-parse-expr (ctx &optional (min-bp 0))
  "Parse an expression using Pratt precedence climbing.
   Looks up NUD handler for current token, then loops on LED handlers
   while their binding power exceeds MIN-BP."
  (let* ((tok (pratt-advance ctx))
         (nud (when tok (gethash (pratt-tok-type ctx tok)
                                 (pratt-context-nud-table ctx)))))
    (when (null nud)
      (when tok
        (pratt-add-diagnostic ctx
          (format nil "unexpected token ~A in expression position"
                  (pratt-tok-type ctx tok))
          (cons (or (pratt-tok-start ctx tok) 0)
                (or (pratt-tok-end ctx tok) 0))))
      (return-from pratt-parse-expr
        (make-cst-error-node :kind :error :message "unexpected token"
                             :start-byte (if tok (or (pratt-tok-start ctx tok) 0) 0)
                             :end-byte (if tok (or (pratt-tok-end ctx tok) 0) 0))))
    (let ((left (funcall nud ctx tok)))
      ;; LED loop: keep consuming infix/postfix operators
      (loop
        (let* ((peek-tok (pratt-peek ctx))
               (led-entry (when peek-tok
                            (gethash (pratt-tok-type ctx peek-tok)
                                     (pratt-context-led-table ctx)))))
          (unless (and led-entry (> (car led-entry) min-bp))
            (return))
          (let ((op-tok (pratt-advance ctx)))
            (setf left (funcall (cdr led-entry) ctx left op-tok)))))
      left)))

;;; ─── List Parsing Helper ────────────────────────────────────────────────────

(defun pratt-parse-list-until (ctx end-type parse-fn)
  "Parse (PARSE-FN ctx)* until END-TYPE token or EOF.
   Consumes the END-TYPE token. Returns list of results."
  (let ((items nil))
    (loop
      (when (or (pratt-at-end-p ctx) (eq (pratt-peek-type ctx) end-type))
        (return))
      (let ((item (funcall parse-fn ctx)))
        (if item
            (push item items)
            (return))))
    (pratt-expect ctx end-type "list")
    (nreverse items)))

;;; ─── Token to CST Conversion ────────────────────────────────────────────────

(defun pratt-tok-to-cst (ctx tok)
  "Convert the current language's token to a cst-token."
  (when tok
    (make-cst-token :kind (pratt-tok-type ctx tok)
                    :value (pratt-tok-value ctx tok)
                    :start-byte (or (pratt-tok-start ctx tok) 0)
                    :end-byte (or (pratt-tok-end ctx tok) 0)
                    :source-file (pratt-context-source-file ctx)
                    :trivia (when (lexer-token-p tok) (lexer-token-trivia tok)))))
