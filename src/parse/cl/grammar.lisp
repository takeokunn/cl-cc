;;;; cl/grammar.lisp — CL grammar using recursive descent on token stream
;;;;
;;;; Parses lexer-token streams into CST nodes. Covers all CL special forms.
;;;; Provides backward-compatible redefinitions of parse-source and
;;;; parse-all-forms that route through the CST pipeline.

(in-package :cl-cc)

;;; ─── Token Stream ────────────────────────────────────────────────────────────

(defstruct token-stream
  "A token stream for the CL grammar parser."
  (tokens      nil :type list)
  (source      ""  :type string)
  (source-file nil :type (or string null))
  (diagnostics nil :type list))

(defun ts-peek (ts)
  "Peek at the current token without consuming it."
  (car (token-stream-tokens ts)))

(defun ts-peek-type (ts)
  "Return the type of the current token."
  (let ((tok (ts-peek ts)))
    (when tok (lexer-token-type tok))))

(defun ts-advance (ts)
  "Consume and return the current token, advancing the stream."
  (let ((tok (car (token-stream-tokens ts))))
    (setf (token-stream-tokens ts) (cdr (token-stream-tokens ts)))
    tok))

(defun ts-expect (ts expected-type &optional context)
  "Consume a token of EXPECTED-TYPE. Add diagnostic if mismatch."
  (let ((tok (ts-peek ts)))
    (cond
      ((null tok)
       (push (make-parse-error
              (format nil "~@[~A: ~]unexpected end of input, expected ~A"
                      context expected-type)
              (cons 0 0)
              :source-file (token-stream-source-file ts))
             (token-stream-diagnostics ts))
       nil)
      ((eq (lexer-token-type tok) expected-type)
       (ts-advance ts))
      (t
       (push (make-parse-error
              (format nil "~@[~A: ~]expected ~A but got ~A"
                      context expected-type (lexer-token-type tok))
              (cons (lexer-token-start-byte tok) (lexer-token-end-byte tok))
              :source-file (token-stream-source-file ts))
             (token-stream-diagnostics ts))
       nil))))

(defun ts-at-end-p (ts)
  "Is the stream at the end?"
  (or (null (token-stream-tokens ts))
      (eq (ts-peek-type ts) :T-EOF)))

(defun ts-token-value (ts)
  "Return the value of the current token."
  (let ((tok (ts-peek ts)))
    (when tok (lexer-token-value tok))))

;;; ─── CST Node Builders ──────────────────────────────────────────────────────

(defun %tok-to-cst (tok)
  "Convert a lexer-token to a cst-token."
  (when tok
    (make-cst-token :kind (lexer-token-type tok)
                    :value (lexer-token-value tok)
                    :start-byte (lexer-token-start-byte tok)
                    :end-byte (lexer-token-end-byte tok)
                    :source-file nil
                    :trivia (lexer-token-trivia tok))))

(defun %make-list-cst (kind children start-byte end-byte)
  "Make an interior CST node with the given kind and children."
  (make-cst-interior :kind kind
                     :children children
                     :start-byte start-byte
                     :end-byte end-byte))

;;; ─── Atom Parser ────────────────────────────────────────────────────────────

(defun parse-cl-atom (ts)
  "Parse an atom (number, string, symbol, keyword, character, boolean)."
  (let ((tok (ts-peek ts)))
    (when (null tok) (return-from parse-cl-atom nil))
    (case (lexer-token-type tok)
      ((:T-INT :T-FLOAT :T-RATIO :T-STRING :T-CHAR :T-KEYWORD
        :T-IDENT :T-BOOL-TRUE :T-BOOL-FALSE)
       (%tok-to-cst (ts-advance ts)))
      (otherwise nil))))

;;; ─── Main Form Parser ──────────────────────────────────────────────────────

(defun parse-cl-form (ts)
  "Parse a single CL form: atom, quoted form, list form, or vector."
  (when (ts-at-end-p ts) (return-from parse-cl-form nil))
  (let ((type (ts-peek-type ts)))
    (case type
      ;; Quote sugar
      (:T-QUOTE
       (let* ((tok (ts-advance ts))
              (start (lexer-token-start-byte tok))
              (inner (parse-cl-form ts)))
         (%make-list-cst :quote (list inner) start
                         (if inner (cst-node-end-byte inner) start))))
      ;; Backquote
      (:T-BACKQUOTE
       (let* ((tok (ts-advance ts))
              (start (lexer-token-start-byte tok))
              (inner (parse-cl-form ts)))
         (%make-list-cst :quasiquote (list inner) start
                         (if inner (cst-node-end-byte inner) start))))
      ;; Unquote
      (:T-UNQUOTE
       (let* ((tok (ts-advance ts))
              (start (lexer-token-start-byte tok))
              (inner (parse-cl-form ts)))
         (%make-list-cst :unquote (list inner) start
                         (if inner (cst-node-end-byte inner) start))))
      ;; Unquote-splicing
      (:T-UNQUOTE-SPLICING
       (let* ((tok (ts-advance ts))
              (start (lexer-token-start-byte tok))
              (inner (parse-cl-form ts)))
         (%make-list-cst :unquote-splicing (list inner) start
                         (if inner (cst-node-end-byte inner) start))))
      ;; Function
      (:T-FUNCTION
       (let* ((tok (ts-advance ts))
              (start (lexer-token-start-byte tok))
              (inner (parse-cl-form ts)))
         (%make-list-cst :function (list inner) start
                         (if inner (cst-node-end-byte inner) start))))
      ;; Vector #(...)
      (:T-VECTOR-START
       (let* ((tok (ts-advance ts))
              (start (lexer-token-start-byte tok))
              (elements nil))
         (loop until (or (ts-at-end-p ts) (eq (ts-peek-type ts) :T-RPAREN))
               do (let ((elem (parse-cl-form ts)))
                    (if elem (push elem elements)
                        (return))))
         (let ((close (ts-expect ts :T-RPAREN "vector")))
           (%make-list-cst :vector (nreverse elements) start
                           (if close (lexer-token-end-byte close) start)))))
      ;; List form
      (:T-LPAREN
       (parse-cl-list-form ts))
      ;; Atom
      (otherwise
       (parse-cl-atom ts)))))

;;; ─── List Form Parser ──────────────────────────────────────────────────────

(defun parse-cl-list-form (ts)
  "Parse a parenthesized list form, dispatching on head symbol."
  (let* ((open-tok (ts-advance ts))
         (start (lexer-token-start-byte open-tok)))
    ;; Empty list
    (when (eq (ts-peek-type ts) :T-RPAREN)
      (let ((close (ts-advance ts)))
        (return-from parse-cl-list-form
          (%make-list-cst :list nil start (lexer-token-end-byte close)))))
    ;; Check for special form dispatch
    (let* ((head-tok (ts-peek ts))
           (head-sym (when (and head-tok (eq (lexer-token-type head-tok) :T-IDENT))
                       (lexer-token-value head-tok)))
           (kind (when head-sym (sexp-head-to-kind head-sym))))
      ;; Parse all elements generically — works for all forms
      (let ((children nil)
            (dotted-p nil))
        (loop until (or (ts-at-end-p ts) (eq (ts-peek-type ts) :T-RPAREN))
              do (cond
                   ;; Dotted pair
                   ((eq (ts-peek-type ts) :T-DOT)
                    (ts-advance ts) ; consume dot
                    (let ((cdr-form (parse-cl-form ts)))
                      (when cdr-form (push cdr-form children)))
                    (setf dotted-p t)
                    (return))
                   (t
                    (let ((elem (parse-cl-form ts)))
                      (if elem (push elem children)
                          (return))))))
        (setf children (nreverse children))
        (let ((close (ts-expect ts :T-RPAREN "list form")))
          (let ((end (if close (lexer-token-end-byte close) start)))
            (%make-list-cst (if dotted-p :dotted-list (or kind :list))
                            children start end)))))))

;;; ─── Specialized Parsers ────────────────────────────────────────────────────
;;;
;;; These provide richer CST structure for specific forms. The generic
;;; parse-cl-list-form above handles all forms uniformly; these parsers
;;; can be used for IDE-quality CST when needed.

(defun parse-cl-defun (ts start)
  "Parse (defun name lambda-list body...) with START being the open-paren byte."
  (let ((children nil))
    ;; defun keyword
    (push (%tok-to-cst (ts-advance ts)) children)
    ;; name
    (let ((name (parse-cl-form ts)))
      (when name (push name children)))
    ;; lambda-list (a sublist)
    (let ((params (parse-cl-form ts)))
      (when params (push params children)))
    ;; body forms
    (loop until (or (ts-at-end-p ts) (eq (ts-peek-type ts) :T-RPAREN))
          do (let ((form (parse-cl-form ts)))
               (when form (push form children))))
    (let ((close (ts-expect ts :T-RPAREN "defun")))
      (%make-list-cst :defun (nreverse children) start
                      (if close (lexer-token-end-byte close) start)))))

(defun parse-cl-let (ts start)
  "Parse (let bindings body...) with START being the open-paren byte."
  (let ((children nil))
    (push (%tok-to-cst (ts-advance ts)) children) ; let keyword
    (let ((bindings (parse-cl-form ts)))           ; binding list
      (when bindings (push bindings children)))
    (loop until (or (ts-at-end-p ts) (eq (ts-peek-type ts) :T-RPAREN))
          do (let ((form (parse-cl-form ts)))
               (when form (push form children))))
    (let ((close (ts-expect ts :T-RPAREN "let")))
      (%make-list-cst :let (nreverse children) start
                      (if close (lexer-token-end-byte close) start)))))

(defun parse-cl-if (ts start)
  "Parse (if cond then else?) with START being the open-paren byte."
  (let ((children nil))
    (push (%tok-to-cst (ts-advance ts)) children)
    ;; condition, then, optional else
    (dotimes (i 3)
      (unless (or (ts-at-end-p ts) (eq (ts-peek-type ts) :T-RPAREN))
        (let ((form (parse-cl-form ts)))
          (when form (push form children)))))
    (let ((close (ts-expect ts :T-RPAREN "if")))
      (%make-list-cst :if (nreverse children) start
                      (if close (lexer-token-end-byte close) start)))))

(defun parse-cl-lambda (ts start)
  "Parse (lambda params body...) with START being open-paren byte."
  (let ((children nil))
    (push (%tok-to-cst (ts-advance ts)) children)
    (let ((params (parse-cl-form ts)))
      (when params (push params children)))
    (loop until (or (ts-at-end-p ts) (eq (ts-peek-type ts) :T-RPAREN))
          do (let ((form (parse-cl-form ts)))
               (when form (push form children))))
    (let ((close (ts-expect ts :T-RPAREN "lambda")))
      (%make-list-cst :lambda (nreverse children) start
                      (if close (lexer-token-end-byte close) start)))))

(defun parse-cl-block (ts start)
  "Parse (block name body...) with START being open-paren byte."
  (let ((children nil))
    (push (%tok-to-cst (ts-advance ts)) children)
    (let ((name (parse-cl-form ts)))
      (when name (push name children)))
    (loop until (or (ts-at-end-p ts) (eq (ts-peek-type ts) :T-RPAREN))
          do (let ((form (parse-cl-form ts)))
               (when form (push form children))))
    (let ((close (ts-expect ts :T-RPAREN "block")))
      (%make-list-cst :block (nreverse children) start
                      (if close (lexer-token-end-byte close) start)))))

(defun parse-cl-quote (ts start)
  "Parse (quote form) with START being open-paren byte."
  (let ((children nil))
    (push (%tok-to-cst (ts-advance ts)) children)
    (let ((form (parse-cl-form ts)))
      (when form (push form children)))
    (let ((close (ts-expect ts :T-RPAREN "quote")))
      (%make-list-cst :quote (nreverse children) start
                      (if close (lexer-token-end-byte close) start)))))

(defun parse-cl-call (ts start)
  "Parse a generic function call (fn arg...) with START being open-paren byte."
  (let ((children nil))
    (loop until (or (ts-at-end-p ts) (eq (ts-peek-type ts) :T-RPAREN))
          do (let ((form (parse-cl-form ts)))
               (when form (push form children))))
    (let ((close (ts-expect ts :T-RPAREN "call")))
      (%make-list-cst :call (nreverse children) start
                      (if close (lexer-token-end-byte close) start)))))

;;; ─── Top-Level Entry Points ──────────────────────────────────────────────────

(defun parse-cl-source (source &optional source-file)
  "Parse CL SOURCE string into CST nodes using the hand-written lexer and
   recursive descent parser. Returns (values cst-list diagnostics)."
  (let* ((tokens (lex-all source))
         (ts (make-token-stream :tokens tokens
                                :source source
                                :source-file source-file))
         (forms nil))
    (loop until (ts-at-end-p ts)
          do (let ((form (parse-cl-form ts)))
               (if form
                   (push form forms)
                   (return))))
    (values (nreverse forms)
            (nreverse (token-stream-diagnostics ts)))))

;;; ─── Pratt Bridge: CL NUD/LED Tables ────────────────────────────────────────
;;;
;;; These tables bind the generic Pratt engine (pratt.lisp) to CL tokens.
;;; CL is fully prefix — all compound forms are written (op arg...) — so
;;; the LED table is always empty.  The NUD table has a handler for each
;;; atom token type plus LPAREN and QUOTE.

(defvar *cl-nud-table*
  (let ((ht (make-hash-table :test #'eq)))
    (flet ((tok-nud (type)
             (setf (gethash type ht)
                   (lambda (ctx tok)
                     (declare (ignore ctx))
                     (%tok-to-cst tok)))))
      (tok-nud :T-INT)
      (tok-nud :T-FLOAT)
      (tok-nud :T-RATIO)
      (tok-nud :T-STRING)
      (tok-nud :T-CHAR)
      (tok-nud :T-IDENT)
      (tok-nud :T-KEYWORD)
      (tok-nud :T-BOOL-TRUE)
      (tok-nud :T-BOOL-FALSE))
    ;; QUOTE: build a quote-wrapper CST node
    (setf (gethash :T-QUOTE ht)
          (lambda (ctx tok)
            (declare (ignore ctx tok))
            (make-cst-interior :kind :quote :children nil
                               :start-byte 0 :end-byte 0)))
    ;; LPAREN: delegate to a minimal list stub
    (setf (gethash :T-LPAREN ht)
          (lambda (ctx tok)
            (declare (ignore ctx tok))
            (make-cst-interior :kind :list :children nil
                               :start-byte 0 :end-byte 0)))
    ht)
  "NUD dispatch table for the CL Pratt parser bridge (token-type → handler).")

(defvar *cl-led-table*
  (make-hash-table :test #'eq)
  "LED dispatch table for the CL Pratt parser bridge.
Always empty — CL has no infix operators.")

(defun make-cl-pratt-context (tokens source source-file)
  "Create a pratt-context pre-loaded with the CL NUD/LED grammar tables."
  (make-pratt-context
   :tokens      tokens
   :source      (or source "")
   :source-file source-file
   :nud-table   *cl-nud-table*
   :led-table   *cl-led-table*))

;;; ─── Backward Compatibility Wrappers ─────────────────────────────────────────
;;;
;;; NOTE: parse-source and parse-all-forms are NOT redefined here.
;;; The original read-from-string-based versions in cl/parser.lisp remain
;;; active for backward compatibility. Use parse-cl-source directly for
;;; the CST pipeline.
