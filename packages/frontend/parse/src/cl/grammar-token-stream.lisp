;;;; cl/grammar.lisp — CL grammar using recursive descent on token stream
;;;;
;;;; Parses lexer-token streams into CST nodes. Covers all CL special forms.
;;;; Provides the token-stream primitives used by the CST-first CL parser.

(in-package :cl-cc/parse)

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
