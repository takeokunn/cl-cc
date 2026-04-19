(in-package :cl-cc/parse)

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

