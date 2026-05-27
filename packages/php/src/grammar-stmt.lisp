;;;; packages/php/src/grammar-stmt.lisp — PHP CST Grammar: Statement Layer
;;;;
;;;; Contains statement-level CST parsers and the top-level entry point.
;;;; Expression-level parsers (php-cst-parse-primary through php-cst-parse-expr)
;;;; are in grammar.lisp (loads before).
;;;;
;;;; Load order: after grammar.lisp.

(in-package :cl-cc/php)

;;; ─── Statement Parsers ──────────────────────────────────────────────────────

(defun php-cst-parse-block (ts)
  "Parse { stmt* }. Returns list of CST nodes."
  (php-ts-expect ts :T-LBRACE nil "block")
  (let ((stmts nil))
    (loop
      (php-ts-skip-semis ts)
      (when (or (php-ts-at-end-p ts) (eq (php-ts-peek-type ts) :T-RBRACE))
        (return))
      (let ((stmt (php-cst-parse-statement ts)))
        (when stmt (push stmt stmts))))
    (php-ts-expect ts :T-RBRACE nil "block")
    (nreverse stmts)))

(defun php-cst-parse-param-list (ts)
  "Parse (type? $param, ...) for function/method definitions. Returns list of CST tokens."
  (php-ts-expect ts :T-LPAREN nil "parameter list")
  (if (eq (php-ts-peek-type ts) :T-RPAREN)
      (progn (php-ts-advance ts) nil)
      (let ((params nil))
        (loop
          ;; Skip optional type annotation
          (when (member (php-ts-peek-type ts) '(:T-TYPE :T-IDENT :T-NULLABLE))
            (php-ts-advance ts)
            (when (member (php-ts-peek-type ts) '(:T-TYPE :T-IDENT))
              (php-ts-advance ts)))
          ;; Expect $var
          (let ((var-tok (php-ts-expect ts :T-VAR nil "parameter")))
            (when var-tok (push (%php-tok-to-cst var-tok) params)))
          ;; Skip optional default value
          (when (and (eq (php-ts-peek-type ts) :T-OP)
                     (equal "=" (php-ts-peek-value ts)))
            (php-ts-advance ts)
            (php-cst-parse-expr ts)) ; parse and discard default
          (if (eq (php-ts-peek-type ts) :T-COMMA)
              (php-ts-advance ts)
              (return)))
        (php-ts-expect ts :T-RPAREN nil "parameter list")
        (nreverse params))))

;;; ─── Statement sub-parsers ──────────────────────────────────────────────────

(defun %parse-php-class-body-members (ts)
  "Parse class member declarations until '}'. Returns list of CST nodes (reversed)."
  (let ((members nil))
    (loop
      (php-ts-skip-semis ts)
      (when (or (php-ts-at-end-p ts) (eq (php-ts-peek-type ts) :T-RBRACE)) (return))
      (loop while (and (not (php-ts-at-end-p ts))
                       (eq (php-ts-peek-type ts) :T-KEYWORD)
                       (member (php-ts-peek-value ts)
                               '(:public :private :protected :static
                                 :abstract :final :readonly)))
            do (php-ts-advance ts))
      (cond
        ((eq (php-ts-peek-type ts) :T-VAR)
         (push (%php-tok-to-cst (php-ts-advance ts)) members)
         (php-ts-skip-semis ts))
        ((member (php-ts-peek-type ts) '(:T-TYPE :T-NULLABLE))
         (php-ts-advance ts)
         (when (member (php-ts-peek-type ts) '(:T-TYPE :T-IDENT)) (php-ts-advance ts))
         (when (eq (php-ts-peek-type ts) :T-VAR)
           (push (%php-tok-to-cst (php-ts-advance ts)) members))
         (php-ts-skip-semis ts))
        ((and (eq (php-ts-peek-type ts) :T-KEYWORD) (eq (php-ts-peek-value ts) :function))
         (let ((method (php-cst-parse-statement ts)))
           (when method (push method members))))
        (t (php-ts-advance ts))))
    members))

(defun %parse-php-class-def (ts kw-tok)
  "Parse `class Name [extends Base] [implements ...] { members }` starting after the 'class' keyword."
  (let ((name-tok (php-ts-expect ts :T-IDENT nil "class name"))
        (extends nil))
    (when (and (not (php-ts-at-end-p ts))
               (eq (php-ts-peek-type ts) :T-KEYWORD)
               (eq (php-ts-peek-value ts) :extends))
      (php-ts-advance ts)
      (let ((super-tok (php-ts-expect ts :T-IDENT nil "superclass")))
        (when super-tok (setf extends (%php-tok-to-cst super-tok)))))
    (when (and (not (php-ts-at-end-p ts))
               (eq (php-ts-peek-type ts) :T-KEYWORD)
               (eq (php-ts-peek-value ts) :implements))
      (php-ts-advance ts)
      (loop (php-ts-expect ts :T-IDENT nil "interface")
            (unless (eq (php-ts-peek-type ts) :T-COMMA) (return))
            (php-ts-advance ts)))
    (php-ts-expect ts :T-LBRACE nil "class body")
    (let ((members (%parse-php-class-body-members ts)))
      (php-ts-expect ts :T-RBRACE nil "class body")
      (%php-cst-interior :class-def
                         (append (list (%php-tok-to-cst kw-tok))
                                 (when name-tok (list (%php-tok-to-cst name-tok)))
                                 (when extends (list extends))
                                 (nreverse members))))))

;;; ─── Named keyword-statement parsers ───────────────────────────────────────

(defun %parse-php-echo-stmt (ts)
  (let ((kw-tok (php-ts-advance ts))
        (expr (php-cst-parse-expr ts)))
    (php-ts-skip-semis ts)
    (%php-cst-interior :echo (list (%php-tok-to-cst kw-tok) expr))))

(defun %parse-php-return-stmt (ts)
  (let ((kw-tok (php-ts-advance ts)))
    (if (eq (php-ts-peek-type ts) :T-SEMI)
        (progn (php-ts-skip-semis ts)
               (%php-cst-interior :return (list (%php-tok-to-cst kw-tok))))
        (let ((expr (php-cst-parse-expr ts)))
          (php-ts-skip-semis ts)
          (%php-cst-interior :return (list (%php-tok-to-cst kw-tok) expr))))))

(defun %parse-php-if-stmt (ts)
  (let* ((kw-tok     (prog1 (php-ts-advance ts) (php-ts-expect ts :T-LPAREN nil "if condition")))
          (cond-expr  (prog1 (php-cst-parse-expr ts) (php-ts-expect ts :T-RPAREN nil "if condition")))
          (then-stmts (%php-cst-parse-stmt-body ts))
          (elseif-stmts nil)
          (else-stmts nil))
    (loop while (%php-cst-keyword-p ts :elseif)
          do (let* ((elseif-tok (prog1 (php-ts-advance ts)
                                  (php-ts-expect ts :T-LPAREN nil "elseif condition")))
                    (elseif-cond (prog1 (php-cst-parse-expr ts)
                                   (php-ts-expect ts :T-RPAREN nil "elseif condition")))
                    (elseif-body (%php-cst-parse-stmt-body ts)))
               (push (%php-cst-interior :elseif
                                        (list (%php-tok-to-cst elseif-tok)
                                              elseif-cond
                                              (%php-cst-interior :then elseif-body)))
                     elseif-stmts)))
    (when (%php-cst-keyword-p ts :else)
      (php-ts-advance ts)
      (setf else-stmts (%php-cst-parse-stmt-body ts)))
    (%php-cst-interior :if
                        (append (list (%php-tok-to-cst kw-tok) cond-expr)
                                 (list (%php-cst-interior :then then-stmts))
                                (nreverse elseif-stmts)
                                (when else-stmts
                                  (list (%php-cst-interior :else else-stmts)))))))

(defun %parse-php-while-stmt (ts)
  (let* ((kw-tok     (prog1 (php-ts-advance ts) (php-ts-expect ts :T-LPAREN nil "while condition")))
         (cond-expr  (prog1 (php-cst-parse-expr ts) (php-ts-expect ts :T-RPAREN nil "while condition")))
         (body-stmts (php-cst-parse-block ts)))
    (%php-cst-interior :while (list (%php-tok-to-cst kw-tok) cond-expr
                                    (%php-cst-interior :body body-stmts)))))

(defun %parse-php-for-stmt (ts)
  (let* ((kw-tok     (prog1 (php-ts-advance ts)     (php-ts-expect ts :T-LPAREN nil "for")))
         (init       (prog1 (php-cst-parse-expr ts)  (php-ts-expect ts :T-SEMI   nil "for")))
         (cond-expr  (prog1 (php-cst-parse-expr ts)  (php-ts-expect ts :T-SEMI   nil "for")))
         (incr       (prog1 (php-cst-parse-expr ts)  (php-ts-expect ts :T-RPAREN nil "for")))
         (body-stmts (php-cst-parse-block ts)))
    (%php-cst-interior :for (list (%php-tok-to-cst kw-tok) init cond-expr incr
                                  (%php-cst-interior :body body-stmts)))))

(defun %parse-php-foreach-stmt (ts)
  (let* ((kw-tok   (prog1 (php-ts-advance ts) (php-ts-expect ts :T-LPAREN nil "foreach")))
         (arr-expr (prog1 (php-cst-parse-expr ts) (php-ts-expect ts :T-KEYWORD :as "foreach")))
         (var-tok  (php-ts-expect ts :T-VAR nil "foreach")))
    (when (and (eq (php-ts-peek-type ts) :T-OP)
               (equal "=>" (php-ts-peek-value ts)))
      (php-ts-advance ts)
      (setf var-tok (php-ts-expect ts :T-VAR nil "foreach value")))
    (php-ts-expect ts :T-RPAREN nil "foreach")
    (let ((body-stmts (php-cst-parse-block ts)))
      (%php-cst-interior :foreach
                         (list (%php-tok-to-cst kw-tok) arr-expr
                               (when var-tok (%php-tok-to-cst var-tok))
                               (%php-cst-interior :body body-stmts))))))

(defun %parse-php-function-def-stmt (ts)
  (let* ((kw-tok   (php-ts-advance ts))
         (name-tok (php-ts-expect ts :T-IDENT nil "function name"))
         (params   (php-cst-parse-param-list ts)))
    (when (eq (php-ts-peek-type ts) :T-COLON)
      (php-ts-advance ts)
      (when (member (php-ts-peek-type ts) '(:T-TYPE :T-IDENT :T-NULLABLE))
        (php-ts-advance ts)
        (when (member (php-ts-peek-type ts) '(:T-TYPE :T-IDENT))
          (php-ts-advance ts))))
    (let ((body-stmts (php-cst-parse-block ts)))
      (%php-cst-interior :function-def
                         (append (list (%php-tok-to-cst kw-tok))
                                 (when name-tok (list (%php-tok-to-cst name-tok)))
                                 (list (%php-cst-interior :params params))
                                 (list (%php-cst-interior :body body-stmts)))))))

(defun %parse-php-class-def-stmt (ts)
  (%parse-php-class-def ts (php-ts-advance ts)))

(defun %parse-php-break-stmt (ts)
  (let ((tok (php-ts-advance ts)))
    (php-ts-skip-semis ts)
    (%php-cst-interior :break (list (%php-tok-to-cst tok)))))

(defun %parse-php-continue-stmt (ts)
  (let ((tok (php-ts-advance ts))
        (expr nil))
    (unless (eq (php-ts-peek-type ts) :T-SEMI)
      (setf expr (php-cst-parse-expr ts)))
    (php-ts-skip-semis ts)
    (%php-cst-interior :continue
                       (append (list (%php-tok-to-cst tok))
                               (when expr (list expr))))))

(defun %parse-php-try-catch-stmt (ts)
  (let ((kw-tok (php-ts-advance ts)))
    (let ((try-body (php-cst-parse-block ts))
          (catches nil)
          (finally-body nil))
      (loop while (and (not (php-ts-at-end-p ts))
                       (eq (php-ts-peek-type ts) :T-KEYWORD)
                       (eq (php-ts-peek-value ts) :catch))
            do (php-ts-advance ts)
               (php-ts-expect ts :T-LPAREN nil "catch")
               (let ((type-tok (php-ts-expect ts :T-IDENT nil "catch type"))
                     (var-tok  (php-ts-expect ts :T-VAR  nil "catch variable")))
                 (php-ts-expect ts :T-RPAREN nil "catch")
                 (let ((catch-body (php-cst-parse-block ts)))
                    (push (%php-cst-interior :catch
                            (append (when type-tok (list (%php-tok-to-cst type-tok)))
                                    (when var-tok  (list (%php-tok-to-cst var-tok)))
                                    catch-body))
                          catches))))
      (when (and (not (php-ts-at-end-p ts))
                 (eq (php-ts-peek-type ts) :T-KEYWORD)
                 (eq (php-ts-peek-value ts) :finally))
        (php-ts-advance ts)
        (setf finally-body (php-cst-parse-block ts)))
      (%php-cst-interior :try-catch
                          (append (list (%php-tok-to-cst kw-tok))
                                  (list (%php-cst-interior :try-body try-body))
                                  (nreverse catches)
                                  (when finally-body
                                    (list (%php-cst-interior :finally finally-body))))))))

(defun %php-cst-keyword-p (ts keyword)
  (and (not (php-ts-at-end-p ts))
       (eq (php-ts-peek-type ts) :T-KEYWORD)
       (eq (php-ts-peek-value ts) keyword)))

(defun %php-cst-parse-stmt-body (ts)
  (if (eq (php-ts-peek-type ts) :T-LBRACE)
      (php-cst-parse-block ts)
      (let ((stmt (php-cst-parse-statement ts)))
        (if stmt (list stmt) nil))))

(defun %php-cst-skip-to-stmt-end (ts)
  (let ((items nil) (depth 0))
    (loop while (and (not (php-ts-at-end-p ts))
                     (or (> depth 0)
                         (not (member (php-ts-peek-type ts) '(:T-SEMI :T-RBRACE)))))
          do (let ((tok (php-ts-advance ts)))
               (when (member (php-tok-type tok) '(:T-LPAREN :T-LBRACE :T-LBRACKET)) (incf depth))
               (when (member (php-tok-type tok) '(:T-RPAREN :T-RBRACE :T-RBRACKET)) (decf depth))
               (push (%php-tok-to-cst tok) items)))
    (php-ts-skip-semis ts)
    (nreverse items)))

(defun %parse-php-do-while-stmt (ts)
  (let ((kw-tok (php-ts-advance ts)))
    (let ((body (%php-cst-parse-stmt-body ts)))
      (php-ts-expect ts :T-KEYWORD :while "do-while")
      (php-ts-expect ts :T-LPAREN nil "do-while")
      (let ((cond-expr (php-cst-parse-expr ts)))
        (php-ts-expect ts :T-RPAREN nil "do-while")
        (php-ts-skip-semis ts)
        (%php-cst-interior :do-while
                           (list (%php-tok-to-cst kw-tok) cond-expr
                                 (%php-cst-interior :body body)))))))

(defun %parse-php-switch-stmt (ts)
  (let* ((kw-tok (prog1 (php-ts-advance ts) (php-ts-expect ts :T-LPAREN nil "switch")))
         (expr (prog1 (php-cst-parse-expr ts) (php-ts-expect ts :T-RPAREN nil "switch")))
         (sections nil))
    (php-ts-expect ts :T-LBRACE nil "switch")
    (loop
      (php-ts-skip-semis ts)
      (when (or (php-ts-at-end-p ts) (eq (php-ts-peek-type ts) :T-RBRACE)) (return))
      (cond
        ((%php-cst-keyword-p ts :case)
         (let ((case-tok (php-ts-advance ts))
               (case-expr (php-cst-parse-expr ts))
               (body nil))
           (when (eq (php-ts-peek-type ts) :T-COLON) (php-ts-advance ts))
           (loop until (or (php-ts-at-end-p ts) (eq (php-ts-peek-type ts) :T-RBRACE)
                           (%php-cst-keyword-p ts :case) (%php-cst-keyword-p ts :default))
                 do (let ((stmt (php-cst-parse-statement ts)))
                      (when stmt (push stmt body))))
           (push (%php-cst-interior :case
                                    (list (%php-tok-to-cst case-tok) case-expr
                                          (%php-cst-interior :body (nreverse body))))
                 sections)))
        ((%php-cst-keyword-p ts :default)
         (let ((default-tok (php-ts-advance ts)) (body nil))
           (when (eq (php-ts-peek-type ts) :T-COLON) (php-ts-advance ts))
           (loop until (or (php-ts-at-end-p ts) (eq (php-ts-peek-type ts) :T-RBRACE)
                           (%php-cst-keyword-p ts :case))
                 do (let ((stmt (php-cst-parse-statement ts)))
                      (when stmt (push stmt body))))
           (push (%php-cst-interior :default
                                    (list (%php-tok-to-cst default-tok)
                                          (%php-cst-interior :body (nreverse body))))
                 sections)))
        (t (%php-cst-skip-to-stmt-end ts))))
    (php-ts-expect ts :T-RBRACE nil "switch")
    (%php-cst-interior :switch
                       (list (%php-tok-to-cst kw-tok) expr
                             (%php-cst-interior :sections (nreverse sections))))))

(defun %parse-php-include-like-stmt (ts)
  (let* ((tok (php-ts-advance ts))
         (expr (php-cst-parse-expr ts)))
    (php-ts-skip-semis ts)
    (%php-cst-interior :include-like (list (%php-tok-to-cst tok) expr))))

(defun %parse-php-noop-keyword-stmt (ts)
  (let ((tok (php-ts-advance ts))
        (items (%php-cst-skip-to-stmt-end ts)))
    (%php-cst-interior :noop (cons (%php-tok-to-cst tok) items))))

(defun %parse-php-goto-stmt (ts)
  (let ((tok (php-ts-advance ts))
        (label (php-ts-expect ts :T-IDENT nil "goto")))
    (php-ts-skip-semis ts)
    (%php-cst-interior :goto (append (list (%php-tok-to-cst tok))
                                     (when label (list (%php-tok-to-cst label)))))))

(defun %parse-php-label-stmt (ts)
  (let ((label (php-ts-advance ts)))
    (php-ts-expect ts :T-COLON nil "label")
    (%php-cst-interior :label (list (%php-tok-to-cst label)))))

(defun %parse-php-classlike-stmt (ts)
  (let ((kw-tok (php-ts-advance ts))
        (children nil))
    (loop while (and (not (php-ts-at-end-p ts))
                     (not (eq (php-ts-peek-type ts) :T-LBRACE)))
          do (push (%php-tok-to-cst (php-ts-advance ts)) children))
    (php-ts-expect ts :T-LBRACE nil "class-like")
    (loop until (or (php-ts-at-end-p ts) (eq (php-ts-peek-type ts) :T-RBRACE))
          do (push (%php-tok-to-cst (php-ts-advance ts)) children))
    (php-ts-expect ts :T-RBRACE nil "class-like")
    (%php-cst-interior :class-like (cons (%php-tok-to-cst kw-tok) (nreverse children)))))

(defun %parse-php-expr-stmt (ts)
  (let ((expr (php-cst-parse-expr ts)))
    (php-ts-skip-semis ts)
    expr))

;;; ─── Keyword → parser dispatch table ───────────────────────────────────────

(defvar *php-keyword-statement-parsers*
  '((:echo     . %parse-php-echo-stmt)
     (:return   . %parse-php-return-stmt)
     (:if       . %parse-php-if-stmt)
     (:while    . %parse-php-while-stmt)
     (:for      . %parse-php-for-stmt)
     (:foreach  . %parse-php-foreach-stmt)
     (:do       . %parse-php-do-while-stmt)
     (:switch   . %parse-php-switch-stmt)
     (:function . %parse-php-function-def-stmt)
     (:class    . %parse-php-class-def-stmt)
     (:trait    . %parse-php-classlike-stmt)
     (:interface . %parse-php-classlike-stmt)
     (:enum     . %parse-php-classlike-stmt)
     (:break    . %parse-php-break-stmt)
     (:continue . %parse-php-continue-stmt)
     (:try      . %parse-php-try-catch-stmt)
     (:include  . %parse-php-include-like-stmt)
     (:require  . %parse-php-include-like-stmt)
     (:include-once . %parse-php-include-like-stmt)
     (:require-once . %parse-php-include-like-stmt)
     (:declare  . %parse-php-noop-keyword-stmt)
     (:goto     . %parse-php-goto-stmt)
     (:namespace . %parse-php-noop-keyword-stmt)
     (:use      . %parse-php-noop-keyword-stmt))
  "Keyword → parser function alist for PHP statement dispatch.")

(defun php-cst-parse-statement (ts)
  "Parse a single PHP statement, dispatching on keyword via *php-keyword-statement-parsers*."
  (cond
    ((eq (php-ts-peek-type ts) :T-KEYWORD)
     (let* ((value  (php-ts-peek-value ts))
            (parser (cdr (assoc value *php-keyword-statement-parsers*))))
       (if parser (funcall parser ts) (%parse-php-expr-stmt ts))))
    ((and (eq (php-ts-peek-type ts) :T-IDENT)
          (let ((next (second (php-token-stream-tokens ts))))
            (and next (eq (php-tok-type next) :T-COLON))))
     (%parse-php-label-stmt ts))
    ((and (eq (php-ts-peek-type ts) :T-IDENT)
          (%php-include-keyword-value (php-ts-peek-value ts)))
     (let ((parser (cdr (assoc (%php-include-keyword-value (php-ts-peek-value ts))
                               *php-keyword-statement-parsers*))))
       (if parser (funcall parser ts) (%parse-php-expr-stmt ts))))
    (t (%parse-php-expr-stmt ts))))

;;; ─── Top-Level Entry Point ──────────────────────────────────────────────────

(defun parse-php-source-to-cst (source &optional source-file)
  "Parse PHP SOURCE string into CST nodes.
   Returns (values cst-list diagnostics)."
  (let* ((tokens (tokenize-php-source source))
         (ts (make-php-token-stream :tokens tokens
                                    :source source
                                    :source-file source-file))
         (forms nil))
    (loop
      (php-ts-skip-semis ts)
      (when (php-ts-at-end-p ts) (return))
      (let ((form (php-cst-parse-statement ts)))
        (when form (push form forms))))
    (values (nreverse forms)
            (nreverse (php-token-stream-diagnostics ts)))))
