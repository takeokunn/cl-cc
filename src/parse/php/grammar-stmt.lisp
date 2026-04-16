;;;; src/parse/php/grammar-stmt.lisp — PHP CST Grammar: Statement Layer
;;;;
;;;; Contains statement-level CST parsers and the top-level entry point.
;;;; Expression-level parsers (php-cst-parse-primary through php-cst-parse-expr)
;;;; are in grammar.lisp (loads before).
;;;;
;;;; Load order: after grammar.lisp.

(in-package :cl-cc/parse)

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

(defun php-cst-parse-statement (ts)
  "Parse a single PHP statement."
  (let ((type (php-ts-peek-type ts))
        (value (php-ts-peek-value ts)))
    (cond
      ;; echo expr;
      ((and (eq type :T-KEYWORD) (eq value :echo))
       (let ((kw-tok (php-ts-advance ts))
             (expr (php-cst-parse-expr ts)))
         (php-ts-skip-semis ts)
         (%php-cst-interior :echo (list (%php-tok-to-cst kw-tok) expr))))

      ;; return [expr];
      ((and (eq type :T-KEYWORD) (eq value :return))
       (let ((kw-tok (php-ts-advance ts)))
         (if (eq (php-ts-peek-type ts) :T-SEMI)
             (progn (php-ts-skip-semis ts)
                    (%php-cst-interior :return (list (%php-tok-to-cst kw-tok))))
             (let ((expr (php-cst-parse-expr ts)))
               (php-ts-skip-semis ts)
               (%php-cst-interior :return (list (%php-tok-to-cst kw-tok) expr))))))

      ;; if (cond) { } [else { }]
      ((and (eq type :T-KEYWORD) (eq value :if))
       (let ((kw-tok (php-ts-advance ts)))
         (php-ts-expect ts :T-LPAREN nil "if condition")
         (let ((cond-expr (php-cst-parse-expr ts)))
           (php-ts-expect ts :T-RPAREN nil "if condition")
           (let ((then-stmts (php-cst-parse-block ts))
                 (else-stmts nil))
             (when (and (not (php-ts-at-end-p ts))
                        (eq (php-ts-peek-type ts) :T-KEYWORD)
                        (eq (php-ts-peek-value ts) :else))
               (php-ts-advance ts)
               (setf else-stmts (php-cst-parse-block ts)))
             (%php-cst-interior :if
                                (append (list (%php-tok-to-cst kw-tok) cond-expr)
                                        (list (%php-cst-interior :then then-stmts))
                                        (when else-stmts
                                          (list (%php-cst-interior :else else-stmts)))))))))

      ;; while (cond) { }
      ((and (eq type :T-KEYWORD) (eq value :while))
       (let ((kw-tok (php-ts-advance ts)))
         (php-ts-expect ts :T-LPAREN nil "while condition")
         (let ((cond-expr (php-cst-parse-expr ts)))
           (php-ts-expect ts :T-RPAREN nil "while condition")
           (let ((body-stmts (php-cst-parse-block ts)))
             (%php-cst-interior :while
                                (list (%php-tok-to-cst kw-tok) cond-expr
                                      (%php-cst-interior :body body-stmts)))))))

      ;; for (init; cond; incr) { }
      ((and (eq type :T-KEYWORD) (eq value :for))
       (let ((kw-tok (php-ts-advance ts)))
         (php-ts-expect ts :T-LPAREN nil "for")
         (let ((init (php-cst-parse-expr ts)))
           (php-ts-expect ts :T-SEMI nil "for")
           (let ((cond-expr (php-cst-parse-expr ts)))
             (php-ts-expect ts :T-SEMI nil "for")
             (let ((incr (php-cst-parse-expr ts)))
               (php-ts-expect ts :T-RPAREN nil "for")
               (let ((body-stmts (php-cst-parse-block ts)))
                 (%php-cst-interior :for
                                    (list (%php-tok-to-cst kw-tok) init cond-expr incr
                                          (%php-cst-interior :body body-stmts)))))))))

      ;; foreach ($arr as [$k =>] $v) { }
      ((and (eq type :T-KEYWORD) (eq value :foreach))
       (let ((kw-tok (php-ts-advance ts)))
         (php-ts-expect ts :T-LPAREN nil "foreach")
         (let ((arr-expr (php-cst-parse-expr ts)))
           ;; expect 'as'
           (php-ts-expect ts :T-KEYWORD :as "foreach")
           (let ((var-tok (php-ts-expect ts :T-VAR nil "foreach")))
             ;; optional => $val
             (when (and (eq (php-ts-peek-type ts) :T-OP)
                        (equal "=>" (php-ts-peek-value ts)))
               (php-ts-advance ts)
               (setf var-tok (php-ts-expect ts :T-VAR nil "foreach value")))
             (php-ts-expect ts :T-RPAREN nil "foreach")
             (let ((body-stmts (php-cst-parse-block ts)))
               (%php-cst-interior :foreach
                                  (list (%php-tok-to-cst kw-tok)
                                        arr-expr
                                        (when var-tok (%php-tok-to-cst var-tok))
                                        (%php-cst-interior :body body-stmts))))))))

      ;; function name(...) { }
      ((and (eq type :T-KEYWORD) (eq value :function))
       (let ((kw-tok (php-ts-advance ts))
             (name-tok (php-ts-expect ts :T-IDENT nil "function name")))
         ;; params
         (let ((params (php-cst-parse-param-list ts)))
           ;; Skip optional return type
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
                                        (list (%php-cst-interior :body body-stmts))))))))

      ;; class Name [extends Base] [implements ...] { }
      ((and (eq type :T-KEYWORD) (eq value :class))
       (let ((kw-tok (php-ts-advance ts))
             (name-tok (php-ts-expect ts :T-IDENT nil "class name")))
         ;; extends
         (let ((extends nil))
           (when (and (not (php-ts-at-end-p ts))
                      (eq (php-ts-peek-type ts) :T-KEYWORD)
                      (eq (php-ts-peek-value ts) :extends))
             (php-ts-advance ts)
             (let ((super-tok (php-ts-expect ts :T-IDENT nil "superclass")))
               (when super-tok (setf extends (%php-tok-to-cst super-tok)))))
           ;; implements (skip)
           (when (and (not (php-ts-at-end-p ts))
                      (eq (php-ts-peek-type ts) :T-KEYWORD)
                      (eq (php-ts-peek-value ts) :implements))
             (php-ts-advance ts)
             (loop
               (php-ts-expect ts :T-IDENT nil "interface")
               (unless (eq (php-ts-peek-type ts) :T-COMMA) (return))
               (php-ts-advance ts)))
           ;; class body
           (php-ts-expect ts :T-LBRACE nil "class body")
           (let ((members nil))
             (loop
               (php-ts-skip-semis ts)
               (when (or (php-ts-at-end-p ts) (eq (php-ts-peek-type ts) :T-RBRACE))
                 (return))
               ;; Skip visibility modifiers
               (loop while (and (not (php-ts-at-end-p ts))
                                (eq (php-ts-peek-type ts) :T-KEYWORD)
                                (member (php-ts-peek-value ts)
                                        '(:public :private :protected :static
                                          :abstract :final :readonly)))
                     do (php-ts-advance ts))
               (cond
                 ;; Property: $var
                 ((eq (php-ts-peek-type ts) :T-VAR)
                  (push (%php-tok-to-cst (php-ts-advance ts)) members)
                  (php-ts-skip-semis ts))
                 ;; Typed property
                 ((member (php-ts-peek-type ts) '(:T-TYPE :T-NULLABLE))
                  (php-ts-advance ts)
                  (when (member (php-ts-peek-type ts) '(:T-TYPE :T-IDENT))
                    (php-ts-advance ts))
                  (when (eq (php-ts-peek-type ts) :T-VAR)
                    (push (%php-tok-to-cst (php-ts-advance ts)) members))
                  (php-ts-skip-semis ts))
                 ;; Method
                 ((and (eq (php-ts-peek-type ts) :T-KEYWORD)
                       (eq (php-ts-peek-value ts) :function))
                  (let ((method (php-cst-parse-statement ts)))
                    (when method (push method members))))
                 ;; Unknown: skip
                 (t (php-ts-advance ts))))
             (php-ts-expect ts :T-RBRACE nil "class body")
             (%php-cst-interior :class-def
                                (append (list (%php-tok-to-cst kw-tok))
                                        (when name-tok (list (%php-tok-to-cst name-tok)))
                                        (when extends (list extends))
                                        (nreverse members)))))))

      ;; break;
      ((and (eq type :T-KEYWORD) (eq value :break))
       (let ((tok (php-ts-advance ts)))
         (php-ts-skip-semis ts)
         (%php-cst-interior :break (list (%php-tok-to-cst tok)))))

      ;; continue;
      ((and (eq type :T-KEYWORD) (eq value :continue))
       (let ((tok (php-ts-advance ts)))
         (php-ts-skip-semis ts)
         (%php-cst-interior :continue (list (%php-tok-to-cst tok)))))

      ;; try { } catch (Type $var) { }
      ((and (eq type :T-KEYWORD) (eq value :try))
       (let ((kw-tok (php-ts-advance ts)))
         (let ((try-body (php-cst-parse-block ts))
               (catches nil))
           (loop while (and (not (php-ts-at-end-p ts))
                            (eq (php-ts-peek-type ts) :T-KEYWORD)
                            (eq (php-ts-peek-value ts) :catch))
                 do (php-ts-advance ts)
                    (php-ts-expect ts :T-LPAREN nil "catch")
                    (let ((type-tok (php-ts-expect ts :T-IDENT nil "catch type"))
                          (var-tok (php-ts-expect ts :T-VAR nil "catch variable")))
                      (php-ts-expect ts :T-RPAREN nil "catch")
                      (let ((catch-body (php-cst-parse-block ts)))
                        (push (%php-cst-interior :catch
                               (append (when type-tok (list (%php-tok-to-cst type-tok)))
                                       (when var-tok (list (%php-tok-to-cst var-tok)))
                                       catch-body))
                              catches))))
           (%php-cst-interior :try-catch
                              (append (list (%php-tok-to-cst kw-tok))
                                      (list (%php-cst-interior :try-body try-body))
                                      (nreverse catches))))))

      ;; Expression statement
      (t
       (let ((expr (php-cst-parse-expr ts)))
         (php-ts-skip-semis ts)
         expr)))))

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
