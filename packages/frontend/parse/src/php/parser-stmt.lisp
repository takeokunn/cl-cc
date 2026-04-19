;;;; packages/frontend/parse/src/php/parser-stmt.lisp — PHP Parser: Statement Layer
;;;;
;;;; Contains statement-level parsing, loop lowering, and the top-level entry
;;;; point. Expression-level parsing (php-parse-primary through php-parse-arglist)
;;;; is in parser.lisp (loads before).
;;;;
;;;; Architecture: *php-stmt-parsers* hash table maps keyword values to handlers.
;;;; Each handler is a named function (php-parse-KEYWORD-stmt) registered via
;;;; define-php-stmt-parser. php-parse-statement is a pure dispatcher (~10 lines).
;;;;
;;;; Load order: after parser.lisp.

(in-package :cl-cc/parse)

;;; ─── Statement Dispatch Table ───────────────────────────────────────────────
;;;
;;; Prolog-style clause database: each PHP keyword maps to its parsing rule.
;;; Populated by define-php-stmt-parser registrations below.

(defvar *php-stmt-parsers* (make-hash-table)
  "Maps PHP keyword values (:echo :return :if …) to handler functions.
Each handler takes (stream-after-keyword known-vars) and returns
(values ast remaining-stream updated-known-vars).")

(defmacro define-php-stmt-parser (keyword (stream known-vars) &body body)
  "Register a statement parser for KEYWORD in *php-stmt-parsers*.
STREAM is positioned after the keyword token has been consumed."
  (list 'setf
        (list 'gethash keyword '*php-stmt-parsers*)
        (cons 'lambda (cons (list stream known-vars) body))))

;;; ─── Shared Parsing Helpers ─────────────────────────────────────────────────

(defun %php-consume-expected (token stream)
  "Consume TOKEN from STREAM (signalling on mismatch); return the remaining stream."
  (nth-value 1 (php-expect token stream)))

(defun %php-parse-paren-expr (stream known-vars)
  "Parse a parenthesised expression ( expr ). Returns (values expr rest known-vars)."
  (let ((rest (%php-consume-expected :T-LPAREN stream)))
    (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
      (values expr (%php-consume-expected :T-RPAREN rest2) kv2))))

(defun %php-parse-expr-stmt (stream known-vars)
  "Parse an expression statement; skip trailing semicolons."
  (multiple-value-bind (expr rest kv) (php-parse-expr stream known-vars)
    (values expr (php-skip-semis rest) kv)))

;;; ─── Block Parser ────────────────────────────────────────────────────────────

(defun php-parse-block (stream known-vars)
  "Parse { stmt* }. Returns (values stmt-list remaining-stream known-vars)."
  (multiple-value-bind (tok rest) (php-expect :T-LBRACE stream)
    (declare (ignore tok))
    (let ((stmts nil) (current rest) (kv known-vars))
      (loop
        (setf current (php-skip-semis current))
        (when (or (php-at-eof-p current) (eq (php-peek-type current) :T-RBRACE))
          (return))
        (multiple-value-bind (stmt rest2 kv2) (php-parse-statement current kv)
          (when stmt (push stmt stmts))
          (setf current rest2 kv kv2)))
      (multiple-value-bind (tok2 rest2) (php-expect :T-RBRACE current)
        (declare (ignore tok2))
        (values (nreverse stmts) rest2 kv)))))

;;; ─── Parameter List Parser ───────────────────────────────────────────────────

(defun %php-skip-type-annotation (stream)
  "Consume an optional type annotation (nullable? type) from STREAM.
Returns the stream positioned after the type tokens."
  (if (or (eq (php-peek-type stream) :T-TYPE)
          (eq (php-peek-type stream) :T-IDENT)
          (eq (php-peek-type stream) :T-NULLABLE))
      (let ((rest (cdr stream)))
        (if (or (eq (php-peek-type rest) :T-TYPE)
                (eq (php-peek-type rest) :T-IDENT))
            (cdr rest)
            rest))
      stream))

(defun php-parse-param-list (stream)
  "Parse (type? $param, ...) for function/method definitions.
Returns (values param-symbol-list remaining-stream)."
  (multiple-value-bind (tok rest) (php-expect :T-LPAREN stream)
    (declare (ignore tok))
    (if (eq (php-peek-type rest) :T-RPAREN)
        (multiple-value-bind (tok2 rest2) (php-consume rest)
          (declare (ignore tok2))
          (values nil rest2))
        (let ((params nil) (current rest))
          (loop
            (setf current (%php-skip-type-annotation current))
            (multiple-value-bind (var-tok rest2) (php-expect :T-VAR current)
              (push (php-var-sym (php-tok-value var-tok)) params)
              (setf current rest2))
            ;; Skip default value: consume until , or )
            (when (and current (eq (php-peek-type current) :T-OP)
                       (equal "=" (php-peek-value current)))
              (setf current (cdr current))
              (loop while (and current
                               (not (eq (php-peek-type current) :T-COMMA))
                               (not (eq (php-peek-type current) :T-RPAREN)))
                    do (setf current (cdr current))))
            (if (and current (eq (php-peek-type current) :T-COMMA))
                (setf current (cdr current))
                (return)))
          (multiple-value-bind (tok2 rest2) (php-expect :T-RPAREN current)
            (declare (ignore tok2))
            (values (nreverse params) rest2))))))

;;; ─── Individual Statement Handlers ──────────────────────────────────────────
;;;
;;; Each handler receives stream positioned AFTER the keyword token.
;;; Returns (values ast rest known-vars).

(define-php-stmt-parser :echo (rest known-vars)
  (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
    (values (make-ast-print :expr expr) (php-skip-semis rest2) kv2)))

(define-php-stmt-parser :return (rest known-vars)
  (if (eq (php-peek-type rest) :T-SEMI)
      (values (make-ast-return-from :name nil :value (make-ast-quote :value nil))
              (php-skip-semis rest) known-vars)
      (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
        (values (make-ast-return-from :name nil :value expr)
                (php-skip-semis rest2) kv2))))

(define-php-stmt-parser :if (rest known-vars)
  (multiple-value-bind (cond-expr rest2 kv2) (%php-parse-paren-expr rest known-vars)
    (multiple-value-bind (then-stmts rest3 kv3) (php-parse-block rest2 kv2)
      (let ((else-ast (make-ast-quote :value nil)))
        (when (and rest3 (eq (php-peek-type rest3) :T-KEYWORD)
                   (eq (php-peek-value rest3) :else))
          (setf rest3 (cdr rest3))
          (multiple-value-bind (else-stmts rest4 kv4) (php-parse-block rest3 kv3)
            (setf else-ast (make-ast-progn :forms else-stmts)
                  rest3 rest4 kv3 kv4)))
        (values (make-ast-if :cond cond-expr
                             :then (make-ast-progn :forms then-stmts)
                             :else else-ast)
                rest3 kv3)))))

(define-php-stmt-parser :while (rest known-vars)
  (multiple-value-bind (cond-expr rest2 kv2) (%php-parse-paren-expr rest known-vars)
    (multiple-value-bind (body-stmts rest3 kv3) (php-parse-block rest2 kv2)
      (values (php-lower-while cond-expr body-stmts) rest3 kv3))))

(define-php-stmt-parser :for (rest known-vars)
  (let ((rest (%php-consume-expected :T-LPAREN rest)))
    (multiple-value-bind (init rest kv) (php-parse-expr rest known-vars)
      (let ((rest (%php-consume-expected :T-SEMI rest)))
        (multiple-value-bind (cond-expr rest kv) (php-parse-expr rest kv)
          (let ((rest (%php-consume-expected :T-SEMI rest)))
            (multiple-value-bind (incr rest kv) (php-parse-expr rest kv)
              (let ((rest (%php-consume-expected :T-RPAREN rest)))
                (multiple-value-bind (body-stmts rest _) (php-parse-block rest kv)
                  (declare (ignore _))
                  (values (make-ast-progn
                           :forms (list init (php-lower-while cond-expr
                                                              (append body-stmts (list incr)))))
                          rest kv))))))))))

(define-php-stmt-parser :foreach (rest known-vars)
  (let ((rest2 (%php-consume-expected :T-LPAREN rest)))
    (multiple-value-bind (arr-expr rest3 kv3) (php-parse-expr rest2 known-vars)
      (let ((rest4 (if (and rest3 (eq (php-peek-type rest3) :T-KEYWORD)
                            (eq (php-peek-value rest3) :as))
                       (cdr rest3)
                       (error "foreach: expected 'as'"))))
        (multiple-value-bind (var-tok rest5) (php-expect :T-VAR rest4)
          (let ((var-sym (php-var-sym (php-tok-value var-tok)))
                (rest6 rest5))
            ;; Skip optional key => value form; bind to the value variable
            (when (and rest6 (eq (php-peek-type rest6) :T-OP)
                       (equal "=>" (php-peek-value rest6)))
              (setf rest6 (cdr rest6))
              (multiple-value-bind (val-tok rest7) (php-expect :T-VAR rest6)
                (setf var-sym (php-var-sym (php-tok-value val-tok)))
                (setf rest6 rest7)))
            (multiple-value-bind (body-stmts rest8 kv8) (php-parse-block (%php-consume-expected :T-RPAREN rest6) kv3)
                (values (php-lower-foreach arr-expr var-sym body-stmts)
                        rest8 kv8))))))))

(define-php-stmt-parser :function (rest known-vars)
  (multiple-value-bind (name-tok rest) (php-expect :T-IDENT rest)
    (let ((fn-name (php-ident-sym (php-tok-value name-tok))))
      (multiple-value-bind (params rest) (php-parse-param-list rest)
        (let ((rest (%php-skip-return-type rest)))
          (multiple-value-bind (body-stmts rest _) (php-parse-block rest (append params known-vars))
            (declare (ignore _))
            (values (make-ast-defun :name fn-name :params params :body body-stmts)
                    rest known-vars)))))))

(defun %php-skip-return-type (stream)
  "Consume an optional : type annotation after a function parameter list.
Returns the stream positioned after the return type (or unchanged if absent)."
  (if (and stream (eq (php-peek-type stream) :T-COLON))
      (%php-skip-type-annotation (cdr stream))
      stream))

