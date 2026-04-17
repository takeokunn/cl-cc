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
  `(setf (gethash ,keyword *php-stmt-parsers*)
         (lambda (,stream ,known-vars) ,@body)))

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
  (multiple-value-bind (tok rest2) (php-expect :T-LPAREN rest)
    (declare (ignore tok))
    (multiple-value-bind (cond-expr rest3 kv3) (php-parse-expr rest2 known-vars)
      (multiple-value-bind (tok2 rest4) (php-expect :T-RPAREN rest3)
        (declare (ignore tok2))
        (multiple-value-bind (then-stmts rest5 kv5) (php-parse-block rest4 kv3)
          (let ((else-ast (make-ast-quote :value nil)))
            (when (and rest5 (eq (php-peek-type rest5) :T-KEYWORD)
                       (eq (php-peek-value rest5) :else))
              (setf rest5 (cdr rest5))
              (multiple-value-bind (else-stmts rest6 kv6) (php-parse-block rest5 kv5)
                (setf else-ast (make-ast-progn :forms else-stmts)
                      rest5 rest6
                      kv5 kv6)))
            (values (make-ast-if :cond cond-expr
                                 :then (make-ast-progn :forms then-stmts)
                                 :else else-ast)
                    rest5 kv5)))))))

(define-php-stmt-parser :while (rest known-vars)
  (multiple-value-bind (tok rest2) (php-expect :T-LPAREN rest)
    (declare (ignore tok))
    (multiple-value-bind (cond-expr rest3 kv3) (php-parse-expr rest2 known-vars)
      (multiple-value-bind (tok2 rest4) (php-expect :T-RPAREN rest3)
        (declare (ignore tok2))
        (multiple-value-bind (body-stmts rest5 kv5) (php-parse-block rest4 kv3)
          (values (php-lower-while cond-expr body-stmts) rest5 kv5))))))

(define-php-stmt-parser :for (rest known-vars)
  (multiple-value-bind (tok rest2) (php-expect :T-LPAREN rest)
    (declare (ignore tok))
    (multiple-value-bind (init rest3 kv3) (php-parse-expr rest2 known-vars)
      (multiple-value-bind (tok2 rest4) (php-expect :T-SEMI rest3)
        (declare (ignore tok2))
        (multiple-value-bind (cond-expr rest5 kv5) (php-parse-expr rest4 kv3)
          (multiple-value-bind (tok3 rest6) (php-expect :T-SEMI rest5)
            (declare (ignore tok3))
            (multiple-value-bind (incr rest7 kv7) (php-parse-expr rest6 kv5)
              (multiple-value-bind (tok4 rest8) (php-expect :T-RPAREN rest7)
                (declare (ignore tok4))
                (multiple-value-bind (body-stmts rest9 kv9) (php-parse-block rest8 kv7)
                  (values (make-ast-progn
                           :forms (list init
                                        (php-lower-while cond-expr
                                                         (append body-stmts (list incr)))))
                          rest9 kv9))))))))))

(define-php-stmt-parser :foreach (rest known-vars)
  (multiple-value-bind (tok rest2) (php-expect :T-LPAREN rest)
    (declare (ignore tok))
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
            (multiple-value-bind (tok2 rest7) (php-expect :T-RPAREN rest6)
              (declare (ignore tok2))
              (multiple-value-bind (body-stmts rest8 kv8) (php-parse-block rest7 kv3)
                (values (php-lower-foreach arr-expr var-sym body-stmts)
                        rest8 kv8)))))))))

(define-php-stmt-parser :function (rest known-vars)
  (multiple-value-bind (name-tok rest2) (php-expect :T-IDENT rest)
    (let ((fn-name (php-ident-sym (php-tok-value name-tok))))
      (multiple-value-bind (params rest3) (php-parse-param-list rest2)
        ;; Skip optional return type annotation
        (let ((rest4 (%php-skip-return-type rest3)))
          (multiple-value-bind (body-stmts rest5 _) (php-parse-block rest4 (append params known-vars))
            (declare (ignore _))
            (values (make-ast-defun :name fn-name :params params :body body-stmts)
                    rest5 known-vars)))))))

(defun %php-skip-return-type (stream)
  "Consume an optional : type annotation after a function parameter list.
Returns the stream positioned after the return type (or unchanged if absent)."
  (if (and stream (eq (php-peek-type stream) :T-COLON))
      (%php-skip-type-annotation (cdr stream))
      stream))

;;; ─── Class Body Parser ───────────────────────────────────────────────────────

(defun %php-skip-visibility-modifiers (stream)
  "Consume zero or more visibility/modifier keywords (public, private, etc.)."
  (loop while (and stream
                   (eq (php-peek-type stream) :T-KEYWORD)
                   (member (php-peek-value stream)
                           '(:public :private :protected :static
                             :abstract :final :readonly)))
        do (setf stream (cdr stream)))
  stream)

(defun %php-parse-class-body-member (stream known-vars)
  "Parse one class body member: a property declaration or a method definition.
Returns (values slot-def-or-nil remaining-stream)."
  (let ((stream (php-skip-semis (%php-skip-visibility-modifiers stream))))
    (cond
      ;; $var [= default];  — direct property
      ((eq (php-peek-type stream) :T-VAR)
       (multiple-value-bind (var-tok rest) (php-consume stream)
         (let ((slot (make-ast-slot-def :name (php-var-sym (php-tok-value var-tok)))))
           ;; Skip optional default value
           (when (and rest (eq (php-peek-type rest) :T-OP)
                      (equal "=" (php-peek-value rest)))
             (setf rest (cdr rest))
             (loop while (and rest (not (eq (php-peek-type rest) :T-SEMI)))
                   do (setf rest (cdr rest))))
           (values slot (php-skip-semis rest)))))
      ;; type $var;  — typed property
      ((or (eq (php-peek-type stream) :T-TYPE)
           (eq (php-peek-type stream) :T-NULLABLE))
       (let ((rest (%php-skip-type-annotation stream)))
         (if (eq (php-peek-type rest) :T-VAR)
             (multiple-value-bind (var-tok rest2) (php-consume rest)
               (values (make-ast-slot-def :name (php-var-sym (php-tok-value var-tok)))
                       (php-skip-semis rest2)))
             (values nil rest))))
      ;; function name(...) { }  — method
      ((and (eq (php-peek-type stream) :T-KEYWORD)
            (eq (php-peek-value stream) :function))
       (multiple-value-bind (method-ast rest _) (php-parse-statement stream known-vars)
         (declare (ignore _))
         (values (when method-ast
                   (make-ast-slot-def :name (ast-defun-name method-ast)))
                 rest)))
      ;; Unknown token in class body — skip it
      (t (values nil (cdr stream))))))

(defun %php-parse-class-superclasses (stream)
  "Consume optional 'extends ClassName' and 'implements A, B, ...' clauses.
Returns (values superclass-list remaining-stream)."
  (let ((supers nil) (current stream))
    (when (and current (eq (php-peek-type current) :T-KEYWORD)
               (eq (php-peek-value current) :extends))
      (setf current (cdr current))
      (multiple-value-bind (super-tok rest) (php-expect :T-IDENT current)
        (push (php-ident-sym (php-tok-value super-tok)) supers)
        (setf current rest)))
    (when (and current (eq (php-peek-type current) :T-KEYWORD)
               (eq (php-peek-value current) :implements))
      (setf current (cdr current))
      (loop
        (multiple-value-bind (tok rest) (php-expect :T-IDENT current)
          (declare (ignore tok))
          (setf current rest))
        (unless (and current (eq (php-peek-type current) :T-COMMA))
          (return))
        (setf current (cdr current))))
    (values (nreverse supers) current)))

(define-php-stmt-parser :class (rest known-vars)
  (multiple-value-bind (name-tok rest2) (php-expect :T-IDENT rest)
    (let ((class-name (php-ident-sym (php-tok-value name-tok))))
      (multiple-value-bind (supers rest3) (%php-parse-class-superclasses rest2)
        (multiple-value-bind (tok rest4) (php-expect :T-LBRACE rest3)
          (declare (ignore tok))
          (let ((slots nil) (current rest4))
            (loop
              (setf current (php-skip-semis current))
              (when (or (php-at-eof-p current) (eq (php-peek-type current) :T-RBRACE))
                (return))
              (multiple-value-bind (slot rest5) (%php-parse-class-body-member current known-vars)
                (when slot (push slot slots))
                (setf current rest5)))
            (multiple-value-bind (tok2 rest5) (php-expect :T-RBRACE current)
              (declare (ignore tok2))
              (values (make-ast-defclass :name class-name
                                         :superclasses supers
                                         :slots (nreverse slots))
                      rest5 known-vars))))))))

;;; ─── Statement Dispatcher ────────────────────────────────────────────────────

(defun php-parse-statement (stream known-vars)
  "Parse a single PHP statement. Returns (values ast rest known-vars).
Dispatches keyword statements through *php-stmt-parsers*; falls through
to php-parse-expr for expression statements."
  (if (eq (php-peek-type stream) :T-KEYWORD)
      (let ((handler (gethash (php-peek-value stream) *php-stmt-parsers*)))
        (if handler
            (multiple-value-bind (tok rest) (php-consume stream)
              (declare (ignore tok))
              (funcall handler rest known-vars))
            ;; Unrecognized keyword — treat as expression statement
            (multiple-value-bind (expr rest kv) (php-parse-expr stream known-vars)
              (values expr (php-skip-semis rest) kv))))
      ;; Not a keyword — expression statement (assignment, call, etc.)
      (multiple-value-bind (expr rest kv) (php-parse-expr stream known-vars)
        (values expr (php-skip-semis rest) kv))))

;;; ─── Top-Level Entry Point ───────────────────────────────────────────────────

(defun php-finish-let-bindings (stmts)
  "Post-process: wrap consecutive ast-let :body nil nodes into proper nested lets.
Iterative CPS-style accumulator — walks backwards, building nesting from the end."
  (when (null stmts) (return-from php-finish-let-bindings nil))
  (let ((tail nil))
    (dolist (stmt (reverse stmts) tail)
      (setf tail
            (if (and (ast-let-p stmt) (null (ast-let-body stmt)))
                (list (make-ast-let :bindings (ast-let-bindings stmt) :body tail))
                (cons stmt tail))))))

(defun parse-php-source (source)
  "Parse PHP SOURCE string and return a list of top-level AST nodes.
Analogous to parse-all-forms for CL."
  (let ((stream (tokenize-php-source source))
        (stmts nil) (kv nil))
    (loop
      (setf stream (php-skip-semis stream))
      (when (php-at-eof-p stream) (return))
      (multiple-value-bind (stmt rest2 kv2) (php-parse-statement stream kv)
        (when stmt (push stmt stmts))
        (setf stream rest2 kv kv2)))
    (php-finish-let-bindings (nreverse stmts))))
