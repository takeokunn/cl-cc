;;;; frontend/php/parser.lisp - PHP 8.x Parser
;;;;
;;;; Parses PHP source code into the common AST (same nodes as CL frontend).
;;;; Uses def-grammar-rule for declarative grammar + recursive descent
;;;; for statement-level parsing.
;;;;
;;;; Key AST mappings:
;;;;   $var = expr     → ast-let (first) / ast-setq (reassignment)
;;;;   if ($c) { }     → ast-if
;;;;   while ($c) { }  → ast-call with :while or ast-progn
;;;;   function f() {} → ast-defun
;;;;   class Foo {}    → ast-defclass
;;;;   echo expr       → ast-print
;;;;   return expr     → ast-return-from
;;;;   new Foo()       → ast-make-instance
;;;;   $o->prop        → ast-slot-value
;;;;   $o->m(args)     → ast-call
;;;;   $o?->m(args)    → ast-if (null check) + ast-call

(in-package :cl-cc)

;;; ─── Grammar Rules (expression-level) ──────────────────────────────────────

(def-grammar-rule :php-literal
  (alt (token :T-INT) (token :T-FLOAT) (token :T-STRING)))

(def-grammar-rule :php-atom
  (alt (token :T-INT)
       (token :T-FLOAT)
       (token :T-STRING)
       (token :T-VAR)
       (token :T-IDENT)))

(def-grammar-rule :php-add-op
  (alt (token :T-OP "+") (token :T-OP "-") (token :T-OP ".")))

(def-grammar-rule :php-mul-op
  (alt (token :T-OP "*") (token :T-OP "/")))

(def-grammar-rule :php-cmp-op
  (alt (token :T-OP "==") (token :T-OP "===") (token :T-OP "!=")
       (token :T-OP "!==") (token :T-OP "<") (token :T-OP ">")
       (token :T-OP "<=") (token :T-OP ">=")))

;;; ─── Loop Lowering Helpers ──────────────────────────────────────────────────
;;;
;;; PHP loops are lowered directly to block/tagbody/go AST rather than
;;; emitting (ast-call 'while ...) — the compiler skips macro expansion
;;; for pre-built PHP AST nodes, so macro names would be unresolvable at
;;; compile time.

(defun php-lower-while (cond-expr body-stmts)
  "Lower a PHP while(cond){body} to CL block/tagbody/go loop AST.
   Equivalent to: (block nil (tagbody LOOP (unless cond (return nil)) body... (go LOOP)))"
  (let ((loop-tag (gensym "WHILE-LOOP-")))
    (make-ast-block :name nil
      :body (list (make-ast-tagbody
                   :tags (list* loop-tag
                                ;; Unless condition is true, return nil
                                (make-ast-if
                                 :cond cond-expr
                                 :then (make-ast-quote :value nil)
                                 :else (make-ast-return-from :name nil
                                                            :value (make-ast-quote :value nil)))
                                (append body-stmts
                                        (list (make-ast-go :tag loop-tag)))))))))

(defun php-lower-foreach (arr-expr var-sym body-stmts)
  "Lower a PHP foreach($arr as $v){body} to a while-loop over the list.
   Equivalent to: (let ((#:list arr)) (while #:list (let ((v (car #:list))) body (setq #:list (cdr #:list)))))"
  (let ((list-sym (gensym "FOREACH-LIST-")))
    (make-ast-let
     :bindings (list (cons list-sym arr-expr))
     :body (list (php-lower-while
                  (make-ast-var :name list-sym)
                  (list (make-ast-let
                         :bindings (list (cons var-sym
                                              (make-ast-call
                                               :func (make-ast-var :name 'car)
                                               :args (list (make-ast-var :name list-sym)))))
                         :body (append body-stmts
                                       (list (make-ast-setq
                                              :var list-sym
                                              :value (make-ast-call
                                                      :func (make-ast-var :name 'cdr)
                                                      :args (list (make-ast-var :name list-sym)))))))))))))

;;; ─── Token Stream Helpers ───────────────────────────────────────────────────

(defun php-tok-type  (tok) (getf tok :type))
(defun php-tok-value (tok) (getf tok :value))

(defun php-peek (stream) (car stream))
(defun php-peek-type (stream) (when stream (php-tok-type (car stream))))
(defun php-peek-value (stream) (when stream (php-tok-value (car stream))))

(defun php-consume (stream)
  (values (car stream) (cdr stream)))

(defun php-expect (type stream &optional value)
  "Consume a token of TYPE (optionally matching VALUE). Signals error if mismatch."
  (if (and stream (eq (php-peek-type stream) type)
           (or (null value) (equal (php-peek-value stream) value)))
      (php-consume stream)
      (error "PHP parse error: expected ~S~@[ ~S~] but got ~S"
             type value (php-peek stream))))

(defun php-at-eof-p (stream)
  (or (null stream) (eq (php-peek-type stream) :T-EOF)))

(defun php-skip-semis (stream)
  "Skip zero or more semicolons."
  (loop while (and stream (eq (php-peek-type stream) :T-SEMI))
        do (setf stream (cdr stream)))
  stream)

;;; ─── Variable Name Normalization ────────────────────────────────────────────

(defun php-var-sym (tok-value)
  "Convert a PHP variable token value to a CL symbol (strips $ conceptually)."
  (if (symbolp tok-value)
      tok-value
      (intern (string-upcase tok-value))))

(defun php-ident-sym (str)
  "Convert PHP identifier string to a CL symbol."
  (intern (string-upcase (if (stringp str) str (symbol-name str)))))

;;; ─── Expression Parser ──────────────────────────────────────────────────────

(defun php-parse-primary (stream known-vars)
  "Parse a primary expression (literal, variable, identifier, parenthesized)."
  (let ((type (php-peek-type stream)))
    (cond
      ((eq type :T-INT)
       (multiple-value-bind (tok rest) (php-consume stream)
         (values (make-ast-int :value (php-tok-value tok)) rest known-vars)))
      ((eq type :T-FLOAT)
       (multiple-value-bind (tok rest) (php-consume stream)
         (values (make-ast-quote :value (php-tok-value tok)) rest known-vars)))
      ((eq type :T-STRING)
       (multiple-value-bind (tok rest) (php-consume stream)
         (values (make-ast-quote :value (php-tok-value tok)) rest known-vars)))
      ((eq type :T-VAR)
       (multiple-value-bind (tok rest) (php-consume stream)
         (values (make-ast-var :name (php-var-sym (php-tok-value tok))) rest known-vars)))
      ((eq type :T-KEYWORD)
       (let ((kw (php-peek-value stream)))
         (cond
           ((eq kw :null)
            (multiple-value-bind (tok rest) (php-consume stream)
              (declare (ignore tok))
              (values (make-ast-quote :value nil) rest known-vars)))
           ((eq kw :true)
            (multiple-value-bind (tok rest) (php-consume stream)
              (declare (ignore tok))
              (values (make-ast-quote :value t) rest known-vars)))
           ((eq kw :false)
            (multiple-value-bind (tok rest) (php-consume stream)
              (declare (ignore tok))
              (values (make-ast-quote :value nil) rest known-vars)))
           ((eq kw :new)
            (php-parse-new stream known-vars))
           (t (error "PHP parse error: unexpected keyword ~S in expression" kw)))))
      ((eq type :T-LPAREN)
       (multiple-value-bind (tok rest) (php-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
           (multiple-value-bind (tok2 rest3) (php-expect :T-RPAREN rest2)
             (declare (ignore tok2))
             (values expr rest3 kv2)))))
      ((eq type :T-IDENT)
       ;; Could be a function call or identifier
       (multiple-value-bind (tok rest) (php-consume stream)
         (let ((name (php-ident-sym (php-tok-value tok))))
           (if (eq (php-peek-type rest) :T-LPAREN)
               ;; Function call
               (multiple-value-bind (args rest2 kv2) (php-parse-arglist rest known-vars)
                 (values (make-ast-call :func (make-ast-var :name name) :args args)
                         rest2 kv2))
               (values (make-ast-var :name name) rest known-vars)))))
      (t (error "PHP parse error: unexpected token ~S in expression" (php-peek stream))))))

(defun php-parse-new (stream known-vars)
  "Parse 'new ClassName(args)'."
  (multiple-value-bind (tok rest) (php-consume stream) ; consume 'new'
    (declare (ignore tok))
    (multiple-value-bind (class-tok rest2) (php-expect :T-IDENT rest)
      (let ((class-name (php-ident-sym (php-tok-value class-tok))))
        (multiple-value-bind (args rest3 kv3) (php-parse-arglist rest2 known-vars)
          (values (make-ast-make-instance
                   :class (make-ast-var :name class-name)
                   :initargs (loop for i from 0 for a in args
                                   collect (cons (intern (format nil "ARG~D" i) :keyword) a)))
                  rest3 kv3))))))

(defun php-parse-postfix (stream known-vars)
  "Parse postfix expressions: method calls, property access, array access."
  (multiple-value-bind (obj rest kv) (php-parse-primary stream known-vars)
    (loop
      (let ((type (php-peek-type rest)))
        (cond
          ;; -> method call or property
          ((eq type :T-ARROW)
           (multiple-value-bind (tok rest2) (php-consume rest)
             (declare (ignore tok))
             (multiple-value-bind (name-tok rest3) (php-expect :T-IDENT rest2)
               (let ((prop (php-ident-sym (php-tok-value name-tok))))
                 (if (eq (php-peek-type rest3) :T-LPAREN)
                     (multiple-value-bind (args rest4 kv4) (php-parse-arglist rest3 kv)
                       (setf obj (make-ast-call
                                  :func (make-ast-slot-value :object obj :slot prop)
                                  :args args)
                             rest rest4
                             kv kv4))
                     (setf obj (make-ast-slot-value :object obj :slot prop)
                           rest rest3))))))
          ;; ?-> nullsafe
          ((eq type :T-NULLSAFE-ARROW)
           (multiple-value-bind (tok rest2) (php-consume rest)
             (declare (ignore tok))
             (multiple-value-bind (name-tok rest3) (php-expect :T-IDENT rest2)
               (let* ((prop (php-ident-sym (php-tok-value name-tok)))
                      (null-check (make-ast-binop :op '= :lhs obj
                                                  :rhs (make-ast-quote :value nil))))
                 (if (eq (php-peek-type rest3) :T-LPAREN)
                     (multiple-value-bind (args rest4 kv4) (php-parse-arglist rest3 kv)
                       (setf obj (make-ast-if
                                  :cond null-check
                                  :then (make-ast-quote :value nil)
                                  :else (make-ast-call
                                         :func (make-ast-slot-value :object obj :slot prop)
                                         :args args))
                             rest rest4
                             kv kv4))
                     (setf obj (make-ast-if
                                :cond null-check
                                :then (make-ast-quote :value nil)
                                :else (make-ast-slot-value :object obj :slot prop))
                           rest rest3))))))
          ;; ++ postfix (ignore for now - just consume)
          ((and (eq type :T-OP) (equal "++" (php-peek-value rest)))
           (multiple-value-bind (tok rest2) (php-consume rest)
             (declare (ignore tok))
             (setf rest rest2)))
          ((and (eq type :T-OP) (equal "--" (php-peek-value rest)))
           (multiple-value-bind (tok rest2) (php-consume rest)
             (declare (ignore tok))
             (setf rest rest2)))
          (t (return))))
    (values obj rest kv))))

(defun php-parse-unary (stream known-vars)
  "Parse unary expressions: !, -, +."
  (if (and (eq (php-peek-type stream) :T-OP)
           (member (php-peek-value stream) '("!" "-" "+") :test #'equal))
      (multiple-value-bind (tok rest) (php-consume stream)
        (multiple-value-bind (expr rest2 kv2) (php-parse-postfix rest known-vars)
          (values (make-ast-call :func (make-ast-var :name (intern (php-tok-value tok)))
                                 :args (list expr))
                  rest2 kv2)))
      (php-parse-postfix stream known-vars)))

(defun php-parse-binop (stream known-vars ops next-parser)
  "Left-associative binary operator parsing."
  (multiple-value-bind (lhs rest kv) (funcall next-parser stream known-vars)
    (loop
      (if (and (eq (php-peek-type rest) :T-OP)
               (member (php-peek-value rest) ops :test #'equal))
          (multiple-value-bind (op-tok rest2) (php-consume rest)
            (multiple-value-bind (rhs rest3 kv3) (funcall next-parser rest2 kv)
              (setf lhs (make-ast-binop
                         :op (intern (php-tok-value op-tok))
                         :lhs lhs
                         :rhs rhs)
                    rest rest3
                    kv kv3)))
          (return)))
    (values lhs rest kv)))

(defun php-parse-mul (stream known-vars)
  (php-parse-binop stream known-vars '("*" "/") #'php-parse-unary))

(defun php-parse-add (stream known-vars)
  (php-parse-binop stream known-vars '("+" "-" ".") #'php-parse-mul))

(defun php-parse-cmp (stream known-vars)
  (php-parse-binop stream known-vars '("==" "===" "!=" "!==" "<" ">" "<=" ">=")
                   #'php-parse-add))

(defun php-parse-and (stream known-vars)
  (php-parse-binop stream known-vars '("&&") #'php-parse-cmp))

(defun php-parse-or (stream known-vars)
  (php-parse-binop stream known-vars '("||") #'php-parse-and))

(defun php-parse-expr (stream known-vars)
  "Parse an expression. Handles assignment $var = expr."
  (if (and (eq (php-peek-type stream) :T-VAR)
           (let ((rest (cdr stream)))
             (and rest (eq (php-peek-type rest) :T-OP)
                  (equal "=" (php-peek-value rest)))))
      ;; Assignment: $var = expr
      (multiple-value-bind (var-tok rest) (php-consume stream)
        (multiple-value-bind (op-tok rest2) (php-consume rest)
          (declare (ignore op-tok))
          (multiple-value-bind (val rest3 kv3) (php-parse-or rest2 known-vars)
            (let* ((var-sym (php-var-sym (php-tok-value var-tok)))
                   (already-known (member var-sym known-vars))
                   (new-kv (if already-known kv3 (cons var-sym kv3))))
              (values
               (if already-known
                   (make-ast-setq :var var-sym :value val)
                   (make-ast-let :bindings (list (cons var-sym val)) :body nil))
               rest3
               new-kv)))))
      (php-parse-or stream known-vars)))

(defun php-parse-arglist (stream known-vars)
  "Parse (arg1, arg2, ...). Assumes stream starts with T-LPAREN."
  (multiple-value-bind (tok rest) (php-expect :T-LPAREN stream)
    (declare (ignore tok))
    (if (eq (php-peek-type rest) :T-RPAREN)
        (multiple-value-bind (tok2 rest2) (php-consume rest)
          (declare (ignore tok2))
          (values nil rest2 known-vars))
        (let ((args nil)
              (current rest)
              (kv known-vars))
          (loop
            (multiple-value-bind (arg rest2 kv2) (php-parse-expr current kv)
              (push arg args)
              (setf current rest2 kv kv2))
            (if (and current (eq (php-peek-type current) :T-COMMA))
                (setf current (cdr current))
                (return)))
          (multiple-value-bind (tok2 rest2) (php-expect :T-RPAREN current)
            (declare (ignore tok2))
            (values (nreverse args) rest2 kv))))))

;;; ─── Statement Parser ───────────────────────────────────────────────────────

(defun php-parse-block (stream known-vars)
  "Parse { stmt* }. Returns (values stmt-list remaining-stream known-vars)."
  (multiple-value-bind (tok rest) (php-expect :T-LBRACE stream)
    (declare (ignore tok))
    (let ((stmts nil)
          (current rest)
          (kv known-vars))
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

(defun php-parse-param-list (stream)
  "Parse (type? $param, ...) for function/method definitions."
  (multiple-value-bind (tok rest) (php-expect :T-LPAREN stream)
    (declare (ignore tok))
    (if (eq (php-peek-type rest) :T-RPAREN)
        (multiple-value-bind (tok2 rest2) (php-consume rest)
          (declare (ignore tok2))
          (values nil rest2))
        (let ((params nil)
              (current rest))
          (loop
            ;; Skip optional type annotation
            (when (or (eq (php-peek-type current) :T-TYPE)
                      (eq (php-peek-type current) :T-IDENT)
                      (eq (php-peek-type current) :T-NULLABLE))
              (setf current (cdr current))
              ;; If nullable ?, skip type too
              (when (or (eq (php-peek-type current) :T-TYPE)
                        (eq (php-peek-type current) :T-IDENT))
                (setf current (cdr current))))
            ;; Expect $var
            (multiple-value-bind (var-tok rest2) (php-expect :T-VAR current)
              (push (php-var-sym (php-tok-value var-tok)) params)
              (setf current rest2))
            ;; Skip default value if present
            (when (and current (eq (php-peek-type current) :T-OP)
                       (equal "=" (php-peek-value current)))
              (setf current (cdr current))
              ;; consume default expression (simplified: consume until , or ))
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

(defun php-parse-statement (stream known-vars)
  "Parse a single PHP statement. Returns (values ast rest known-vars)."
  (let ((type  (php-peek-type  stream))
        (value (php-peek-value stream)))
    (cond
      ;; echo expr;
      ((and (eq type :T-KEYWORD) (eq value :echo))
       (multiple-value-bind (tok rest) (php-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
           (let ((rest3 (php-skip-semis rest2)))
             (values (make-ast-print :expr expr) rest3 kv2)))))

      ;; return expr;
      ((and (eq type :T-KEYWORD) (eq value :return))
       (multiple-value-bind (tok rest) (php-consume stream)
         (declare (ignore tok))
         (if (eq (php-peek-type rest) :T-SEMI)
             (values (make-ast-return-from :name nil :value (make-ast-quote :value nil))
                     (php-skip-semis rest) known-vars)
             (multiple-value-bind (expr rest2 kv2) (php-parse-expr rest known-vars)
               (values (make-ast-return-from :name nil :value expr)
                       (php-skip-semis rest2) kv2)))))

      ;; if ($cond) { } [else { }]
      ((and (eq type :T-KEYWORD) (eq value :if))
       (multiple-value-bind (tok rest) (php-consume stream) ; consume 'if'
         (declare (ignore tok))
         (multiple-value-bind (cond-tok rest2) (php-expect :T-LPAREN rest)
           (declare (ignore cond-tok))
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
                   (values (make-ast-if
                            :cond cond-expr
                            :then (make-ast-progn :forms then-stmts)
                            :else else-ast)
                           rest5 kv5))))))))

      ;; while ($cond) { }
      ((and (eq type :T-KEYWORD) (eq value :while))
       (multiple-value-bind (tok rest) (php-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (tok2 rest2) (php-expect :T-LPAREN rest)
           (declare (ignore tok2))
           (multiple-value-bind (cond-expr rest3 kv3) (php-parse-expr rest2 known-vars)
             (multiple-value-bind (tok3 rest4) (php-expect :T-RPAREN rest3)
               (declare (ignore tok3))
               (multiple-value-bind (body-stmts rest5 kv5) (php-parse-block rest4 kv3)
                 (values (php-lower-while cond-expr body-stmts) rest5 kv5)))))))

      ;; for ($init; $cond; $incr) { }
      ((and (eq type :T-KEYWORD) (eq value :for))
       (multiple-value-bind (tok rest) (php-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (tok2 rest2) (php-expect :T-LPAREN rest)
           (declare (ignore tok2))
           (multiple-value-bind (init rest3 kv3) (php-parse-expr rest2 known-vars)
             (multiple-value-bind (tok3 rest4) (php-expect :T-SEMI rest3)
               (declare (ignore tok3))
               (multiple-value-bind (cond-expr rest5 kv5) (php-parse-expr rest4 kv3)
                 (multiple-value-bind (tok4 rest6) (php-expect :T-SEMI rest5)
                   (declare (ignore tok4))
                   (multiple-value-bind (incr rest7 kv7) (php-parse-expr rest6 kv5)
                     (multiple-value-bind (tok5 rest8) (php-expect :T-RPAREN rest7)
                       (declare (ignore tok5))
                       (multiple-value-bind (body-stmts rest9 kv9) (php-parse-block rest8 kv7)
                         (values (make-ast-progn
                                  :forms (list init
                                               (php-lower-while
                                                cond-expr
                                                (append body-stmts (list incr)))))
                                 rest9 kv9)))))))))))

      ;; foreach ($arr as [$k =>] $v) { }
      ((and (eq type :T-KEYWORD) (eq value :foreach))
       (multiple-value-bind (tok rest) (php-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (tok2 rest2) (php-expect :T-LPAREN rest)
           (declare (ignore tok2))
           (multiple-value-bind (arr-expr rest3 kv3) (php-parse-expr rest2 known-vars)
             ;; expect 'as'
             (let ((rest4 (if (and rest3 (eq (php-peek-type rest3) :T-KEYWORD)
                                   (eq (php-peek-value rest3) :as))
                              (cdr rest3)
                              (error "foreach: expected 'as'"))))
               (multiple-value-bind (var-tok rest5) (php-expect :T-VAR rest4)
                 (let ((var-sym (php-var-sym (php-tok-value var-tok)))
                       (rest6 rest5))
                   ;; Skip optional => $val (key => value form; we use the value var)
                   (when (and rest6 (eq (php-peek-type rest6) :T-OP)
                              (equal "=>" (php-peek-value rest6)))
                     (setf rest6 (cdr rest6))
                     (multiple-value-bind (val-tok rest7) (php-expect :T-VAR rest6)
                       (setf var-sym (php-var-sym (php-tok-value val-tok)))
                       (setf rest6 rest7)))
                   (multiple-value-bind (tok3 rest7) (php-expect :T-RPAREN rest6)
                     (declare (ignore tok3))
                     (multiple-value-bind (body-stmts rest8 kv8) (php-parse-block rest7 kv3)
                       (values (php-lower-foreach arr-expr var-sym body-stmts)
                               rest8 kv8))))))))))

      ;; function name($params) { }
      ((and (eq type :T-KEYWORD) (eq value :function))
       (multiple-value-bind (tok rest) (php-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (name-tok rest2) (php-expect :T-IDENT rest)
           (let ((fn-name (php-ident-sym (php-tok-value name-tok))))
             (multiple-value-bind (params rest3) (php-parse-param-list rest2)
               ;; Skip optional return type
               (let ((rest4 rest3))
                 (when (and rest4 (eq (php-peek-type rest4) :T-COLON))
                   (setf rest4 (cdr rest4))
                   (when (or (eq (php-peek-type rest4) :T-TYPE)
                             (eq (php-peek-type rest4) :T-IDENT)
                             (eq (php-peek-type rest4) :T-NULLABLE))
                     (setf rest4 (cdr rest4))
                     (when (or (eq (php-peek-type rest4) :T-TYPE)
                               (eq (php-peek-type rest4) :T-IDENT))
                       (setf rest4 (cdr rest4)))))
                 (let ((fn-kv (append params known-vars)))
                   (multiple-value-bind (body-stmts rest5 _) (php-parse-block rest4 fn-kv)
                     (declare (ignore _))
                     (values (make-ast-defun :name fn-name :params params :body body-stmts)
                             rest5 known-vars)))))))))

      ;; class Foo [extends Bar] [implements A, B] { }
      ((and (eq type :T-KEYWORD) (eq value :class))
       (multiple-value-bind (tok rest) (php-consume stream)
         (declare (ignore tok))
         (multiple-value-bind (name-tok rest2) (php-expect :T-IDENT rest)
           (let ((class-name (php-ident-sym (php-tok-value name-tok)))
                 (supers nil)
                 (current rest2))
             ;; extends
             (when (and current (eq (php-peek-type current) :T-KEYWORD)
                        (eq (php-peek-value current) :extends))
               (setf current (cdr current))
               (multiple-value-bind (super-tok rest3) (php-expect :T-IDENT current)
                 (push (php-ident-sym (php-tok-value super-tok)) supers)
                 (setf current rest3)))
             ;; implements (skip interface list)
             (when (and current (eq (php-peek-type current) :T-KEYWORD)
                        (eq (php-peek-value current) :implements))
               (setf current (cdr current))
               (loop
                 (multiple-value-bind (tok2 rest3) (php-expect :T-IDENT current)
                   (declare (ignore tok2))
                   (setf current rest3))
                 (unless (and current (eq (php-peek-type current) :T-COMMA))
                   (return))
                 (setf current (cdr current))))
             ;; class body { ... }
             (multiple-value-bind (tok2 rest3) (php-expect :T-LBRACE current)
               (declare (ignore tok2))
               (let ((slots nil))
                 ;; Parse class body: collect property declarations and methods
                 (loop
                   (setf rest3 (php-skip-semis rest3))
                   (when (or (php-at-eof-p rest3) (eq (php-peek-type rest3) :T-RBRACE))
                     (return))
                   ;; Skip visibility modifiers
                   (loop while (and rest3
                                    (eq (php-peek-type rest3) :T-KEYWORD)
                                    (member (php-peek-value rest3)
                                            '(:public :private :protected :static
                                              :abstract :final :readonly)))
                         do (setf rest3 (cdr rest3)))
                   (cond
                     ;; Property: type? $var [= default];
                     ((eq (php-peek-type rest3) :T-VAR)
                      (multiple-value-bind (var-tok rest4) (php-consume rest3)
                        (push (make-ast-slot-def :name (php-var-sym (php-tok-value var-tok)))
                              slots)
                        (setf rest3 rest4)
                        ;; Skip optional default
                        (when (and rest3 (eq (php-peek-type rest3) :T-OP)
                                   (equal "=" (php-peek-value rest3)))
                          (setf rest3 (cdr rest3))
                          (loop while (and rest3
                                           (not (eq (php-peek-type rest3) :T-SEMI)))
                                do (setf rest3 (cdr rest3))))
                        (setf rest3 (php-skip-semis rest3))))
                     ;; Typed property: type $var
                     ((or (eq (php-peek-type rest3) :T-TYPE)
                          (eq (php-peek-type rest3) :T-NULLABLE))
                      (setf rest3 (cdr rest3))
                      (when (or (eq (php-peek-type rest3) :T-TYPE)
                                (eq (php-peek-type rest3) :T-IDENT))
                        (setf rest3 (cdr rest3)))
                      (when (eq (php-peek-type rest3) :T-VAR)
                        (multiple-value-bind (var-tok rest4) (php-consume rest3)
                          (push (make-ast-slot-def :name (php-var-sym (php-tok-value var-tok)))
                                slots)
                          (setf rest3 rest4)
                          (setf rest3 (php-skip-semis rest3)))))
                     ;; Method: function name(...) { }
                     ((and (eq (php-peek-type rest3) :T-KEYWORD)
                           (eq (php-peek-value rest3) :function))
                      (multiple-value-bind (method-ast rest4 _)
                          (php-parse-statement rest3 known-vars)
                        (declare (ignore _))
                        ;; Methods become slots with function values (simplified)
                        (when method-ast
                          (push (make-ast-slot-def :name (ast-defun-name method-ast)) slots))
                        (setf rest3 rest4)))
                     (t ; skip unknown tokens in class body
                      (setf rest3 (cdr rest3)))))
                 (multiple-value-bind (tok3 rest4) (php-expect :T-RBRACE rest3)
                   (declare (ignore tok3))
                   (values (make-ast-defclass :name class-name
                                              :superclasses (nreverse supers)
                                              :slots (nreverse slots))
                           rest4 known-vars))))))))

      ;; Expression statement (assignment, function call, etc.)
      (t
       (multiple-value-bind (expr rest kv) (php-parse-expr stream known-vars)
         (values expr (php-skip-semis rest) kv))))))

;;; ─── Top-Level Entry Point ──────────────────────────────────────────────────

(defun php-finish-let-bindings (stmts)
  "Post-process: wrap consecutive ast-let :body nil nodes into proper nested lets.
   (ast-let :bindings (...) :body nil) means 'declare var in scope for rest of stmts'.
   Iterative implementation to avoid stack overflow on large flat statement lists."
  (when (null stmts) (return-from php-finish-let-bindings nil))
  ;; Walk backwards through stmts, building up the nesting from the end.
  ;; We use a continuation-passing accumulator: 'tail' is the already-processed suffix.
  (let ((tail nil))
    (dolist (stmt (reverse stmts) tail)
      (setf tail
            (if (and (ast-let-p stmt) (null (ast-let-body stmt)))
                (list (make-ast-let :bindings (ast-let-bindings stmt) :body tail))
                (cons stmt tail))))))

(defun parse-php-source (source)
  "Parse PHP SOURCE string and return a list of top-level AST nodes.
   Analogous to parse-all-forms for CL."
  (let* ((tokens  (tokenize-php-source source))
         (stream  tokens)
         (stmts   nil)
         (kv      nil))
    (loop
      (setf stream (php-skip-semis stream))
      (when (php-at-eof-p stream) (return))
      (multiple-value-bind (stmt rest2 kv2) (php-parse-statement stream kv)
        (when stmt (push stmt stmts))
        (setf stream rest2 kv kv2)))
    (php-finish-let-bindings (nreverse stmts))))
