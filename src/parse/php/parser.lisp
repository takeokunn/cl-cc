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
          (t (return)))))
    (values obj rest kv)))

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

;;; Statement parser, loop lowering helpers, and top-level entry point
;;; are in parser-stmt.lisp (loads after this file).
