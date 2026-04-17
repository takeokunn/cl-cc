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

(in-package :cl-cc/parse)

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

;;; Expression parser (php-parse-primary, php-parse-new, php-parse-postfix,
;;; php-parse-unary, php-parse-binop, php-parse-mul/add/cmp/and/or,
;;; php-parse-expr, php-parse-arglist) is in parser-expr.lisp (loads next).
;;; Statement parser and top-level entry point are in parser-stmt.lisp.
