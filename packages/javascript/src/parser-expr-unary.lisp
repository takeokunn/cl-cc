;;;; packages/javascript/src/parser-expr-unary.lisp — Template literals and unary operators
;;;;
;;;; Contains: %js-template-parts-and-rest, %js-parse-tagged-template,
;;;; %js-parse-template-literal, *js-unary-op-builders*, *js-unary-kw-builders*,
;;;; js-parse-unary.
;;;;
;;;; Load order: after parser-expr-postfix.lisp (needs js-parse-postfix,
;;;; %js-place-get-prop-p, %js-lower-place-incdec).
;;;; js-parse-primary is forward-referenced (defined in parser-expr-primary.lisp).

(in-package :cl-cc/javascript)

;;; ─── Template Literal ────────────────────────────────────────────────────────

(defun %js-template-parts-and-rest (stream)
  "Return (values PARTS rest) for a template literal at STREAM."
  (let ((tok (js-peek stream)))
    (case (js-tok-type tok)
      (:T-STRING
       (multiple-value-bind (str-tok rest) (js-consume stream)
         (values (list (js-tok-value str-tok)) rest)))
      (:T-TEMPLATE-PARTS
       (multiple-value-bind (parts-tok rest) (js-consume stream)
         (values (js-tok-value parts-tok) rest)))
      (:T-TEMPLATE-START
       (multiple-value-bind (tok2 rest) (js-consume stream)
         (declare (ignore tok2))
         (%js-template-parts-and-rest rest)))
      (t (error "JS parse error: expected template literal, got ~S" tok)))))

(defun %js-parse-tagged-template (tag-ast stream)
  "Lower a tagged template tag`...` to (call TAG strings-array value1 value2 ...),
per the TC39 tagged-template protocol."
  (multiple-value-bind (parts rest) (%js-template-parts-and-rest stream)
    (let ((cooked nil) (values nil) (current ""))
      (dolist (part parts)
        (cond
          ((stringp part)
           (setf current (concatenate 'string current part)))
          ((and (consp part) (eq (car part) :template-expr))
           (push (make-ast-quote :value current) cooked)
           (setf current "")
           (multiple-value-bind (expr _r) (js-parse-assignment-expr (cadr part))
             (declare (ignore _r))
             (push expr values)))))
      (push (make-ast-quote :value current) cooked)
      (values (make-ast-call
               :func tag-ast
               :args (cons (apply #'%js-call '%js-make-array (nreverse cooked))
                           (nreverse values)))
              rest))))

(defun %js-parse-template-literal (stream)
  "Parse a template literal starting at :T-TEMPLATE-START or :T-STRING.
Returns (values ast rest)."
  (let ((tok (js-peek stream)))
    (cond
      ;; Simple string template (no interpolation)
      ((eq (js-tok-type tok) :T-STRING)
       (multiple-value-bind (str-tok rest) (js-consume stream)
         (values (make-ast-quote :value (js-tok-value str-tok)) rest)))
      ;; Template with parts: :T-TEMPLATE-PARTS
      ((eq (js-tok-type tok) :T-TEMPLATE-PARTS)
       (multiple-value-bind (parts-tok rest) (js-consume stream)
         (let ((parts (js-tok-value parts-tok))
               (segments nil))
           (dolist (part parts)
             (cond
               ((stringp part)
                (push (make-ast-quote :value part) segments))
               ((and (consp part) (eq (car part) :template-expr))
                (let ((inner-tokens (cadr part)))
                  (multiple-value-bind (expr _rest)
                      (js-parse-assignment-expr inner-tokens)
                    (declare (ignore _rest))
                    (push (%js-call '%js-to-string expr) segments))))
               (t
                (push (make-ast-quote :value (format nil "~A" part)) segments))))
           (let ((parts-list (nreverse segments)))
             (values (if (null (cdr parts-list))
                         (car parts-list)
                         (reduce (lambda (l r) (%js-call '%js-add l r))
                                 parts-list))
                     rest)))))
      ;; :T-TEMPLATE-START — consumed by the template lexer inline
      ((eq (js-tok-type tok) :T-TEMPLATE-START)
       (multiple-value-bind (tok2 rest) (js-consume stream)
         (declare (ignore tok2))
         (%js-parse-template-literal rest)))
      (t
       (error "JS parse error: expected template literal, got ~S" tok)))))

;;; ─── Unary ───────────────────────────────────────────────────────────────────
;;;
;;; Data-driven dispatch for simple OP-string and keyword unary operators.
;;; Prefix ++/--, unary -, and yield have special logic and are handled explicitly.

;;; OP-string unaries: maps operator string -> (lambda (expr) -> ast).
;;; Excludes - (constant folding), ++ and -- (place/var dispatch).
(defparameter *js-unary-op-builders*
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (entry
             `(("!" . ,(lambda (expr)
                         (make-ast-call :func (make-ast-var :name 'not)
                                        :args (list (%js-call '%js-truthy expr)))))
               ("~" . ,(lambda (expr) (%js-call '%js-bitwise-not expr)))
               ("+" . ,(lambda (expr) (%js-call '%js-unary-plus expr)))))
      (setf (gethash (car entry) ht) (cdr entry)))
    ht)
  "Data table: JS unary OP string -> AST builder (lambda (expr) -> ast).
Only for simple single-argument operators; -, ++, --, yield handled separately.")

;;; Keyword unaries: maps token type -> (lambda (expr) -> ast).
;;; typeof/void/delete/await all follow the same consume-one-token pattern.
(defparameter *js-unary-kw-builders*
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (entry
             `((:T-TYPEOF . ,(lambda (expr) (%js-call '%js-typeof expr)))
               (:T-DELETE . ,(lambda (expr) (%js-call '%js-delete expr)))
               (:T-AWAIT  . ,(lambda (expr) (%js-call '%js-await expr)))
               (:T-VOID   . ,(lambda (expr)
                               (make-ast-progn
                                :forms (list expr (make-ast-quote :value :js-undefined)))))))
      (setf (gethash (car entry) ht) (cdr entry)))
    ht)
  "Data table: keyword unary token type -> AST builder (lambda (expr) -> ast).
typeof/void/delete/await all consume one token and wrap the sub-expression.")

(defun js-parse-unary (stream)
  "Parse prefix unary: ! ~ + - ++ -- typeof void delete await yield.
Returns (values ast rest)."
  (let ((type (js-peek-type stream))
        (val  (js-peek-value stream)))
    ;; CPS helper: consume one token, recurse into sub-expression, apply BUILDER.
    (labels ((consume-and-build (builder)
               (multiple-value-bind (tok rest) (js-consume stream)
                 (declare (ignore tok))
                 (multiple-value-bind (expr rest2) (js-parse-unary rest)
                   (values (funcall builder expr) rest2)))))
      (cond
        ;; Table-driven: simple OP-string unary operators (!, ~, +)
        ((and (eq type :T-OP) (gethash val *js-unary-op-builders*))
         (consume-and-build (gethash val *js-unary-op-builders*)))
        ;; Unary - : constant-fold integer literals, else negate
        ((and (eq type :T-OP) (string= val "-"))
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           (multiple-value-bind (expr rest2) (js-parse-unary rest)
             (if (ast-int-p expr)
                 (values (make-ast-int :value (- (ast-int-value expr))) rest2)
                 (values (make-ast-call :func (make-ast-var :name '-)
                                        :args (list expr))
                         rest2)))))
        ;; Table-driven: keyword unary operators (typeof, void, delete, await)
        ((gethash type *js-unary-kw-builders*)
         (consume-and-build (gethash type *js-unary-kw-builders*)))
        ;; Prefix ++ (var or place)
        ((and (eq type :T-OP) (string= val "++"))
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           (multiple-value-bind (expr rest2) (js-parse-unary rest)
             (if (ast-var-p expr)
                 (let ((var-sym (ast-var-name expr)))
                   (values (make-ast-setq :var var-sym
                                          :value (make-ast-binop :op '+
                                                                 :lhs expr
                                                                 :rhs (make-ast-int :value 1)))
                           rest2))
                 (if (%js-place-get-prop-p expr)
                     (values (%js-lower-place-incdec expr '+ t) rest2)
                     (values (%js-call '%js-prefix-inc expr) rest2))))))
        ;; Prefix -- (var or place)
        ((and (eq type :T-OP) (string= val "--"))
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           (multiple-value-bind (expr rest2) (js-parse-unary rest)
             (if (ast-var-p expr)
                 (let ((var-sym (ast-var-name expr)))
                   (values (make-ast-setq :var var-sym
                                          :value (make-ast-binop :op '-
                                                                 :lhs expr
                                                                 :rhs (make-ast-int :value 1)))
                           rest2))
                 (if (%js-place-get-prop-p expr)
                     (values (%js-lower-place-incdec expr '- t) rest2)
                     (values (%js-call '%js-prefix-dec expr) rest2))))))
        ;; yield (prefix usage)
        ((eq type :T-YIELD)
         (multiple-value-bind (tok rest) (js-consume stream)
           (declare (ignore tok))
           ;; yield* iterable
           (if (and (eq (js-peek-type rest) :T-OP) (string= (js-peek-value rest) "*"))
               (multiple-value-bind (tok2 rest2) (js-consume rest)
                 (declare (ignore tok2))
                 (multiple-value-bind (expr rest3) (js-parse-assignment-expr rest2)
                   (values (%js-call '%js-yield-from expr) rest3)))
               ;; yield expr or bare yield
               (if (or (js-at-eof-p rest)
                       (member (js-peek-type rest)
                               '(:T-SEMI :T-COMMA :T-RBRACE :T-RPAREN :T-RBRACKET) :test #'eq))
                   (values (%js-call '%js-yield) rest)
                   (multiple-value-bind (expr rest2) (js-parse-assignment-expr rest)
                     (values (%js-call '%js-yield expr) rest2))))))
        ;; Fall through to postfix
        (t
         (multiple-value-bind (ast rest) (js-parse-primary stream)
           (js-parse-postfix ast rest)))))))
