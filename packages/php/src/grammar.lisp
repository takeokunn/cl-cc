;;;; php/grammar.lisp — CST-based PHP grammar parser
;;;;
;;;; Parses PHP source into CST nodes using a php-token-stream similar
;;;; to the CL token-stream. Covers expressions, statements, and
;;;; class/function declarations.

(in-package :cl-cc/php)

;;; ─── PHP Token Stream ────────────────────────────────────────────────────────

(defstruct php-token-stream
  "A token stream for the PHP grammar parser."
  (tokens      nil :type list)
  (source      ""  :type string)
  (source-file nil :type (or string null))
  (diagnostics nil :type list))

(defun php-ts-peek (ts)
  "Peek at the current token."
  (car (php-token-stream-tokens ts)))

(defun php-ts-peek-type (ts)
  "Return the type of the current token."
  (let ((tok (php-ts-peek ts)))
    (when tok (php-tok-type tok))))

(defun php-ts-peek-value (ts)
  "Return the value of the current token."
  (let ((tok (php-ts-peek ts)))
    (when tok (php-tok-value tok))))

(defun php-ts-advance (ts)
  "Consume and return the current token."
  (let ((tok (car (php-token-stream-tokens ts))))
    (setf (php-token-stream-tokens ts) (cdr (php-token-stream-tokens ts)))
    tok))

(defun php-ts-expect (ts expected-type &optional expected-value context)
  "Consume a token of EXPECTED-TYPE (optionally matching EXPECTED-VALUE).
   Add diagnostic on mismatch."
  (let ((tok (php-ts-peek ts)))
    (cond
      ((null tok)
       (push (make-parse-error
              (format nil "~@[~A: ~]unexpected end of input, expected ~A"
                      context expected-type)
              (cons 0 0)
              :source-file (php-token-stream-source-file ts))
             (php-token-stream-diagnostics ts))
       nil)
      ((and (eq (php-tok-type tok) expected-type)
            (or (null expected-value) (equal (php-tok-value tok) expected-value)))
       (php-ts-advance ts))
      (t
       (push (make-parse-error
              (format nil "~@[~A: ~]expected ~A~@[ ~S~] but got ~A ~S"
                      context expected-type expected-value
                      (php-tok-type tok) (php-tok-value tok))
              (cons 0 0)
              :source-file (php-token-stream-source-file ts))
             (php-token-stream-diagnostics ts))
       nil))))

(defun php-ts-at-end-p (ts)
  (or (null (php-token-stream-tokens ts))
      (eq (php-ts-peek-type ts) :T-EOF)))

(defun php-ts-skip-semis (ts)
  "Skip zero or more semicolons."
  (loop while (and (not (php-ts-at-end-p ts))
                   (eq (php-ts-peek-type ts) :T-SEMI))
        do (php-ts-advance ts)))

;;; ─── CST Node Builders ──────────────────────────────────────────────────────

(defun %php-tok-to-cst (tok)
  "Convert a PHP token plist to a cst-token."
  (when tok
    (make-cst-token :kind (php-tok-type tok)
                    :value (php-tok-value tok)
                    :start-byte 0
                    :end-byte 0)))

(defun %php-cst-interior (kind children)
  "Make a PHP CST interior node."
  (make-cst-interior :kind kind :children children
                     :start-byte 0 :end-byte 0))

;;; ─── Expression Parsers ──────────────────────────────────────────────────────

(defun php-cst-parse-primary (ts)
  "Parse a primary expression: literal, variable, identifier, parenthesized, new."
  (let ((type (php-ts-peek-type ts)))
    (case type
      (:T-INT    (%php-tok-to-cst (php-ts-advance ts)))
      (:T-FLOAT  (%php-tok-to-cst (php-ts-advance ts)))
      (:T-STRING (%php-tok-to-cst (php-ts-advance ts)))
      (:T-VAR    (%php-tok-to-cst (php-ts-advance ts)))
      (:T-KEYWORD
       (let ((kw (php-ts-peek-value ts)))
         (cond
           ((member kw '(:null :true :false))
            (%php-tok-to-cst (php-ts-advance ts)))
           ((eq kw :new)
            (php-cst-parse-new ts))
           (t
            (push (make-parse-error
                   (format nil "unexpected keyword ~S in expression" kw)
                   (cons 0 0))
                  (php-token-stream-diagnostics ts))
            nil))))
      (:T-LPAREN
       (php-ts-advance ts)
       (let ((inner (php-cst-parse-expr ts)))
         (php-ts-expect ts :T-RPAREN nil "expression")
         inner))
      (:T-IDENT
       (let ((tok (php-ts-advance ts)))
         (if (eq (php-ts-peek-type ts) :T-LPAREN)
             ;; Function call
             (let ((args (php-cst-parse-arglist ts)))
               (%php-cst-interior :call (cons (%php-tok-to-cst tok) args)))
             (%php-tok-to-cst tok))))
      (otherwise nil))))

(defun php-cst-parse-new (ts)
  "Parse 'new ClassName(args...)'."
  (let ((new-tok (php-ts-advance ts)))
    (let ((class-tok (php-ts-expect ts :T-IDENT nil "new")))
      (let ((args (if (eq (php-ts-peek-type ts) :T-LPAREN)
                      (php-cst-parse-arglist ts)
                      nil)))
        (%php-cst-interior :new
                           (append (list (%php-tok-to-cst new-tok))
                                   (when class-tok (list (%php-tok-to-cst class-tok)))
                                   args))))))

(defun php-cst-parse-arglist (ts)
  "Parse (arg1, arg2, ...). Returns list of CST nodes."
  (php-ts-expect ts :T-LPAREN nil "arglist")
  (if (eq (php-ts-peek-type ts) :T-RPAREN)
      (progn (php-ts-advance ts) nil)
      (let ((args nil))
        (loop
          (let ((arg (php-cst-parse-expr ts)))
            (when arg (push arg args)))
          (if (eq (php-ts-peek-type ts) :T-COMMA)
              (php-ts-advance ts)
              (return)))
        (php-ts-expect ts :T-RPAREN nil "arglist")
        (nreverse args))))

(defun php-cst-parse-postfix (ts)
  "Parse postfix: method calls, property access."
  (let ((obj (php-cst-parse-primary ts)))
    (loop
      (let ((type (php-ts-peek-type ts)))
        (cond
          ;; -> member access
          ((eq type :T-ARROW)
           (php-ts-advance ts)
           (let ((name-tok (php-ts-expect ts :T-IDENT nil "member access")))
             (if (eq (php-ts-peek-type ts) :T-LPAREN)
                 (let ((args (php-cst-parse-arglist ts)))
                   (setf obj (%php-cst-interior :method-call
                              (append (list obj)
                                      (when name-tok (list (%php-tok-to-cst name-tok)))
                                      args))))
                 (setf obj (%php-cst-interior :property-access
                            (list obj (when name-tok (%php-tok-to-cst name-tok))))))))
          ;; ?-> nullsafe
          ((eq type :T-NULLSAFE-ARROW)
           (php-ts-advance ts)
           (let ((name-tok (php-ts-expect ts :T-IDENT nil "nullsafe access")))
             (if (eq (php-ts-peek-type ts) :T-LPAREN)
                 (let ((args (php-cst-parse-arglist ts)))
                   (setf obj (%php-cst-interior :nullsafe-method-call
                              (append (list obj)
                                      (when name-tok (list (%php-tok-to-cst name-tok)))
                                      args))))
                 (setf obj (%php-cst-interior :nullsafe-access
                            (list obj (when name-tok (%php-tok-to-cst name-tok))))))))
          ;; [ array access
          ((eq type :T-LBRACKET)
           (php-ts-advance ts)
           (let ((index (php-cst-parse-expr ts)))
             (php-ts-expect ts :T-RBRACKET nil "array access")
             (setf obj (%php-cst-interior :array-access
                        (list obj index)))))
          (t (return)))))
    obj))

(defun php-cst-parse-unary (ts)
  "Parse unary: !, -, +."
  (if (and (eq (php-ts-peek-type ts) :T-OP)
           (member (php-ts-peek-value ts) '("!" "-" "+") :test #'equal))
      (let ((op-tok (php-ts-advance ts))
            (operand (php-cst-parse-postfix ts)))
        (%php-cst-interior :unary-op (list (%php-tok-to-cst op-tok) operand)))
      (php-cst-parse-postfix ts)))

(defun php-cst-parse-binop (ts ops next-parser)
  "Left-associative binary operator parsing."
  (let ((lhs (funcall next-parser ts)))
    (loop while (and (eq (php-ts-peek-type ts) :T-OP)
                     (member (php-ts-peek-value ts) ops :test #'equal))
          do (let ((op-tok (php-ts-advance ts))
                   (rhs (funcall next-parser ts)))
               (setf lhs (%php-cst-interior :binary-op
                           (list (%php-tok-to-cst op-tok) lhs rhs)))))
    lhs))

(defun php-cst-parse-mul (ts)
  (php-cst-parse-binop ts '("*" "/") #'php-cst-parse-unary))

(defun php-cst-parse-add (ts)
  (php-cst-parse-binop ts '("+" "-" ".") #'php-cst-parse-mul))

(defun php-cst-parse-cmp (ts)
  (php-cst-parse-binop ts '("==" "===" "!=" "!==" "<" ">" "<=" ">=")
                       #'php-cst-parse-add))

(defun php-cst-parse-and (ts)
  (php-cst-parse-binop ts '("&&") #'php-cst-parse-cmp))

(defun php-cst-parse-or (ts)
  (php-cst-parse-binop ts '("||") #'php-cst-parse-and))

(defun php-cst-parse-expr (ts)
  "Parse an expression, including assignment."
  (if (and (eq (php-ts-peek-type ts) :T-VAR)
           (let ((rest (cdr (php-token-stream-tokens ts))))
             (and rest (eq (php-tok-type (car rest)) :T-OP)
                  (equal "=" (php-tok-value (car rest))))))
      ;; Assignment
      (let ((var-tok (php-ts-advance ts)))
        (php-ts-advance ts) ; consume =
        (let ((val (php-cst-parse-or ts)))
          (%php-cst-interior :assign (list (%php-tok-to-cst var-tok) val))))
      (php-cst-parse-or ts)))


;;; Statement parsers and top-level entry point (parse-php-source-to-cst)
;;; are in grammar-stmt.lisp (loads after this file).
