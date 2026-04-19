;;;; tests/pbt/generators-typed-ast.lisp — Typed AST generators, utilities, and example properties
(in-package :cl-cc/pbt)

;;; Typed AST Generators

;; Type-annotated AST node structures
(defstruct (typed-ast (:constructor make-typed-ast-raw))
  "Base structure for typed AST nodes."
  node-type
  source-node)

(defstruct (typed-ast-int (:include typed-ast)
                          (:constructor make-typed-ast-int-raw))
  "Typed integer literal."
  value)

(defstruct (typed-ast-float (:include typed-ast)
                            (:constructor make-typed-ast-float-raw))
  "Typed float literal."
  value)

(defstruct (typed-ast-string (:include typed-ast)
                             (:constructor make-typed-ast-string-raw))
  "Typed string literal."
  value)

(defstruct (typed-ast-boolean (:include typed-ast)
                              (:constructor make-typed-ast-boolean-raw))
  "Typed boolean literal."
  value)

(defstruct (typed-ast-var (:include typed-ast)
                          (:constructor make-typed-ast-var-raw))
  "Typed variable reference."
  name)

(defstruct (typed-ast-binop (:include typed-ast)
                            (:constructor make-typed-ast-binop-raw))
  "Typed binary operation."
  op
  lhs
  rhs)

(defstruct (typed-ast-if (:include typed-ast)
                         (:constructor make-typed-ast-if-raw))
  "Typed conditional expression."
  cond
  then
  else)

(defstruct (typed-ast-lambda (:include typed-ast)
                             (:constructor make-typed-ast-lambda-raw))
  "Typed lambda expression with typed parameters."
  params      ; List of (name . type) pairs
  body)

(defstruct (typed-ast-call (:include typed-ast)
                           (:constructor make-typed-ast-call-raw))
  "Typed function call."
  func
  func-type   ; Function type
  args)

(defstruct (typed-ast-let (:include typed-ast)
                          (:constructor make-typed-ast-let-raw))
  "Typed let binding."
  bindings    ; List of (name . (type . expr))
  body)

(defun gen-typed-primitive-value ()
  "Generate a typed primitive value with its type."
  (gen-bind
   (gen-one-of '(fixnum single-float string boolean))
   (lambda (type)
     (case type
       (fixnum (gen-fmap
                (lambda (v)
                  (make-typed-ast-int-raw
                   :node-type 'fixnum
                   :value v))
                (gen-integer :min -1000 :max 1000)))
       (single-float (gen-fmap
                      (lambda (v)
                        (make-typed-ast-float-raw
                         :node-type 'single-float
                         :value v))
                      (gen-float :min -1000.0 :max 1000.0)))
       (string (gen-fmap
                (lambda (v)
                  (make-typed-ast-string-raw
                   :node-type 'string
                   :value v))
                (gen-string :min-length 0 :max-length 20)))
       (boolean (gen-fmap
                 (lambda (v)
                   (make-typed-ast-boolean-raw
                    :node-type 'boolean
                    :value v))
                 (gen-boolean)))))))

(defun gen-typed-terminal ()
  "Generate typed terminal AST nodes (literals and variables)."
  (gen-bind
   (gen-one-of '(0 1 2))
   (lambda (choice)
     (case choice
       (0 (gen-typed-primitive-value))
       (1 (gen-fmap
           (lambda (name)
             (make-typed-ast-var-raw
              :node-type (generate (gen-primitive-type))
              :name name))
           (gen-symbol :package nil :prefix "VAR")))
       (2 (gen-typed-primitive-value))))))

(defun gen-typed-binop ()
  "Generate typed binary operation AST nodes."
  (gen-bind
   (gen-one-of '(+ - * /))
   (lambda (op)
     (gen-fmap
      (lambda (data)
        (destructuring-bind (lhs rhs) data
          (make-typed-ast-binop-raw
           :node-type 'fixnum
           :op op
           :lhs lhs
           :rhs rhs)))
      (gen-tuple (gen-typed-ast-node :depth 1)
                 (gen-typed-ast-node :depth 1))))))

(defun gen-typed-if ()
  "Generate typed if expression AST nodes."
  (gen-fmap
   (lambda (data)
     (destructuring-bind (cond then else) data
       (make-typed-ast-if-raw
        :node-type (typed-ast-node-type then)
        :cond cond
        :then then
        :else else)))
   (gen-tuple (gen-fmap
               (lambda (v)
                 (make-typed-ast-boolean-raw :node-type 'boolean :value v))
               (gen-boolean))
              (gen-typed-ast-node :depth 1)
              (gen-typed-ast-node :depth 1))))

(defun gen-typed-param ()
  "Generate a typed function parameter (name . type)."
  (gen-fmap
   (lambda (data)
     (destructuring-bind (name type) data
       (cons name type)))
   (gen-tuple (gen-symbol :package nil :prefix "ARG")
              (gen-type-expr :depth 0))))

(defun gen-typed-lambda ()
  "Generate typed lambda expression AST nodes."
  (gen-bind
   (gen-list-of (gen-typed-param) :min-length 0 :max-length 3)
   (lambda (params)
     (gen-fmap
      (lambda (body)
        (let ((ret-type (typed-ast-node-type body))
              (arg-types (mapcar #'cdr params)))
          (make-typed-ast-lambda-raw
           :node-type (list 'function arg-types ret-type)
           :params params
           :body (list body))))
      (gen-typed-ast-node :depth 1)))))

(defun gen-typed-call ()
  "Generate typed function call AST nodes."
  (gen-bind
   (gen-typed-lambda)
   (lambda (func)
     (let* ((fn-type (typed-ast-node-type func))
            (arg-types (second fn-type)))
       (gen-fmap
        (lambda (args)
          (make-typed-ast-call-raw
           :node-type (third fn-type)
           :func func
           :func-type fn-type
           :args args))
        (gen-list-of (gen-typed-ast-node :depth 0)
                     :min-length (length arg-types)
                     :max-length (length arg-types)))))))

(defun gen-typed-let ()
  "Generate typed let binding AST nodes."
  (gen-bind
   (gen-list-of (gen-typed-param) :min-length 1 :max-length 3)
   (lambda (bindings)
     (gen-fmap
      (lambda (data)
        (destructuring-bind (values body) data
          (let ((typed-bindings
                  (loop for (name . type) in bindings
                        for val in values
                        collect (cons name (cons type val)))))
            (make-typed-ast-let-raw
             :node-type (typed-ast-node-type body)
             :bindings typed-bindings
             :body (list body)))))
      (gen-tuple (gen-list-of (gen-typed-ast-node :depth 0)
                              :min-length (length bindings)
                              :max-length (length bindings))
                 (gen-typed-ast-node :depth 1))))))

(defun gen-typed-ast-node (&key (depth *max-type-depth*))
  "Generate AST nodes with type annotations.

   Supports:
   - ast-int, ast-binop, ast-if with type annotations
   - ast-lambda with typed parameters
   - ast-call with function types"
  (let ((effective-depth (max 0 (min depth *max-type-depth*))))
    (if (or (zerop effective-depth)
            (< (random 100 (%pbt-rng))
               (- 100 (* effective-depth 15))))
        ;; Generate terminal node
        (gen-typed-terminal)
        ;; Generate recursive node
        (gen-bind
         (gen-one-of (list 0 1 2 3 4))
         (lambda (choice)
           (case choice
             (0 (gen-typed-binop))
             (1 (gen-typed-if))
             (2 (gen-typed-lambda))
             (3 (gen-typed-call))
             (4 (gen-typed-terminal))))))))

;;; Utility functions (typed-ast-to-sexp, extract-type-from-ast) and example
;;; properties are in generators-typed-ast-utils.lisp.
