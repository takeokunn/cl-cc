;;;; tests/pbt/generators-typed-ast-utils.lisp — Typed AST Utilities and Example Properties

(in-package :cl-cc/pbt)

;;; Utility Functions for Typed AST

(defun typed-ast-to-sexp (node)
  "Convert a typed AST node to an S-expression for debugging."
  (etypecase node
    (typed-ast-int     (list 'the (typed-ast-node-type node) (typed-ast-int-value node)))
    (typed-ast-float   (list 'the (typed-ast-node-type node) (typed-ast-float-value node)))
    (typed-ast-string  (list 'the (typed-ast-node-type node) (typed-ast-string-value node)))
    (typed-ast-boolean (list 'the (typed-ast-node-type node) (typed-ast-boolean-value node)))
    (typed-ast-var     (list 'the (typed-ast-node-type node) (typed-ast-var-name node)))
    (typed-ast-binop
     (list 'the (typed-ast-node-type node)
           (list (typed-ast-binop-op node)
                 (typed-ast-to-sexp (typed-ast-binop-lhs node))
                 (typed-ast-to-sexp (typed-ast-binop-rhs node)))))
    (typed-ast-if
     (list 'the (typed-ast-node-type node)
           (list 'if
                 (typed-ast-to-sexp (typed-ast-if-cond node))
                 (typed-ast-to-sexp (typed-ast-if-then node))
                 (typed-ast-to-sexp (typed-ast-if-else node)))))
    (typed-ast-lambda
     (list 'the (typed-ast-node-type node)
           (list* 'lambda
                  (mapcar #'car (typed-ast-lambda-params node))
                  (mapcar #'typed-ast-to-sexp (typed-ast-lambda-body node)))))
    (typed-ast-call
     (list 'the (typed-ast-node-type node)
           (cons (typed-ast-to-sexp (typed-ast-call-func node))
                 (mapcar #'typed-ast-to-sexp (typed-ast-call-args node)))))
    (typed-ast-let
     (list 'the (typed-ast-node-type node)
           (list* 'let
                  (mapcar (lambda (b)
                            (list (car b) (typed-ast-to-sexp (cddr b))))
                          (typed-ast-let-bindings node))
                  (mapcar #'typed-ast-to-sexp (typed-ast-let-body node)))))))

(defun extract-type-from-ast (node)
  "Extract the type from a typed AST node."
  (typed-ast-node-type node))

;;; Example Properties Using New Generators

(in-suite cl-cc-pbt-suite)

;; Type Expression Properties

(defproperty type-expr-is-sexp
    (type-expr (gen-type-expr))
  (or (symbolp type-expr)
      (and (consp type-expr) (symbolp (car type-expr)))))

(defproperty fn-type-has-function-symbol
    (fn-type (gen-fn-type))
  (and (consp fn-type)
       (eq (car fn-type) 'function)
       (= (length fn-type) 3)
       (listp (second fn-type))))

(defproperty type-variables-are-keywords
    (type-var (gen-type-variable))
  (and (symbolp type-var)
       (keywordp type-var)
       (char= (char (symbol-name type-var) 0) #\?)))

;; Mach-O Structure Properties

(defproperty mach-header-has-valid-magic
    (header (cl-cc/pbt-macho:gen-mach-header))
  (member (cl-cc/pbt-macho:mach-header-magic header)
          (list cl-cc/pbt-macho:+mh-magic+
                cl-cc/pbt-macho:+mh-magic-64+
                cl-cc/pbt-macho:+mh-cigam+
                cl-cc/pbt-macho:+mh-cigam-64+)))

(defproperty mach-segment-has-valid-permissions
    (segment (cl-cc/pbt-macho:gen-mach-segment-command))
  (and (<= 0 (cl-cc/pbt-macho:mach-segment-command-maxprot segment) 7)
       (<= 0 (cl-cc/pbt-macho:mach-segment-command-initprot segment) 7)))

(defproperty mach-section-count-matches
    (segment (cl-cc/pbt-macho:gen-mach-segment-command))
  (= (cl-cc/pbt-macho:mach-segment-command-nsects segment)
     (length (cl-cc/pbt-macho:mach-segment-command-sections segment))))

;; Typed AST Properties

(defproperty typed-ast-has-type
    (node (gen-typed-ast-node :depth 2))
  (not (null (typed-ast-node-type node))))

(defproperty typed-lambda-has-function-type
    (node (gen-typed-lambda))
  (let ((type (typed-ast-node-type node)))
    (and (consp type) (eq (car type) 'function) (= (length type) 3))))

(defproperty typed-binop-returns-numeric
    (node (gen-typed-binop))
  (member (typed-ast-node-type node)
          '(fixnum integer single-float double-float number)))
