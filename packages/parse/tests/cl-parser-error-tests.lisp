;;;; tests/unit/parse/cl-parser-error-tests.lisp — CL parser lower-sexp special forms and error tests
;;;;
;;;; Tests: lower-sexp-to-ast special forms, binding forms, function forms,
;;;; type-checking forms, and error/arity signalling cases.
;;;; Requires helpers (parse-one, parse-many, lower) from cl-parser-tests.lisp (loaded first).

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─── lower-sexp-to-ast: special forms ───────────────────────────────────────

(deftest-each lower-control-flow-forms
  "lower-sexp-to-ast: if, if-without-else, progn, and let produce correct node types and fields."
  :cases (("if-with-else"    '(if x 1 2)         #'cl-cc/ast:ast-if-p)
          ("if-without-else" '(if x 1)            #'cl-cc/ast:ast-if-p)
          ("progn"           '(progn 1 2 3)        #'cl-cc/ast:ast-progn-p)
          ("let"             '(let ((x 1) (y 2)) x) #'cl-cc/ast:ast-let-p))
  (form pred)
  (assert-true (funcall pred (lower form))))

(deftest-each lower-let-lambda-and-defun-details
  "lower-sexp-to-ast: let declarations, bare-symbol binding, lambda params, defun, and single-element binding."
  :cases (("let-declaration"
           (lambda ()
             (let ((node (lower '(let ((x 1)) (declare (ignore x)) 42))))
               (assert-true (cl-cc/ast:ast-let-p node))
               (assert-equal '((ignore x)) (cl-cc/ast:ast-let-declarations node)))))
          ("let-bare-symbol"
           (lambda ()
             (let ((node (lower '(let (x) x))))
               (assert-true (cl-cc/ast:ast-let-p node))
               (let ((binding (first (cl-cc/ast:ast-let-bindings node))))
                 (assert-eq 'x (car binding))
                 (assert-true (cl-cc/ast:ast-quote-p (cdr binding)))))))
          ("lambda-params"
           (lambda ()
             (let ((node (lower '(lambda (x y) (+ x y)))))
               (assert-true (cl-cc/ast:ast-lambda-p node))
               (assert-equal '(x y) (cl-cc::ast-lambda-params node))
               (assert-= 1 (length (cl-cc::ast-lambda-body node))))))
          ("defun-form"
           (lambda ()
             (let ((node (lower '(defun my-func (a b) (+ a b)))))
               (assert-true (cl-cc/ast:ast-defun-p node))
               (assert-eq 'my-func (cl-cc/ast:ast-defun-name node))
               (assert-equal '(a b) (cl-cc::ast-defun-params node))
               (assert-= 1 (length (cl-cc::ast-defun-body node))))))
          ("let-single-element"
           (lambda ()
             (let ((node (lower '(let ((x)) x))))
               (assert-true (cl-cc/ast:ast-let-p node))
               (let ((binding (first (cl-cc/ast:ast-let-bindings node))))
                 (assert-eq 'x (car binding))
                 (assert-true (cl-cc/ast:ast-quote-p (cdr binding)))
                 (assert-null (cl-cc/ast:ast-quote-value (cdr binding))))))))
  (verify)
  (funcall verify))

(deftest-each lower-definition-and-binding-forms
  "lower-sexp-to-ast: defvar, setq, quote, block, and return-from produce correct node types."
  :cases (("defvar-with-value" '(defvar *x* 42)          #'cl-cc/ast:ast-defvar-p)
          ("defvar-no-value"   '(defvar *x*)              #'cl-cc/ast:ast-defvar-p)
          ("setq"              '(setq x 10)               #'cl-cc/ast:ast-setq-p)
          ("setq-multi"        '(setq a 1 b 2)            #'cl-cc/ast:ast-progn-p)
          ("quote"             '(quote hello)             #'cl-cc/ast:ast-quote-p)
          ("block"             '(block my-block 1 2)      #'cl-cc/ast:ast-block-p)
          ("return-from"       '(return-from my-block 42) #'cl-cc/ast:ast-return-from-p))
  (form pred)
  (assert-true (funcall pred (lower form))))

(deftest-each lower-function-name-forms
  "lower-sexp-to-ast: (function <name>) produces ast-function with correct name."
  :cases (("symbol-ref" '(function foo)        'foo)
          ("setf-name"  '(function (setf foo))  '(setf foo)))
  (form expected-name)
  (let ((node (lower form)))
    (assert-true (cl-cc/ast:ast-function-p node))
    (assert-equal expected-name (cl-cc/ast:ast-function-name node))))

(deftest lower-function-lambda
  "lower-sexp-to-ast: (function (lambda ...)) signals error (not yet supported)"
  (assert-signals error (lower '(function (lambda (x) x)))))

(deftest-each lower-type-only
  "lower-sexp-to-ast: forms whose only check is the AST node type predicate."
  :cases (("values"          '(values 1 2 3)                                    #'cl-cc/ast:ast-values-p)
          ("mvb"             '(multiple-value-bind (a b) (values 1 2) (+ a b))  #'cl-cc/ast:ast-multiple-value-bind-p)
          ("go"              '(go my-tag)                                        #'cl-cc/ast:ast-go-p)
          ("catch"           '(catch 'my-tag 1 2)                               #'cl-cc/ast:ast-catch-p)
          ("throw"           '(throw 'my-tag 42)                                #'cl-cc/ast:ast-throw-p)
          ("apply"           '(apply #'foo '(1 2))                              #'cl-cc/ast:ast-apply-p)
          ("funcall"         '(funcall #'foo 1 2)                               #'cl-cc/ast:ast-call-p)
          ("tagbody"         '(tagbody start (print 1) end (print 2))           #'cl-cc/ast:ast-tagbody-p)
          ("setf-gethash"    '(setf (gethash 'k tbl) 42)                       #'cl-cc/ast:ast-set-gethash-p)
          ("mv-prog1"        '(multiple-value-prog1 (values 1 2) (print 3))     #'cl-cc::ast-multiple-value-prog1-p))
  (form pred)
  (assert-true (funcall pred (lower form))))

(deftest-each lower-unwind-generic-the-cases
  "lower-sexp-to-ast: unwind-protect, generic call, and the form produce correct node types."
  :cases (("unwind-protect"
           (lambda ()
             (let ((node (lower '(unwind-protect (risky) (cleanup)))))
               (assert-true (cl-cc/ast:ast-unwind-protect-p node))
               (assert-= 1 (length (cl-cc::ast-unwind-cleanup node))))))
          ("generic-call"
           (lambda ()
             (let ((node (lower '(my-func 1 2 3))))
               (assert-true (cl-cc/ast:ast-call-p node))
               (assert-= 3 (length (cl-cc/ast:ast-call-args node))))))
          ("the-form"
           (lambda ()
             (let ((node (lower '(the fixnum x))))
               (assert-true (cl-cc/ast:ast-the-p node))
               (assert-eq 'fixnum (cl-cc/ast:ast-the-type node))))))
  (verify)
  (funcall verify))

(deftest-each lower-local-and-binding-forms
  "lower-sexp-to-ast: flet, labels, handler-case, and declarations produce correct node types."
  :cases (("flet"
           (lambda ()
             (let ((node (lower '(flet ((helper (x) (* x 2))) (helper 5)))))
               (assert-true (cl-cc/ast:ast-flet-p node))
               (assert-= 1 (length (cl-cc::ast-flet-bindings node)))
               (assert-= 1 (length (cl-cc::ast-flet-body node))))))
          ("labels"
           (lambda ()
             (let ((node (lower '(labels ((fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (fact 5)))))
               (assert-true (cl-cc/ast:ast-labels-p node))
               (assert-= 1 (length (cl-cc::ast-labels-bindings node))))))
          ("handler-case"
           (lambda ()
             (let ((node (lower '(handler-case (risky) (error (e) (print e))))))
               (assert-true (cl-cc/ast:ast-handler-case-p node))
               (assert-= 1 (length (cl-cc/ast:ast-handler-case-clauses node))))))
          ("declare-type"
           (lambda ()
             (let ((node (lower '(defun add1 (x) (declare (type fixnum x)) (+ x 1)))))
               (assert-true (cl-cc/ast:ast-defun-p node))
               (assert-equal '((x fixnum)) (cl-cc::ast-defun-params node)))
             (let ((node (lower '(lambda (x) (declare (type fixnum x)) (+ x 1)))))
               (assert-true (cl-cc/ast:ast-lambda-p node))
               (assert-equal '((x fixnum)) (cl-cc::ast-lambda-params node)))))
          ("declare-dynamic"
           (lambda ()
             (let ((node (lower '(lambda (&rest args) (declare (dynamic-extent args)) args))))
               (assert-true (cl-cc/ast:ast-lambda-p node))
               (assert-eq 'args (cl-cc::ast-lambda-rest-param node))
               (assert-equal '((dynamic-extent args)) (cl-cc::ast-lambda-declarations node))
               (assert-= 1 (length (cl-cc::ast-lambda-body node)))))))
  (verify)
  (funcall verify))

;;; ─── lower-sexp-to-ast: error cases ────────────────────────────────────────

(deftest-each lower-sexp-arity-errors
  "lower-sexp-to-ast signals error for malformed special forms."
  :cases (("if-missing-args"   '(if))
          ("if-too-many-args"  '(if a b c d))
          ("let-no-bindings"   '(let))
          ("defun-no-params"   '(defun f))
          ("setq-odd-args"     '(setq x))
          ("quote-wrong-arity" '(quote))
          ("function-no-arg"   '(function)))
  (form)
  (assert-signals error (lower form)))
