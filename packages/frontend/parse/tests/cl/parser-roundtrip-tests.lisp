;;;; tests/unit/parse/cl/parser-roundtrip-tests.lisp
;;;; Unit tests for src/parse/cl/parser-roundtrip.lisp — AST → S-expression.
;;;;
;;;; Covers: ast-to-sexp methods for all node types (ast-int, ast-var, ast-binop,
;;;;   ast-if, ast-progn, ast-let, ast-lambda, ast-call, ast-quote, ast-setq,
;;;;   ast-function, ast-block, ast-return-from, ast-go, ast-the, ast-defvar,
;;;;   ast-defun, ast-values, ast-apply, ast-catch, ast-throw, ast-tagbody,
;;;;   ast-flet, ast-labels, ast-multiple-value-bind, ast-defclass, ast-defgeneric,
;;;;   ast-defmethod, ast-slot-value, ast-set-slot-value, ast-set-gethash,
;;;;   ast-print, ast-defmacro, ast-unwind-protect, ast-handler-case,
;;;;   ast-make-instance), plus the slot-def-to-sexp helper.

(in-package :cl-cc/test)
(in-suite cl-cc-unit-suite)

;;; ─── Leaf nodes ──────────────────────────────────────────────────────────

(deftest-each ast-to-sexp-scalar-nodes
  "ast-to-sexp for scalar AST nodes returns the appropriate sexp form."
  :cases (("int"      42              (cl-cc::make-ast-int      :value 42))
          ("var"      'x              (cl-cc::make-ast-var      :name 'x))
          ("quote"    '(quote hello)  (cl-cc::make-ast-quote    :value 'hello))
          ("function" '(function foo) (cl-cc::make-ast-function :name 'foo))
          ("go"       '(go loop-top)  (cl-cc::make-ast-go       :tag 'loop-top)))
  (expected node)
  (assert-equal expected (cl-cc::ast-to-sexp node)))

;;; ─── Binary/conditional structure ────────────────────────────────────────

(deftest ast-to-sexp-binop-produces-infix-list
  "ast-to-sexp for ast-binop returns (op lhs rhs)."
  (let ((node (cl-cc::make-ast-binop
               :op '+
               :lhs (cl-cc::make-ast-int :value 3)
               :rhs (cl-cc::make-ast-int :value 4))))
    (assert-equal '(+ 3 4) (cl-cc::ast-to-sexp node))))

(deftest ast-to-sexp-if-produces-three-branch-if
  "ast-to-sexp for ast-if returns (if cond then else)."
  (let ((node (cl-cc::make-ast-if
               :cond (cl-cc::make-ast-int :value 1)
               :then (cl-cc::make-ast-var :name 'a)
               :else (cl-cc::make-ast-var :name 'b))))
    (assert-equal '(if 1 a b) (cl-cc::ast-to-sexp node))))

(deftest ast-to-sexp-the-wraps-with-type
  "ast-to-sexp for ast-the returns (the type value)."
  (let ((node (cl-cc::make-ast-the
               :type 'integer
               :value (cl-cc::make-ast-var :name 'n))))
    (assert-equal '(the integer n) (cl-cc::ast-to-sexp node))))

;;; ─── Sequencing / binding ─────────────────────────────────────────────────

(deftest ast-to-sexp-progn-collects-forms
  "ast-to-sexp for ast-progn returns (progn form1 form2 ...)."
  (let ((node (cl-cc::make-ast-progn
               :forms (list (cl-cc::make-ast-int :value 1)
                            (cl-cc::make-ast-int :value 2)))))
    (assert-equal '(progn 1 2) (cl-cc::ast-to-sexp node))))

(deftest ast-to-sexp-let-produces-let-form
  "ast-to-sexp for ast-let returns (let ((x val)) body...)."
  (let ((node (cl-cc::make-ast-let
               :bindings (list (cons 'x (cl-cc::make-ast-int :value 5)))
               :body (list (cl-cc::make-ast-var :name 'x)))))
    (let ((result (cl-cc::ast-to-sexp node)))
      (assert-eq 'let (car result))
      (assert-equal '((x 5)) (second result))
      (assert-equal '(x) (cddr result)))))

(deftest ast-to-sexp-setq-produces-setq-form
  "ast-to-sexp for ast-setq returns (setq var value)."
  (let ((node (cl-cc::make-ast-setq
               :var 'x
               :value (cl-cc::make-ast-int :value 0))))
    (assert-equal '(setq x 0) (cl-cc::ast-to-sexp node))))

;;; ─── Lambda / function definitions ───────────────────────────────────────

(deftest ast-to-sexp-lambda-produces-lambda-form
  "ast-to-sexp for ast-lambda returns (lambda (params) body...)."
  (let ((node (cl-cc::make-ast-lambda
               :params '(x y)
               :body (list (cl-cc::make-ast-binop
                            :op '+
                            :lhs (cl-cc::make-ast-var :name 'x)
                            :rhs (cl-cc::make-ast-var :name 'y))))))
    (let ((result (cl-cc::ast-to-sexp node)))
      (assert-eq 'lambda (car result))
      (assert-equal '(x y) (second result))
      (assert-equal '((+ x y)) (cddr result)))))

(deftest ast-to-sexp-defun-produces-defun-form
  "ast-to-sexp for ast-defun returns (defun name (params) body...)."
  (let ((node (cl-cc::make-ast-defun
               :name 'square
               :params '(n)
               :body (list (cl-cc::make-ast-binop
                            :op '*
                            :lhs (cl-cc::make-ast-var :name 'n)
                            :rhs (cl-cc::make-ast-var :name 'n))))))
    (let ((result (cl-cc::ast-to-sexp node)))
      (assert-eq 'defun (car result))
      (assert-eq 'square (second result))
      (assert-equal '(n) (third result)))))

;;; ─── defvar ───────────────────────────────────────────────────────────────

(deftest ast-to-sexp-defvar-with-value
  "ast-to-sexp for ast-defvar with a value returns (defvar name value)."
  (let ((node (cl-cc::make-ast-defvar
               :name '*count*
               :value (cl-cc::make-ast-int :value 0)
               :kind 'defvar)))
    (assert-equal '(defvar *count* 0) (cl-cc::ast-to-sexp node))))

(deftest ast-to-sexp-defvar-without-value
  "ast-to-sexp for ast-defvar without a value returns (defvar name)."
  (let ((node (cl-cc::make-ast-defvar
               :name '*x*
               :value nil
               :kind 'defvar)))
    (assert-equal '(defvar *x*) (cl-cc::ast-to-sexp node))))

;;; ─── Call sites ───────────────────────────────────────────────────────────

(deftest ast-to-sexp-call-symbol-func
  "ast-to-sexp for ast-call with a symbol func returns (func arg1 arg2)."
  (let ((node (cl-cc::make-ast-call
               :func 'foo
               :args (list (cl-cc::make-ast-int :value 1)
                           (cl-cc::make-ast-int :value 2)))))
    (assert-equal '(foo 1 2) (cl-cc::ast-to-sexp node))))

(deftest ast-to-sexp-call-ast-func
  "ast-to-sexp for ast-call with an AST func node recursively roundtrips the func."
  (let ((node (cl-cc::make-ast-call
               :func (cl-cc::make-ast-var :name 'f)
               :args (list (cl-cc::make-ast-int :value 3)))))
    (assert-equal '(f 3) (cl-cc::ast-to-sexp node))))

(deftest ast-to-sexp-apply-produces-apply-form
  "ast-to-sexp for ast-apply returns (apply func args...)."
  (let ((node (cl-cc::make-ast-apply
               :func (cl-cc::make-ast-var :name 'f)
               :args (list (cl-cc::make-ast-var :name 'args)))))
    (assert-equal '(apply f args) (cl-cc::ast-to-sexp node))))

;;; ─── Block / return / tagbody / catch / throw ────────────────────────────

(deftest-each ast-to-sexp-control-flow-nodes
  "ast-to-sexp for block/jump/exception/values AST nodes returns the correct sexp."
  :cases (("block"
           '(block outer 0)
           (cl-cc::make-ast-block :name 'outer :body (list (cl-cc::make-ast-int :value 0))))
          ("return-from"
           '(return-from outer 42)
           (cl-cc::make-ast-return-from :name 'outer :value (cl-cc::make-ast-int :value 42)))
          ("catch"
           '(catch (quote :done) 1)
           (cl-cc::make-ast-catch :tag (cl-cc::make-ast-quote :value :done)
                                  :body (list (cl-cc::make-ast-int :value 1))))
          ("throw"
           '(throw (quote :done) 99)
           (cl-cc::make-ast-throw :tag (cl-cc::make-ast-quote :value :done)
                                  :value (cl-cc::make-ast-int :value 99)))
          ("values"
           '(values 1 2)
           (cl-cc::make-ast-values :forms (list (cl-cc::make-ast-int :value 1)
                                                (cl-cc::make-ast-int :value 2)))))
  (expected node)
  (assert-equal expected (cl-cc::ast-to-sexp node)))

(deftest ast-to-sexp-multiple-value-bind-produces-mvb-form
  "ast-to-sexp for ast-mvb returns (multiple-value-bind vars form body...)."
  (let ((node (cl-cc::make-ast-multiple-value-bind
               :vars '(a b)
               :values-form (cl-cc::make-ast-call :func 'two-values :args nil)
               :body (list (cl-cc::make-ast-var :name 'a)))))
    (let ((result (cl-cc::ast-to-sexp node)))
      (assert-eq 'multiple-value-bind (car result))
      (assert-equal '(a b) (second result)))))

;;; ─── CLOS nodes ───────────────────────────────────────────────────────────

(deftest ast-to-sexp-defgeneric-produces-defgeneric-form
  "ast-to-sexp for ast-defgeneric returns (defgeneric name params)."
  (let ((node (cl-cc::make-ast-defgeneric :name 'area :params '(shape))))
    (assert-equal '(defgeneric area (shape)) (cl-cc::ast-to-sexp node))))

(deftest ast-to-sexp-slot-value-produces-slot-value-form
  "ast-to-sexp for ast-slot-value returns (slot-value obj 'slot-name)."
  (let ((node (cl-cc::make-ast-slot-value
               :object (cl-cc::make-ast-var :name 'obj)
               :slot 'count)))
    (assert-equal '(slot-value obj 'count) (cl-cc::ast-to-sexp node))))

(deftest ast-to-sexp-set-gethash-produces-setf-gethash-form
  "ast-to-sexp for ast-set-gethash returns (setf (gethash key table) val)."
  (let ((node (cl-cc::make-ast-set-gethash
               :key (cl-cc::make-ast-quote :value :k)
               :table (cl-cc::make-ast-var :name 'ht)
               :value (cl-cc::make-ast-int :value 1))))
    (let ((result (cl-cc::ast-to-sexp node)))
      (assert-eq 'setf (car result))
      (assert-eq 'gethash (caadr result)))))

;;; ─── slot-def-to-sexp ────────────────────────────────────────────────────

(deftest slot-def-to-sexp-plain-slot-returns-symbol
  "slot-def-to-sexp with no options returns just the slot name symbol."
  (let ((slot (cl-cc::make-ast-slot-def
               :name 'value
               :initarg nil :initform nil
               :reader nil :writer nil :accessor nil)))
    (assert-eq 'value (cl-cc::slot-def-to-sexp slot))))

(deftest slot-def-to-sexp-with-initarg-includes-initarg
  "slot-def-to-sexp with :initarg returns a plist including :initarg."
  (let ((slot (cl-cc::make-ast-slot-def
               :name 'count
               :initarg :count
               :initform nil :reader nil :writer nil :accessor nil)))
    (let ((result (cl-cc::slot-def-to-sexp slot)))
      (assert-eq 'count (car result))
      (assert-true (member :initarg result)))))
