;;;; tests/unit/ast/ast-analysis-tests.lisp — AST Analysis Tests
;;;;
;;;; Tests for ast-children (structural data layer) and ast-bound-names
;;;; (scoping data layer), plus free-variable and mutation analysis.
;;;;
;;;; Helper %ast-roundtrip is defined in ast-tests.lisp (loaded first).

(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; ast-children — structural data layer
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each ast-children-leaves
  "Leaf AST nodes have no children."
  :cases (("int"      (cl-cc:make-ast-int :value 42))
          ("var"      (cl-cc:make-ast-var :name 'x))
          ("hole"     (cl-cc/ast::make-ast-hole))
          ("quote"    (cl-cc:make-ast-quote :value 'hello))
          ("function" (cl-cc:make-ast-function :name 'foo))
          ("go"       (cl-cc:make-ast-go :tag 'start)))
  (node)
  (assert-null (cl-cc:ast-children node)))

(deftest ast-children-node-types
  "ast-children returns the correct child sub-expressions for each node type."
  ;; binop: (lhs rhs)
  (let* ((lhs (cl-cc:make-ast-int :value 1))
         (rhs (cl-cc:make-ast-int :value 2))
         (node (cl-cc:make-ast-binop :op '+ :lhs lhs :rhs rhs))
         (children (cl-cc:ast-children node)))
    (assert-equal 2 (length children))
    (assert-eq lhs (first children))
    (assert-eq rhs (second children)))
  ;; if: (cond then else)
  (let* ((c (cl-cc:make-ast-int :value 1))
         (th (cl-cc:make-ast-int :value 2))
         (el (cl-cc:make-ast-int :value 3))
         (node (cl-cc:make-ast-if :cond c :then th :else el)))
    (assert-equal 3 (length (cl-cc:ast-children node))))
  ;; progn: forms
  (let* ((f1 (cl-cc:make-ast-int :value 1))
         (f2 (cl-cc:make-ast-int :value 2))
         (node (cl-cc:make-ast-progn :forms (list f1 f2))))
    (assert-equal 2 (length (cl-cc:ast-children node))))
  ;; let: init-exprs + body
  (let* ((init (cl-cc:make-ast-int :value 1))
         (body (cl-cc:make-ast-var :name 'x))
         (node (cl-cc:make-ast-let :bindings (list (cons 'x init))
                                   :body (list body)))
         (children (cl-cc:ast-children node)))
    (assert-equal 2 (length children))
    (assert-true (member init children :test #'eq))
    (assert-true (member body children :test #'eq)))
  ;; lambda: body forms
  (let* ((body (cl-cc:make-ast-var :name 'x))
         (node (cl-cc:make-ast-lambda :params '(x) :body (list body))))
    (assert-equal 1 (length (cl-cc:ast-children node))))
  ;; setq: value expression
  (let* ((val (cl-cc:make-ast-int :value 42))
         (node (cl-cc:make-ast-setq :var 'x :value val)))
    (assert-equal 1 (length (cl-cc:ast-children node)))
    (assert-eq val (first (cl-cc:ast-children node))))
  ;; call with symbol func: args only
  (let* ((arg1 (cl-cc:make-ast-int :value 1))
         (arg2 (cl-cc:make-ast-int :value 2))
         (node (cl-cc:make-ast-call :func 'foo :args (list arg1 arg2))))
    (assert-equal 2 (length (cl-cc:ast-children node))))
  ;; call with AST func: func + args
  (let* ((func (cl-cc:make-ast-var :name 'f))
         (arg1 (cl-cc:make-ast-int :value 1))
         (node (cl-cc:make-ast-call :func func :args (list arg1))))
    (assert-equal 2 (length (cl-cc:ast-children node)))
    (assert-eq func (first (cl-cc:ast-children node))))
  ;; block: body forms
  (let* ((body (cl-cc:make-ast-int :value 1))
         (node (cl-cc:make-ast-block :name 'b :body (list body))))
    (assert-equal 1 (length (cl-cc:ast-children node))))
  ;; catch: tag + body
  (let* ((tag (cl-cc:make-ast-var :name 'tag))
         (body (cl-cc:make-ast-int :value 1))
         (node (cl-cc:make-ast-catch :tag tag :body (list body))))
    (assert-equal 2 (length (cl-cc:ast-children node)))
    (assert-eq tag (first (cl-cc:ast-children node))))
  ;; throw: (tag value)
  (let* ((tag (cl-cc:make-ast-var :name 'tag))
         (val (cl-cc:make-ast-int :value 42))
         (node (cl-cc:make-ast-throw :tag tag :value val)))
    (assert-equal 2 (length (cl-cc:ast-children node))))
  ;; the: value expression
  (let* ((val (cl-cc:make-ast-int :value 1))
         (node (cl-cc:make-ast-the :type 'fixnum :value val)))
    (assert-equal 1 (length (cl-cc:ast-children node)))
    (assert-eq val (first (cl-cc:ast-children node))))
  ;; defvar with value: one child
  (let* ((val (cl-cc:make-ast-int :value 0))
         (node (cl-cc/ast::make-ast-defvar :name '*x* :value val)))
    (assert-equal 1 (length (cl-cc:ast-children node))))
  ;; defvar without value: no children
  (let ((node (cl-cc/ast::make-ast-defvar :name '*x*)))
    (assert-null (cl-cc:ast-children node))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; ast-bound-names — scoping data layer
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each ast-bound-names-binding-forms
  "ast-bound-names returns the names introduced by each binding form."
  :cases
  (("let"
    (cl-cc:make-ast-let :bindings (list (cons 'x (cl-cc:make-ast-int :value 1))
                                        (cons 'y (cl-cc:make-ast-int :value 2)))
                        :body (list (cl-cc:make-ast-var :name 'x)))
    '(x y))
   ("lambda-required"
    (cl-cc:make-ast-lambda :params '(a b) :body (list (cl-cc:make-ast-var :name 'a)))
    '(a b))
   ("lambda-optional"
    (cl-cc:make-ast-lambda :params '(a)
                           :optional-params '((b nil))
                           :body (list (cl-cc:make-ast-var :name 'a)))
    '(a b))
   ("lambda-rest"
    (cl-cc:make-ast-lambda :params '(a)
                           :rest-param 'rest
                           :body (list (cl-cc:make-ast-var :name 'a)))
    '(a rest))
   ("defun"
    (cl-cc/ast::make-ast-defun :name 'foo :params '(x y)
                           :body (list (cl-cc:make-ast-var :name 'x)))
    '(x y))
   ("flet"
    (cl-cc:make-ast-flet :bindings (list (list 'f '(x) (cl-cc:make-ast-var :name 'x)))
                         :body (list (cl-cc:make-ast-int :value 1)))
    '(f))
   ("labels"
    (cl-cc:make-ast-labels :bindings (list (list 'g '(x) (cl-cc:make-ast-var :name 'x)))
                           :body (list (cl-cc:make-ast-int :value 1)))
    '(g))
   ("mvb"
    (cl-cc:make-ast-multiple-value-bind
     :vars '(a b c)
     :values-form (cl-cc:make-ast-values :forms (list (cl-cc:make-ast-int :value 1)))
     :body (list (cl-cc:make-ast-var :name 'a)))
    '(a b c)))
  (node expected)
  (assert-equal expected (cl-cc:ast-bound-names node)))

(deftest ast-bound-names-non-binding
  "Non-binding nodes return nil."
  (assert-null (cl-cc:ast-bound-names (cl-cc:make-ast-int :value 42)))
  (assert-null (cl-cc:ast-bound-names (cl-cc:make-ast-if
                                       :cond (cl-cc:make-ast-int :value 1)
                                       :then (cl-cc:make-ast-int :value 2)
                                       :else (cl-cc:make-ast-int :value 3)))))
