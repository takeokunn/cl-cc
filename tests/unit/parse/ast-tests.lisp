;;;; tests/unit/parse/ast-tests.lisp — AST Node Roundtrip Tests
;;;;
;;;; Verifies that ast-to-sexp and lower-sexp-to-ast are inverses:
;;;;   AST -> sexp -> AST preserves structural identity.
;;;;
;;;; Each case provides (ast-node verify-fn) where verify-fn checks
;;;; the relevant structural property of the reconstructed node.

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Helper: shared roundtrip combinator
;;; ─────────────────────────────────────────────────────────────────────────

(defun %ast-roundtrip (ast)
  "Convert AST → sexp → AST and return the reconstructed node."
  (cl-cc:lower-sexp-to-ast (cl-cc:ast-to-sexp ast)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Primitive nodes
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each ast-roundtrip-primitives
  "Primitive AST node types survive a full serialization cycle"
  :cases
  (("int"
    (cl-cc:make-ast-int :value 42)
    (lambda (ast2) (assert-= 42 (cl-cc:ast-int-value ast2))))

   ("var"
    (cl-cc:make-ast-var :name 'x)
    (lambda (ast2) (assert-eq 'x (cl-cc:ast-var-name ast2))))

   ("quote-atom"
    (cl-cc:make-ast-quote :value 'hello)
    (lambda (ast2) (assert-type cl-cc:ast-quote ast2)))

   ("quote-list"
    (cl-cc:make-ast-quote :value '(x y))
    (lambda (ast2) (assert-type cl-cc:ast-quote ast2))))
  (ast verify)
  (funcall verify (%ast-roundtrip ast)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Expression nodes
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each ast-roundtrip-expressions
  "Expression AST node types survive a full serialization cycle"
  :cases
  (("binop"
    (cl-cc:make-ast-binop :op '+
                          :lhs (cl-cc:make-ast-int :value 1)
                          :rhs (cl-cc:make-ast-int :value 2))
    (lambda (ast2) (assert-eq '+ (cl-cc:ast-binop-op ast2))))

   ("if"
    (cl-cc:make-ast-if :cond (cl-cc:make-ast-int :value 1)
                       :then (cl-cc:make-ast-int :value 2)
                       :else (cl-cc:make-ast-int :value 3))
    (lambda (ast2) (assert-type cl-cc:ast-if ast2)))

   ("progn"
    (cl-cc:make-ast-progn :forms (list (cl-cc:make-ast-int :value 1)
                                       (cl-cc:make-ast-int :value 2)))
    (lambda (ast2) (assert-= 2 (length (cl-cc:ast-progn-forms ast2)))))

   ("setq"
    (cl-cc:make-ast-setq :var 'x :value (cl-cc:make-ast-int :value 42))
    (lambda (ast2) (assert-eq 'x (cl-cc:ast-setq-var ast2)))))
  (ast verify)
  (funcall verify (%ast-roundtrip ast)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Binding nodes
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each ast-roundtrip-bindings
  "Binding AST node types survive a full serialization cycle"
  :cases
  (("let"
    (cl-cc:make-ast-let :bindings (list (cons 'x (cl-cc:make-ast-int :value 1)))
                        :body     (list (cl-cc:make-ast-var :name 'x)))
    (lambda (ast2) (assert-= 1 (length (cl-cc:ast-let-bindings ast2)))))

   ("lambda"
    (cl-cc:make-ast-lambda :params (list 'x)
                           :body   (list (cl-cc:make-ast-var :name 'x)))
    (lambda (ast2) (assert-= 1 (length (cl-cc:ast-lambda-params ast2)))))

   ("flet"
    (cl-cc:make-ast-flet
     :bindings (list (list 'double '(x)
                           (cl-cc:make-ast-binop
                            :op '* :lhs (cl-cc:make-ast-int :value 2)
                            :rhs (cl-cc:make-ast-var :name 'x))))
     :body (list (cl-cc:make-ast-int :value 1)))
    (lambda (ast2) (assert-= 1 (length (cl-cc:ast-flet-bindings ast2)))))

   ("labels"
    (cl-cc:make-ast-labels
     :bindings (list (list 'id '(x) (cl-cc:make-ast-var :name 'x)))
     :body (list (cl-cc:make-ast-int :value 1)))
    (lambda (ast2) (assert-= 1 (length (cl-cc:ast-labels-bindings ast2))))))
  (ast verify)
  (funcall verify (%ast-roundtrip ast)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Control flow nodes
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each ast-roundtrip-control-flow
  "Control flow AST node types survive a full serialization cycle"
  :cases
  (("block"
    (cl-cc:make-ast-block :name 'loop
                          :body (list (cl-cc:make-ast-int :value 1)))
    (lambda (ast2) (assert-type cl-cc:ast-block ast2)))

   ("return-from"
    (cl-cc:make-ast-return-from :name 'loop
                                :value (cl-cc:make-ast-int :value 42))
    (lambda (ast2) (assert-eq 'loop (cl-cc:ast-return-from-name ast2))))

   ("tagbody"
    (cl-cc:make-ast-tagbody :tags (list (cons 'start (list (cl-cc:make-ast-int :value 1)))))
    (lambda (ast2) (assert-type cl-cc:ast-tagbody ast2)))

   ("go"
    (cl-cc:make-ast-go :tag 'start)
    (lambda (ast2) (assert-eq 'start (cl-cc:ast-go-tag ast2)))))
  (ast verify)
  (funcall verify (%ast-roundtrip ast)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Exception handling nodes
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each ast-roundtrip-exceptions
  "Exception handling AST node types survive a full serialization cycle"
  :cases
  (("catch"
    (cl-cc:make-ast-catch :tag  (cl-cc:make-ast-var :name 'my-tag)
                          :body (list (cl-cc:make-ast-int :value 42)))
    (lambda (ast2) (assert-type cl-cc:ast-catch ast2)))

   ("throw"
    (cl-cc:make-ast-throw :tag   (cl-cc:make-ast-var :name 'my-tag)
                          :value (cl-cc:make-ast-int :value 42))
    (lambda (ast2) (assert-type cl-cc:ast-throw ast2)))

   ("unwind-protect"
    (cl-cc:make-ast-unwind-protect :protected (cl-cc:make-ast-int :value 1)
                                   :cleanup   (list (cl-cc:make-ast-int :value 0)))
    (lambda (ast2) (assert-type cl-cc:ast-unwind-protect ast2))))
  (ast verify)
  (funcall verify (%ast-roundtrip ast)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Multiple-values nodes
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each ast-roundtrip-multiple-values
  "Multiple-values AST node types survive a full serialization cycle"
  :cases
  (("values"
    (cl-cc:make-ast-values :forms (list (cl-cc:make-ast-int :value 1)
                                        (cl-cc:make-ast-int :value 2)))
    (lambda (ast2) (assert-= 2 (length (cl-cc:ast-values-forms ast2)))))

   ("multiple-value-bind"
    (cl-cc:make-ast-multiple-value-bind
     :vars        '(a b)
     :values-form (cl-cc:make-ast-values :forms (list (cl-cc:make-ast-int :value 1)
                                                      (cl-cc:make-ast-int :value 2)))
     :body        (list (cl-cc:make-ast-var :name 'a)))
    (lambda (ast2) (assert-= 2 (length (cl-cc:ast-mvb-vars ast2))))))
  (ast verify)
  (funcall verify (%ast-roundtrip ast)))

;;; ─────────────────────────────────────────────────────────────────────────
;;; Source location utility
;;; ─────────────────────────────────────────────────────────────────────────

(deftest ast-location-string-full
  "ast-location-string returns file:line:col when all slots are present"
  (let ((node (cl-cc:make-ast-int :value 1
                                  :source-file "foo.lisp"
                                  :source-line 10
                                  :source-column 5)))
    (assert-equal "foo.lisp:10:5" (cl-cc:ast-location-string node))))

(deftest ast-location-string-file-line
  "ast-location-string returns file:line when column is absent"
  (let ((node (cl-cc:make-ast-int :value 1
                                  :source-file "foo.lisp"
                                  :source-line 3)))
    (assert-equal "foo.lisp:3" (cl-cc:ast-location-string node))))

(deftest ast-location-string-unknown
  "ast-location-string returns <unknown location> when no slots are set"
  (let ((node (cl-cc:make-ast-int :value 1)))
    (assert-equal "<unknown location>" (cl-cc:ast-location-string node))))

(deftest ast-error-signals-condition
  "ast-error signals ast-compilation-error with location from node"
  (let ((node (cl-cc:make-ast-int :value 1 :source-file "t.lisp" :source-line 1)))
    (assert-signals cl-cc:ast-compilation-error
      (cl-cc:ast-error node "test error ~A" 42))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; ast-children — structural data layer
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each ast-children-leaves
  "Leaf AST nodes have no children."
  :cases (("int"      (cl-cc:make-ast-int :value 42))
          ("var"      (cl-cc:make-ast-var :name 'x))
          ("quote"    (cl-cc:make-ast-quote :value 'hello))
          ("function" (cl-cc:make-ast-function :name 'foo))
          ("go"       (cl-cc:make-ast-go :tag 'start)))
  (node)
  (assert-null (cl-cc:ast-children node)))

(deftest ast-children-binop
  "ast-binop children are (lhs rhs)."
  (let* ((lhs (cl-cc:make-ast-int :value 1))
         (rhs (cl-cc:make-ast-int :value 2))
         (node (cl-cc:make-ast-binop :op '+ :lhs lhs :rhs rhs))
         (children (cl-cc:ast-children node)))
    (assert-equal 2 (length children))
    (assert-eq lhs (first children))
    (assert-eq rhs (second children))))

(deftest ast-children-if
  "ast-if children are (cond then else)."
  (let* ((c (cl-cc:make-ast-int :value 1))
         (th (cl-cc:make-ast-int :value 2))
         (el (cl-cc:make-ast-int :value 3))
         (node (cl-cc:make-ast-if :cond c :then th :else el)))
    (assert-equal 3 (length (cl-cc:ast-children node)))))

(deftest ast-children-progn
  "ast-progn children are its forms."
  (let* ((f1 (cl-cc:make-ast-int :value 1))
         (f2 (cl-cc:make-ast-int :value 2))
         (node (cl-cc:make-ast-progn :forms (list f1 f2))))
    (assert-equal 2 (length (cl-cc:ast-children node)))))

(deftest ast-children-let
  "ast-let children include init-exprs and body."
  (let* ((init (cl-cc:make-ast-int :value 1))
         (body (cl-cc:make-ast-var :name 'x))
         (node (cl-cc:make-ast-let :bindings (list (cons 'x init))
                                   :body (list body)))
         (children (cl-cc:ast-children node)))
    (assert-equal 2 (length children))
    (assert-true (member init children :test #'eq))
    (assert-true (member body children :test #'eq))))

(deftest ast-children-lambda
  "ast-lambda children are body forms."
  (let* ((body (cl-cc:make-ast-var :name 'x))
         (node (cl-cc:make-ast-lambda :params '(x) :body (list body))))
    (assert-equal 1 (length (cl-cc:ast-children node)))))

(deftest ast-children-setq
  "ast-setq child is the value expression."
  (let* ((val (cl-cc:make-ast-int :value 42))
         (node (cl-cc:make-ast-setq :var 'x :value val)))
    (assert-equal 1 (length (cl-cc:ast-children node)))
    (assert-eq val (first (cl-cc:ast-children node)))))

(deftest ast-children-call
  "ast-call children are func (if AST) + args."
  (let* ((arg1 (cl-cc:make-ast-int :value 1))
         (arg2 (cl-cc:make-ast-int :value 2))
         (node (cl-cc:make-ast-call :func 'foo :args (list arg1 arg2))))
    (assert-equal 2 (length (cl-cc:ast-children node))))
  ;; With AST func
  (let* ((func (cl-cc:make-ast-var :name 'f))
         (arg1 (cl-cc:make-ast-int :value 1))
         (node (cl-cc:make-ast-call :func func :args (list arg1))))
    (assert-equal 2 (length (cl-cc:ast-children node)))
    (assert-eq func (first (cl-cc:ast-children node)))))

(deftest ast-children-block
  "ast-block children are body forms."
  (let* ((body (cl-cc:make-ast-int :value 1))
         (node (cl-cc:make-ast-block :name 'b :body (list body))))
    (assert-equal 1 (length (cl-cc:ast-children node)))))

(deftest ast-children-catch
  "ast-catch children are tag + body."
  (let* ((tag (cl-cc:make-ast-var :name 'tag))
         (body (cl-cc:make-ast-int :value 1))
         (node (cl-cc:make-ast-catch :tag tag :body (list body))))
    (assert-equal 2 (length (cl-cc:ast-children node)))
    (assert-eq tag (first (cl-cc:ast-children node)))))

(deftest ast-children-throw
  "ast-throw children are (tag value)."
  (let* ((tag (cl-cc:make-ast-var :name 'tag))
         (val (cl-cc:make-ast-int :value 42))
         (node (cl-cc:make-ast-throw :tag tag :value val)))
    (assert-equal 2 (length (cl-cc:ast-children node)))))

(deftest ast-children-the
  "ast-the child is the value expression."
  (let* ((val (cl-cc:make-ast-int :value 1))
         (node (cl-cc:make-ast-the :type 'fixnum :value val)))
    (assert-equal 1 (length (cl-cc:ast-children node)))
    (assert-eq val (first (cl-cc:ast-children node)))))

(deftest ast-children-defvar-with-value
  "ast-defvar with value has one child."
  (let* ((val (cl-cc:make-ast-int :value 0))
         (node (cl-cc::make-ast-defvar :name '*x* :value val)))
    (assert-equal 1 (length (cl-cc:ast-children node)))))

(deftest ast-children-defvar-no-value
  "ast-defvar without value has no children."
  (let ((node (cl-cc::make-ast-defvar :name '*x*)))
    (assert-null (cl-cc:ast-children node))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; ast-bound-names — scoping data layer
;;; ─────────────────────────────────────────────────────────────────────────

(deftest ast-bound-names-let
  "ast-let binds variable names from bindings."
  (let ((node (cl-cc:make-ast-let :bindings (list (cons 'x (cl-cc:make-ast-int :value 1))
                                                  (cons 'y (cl-cc:make-ast-int :value 2)))
                                  :body (list (cl-cc:make-ast-var :name 'x)))))
    (assert-equal '(x y) (cl-cc:ast-bound-names node))))

(deftest ast-bound-names-lambda
  "ast-lambda binds required params."
  (let ((node (cl-cc:make-ast-lambda :params '(a b) :body (list (cl-cc:make-ast-var :name 'a)))))
    (assert-equal '(a b) (cl-cc:ast-bound-names node))))

(deftest ast-bound-names-lambda-optional
  "ast-lambda with optional params includes them."
  (let ((node (cl-cc:make-ast-lambda :params '(a)
                                     :optional-params '((b nil))
                                     :body (list (cl-cc:make-ast-var :name 'a)))))
    (assert-equal '(a b) (cl-cc:ast-bound-names node))))

(deftest ast-bound-names-lambda-rest
  "ast-lambda with rest param includes it."
  (let ((node (cl-cc:make-ast-lambda :params '(a)
                                     :rest-param 'rest
                                     :body (list (cl-cc:make-ast-var :name 'a)))))
    (assert-equal '(a rest) (cl-cc:ast-bound-names node))))

(deftest ast-bound-names-defun
  "ast-defun binds params."
  (let ((node (cl-cc::make-ast-defun :name 'foo :params '(x y) :body (list (cl-cc:make-ast-var :name 'x)))))
    (assert-equal '(x y) (cl-cc:ast-bound-names node))))

(deftest ast-bound-names-flet
  "ast-flet binds function names."
  (let ((node (cl-cc:make-ast-flet :bindings (list (list 'f '(x) (cl-cc:make-ast-var :name 'x)))
                                   :body (list (cl-cc:make-ast-int :value 1)))))
    (assert-equal '(f) (cl-cc:ast-bound-names node))))

(deftest ast-bound-names-labels
  "ast-labels binds function names."
  (let ((node (cl-cc:make-ast-labels :bindings (list (list 'g '(x) (cl-cc:make-ast-var :name 'x)))
                                     :body (list (cl-cc:make-ast-int :value 1)))))
    (assert-equal '(g) (cl-cc:ast-bound-names node))))

(deftest ast-bound-names-mvb
  "ast-multiple-value-bind binds variable names."
  (let ((node (cl-cc:make-ast-multiple-value-bind
               :vars '(a b c)
               :values-form (cl-cc:make-ast-values :forms (list (cl-cc:make-ast-int :value 1)))
               :body (list (cl-cc:make-ast-var :name 'a)))))
    (assert-equal '(a b c) (cl-cc:ast-bound-names node))))

(deftest ast-bound-names-non-binding
  "Non-binding nodes return nil."
  (assert-null (cl-cc:ast-bound-names (cl-cc:make-ast-int :value 42)))
  (assert-null (cl-cc:ast-bound-names (cl-cc:make-ast-if
                                       :cond (cl-cc:make-ast-int :value 1)
                                       :then (cl-cc:make-ast-int :value 2)
                                       :else (cl-cc:make-ast-int :value 3)))))
