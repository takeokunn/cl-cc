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
