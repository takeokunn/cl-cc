(in-package :cl-cc/test)

(in-suite cl-cc-suite)

(deftest ast-int-roundtrip
  (let* ((ast (cl-cc:make-ast-int :value 42))
         (sexp (cl-cc:ast-to-sexp ast))
         (ast2 (cl-cc:lower-sexp-to-ast sexp)))
    (assert-= 42 (cl-cc:ast-int-value ast2))))

(deftest ast-var-roundtrip
  (let* ((ast (cl-cc:make-ast-var :name 'x))
         (sexp (cl-cc:ast-to-sexp ast))
         (ast2 (cl-cc:lower-sexp-to-ast sexp)))
    (assert-eq 'x (cl-cc:ast-var-name ast2))))

(deftest ast-binop-roundtrip
  (let* ((ast (cl-cc:make-ast-binop :op '+
                             :lhs (cl-cc:make-ast-int :value 1)
                             :rhs (cl-cc:make-ast-int :value 2)))
         (sexp (cl-cc:ast-to-sexp ast))
         (ast2 (cl-cc:lower-sexp-to-ast sexp)))
    (assert-eq '+ (cl-cc:ast-binop-op ast2))))

(deftest ast-if-roundtrip
  (let* ((ast (cl-cc:make-ast-if
                             :cond (cl-cc:make-ast-int :value 1)
                             :then (cl-cc:make-ast-int :value 2)
                             :else (cl-cc:make-ast-int :value 3)))
         (sexp (cl-cc:ast-to-sexp ast))
         (ast2 (cl-cc:lower-sexp-to-ast sexp)))
    (assert-type cl-cc:ast-if ast2)))

(deftest ast-progn-roundtrip
  (let* ((ast (cl-cc:make-ast-progn
                             :forms (list (cl-cc:make-ast-int :value 1)
                                          (cl-cc:make-ast-int :value 2))))
         (sexp (cl-cc:ast-to-sexp ast))
         (ast2 (cl-cc:lower-sexp-to-ast sexp)))
    (assert-= 2 (length (cl-cc:ast-progn-forms ast2)))))

(deftest ast-let-roundtrip
  (let* ((ast (cl-cc:make-ast-let
                             :bindings (list (cons 'x (cl-cc:make-ast-int :value 1)))
                             :body (list (cl-cc:make-ast-var :name 'x))))
         (sexp (cl-cc:ast-to-sexp ast))
         (ast2 (cl-cc:lower-sexp-to-ast sexp)))
    (assert-= 1 (length (cl-cc:ast-let-bindings ast2)))))

(deftest ast-lambda-roundtrip
  (let* ((ast (cl-cc:make-ast-lambda
                             :params (list 'x)
                             :body (list (cl-cc:make-ast-var :name 'x))))
         (sexp (cl-cc:ast-to-sexp ast))
         (ast2 (cl-cc:lower-sexp-to-ast sexp)))
    (assert-= 1 (length (cl-cc:ast-lambda-params ast2)))))

(deftest ast-setq-roundtrip
  (let* ((ast (cl-cc:make-ast-setq
                             :var 'x
                             :value (cl-cc:make-ast-int :value 42)))
         (sexp (cl-cc:ast-to-sexp ast))
         (ast2 (cl-cc:lower-sexp-to-ast sexp)))
    (assert-eq 'x (cl-cc:ast-setq-var ast2))))

(deftest ast-quote-roundtrip
  (let* ((ast (cl-cc:make-ast-quote
                             :value '(x y)))
         (sexp (cl-cc:ast-to-sexp ast))
         (ast2 (cl-cc:lower-sexp-to-ast sexp)))
    (assert-type cl-cc:ast-quote ast2)))
