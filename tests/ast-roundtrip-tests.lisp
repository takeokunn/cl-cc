(in-package :cl-cc/test)

(in-suite cl-cc-suite)

(test ast-int-roundtrip
  (let* ((ast (make-instance 'cl-cc:ast-int :value 42))
         (sexp (cl-cc:ast-to-sexp ast))
         (ast2 (cl-cc:lower-sexp-to-ast sexp)))
    (is (= 42 (cl-cc:ast-int-value ast2)))))

(test ast-var-roundtrip
  (let* ((ast (make-instance 'cl-cc:ast-var :name 'x))
         (sexp (cl-cc:ast-to-sexp ast))
         (ast2 (cl-cc:lower-sexp-to-ast sexp)))
    (is (eq 'x (cl-cc:ast-var-name ast2)))))

(test ast-binop-roundtrip
  (let* ((ast (make-instance 'cl-cc:ast-binop :op '+
                             :lhs (make-instance 'cl-cc:ast-int :value 1)
                             :rhs (make-instance 'cl-cc:ast-int :value 2)))
         (sexp (cl-cc:ast-to-sexp ast))
         (ast2 (cl-cc:lower-sexp-to-ast sexp)))
    (is (eq '+ (cl-cc:ast-binop-op ast2)))))

(test ast-if-roundtrip
  (let* ((ast (make-instance 'cl-cc:ast-if
                             :cond (make-instance 'cl-cc:ast-int :value 1)
                             :then (make-instance 'cl-cc:ast-int :value 2)
                             :else (make-instance 'cl-cc:ast-int :value 3)))
         (sexp (cl-cc:ast-to-sexp ast))
         (ast2 (cl-cc:lower-sexp-to-ast sexp)))
    (is (typep ast2 'cl-cc:ast-if))))

(test ast-progn-roundtrip
  (let* ((ast (make-instance 'cl-cc:ast-progn
                             :forms (list (make-instance 'cl-cc:ast-int :value 1)
                                          (make-instance 'cl-cc:ast-int :value 2))))
         (sexp (cl-cc:ast-to-sexp ast))
         (ast2 (cl-cc:lower-sexp-to-ast sexp)))
    (is (= 2 (length (cl-cc:ast-progn-forms ast2))))))

(test ast-let-roundtrip
  (let* ((ast (make-instance 'cl-cc:ast-let
                             :bindings (list (cons 'x (make-instance 'cl-cc:ast-int :value 1)))
                             :body (list (make-instance 'cl-cc:ast-var :name 'x))))
         (sexp (cl-cc:ast-to-sexp ast))
         (ast2 (cl-cc:lower-sexp-to-ast sexp)))
    (is (= 1 (length (cl-cc:ast-let-bindings ast2))))))

(test ast-lambda-roundtrip
  (let* ((ast (make-instance 'cl-cc:ast-lambda
                             :params (list 'x)
                             :body (list (make-instance 'cl-cc:ast-var :name 'x))))
         (sexp (cl-cc:ast-to-sexp ast))
         (ast2 (cl-cc:lower-sexp-to-ast sexp)))
    (is (= 1 (length (cl-cc:ast-lambda-params ast2))))))

(test ast-setq-roundtrip
  (let* ((ast (make-instance 'cl-cc:ast-setq
                             :var 'x
                             :value (make-instance 'cl-cc:ast-int :value 42)))
         (sexp (cl-cc:ast-to-sexp ast))
         (ast2 (cl-cc:lower-sexp-to-ast sexp)))
    (is (eq 'x (cl-cc:ast-setq-var ast2)))))

(test ast-quote-roundtrip
  (let* ((ast (make-instance 'cl-cc:ast-quote
                             :value '(x y)))
         (sexp (cl-cc:ast-to-sexp ast))
         (ast2 (cl-cc:lower-sexp-to-ast sexp)))
    (is (typep ast2 'cl-cc:ast-quote))))
