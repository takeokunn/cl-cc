(in-package :cl-cc/test)

(in-suite cl-cc-unit-suite)

;;; Type Inference Tests

(deftest-each infer-forms-return-type-int
  "Forms that always infer to type-int via infer-with-env."
  :cases (("integer-literal"    '42)
          ("binop-addition"     '(+ 1 2))
          ("binop-nested"       '(+ (* 2 3) (- 4 1)))
          ("let-simple"         '(let ((x 42)) x))
          ("let-multi-binop"    '(let ((x 10) (y 20)) (+ x y)))
          ("function-call"      '(let ((f (lambda (x) (+ x 1)))) (f 5)))
          ("print-expr"         '(print 42))
          ("progn-last"         '(progn 1 2 3))
          ("let-poly-identity"  '(let ((id (lambda (x) x))) (id 42))))
  (form)
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast form)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty type-int))))

(deftest-each infer-env-and-control-flow
  "Variable env lookup infers declared type; if-expr unifies branches when condition is bool."
  :cases (("variable-from-env"
           (lambda ()
             (let* ((ast (lower-sexp-to-ast 'x))
                    (env (type-env-extend 'x (type-to-scheme type-int) (type-env-empty))))
               (multiple-value-bind (ty subst) (infer ast env)
                 (declare (ignore subst))
                 (assert-type-equal ty type-int)))))
          ("if-expression"
           (lambda ()
             (let* ((ast (lower-sexp-to-ast '(if cond-var 1 2)))
                    (env (type-env-extend 'cond-var
                                          (type-to-scheme type-bool)
                                          (type-env-empty))))
               (multiple-value-bind (ty subst) (infer ast env)
                 (declare (ignore subst))
                 (assert-type-equal ty type-int))))))
  (verify)
  (reset-type-vars!)
  (funcall verify))

(deftest-each infer-type-error-signals
  "Unbound variables signal unbound-variable-error; typed holes signal typed-hole-error."
  :cases (("unbound-var"
           (lambda ()
             (let ((ast (lower-sexp-to-ast 'undefined-var)))
               (assert-signals unbound-variable-error
                 (infer-with-env ast)))))
          ("typed-hole"
           (lambda ()
             (let* ((ast (lower-sexp-to-ast '(+ x _)))
                    (env (type-env-extend 'x (type-to-scheme type-int) (type-env-empty))))
               (assert-signals cl-cc/type::typed-hole-error
                 (infer ast env))))))
  (verify)
  (reset-type-vars!)
  (funcall verify))

(deftest infer-lambda
  "Lambda types inferred as function type: identity has 1 param; arithmetic constrains to int."
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(lambda (x) x))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type type-function ty)
      (assert-= 1 (length (type-function-params ty)))))
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(lambda (x) (+ x 1)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type type-function ty)
      (assert-type-equal (type-function-return ty) type-int)
      (assert-type-equal (first (type-function-params ty)) type-int))))


(deftest-each infer-quote-type
  "Quoted forms infer to the type corresponding to their datum."
  :cases (("symbol"  '(quote hello) type-symbol)
          ("integer" '(quote 42)    type-int))
  (form expected-type)
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast form)))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type-equal ty expected-type))))

;;; Generalization / Instantiation Tests

(deftest-each generalize-and-scheme-ops
  "Generalization: nil env quantifies all free vars; non-nil env excludes env vars. Scheme: instantiate/monomorphic."
  :cases (("nil-env-quantifies-all"
           (lambda ()
             (let* ((v  (make-type-variable 'a))
                    (fn (make-type-function-raw :params (list v) :return v))
                    (s  (generalize nil fn)))
               (assert-type type-scheme s)
               (assert-= 1 (length (type-scheme-quantified-vars s))))))
          ("non-nil-env-excludes"
           (lambda ()
             (let* ((v1   (make-type-variable 'a))
                    (v2   (make-type-variable 'b))
                    (fn   (make-type-function-raw :params (list v1) :return v2))
                    (env  (list (cons 'x v1)))
                    (s    (generalize env fn)))
               (assert-= 1 (length (type-scheme-quantified-vars s)))
               (assert-true (type-variable-equal-p (first (type-scheme-quantified-vars s)) v2)))))
          ("instantiate-fresh-vars"
           (lambda ()
             (let* ((v (make-type-variable 'a))
                    (fn-type (make-type-function-raw :params (list v) :return v))
                    (scheme (make-type-scheme (list v) fn-type))
                    (inst (instantiate scheme)))
               (assert-type type-function inst)
               (let ((new-param (first (type-function-params inst)))
                     (new-ret (type-function-return inst)))
                 (assert-type type-variable new-param)
                 (assert-false (type-variable-equal-p new-param v))
                 (assert-true (type-variable-equal-p new-param new-ret))))))
          ("monomorphic-scheme"
           (lambda ()
             (let ((scheme (type-to-scheme type-int)))
               (assert-type type-scheme scheme)
               (assert-null (type-scheme-quantified-vars scheme))
               (assert-type-equal (type-scheme-type scheme) type-int)))))
  (verify)
  (funcall verify))
