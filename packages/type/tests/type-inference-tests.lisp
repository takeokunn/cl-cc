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
      (assert-type type-arrow ty)
      (assert-= 1 (length (type-arrow-params ty)))))
  (reset-type-vars!)
  (let ((ast (lower-sexp-to-ast '(lambda (x) (+ x 1)))))
    (multiple-value-bind (ty subst) (infer-with-env ast)
      (declare (ignore subst))
      (assert-type type-arrow ty)
      (assert-type-equal (type-arrow-return ty) type-int)
      (assert-type-equal (first (type-arrow-params ty)) type-int))))


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

;;; ─── syntactic-value-p (value restriction) ──────────────────────────────────

(deftest-each infer-syntactic-value-p-truthy
  "Syntactic values: int, var, lambda, quote, function-ref, typed-hole are generalizable."
  :cases (("int"       (cl-cc/ast::make-ast-int      :value 42))
          ("var"       (cl-cc/ast::make-ast-var       :name 'x))
          ("lambda"    (cl-cc/ast::make-ast-lambda    :params '(x) :body nil))
          ("quote"     (cl-cc/ast::make-ast-quote     :value 'foo))
          ("function"  (cl-cc/ast::make-ast-function  :name 'f))
          ("hole"      (cl-cc/ast::make-ast-hole)))
  (ast)
  (assert-true (cl-cc/type::syntactic-value-p ast)))

(deftest-each infer-syntactic-value-p-falsy
  "Non-syntactic values: call, binop, if, let, progn are not generalizable (value restriction)."
  :cases (("call"   (cl-cc/ast::make-ast-call   :func 'f :args nil))
          ("binop"  (cl-cc/ast::make-ast-binop  :op '+ :lhs (cl-cc/ast::make-ast-int :value 1)
                                                        :rhs (cl-cc/ast::make-ast-int :value 2)))
          ("if"     (cl-cc/ast::make-ast-if     :cond (cl-cc/ast::make-ast-var :name 'c)
                                                 :then (cl-cc/ast::make-ast-int :value 1)
                                                 :else (cl-cc/ast::make-ast-int :value 2)))
          ("let"    (cl-cc/ast::make-ast-let    :bindings nil :body nil))
          ("progn"  (cl-cc/ast::make-ast-progn  :forms nil)))
  (ast)
  (assert-false (cl-cc/type::syntactic-value-p ast)))

;;; ─── infer-if type narrowing ──────────────────────────────────────────────────

(deftest infer-if-narrows-type-in-then-branch
  "infer-if narrows the guard variable's type to the predicate type in the then branch."
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast '(if (numberp x) (+ x 1) 0)))
         (env (type-env-extend 'x
               (make-type-scheme nil
                 (make-type-union (list type-int type-string)))
               (type-env-empty))))
    (multiple-value-bind (ty subst) (infer ast env)
      (declare (ignore subst))
      (assert-true ty))))

(deftest infer-if-no-narrowing-without-predicate
  "infer-if with a plain boolean condition (no predicate call) leaves types unchanged."
  (reset-type-vars!)
  (let* ((ast (lower-sexp-to-ast '(if flag 1 2)))
         (env (type-env-extend 'flag (type-to-scheme type-bool) (type-env-empty))))
    (multiple-value-bind (ty subst) (infer ast env)
      (declare (ignore subst))
      (assert-type-equal ty type-int))))

;;; Generalization / Instantiation Tests

(deftest-each generalize-and-scheme-ops
  "Generalization: nil env quantifies all free vars; non-nil env excludes env vars. Scheme: instantiate/monomorphic."
  :cases (("nil-env-quantifies-all"
           (lambda ()
             (let* ((v  (fresh-type-var 'a))
                    (fn (make-type-arrow-raw :params (list v) :return v))
                    (s  (generalize nil fn)))
               (assert-type type-scheme s)
               (assert-= 1 (length (type-scheme-quantified-vars s))))))
          ("non-nil-env-excludes"
           (lambda ()
              (let* ((v1   (fresh-type-var 'a))
                     (v2   (fresh-type-var 'b))
                     (fn   (make-type-arrow-raw :params (list v1) :return v2))
                     (env  (type-env-extend 'x (type-to-scheme v1) (type-env-empty)))
                     (s    (generalize env fn)))
                (assert-= 1 (length (type-scheme-quantified-vars s)))
               (assert-true (type-var-equal-p (first (type-scheme-quantified-vars s)) v2)))))
          ("instantiate-fresh-vars"
           (lambda ()
             (let* ((v (fresh-type-var 'a))
                    (fn-type (make-type-arrow-raw :params (list v) :return v))
                    (scheme (make-type-scheme (list v) fn-type))
                    (inst (instantiate scheme)))
               (assert-type type-arrow inst)
               (let ((new-param (first (type-arrow-params inst)))
                     (new-ret (type-arrow-return inst)))
                 (assert-type type-var new-param)
                 (assert-false (type-var-equal-p new-param v))
                 (assert-true (type-var-equal-p new-param new-ret))))))
          ("monomorphic-scheme"
           (lambda ()
             (let ((scheme (type-to-scheme type-int)))
               (assert-type type-scheme scheme)
               (assert-null (type-scheme-quantified-vars scheme))
               (assert-type-equal (type-scheme-type scheme) type-int)))))
  (verify)
  (funcall verify))
