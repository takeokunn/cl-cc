;;;; tests/unit/core/cps-tests.lisp — CPS Transformation tests
;;;;
;;;; Three layers of tests:
;;;;   1. S-expression CPS (bootstrap transformer) — semantic evaluation
;;;;   2. AST CPS — semantic evaluation where the form is evaluable
;;;;   3. AST CPS — structural "is a CPS lambda?" for non-evaluable forms
;;;;      (tagbody/go/throw/block/return-from don't return values cleanly)
;;;;
;;;; Uses deftest-each to group the structural-shape tests.

(in-package :cl-cc/test)

(in-suite cl-cc-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; Helpers
;;; ─────────────────────────────────────────────────────────────────────────

(defun eval-cps-ast (ast)
  "CPS-transform an AST node to a (lambda (k) ...) sexp and evaluate it.
Returns a function that takes a continuation."
  (eval (cl-cc:cps-transform-ast* ast)))

(defun run-cps-ast (ast)
  "Run CPS-transformed AST with the identity continuation."
  (funcall (eval-cps-ast ast) #'identity))

(defun is-cps-lambda (result)
  "Return t if RESULT is a (lambda (k) ...) sexp as produced by cps-transform-ast*."
  (and (listp result)
       (eq 'lambda (car result))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; S-expression CPS (bootstrap transformer)
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cps-sexp-transform
  "S-expression CPS transformer: evaluate with identity continuation"
  :cases (("integer"           '42               42)
          ("add"               '(+ 1 2)           3)
          ("sub"               '(- 10 3)          7)
          ("mul"               '(* 3 4)          12)
          ("if-true"           '(if 1 10 20)     10)
          ("if-false"          '(if 0 10 20)     20)
          ("progn-returns-last" '(progn 1 2 3)    3)
          ("let-binding"       '(let ((x 1) (y 2)) (+ x y)) 3))
  (expr expected)
  (let ((fn (cl-cc:cps-transform-eval expr)))
    (assert-equal expected (funcall fn #'identity))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; AST CPS — semantic (evaluable forms)
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-ast-integer
  "cps-transform-ast: integer node evaluates to its value"
  (assert-= 42 (run-cps-ast (cl-cc:make-ast-int :value 42))))

(deftest-each cps-ast-binop
  "cps-transform-ast: binary arithmetic operations evaluate correctly"
  :cases (("add"      '+ 3  4  7)
          ("sub"      '- 9  4  5)
          ("mul"      '* 3  4 12))
  (op lhs rhs expected)
  (let ((ast (cl-cc:make-ast-binop :op op
                                   :lhs (cl-cc:make-ast-int :value lhs)
                                   :rhs (cl-cc:make-ast-int :value rhs))))
    (assert-= expected (run-cps-ast ast))))

(deftest-each cps-ast-if-branch
  "cps-transform-ast: if selects the correct branch based on condition truthiness"
  :cases (("truthy-takes-then" 1  10 20 10)
          ("zero-takes-else"   0  10 20 20))
  (cond-val then-val else-val expected)
  (let ((ast (cl-cc:make-ast-if :cond (cl-cc:make-ast-int :value cond-val)
                                :then (cl-cc:make-ast-int :value then-val)
                                :else (cl-cc:make-ast-int :value else-val))))
    (assert-= expected (run-cps-ast ast))))

(deftest cps-ast-progn-last-value
  "cps-transform-ast: progn returns the value of its last form"
  (let* ((forms (list (cl-cc:make-ast-int :value 1)
                      (cl-cc:make-ast-int :value 2)
                      (cl-cc:make-ast-int :value 99)))
         (ast (cl-cc:make-ast-progn :forms forms)))
    (assert-= 99 (run-cps-ast ast))))

(deftest cps-ast-let-bindings
  "cps-transform-ast: let binds values and body uses them"
  (let* ((bindings (list (cons 'x (cl-cc:make-ast-int :value 3))
                         (cons 'y (cl-cc:make-ast-int :value 4))))
         (body     (list (cl-cc:make-ast-binop
                          :op '+
                          :lhs (cl-cc:make-ast-var :name 'x)
                          :rhs (cl-cc:make-ast-var :name 'y))))
         (ast (cl-cc:make-ast-let :bindings bindings :body body)))
    (assert-= 7 (run-cps-ast ast))))

(deftest cps-ast-print-returns-value
  "cps-transform-ast: print node passes printed value to continuation"
  (let* ((expr (cl-cc:make-ast-int :value 42))
         (ast  (cl-cc:make-ast-print :expr expr)))
    (assert-= 42 (run-cps-ast ast))))

(deftest cps-ast-quote
  "cps-transform-ast: quoted literal is passed to continuation as-is"
  (let* ((ast (cl-cc:make-ast-quote :value 'hello)))
    (assert-eq 'hello (run-cps-ast ast))))

(deftest cps-ast-quote-list
  "cps-transform-ast: quoted list is passed unchanged"
  (let* ((ast (cl-cc:make-ast-quote :value '(1 2 3))))
    (assert-equal '(1 2 3) (run-cps-ast ast))))

(deftest cps-ast-the-transparent
  "cps-transform-ast: the wraps in a CL type declaration but passes value through"
  (let* ((inner (cl-cc:make-ast-int :value 7))
         (ast   (cl-cc:make-ast-the :type 'integer :value inner)))
    (assert-= 7 (run-cps-ast ast))))

(deftest cps-ast-setq-returns-value
  "cps-transform-ast: setq evaluates the value and passes it to continuation"
  (let ((setq-ast (cl-cc:make-ast-setq
                   :var 'cl-cc-test-setq-var
                   :value (cl-cc:make-ast-int :value 55))))
    ;; Bind the target var so SBCL doesn't complain about an unbound special
    (let ((cl-cc-test-setq-var nil))
      (declare (special cl-cc-test-setq-var))
      (let ((result (run-cps-ast setq-ast)))
        (assert-= 55 result)
        (assert-= 55 cl-cc-test-setq-var)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; AST CPS — structural ("is it a CPS lambda?")
;;; These forms are transformed correctly but cannot be trivially evaluated
;;; because they involve non-local control (block, go, throw) or closures.
;;; ─────────────────────────────────────────────────────────────────────────

(deftest-each cps-ast-structural-shape
  "cps-transform-ast*: every node type produces a (lambda (k) ...) form"
  :cases
  (("lambda"
    (cl-cc:make-ast-lambda
     :params '(x)
     :body (list (cl-cc:make-ast-int :value 1))))

   ("block"
    (cl-cc:make-ast-block
     :name 'b
     :body (list (cl-cc:make-ast-int :value 1))))

   ("return-from"
    (cl-cc:make-ast-return-from
     :name 'b
     :value (cl-cc:make-ast-int :value 1)))

   ("tagbody"
    (cl-cc:make-ast-tagbody
     :tags (list (cons 'tag1 (list (cl-cc:make-ast-int :value 1))))))

   ("go"
    (cl-cc:make-ast-go :tag 'tag1))

   ("catch"
    (cl-cc:make-ast-catch
     :tag  (cl-cc:make-ast-var :name 'my-tag)
     :body (list (cl-cc:make-ast-int :value 42))))

   ("throw"
    (cl-cc:make-ast-throw
     :tag   (cl-cc:make-ast-var :name 'my-tag)
     :value (cl-cc:make-ast-int :value 42)))

   ("unwind-protect"
    (cl-cc:make-ast-unwind-protect
     :protected (cl-cc:make-ast-int :value 42)
     :cleanup   (list (cl-cc:make-ast-int :value 0))))

   ("flet"
    (cl-cc:make-ast-flet
     :bindings (list (list 'double '(x)
                           (cl-cc:make-ast-binop
                            :op '*
                            :lhs (cl-cc:make-ast-int :value 2)
                            :rhs (cl-cc:make-ast-var :name 'x))))
     :body (list (cl-cc:make-ast-int :value 1))))

   ("labels"
    (cl-cc:make-ast-labels
     :bindings (list (list 'id '(x) (cl-cc:make-ast-var :name 'x)))
     :body (list (cl-cc:make-ast-int :value 1)))))
  (ast)
  (assert-true (is-cps-lambda (cl-cc:cps-transform-ast* ast))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; cps-transform* dispatcher
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-transform*-on-ast-node
  "cps-transform* dispatches to the AST transformer for AST nodes"
  (let* ((ast    (cl-cc:make-ast-int :value 42))
         (result (cl-cc:cps-transform* ast)))
    (assert-true (is-cps-lambda result))))

(deftest cps-transform*-on-sexp
  "cps-transform* dispatches to the sexp transformer for non-AST values"
  (let ((result (cl-cc:cps-transform* '(+ 1 2))))
    (assert-true (is-cps-lambda result))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; cps-transform-sequence edge cases
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-sequence-empty-returns-nil
  "cps-transform-sequence with empty list calls continuation with nil"
  (let* ((k-var (gensym "K"))
         (sexp  (cl-cc:cps-transform-sequence nil k-var)))
    ;; Should be (funcall k nil)
    (assert-true (listp sexp))
    (assert-eq 'funcall (car sexp))))

(deftest cps-sequence-single-delegates
  "cps-transform-sequence with one form delegates directly to cps-transform-ast"
  (let* ((node  (cl-cc:make-ast-int :value 5))
         (k-var (gensym "K"))
         (sexp  (cl-cc:cps-transform-sequence (list node) k-var)))
    ;; Should be the direct CPS form for the integer, not an extra lambda wrap
    (assert-true (listp sexp))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS for block / return-from
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-block-outer-is-block
  "CPS of ast-block produces a (block NAME ...) form."
  (let* ((node (cl-cc:make-ast-block
                :name 'nil
                :body (list (cl-cc:make-ast-int :value 1))))
         (k (gensym "K"))
         (result (cl-cc::cps-transform-ast node k)))
    (assert-eq 'block (car result))))

(deftest cps-return-from-ignores-k
  "CPS of ast-return-from produces (return-from NAME ...) — k is not called."
  (let* ((node (cl-cc:make-ast-return-from
                :name 'nil
                :value (cl-cc:make-ast-int :value 42)))
         (k (gensym "K"))
         (result (format nil "~S" (cl-cc::cps-transform-ast node k))))
    (assert-true (search "RETURN-FROM" result))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS for catch / throw
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-catch-outer-is-funcall
  "CPS of ast-catch evaluates the tag first (funcall on outer lambda)."
  (let* ((node (cl-cc:make-ast-catch
                :tag (cl-cc:make-ast-quote :value :done)
                :body (list (cl-cc:make-ast-int :value 0))))
         (k (gensym "K"))
         (result (cl-cc::cps-transform-ast node k)))
    ;; Outer form should be (funcall <lambda> ...) or (lambda ...) wrapping catch
    (assert-eq 'funcall (car result))))

(deftest cps-throw-produces-throw
  "CPS of ast-throw produces a (throw TAG VAL) form."
  (let* ((node (cl-cc:make-ast-throw
                :tag (cl-cc:make-ast-quote :value :done)
                :value (cl-cc:make-ast-int :value 99)))
         (k (gensym "K"))
         (result (format nil "~S" (cl-cc::cps-transform-ast node k))))
    (assert-true (search "THROW" result))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS for tagbody-section
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-tagbody-section-empty-calls-k-with-nil
  "cps-transform-tagbody-section with empty list calls (funcall k nil)."
  (let* ((k (gensym "K"))
         (result (cl-cc::cps-transform-tagbody-section nil k)))
    (assert-eq 'funcall (car result))
    (assert-eq k (second result))
    (assert-eq nil (third result))))

(deftest cps-tagbody-section-single-form-is-wrapped
  "cps-transform-tagbody-section with one form wraps it in a lambda continuation."
  (let* ((k (gensym "K"))
         (result (cl-cc::cps-transform-tagbody-section
                  (list (cl-cc:make-ast-int :value 1)) k)))
    ;; Result should be the CPS of the form, which is a funcall
    (assert-eq 'funcall (car result))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS for local function bindings (flet/labels helpers)
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-fn-binding-outer-is-name
  "cps-transform-fn-binding preserves the function name as car."
  (let* ((k-var (gensym "K"))
         ;; body must be AST nodes — use a single ast-int
         (binding (list 'my-fn '(x) (cl-cc:make-ast-int :value 42)))
         (result (cl-cc::cps-transform-fn-binding binding k-var)))
    (assert-eq 'my-fn (first result))))

(deftest cps-fn-binding-body-is-lambda
  "cps-transform-fn-binding wraps params+k in a lambda."
  (let* ((k-var (gensym "K"))
         (binding (list 'my-fn '(x) (cl-cc:make-ast-int :value 1)))
         (result (cl-cc::cps-transform-fn-binding binding k-var)))
    (assert-eq 'lambda (first (second result)))))

(deftest cps-fn-binding-k-appended-to-params
  "cps-transform-fn-binding appends the continuation var to the param list."
  (let* ((k-var 'my-k)
         (binding (list 'my-fn '(a b) (cl-cc:make-ast-int :value 0)))
         (result (cl-cc::cps-transform-fn-binding binding k-var)))
    (let ((lambda-list (second (second result))))
      (assert-eq 'a (first lambda-list))
      (assert-eq 'b (second lambda-list))
      (assert-eq 'my-k (third lambda-list)))))

(deftest cps-local-fns-outer-is-form-kw
  "cps-transform-local-fns produces (flet ...) or (labels ...) as outer form."
  (let* ((k (gensym "K"))
         (body (list (cl-cc:make-ast-int :value 42)))
         (result-flet   (cl-cc::cps-transform-local-fns 'flet   nil body k))
         (result-labels (cl-cc::cps-transform-local-fns 'labels nil body k)))
    (assert-eq 'flet   (first result-flet))
    (assert-eq 'labels (first result-labels))))

(deftest cps-local-fns-bindings-transformed
  "cps-transform-local-fns applies cps-transform-fn-binding to each binding."
  (let* ((k (gensym "K"))
         (body (list (cl-cc:make-ast-int :value 1)))
         ;; binding = (f (x) <ast-node>)
         (binding (list 'f '(x) (cl-cc:make-ast-int :value 99)))
         (result (cl-cc::cps-transform-local-fns
                  'flet (list binding) body k)))
    ;; second element is the binding list: ((f (lambda (x K) ...)))
    (assert-eq 'f (first (first (second result))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS for unwind-protect
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-unwind-protect-outer-is-unwind-protect
  "ast-unwind-protect CPS produces (unwind-protect ...)."
  (let* ((node (cl-cc:make-ast-unwind-protect
                :protected (cl-cc:make-ast-int :value 1)
                :cleanup   (list (cl-cc:make-ast-int :value 2))))
         (k (gensym "K"))
         (result (cl-cc::cps-transform-ast node k)))
    (assert-eq 'unwind-protect (first result))))

(deftest cps-unwind-protect-no-cleanup-second-is-nil
  "ast-unwind-protect with no cleanup emits nil as the cleanup form."
  (let* ((node (cl-cc:make-ast-unwind-protect
                :protected (cl-cc:make-ast-int :value 1)
                :cleanup   nil))
         (k (gensym "K"))
         (result (cl-cc::cps-transform-ast node k)))
    (assert-eq nil (third result))))
