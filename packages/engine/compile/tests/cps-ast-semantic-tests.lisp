;;;; tests/unit/compile/cps-ast-semantic-tests.lisp — CPS AST semantic evaluation tests

(in-package :cl-cc/test)

(in-suite cps-ast-suite)

;;; ─────────────────────────────────────────────────────────────────────────
;;; AST CPS — semantic (evaluable forms)
;;; ─────────────────────────────────────────────────────────────────────────

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
          ("nil-takes-else"    nil 10 20 20))
  (cond-val then-val else-val expected)
  (let ((ast (cl-cc:make-ast-if :cond (cl-cc:make-ast-int :value cond-val)
                                :then (cl-cc:make-ast-int :value then-val)
                                :else (cl-cc:make-ast-int :value else-val))))
    (assert-= expected (run-cps-ast ast))))

(deftest-each cps-evaluable-forms
  "cps-transform-ast: each evaluable AST node type evaluates to its expected value via run-cps-ast."
  :cases (("integer"      (cl-cc:make-ast-int :value 42)
                          42)
          ("progn"        (cl-cc:make-ast-progn
                           :forms (list (cl-cc:make-ast-int :value 1)
                                        (cl-cc:make-ast-int :value 2)
                                        (cl-cc:make-ast-int :value 99)))
                          99)
          ("let"          (cl-cc:make-ast-let
                           :bindings (list (cons 'x (cl-cc:make-ast-int :value 3))
                                           (cons 'y (cl-cc:make-ast-int :value 4)))
                           :body (list (cl-cc:make-ast-binop
                                        :op '+
                                        :lhs (cl-cc:make-ast-var :name 'x)
                                        :rhs (cl-cc:make-ast-var :name 'y))))
                          7)
          ("print"        (cl-cc:make-ast-print :expr (cl-cc:make-ast-int :value 42))
                          42)
          ("quote-symbol" (cl-cc:make-ast-quote :value 'hello)
                          'hello)
          ("quote-list"   (cl-cc:make-ast-quote :value '(1 2 3))
                          '(1 2 3))
          ("the"          (cl-cc:make-ast-the :type 'integer
                                              :value (cl-cc:make-ast-int :value 7))
                          7))
  (ast expected)
  (assert-equal expected (run-cps-ast ast)))

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

(deftest-each cps-transform*-dispatch
  "cps-transform* dispatches to AST transformer for AST nodes and sexp transformer otherwise."
  :cases (("ast-node" (cl-cc:make-ast-int :value 42))
          ("sexp"     '(+ 1 2)))
  (input)
  (assert-true (is-cps-lambda (cl-cc:cps-transform* input))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; cps-transform-sequence edge cases
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-sequence-behavior
  "cps-transform-sequence: empty→(funcall k nil); single→direct CPS form."
  (let ((k-var (gensym "K")))
    ;; empty: calls continuation with nil
    (let ((sexp (cl-cc:cps-transform-sequence nil k-var)))
      (assert-true (listp sexp))
      (assert-eq 'funcall (car sexp)))
    ;; single form: delegates to cps-transform-ast (still a list)
    (let ((sexp (cl-cc:cps-transform-sequence (list (cl-cc:make-ast-int :value 5)) k-var)))
      (assert-true (listp sexp)))))

(deftest cps-simplify-fixed-point-stops-on-stable-form
  "%cps-simplify-fixed-point keeps applying a step until the form stabilizes."
  (let ((calls 0))
    (assert-equal 'done
                  (cl-cc/compile::%cps-simplify-fixed-point
                   'start
                   (lambda (form)
                     (incf calls)
                     (if (eq form 'start) 'done 'done))))
    (assert-= 2 calls)))

(deftest cps-dispatch-table-covers-bootstrap-special-forms
  "The bootstrap CPS dispatch table keeps handlers for every supported special form."
  (dolist (operator '(+ - * if progn let print))
    (assert-true (functionp (gethash operator cl-cc/compile::*cps-sexp-dispatch-table*)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS for block / return-from
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-block-outer-is-block
  (let* ((node (cl-cc:make-ast-block
                :name 'nil
                :body (list (cl-cc:make-ast-int :value 1))))
         (k      (gensym "K"))
         (result (cl-cc/compile::cps-transform-ast node k)))
    (assert-eq 'block (car result))))

(deftest cps-return-from-contains-token
  (let* ((node (cl-cc:make-ast-return-from
                :name 'nil
                :value (cl-cc:make-ast-int :value 42)))
         (k      (gensym "K"))
         (result (format nil "~S" (cl-cc/compile::cps-transform-ast node k))))
    (assert-true (search "RETURN-FROM" result))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS for catch / throw
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-catch-outer-is-funcall
  (let* ((node (cl-cc:make-ast-catch
                :tag  (cl-cc:make-ast-quote :value :done)
                :body (list (cl-cc:make-ast-int :value 0))))
         (k      (gensym "K"))
         (result (cl-cc/compile::cps-transform-ast node k)))
    (assert-eq 'funcall (car result))))

(deftest cps-throw-contains-token
  (let* ((node (cl-cc:make-ast-throw
                :tag   (cl-cc:make-ast-quote :value :done)
                :value (cl-cc:make-ast-int :value 99)))
         (k      (gensym "K"))
         (result (format nil "~S" (cl-cc/compile::cps-transform-ast node k))))
    (assert-true (search "THROW" result))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS for tagbody-section
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-tagbody-section-behavior
  "cps-transform-tagbody-section: empty→(funcall k nil); single form→funcall."
  (let ((k (gensym "K")))
    ;; empty: calls (funcall k nil)
    (let ((result (cl-cc/compile::cps-transform-tagbody-section nil k)))
      (assert-eq 'funcall (car result))
      (assert-eq k (second result))
      (assert-eq nil (third result)))
    ;; single form: CPS wrapper is a funcall
    (let ((result (cl-cc/compile::cps-transform-tagbody-section
                   (list (cl-cc:make-ast-int :value 1)) k)))
      (assert-eq 'funcall (car result)))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS for local function bindings (flet/labels helpers)
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-fn-binding-structure
  "cps-transform-fn-binding: preserves name as car, wraps in lambda, appends k to params."
  (let* ((k-var 'my-k)
         (binding (list 'my-fn '(a b) (cl-cc:make-ast-int :value 42)))
         (result (cl-cc/compile::cps-transform-fn-binding binding k-var)))
    (assert-eq 'my-fn (first result))
    (assert-eq 'lambda (first (second result)))
    (let ((lambda-list (second (second result))))
      (assert-eq 'a    (first  lambda-list))
      (assert-eq 'b    (second lambda-list))
      (assert-eq 'my-k (third  lambda-list)))))

(deftest-each cps-local-fns-outer-is-form-kw
  "cps-transform-local-fns produces (flet ...) or (labels ...) as the outer form."
  :cases (("flet"   'flet)
          ("labels" 'labels))
  (form-kw)
  (let* ((k    (gensym "K"))
         (body (list (cl-cc:make-ast-int :value 42))))
    (assert-eq form-kw (first (cl-cc/compile::cps-transform-local-fns form-kw nil body k)))))

(deftest cps-local-fns-bindings-transformed
  "cps-transform-local-fns applies cps-transform-fn-binding to each binding."
  (let* ((k (gensym "K"))
         (body (list (cl-cc:make-ast-int :value 1)))
         ;; binding = (f (x) <ast-node>)
         (binding (list 'f '(x) (cl-cc:make-ast-int :value 99)))
         (result (cl-cc/compile::cps-transform-local-fns
                  'flet (list binding) body k)))
    ;; second element is the binding list: ((f (lambda (x K) ...)))
    (assert-eq 'f (first (first (second result))))))

;;; ─────────────────────────────────────────────────────────────────────────
;;; CPS for unwind-protect
;;; ─────────────────────────────────────────────────────────────────────────

(deftest cps-unwind-protect-structure
  "ast-unwind-protect CPS: outer is (unwind-protect ...) and nil cleanup emits nil."
  (let ((k (gensym "K")))
    (let* ((node (cl-cc:make-ast-unwind-protect
                  :protected (cl-cc:make-ast-int :value 1)
                  :cleanup   (list (cl-cc:make-ast-int :value 2))))
           (result (cl-cc/compile::cps-transform-ast node k)))
      (assert-eq 'unwind-protect (first result)))
    (let* ((node (cl-cc:make-ast-unwind-protect
                  :protected (cl-cc:make-ast-int :value 1)
                  :cleanup   nil))
           (result (cl-cc/compile::cps-transform-ast node k)))
      (assert-eq nil (third result)))))
