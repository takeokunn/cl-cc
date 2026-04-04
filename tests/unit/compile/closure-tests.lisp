;;;; tests/unit/compile/closure-tests.lisp — Unit tests for find-free-variables
;;;;
;;;; Tests the free variable analysis on hand-constructed AST nodes.
;;;; Each test builds an AST directly (no parsing) and checks the result
;;;; of find-free-variables against the expected set of free variables.

(in-package :cl-cc/test)

(defsuite closure-suite :description "Free variable analysis unit tests")

;;; ─── Literals ─────────────────────────────────────────────────────────────

(deftest-each free-vars-atomic-forms
  "Atomic forms (integer literal, quote) have no free variables."
  :cases (("int"   (cl-cc::make-ast-int   :value 42))
          ("quote" (cl-cc::make-ast-quote :value '(a b c))))
  (node)
  (assert-equal nil (cl-cc::find-free-variables node)))

;;; ─── Simple references ────────────────────────────────────────────────────

(deftest free-vars-single-var
  "A single variable reference is free."
  (assert-equal '(x)
    (cl-cc::find-free-variables (cl-cc::make-ast-var :name 'x))))

(deftest free-vars-binop-two-vars
  "A binop with two different vars has both free."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-binop
                  :op '+
                  :lhs (cl-cc::make-ast-var :name 'x)
                  :rhs (cl-cc::make-ast-var :name 'y)))))
    (assert-true (and (member 'x result) (member 'y result)))
    (assert-equal 2 (length result))))

(deftest free-vars-binop-same-var
  "A binop referencing the same var twice returns it once (union)."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-binop
                  :op '+
                  :lhs (cl-cc::make-ast-var :name 'x)
                  :rhs (cl-cc::make-ast-var :name 'x)))))
    (assert-equal '(x) result)))

;;; ─── Let binding ──────────────────────────────────────────────────────────

(deftest free-vars-let-scenarios
  "Let: bound vars shadow body; binding exprs are free; unbound body vars remain free."
  ;; x bound in let → not free
  (assert-equal nil (cl-cc::find-free-variables
                     (cl-cc::make-ast-let
                      :bindings (list (cons 'x (cl-cc::make-ast-int :value 1)))
                      :body     (list (cl-cc::make-ast-var :name 'x)))))
  ;; binding expr references y → y is free; x is bound
  (assert-equal '(y) (cl-cc::find-free-variables
                      (cl-cc::make-ast-let
                       :bindings (list (cons 'x (cl-cc::make-ast-var :name 'y)))
                       :body     (list (cl-cc::make-ast-var :name 'x)))))
  ;; x bound, z is free in body
  (assert-equal '(z) (cl-cc::find-free-variables
                      (cl-cc::make-ast-let
                       :bindings (list (cons 'x (cl-cc::make-ast-int :value 1)))
                       :body     (list (cl-cc::make-ast-binop
                                        :op '+
                                        :lhs (cl-cc::make-ast-var :name 'x)
                                        :rhs (cl-cc::make-ast-var :name 'z)))))))

;;; ─── Lambda params ────────────────────────────────────────────────────────

(deftest free-vars-lambda-scope
  "Lambda params are not free in body; outer variables referenced in body are free."
  ;; params shadow body references — no free vars
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-lambda
                  :params '(x y)
                  :body (list (cl-cc::make-ast-binop
                               :op '+
                               :lhs (cl-cc::make-ast-var :name 'x)
                               :rhs (cl-cc::make-ast-var :name 'y)))))))
    (assert-equal nil result))
  ;; x is a param (bound), z is free
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-lambda
                  :params '(x)
                  :body (list (cl-cc::make-ast-binop
                               :op '+
                               :lhs (cl-cc::make-ast-var :name 'x)
                               :rhs (cl-cc::make-ast-var :name 'z)))))))
    (assert-equal '(z) result)))

(deftest free-vars-lambda-extended-params-shadow
  "Lambda &rest and &optional params shadow their names in the body."
  ;; &rest param
  (assert-equal nil (cl-cc::find-free-variables
                     (cl-cc::make-ast-lambda
                      :params '(x)
                      :rest-param 'rest
                      :body (list (cl-cc::make-ast-var :name 'rest)))))
  ;; &optional param
  (assert-equal nil (cl-cc::find-free-variables
                     (cl-cc::make-ast-lambda
                      :params nil
                      :optional-params (list (list 'a (cl-cc::make-ast-int :value 0)))
                      :body (list (cl-cc::make-ast-var :name 'a))))))

(deftest free-vars-lambda-optional-default-free
  "Free variables in optional param defaults are captured."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-lambda
                  :params nil
                  :optional-params (list (list 'a (cl-cc::make-ast-var :name 'z)))
                  :body (list (cl-cc::make-ast-var :name 'a))))))
    ;; a is bound as optional, z is free from the default expression
    (assert-equal '(z) result)))

(deftest free-vars-lambda-key-param
  "Lambda keyword params are not free in body."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-lambda
                  :params nil
                  :key-params (list (list 'k nil))
                  :body (list (cl-cc::make-ast-var :name 'k))))))
    (assert-equal nil result)))

;;; ─── Nested scope ─────────────────────────────────────────────────────────

(deftest free-vars-nested-let
  "Inner let does not shadow outer free variables."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-let
                  :bindings (list (cons 'x (cl-cc::make-ast-int :value 1)))
                  :body (list
                         (cl-cc::make-ast-let
                          :bindings (list (cons 'y (cl-cc::make-ast-var :name 'x)))
                          :body (list (cl-cc::make-ast-binop
                                       :op '+
                                       :lhs (cl-cc::make-ast-var :name 'y)
                                       :rhs (cl-cc::make-ast-var :name 'w)))))))))
    ;; x is bound by outer let, y is bound by inner let, w is free
    (assert-equal '(w) result)))

;;; ─── Defun ────────────────────────────────────────────────────────────────

(deftest free-vars-defun-shadows-params
  "Defun params are not free in body."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-defun
                  :name 'my-fn
                  :params '(a b)
                  :body (list (cl-cc::make-ast-binop
                               :op '+
                               :lhs (cl-cc::make-ast-var :name 'a)
                               :rhs (cl-cc::make-ast-var :name 'c)))))))
    ;; a,b are params, c is free
    (assert-equal '(c) result)))

;;; ─── Setq ─────────────────────────────────────────────────────────────────

(deftest free-vars-setq
  "Setq marks both the variable and the value expression's free vars."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-setq
                  :var 'x
                  :value (cl-cc::make-ast-var :name 'y)))))
    (assert-true (and (member 'x result) (member 'y result)))
    (assert-equal 2 (length result))))

;;; ─── Call ─────────────────────────────────────────────────────────────────

(deftest free-vars-call-function-forms
  "Call: symbol func contributes no free vars (only args); AST func node contributes free vars from both func and args."
  ;; symbol func — only arg x is free
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-call
                  :func 'foo
                  :args (list (cl-cc::make-ast-var :name 'x))))))
    (assert-equal '(x) result))
  ;; AST func node — both f and x are free
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-call
                  :func (cl-cc::make-ast-var :name 'f)
                  :args (list (cl-cc::make-ast-var :name 'x))))))
    (assert-true (and (member 'f result) (member 'x result)))
    (assert-equal 2 (length result))))

;;; ─── Flet / Labels ───────────────────────────────────────────────────────

(deftest free-vars-flet-shadows-func-name
  "Flet function names are not free in body."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-flet
                  :bindings (list (list 'my-fn '(a) (cl-cc::make-ast-var :name 'a)))
                  :body (list (cl-cc::make-ast-call
                               :func 'my-fn
                               :args (list (cl-cc::make-ast-int :value 1))))))))
    ;; my-fn is shadowed by flet, a is shadowed inside the binding body
    (assert-equal nil result)))

(deftest free-vars-labels-captures-outer
  "Labels bodies with outer references mark them free."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-labels
                  :bindings (list (list 'rec '(n) (cl-cc::make-ast-var :name 'limit)))
                  :body (list (cl-cc::make-ast-call
                               :func 'rec
                               :args (list (cl-cc::make-ast-int :value 0))))))))
    ;; rec is bound by labels, n is param inside binding, limit is free
    (assert-equal '(limit) result)))

;;; ─── If / Progn ──────────────────────────────────────────────────────────

(deftest free-vars-control-forms
  "If collects free vars from all three branches; progn collects free vars from all forms."
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-if
                  :cond (cl-cc::make-ast-var :name 'p)
                  :then (cl-cc::make-ast-var :name 'x)
                  :else (cl-cc::make-ast-var :name 'y)))))
    (assert-true (and (member 'p result) (member 'x result) (member 'y result)))
    (assert-equal 3 (length result)))
  (let ((result (cl-cc::find-free-variables
                 (cl-cc::make-ast-progn
                  :forms (list (cl-cc::make-ast-var :name 'a)
                               (cl-cc::make-ast-var :name 'b))))))
    (assert-true (and (member 'a result) (member 'b result)))
    (assert-equal 2 (length result))))

;;; ─── Conservative escape analysis helper ─────────────────────────────────

(deftest binding-escapes-when-returned
  "A binding escapes when it is returned directly from the body."
  (assert-true
   (cl-cc::binding-escapes-in-body-p
    (list (cl-cc::make-ast-var :name 'p))
    'p)))

(deftest binding-does-not-escape-through-safe-consumer
  "A binding does not escape when only consumed by a whitelisted safe call."
  (assert-null
   (cl-cc::binding-escapes-in-body-p
    (list (cl-cc::make-ast-call
           :func 'car
           :args (list (cl-cc::make-ast-var :name 'p))))
    'p
     :safe-consumers '("CAR"))))

(deftest binding-escape-kinds-report-return
  "The classifier reports :return when a binding flows out directly."
  (assert-equal '(:return)
                (cl-cc::binding-escape-kinds-in-body
                 (list (cl-cc::make-ast-var :name 'p))
                 'p)))

(deftest binding-escape-kinds-report-external-call
  "The classifier reports :external-call when a binding is passed to an unknown callee."
  (assert-true
   (member :external-call
           (cl-cc::binding-escape-kinds-in-body
            (list (cl-cc::make-ast-call
                   :func 'list
                   :args (list (cl-cc::make-ast-var :name 'p))))
            'p))))

(deftest binding-escapes-when-captured-by-inner-lambda
  "A binding escapes when captured by a nested lambda."
  (assert-true
   (cl-cc::binding-escapes-in-body-p
     (list (cl-cc::make-ast-lambda
            :params '()
            :body (list (cl-cc::make-ast-var :name 'p))))
     'p)))

(deftest binding-escape-kinds-report-capture
  "The classifier reports :capture when a binding is captured by an inner lambda."
  (assert-true
   (member :capture
           (cl-cc::binding-escape-kinds-in-body
            (list (cl-cc::make-ast-lambda
                   :params '()
                   :body (list (cl-cc::make-ast-var :name 'p))))
            'p))))
