;;;; tests/unit/compile/closure-tests.lisp — Unit tests for find-free-variables
;;;;
;;;; Tests the free variable analysis on hand-constructed AST nodes.
;;;; Each test builds an AST directly (no parsing) and checks the result
;;;; of find-free-variables against the expected set of free variables.

(in-package :cl-cc/test)

(defsuite closure-suite :description "Free variable analysis unit tests"
  :parent cl-cc-unit-suite)


(in-suite closure-suite)
;;; ─── Literals ─────────────────────────────────────────────────────────────

(deftest-each free-vars-atomic-forms
  "Atomic forms (integer literal, quote) have no free variables."
  :cases (("int"   (cl-cc/ast::make-ast-int   :value 42))
          ("quote" (cl-cc/ast::make-ast-quote :value '(a b c))))
  (node)
  (assert-equal nil (cl-cc/compile::find-free-variables node)))

;;; ─── Simple references ────────────────────────────────────────────────────

(deftest ast-free-vars-single-var
  "A single variable reference is free in find-free-variables."
  (assert-equal '(x)
    (cl-cc/compile::find-free-variables (cl-cc/ast::make-ast-var :name 'x))))

(deftest free-vars-binop-cases
  "Binop free vars: two distinct vars → both free; same var twice → deduplicated to one."
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast::make-ast-binop
                  :op '+
                  :lhs (cl-cc/ast::make-ast-var :name 'x)
                  :rhs (cl-cc/ast::make-ast-var :name 'y)))))
    (assert-true (and (member 'x result) (member 'y result)))
    (assert-equal 2 (length result)))
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast::make-ast-binop
                  :op '+
                  :lhs (cl-cc/ast::make-ast-var :name 'x)
                  :rhs (cl-cc/ast::make-ast-var :name 'x)))))
    (assert-equal '(x) result)))

;;; ─── Let binding ──────────────────────────────────────────────────────────

(deftest free-vars-let-scenarios
  "Let: bound vars shadow body; binding exprs are free; unbound body vars remain free."
  ;; x bound in let → not free
  (assert-equal nil (cl-cc/compile::find-free-variables
                     (cl-cc/ast::make-ast-let
                      :bindings (list (cons 'x (cl-cc/ast::make-ast-int :value 1)))
                      :body     (list (cl-cc/ast::make-ast-var :name 'x)))))
  ;; binding expr references y → y is free; x is bound
  (assert-equal '(y) (cl-cc/compile::find-free-variables
                      (cl-cc/ast::make-ast-let
                       :bindings (list (cons 'x (cl-cc/ast::make-ast-var :name 'y)))
                       :body     (list (cl-cc/ast::make-ast-var :name 'x)))))
  ;; x bound, z is free in body
  (assert-equal '(z) (cl-cc/compile::find-free-variables
                      (cl-cc/ast::make-ast-let
                       :bindings (list (cons 'x (cl-cc/ast::make-ast-int :value 1)))
                       :body     (list (cl-cc/ast::make-ast-binop
                                        :op '+
                                        :lhs (cl-cc/ast::make-ast-var :name 'x)
                                        :rhs (cl-cc/ast::make-ast-var :name 'z)))))))

;;; ─── Lambda params ────────────────────────────────────────────────────────

(deftest free-vars-lambda-scope
  "Lambda params are not free in body; outer variables referenced in body are free."
  ;; params shadow body references — no free vars
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast::make-ast-lambda
                  :params '(x y)
                  :body (list (cl-cc/ast::make-ast-binop
                               :op '+
                               :lhs (cl-cc/ast::make-ast-var :name 'x)
                               :rhs (cl-cc/ast::make-ast-var :name 'y)))))))
    (assert-equal nil result))
  ;; x is a param (bound), z is free
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast::make-ast-lambda
                  :params '(x)
                  :body (list (cl-cc/ast::make-ast-binop
                               :op '+
                               :lhs (cl-cc/ast::make-ast-var :name 'x)
                               :rhs (cl-cc/ast::make-ast-var :name 'z)))))))
    (assert-equal '(z) result)))

(deftest free-vars-lambda-extended-params-shadow
  "Lambda &rest and &optional params shadow their names in the body."
  ;; &rest param
  (assert-equal nil (cl-cc/compile::find-free-variables
                     (cl-cc/ast::make-ast-lambda
                      :params '(x)
                      :rest-param 'rest
                      :body (list (cl-cc/ast::make-ast-var :name 'rest)))))
  ;; &optional param
  (assert-equal nil (cl-cc/compile::find-free-variables
                     (cl-cc/ast::make-ast-lambda
                      :params nil
                      :optional-params (list (list 'a (cl-cc/ast::make-ast-int :value 0)))
                      :body (list (cl-cc/ast::make-ast-var :name 'a))))))

(deftest free-vars-lambda-optional-default-free
  "Free variables in optional param defaults are captured."
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast::make-ast-lambda
                  :params nil
                  :optional-params (list (list 'a (cl-cc/ast::make-ast-var :name 'z)))
                  :body (list (cl-cc/ast::make-ast-var :name 'a))))))
    ;; a is bound as optional, z is free from the default expression
    (assert-equal '(z) result)))

(deftest free-vars-lambda-key-param
  "Lambda keyword params are not free in body."
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast::make-ast-lambda
                  :params nil
                  :key-params (list (list 'k nil))
                  :body (list (cl-cc/ast::make-ast-var :name 'k))))))
    (assert-equal nil result)))

;;; ─── Nested scope ─────────────────────────────────────────────────────────

(deftest free-vars-nested-let
  "Inner let does not shadow outer free variables."
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast::make-ast-let
                  :bindings (list (cons 'x (cl-cc/ast::make-ast-int :value 1)))
                  :body (list
                         (cl-cc/ast::make-ast-let
                          :bindings (list (cons 'y (cl-cc/ast::make-ast-var :name 'x)))
                          :body (list (cl-cc/ast::make-ast-binop
                                       :op '+
                                       :lhs (cl-cc/ast::make-ast-var :name 'y)
                                       :rhs (cl-cc/ast::make-ast-var :name 'w)))))))))
    ;; x is bound by outer let, y is bound by inner let, w is free
    (assert-equal '(w) result)))

;;; ─── Defun ────────────────────────────────────────────────────────────────

(deftest free-vars-defun-shadows-params
  "Defun params are not free in body."
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast::make-ast-defun
                  :name 'my-fn
                  :params '(a b)
                  :body (list (cl-cc/ast::make-ast-binop
                               :op '+
                               :lhs (cl-cc/ast::make-ast-var :name 'a)
                               :rhs (cl-cc/ast::make-ast-var :name 'c)))))))
    ;; a,b are params, c is free
    (assert-equal '(c) result)))

;;; ─── Setq ─────────────────────────────────────────────────────────────────

(deftest free-vars-setq
  "Setq marks both the variable and the value expression's free vars."
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast::make-ast-setq
                  :var 'x
                  :value (cl-cc/ast::make-ast-var :name 'y)))))
    (assert-true (and (member 'x result) (member 'y result)))
    (assert-equal 2 (length result))))

;;; ─── Call ─────────────────────────────────────────────────────────────────

(deftest free-vars-call-function-forms
  "Call: symbol func contributes no free vars (only args); AST func node contributes free vars from both func and args."
  ;; symbol func — only arg x is free
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast::make-ast-call
                  :func 'foo
                  :args (list (cl-cc/ast::make-ast-var :name 'x))))))
    (assert-equal '(x) result))
  ;; AST func node — both f and x are free
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast::make-ast-call
                  :func (cl-cc/ast::make-ast-var :name 'f)
                  :args (list (cl-cc/ast::make-ast-var :name 'x))))))
    (assert-true (and (member 'f result) (member 'x result)))
    (assert-equal 2 (length result))))

;;; ─── Flet / Labels ───────────────────────────────────────────────────────

(deftest free-vars-flet-labels-cases
  "Flet: bound func name not free in body; Labels: outer vars referenced inside binding are free."
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast::make-ast-flet
                  :bindings (list (list 'my-fn '(a) (cl-cc/ast::make-ast-var :name 'a)))
                  :body (list (cl-cc/ast::make-ast-call
                               :func 'my-fn
                               :args (list (cl-cc/ast::make-ast-int :value 1))))))))
    (assert-equal nil result))
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast::make-ast-labels
                  :bindings (list (list 'rec '(n) (cl-cc/ast::make-ast-var :name 'limit)))
                  :body (list (cl-cc/ast::make-ast-call
                               :func 'rec
                               :args (list (cl-cc/ast::make-ast-int :value 0))))))))
    (assert-equal '(limit) result)))

;;; ─── If / Progn ──────────────────────────────────────────────────────────

(deftest free-vars-control-forms
  "If collects free vars from all three branches; progn collects free vars from all forms."
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast::make-ast-if
                  :cond (cl-cc/ast::make-ast-var :name 'p)
                  :then (cl-cc/ast::make-ast-var :name 'x)
                  :else (cl-cc/ast::make-ast-var :name 'y)))))
    (assert-true (and (member 'p result) (member 'x result) (member 'y result)))
    (assert-equal 3 (length result)))
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast::make-ast-progn
                  :forms (list (cl-cc/ast::make-ast-var :name 'a)
                               (cl-cc/ast::make-ast-var :name 'b))))))
    (assert-true (and (member 'a result) (member 'b result)))
    (assert-equal 2 (length result))))

;;; ─── %escape-add-kind / %escape-merge-kinds (extracted pure helpers) ─────

(deftest-each escape-add-kind-deduplicates
  "%escape-add-kind adds a kind only when absent."
  :cases (("fresh-add"     :return   nil              '(:return))
          ("already-there" :return   '(:return)       '(:return))
          ("second-kind"   :capture  '(:return)       '(:capture :return)))
  (kind acc expected)
  (assert-equal expected (cl-cc/compile::%escape-add-kind kind acc)))

(deftest escape-merge-kinds-merges-multiple-lists
  "%escape-merge-kinds deduplicates across multiple kind lists."
  (assert-equal nil (cl-cc/compile::%escape-merge-kinds nil nil))
  (let ((result (cl-cc/compile::%escape-merge-kinds '(:return) '(:return :capture))))
    (assert-= 2 (length result))
    (assert-true (member :return  result))
    (assert-true (member :capture result))))

;;; ─── %count-ast-calls (extracted recursive helper) ───────────────────────

(deftest count-ast-calls-leaf-match
  "%count-ast-calls returns 1 for a direct ast-call of BINDING-NAME."
  (let ((node (cl-cc/ast::make-ast-call
               :func 'my-fn
               :args (list (cl-cc/ast::make-ast-int :value 1)))))
    (assert-= 1 (cl-cc/compile::%count-ast-calls node 'my-fn))))

(deftest count-ast-calls-no-match
  "%count-ast-calls returns 0 when BINDING-NAME is not called."
  (let ((node (cl-cc/ast::make-ast-call
               :func 'other-fn
               :args (list (cl-cc/ast::make-ast-int :value 1)))))
    (assert-= 0 (cl-cc/compile::%count-ast-calls node 'my-fn))))

(deftest count-ast-calls-nested
  "%count-ast-calls counts across nested children."
  (let* ((inner (cl-cc/ast::make-ast-call :func 'my-fn :args nil))
         (outer (cl-cc/ast::make-ast-progn :forms (list inner inner))))
    (assert-= 2 (cl-cc/compile::%count-ast-calls outer 'my-fn))))

;;; ─── %escape-mentions-node-p / %escape-mentions-forms-p ─────────────────

(deftest-each escape-mentions-node-p-cases
  "%escape-mentions-node-p: T for matching ast-var; NIL for non-matching or integer."
  :cases (("match"     (cl-cc/ast::make-ast-var :name 'x) 'x   t)
          ("no-match"  (cl-cc/ast::make-ast-var :name 'y) 'x   nil)
          ("literal"   (cl-cc/ast::make-ast-int :value 1) 'x   nil))
  (node binding expected)
  (if expected
      (assert-true  (cl-cc/compile::%escape-mentions-node-p node binding))
      (assert-false (cl-cc/compile::%escape-mentions-node-p node binding))))

(deftest escape-mentions-forms-p-cases
  "%escape-mentions-forms-p: T when any form in list mentions the name."
  (assert-true (cl-cc/compile::%escape-mentions-forms-p
                (list (cl-cc/ast::make-ast-var :name 'x)) 'x))
  (assert-false (cl-cc/compile::%escape-mentions-forms-p
                 (list (cl-cc/ast::make-ast-int :value 1)) 'x))
  (assert-false (cl-cc/compile::%escape-mentions-forms-p nil 'x)))

;;; ─── %escape-classify-children ──────────────────────────────────────────

(deftest escape-classify-children-nil-for-int-child
  "%escape-classify-children returns NIL when no child references the binding."
  (let* ((child  (cl-cc/ast::make-ast-int :value 42))
         (parent (cl-cc/ast::make-ast-progn :forms (list child))))
    (assert-null (cl-cc/compile::%escape-classify-children parent 'x nil))))

(deftest escape-classify-children-detects-return-from-var-child
  "%escape-classify-children reports kinds when a child is an ast-var matching the binding."
  (let* ((child  (cl-cc/ast::make-ast-var :name 'x))
         (parent (cl-cc/ast::make-ast-progn :forms (list child))))
    (let ((kinds (cl-cc/compile::%escape-classify-children parent 'x nil)))
      (assert-true (member :return kinds)))))

;;; ─── %escape-capture-kinds ───────────────────────────────────────────────

(deftest escape-capture-kinds-nil-when-binding-absent
  "%escape-capture-kinds returns NIL when body does not reference binding-name."
  (let ((body (list (cl-cc/ast::make-ast-int :value 0))))
    (assert-null (cl-cc/compile::%escape-capture-kinds body 'x nil))))

(deftest escape-capture-kinds-capture-when-mentioned
  "%escape-capture-kinds returns :capture when body directly references binding-name."
  (let ((body (list (cl-cc/ast::make-ast-var :name 'x))))
    (let ((kinds (cl-cc/compile::%escape-capture-kinds body 'x nil)))
      (assert-true (member :capture kinds)))))

;;; ─── %escape-classify / binding-escape-kinds-in-body ────────────────────

(deftest escape-classify-var-returns-return-kind
  "%escape-classify on an ast-var matching binding-name yields (:return)."
  (let ((node (cl-cc/ast::make-ast-var :name 'x)))
    (assert-equal '(:return) (cl-cc/compile::%escape-classify node 'x nil))))

(deftest escape-classify-var-no-match-returns-nil
  "%escape-classify on an ast-var not matching binding-name yields NIL."
  (let ((node (cl-cc/ast::make-ast-var :name 'y)))
    (assert-null (cl-cc/compile::%escape-classify node 'x nil))))

(deftest binding-escape-kinds-capture-from-lambda
  "binding-escape-kinds-in-body detects :capture when binding is referenced inside a lambda."
  (let* ((body (list (cl-cc/ast::make-ast-lambda
                      :params '(z)
                      :body   (list (cl-cc/ast::make-ast-var :name 'x))))))
    (let ((kinds (cl-cc/compile::binding-escape-kinds-in-body body 'x)))
      (assert-true (member :capture kinds)))))

(deftest binding-escape-kinds-empty-body
  "binding-escape-kinds-in-body returns NIL for empty list body."
  (assert-null (cl-cc/compile::binding-escape-kinds-in-body nil 'x)))

