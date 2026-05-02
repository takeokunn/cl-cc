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
  :cases (("int"   (cl-cc/ast:make-ast-int   :value 42))
          ("quote" (cl-cc/ast:make-ast-quote :value '(a b c))))
  (node)
  (assert-equal nil (cl-cc/compile::find-free-variables node)))

;;; ─── Simple references ────────────────────────────────────────────────────

(deftest free-vars-single-var-is-free
  "A bare ast-var is free."
  (assert-equal '(x) (cl-cc/compile::find-free-variables
                      (cl-cc/ast:make-ast-var :name 'x))))

(deftest free-vars-binop-two-distinct-vars
  "Binop with two distinct var operands → both are free (no duplicates)."
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast:make-ast-binop
                  :op '+
                  :lhs (cl-cc/ast:make-ast-var :name 'x)
                  :rhs (cl-cc/ast:make-ast-var :name 'y)))))
    (assert-true (member 'x result))
    (assert-true (member 'y result))
    (assert-equal 2 (length result))))

(deftest free-vars-same-var-deduplicated
  "Same var in both operands of a binop appears only once in the result."
  (assert-equal '(x)
    (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-binop
      :op '+
      :lhs (cl-cc/ast:make-ast-var :name 'x)
      :rhs (cl-cc/ast:make-ast-var :name 'x)))))

;;; ─── Let binding ──────────────────────────────────────────────────────────

(deftest free-vars-let-bound-var-not-free
  "A variable bound by let and referenced only in its body is not free."
  (assert-equal nil
    (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-let
      :bindings (list (cons 'x (cl-cc/ast:make-ast-int :value 1)))
      :body     (list (cl-cc/ast:make-ast-var :name 'x))))))

(deftest free-vars-let-binding-expr-is-free
  "Variable referenced in a let binding expression is free (the bound name is not)."
  (assert-equal '(y)
    (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-let
      :bindings (list (cons 'x (cl-cc/ast:make-ast-var :name 'y)))
      :body     (list (cl-cc/ast:make-ast-var :name 'x))))))

(deftest free-vars-let-unbound-body-var-is-free
  "Var not bound by let but used in body is free; the bound var is not."
  (assert-equal '(z)
    (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-let
      :bindings (list (cons 'x (cl-cc/ast:make-ast-int :value 1)))
      :body     (list (cl-cc/ast:make-ast-binop
                       :op '+
                       :lhs (cl-cc/ast:make-ast-var :name 'x)
                       :rhs (cl-cc/ast:make-ast-var :name 'z)))))))

;;; ─── Lambda params ────────────────────────────────────────────────────────

(deftest free-vars-lambda-all-params-bound
  "All referenced vars are params → no free variables."
  (assert-equal nil
    (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-lambda
      :params '(x y)
      :body (list (cl-cc/ast:make-ast-binop
                   :op '+
                   :lhs (cl-cc/ast:make-ast-var :name 'x)
                   :rhs (cl-cc/ast:make-ast-var :name 'y)))))))

(deftest free-vars-lambda-outer-var-is-free
  "Param is bound; outer variable referenced in body is free."
  (assert-equal '(z)
    (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-lambda
      :params '(x)
      :body (list (cl-cc/ast:make-ast-binop
                   :op '+
                   :lhs (cl-cc/ast:make-ast-var :name 'x)
                   :rhs (cl-cc/ast:make-ast-var :name 'z)))))))

(deftest free-vars-lambda-rest-param-shadows-body
  "&rest param shadows the name in the body — not free."
  (assert-equal nil
    (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-lambda
      :params '(x)
      :rest-param 'rest
      :body (list (cl-cc/ast:make-ast-var :name 'rest))))))

(deftest free-vars-lambda-optional-param-shadows-body
  "&optional param shadows the name in the body — not free."
  (assert-equal nil
    (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-lambda
      :params nil
      :optional-params (list (list 'a (cl-cc/ast:make-ast-int :value 0)))
      :body (list (cl-cc/ast:make-ast-var :name 'a))))))

(deftest free-vars-optional-param-default-is-free-var
  "The default expression of an optional param contributes free variables."
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast:make-ast-lambda
                  :params nil
                  :optional-params (list (list 'a (cl-cc/ast:make-ast-var :name 'z)))
                  :body (list (cl-cc/ast:make-ast-var :name 'a))))))
    (assert-equal '(z) result)))

(deftest free-vars-keyword-param-is-not-free-in-body
  "A keyword parameter name is bound and therefore not free in the body."
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast:make-ast-lambda
                  :params nil
                  :key-params (list (list 'k nil))
                  :body (list (cl-cc/ast:make-ast-var :name 'k))))))
    (assert-equal nil result)))

;;; ─── Nested scope ─────────────────────────────────────────────────────────

(deftest free-vars-nested-let
  "Inner let does not shadow outer free variables."
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast:make-ast-let
                  :bindings (list (cons 'x (cl-cc/ast:make-ast-int :value 1)))
                  :body (list
                         (cl-cc/ast:make-ast-let
                          :bindings (list (cons 'y (cl-cc/ast:make-ast-var :name 'x)))
                          :body (list (cl-cc/ast:make-ast-binop
                                       :op '+
                                       :lhs (cl-cc/ast:make-ast-var :name 'y)
                                       :rhs (cl-cc/ast:make-ast-var :name 'w)))))))))
    ;; x is bound by outer let, y is bound by inner let, w is free
    (assert-equal '(w) result)))

;;; ─── Defun ────────────────────────────────────────────────────────────────

(deftest free-vars-defun-shadows-params
  "Defun params are not free in body."
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast:make-ast-defun
                  :name 'my-fn
                  :params '(a b)
                  :body (list (cl-cc/ast:make-ast-binop
                               :op '+
                               :lhs (cl-cc/ast:make-ast-var :name 'a)
                               :rhs (cl-cc/ast:make-ast-var :name 'c)))))))
    ;; a,b are params, c is free
    (assert-equal '(c) result)))

;;; ─── Setq ─────────────────────────────────────────────────────────────────

(deftest free-vars-setq
  "Setq marks both the variable and the value expression's free vars."
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast:make-ast-setq
                  :var 'x
                  :value (cl-cc/ast:make-ast-var :name 'y)))))
    (assert-true (and (member 'x result) (member 'y result)))
    (assert-equal 2 (length result))))

;;; ─── Call ─────────────────────────────────────────────────────────────────

(deftest free-vars-call-symbol-func
  "Symbol func in ast-call contributes no free vars; only the argument does."
  (assert-equal '(x)
    (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-call
      :func 'foo
      :args (list (cl-cc/ast:make-ast-var :name 'x))))))

(deftest free-vars-call-ast-func-node
  "AST func node in ast-call contributes its own free vars alongside the args."
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast:make-ast-call
                  :func (cl-cc/ast:make-ast-var :name 'f)
                  :args (list (cl-cc/ast:make-ast-var :name 'x))))))
    (assert-true (member 'f result))
    (assert-true (member 'x result))
    (assert-equal 2 (length result))))

;;; ─── Flet / Labels ───────────────────────────────────────────────────────

(deftest free-vars-flet-bound-name-not-free
  "Flet binds the function name; calling it in the body does not introduce a free var."
  (assert-equal nil
    (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-flet
      :bindings (list (list 'my-fn '(a) (cl-cc/ast:make-ast-var :name 'a)))
      :body (list (cl-cc/ast:make-ast-call
                   :func 'my-fn
                   :args (list (cl-cc/ast:make-ast-int :value 1))))))))

(deftest free-vars-labels-outer-var-in-binding-is-free
  "Labels: a variable from outside the labels scope referenced inside a binding body is free."
  (assert-equal '(limit)
    (cl-cc/compile::find-free-variables
     (cl-cc/ast:make-ast-labels
      :bindings (list (list 'rec '(n) (cl-cc/ast:make-ast-var :name 'limit)))
      :body (list (cl-cc/ast:make-ast-call
                   :func 'rec
                   :args (list (cl-cc/ast:make-ast-int :value 0))))))))

;;; ─── If / Progn ──────────────────────────────────────────────────────────

(deftest free-vars-if-collects-all-branches
  "If collects free vars from cond, then, and else — all three contribute."
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast:make-ast-if
                  :cond (cl-cc/ast:make-ast-var :name 'p)
                  :then (cl-cc/ast:make-ast-var :name 'x)
                  :else (cl-cc/ast:make-ast-var :name 'y)))))
    (assert-true (member 'p result))
    (assert-true (member 'x result))
    (assert-true (member 'y result))
    (assert-equal 3 (length result))))

(deftest free-vars-progn-collects-all-forms
  "Progn collects free vars from every sub-form."
  (let ((result (cl-cc/compile::find-free-variables
                 (cl-cc/ast:make-ast-progn
                  :forms (list (cl-cc/ast:make-ast-var :name 'a)
                               (cl-cc/ast:make-ast-var :name 'b))))))
    (assert-true (member 'a result))
    (assert-true (member 'b result))
    (assert-equal 2 (length result))))

;;; ─── %escape-add-kind / %escape-merge-kinds (extracted pure helpers) ─────

(deftest-each escape-add-kind-deduplicates
  "%escape-add-kind adds a kind only when absent."
  :cases (("fresh-add"     :return   nil              '(:return))
          ("already-there" :return   '(:return)       '(:return))
          ("second-kind"   :capture  '(:return)       '(:capture :return)))
  (kind acc expected)
  (assert-equal expected (cl-cc/ast::%escape-add-kind kind acc)))

(deftest escape-merge-kinds-merges-multiple-lists
  "%escape-merge-kinds deduplicates across multiple kind lists."
  (assert-equal nil (cl-cc/ast::%escape-merge-kinds nil nil))
  (let ((result (cl-cc/ast::%escape-merge-kinds '(:return) '(:return :capture))))
    (assert-= 2 (length result))
    (assert-true (member :return  result))
    (assert-true (member :capture result))))

;;; ─── %count-ast-calls (extracted recursive helper) ───────────────────────

(deftest count-ast-calls-direct-match-returns-one
  "%count-ast-calls returns 1 when the node directly calls the target function."
  (let ((node (cl-cc/ast:make-ast-call
               :func 'my-fn
               :args (list (cl-cc/ast:make-ast-int :value 1)))))
    (assert-= 1 (cl-cc/ast::%count-ast-calls node 'my-fn))))

(deftest count-ast-calls-no-match-returns-zero
  "%count-ast-calls returns 0 when the node calls a different function."
  (let ((node (cl-cc/ast:make-ast-call
               :func 'other-fn
               :args (list (cl-cc/ast:make-ast-int :value 1)))))
    (assert-= 0 (cl-cc/ast::%count-ast-calls node 'my-fn))))

(deftest count-ast-calls-counts-across-nested-children
  "%count-ast-calls counts all occurrences of the target across nested child nodes."
  (let* ((inner (cl-cc/ast:make-ast-call :func 'my-fn :args nil))
         (outer (cl-cc/ast:make-ast-progn :forms (list inner inner))))
    (assert-= 2 (cl-cc/ast::%count-ast-calls outer 'my-fn))))

;;; ─── %escape-mentions-node-p / %escape-mentions-forms-p ─────────────────

(deftest-each escape-mentions-node-p-cases
  "%escape-mentions-node-p: T for matching ast-var; NIL for non-matching or integer."
  :cases (("match"     (cl-cc/ast:make-ast-var :name 'x) 'x   t)
          ("no-match"  (cl-cc/ast:make-ast-var :name 'y) 'x   nil)
          ("literal"   (cl-cc/ast:make-ast-int :value 1) 'x   nil))
  (node binding expected)
  (if expected
      (assert-true  (cl-cc/ast::%escape-mentions-node-p node binding))
      (assert-false (cl-cc/ast::%escape-mentions-node-p node binding))))

(deftest escape-mentions-forms-p-returns-true-when-a-form-matches
  "%escape-mentions-forms-p returns T when any form in the list mentions the binding name."
  (assert-true (cl-cc/ast::%escape-mentions-forms-p
                (list (cl-cc/ast:make-ast-var :name 'x)) 'x)))

(deftest escape-mentions-forms-p-returns-false-when-no-form-matches
  "%escape-mentions-forms-p returns NIL when no form in the list references the binding."
  (assert-false (cl-cc/ast::%escape-mentions-forms-p
                 (list (cl-cc/ast:make-ast-int :value 1)) 'x)))

(deftest escape-mentions-forms-p-returns-false-for-nil-list
  "%escape-mentions-forms-p returns NIL for an empty form list."
  (assert-false (cl-cc/ast::%escape-mentions-forms-p nil 'x)))

;;; ─── %escape-classify-children ──────────────────────────────────────────

(deftest escape-classify-children-returns-nil-when-no-child-matches
  "%escape-classify-children returns NIL when no child node references the binding."
  (let* ((child  (cl-cc/ast:make-ast-int :value 42))
         (parent (cl-cc/ast:make-ast-progn :forms (list child))))
    (assert-null (cl-cc/ast::%escape-classify-children parent 'x nil))))

(deftest escape-classify-children-reports-return-when-child-is-matching-var
  "%escape-classify-children reports :return when a child is an ast-var matching the binding."
  (let* ((child  (cl-cc/ast:make-ast-var :name 'x))
         (parent (cl-cc/ast:make-ast-progn :forms (list child))))
    (let ((kinds (cl-cc/ast::%escape-classify-children parent 'x nil)))
      (assert-true (member :return kinds)))))

;;; ─── %escape-capture-kinds ───────────────────────────────────────────────

(deftest escape-capture-kinds-returns-nil-when-body-does-not-reference-binding
  "%escape-capture-kinds returns NIL when the body does not reference the binding name."
  (let ((body (list (cl-cc/ast:make-ast-int :value 0))))
    (assert-null (cl-cc/ast::%escape-capture-kinds body 'x nil))))

(deftest escape-capture-kinds-returns-capture-when-body-references-binding
  "%escape-capture-kinds includes :capture when the body directly references the binding."
  (let ((body (list (cl-cc/ast:make-ast-var :name 'x))))
    (let ((kinds (cl-cc/ast::%escape-capture-kinds body 'x nil)))
      (assert-true (member :capture kinds)))))

;;; ─── %escape-classify / binding-escape-kinds-in-body ────────────────────

(deftest escape-classify-matching-var-yields-return
  "%escape-classify on an ast-var matching the binding yields (:return)."
  (let ((node (cl-cc/ast:make-ast-var :name 'x)))
    (assert-equal '(:return) (cl-cc/ast::%escape-classify node 'x nil))))

(deftest escape-classify-non-matching-var-yields-nil
  "%escape-classify on an ast-var not matching the binding yields NIL."
  (let ((node (cl-cc/ast:make-ast-var :name 'y)))
    (assert-null (cl-cc/ast::%escape-classify node 'x nil))))

(deftest binding-escape-kinds-detects-capture-via-lambda
  "binding-escape-kinds-in-body includes :capture when the binding is referenced inside a nested lambda."
  (let* ((body (list (cl-cc/ast:make-ast-lambda
                      :params '(z)
                      :body   (list (cl-cc/ast:make-ast-var :name 'x))))))
    (let ((kinds (cl-cc/compile::binding-escape-kinds-in-body body 'x)))
      (assert-true (member :capture kinds)))))

(deftest binding-escape-kinds-returns-nil-for-empty-body
  "binding-escape-kinds-in-body returns NIL when the body is empty."
  (assert-null (cl-cc/compile::binding-escape-kinds-in-body nil 'x)))
